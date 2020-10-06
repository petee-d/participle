package codegen

import (
	"fmt"
	"io"
	"regexp"
	"regexp/syntax"
	"text/template"
	"unicode/utf8"

	"github.com/alecthomas/participle/lexer/stateful"
)

var backrefRe = regexp.MustCompile(`(\\+)(\d)`)

var tmpl = template.Must(template.New("lexgen").Funcs(template.FuncMap{
	"IsPush": func(r stateful.Rule) string {
		if p, ok := r.Action.(stateful.ActionPush); ok {
			return p.State
		}
		return ""
	},
	"IsPop": func(r stateful.Rule) bool {
		_, ok := r.Action.(stateful.ActionPop)
		return ok
	},
	"HaveBackrefs": func(def *stateful.Definition, state string) bool {
		for _, rule := range def.Rules()[state] {
			if backrefRe.MatchString(rule.Pattern) {
				return true
			}
		}
		return false
	},
}).Parse(`
// Code generated by Participle. DO NOT EDIT.
package {{.Package}}

import (
	"strings"
	"unicode/utf8"

	"github.com/alecthomas/participle"
	"github.com/alecthomas/participle/lexer"
)

var Lexer lexer.Definition = definitionImpl{}

type definitionImpl struct {}

func (definitionImpl) Symbols() map[string]rune {
	return map[string]rune{
{{- range $sym, $rn := .Def.Symbols}}
      "{{$sym}}": {{$rn}},
{{- end}}
	}
}

func (definitionImpl) LexString(filename string, s string) (lexer.Lexer, error) {
	return &lexerImpl{
		s: s,
		pos: lexer.Position{
			Filename: filename,
			Line:     1,
			Column:   1,
		},
		states: []lexerState{lexerState{name: "Root"}},
	}, nil
}

func (d definitionImpl) LexBytes(filename string, b []byte) (lexer.Lexer, error) {
	return d.LexString(filename, string(b))
}

func (d definitionImpl) Lex(filename string, r io.Reader) (lexer.Lexer, error) {
	s := &strings.Builder{}
	_, err := io.Copy(s, r)
	if err != nil {
		return nil, err
	}
	return d.LexString(filename, s.String())
}

type lexerState struct {
	name    string
	groups  []string
}

type lexerImpl struct {
	s       string
	p       int
	pos     lexer.Position
	states  []lexerState
}

func (l *lexerImpl) Next() (lexer.Token, error) {
	if l.p == len(l.s) {
		return lexer.EOFToken(l.pos), nil
	}
	var (
		state = l.states[len(l.states)-1]
		groups []int
		sym rune
	)
	switch state.name {
{{- range $state, $rules := .Def.Rules}}
	case "{{$state}}":
{{- range $i, $rule := $rules}}
		{{- if $i}} else {{end -}}
{{- if .Pattern -}}
		if match := match{{.Name}}(l.s, l.p); match[1] != 0 {
			sym = {{index $.Def.Symbols .Name}}
			groups = match[:]
{{- else}}
		if true {
{{- end}}
{{- if .|IsPush}}
			l.states = append(l.states, lexerState{name: "{{.|IsPush}}"{{if HaveBackrefs $.Def $state}}, groups: l.sgroups(groups){{end}}})
{{- else if .|IsPop}}
			l.states = l.states[:len(l.states)-1]
{{- else if not .Action}}
{{- else}}
		Unsupported action {{.Action}}
{{- end}}
		}
{{- end}}
{{- end}}
	}
	if groups == nil {
		return lexer.Token{}, participle.Errorf(l.pos, "no lexer rules in state %q matched input text", l.states[len(l.states)-1])
	}
	pos := l.pos
	span := l.s[groups[0]:groups[1]]
	l.p = groups[1]
	l.pos.Offset = groups[1]
	lines := strings.Count(span, "\n")
	l.pos.Line += lines
	// Update column.
	if lines == 0 {
		l.pos.Column += utf8.RuneCountInString(span)
	} else {
		l.pos.Column = utf8.RuneCountInString(span[strings.LastIndex(span, "\n"):])
	}
	return lexer.Token{
		Type:  sym,
		Value: span,
		Pos:   pos,
	}, nil
}

func (l *lexerImpl) sgroups(match []int) []string {
	sgroups := make([]string, len(match)/2)
	for i := 0; i < len(match)-1; i += 2 {
		sgroups[i/2] = l.s[l.p+match[i]:l.p+match[i+1]]
	}
	return sgroups
}

`))

func Generate(w io.Writer, pkg string, def *stateful.Definition) error {
	type ctx struct {
		Package string
		Def     *stateful.Definition
	}
	rules := def.Rules()
	err := tmpl.Execute(w, ctx{pkg, def})
	if err != nil {
		return err
	}
	seen := map[string]bool{} // Rules can be duplicated by Include().
	for _, rules := range rules {
		for _, rule := range rules {
			if rule.Name == "" {
				panic(rule)
			}
			if seen[rule.Name] {
				continue
			}
			seen[rule.Name] = true
			fmt.Fprintf(w, "\n")
			err := generateRegexMatch(w, rule.Name, rule.Pattern)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func generateRegexMatch(w io.Writer, name, pattern string) error {
	re, err := syntax.Parse(pattern, syntax.Perl)
	if err != nil {
		return err
	}
	ids := map[string]int{}
	idn := 0
	reid := func(re *syntax.Regexp) int {
		key := re.Op.String() + ":" + re.String()
		id, ok := ids[key]
		if ok {
			return id
		}
		id = idn
		idn++
		ids[key] = id
		return id
	}
	exists := func(re *syntax.Regexp) bool {
		key := re.Op.String() + ":" + re.String()
		_, ok := ids[key]
		return ok
	}
	re = re.Simplify()
	fmt.Fprintf(w, "// %s\n", re)
	fmt.Fprintf(w, "func match%s(s string, p int) (groups [%d]int) {\n", name, 2*re.MaxCap()+2)
	flattened := flatten(re)

	// Fast-path a single literal.
	if len(flattened) == 1 && re.Op == syntax.OpLiteral {
		n := utf8.RuneCountInString(string(re.Rune))
		if n == 1 {
			fmt.Fprintf(w, "if p < len(s) && s[p] == %q {\n", re.Rune[0])
		} else {
			fmt.Fprintf(w, "if p+%d < len(s) && s[p:p+%d] == %q {\n", n, n, string(re.Rune))
		}
		fmt.Fprintf(w, "groups[0] = p\n")
		fmt.Fprintf(w, "groups[1] = p + %d\n", n)
		fmt.Fprintf(w, "}\n")
		fmt.Fprintf(w, "return\n")
		fmt.Fprintf(w, "}\n")
		return nil
	}
	for _, re := range flattened {
		if exists(re) {
			continue
		}
		fmt.Fprintf(w, "// %s (%s)\n", re, re.Op)
		fmt.Fprintf(w, "l%d := func(s string, p int) int {\n", reid(re))
		if re.Flags&syntax.NonGreedy != 0 {
			panic("non-greedy match not supported: " + re.String())
		}
		switch re.Op {
		case syntax.OpNoMatch: // matches no strings
			fmt.Fprintf(w, "return p\n")

		case syntax.OpEmptyMatch: // matches empty string
			fmt.Fprintf(w, "if len(s) == 0 { return p }\n")
			fmt.Fprintf(w, "return -1\n")

		case syntax.OpLiteral: // matches Runes sequence
			n := utf8.RuneCountInString(string(re.Rune))
			if n == 1 {
				fmt.Fprintf(w, "if p < len(s) && s[p] == %q { return p+1 }\n", re.Rune[0])
			} else {
				fmt.Fprintf(w, "if p+%d < len(s) && s[p:p+%d] == %q { return p+%d }\n", n, n, string(re.Rune), n)
			}
			fmt.Fprintf(w, "return -1\n")

		case syntax.OpCharClass: // matches Runes interpreted as range pair list
			fmt.Fprintf(w, "if len(s) <= p { return -1 }\n")
			needDecode := false
			for i := 0; i < len(re.Rune); i += 2 {
				l, r := re.Rune[i], re.Rune[i+1]
				ln, rn := utf8.RuneLen(l), utf8.RuneLen(r)
				if ln != 1 || rn != 1 {
					needDecode = true
					break
				}
			}
			if needDecode {
				fmt.Fprintf(w, "var (rn rune; n int)\n")
				decodeRune(w, "p", "rn", "n")
			} else {
				fmt.Fprintf(w, "rn := s[p]\n")
			}
			fmt.Fprintf(w, "switch {\n")
			for i := 0; i < len(re.Rune); i += 2 {
				l, r := re.Rune[i], re.Rune[i+1]
				ln, rn := utf8.RuneLen(l), utf8.RuneLen(r)
				if ln == 1 && rn == 1 {
					if l == r {
						fmt.Fprintf(w, "case rn == %q: return p+1\n", l)
					} else {
						fmt.Fprintf(w, "case rn >= %q && rn <= %q: return p+1\n", l, r)
					}
				} else {
					if l == r {
						fmt.Fprintf(w, "case rn == %q: return p+n\n", l)
					} else {
						fmt.Fprintf(w, "case rn >= %q && rn <= %q: return p+n\n", l, r)
					}
				}
			}
			fmt.Fprintf(w, "}\n")
			fmt.Fprintf(w, "return -1\n")

		case syntax.OpAnyCharNotNL: // matches any character except newline
			fmt.Fprintf(w, "var (rn rune; n int)\n")
			decodeRune(w, "p", "rn", "n")
			fmt.Fprintf(w, "if len(s) <= p+n || rn == '\\n' { return -1 }\n")
			fmt.Fprintf(w, "return p+n\n")

		case syntax.OpAnyChar: // matches any character
			fmt.Fprintf(w, "var n int\n")
			fmt.Fprintf(w, "if s[p] < utf8.RuneSelf {\n")
			fmt.Fprintf(w, "  n = 1\n")
			fmt.Fprintf(w, "} else {\n")
			fmt.Fprintf(w, "  _, n = utf8.DecodeRuneInString(s[p:])\n")
			fmt.Fprintf(w, "}\n")
			fmt.Fprintf(w, "if len(s) <= p+n { return -1 }\n")
			fmt.Fprintf(w, "return p+n\n")

		case syntax.OpWordBoundary, syntax.OpNoWordBoundary,
			syntax.OpBeginText, syntax.OpEndText,
			syntax.OpBeginLine, syntax.OpEndLine:
			fmt.Fprintf(w, "var l, u rune = -1, -1\n")
			fmt.Fprintf(w, "if p == 0 {\n")
			decodeRune(w, "0", "u", "_")
			fmt.Fprintf(w, "} else if p == len(s) {\n")
			fmt.Fprintf(w, "  l, _ = utf8.DecodeLastRuneInString(s)\n")
			fmt.Fprintf(w, "} else {\n")
			fmt.Fprintf(w, "  var ln int\n")
			decodeRune(w, "p", "l", "ln")
			fmt.Fprintf(w, "  if p+ln <= len(s) {\n")
			decodeRune(w, "p+ln", "u", "_")
			fmt.Fprintf(w, "  }\n")
			fmt.Fprintf(w, "}\n")
			fmt.Fprintf(w, "op := syntax.EmptyOpContext(l, u)\n")
			lut := map[syntax.Op]string{
				syntax.OpWordBoundary:   "EmptyWordBoundary",
				syntax.OpNoWordBoundary: "EmptyNoWordBoundary",
				syntax.OpBeginText:      "EmptyBeginText",
				syntax.OpEndText:        "EmptyEndText",
				syntax.OpBeginLine:      "EmptyBeginLine",
				syntax.OpEndLine:        "EmptyEndLine",
			}
			fmt.Fprintf(w, "if op & syntax.%s != 0 { return p }\n", lut[re.Op])
			fmt.Fprintf(w, "return -1\n")

		case syntax.OpCapture: // capturing subexpression with index Cap, optional name Name
			fmt.Fprintf(w, "np := l%d(s, p)\n", reid(re.Sub0[0]))
			fmt.Fprintf(w, "if np != -1 {\n")
			fmt.Fprintf(w, "  groups[%d] = p\n", re.Cap*2)
			fmt.Fprintf(w, "  groups[%d] = np\n", re.Cap*2+1)
			fmt.Fprintf(w, "}\n")
			fmt.Fprintf(w, "return np")

		case syntax.OpStar: // matches Sub[0] zero or more times
			fmt.Fprintf(w, "for len(s) > p {\n")
			fmt.Fprintf(w, "if np := l%d(s, p); np == -1 { return p } else { p = np }\n", reid(re.Sub0[0]))
			fmt.Fprintf(w, "}\n")
			fmt.Fprintf(w, "return p\n")

		case syntax.OpPlus: // matches Sub[0] one or more times
			fmt.Fprintf(w, "if p = l%d(s, p); p == -1 { return -1 }\n", reid(re.Sub0[0]))
			fmt.Fprintf(w, "for len(s) > p {\n")
			fmt.Fprintf(w, "if np := l%d(s, p); np == -1 { return p } else { p = np }\n", reid(re.Sub0[0]))
			fmt.Fprintf(w, "}\n")
			fmt.Fprintf(w, "return p\n")

		case syntax.OpQuest: // matches Sub[0] zero or one times
			fmt.Fprintf(w, "if np := l%d(s, p); np != -1 { return np }\n", reid(re.Sub0[0]))
			fmt.Fprintf(w, "return p\n")

		case syntax.OpRepeat: // matches Sub[0] at least Min times, at most Max (Max == -1 is no limit)
			panic("??")

		case syntax.OpConcat: // matches concatenation of Subs
			for _, sub := range re.Sub {
				fmt.Fprintf(w, "if p = l%d(s, p); p == -1 { return -1 }\n", reid(sub))
			}
			fmt.Fprintf(w, "return p\n")

		case syntax.OpAlternate: // matches alternation of Subs
			for _, sub := range re.Sub {
				fmt.Fprintf(w, "if np := l%d(s, p); np != -1 { return np }\n", reid(sub))
			}
			fmt.Fprintf(w, "return -1\n")
		}
		fmt.Fprintf(w, "}\n")
	}
	fmt.Fprintf(w, "np := l%d(s, p)\n", reid(re))
	fmt.Fprintf(w, "if np == -1 {\n")
	fmt.Fprintf(w, "  return\n")
	fmt.Fprintf(w, "}\n")
	fmt.Fprintf(w, "groups[0] = p\n")
	fmt.Fprintf(w, "groups[1] = np\n")
	fmt.Fprintf(w, "return\n")
	fmt.Fprintf(w, "}\n")
	return nil
}

// This exists because of https://github.com/golang/go/issues/31666
func decodeRune(w io.Writer, offset string, rn string, n string) {
	fmt.Fprintf(w, "if s[%s] < utf8.RuneSelf {\n", offset)
	fmt.Fprintf(w, "  %s, %s = rune(s[%s]), 1\n", rn, n, offset)
	fmt.Fprintf(w, "} else {\n")
	fmt.Fprintf(w, "  %s, %s = utf8.DecodeRuneInString(s[%s:])\n", rn, n, offset)
	fmt.Fprintf(w, "}\n")
}

func flatten(re *syntax.Regexp) (out []*syntax.Regexp) {
	for _, sub := range re.Sub {
		out = append(out, flatten(sub)...)
	}
	out = append(out, re)
	return
}
