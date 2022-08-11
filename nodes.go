package participle

import (
	"encoding"
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
)

var (
	// MaxIterations limits the number of elements capturable by {}.
	MaxIterations = 1000000

	positionType        = reflect.TypeOf(lexer.Position{})
	tokenType           = reflect.TypeOf(lexer.Token{})
	tokensType          = reflect.TypeOf([]lexer.Token{})
	captureType         = reflect.TypeOf((*Capture)(nil)).Elem()
	textUnmarshalerType = reflect.TypeOf((*encoding.TextUnmarshaler)(nil)).Elem()
	parseableType       = reflect.TypeOf((*Parseable)(nil)).Elem()

	// NextMatch should be returned by Parseable.Parse() method implementations to indicate
	// that the node did not match and that other matches should be attempted, if appropriate.
	NextMatch = errors.New("no match") // nolint: golint
)

// A node in the grammar.
type node interface {
	// Parse from scanner into value.
	//
	// Returned slice will be nil if the node does not match.
	Parse(ctx *parseContext, parent reflect.Value) ([]reflect.Value, error)

	// Generate Go code for parsing this node
	Generate(state generatorState, gen *codeGenerator)

	// Return a decent string representation of the Node.
	fmt.Stringer

	fmt.GoStringer
}

func decorate(err *error, name func() string) {
	if *err == nil {
		return
	}
	if perr, ok := (*err).(Error); ok {
		*err = Errorf(perr.Position(), "%s: %s", name(), perr.Message())
	} else {
		*err = &parseError{Msg: fmt.Sprintf("%s: %s", name(), *err)}
	}
}

// A node that proxies to an implementation that implements the Parseable interface.
type parseable struct {
	t reflect.Type
}

func (p *parseable) String() string   { return ebnf(p) }
func (p *parseable) GoString() string { return p.t.String() }

func (p *parseable) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	rv := reflect.New(p.t)
	v := rv.Interface().(Parseable)
	err = v.Parse(ctx.PeekingLexer)
	if err != nil {
		if err == NextMatch {
			return nil, nil
		}
		return nil, err
	}
	return []reflect.Value{rv.Elem()}, nil
}

func (p *parseable) Generate(state generatorState, gen *codeGenerator) {
	gen.statement(`// parseable ` + p.t.String())
	gen.statement(`if err := %s.Parse(p.Lex); err == participle.NextMatch {`) // TODO handle errors somehow
	gen.statement(`} else if err != nil {`)                                   // TODO
	gen.statement(`}`)
	gen.statement(`current = p.Lex.Current() // The token stream has likely progressed, restore current`)
	gen.statement(``)
}

// @@
type strct struct {
	typ              reflect.Type
	expr             node
	tokensFieldIndex []int
	posFieldIndex    []int
	endPosFieldIndex []int
	usages           int
}

func newStrct(typ reflect.Type) *strct {
	s := &strct{
		typ:    typ,
		usages: 1,
	}
	field, ok := typ.FieldByName("Pos")
	if ok && field.Type == positionType {
		s.posFieldIndex = field.Index
	}
	field, ok = typ.FieldByName("EndPos")
	if ok && field.Type == positionType {
		s.endPosFieldIndex = field.Index
	}
	field, ok = typ.FieldByName("Tokens")
	if ok && field.Type == tokensType {
		s.tokensFieldIndex = field.Index
	}
	return s
}

func (s *strct) String() string   { return ebnf(s) }
func (s *strct) GoString() string { return s.typ.Name() }

func (s *strct) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	sv := reflect.New(s.typ).Elem()
	start := ctx.RawCursor()
	t, err := ctx.Peek(0)
	if err != nil {
		return nil, err
	}
	s.maybeInjectStartToken(t, sv)
	if out, err = s.expr.Parse(ctx, sv); err != nil {
		_ = ctx.Apply() // Best effort to give partial AST.
		ctx.MaybeUpdateError(err)
		return []reflect.Value{sv}, err
	} else if out == nil {
		return nil, nil
	}
	end := ctx.RawCursor()
	t, _ = ctx.RawPeek(0)
	s.maybeInjectEndToken(t, sv)
	s.maybeInjectTokens(ctx.Range(start, end), sv)
	return []reflect.Value{sv}, ctx.Apply()
}

func (s *strct) Generate(state generatorState, gen *codeGenerator) {
	gen.statement(`// strct ` + s.GoString())
	normalizedName := s.normalizedName()
	variable := generatedVariable{
		name: "v" + normalizedName,
		typ:  s.typ,
	}

	gen.statement(`{`)
	gen.indent++
	gen.statement(fmt.Sprintf(`var %s %s%s`, variable.name, gen.packagePrefix, variable.typ.Name()))
	if s.usages > 1 {
		// Used multiple times, cannot be inlined, schedule generating a function and call it here
		gen.queueGeneratingStruct(s)
		gen.statement(fmt.Sprintf(`p.parse%s(&%s)`, normalizedName, variable.name))
		gen.statementIndent(`current = p.Lex.Current()`)
		gen.statement(`if p.HasErr() {`)
		if state.failUnexpectedWith != "" {
			gen.statementIndent(fmt.Sprintf(`p.AddTokenErrorExpected(%q)`, state.failUnexpectedWith))
		}
		gen.gotoLabelIndent(state.errorLabel, 1)
		gen.statement(`}`)
	} else {
		// Used only once, inline the body to avoid call overhead
		childState := state
		childState.target = variable
		childState.capture = nil
		s.generateBody(childState, gen)
	}
	gen.captureStruct(state, variable, s.usages)
	gen.indent--
	gen.statement(`}`)
	gen.statement(``)
}

func (s *strct) generateBody(state generatorState, gen *codeGenerator) {
	if s.tokensFieldIndex != nil {
		gen.statement(`rawStart := p.Lex.RawCursor()`)
	}
	if s.posFieldIndex != nil {
		gen.statement(gen.getFieldRef(state.target, s.posFieldIndex) + ` = current.Pos`)
	}
	gen.statement(``)
	s.expr.Generate(state, gen)
	if s.endPosFieldIndex != nil {
		gen.statement(gen.getFieldRef(state.target, s.endPosFieldIndex) + ` = current.Pos`)
	}
	if s.tokensFieldIndex != nil {
		gen.statement(gen.getFieldRef(state.target, s.tokensFieldIndex) + ` = p.Lex.Range(rawStart, p.Lex.RawCursor())`)
	}
}

func (s *strct) maybeInjectStartToken(token lexer.Token, v reflect.Value) {
	if s.posFieldIndex == nil {
		return
	}
	v.FieldByIndex(s.posFieldIndex).Set(reflect.ValueOf(token.Pos))
}

func (s *strct) maybeInjectEndToken(token lexer.Token, v reflect.Value) {
	if s.endPosFieldIndex == nil {
		return
	}
	v.FieldByIndex(s.endPosFieldIndex).Set(reflect.ValueOf(token.Pos))
}

func (s *strct) maybeInjectTokens(tokens []lexer.Token, v reflect.Value) {
	if s.tokensFieldIndex == nil {
		return
	}
	v.FieldByIndex(s.tokensFieldIndex).Set(reflect.ValueOf(tokens))
}

func (s *strct) normalizedName() string {
	return strings.ToUpper(s.typ.Name()[:1]) + s.typ.Name()[1:]
}

type groupMatchMode int

func (g groupMatchMode) String() string {
	switch g {
	case groupMatchOnce:
		return "n"
	case groupMatchZeroOrOne:
		return "n?"
	case groupMatchZeroOrMore:
		return "n*"
	case groupMatchOneOrMore:
		return "n+"
	case groupMatchNonEmpty:
		return "n!"
	}
	panic("??")
}

const (
	groupMatchOnce       groupMatchMode = iota
	groupMatchZeroOrOne                 = iota
	groupMatchZeroOrMore                = iota
	groupMatchOneOrMore                 = iota
	groupMatchNonEmpty                  = iota
)

// ( <expr> ) - match once
// ( <expr> )* - match zero or more times
// ( <expr> )+ - match one or more times
// ( <expr> )? - match zero or once
// ( <expr> )! - must be a non-empty match
//
// The additional modifier "!" forces the content of the group to be non-empty if it does match.
type group struct {
	expr node
	mode groupMatchMode
}

func (g *group) String() string   { return ebnf(g) }
func (g *group) GoString() string { return fmt.Sprintf("group{%s}", g.mode) }

func (g *group) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	// Configure min/max matches.
	min := 1
	max := 1
	switch g.mode {
	case groupMatchNonEmpty:
		out, err = g.expr.Parse(ctx, parent)
		if err != nil {
			return out, err
		}
		if len(out) == 0 {
			t, _ := ctx.Peek(0)
			return out, Errorf(t.Pos, "sub-expression %s cannot be empty", g)
		}
		return out, nil
	case groupMatchOnce:
		return g.expr.Parse(ctx, parent)
	case groupMatchZeroOrOne:
		min = 0
	case groupMatchZeroOrMore:
		min = 0
		max = MaxIterations
	case groupMatchOneOrMore:
		min = 1
		max = MaxIterations
	}
	matches := 0
	for ; matches < max; matches++ {
		branch := ctx.Branch()
		v, err := g.expr.Parse(branch, parent)
		if err != nil {
			ctx.MaybeUpdateError(err)
			// Optional part failed to match.
			if ctx.Stop(err, branch) {
				out = append(out, v...) // Try to return as much of the parse tree as possible
				return out, err
			}
			break
		}
		out = append(out, v...)
		ctx.Accept(branch)
		if v == nil {
			break
		}
	}
	// fmt.Printf("%d < %d < %d: out == nil? %v\n", min, matches, max, out == nil)
	t, _ := ctx.Peek(0)
	if matches >= MaxIterations {
		panic(Errorf(t.Pos, "too many iterations of %s (> %d)", g, MaxIterations))
	}
	if matches < min {
		return out, Errorf(t.Pos, "sub-expression %s must match at least once", g)
	}
	// The idea here is that something like "a"? is a successful match and that parsing should proceed.
	if min == 0 && out == nil {
		out = []reflect.Value{}
	}
	return out, nil
}

func (g *group) Generate(state generatorState, gen *codeGenerator) {
	if g.mode == groupMatchOnce {
		g.expr.Generate(state, gen)
		return
	}

	gen.statement(`// group ` + g.String())
	if g.mode == groupMatchNonEmpty {
		g.generateNonEmpty(state, gen)
		return
	}

	childState := state
	childState.failUnexpectedWith = ""
	childState.errorLabel = gen.newLabel("group", "Error")
	switch g.mode {
	case groupMatchZeroOrOne:
		gen.statement(`for {`)
	case groupMatchZeroOrMore, groupMatchOneOrMore:
		gen.statement(`for matches := 0; ; matches++ {`)
	default:
		panic(fmt.Errorf("unknown group mode: %s", g.String()))
	}
	gen.indent++
	gen.statement(`branchCheckpoint := p.Lex.MakeCheckpoint()`)
	gen.statement(``)
	g.expr.Generate(childState, gen)
	switch g.mode {
	case groupMatchZeroOrOne:
		gen.statement(`break`)
	case groupMatchZeroOrMore, groupMatchOneOrMore:
		gen.statement(`if matches >= participle.MaxIterations {`)
		gen.statement("\t" + fmt.Sprintf(`panic(participle.NewError(current.Pos, %q))`,
			fmt.Sprintf("too many iterations of %s (> %d)", g, MaxIterations)))
		gen.statement(`}`)
		gen.statement(`continue`)
	}
	gen.writeLabel(childState.errorLabel)
	gen.statement(`p.Lex.LoadCheckpoint(branchCheckpoint)`)
	gen.statement(`current = p.Lex.Current()`)
	gen.statement(`if p.AboveLookahead(branchCheckpoint) {`) // TODO lookahead
	gen.gotoLabelIndent(state.errorLabel, 1)
	gen.statement(`}`)
	if g.mode == groupMatchOneOrMore {
		gen.statement(`if matches == 0 {`)
		// TODO lookahead
		gen.statement("\t" + fmt.Sprintf(`p.SetParseError(%q)`,
			fmt.Sprintf("sub-expression %s must match at least once", g)))
		gen.gotoLabelIndent(state.errorLabel, 1)
		gen.statement(`}`)
	}
	gen.statement(`p.SetNoError(true)`) /// TODO CONTINUE FROM HERE, study the difference between Parse nil result and err
	gen.statement(`break`)
	gen.indent--
	gen.statement(`}`)
	gen.statement(``)
}

func (g *group) generateNonEmpty(state generatorState, gen *codeGenerator) {
	gen.statement(`{`)
	gen.indent++
	gen.statement(`nonEmptyCheckpoint := p.Lex.MakeCheckpoint()`)
	g.expr.Generate(state, gen)
	gen.statement(`if p.Lex.Cursor() == nonEmptyCheckpoint.Cursor() {`)
	gen.statementIndent(fmt.Sprintf(`p.SetParseError(%q)`,
		fmt.Sprintf("sub-expression %s cannot be empty", g))) // TODO lookahead?
	gen.gotoLabelIndent(state.errorLabel, 1)
	gen.statement(`}`)
	gen.indent--
	gen.statement(`}`)
	return
}

// (?= <expr> ) for positive lookahead, (?! <expr> ) for negative lookahead; neither consumes input
type lookaheadGroup struct {
	expr     node
	negative bool
}

func (l *lookaheadGroup) String() string   { return ebnf(l) }
func (l *lookaheadGroup) GoString() string { return "lookaheadGroup{}" }

func (l *lookaheadGroup) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	// Create a branch to avoid advancing the parser as any match will be discarded
	branch := ctx.Branch()
	out, err = l.expr.Parse(branch, parent)
	matchedLookahead := err == nil && out != nil
	expectingMatch := !l.negative
	if matchedLookahead != expectingMatch {
		peek, err := ctx.Peek(0)
		if err != nil {
			return nil, err
		}
		return nil, UnexpectedTokenError{Unexpected: peek} // TODO breaking change!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	}
	return []reflect.Value{}, nil // Empty match slice means a match, unlike nil
}

func (l *lookaheadGroup) Generate(state generatorState, gen *codeGenerator) {
	gen.statement(`// lookaheadGroup ` + l.String())
	childState := state
	gen.statement(`{`)
	gen.indent++
	gen.statement(`branchCheckpoint := p.Lex.MakeCheckpoint()`)
	gen.statement(``)
	if l.negative {
		// Negative lookahead - expect and silence errors in the branch, raise if there is none
		childState.failUnexpectedWith = ""
		childState.errorLabel = gen.newLabel("lookahead", "Error")
	}
	l.expr.Generate(childState, gen)
	gen.statement(`p.Lex.LoadCheckpoint(branchCheckpoint)`)
	gen.statement(`current = p.Lex.Current()`)
	if l.negative {
		// Negative lookahead - if here, there was no error, so this should not match
		gen.statement(`p.SetTokenError("")`)
		gen.gotoLabelIndent(state.errorLabel, 0)

		// Catch errors and convert them to a success
		gen.writeLabel(childState.errorLabel)
		gen.statement(`p.Lex.LoadCheckpoint(branchCheckpoint)`) // Needed here as execution skipped the above code
		gen.statement(`current = p.Lex.Current()`)
		gen.statement(`p.SetNoError(false)`)
	}
	gen.indent--
	gen.statement(`}`)
	gen.statement(``)
}

// <expr> {"|" <expr>}
type disjunction struct {
	nodes []node
}

func (d *disjunction) String() string   { return ebnf(d) }
func (d *disjunction) GoString() string { return "disjunction{}" }

func (d *disjunction) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	var (
		deepestError = 0
		firstError   error
		firstValues  []reflect.Value
	)
	for _, a := range d.nodes {
		branch := ctx.Branch()
		if value, err := a.Parse(branch, parent); err != nil {
			// If this branch progressed too far and still didn't match, error out.
			if ctx.Stop(err, branch) {
				return value, err
			}
			// Show the closest error returned. The idea here is that the further the parser progresses
			// without error, the more difficult it is to trace the error back to its root.
			if branch.Cursor() >= deepestError {
				firstError = err
				firstValues = value
				deepestError = branch.Cursor()
			}
		} else if value != nil {
			bt, _ := branch.Peek(0)
			ct, _ := ctx.Peek(0)
			if bt == ct && bt.Type != lexer.EOF {
				panic(Errorf(bt.Pos, "branch %s was accepted but did not progress the lexer at %s (%q)", a, bt.Pos, bt.Value))
			}
			ctx.Accept(branch)
			return value, nil
		}
	}
	if firstError != nil {
		ctx.MaybeUpdateError(firstError)
		return firstValues, firstError
	}
	return nil, nil
}

func (d *disjunction) Generate(state generatorState, gen *codeGenerator) {
	gen.statement(`// disjunction ` + d.String())
	if d.generateLiteralSwitch(state, gen) {
		return
	}

	successLabel := gen.newLabel("disjunction", "Success")
	afterBranch := make([]*jumpLabel, len(d.nodes)) // Makes labels have a consistent line number
	for i := range d.nodes {
		afterBranch[i] = gen.newLabel("disjunction", fmt.Sprintf("Alt%dError", i))
	}

	gen.statement(`{`)
	gen.indent++
	gen.statement(`branchCheckpoint := p.Lex.MakeCheckpoint()`)
	gen.statement(`var _ = ` + state.target.rValuePrefix + state.target.name)

	for i, alt := range d.nodes {
		if i > 0 {
			gen.statement(`p.Lex.LoadCheckpoint(branchCheckpoint)`)
			gen.statement(`current = p.Lex.Current()`)
		}
		childState := state
		childState.failUnexpectedWith = ""
		childState.errorLabel = afterBranch[i]
		gen.statement(``)
		alt.Generate(childState, gen)
		// TODO: check for the panic as in Parse?
		gen.gotoLabelIndent(successLabel, 0)
		gen.writeLabel(childState.errorLabel)
		gen.statement(`if p.AboveLookahead(branchCheckpoint) {`) // TODO lookahead
		gen.gotoLabelIndent(state.errorLabel, 1)
		gen.statement(`}`)
	}
	gen.gotoLabelIndent(state.errorLabel, 0) // TODO: Choose deepest alternative error
	gen.indent--
	gen.statement(`}`)
	gen.writeLabel(successLabel)
	gen.statement(`p.SetNoError(false)`)
	gen.statement(``)
}

func (d *disjunction) generateLiteralSwitch(state generatorState, gen *codeGenerator) bool {
	literals := make([]string, 0, len(d.nodes))
	for _, n := range d.nodes {
		if lit, ok := n.(*literal); ok && lit.t == -1 {
			literals = append(literals, strconv.Quote(lit.s))
		} else {
			return false
		}
	}
	gen.statement(`switch current.Value {`)
	gen.statement(`case ` + strings.Join(literals, ", ") + `:`)
	gen.indent++
	gen.processToken(state)
	gen.indent--
	gen.statement(`default:`)
	gen.handleMismatchIndent(state, 1)
	gen.statement(`}`)
	gen.statement(``)
	return true
}

// <node> ...
type sequence struct {
	head bool // True if this is the head node.
	node node
	next *sequence
}

func (s *sequence) String() string   { return ebnf(s) }
func (s *sequence) GoString() string { return "sequence{}" }

func (s *sequence) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	for n := s; n != nil; n = n.next {
		child, err := n.node.Parse(ctx, parent)
		out = append(out, child...)
		if err != nil {
			return out, err
		}
		if child == nil {
			// Early exit if first value doesn't match, otherwise all values must match.
			if n == s {
				return nil, nil
			}
			token, err := ctx.Peek(0)
			if err != nil {
				return nil, err
			}
			return out, UnexpectedTokenError{Unexpected: token, at: n}
		}
	}
	return out, nil
}
func (s *sequence) Generate(state generatorState, gen *codeGenerator) {
	gen.statement(`// sequence ` + s.String())
	for n := s; n != nil; n = n.next {
		if n != s {
			state.failUnexpectedWith = n.String()
		}
		n.node.Generate(state, gen)
	}
}

// @<expr>
type capture struct {
	field structLexerField
	node  node
}

func (c *capture) String() string   { return ebnf(c) }
func (c *capture) GoString() string { return "capture{}" }

func (c *capture) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	start := ctx.RawCursor()
	v, err := c.node.Parse(ctx, parent)
	if v != nil {
		ctx.Defer(ctx.Range(start, ctx.RawCursor()), parent, c.field, v)
	}
	if err != nil {
		return []reflect.Value{parent}, err
	}
	if v == nil {
		return nil, nil
	}
	return []reflect.Value{parent}, nil
}

func (c *capture) Generate(state generatorState, gen *codeGenerator) {
	directCapture := false
	switch c.node.(type) {
	case *strct, *reference, *literal:
		directCapture = true // Structs require direct capture, single-token nodes can use it as an optimization
	}
	state.capture = &generatedCapture{
		field:  c.field.StructField,
		direct: directCapture || gen.shouldUseDirectCaptureForType(c.field.Type),
	}

	gen.statement(`// capture ` + c.field.Name + ` from ` + c.String())
	if state.capture.direct {
		c.node.Generate(state, gen)
		return
	}
	gen.statement(`{`)
	gen.indent++
	gen.statement(`var ` + state.capture.sourceRef() + ` string`)
	c.node.Generate(state, gen)
	gen.captureTokens(state)
	gen.indent--
	gen.statement(`}`)
	gen.statement(``)
}

// <identifier> - named lexer token reference
type reference struct {
	typ        lexer.TokenType
	identifier string // Used for informational purposes.
}

func (r *reference) String() string   { return ebnf(r) }
func (r *reference) GoString() string { return fmt.Sprintf("reference{%s}", r.identifier) }

func (r *reference) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	token, err := ctx.Peek(0)
	if err != nil {
		return nil, err
	}
	if token.Type != r.typ {
		return nil, nil
	}
	_, _ = ctx.Next()
	return []reflect.Value{reflect.ValueOf(token.Value)}, nil
}

func (r *reference) Generate(state generatorState, gen *codeGenerator) {
	gen.statement(`// reference ` + r.String())
	gen.statement(fmt.Sprintf(`if current.Type != %d {`, r.typ))
	gen.handleMismatchIndent(state, 1)
	gen.statement(`}`)
	gen.processToken(state)
	gen.statement(``)
}

// [ <expr> ] <sequence>
type optional struct {
	node node
}

func (o *optional) String() string   { return ebnf(o) }
func (o *optional) GoString() string { return "optional{}" }

func (o *optional) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	branch := ctx.Branch()
	out, err = o.node.Parse(branch, parent)
	if err != nil {
		// Optional part failed to match.
		if ctx.Stop(err, branch) {
			return out, err
		}
	} else {
		ctx.Accept(branch)
	}
	if out == nil {
		out = []reflect.Value{}
	}
	return out, nil
}

func (o *optional) Generate(state generatorState, gen *codeGenerator) {
	// TODO remove this node type? Never used
}

// { <expr> } <sequence>
type repetition struct {
	node node
}

func (r *repetition) String() string   { return ebnf(r) }
func (r *repetition) GoString() string { return "repetition{}" }

// Parse a repetition. Once a repetition is encountered it will always match, so grammars
// should ensure that branches are differentiated prior to the repetition.
func (r *repetition) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	i := 0
	for ; i < MaxIterations; i++ {
		branch := ctx.Branch()
		v, err := r.node.Parse(branch, parent)
		out = append(out, v...)
		if err != nil {
			// Optional part failed to match.
			if ctx.Stop(err, branch) {
				return out, err
			}
			break
		} else {
			ctx.Accept(branch)
		}
		if v == nil {
			break
		}
	}
	if i >= MaxIterations {
		t, _ := ctx.Peek(0)
		panic(Errorf(t.Pos, "too many iterations of %s (> %d)", r, MaxIterations))
	}
	if out == nil {
		out = []reflect.Value{}
	}
	return out, nil
}

func (r *repetition) Generate(state generatorState, gen *codeGenerator) {
	// TODO remove this node type? Never used
}

// Match a token literal exactly "..."[:<type>].
type literal struct {
	s  string
	t  lexer.TokenType
	tt string // Used for display purposes - symbolic name of t.
}

func (l *literal) String() string   { return ebnf(l) }
func (l *literal) GoString() string { return fmt.Sprintf("literal{%q, %q}", l.s, l.tt) }

func (l *literal) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	token, err := ctx.Peek(0)
	if err != nil {
		return nil, err
	}
	var equal bool
	if ctx.caseInsensitive[token.Type] {
		equal = strings.EqualFold(token.Value, l.s)
	} else {
		equal = token.Value == l.s
	}
	if equal && (l.t == -1 || l.t == token.Type) {
		next, err := ctx.Next()
		if err != nil {
			return nil, err
		}
		return []reflect.Value{reflect.ValueOf(next.Value)}, nil
	}
	return nil, nil
}

func (l *literal) Generate(state generatorState, gen *codeGenerator) {
	// TODO case insensitive
	gen.statement(`// literal ` + l.String())
	typeMatch := ""
	if l.t != -1 {
		typeMatch = fmt.Sprintf(` && current.Type == %d`, l.t)
	}
	gen.statement(fmt.Sprintf(`if !(current.Value == %s%s) {`, strconv.Quote(l.s), typeMatch))
	gen.handleMismatchIndent(state, 1)
	gen.statement("}")
	gen.processToken(state)
	gen.statement(``)
}

type negation struct {
	node node
}

func (n *negation) String() string   { return ebnf(n) }
func (n *negation) GoString() string { return "negation{}" }

func (n *negation) Parse(ctx *parseContext, parent reflect.Value) (out []reflect.Value, err error) {
	// Create a branch to avoid advancing the parser, but call neither Stop nor Accept on it
	// since we will discard a match.
	branch := ctx.Branch()
	notEOF, err := ctx.Peek(0)
	if err != nil {
		return nil, err
	}
	if notEOF.EOF() {
		// EOF cannot match a negation, which expects something
		return nil, nil
	}

	out, err = n.node.Parse(branch, parent)

	if out != nil && err == nil {
		// out being non-nil means that what we don't want is actually here, so we report nomatch
		return nil, Errorf(notEOF.Pos, "unexpected '%s'", notEOF.Value)
	}

	// Just give the next token
	next, err := ctx.Next()
	if err != nil {
		return nil, err
	}
	return []reflect.Value{reflect.ValueOf(next.Value)}, nil
}

func (n *negation) Generate(state generatorState, gen *codeGenerator) {
	gen.statement(`// negation ` + n.String())
	childState := state
	childState.errorLabel = gen.newLabel("negation", "Error")
	gen.statement(`{`)
	gen.indent++

	gen.statement(`branchCheckpoint := p.Lex.MakeCheckpoint()`)
	n.node.Generate(childState, gen)
	// Matched if here, unwanted //TODO failUnexpectedWith?
	gen.handleMismatchIndent(state, 0)
	gen.writeLabel(childState.errorLabel)
	// Had an error if here, wanted
	gen.statement(`p.Lex.LoadCheckpoint(branchCheckpoint)`)
	gen.statement(`current = p.Lex.Current()`)
	gen.statement(`p.SetNoError(false)`) // TODO: Only if the error hasn't progressed beyond lookahead?
	gen.processToken(state)

	gen.indent--
	gen.statement(`}`)
	gen.statement(``)
}

// Attempt to transform values to given type.
//
// This will dereference pointers, and attempt to parse strings into integer values, floats, etc.
func conform(t reflect.Type, values []reflect.Value) (out []reflect.Value, err error) {
	for _, v := range values {
		for t != v.Type() && t.Kind() == reflect.Ptr && v.Kind() != reflect.Ptr {
			// This can occur during partial failure.
			if !v.CanAddr() {
				return
			}
			v = v.Addr()
		}

		// Already of the right kind, don't bother converting.
		if v.Kind() == t.Kind() {
			if v.Type() != t {
				v = v.Convert(t)
			}
			out = append(out, v)
			continue
		}

		kind := t.Kind()
		switch kind { // nolint: exhaustive
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			n, err := strconv.ParseInt(v.String(), 0, sizeOfKind(kind))
			if err != nil {
				return nil, fmt.Errorf("invalid integer %q: %s", v.String(), err)
			}
			v = reflect.New(t).Elem()
			v.SetInt(n)

		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			n, err := strconv.ParseUint(v.String(), 0, sizeOfKind(kind))
			if err != nil {
				return nil, fmt.Errorf("invalid integer %q: %s", v.String(), err)
			}
			v = reflect.New(t).Elem()
			v.SetUint(n)

		case reflect.Bool:
			v = reflect.ValueOf(true)

		case reflect.Float32, reflect.Float64:
			n, err := strconv.ParseFloat(v.String(), sizeOfKind(kind))
			if err != nil {
				return nil, fmt.Errorf("invalid float %q: %s", v.String(), err)
			}
			v = reflect.New(t).Elem()
			v.SetFloat(n)
		}

		out = append(out, v)
	}
	return out, nil
}

func sizeOfKind(kind reflect.Kind) int {
	switch kind { // nolint: exhaustive
	case reflect.Int8, reflect.Uint8:
		return 8
	case reflect.Int16, reflect.Uint16:
		return 16
	case reflect.Int32, reflect.Uint32, reflect.Float32:
		return 32
	case reflect.Int64, reflect.Uint64, reflect.Float64:
		return 64
	case reflect.Int, reflect.Uint:
		return strconv.IntSize
	}
	panic("unsupported kind " + kind.String())
}

// Set field.
//
// If field is a pointer the pointer will be set to the value. If field is a string, value will be
// appended. If field is a slice, value will be appended to slice.
//
// For all other types, an attempt will be made to convert the string to the corresponding
// type (int, float32, etc.).
func setField(tokens []lexer.Token, strct reflect.Value, field structLexerField, fieldValue []reflect.Value) (err error) { // nolint: gocognit
	defer decorate(&err, func() string { return strct.Type().Name() + "." + field.Name })

	f := strct.FieldByIndex(field.Index)

	// Any kind of pointer, hydrate it first.
	if f.Kind() == reflect.Ptr {
		if f.IsNil() {
			fv := reflect.New(f.Type().Elem()).Elem()
			f.Set(fv.Addr())
			f = fv
		} else {
			f = f.Elem()
		}
	}

	if f.Type() == tokenType {
		f.Set(reflect.ValueOf(tokens[0]))
		return nil
	}

	if f.Type() == tokensType {
		f.Set(reflect.ValueOf(tokens))
		return nil
	}

	if f.CanAddr() {
		if d, ok := f.Addr().Interface().(Capture); ok {
			ifv := make([]string, 0, len(fieldValue))
			for _, v := range fieldValue {
				ifv = append(ifv, v.Interface().(string))
			}
			return d.Capture(ifv)
		} else if d, ok := f.Addr().Interface().(encoding.TextUnmarshaler); ok {
			for _, v := range fieldValue {
				if err := d.UnmarshalText([]byte(v.Interface().(string))); err != nil {
					return err
				}
			}
			return nil
		}
	}

	if f.Kind() == reflect.Slice {
		sliceElemType := f.Type().Elem()
		if sliceElemType.Implements(captureType) || reflect.PtrTo(sliceElemType).Implements(captureType) {
			if sliceElemType.Kind() == reflect.Ptr {
				sliceElemType = sliceElemType.Elem()
			}
			for _, v := range fieldValue {
				d := reflect.New(sliceElemType).Interface().(Capture)
				if err := d.Capture([]string{v.Interface().(string)}); err != nil {
					return err
				}
				eltValue := reflect.ValueOf(d)
				if f.Type().Elem().Kind() != reflect.Ptr {
					eltValue = eltValue.Elem()
				}
				f.Set(reflect.Append(f, eltValue))
			}
		} else {
			fieldValue, err = conform(sliceElemType, fieldValue)
			if err != nil {
				return err
			}
			f.Set(reflect.Append(f, fieldValue...))
		}
		return nil
	}

	// Strings concatenate all captured tokens.
	if f.Kind() == reflect.String {
		fieldValue, err = conform(f.Type(), fieldValue)
		if err != nil {
			return err
		}
		if len(fieldValue) == 0 {
			return nil
		}
		accumulated := f.String()
		for _, v := range fieldValue {
			added := v.String()
			if accumulated != "" {
				accumulated += added
			} else {
				accumulated = added
			}
		}
		f.SetString(accumulated)
		return nil
	}

	// Coalesce multiple tokens into one. This allows eg. ["-", "10"] to be captured as separate tokens but
	// parsed as a single string "-10".
	if len(fieldValue) > 1 {
		out := []string{}
		for _, v := range fieldValue {
			out = append(out, v.String())
		}
		fieldValue = []reflect.Value{reflect.ValueOf(strings.Join(out, ""))}
	}

	fieldValue, err = conform(f.Type(), fieldValue)
	if err != nil {
		return err
	}

	fv := fieldValue[0]

	switch f.Kind() { // nolint: exhaustive
	// Numeric types will increment if the token can not be coerced.
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		if fv.Type() != f.Type() {
			f.SetInt(f.Int() + 1)
		} else {
			f.Set(fv)
		}
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		if fv.Type() != f.Type() {
			f.SetUint(f.Uint() + 1)
		} else {
			f.Set(fv)
		}

	case reflect.Float32, reflect.Float64:
		if fv.Type() != f.Type() {
			f.SetFloat(f.Float() + 1)
		} else {
			f.Set(fv)
		}

	case reflect.Bool, reflect.Struct:
		if f.Kind() == reflect.Bool && fv.Kind() == reflect.Bool {
			f.SetBool(fv.Bool())
			break
		}
		if fv.Type() != f.Type() {
			return fmt.Errorf("value %q is not correct type %s", fv, f.Type())
		}
		f.Set(fv)

	default:
		return fmt.Errorf("unsupported field type %s for field %s", f.Type(), field.Name)
	}
	return nil
}
