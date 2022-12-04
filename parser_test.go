package participle_test

import (
	"errors"
	"fmt"
	"math"
	"net"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"text/scanner"

	require "github.com/alecthomas/assert/v2"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

func TestProductionCapture(t *testing.T) {
	type testCapture struct {
		A string `@Test`
	}

	_, err := participle.Build[testCapture]()
	require.Error(t, err)
}

func TestTermCapture(t *testing.T) {
	type grammar struct {
		A string `@"."*`
	}

	parser := mustTestParser[grammar](t)

	expected := &grammar{"..."}

	actual, err := parser.ParseString("", "...")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestParseScalar(t *testing.T) {
	type grammar struct {
		A string `@"one"`
	}

	parser := mustTestParser[grammar](t)

	actual, err := parser.ParseString("", "one")
	require.NoError(t, err)
	require.Equal(t, &grammar{"one"}, actual)
}

func TestParseGroup(t *testing.T) {
	type grammar struct {
		A string `@("one" | "two")`
	}

	parser := mustTestParser[grammar](t)

	actual, err := parser.ParseString("", "one")
	require.NoError(t, err)
	require.Equal(t, &grammar{"one"}, actual)

	actual, err = parser.ParseString("", "two")
	require.NoError(t, err)
	require.Equal(t, &grammar{"two"}, actual)
}

func TestParseAlternative(t *testing.T) {
	type grammar struct {
		A string `@"one" |`
		B string `@"two"`
	}

	parser := mustTestParser[grammar](t)

	actual, err := parser.ParseString("", "one")
	require.NoError(t, err)
	require.Equal(t, &grammar{A: "one"}, actual)

	actual, err = parser.ParseString("", "two")
	require.NoError(t, err)
	require.Equal(t, &grammar{B: "two"}, actual)
}

func TestParseSequence(t *testing.T) {
	type grammar struct {
		A string `@"one"`
		B string `@"two"`
		C string `@"three"`
	}

	parser := mustTestParser[grammar](t)

	expected := &grammar{"one", "two", "three"}
	actual, err := parser.ParseString("", "one two three")
	require.NoError(t, err)
	require.Equal(t, expected, actual)

	expected = &grammar{}
	actual, err = parser.ParseString("", "moo")
	require.Error(t, err)
	require.Equal(t, expected, actual)
}

func TestNested(t *testing.T) {
	type nestedInner struct {
		B string `@"one"`
		C string `@"two"`
	}

	type testNested struct {
		A *nestedInner `@@`
	}

	parser := mustTestParser[testNested](t)

	expected := &testNested{A: &nestedInner{B: "one", C: "two"}}
	actual, err := parser.ParseString("", "one two")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestAccumulateNested(t *testing.T) {
	type nestedInner struct {
		B string `@"one"`
		C string `@"two"`
	}
	type testAccumulateNested struct {
		A []*nestedInner `@@+`
	}

	parser := mustTestParser[testAccumulateNested](t)

	expected := &testAccumulateNested{A: []*nestedInner{{B: "one", C: "two"}, {B: "one", C: "two"}}}
	actual, err := parser.ParseString("", "one two one two")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestRepetitionNoMatch(t *testing.T) {
	type grammar struct {
		A []string `@"."*`
	}
	parser := mustTestParser[grammar](t)

	expected := &grammar{}
	actual, err := parser.ParseString("", ``)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestRepetition(t *testing.T) {
	type grammar struct {
		A []string `@"."*`
	}
	parser := mustTestParser[grammar](t)

	expected := &grammar{A: []string{".", ".", "."}}
	actual, err := parser.ParseString("", `...`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestRepetitionAcrossFields(t *testing.T) {
	type testRepetition struct {
		A []string `@"."*`
		B *string  `(@"b" |`
		C *string  ` @"c")`
	}

	parser := mustTestParser[testRepetition](t)

	b := "b"
	c := "c"

	expected := &testRepetition{
		A: []string{".", ".", "."},
		B: &b,
	}
	actual, err := parser.ParseString("", "...b")
	require.NoError(t, err)
	require.Equal(t, expected, actual)

	expected = &testRepetition{
		A: []string{".", ".", "."},
		C: &c,
	}
	actual, err = parser.ParseString("", "...c")
	require.NoError(t, err)
	require.Equal(t, expected, actual)

	expected = &testRepetition{
		B: &b,
	}
	actual, err = parser.ParseString("", "b")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestAccumulateString(t *testing.T) {
	type testAccumulateString struct {
		A string `@"."+`
	}

	parser := mustTestParser[testAccumulateString](t)

	expected := &testAccumulateString{
		A: "...",
	}
	actual, err := parser.ParseString("", "...")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

type Group struct {
	Expression *Expression `"(" @@ ")"`
}

type LookaheadGroup struct {
	Expression *Expression `"(" "?" ("=" | "!") @@ ")"`
}

type EBNFOption struct {
	Expression *Expression `"[" @@ "]"`
}

type Repetition struct {
	Expression *Expression `"{" @@ "}"`
}

type Negation struct {
	Expression *Expression `"!" @@`
}

type Literal struct {
	Start string `@String`
}

type Range struct {
	Start string `@String`
	End   string `"…" @String`
}

type Term struct {
	Name           string          `@Ident |`
	Literal        *Literal        `@@ |`
	Range          *Range          `@@ |`
	Group          *Group          `@@ |`
	LookaheadGroup *LookaheadGroup `@@ |`
	Option         *EBNFOption     `@@ |`
	Repetition     *Repetition     `@@ |`
	Negation       *Negation       `@@`
}

type Sequence struct {
	Terms []*Term `@@+`
}

type Expression struct {
	Alternatives []*Sequence `@@ ( "|" @@ )*`
}

type Production struct {
	Name       string        `@Ident "="`
	Expression []*Expression `@@+ "."`
}

type EBNF struct {
	Productions []*Production `@@*`
}

func TestEBNFParser(t *testing.T) {
	parser := mustTestParser[EBNF](t, participle.Unquote())

	expected := &EBNF{
		Productions: []*Production{
			{
				Name: "Production",
				Expression: []*Expression{
					{
						Alternatives: []*Sequence{
							{
								Terms: []*Term{
									{Name: "name"},
									{Literal: &Literal{Start: "="}},
									{
										Option: &EBNFOption{
											Expression: &Expression{
												Alternatives: []*Sequence{
													{
														Terms: []*Term{
															{Name: "Expression"},
														},
													},
												},
											},
										},
									},
									{Literal: &Literal{Start: "."}},
								},
							},
						},
					},
				},
			},
			{
				Name: "Expression",
				Expression: []*Expression{
					{
						Alternatives: []*Sequence{
							{
								Terms: []*Term{
									{Name: "Alternative"},
									{
										Repetition: &Repetition{
											Expression: &Expression{
												Alternatives: []*Sequence{
													{
														Terms: []*Term{
															{Literal: &Literal{Start: "|"}},
															{Name: "Alternative"},
														},
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
			{
				Name: "Alternative",
				Expression: []*Expression{
					{
						Alternatives: []*Sequence{
							{
								Terms: []*Term{
									{Name: "Term"},
									{
										Repetition: &Repetition{
											Expression: &Expression{
												Alternatives: []*Sequence{
													{
														Terms: []*Term{
															{Name: "Term"},
														},
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
			{
				Name: "Term",
				Expression: []*Expression{
					{
						Alternatives: []*Sequence{
							{Terms: []*Term{{Name: "name"}}},
							{
								Terms: []*Term{
									{Name: "token"},
									{
										Option: &EBNFOption{
											Expression: &Expression{
												Alternatives: []*Sequence{
													{
														Terms: []*Term{
															{Literal: &Literal{Start: "…"}},
															{Name: "token"},
														},
													},
												},
											},
										},
									},
								},
							},
							{Terms: []*Term{{Literal: &Literal{Start: "@@"}}}},
							{Terms: []*Term{{Name: "Group"}}},
							{Terms: []*Term{{Name: "EBNFOption"}}},
							{Terms: []*Term{{Name: "Repetition"}}},
						},
					},
				},
			},
			{
				Name: "Group",
				Expression: []*Expression{
					{
						Alternatives: []*Sequence{
							{
								Terms: []*Term{
									{Literal: &Literal{Start: "("}},
									{Name: "Expression"},
									{Literal: &Literal{Start: ")"}},
								},
							},
						},
					},
				},
			},
			{
				Name: "EBNFOption",
				Expression: []*Expression{
					{
						Alternatives: []*Sequence{
							{
								Terms: []*Term{
									{Literal: &Literal{Start: "["}},
									{Name: "Expression"},
									{Literal: &Literal{Start: "]"}},
								},
							},
						},
					},
				},
			},
			{
				Name: "Repetition",
				Expression: []*Expression{
					{
						Alternatives: []*Sequence{
							{
								Terms: []*Term{
									{Literal: &Literal{Start: "{"}},
									{Name: "Expression"},
									{Literal: &Literal{Start: "}"}},
								},
							},
						},
					},
				},
			},
		},
	}
	actual, err := parser.ParseString("", strings.TrimSpace(`
Production  = name "=" [ Expression ] "." .
Expression  = Alternative { "|" Alternative } .
Alternative = Term { Term } .
Term        = name | token [ "…" token ] | "@@" | Group | EBNFOption | Repetition .
Group       = "(" Expression ")" .
EBNFOption      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
`))
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestParseExpression(t *testing.T) {
	type testNestA struct {
		A string `":" @"a"*`
	}
	type testNestB struct {
		B string `";" @"b"*`
	}
	type testExpression struct {
		A *testNestA `@@ |`
		B *testNestB `@@`
	}

	parser := mustTestParser[testExpression](t)

	expected := &testExpression{
		B: &testNestB{
			B: "b",
		},
	}
	actual, err := parser.ParseString("", ";b")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestParseOptional(t *testing.T) {
	type testOptional struct {
		A string `( @"a" @"b" )?`
		B string `@"c"`
	}

	parser := mustTestParser[testOptional](t)

	expected := &testOptional{B: "c"}
	actual, err := parser.ParseString("", `c`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestHello(t *testing.T) {
	type testHello struct {
		Hello string `@"hello"`
		To    string `@String`
	}

	parser := mustTestParser[testHello](t, participle.Unquote())

	expected := &testHello{"hello", `Bobby Brown`}
	actual, err := parser.ParseString("", `hello "Bobby Brown"`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func mustTestParser[G any](t *testing.T, options ...participle.Option) *participle.Parser[G] {
	t.Helper()
	parser, err := participle.Build[G](options...)
	require.NoError(t, err)
	return parser
}

func BenchmarkEBNFParser(b *testing.B) {
	parser, err := participle.Build[EBNF]()
	require.NoError(b, err)
	b.ResetTimer()
	source := strings.TrimSpace(`
Production  = name "=" [ Expression ] "." .
Expression  = Alternative { "|" Alternative } .
Alternative = Term { Term } .
Term        = name | token [ "…" token ] | "@@" | Group | EBNFOption | Repetition .
Group       = "(" Expression ")" .
EBNFOption      = "[" Expression "]" .
Repetition  = "{" Expression "}" .

`)
	for i := 0; i < b.N; i++ {
		_, _ = parser.ParseString("", source)
	}
}

func TestRepeatAcrossFields(t *testing.T) {
	type grammar struct {
		A string `( @("." ">") |`
		B string `  @("," "<") )*`
	}

	parser := mustTestParser[grammar](t)

	expected := &grammar{A: ".>.>.>.>", B: ",<,<,<"}

	actual, err := parser.ParseString("", ".>,<.>.>,<.>,<")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestPosInjection(t *testing.T) {
	type subgrammar struct {
		Pos    lexer.Position
		B      string `@","*`
		EndPos lexer.Position
	}
	type grammar struct {
		Pos    lexer.Position
		A      string      `@"."*`
		B      *subgrammar `@@`
		C      string      `@"."`
		EndPos lexer.Position
	}

	parser := mustTestParser[grammar](t)

	expected := &grammar{
		Pos: lexer.Position{
			Offset: 3,
			Line:   1,
			Column: 4,
		},
		A: "...",
		B: &subgrammar{
			B: ",,,",
			Pos: lexer.Position{
				Offset: 6,
				Line:   1,
				Column: 7,
			},
			EndPos: lexer.Position{
				Offset: 9,
				Line:   1,
				Column: 10,
			},
		},
		C: ".",
		EndPos: lexer.Position{
			Offset: 10,
			Line:   1,
			Column: 11,
		},
	}

	actual, err := parser.ParseString("", "   ...,,,.")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

type parseableCount int

func (c *parseableCount) Capture(values []string) error {
	*c += parseableCount(len(values))
	return nil
}

func TestCaptureInterface(t *testing.T) {
	type grammar struct {
		Count parseableCount `@"a"*`
	}

	parser := mustTestParser[grammar](t)
	expected := &grammar{Count: 3}
	actual, err := parser.ParseString("", "a a a")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

type unmarshallableCount int

func (u *unmarshallableCount) UnmarshalText(text []byte) error {
	*u += unmarshallableCount(len(text))
	return nil
}

func TestTextUnmarshalerInterface(t *testing.T) {
	type grammar struct {
		Count unmarshallableCount `{ @"a" }`
	}

	parser := mustTestParser[grammar](t)
	expected := &grammar{Count: 3}
	actual, err := parser.ParseString("", "a a a")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestLiteralTypeConstraint(t *testing.T) {
	type grammar struct {
		Literal string `@"123456":String`
	}

	parser := mustTestParser[grammar](t, participle.Unquote())

	expected := &grammar{Literal: "123456"}
	actual, err := parser.ParseString("", `"123456"`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)

	_, err = parser.ParseString("", `123456`)
	require.Error(t, err)
}

type nestedCapture struct {
	Tokens []string
}

func (n *nestedCapture) Capture(tokens []string) error {
	n.Tokens = tokens
	return nil
}

func TestStructCaptureInterface(t *testing.T) {
	type grammar struct {
		Capture *nestedCapture `@String`
	}

	parser, err := participle.Build[grammar](participle.Unquote())
	require.NoError(t, err)

	expected := &grammar{Capture: &nestedCapture{Tokens: []string{"hello"}}}
	actual, err := parser.ParseString("", `"hello"`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

type parseableStruct struct {
	Tokens []string
}

func (p *parseableStruct) Parse(lex *lexer.PeekingLexer) error {
	for {
		tok := lex.Next()
		if tok.EOF() {
			break
		}
		p.Tokens = append(p.Tokens, tok.Value)
	}
	return nil
}

func TestParseable(t *testing.T) {
	type grammar struct {
		Inner *parseableStruct `@@`
	}

	parser, err := participle.Build[grammar](participle.Unquote())
	require.NoError(t, err)

	expected := &grammar{Inner: &parseableStruct{Tokens: []string{"hello", "123", "world"}}}
	actual, err := parser.ParseString("", `hello 123 "world"`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestStringConcat(t *testing.T) {
	type grammar struct {
		Field string `@"."+`
	}

	parser, err := participle.Build[grammar]()
	require.NoError(t, err)

	expected := &grammar{"...."}
	actual, err := parser.ParseString("", `. . . .`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestParseIntSlice(t *testing.T) {
	type grammar struct {
		Field []int `@Int+`
	}

	parser := mustTestParser[grammar](t)

	expected := &grammar{[]int{1, 2, 3, 4}}
	actual, err := parser.ParseString("", `1 2 3 4`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestEmptyStructErrorsNotPanicsIssue21(t *testing.T) {
	type grammar struct {
		Foo struct{} `@@`
	}
	_, err := participle.Build[grammar]()
	require.Error(t, err)
}

func TestMultipleTokensIntoScalar(t *testing.T) {
	type grammar struct {
		Field int `@("-"? Int)`
	}
	p, err := participle.Build[grammar]()
	require.NoError(t, err)
	actual, err := p.ParseString("", `- 10`)
	require.NoError(t, err)
	require.Equal(t, -10, actual.Field)

	_, err = p.ParseString("", `x`)
	require.EqualError(t, err, `1:1: unexpected token "x" (expected <int>)`)
	_, err = p.ParseString("", ` `)
	require.EqualError(t, err, `1:2: unexpected token "<EOF>" (expected <int>)`)
}

type posMixin struct {
	Pos lexer.Position
}

func TestMixinPosIsPopulated(t *testing.T) {
	type grammar struct {
		posMixin

		Int int `@Int`
	}

	p := mustTestParser[grammar](t)
	actual, err := p.ParseString("", "10")
	require.NoError(t, err)
	require.Equal(t, 10, actual.Int)
	require.Equal(t, 1, actual.Pos.Column)
	require.Equal(t, 1, actual.Pos.Line)
}

type testParserMixin struct {
	A string `@Ident`
	B string `@Ident`
}

func TestMixinFieldsAreParsed(t *testing.T) {
	type grammar struct {
		testParserMixin
		C string `@Ident`
	}
	p := mustTestParser[grammar](t)
	actual, err := p.ParseString("", "one two three")
	require.NoError(t, err)
	require.Equal(t, "one", actual.A)
	require.Equal(t, "two", actual.B)
	require.Equal(t, "three", actual.C)
}

func TestNestedOptional(t *testing.T) {
	type grammar struct {
		Args []string `"(" [ @Ident ( "," @Ident )* ] ")"`
	}
	p := mustTestParser[grammar](t)
	_, err := p.ParseString("", `()`)
	require.NoError(t, err)
	_, err = p.ParseString("", `(a)`)
	require.NoError(t, err)
	_, err = p.ParseString("", `(a, b, c)`)
	require.NoError(t, err)
	_, err = p.ParseString("", `(1)`)
	require.Error(t, err)
}

func TestInvalidNumbers(t *testing.T) {
	type grammar struct {
		Int8    int8    `  "int8" @Int`
		Int16   int16   `| "int16" @Int`
		Int32   int32   `| "int32" @Int`
		Int64   int64   `| "int64" @Int`
		Uint8   uint8   `| "uint8" @Int`
		Uint16  uint16  `| "uint16" @Int`
		Uint32  uint32  `| "uint32" @Int`
		Uint64  uint64  `| "uint64" @Int`
		Float32 float32 `| "float32" @Float`
		Float64 float64 `| "float64" @Float`
	}

	p := mustTestParser[grammar](t)

	tests := []struct {
		name     string
		input    string
		expected *grammar
		err      bool
	}{
		{name: "ValidInt8", input: "int8 127", expected: &grammar{Int8: 127}},
		{name: "InvalidInt8", input: "int8 129", err: true},
		{name: "ValidInt16", input: "int16 32767", expected: &grammar{Int16: 32767}},
		{name: "InvalidInt16", input: "int16 32768", err: true},
		{name: "ValidInt32", input: fmt.Sprintf("int32 %d", math.MaxInt32), expected: &grammar{Int32: math.MaxInt32}},
		{name: "InvalidInt32", input: fmt.Sprintf("int32 %d", int64(math.MaxInt32+1)), err: true},
		{name: "ValidInt64", input: fmt.Sprintf("int64 %d", int64(math.MaxInt64)), expected: &grammar{Int64: math.MaxInt64}},
		{name: "InvalidInt64", input: "int64 9223372036854775808", err: true},
		{name: "ValidFloat64", input: "float64 1234.5", expected: &grammar{Float64: 1234.5}},
		{name: "InvalidFloat64", input: "float64 asdf", err: true},
	}
	for _, test := range tests {
		// nolint: scopelint
		t.Run(test.name, func(t *testing.T) {
			actual, err := p.ParseString("", test.input)
			if test.err {
				require.Error(t, err, fmt.Sprintf("%#v", actual))
			} else {
				require.NoError(t, err)
				require.Equal(t, test.expected, actual)
			}
		})
	}
}

// We'd like this to work, but it can wait.

func TestPartialAST(t *testing.T) {
	type grammar struct {
		Succeed string `@Ident`
		Fail    string `@"foo"`
	}
	p := mustTestParser[grammar](t)
	actual, err := p.ParseString("", `foo bar`)
	require.Error(t, err)
	expected := &grammar{Succeed: "foo"}
	require.Equal(t, expected, actual)
}

func TestCaseInsensitive(t *testing.T) {
	type grammar struct {
		Select string `"select":Keyword @Ident`
	}

	// lex := lexer.MustStateful(lexer.Regexp(
	// 	`(?i)(?P<Keyword>SELECT)` +
	// 		`|(?P<Ident>\w+)` +
	// 		`|(\s+)`,
	// ))
	lex := lexer.MustSimple([]lexer.SimpleRule{
		{"Keyword", `(?i)SELECT`},
		{"Ident", `\w+`},
		{"whitespace", `\s+`},
	})

	p := mustTestParser[grammar](t, participle.Lexer(lex), participle.CaseInsensitive("Keyword"))
	actual, err := p.ParseString("", `SELECT foo`)
	expected := &grammar{"foo"}
	require.NoError(t, err)
	require.Equal(t, expected, actual)

	actual, err = p.ParseString("", `select foo`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

func TestTokenAfterRepeatErrors(t *testing.T) {
	type grammar struct {
		Text string `@Ident* "foo"`
	}
	p := mustTestParser[grammar](t)
	_, err := p.ParseString("", ``)
	require.Error(t, err)
}

func TestEOFAfterRepeat(t *testing.T) {
	type grammar struct {
		Text string `@Ident*`
	}
	p := mustTestParser[grammar](t)
	_, err := p.ParseString("", ``)
	require.NoError(t, err)
}

func TestTrailing(t *testing.T) {
	type grammar struct {
		Text string `@Ident`
	}
	p := mustTestParser[grammar](t)
	_, err := p.ParseString("", `foo bar`)
	require.Error(t, err)
}

type modifierTest[G any] struct {
	name     string
	input    string
	expected string
	fail     bool
}

func (test modifierTest[G]) test(t *testing.T) {
	t.Helper()
	t.Run(test.name, func(t *testing.T) {
		p := mustTestParser[G](t)
		grammar, err := p.ParseString("", test.input)
		if test.fail {
			require.Error(t, err)
		} else {
			require.NoError(t, err)
			actual := reflect.ValueOf(grammar).Elem().FieldByName("A").String()
			require.Equal(t, test.expected, actual)
		}
	})
}

func TestModifiers(t *testing.T) {
	type nonEmptyGrammar struct {
		A string `@( ("x"? "y"? "z"?)! "b" )`
	}
	tests := []interface{ test(t *testing.T) }{
		modifierTest[nonEmptyGrammar]{name: "NonMatchingOptionalNonEmpty",
			input: "b",
			fail:  true,
		},
		modifierTest[nonEmptyGrammar]{name: "NonEmptyMatch",
			input:    "x b",
			expected: "xb",
		},
		modifierTest[nonEmptyGrammar]{name: "NonEmptyMatchAll",
			input:    "x y z b",
			expected: "xyzb",
		},
		modifierTest[nonEmptyGrammar]{name: "NonEmptyMatchSome",
			input:    "x z b",
			expected: "xzb",
		},
		modifierTest[struct {
			A string `@( "a"? "b" )`
		}]{name: "MatchingOptional",
			input:    "a b",
			expected: "ab",
		},
		modifierTest[struct {
			A string `@( "a"? "b" )`
		}]{name: "NonMatchingOptionalIsSkipped",
			input:    "b",
			expected: "b",
		},
		modifierTest[struct {
			A string `@( "a"+ )`
		}]{name: "MatchingOneOrMore",
			input:    "a a a a a",
			expected: "aaaaa",
		},
		modifierTest[struct {
			A string `@( "a"+ )`
		}]{name: "NonMatchingOneOrMore",
			input: "",
			fail:  true,
		},
		modifierTest[struct {
			A string `@( "a"* )`
		}]{name: "MatchingZeroOrMore",
			input: "aaaaaaa",
			fail:  true,
		},
		modifierTest[struct {
			A string `@( "a"* )`
		}]{name: "NonMatchingZeroOrMore",
			input: "",
		},
	}
	for _, test := range tests {
		test.test(t)
	}
}

func TestNonEmptyMatchWithOptionalGroup(t *testing.T) {
	type term struct {
		Minus bool   `@'-'?`
		Name  string `@Ident`
	}
	type grammar struct {
		Start term `parser:"'[' (@@?"`
		End   term `parser:"     (':' @@)?)! ']'"`
	}

	p := mustTestParser[grammar](t)

	result, err := p.ParseString("", "[-x]")
	require.NoError(t, err)
	require.Equal(t, &grammar{Start: term{Minus: true, Name: "x"}}, result)

	result, err = p.ParseString("", "[a:-b]")
	require.NoError(t, err)
	require.Equal(t, &grammar{Start: term{Name: "a"}, End: term{Minus: true, Name: "b"}}, result)

	result, err = p.ParseString("", "[:end]")
	require.NoError(t, err)
	require.Equal(t, &grammar{End: term{Name: "end"}}, result)

	_, err = p.ParseString("", "[]")
	require.EqualError(t, err, `1:2: sub-expression (Term? (":" Term)?)! cannot be empty`)
}

func TestIssue60(t *testing.T) {
	type grammar struct {
		A string `@("one" | | "two")`
	}
	_, err := participle.Build[grammar]()
	require.Error(t, err)
}

type Issue62Bar struct {
	A int
}

func (x *Issue62Bar) Parse(lex *lexer.PeekingLexer) error {
	token := lex.Next()
	var err error
	x.A, err = strconv.Atoi(token.Value)
	return err
}

type Issue62Foo struct {
	Bars []Issue62Bar `parser:"@@+"`
}

func TestIssue62(t *testing.T) {
	_, err := participle.Build[Issue62Foo]()
	require.NoError(t, err)
}

// nolint: structcheck, unused
func TestIssue71(t *testing.T) {
	type Sub struct {
		name string `@Ident`
	}
	type grammar struct {
		pattern *Sub `@@`
	}

	_, err := participle.Build[grammar]()
	require.Error(t, err)
}

func TestAllowTrailing(t *testing.T) {
	type G struct {
		Name string `@Ident`
	}

	p, err := participle.Build[G]()
	require.NoError(t, err)

	_, err = p.ParseString("", `hello world`)
	require.Error(t, err)
	g, err := p.ParseString("", `hello world`, participle.AllowTrailing(true))
	require.NoError(t, err)
	require.Equal(t, &G{"hello"}, g)
}

func TestDisjunctionErrorReporting(t *testing.T) {
	type statement struct {
		Add    bool `  @"add"`
		Remove bool `| @"remove"`
	}
	type grammar struct {
		Statements []*statement `"{" ( @@ )* "}"`
	}
	p := mustTestParser[grammar](t)
	_, err := p.ParseString("", `{ add foo }`)
	// TODO: This should produce a more useful error. This is returned by sequence.Parse().
	require.EqualError(t, err, `1:7: unexpected token "foo" (expected "}")`)
}

func TestCustomInt(t *testing.T) {
	type MyInt int
	type G struct {
		Value MyInt `@Int`
	}

	p, err := participle.Build[G]()
	require.NoError(t, err)

	g, err := p.ParseString("", `42`)
	require.NoError(t, err)
	require.Equal(t, &G{42}, g)
}

func TestBoolIfSet(t *testing.T) {
	type G struct {
		Value bool `@"true"?`
	}

	p, err := participle.Build[G]()
	require.NoError(t, err)

	g, err := p.ParseString("", `true`)
	require.NoError(t, err)
	require.Equal(t, &G{true}, g)
	g, err = p.ParseString("", ``)
	require.NoError(t, err)
	require.Equal(t, &G{false}, g)
}

func TestCustomBoolIfSet(t *testing.T) {
	type MyBool bool
	type G struct {
		Value MyBool `@"true"?`
	}

	p, err := participle.Build[G]()
	require.NoError(t, err)

	g, err := p.ParseString("", `true`)
	require.NoError(t, err)
	require.Equal(t, &G{true}, g)
	g, err = p.ParseString("", ``)
	require.NoError(t, err)
	require.Equal(t, &G{false}, g)
}

func TestPointerToList(t *testing.T) {
	type grammar struct {
		List *[]string `@Ident*`
	}
	p := mustTestParser[grammar](t)
	ast, err := p.ParseString("", `foo bar`)
	require.NoError(t, err)
	l := []string{"foo", "bar"}
	require.Equal(t, &grammar{List: &l}, ast)
}

// I'm not sure if this is a problem that should be solved like this.

// func TestMatchHydratesNullFields(t *testing.T) {
// 	type grammar struct {
// 		List []string `"{" @Ident* "}"`
// 	}
// 	p := mustTestParser[grammar](t)
// 	ast := &grammar{}
// 	err := p.ParseString(`{}`, ast)
// 	require.NoError(t, err)
// 	require.NotNil(t, ast.List)
// }

func TestNegation(t *testing.T) {
	type grammar struct {
		EverythingUntilSemicolon *[]string `@!';'* @';'`
	}
	p := mustTestParser[grammar](t)
	ast, err := p.ParseString("", `hello world ;`)
	require.NoError(t, err)
	require.Equal(t, &[]string{"hello", "world", ";"}, ast.EverythingUntilSemicolon)

	_, err = p.ParseString("", `hello world`)
	require.Error(t, err)
}

func TestNegationWithPattern(t *testing.T) {
	type grammar struct {
		EverythingMoreComplex *[]string `@!(';' String)* @';' @String`
	}

	p := mustTestParser[grammar](t, participle.Unquote())
	// j, err := json.MarshalIndent(p.root, "", "  ")
	// log.Print(j)
	// log.Print(ebnf(p.root))
	ast, err := p.ParseString("", `hello world ; "some-str"`)
	require.NoError(t, err)
	require.Equal(t, &[]string{"hello", "world", ";", `some-str`}, ast.EverythingMoreComplex)

	ast, err = p.ParseString("", `hello ; world ; "hey"`)
	require.NoError(t, err)
	require.Equal(t, &[]string{"hello", ";", "world", ";", `hey`}, ast.EverythingMoreComplex)

	_, err = p.ParseString("", `hello ; world ;`)
	require.Error(t, err)
}

func TestNegationWithDisjunction(t *testing.T) {
	type grammar struct {
		EverythingMoreComplex *[]string `@!(';' | ',')* @(';' | ',')`
	}

	// Note: we need more lookahead since (';' String) needs some before failing to match
	p := mustTestParser[grammar](t)
	ast, err := p.ParseString("", `hello world ;`)
	require.NoError(t, err)
	require.Equal(t, &[]string{"hello", "world", ";"}, ast.EverythingMoreComplex)

	ast, err = p.ParseString("", `hello world , `)
	require.NoError(t, err)
	require.Equal(t, &[]string{"hello", "world", ","}, ast.EverythingMoreComplex)
}

func TestNegationLookaheadError(t *testing.T) {
	type grammar struct {
		Stuff []string `@Ident @!('.' | '#') @Ident`
	}
	p := mustTestParser[grammar](t)

	ast, err := p.ParseString("", `hello, world`)
	require.NoError(t, err)
	require.Equal(t, []string{"hello", ",", "world"}, ast.Stuff)

	_, err = p.ParseString("", `hello . world`)
	require.EqualError(t, err, `1:7: unexpected token "."`)
}

func TestLookaheadGroup_Positive_SingleToken(t *testing.T) {
	type val struct {
		Str string `  @String`
		Int int    `| @Int`
	}
	type op struct {
		Op      string `@('+' | '*' (?= @Int))`
		Operand val    `@@`
	}
	type sum struct {
		Left val  `@@`
		Ops  []op `@@*`
	}
	p := mustTestParser[sum](t)

	ast, err := p.ParseString("", `"x" + "y" + 4`)
	require.NoError(t, err)
	require.Equal(t, &sum{Left: val{Str: `"x"`}, Ops: []op{{"+", val{Str: `"y"`}}, {"+", val{Int: 4}}}}, ast)

	ast, err = p.ParseString("", `"a" * 4 + "b"`)
	require.NoError(t, err)
	require.Equal(t, &sum{Left: val{Str: `"a"`}, Ops: []op{{"*", val{Int: 4}}, {"+", val{Str: `"b"`}}}}, ast)

	ast, err = p.ParseString("", `1 * 2 * 3`)
	require.NoError(t, err)
	require.Equal(t, &sum{Left: val{Int: 1}, Ops: []op{{"*", val{Int: 2}}, {"*", val{Int: 3}}}}, ast)

	_, err = p.ParseString("", `"a" * 'x' + "b"`)
	require.EqualError(t, err, `1:7: unexpected token "'x'"`)
	_, err = p.ParseString("", `4 * 2 + 0 * "b"`)
	require.EqualError(t, err, `1:13: unexpected token "\"b\""`)
}

func TestLookaheadGroup_Negative_SingleToken(t *testing.T) {
	type variable struct {
		Name string `@Ident`
	}
	type grammar struct {
		Identifiers []variable `((?! 'except'|'end') @@)*`
		Except      *variable  `('except' @@)? 'end'`
	}
	p := mustTestParser[grammar](t)

	ast, err := p.ParseString("", `one two three exception end`)
	require.NoError(t, err)
	require.Equal(t, []variable{{"one"}, {"two"}, {"three"}, {"exception"}}, ast.Identifiers)
	require.Zero(t, ast.Except)

	ast, err = p.ParseString("", `anything except this end`)
	require.NoError(t, err)
	require.Equal(t, []variable{{"anything"}}, ast.Identifiers)
	require.Equal(t, &variable{"this"}, ast.Except)

	ast, err = p.ParseString("", `except the end`)
	require.NoError(t, err)
	require.Zero(t, ast.Identifiers)
	require.Equal(t, &variable{"the"}, ast.Except)

	_, err = p.ParseString("", `no ending`)
	require.EqualError(t, err, `1:10: unexpected token "<EOF>" (expected "end")`)

	_, err = p.ParseString("", `no end in sight`)
	require.EqualError(t, err, `1:8: unexpected token "in"`)
}

func TestLookaheadGroup_Negative_MultipleTokens(t *testing.T) {
	type grammar struct {
		Parts []string `((?! '.' '.' '.') @(Ident | '.'))*`
	}
	p := mustTestParser[grammar](t)

	ast, err := p.ParseString("", `x.y.z.`)
	require.NoError(t, err)
	require.Equal(t, []string{"x", ".", "y", ".", "z", "."}, ast.Parts)

	ast, err = p.ParseString("", `..x..`)
	require.NoError(t, err)
	require.Equal(t, []string{".", ".", "x", ".", "."}, ast.Parts)

	ast, err = p.ParseString("", `two.. are fine`)
	require.NoError(t, err)
	require.Equal(t, []string{"two", ".", ".", "are", "fine"}, ast.Parts)

	_, err = p.ParseString("", `but this... is just wrong`)
	require.EqualError(t, err, `1:9: unexpected token "."`)
}

func TestASTTokens(t *testing.T) {
	type subject struct {
		Tokens []lexer.Token

		Word string `@Ident`
	}

	type hello struct {
		Tokens []lexer.Token

		Subject subject `"hello" @@`
	}

	p := mustTestParser[hello](t,
		participle.Elide("Whitespace"),
		participle.Lexer(lexer.MustSimple([]lexer.SimpleRule{
			{"Ident", `\w+`},
			{"Whitespace", `\s+`},
		})))
	actual, err := p.ParseString("", "hello world")
	require.NoError(t, err)
	tokens := []lexer.Token{
		{-2, "hello", lexer.Position{Line: 1, Column: 1}},
		{-3, " ", lexer.Position{Offset: 5, Line: 1, Column: 6}},
		{-2, "world", lexer.Position{Offset: 6, Line: 1, Column: 7}},
	}
	expected := &hello{
		Tokens: tokens,
		Subject: subject{
			Tokens: tokens[1:],
			Word:   "world",
		},
	}
	require.Equal(t, expected, actual)
}

func TestCaptureIntoToken(t *testing.T) {
	type ast struct {
		Head lexer.Token   `@Ident`
		Tail []lexer.Token `@(Ident*)`
	}

	p := mustTestParser[ast](t)
	actual, err := p.ParseString("", "hello waz baz")
	require.NoError(t, err)
	expected := &ast{
		Head: lexer.Token{-2, "hello", lexer.Position{Line: 1, Column: 1}},
		Tail: []lexer.Token{
			{-2, "waz", lexer.Position{Offset: 6, Line: 1, Column: 7}},
			{-2, "baz", lexer.Position{Offset: 10, Line: 1, Column: 11}},
		},
	}
	require.Equal(t, expected, actual)
}

func TestEndPos(t *testing.T) {
	type Ident struct {
		Pos    lexer.Position
		EndPos lexer.Position
		Text   string `parser:"@Ident"`
	}

	type AST struct {
		First  *Ident `parser:"@@"`
		Second *Ident `parser:"@@"`
	}

	var (
		Lexer = lexer.Must(lexer.New(lexer.Rules{
			"Root": {
				{"Ident", `[\w:]+`, nil},
				{"Whitespace", `[\r\t ]+`, nil},
			},
		}))

		Parser = participle.MustBuild[AST](
			participle.Lexer(Lexer),
			participle.Elide("Whitespace"),
		)
	)

	mod, err := Parser.Parse("", strings.NewReader("foo bar"))
	require.NoError(t, err)
	require.Equal(t, 0, mod.First.Pos.Offset)
	require.Equal(t, 3, mod.First.EndPos.Offset)
}

func TestBug(t *testing.T) {
	type A struct {
		Shared string `parser:"@'1'"`
		Diff   string `parser:"@A"`
	}
	type B struct {
		Shared string `parser:"@'1'"`
		Diff   string `parser:"@B"`
	}
	type AST struct {
		Branch string `parser:"@'branch'"`
		A      *A     `parser:"( @@"`
		B      *B     `parser:"| @@ )"`
	}
	var (
		lexer = lexer.Must(lexer.New(lexer.Rules{
			"Root": {
				{"A", `@`, nil},
				{"B", `!`, nil},
				{"Ident", `[\w:]+`, nil},
				{"Whitespace", `[\r\t ]+`, nil},
			},
		}))
		parser = participle.MustBuild[AST](
			participle.Lexer(lexer),
			participle.Elide("Whitespace"),
		)
	)
	expected := &AST{
		Branch: "branch",
		B: &B{
			Shared: "1",
			Diff:   "!",
		},
	}
	actual, err := parser.Parse("name", strings.NewReader(`branch 1!`))
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

type sliceCapture string

func (c *sliceCapture) Capture(values []string) error {
	*c = sliceCapture(strings.ToUpper(values[0]))
	return nil
}

func TestCaptureOnSliceElements(t *testing.T) { // nolint:dupl
	type capture struct {
		Single   *sliceCapture   `@Capture`
		Slice    []sliceCapture  `@Capture @Capture`
		SlicePtr []*sliceCapture `@Capture @Capture`
	}

	parser := participle.MustBuild[capture]([]participle.Option{
		participle.Lexer(lexer.MustSimple([]lexer.SimpleRule{
			{Name: "Capture", Pattern: `[a-z]{3}`},
			{Name: "Whitespace", Pattern: `[\s|\n]+`},
		})),
		participle.Elide("Whitespace"),
	}...)

	captured, err := parser.ParseString("capture_slice", `abc def ijk lmn opq`)
	require.NoError(t, err)

	expectedSingle := sliceCapture("ABC")
	expectedSlicePtr1 := sliceCapture("LMN")
	expectedSlicePtr2 := sliceCapture("OPQ")
	expected := &capture{
		Single:   &expectedSingle,
		Slice:    []sliceCapture{"DEF", "IJK"},
		SlicePtr: []*sliceCapture{&expectedSlicePtr1, &expectedSlicePtr2},
	}

	require.Equal(t, expected, captured)
}

type sliceParse string

func (s *sliceParse) Parse(lex *lexer.PeekingLexer) error {
	token := lex.Peek()
	if len(token.Value) != 3 {
		return participle.NextMatch
	}
	lex.Next()
	*s = sliceParse(strings.Repeat(token.Value, 2))
	return nil
}

func TestParseOnSliceElements(t *testing.T) { // nolint:dupl
	type parse struct {
		Single *sliceParse  `@@`
		Slice  []sliceParse `@@+`
	}

	parser := participle.MustBuild[parse]([]participle.Option{
		participle.Lexer(lexer.MustSimple([]lexer.SimpleRule{
			{Name: "Element", Pattern: `[a-z]{3}`},
			{Name: "Whitespace", Pattern: `[\s|\n]+`},
		})),
		participle.Elide("Whitespace"),
	}...)

	parsed, err := parser.ParseString("parse_slice", `abc def ijk`)
	require.NoError(t, err)

	expectedSingle := sliceParse("abcabc")
	expected := &parse{
		Single: &expectedSingle,
		Slice:  []sliceParse{"defdef", "ijkijk"},
	}

	require.Equal(t, expected, parsed)
}

func TestUnmarshalNetIP(t *testing.T) {
	type grammar struct {
		IP net.IP `@IP`
	}

	parser := mustTestParser[grammar](t, participle.Lexer(lexer.MustSimple([]lexer.SimpleRule{
		{"IP", `[\d.]+`},
	})))
	ast, err := parser.ParseString("", "10.2.3.4")
	require.NoError(t, err)
	require.Equal(t, "10.2.3.4", ast.IP.String())
}

type Address net.IP

func (a *Address) Capture(values []string) error {
	fmt.Println("does not run at all")
	*a = Address(net.ParseIP(values[0]))
	return nil
}

func TestCaptureIP(t *testing.T) {
	type grammar struct {
		IP Address `@IP`
	}

	parser := mustTestParser[grammar](t, participle.Lexer(lexer.MustSimple([]lexer.SimpleRule{
		{"IP", `[\d.]+`},
	})))
	ast, err := parser.ParseString("", "10.2.3.4")
	require.NoError(t, err)
	require.Equal(t, "10.2.3.4", (net.IP)(ast.IP).String())
}

func BenchmarkIssue143(b *testing.B) {
	type Disjunction struct {
		Long1 bool `parser:"  '<' '1' ' ' 'l' 'o' 'n' 'g' ' ' 'r' 'u' 'l' 'e' ' ' 't' 'o' ' ' 'f' 'o' 'r' 'm' 'a' 't' '>'"`
		Long2 bool `parser:"| '<' '2' ' ' 'l' 'o' 'n' 'g' ' ' 'r' 'u' 'l' 'e' ' ' 't' 'o' ' ' 'f' 'o' 'r' 'm' 'a' 't' '>'"`
		Long3 bool `parser:"| '<' '3' ' ' 'l' 'o' 'n' 'g' ' ' 'r' 'u' 'l' 'e' ' ' 't' 'o' ' ' 'f' 'o' 'r' 'm' 'a' 't' '>'"`
		Long4 bool `parser:"| '<' '4' ' ' 'l' 'o' 'n' 'g' ' ' 'r' 'u' 'l' 'e' ' ' 't' 'o' ' ' 'f' 'o' 'r' 'm' 'a' 't' '>'"`
		Real  bool `parser:"| '<' 'x' '>'"`
	}

	type Disjunctions struct {
		List []Disjunction `parser:"@@*"`
	}

	var disjunctionParser = participle.MustBuild[Disjunctions]()
	input := "<x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x> <x>"
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if _, err := disjunctionParser.ParseString("", input); err != nil {
			panic(err)
		}
	}
}

type Boxes struct {
	Pos   lexer.Position
	Boxes Box `@Ident`
}

type Box struct {
	Pos lexer.Position
	Val string `@Ident`
}

func (b *Box) Capture(values []string) error {
	b.Val = values[0]
	return nil
}

func TestBoxedCapture(t *testing.T) {
	lex := lexer.MustSimple([]lexer.SimpleRule{
		{"Ident", `[a-zA-Z](\w|\.|/|:|-)*`},
		{"whitespace", `\s+`},
	})

	parser := participle.MustBuild[Boxes](
		participle.Lexer(lex),
		participle.UseLookahead(2),
	)

	if _, err := parser.ParseString("test", "abc::cdef.abc"); err != nil {
		t.Fatal(err)
	}
}

func TestMatchEOF(t *testing.T) {
	type testMatchNewlineOrEOF struct {
		Text []string `@Ident+ ("\n" | EOF)`
	}
	p := mustTestParser[testMatchNewlineOrEOF](t)
	_, err := p.ParseString("", "hell world")
	require.NoError(t, err)
	_, err = p.ParseString("", "hell world\n")
	require.NoError(t, err)
}

func TestParseExplicitElidedIdent(t *testing.T) { // nolint
	lex := lexer.MustSimple([]lexer.SimpleRule{
		{"Ident", `[a-zA-Z](\w|\.|/|:|-)*`},
		{"Comment", `/\*[^*]*\*/`},
		{"whitespace", `\s+`},
	})
	type grammar struct {
		Comment string `@Comment?`
		Ident   string `@Ident`
	}
	p := mustTestParser[grammar](t, participle.Lexer(lex), participle.Elide("Comment"))

	actual, err := p.ParseString("", `hello`)
	require.NoError(t, err)
	require.Equal(t, &grammar{Ident: "hello"}, actual)

	actual, err = p.ParseString("", `/* Comment */ hello`)
	require.NoError(t, err)
	require.Equal(t, &grammar{Comment: `/* Comment */`, Ident: "hello"}, actual)
}

func TestParseExplicitElidedTypedLiteral(t *testing.T) { // nolint
	lex := lexer.MustSimple([]lexer.SimpleRule{
		{"Ident", `[a-zA-Z](\w|\.|/|:|-)*`},
		{"Comment", `/\*[^*]*\*/`},
		{"whitespace", `\s+`},
	})
	type grammar struct {
		Comment string `@"/* Comment */":Comment?`
		Ident   string `@Ident`
	}
	p := mustTestParser[grammar](t, participle.Lexer(lex), participle.Elide("Comment"))

	actual, err := p.ParseString("", `hello`)
	require.NoError(t, err)
	require.Equal(t, &grammar{Ident: "hello"}, actual)

	actual, err = p.ParseString("", `/* Comment */ hello`)
	require.NoError(t, err)
	require.Equal(t, &grammar{Comment: `/* Comment */`, Ident: "hello"}, actual)
}

func TestEmptySequenceMatches(t *testing.T) {
	lex := lexer.MustSimple([]lexer.SimpleRule{
		{"Ident", `[a-zA-Z](\w|\.|/|:|-)*`},
		{"Comment", `/\*[^*]*\*/`},
		{"Whitespace", `\s+`},
	})
	type grammar struct {
		Ident    []string `@Ident*`
		Comments []string `@Comment*`
	}
	p := mustTestParser[grammar](t, participle.Lexer(lex), participle.Elide("Whitespace"))
	expected := &grammar{}
	actual, err := p.ParseString("", "")
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}

type RootParseableFail struct{}

func (*RootParseableFail) String() string   { return "" }
func (*RootParseableFail) GoString() string { return "" }
func (*RootParseableFail) Parse(lex *lexer.PeekingLexer) error {
	return errors.New("always fail immediately")
}

func TestRootParseableFail(t *testing.T) {
	p := mustTestParser[RootParseableFail](t)
	_, err := p.ParseString("<test>", "blah")
	require.EqualError(t, err, "<test>:1:1: always fail immediately")
}

type (
	TestCustom interface{ isTestCustom() }

	CustomIdent   string
	CustomNumber  float64
	CustomBoolean bool
)

func (CustomIdent) isTestCustom()   {}
func (CustomNumber) isTestCustom()  {}
func (CustomBoolean) isTestCustom() {}

func TestParserWithCustomProduction(t *testing.T) {
	type grammar struct {
		Custom TestCustom `@@`
	}

	p := mustTestParser[grammar](t, participle.ParseTypeWith(func(lex *lexer.PeekingLexer) (TestCustom, error) {
		switch peek := lex.Peek(); {
		case peek.Type == scanner.Int || peek.Type == scanner.Float:
			v, err := strconv.ParseFloat(lex.Next().Value, 64)
			if err != nil {
				return nil, err
			}
			return CustomNumber(v), nil
		case peek.Type == scanner.Ident:
			name := lex.Next().Value
			if name == "true" || name == "false" {
				return CustomBoolean(name == "true"), nil
			}
			return CustomIdent(name), nil
		default:
			return nil, participle.NextMatch
		}
	}))

	type testCase struct {
		src      string
		expected TestCustom
	}

	for _, c := range []testCase{
		{"a", CustomIdent("a")},
		{"12.5", CustomNumber(12.5)},
		{"true", CustomBoolean(true)},
		{"false", CustomBoolean(false)},
	} {
		actual, err := p.ParseString("", c.src)
		require.NoError(t, err)
		require.Equal(t, c.expected, actual.Custom)
	}

	require.Equal(t, `Grammar = TestCustom .`, p.String())
}

type (
	TestUnionA interface{ isTestUnionA() }
	TestUnionB interface{ isTestUnionB() }

	AMember1 struct {
		V string `@Ident`
	}

	AMember2 struct {
		V TestUnionB `"[" @@ "]"`
	}

	BMember1 struct {
		V float64 `@Int | @Float`
	}

	BMember2 struct {
		V TestUnionA `"{" @@ "}"`
	}
)

func (AMember1) isTestUnionA() {}
func (AMember2) isTestUnionA() {}

func (BMember1) isTestUnionB() {}
func (BMember2) isTestUnionB() {}

func TestParserWithUnion(t *testing.T) {
	type grammar struct {
		A TestUnionA `@@`
		B TestUnionB `| @@`
	}

	parser := mustTestParser[grammar](t, participle.UseLookahead(10),
		participle.Union[TestUnionA](AMember1{}, AMember2{}),
		participle.Union[TestUnionB](BMember1{}, BMember2{}))

	type testCase struct {
		src      string
		expected grammar
	}

	for _, c := range []testCase{
		{`a`, grammar{A: AMember1{"a"}}},
		{`1.5`, grammar{B: BMember1{1.5}}},
		{`[2.5]`, grammar{A: AMember2{BMember1{2.5}}}},
		{`{x}`, grammar{B: BMember2{AMember1{"x"}}}},
		{`{ [ { [12] } ] }`, grammar{B: BMember2{AMember2{BMember2{AMember2{BMember1{12}}}}}}},
	} {
		var trace strings.Builder
		actual, err := parser.ParseString("", c.src, participle.Trace(&trace))
		require.NoError(t, err)
		require.Equal(t, &c.expected, actual)
		require.NotEqual(t, "", trace.String())
	}

	require.Equal(t, strings.TrimSpace(`
Grammar = TestUnionA | TestUnionB .
TestUnionA = AMember1 | AMember2 .
AMember1 = <ident> .
AMember2 = "[" TestUnionB "]" .
TestUnionB = BMember1 | BMember2 .
BMember1 = <int> | <float> .
BMember2 = "{" TestUnionA "}" .
	`), parser.String())
}

func TestParseSubProduction(t *testing.T) {
	type (
		ListItem struct {
			Number *float64 `(@Int | @Float)`
			String *string  `| @String`
		}

		Grammar struct {
			List []ListItem `"[" @@ ("," @@)* "]"`
		}
	)

	numberItem := func(n float64) ListItem { return ListItem{Number: &n} }
	stringItem := func(s string) ListItem { return ListItem{String: &s} }

	p := mustTestParser[Grammar](t, participle.Unquote())

	expected := &Grammar{List: []ListItem{numberItem(1), stringItem("test")}}

	actual, err := p.ParseString("", `[ 1, "test" ]`)
	require.NoError(t, err)
	require.Equal(t, expected, actual)

	expectedItem := numberItem(1.234e5)

	ip, err := participle.ParserForProduction[ListItem](p)
	require.NoError(t, err)

	actualItem, err := ip.ParseString("", `1.234e5`)
	require.NoError(t, err)
	require.Equal(t, &expectedItem, actualItem)

	expectedItem2 := stringItem("\t\ttest\t\t")

	actualItem2, err := ip.ParseString("", `"\t\ttest\t\t"`)
	require.NoError(t, err)
	require.Equal(t, &expectedItem2, actualItem2)
}

type I255Grammar struct {
	Union I255Union `@@`
}

type I255Union interface{ union() }

type I255String struct {
	Value string `@String`
}

func (*I255String) union() {}

func TestIssue255(t *testing.T) {
	parser, err := participle.Build[I255Grammar](
		participle.Union[I255Union](&I255String{}),
	)
	require.NoError(t, err)
	g, err := parser.ParseString("", `"Hello, World!"`)
	require.NoError(t, err)
	require.Equal(t, &I255Grammar{Union: &I255String{Value: `"Hello, World!"`}}, g)
}

func TestParseNumbers(t *testing.T) {
	type grammar struct {
		Int   int8    `@('-'? Int)`
		Uint  uint16  `@('-'? Int)`
		Float float64 `@Ident`
	}
	parser := participle.MustBuild[grammar]()
	_, err := parser.ParseString("", `300 0 x`)
	require.EqualError(t, err, `grammar.Int: strconv.ParseInt: parsing "300": value out of range`)
	_, err = parser.ParseString("", `-2 -2 x`)
	require.EqualError(t, err, `grammar.Uint: strconv.ParseUint: parsing "-2": invalid syntax`)
	_, err = parser.ParseString("", `0 0 nope`)
	require.EqualError(t, err, `grammar.Float: strconv.ParseFloat: parsing "nope": invalid syntax`)
	result, err := parser.ParseString("", `-30 3000 Inf`)
	require.NoError(t, err)
	require.Equal(t, grammar{Int: -30, Uint: 3000, Float: math.Inf(1)}, *result)
}
