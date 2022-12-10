package testgen

import (
	"math"
	"strconv"
	"text/scanner"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

// TODO: **int or []*int doesn't work in the native parser, works in the generated one

type Combined struct {
	Features []Feature `parser:"@@+"`
}

type Feature interface{}

var featuresUnion = participle.Union[Feature](
	Nil{}, Strings{}, Ints{}, Uints{}, Floats{}, Structs{}, Negation{}, Custom{}, Parseable{},
)

type Nil struct {
	Ignored interface{} `parser:"'nil'"`
}

// Strings tests string capture, + ? * groups and multi-reference capture with literal
type Strings struct {
	Const  string  `parser:"@'str'"`
	Quoted string  `parser:"@String+"`
	Ptr    *string `parser:"@(Ident ('.' Ident)*)?"`
}

// Ints tests signed int capture, * ? groups, single-token capture with literal
type Ints struct {
	Normal int64   `parser:"'int' @Int"`
	Array  []int16 `parser:"@Int*"`
	Ptr    *int8   `parser:"@('-' Int)?"`
}

// Uints tests unsigned int capture, optional prefix capture, repeated capture of multiple tokens to a number or slice
type Uints struct {
	Normal uint     `parser:"'uint' @('-'? Int)"`
	Small  *uint32  `parser:"(@(Int Int)"`
	Big    []uint64 `parser:" @Int @Int)*"`
}

// Floats tests floats, mixed combined capture to numbers, capture of disjunctions of references
type Floats struct {
	A *float64 `parser:"'float' @(Int ('.' Int)?)"`
	B float32  `parser:"@(Float | Int)?"`
}

type Structs struct {
	Normal   Root    `parser:"'struct' @@"`
	Array    []Rec   `parser:"@@+"`
	PtrArray []*Root `parser:"('[' @@ (',' @@)* ']')?"`
}

type Root struct {
	Str string `parser:"@Ident"`
}

type Rec struct {
	Root Root `parser:"  @@"`
	In   *Rec `parser:"| '{' @@ '}'"`
}

type Negation struct {
	Trash  []string `parser:"'negation' @~(Ident | '.' '.' '.' '.'+ | '42')*"`
	Answer int      `parser:"@Int?"`
}

type Custom struct {
	Opt CustomContent `parser:"'custom' (@@ '-')*"`
	Req CustomContent `parser:"@@"`
}

type CustomContent interface{}

func customContentFn(lex *lexer.PeekingLexer) (CustomContent, error) {
	t := lex.Peek()
	if t.Type != scanner.Ident {
		return "", participle.NextMatch
	}
	if t.Value[0] != 'c' {
		return "", participle.Errorf(t.Pos, "must start with c")
	}
	return lex.Next().Value, nil
}

type Parseable struct {
	Rounded int `parser:"this is ignored"`
}

func (p *Parseable) Parse(lex *lexer.PeekingLexer) error {
	if t := lex.Next(); t.Type != scanner.Ident || t.Value != "parseable" {
		return participle.NextMatch
	}
	t := lex.Next()
	content := t.Value
	if t.Value == "-" {
		t = lex.Next()
		content += t.Value
	}
	if t.Type != scanner.Float {
		return participle.Errorf(t.Pos, "should have been a float")
	}
	value, _ := strconv.ParseFloat(content, 64) // Token was a float, has to be valid
	p.Rounded = int(math.Round(value))
	return nil
}

var _ participle.Parseable = (*Parseable)(nil)

type combinedGenerator struct {
	participle.GeneratedParserBase
}

var combinedParser = participle.MustBuild[Combined](
	participle.Lexer(lexer.TextScannerLexer),
	participle.UseLookahead(2),
	participle.Unquote(),
	featuresUnion,
	participle.ParseTypeWith[CustomContent](customContentFn),
	participle.UseGeneratedParser[combinedGenerator](),
)
