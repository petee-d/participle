package participle

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
)

type Stuff struct {
	Pos    lexer.Position
	A      string     `parser:"'test' ((?! 'str' 'ing') @Ident | @Ident @Ident)"`
	Sub    []SubStuff `parser:"@@+"`
	B      *string    `parser:"(@'x'? @'y'? @'z'?)!"`
	C      bool       `parser:"@('.' '.')? '.'?"`
	D      *int       `parser:"@('4' '2'?)"`
	E      []string   `parser:"@(Ident Ident)"`
	EndPos lexer.Position
}

type SubStuff struct {
	IntPrefix string       `parser:"( @'!'"`
	Int       *IntStuff    `parser:"        @@)"`
	Str       *StringStuff `parser:"| ('!'? @@)"`
}

func (s SubStuff) String() string {
	switch {
	case s.Int != nil:
		return fmt.Sprintf("SubStuff{P: %s, Int: %+v}", s.IntPrefix, *s.Int)
	case s.Str != nil:
		return fmt.Sprintf("SubStuff{P: %s, Str: %+v}", s.IntPrefix, *s.Str)
	}
	return "SubStuff{}"
}

type StringStuff struct {
	Object    string `parser:"'(' @Ident 'is' "`
	Adjective string `parser:"    @Ident ')'"`
	Tokens    []lexer.Token
}

type IntStuff struct {
	A  int32   `parser:"'(' @Int"`
	Op string  `parser:"@('+' | '-')"`
	B  float64 `parser:"    @Int ')'"`
}

var StuffLexer = lexer.TextScannerLexer
var StuffParser = MustBuild[Stuff](Lexer(StuffLexer), UseLookahead(2))
