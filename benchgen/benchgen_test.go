package benchgen

import (
	"os"
	"strings"
	"testing"

	require "github.com/alecthomas/assert/v2"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

var parser = participle.MustBuild[AST]()

func TestGenerateOther(t *testing.T) {
	code := participle.GenerateCode(parser, "benchgen", false)
	println(string(code))
	err := os.WriteFile("./generated.go", code, 0774)
	require.NoError(t, err)
}

func BenchmarkGeneratedOld(b *testing.B) {
	b.ReportAllocs()
	lex := makeBenchmarkLexer(b)
	checkpoint := lex.MakeCheckpoint()
	for i := 0; i < b.N; i++ {
		lex.LoadCheckpoint(checkpoint)
		var out AST
		if err := out.ParseOld(lex); err != nil {
			require.NoError(b, err)
		}
	}
}

func BenchmarkGeneratedNew(b *testing.B) {
	b.ReportAllocs()
	lex := makeBenchmarkLexer(b)
	checkpoint := lex.MakeCheckpoint()
	for i := 0; i < b.N; i++ {
		lex.LoadCheckpoint(checkpoint)
		var out AST
		if err := ParseAST(lex, &out, false); err != nil {
			require.NoError(b, err)
		}
	}
}

const BenchmarkInput = `
string = "hello world"
number = 1234
`

func makeBenchmarkLexer(b *testing.B) *lexer.PeekingLexer {
	lex, err := lexer.TextScannerLexer.Lex("", strings.NewReader(BenchmarkInput))
	if err != nil {
		require.NoError(b, err)
	}
	peeking, err := lexer.Upgrade(lex)
	if err != nil {
		require.NoError(b, err)
	}
	return peeking
}
