package generated

import (
	"fmt"
	"strings"
	"testing"

	require "github.com/alecthomas/assert/v2"

	source "github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

// TODO:
// TODO: BUG bool true if only one dot
const input = `test str ing (this is fast) !(4 + 3) (this is fast) !(7 - 65) !(this is LAST) x z . . 4 2 the end`

func TestGenerated(t *testing.T) {
	parser := source.StuffParser
	var stuffNew source.Stuff

	lex, err := source.StuffLexer.Lex("", strings.NewReader(input))
	require.NoError(t, err)
	lexOrig, err := lexer.Upgrade(lex)
	require.NoError(t, err)
	lexNew := lexOrig.Clone()

	stuffOrig, err := parser.ParseFromLexer(lexOrig)
	require.NoError(t, err)
	fmt.Printf("%+v\n", stuffOrig)

	require.NoError(t, ParseStuff(lexNew, &stuffNew, false))
	fmt.Printf("%+v\n", &stuffNew)

	require.Equal(t, *stuffOrig, stuffNew)
}

func BenchmarkNative(b *testing.B) {
	lex := makeBenchmarkLexer(b)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		lexCopy := lex.Clone()
		if _, err := source.StuffParser.ParseFromLexer(lexCopy); err != nil {
			require.NoError(b, err)
		}
	}
}

func BenchmarkGenerated(b *testing.B) {
	lex := makeBenchmarkLexer(b)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		var stuff source.Stuff
		lexCopy := lex.Clone()
		if err := ParseStuff(lexCopy, &stuff, false); err != nil {
			require.NoError(b, err)
		}
		//os.Exit(0)
	}
}

var _ = "test str ing " + strings.Repeat("(which is long) ", 5) + "end"
var benchInput = input

func makeBenchmarkLexer(b *testing.B) *lexer.PeekingLexer {
	lex, err := source.StuffLexer.Lex("", strings.NewReader(benchInput))
	if err != nil {
		require.NoError(b, err)
	}
	peeking, err := lexer.Upgrade(lex)
	if err != nil {
		require.NoError(b, err)
	}
	return peeking
}
