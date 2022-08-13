package lexer_test

import (
	"testing"

	require "github.com/alecthomas/assert/v2"

	"github.com/alecthomas/participle/v2/lexer"
)

type staticLexer struct {
	tokens []lexer.Token
}

func (s *staticLexer) Next() (lexer.Token, error) {
	if len(s.tokens) == 0 {
		return lexer.EOFToken(lexer.Position{}), nil
	}
	t := s.tokens[0]
	s.tokens = s.tokens[1:]
	return t, nil
}

type batchLexer struct {
	batches [][]lexer.Token
}

func (b *batchLexer) Next() (lexer.Token, error) {
	panic("shouldn't be called")
}

func (b *batchLexer) NextBatch() ([]lexer.Token, error) {
	ret := b.batches[0] // Should never be called after EOF is returned at the end of a batch
	b.batches = b.batches[1:]
	return ret, nil
}

func TestUpgrade_Lexer(t *testing.T) {
	t0 := lexer.Token{Type: 1, Value: "moo"}
	ts := lexer.Token{Type: 3, Value: " "}
	t1 := lexer.Token{Type: 2, Value: "blah"}
	tokens := []lexer.Token{t0, ts, t1}
	l, err := lexer.Upgrade(&staticLexer{tokens: tokens}, 3)
	require.NoError(t, err)
	require.Equal(t, t0, *l.Peek())
	require.Equal(t, t0, *l.Peek())
	require.Equal(t, tokens, l.Range(0, 3))
}

func TestUpgrade_BatchLexer(t *testing.T) {
	batches := [][]lexer.Token{
		{{Type: 1, Value: "x"}, {Type: 3, Value: " "}},
		{{Type: 1, Value: "y"}},
		{{Type: 3, Value: " "}, {Type: 2, Value: "z"}, lexer.EOFToken(lexer.Position{})},
	}
	l, err := lexer.Upgrade(&batchLexer{batches: batches}, 3)
	require.NoError(t, err)
	require.Equal(t, 1, l.Peek().Type)
	require.Equal(t, "x", l.Next().Value)
	require.Equal(t, "y", l.Next().Value)
	require.Equal(t, "z", l.Next().Value)
	require.Equal(t, lexer.EOF, l.Next().Type)
	require.Equal(t, lexer.EOF, l.Peek().Type)
}

func TestPeekingLexer_Peek_Next_Checkpoint(t *testing.T) {
	slexdef := lexer.MustSimple([]lexer.SimpleRule{
		{"Ident", `\w+`},
		{"Whitespace", `\s+`},
	})
	slex, err := slexdef.LexString("", `hello world last`)
	require.NoError(t, err)
	plex, err := lexer.Upgrade(slex, slexdef.Symbols()["Whitespace"])
	require.NoError(t, err)
	expected := []lexer.Token{
		{Type: -2, Value: "hello", Pos: lexer.Position{Line: 1, Column: 1}},
		{Type: -3, Value: " ", Pos: lexer.Position{Line: 1, Column: 6, Offset: 5}},
		{Type: -2, Value: "world", Pos: lexer.Position{Line: 1, Column: 7, Offset: 6}},
		{Type: -3, Value: " ", Pos: lexer.Position{Line: 1, Column: 12, Offset: 11}},
		{Type: -2, Value: "last", Pos: lexer.Position{Line: 1, Column: 13, Offset: 12}},
	}
	checkpoint := plex.Checkpoint
	require.Equal(t, expected[0], *plex.Next())
	require.Equal(t, expected[2], *plex.Peek(), "should have skipped whitespace")
	plex.Checkpoint = checkpoint
	require.Equal(t, expected[0], *plex.Peek(), "should have reverted to pre-Next state")
}

func BenchmarkPeekingLexer_Peek(b *testing.B) {
	tokens := []lexer.Token{{Type: 1, Value: "x"}, {Type: 3, Value: " "}, {Type: 2, Value: "y"}}
	l, err := lexer.Upgrade(&staticLexer{tokens: tokens}, 3)
	require.NoError(b, err)
	l.Next()
	t := l.Peek()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		t = l.Peek()
		if t.EOF() {
			return
		}
	}
	require.Equal(b, lexer.Token{Type: 2, Value: "y"}, *t)
}
