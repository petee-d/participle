package lexer

// PeekingLexer supports arbitrary lookahead as well as cloning.
type PeekingLexer struct {
	Checkpoint
	eof    Token
	tokens []Token
	elide  map[TokenType]bool
}

// RawCursor index in the token stream.
type RawCursor int

type Checkpoint struct {
	rawCursor RawCursor
	cursor    int
}

// Upgrade a Lexer to a PeekingLexer with arbitrary lookahead.
//
// "elide" is a slice of token types to elide from processing.
func Upgrade(lex Lexer, elide ...TokenType) (*PeekingLexer, error) {
	r := &PeekingLexer{
		elide: make(map[TokenType]bool, len(elide)),
	}
	for _, rn := range elide {
		r.elide[rn] = true
	}
	if batchLex, ok := lex.(BatchLexer); ok {
		for {
			batch, err := batchLex.NextBatch()
			if err != nil {
				return r, err
			}
			r.tokens = append(r.tokens, batch...)
			last := batch[len(batch)-1]
			if last.EOF() {
				r.eof = last
				break
			}
		}
	} else {
		for {
			t, err := lex.Next()
			if err != nil {
				return r, err
			}
			r.tokens = append(r.tokens, t)
			if t.EOF() {
				r.eof = t
				break
			}
		}
	}
	r.rawCursor = r.findNonElided(0) // Set rawCursor to the first non-elided token
	return r, nil
}

// Range returns the slice of tokens between the two cursor points.
func (p *PeekingLexer) Range(rawStart, rawEnd RawCursor) []Token {
	return p.tokens[rawStart:rawEnd]
}

// Cursor position in tokens, excluding elided tokens.
func (c Checkpoint) Cursor() int {
	return c.cursor
}

// RawCursor position in tokens, including elided tokens.
func (c Checkpoint) RawCursor() RawCursor {
	return c.rawCursor
}

// At returns the token at the given raw cursor point.
func (p *PeekingLexer) At(raw RawCursor) Token {
	return p.tokens[raw]
}

// Next consumes and returns the next token.
func (p *PeekingLexer) Next() Token {
	for int(p.rawCursor) < len(p.tokens) {
		t := p.tokens[p.rawCursor]
		p.rawCursor++
		if p.elide[t.Type] {
			continue
		}
		p.cursor++
		return t
	}
	return p.eof
}

// Peek ahead at the next token.
func (p *PeekingLexer) Peek() Token {
	for i := int(p.rawCursor); i < len(p.tokens); i++ {
		t := p.tokens[i]
		if p.elide[t.Type] {
			continue
		}
		return t
	}
	return p.eof
}

// Next consumes and returns the next token.
func (p *PeekingLexer) xNext() (Token, error) { // TODO: can remove error from signature?
	if int(p.rawCursor) >= len(p.tokens) {
		return p.eof, nil
	}
	result := p.tokens[p.rawCursor]
	p.rawCursor++
	p.cursor++
	p.rawCursor = p.findNonElided(0)
	return result, nil
}

// Peek ahead at the n+1 token. e.g. Peek(0) will peek at the next token. // TODO: n > 0 never used outside a test
func (p *PeekingLexer) xPeek(n int) (Token, error) {
	raw := p.rawCursor
	if n > 0 {
		raw = p.findNonElided(n)
	}
	if int(raw) >= len(p.tokens) {
		return p.eof, nil
	}
	return p.tokens[raw], nil
}

// Current returns the address of the token at the current raw cursor point, same as Peek(0).
func (p *PeekingLexer) Current() *Token {
	return &p.tokens[p.rawCursor]
}

// findNonElided finds the next non-elided tokens, skipping n other such tokens, and returns its RawCursor
func (p *PeekingLexer) findNonElided(skip int) RawCursor {
	raw := p.rawCursor
	for ; int(raw) < len(p.tokens); raw++ {
		if p.elide[p.tokens[raw].Type] {
			continue
		}
		// Non-elided token was found
		if skip <= 0 {
			return raw
		}
		skip--
	}
	return raw
}

// PeekAny peeks forward over elided and non-elided tokens.
//
// Elided tokens will be returned if they match, otherwise the next
// non-elided token will be returned.
//
// The returned RawCursor position is the location of the returned token.
// Use FastForward to move the internal cursors forward.
func (p *PeekingLexer) PeekAny(match func(Token) bool) (t Token, rawCursor RawCursor) {
	tokenCount := RawCursor(len(p.tokens))
	for i := p.rawCursor; i < tokenCount; i++ {
		t = p.tokens[i]
		if match(t) || !p.elide[t.Type] {
			return t, i
		}
	}
	return p.eof, tokenCount
}

// FastForward the internal cursors to this RawCursor position.
func (p *PeekingLexer) FastForward(rawCursor RawCursor) {
	tokenCount := RawCursor(len(p.tokens))
	for ; p.rawCursor <= rawCursor && p.rawCursor < tokenCount; p.rawCursor++ {
		t := p.tokens[p.rawCursor]
		if p.elide[t.Type] {
			continue
		}
		p.cursor++
	}
}

// RawPeek peeks ahead at the next raw token.
//
// Unlike Peek, this will include elided tokens.
func (p *PeekingLexer) RawPeek() Token {
	if int(p.rawCursor) < len(p.tokens) {
		return p.tokens[p.rawCursor]
	}
	return p.eof
}

// Clone creates a clone of this PeekingLexer at its current token.
//
// The parent and clone are completely independent.
func (p *PeekingLexer) Clone() *PeekingLexer {
	clone := *p
	return &clone
}

// MakeCheckpoint creates a copy of any internal state changed by Next, to be loaded with LoadCheckpoint
// It is a faster (allocation-free) alternative to Clone
func (p *PeekingLexer) MakeCheckpoint() Checkpoint {
	return p.Checkpoint
}

// LoadCheckpoint loads a Checkpoint created by MakeCheckpoint, changing the position of this instance
func (p *PeekingLexer) LoadCheckpoint(checkpoint Checkpoint) {
	p.Checkpoint = checkpoint
}
