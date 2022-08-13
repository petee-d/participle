package benchgen

type Value struct {
	String string `parser:"  @String"`
	Number int    `parser:"| @Int"`
}

type Entry struct {
	Key   string `parser:"@Ident '='"`
	Value *Value `parser:"@@"`
}

type AST struct {
	Entries []Entry `parser:"@@*"`
}
