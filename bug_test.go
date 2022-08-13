package participle

import (
	"testing"

	require "github.com/alecthomas/assert/v2"
)

type BugStructCapturesInBadBranch struct {
	Bad bool                `parser:"(@'!'"`
	A   BugFirstAlternative `parser:" @@) |"`
	B   int                 `parser:"('!' '#' @Int)"`
}

type BugFirstAlternative struct {
	Value string `parser:"'#' @Ident"`
}

func TestBug_GroupCapturesInBadBranch(t *testing.T) {
	out, err := MustBuild[BugStructCapturesInBadBranch](UseLookahead(2)).ParseString("", "!#4")
	require.NoError(t, err)
	require.Equal(t, &BugStructCapturesInBadBranch{B: 4}, out)
}
