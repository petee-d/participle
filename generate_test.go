package participle

import (
	"os"
	"testing"

	require "github.com/alecthomas/assert/v2"
)

func TestGenerate(t *testing.T) {
	code := GenerateCode(StuffParser, "generated", true)
	println(string(code))
	err := os.WriteFile("generated/generated.go", code, 0777)
	require.NoError(t, err)
}
