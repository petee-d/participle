package main

import (
	"strings"
	"testing"
	"time"

	require "github.com/alecthomas/assert/v2"
	thriftparser "github.com/alecthomas/go-thrift/parser"
)

var (
	source = strings.TrimSpace(`
namespace cpp thrift.example
namespace java thrift.example

enum TweetType {
    TWEET
    RETWEET = 2
    DM = 3
    REPLY
}

struct Location {
    1: required double latitude
    2: required double longitude
}

struct Tweet {
    1: required i32 userId
    2: required string userName
    3: required string text
    4: optional Location loc
    5: optional TweetType tweetType = TweetType.TWEET
    16: optional string language = "english"
}

typedef list<Tweet> TweetList

struct TweetSearchResult {
    1: TweetList tweets
}

exception TwitterUnavailable {
    1: string message
}

const i32 MAX_RESULTS = 100

service Twitter {
    void ping()
    bool postTweet(1:Tweet tweet) throws (1:TwitterUnavailable unavailable)
    TweetSearchResult searchTweets(1:string query)
    void zip()
}
`)
)

func BenchmarkParticipleThrift(b *testing.B) {
	_, err := parserNativeLexer.ParseString("", source)
	require.NoError(b, err)

	b.ResetTimer()
	b.ReportAllocs()

	start := time.Now()
	for i := 0; i < b.N; i++ {
		_, _ = parserNativeLexer.ParseString("", source)
	}
	b.ReportMetric(float64(len(source)*b.N)*float64(time.Since(start)/time.Second)/1024/1024, "MiB/s")
}

func BenchmarkParticipleThriftGenerated(b *testing.B) {
	_, err := parserGeneratedLexer.ParseString("", source)
	require.NoError(b, err)

	b.ResetTimer()
	b.ReportAllocs()

	start := time.Now()
	for i := 0; i < b.N; i++ {
		_, _ = parserGeneratedLexer.ParseString("", source)
	}
	b.ReportMetric(float64(len(source)*b.N)*float64(time.Since(start)/time.Second)/1024/1024, "MiB/s")
}

func BenchmarkGoThriftParser(b *testing.B) {
	_, err := thriftparser.ParseReader("user.thrift", strings.NewReader(source))
	require.NoError(b, err)

	b.ResetTimer()
	b.ReportAllocs()

	start := time.Now()
	for i := 0; i < b.N; i++ {
		_, _ = thriftparser.ParseReader("user.thrift", strings.NewReader(source))
	}
	b.ReportMetric(float64(len(source)*b.N)*float64(time.Since(start)/time.Second)/1024/1024, "MiB/s")
}

func TestThriftParser_GeneratedConformance(t *testing.T) {
	native, err := parserNativeLexer.ParseString("", source)
	require.NoError(t, err)
	generated, err := parserGeneratedLexer.ParseString("", source)
	require.NoError(t, err)
	require.Equal(t, native, generated)
}
