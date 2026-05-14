"""Tests for content format classification in _lib.py classify_format()."""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from _lib import classify_format


def test_quote():
    assert classify_format({"title": "Quoting Mitchell Hashimoto"}) == "quote"
    assert classify_format({"title": "Quoting someone on something"}) == "quote"


def test_release():
    # version number in title
    assert classify_format({"title": "llm 0.32a2"}) == "release"
    assert classify_format({"title": "Bun 1.2.0 released"}) == "release"
    assert classify_format({"title": "Node.js 22.1.0 is out"}) == "release"
    # release keywords in title/summary
    assert classify_format({"title": "Foo launched today", "summary": "..."}) == "release"
    assert classify_format({"title": "Bar now available", "summary": "..."}) == "release"
    assert classify_format({"title": "Baz GA", "summary": "..."}) == "release"


def test_postmortem():
    assert classify_format({
        "title": "Cloudflare QUIC death spiral fix",
        "summary": "We investigated a bug..."
    }) == "postmortem"
    assert classify_format({
        "title": "AWS outage postmortem",
        "summary": "..."
    }) == "postmortem"
    assert classify_format({
        "title": "Root cause analysis of the incident",
        "summary": "..."
    }) == "postmortem"
    assert classify_format({
        "title": "RCA: database failover",
        "summary": "..."
    }) == "postmortem"


def test_essay_domain():
    assert classify_format({"title": "Some thoughts", "primary": "https://danluu.com/foo"}) == "essay"
    assert classify_format({"title": "Deep dive", "primary": "https://fasterthanli.me/bar"}) == "essay"
    assert classify_format({"title": "Notes", "primary": "https://sirupsen.com/baz"}) == "essay"


def test_essay_long_title():
    # ≥ 6 words, no other tag matched
    assert classify_format({"title": "This is a long form essay title here"}) == "essay"
    assert classify_format({"title": "Short title"}) == "news"


def test_news_fallback():
    assert classify_format({"title": "Tech news today"}) == "news"
    assert classify_format({"title": "Update"}) == "news"


def test_priority_quote_over_postmortem():
    # quote wins over postmortem
    assert classify_format({"title": "Quoting someone about an outage"}) == "quote"


def test_priority_postmortem_over_release():
    # postmortem wins over release
    assert classify_format({"title": "Postmortem of the 2.0 release"}) == "postmortem"


def test_priority_release_over_essay():
    # release wins over essay (even with long title)
    assert classify_format({"title": "This is a very long title about version 1.0"}) == "release"


if __name__ == "__main__":
    test_quote()
    test_release()
    test_postmortem()
    test_essay_domain()
    test_essay_long_title()
    test_news_fallback()
    test_priority_quote_over_postmortem()
    test_priority_postmortem_over_release()
    test_priority_release_over_essay()
    print("PASS")
