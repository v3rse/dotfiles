"""Tests for word-boundary keyword matching in _lib.py kw_hit()."""
import sys
from pathlib import Path

# Add scripts dir to path so we can import _lib
sys.path.insert(0, str(Path(__file__).parent))

from _lib import kw_hit


def test_ops_boundary():
    # "ops" should NOT match inside "Desktops"
    assert kw_hit("Dungeons & Desktops: roguelike", ["ops"]) is False
    # "ops" SHOULD match standalone
    assert kw_hit("Backend ops engineering", ["ops"]) is True


def test_crypto_boundary():
    # "crypto" should NOT match inside "Cryptography"
    assert kw_hit("Cryptography paper", ["crypto"]) is False
    # "crypto" SHOULD match standalone
    assert kw_hit("Crypto/web3 nonsense", ["crypto"]) is True


def test_hyphenated_keywords():
    # Hyphens, dots, slashes in keywords must work via re.escape
    assert kw_hit("Using node.js for backend", ["node.js"]) is True
    assert kw_hit("A nodejs article", ["node.js"]) is False
    assert kw_hit("CI/CD pipeline setup", ["ci/cd"]) is True
    assert kw_hit("CICD pipeline", ["ci/cd"]) is False
    assert kw_hit("Staff-eng roles at BigCo", ["staff-eng"]) is True


def test_multiword_keywords():
    # Multi-word keywords should still work (word boundary on first word)
    assert kw_hit("Staff engineering is a real role", ["staff engineering"]) is True
    assert kw_hit("Engineering staff meeting", ["staff engineering"]) is False


def test_empty_and_whitespace():
    assert kw_hit("anything", []) is False
    assert kw_hit("anything", [""]) is False
    assert kw_hit("anything", ["  "]) is False


def test_case_insensitive():
    assert kw_hit("OPS Engineering", ["ops"]) is True
    assert kw_hit("Crypto News", ["crypto"]) is True


if __name__ == "__main__":
    test_ops_boundary()
    test_crypto_boundary()
    test_hyphenated_keywords()
    test_multiword_keywords()
    test_empty_and_whitespace()
    test_case_insensitive()
    print("PASS")
