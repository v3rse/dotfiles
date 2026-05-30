import os
import random
import sys

def get_random_page(wiki_path):
    IGNORE = {"index.md", "README.md", "log.md"}
    files = [f for f in os.listdir(wiki_path) if f.endswith(".md") and f not in IGNORE]
    if not files:
        return None
    return random.choice(files)

if __name__ == "__main__":
    path = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser("~/org/wiki")
    page = get_random_page(path)
    if page:
        print(f"Found a random concept for you: **{page.replace('.md', '')}**")
        print(f"↳ Read it here: `{os.path.join(path, page)}`")
    else:
        print("No wiki pages found.")
