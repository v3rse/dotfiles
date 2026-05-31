import os
import sys
import shutil
import subprocess

def archive_page(wiki_root, slug, category):
    wiki_root = os.path.expanduser(wiki_root)
    source_path = os.path.join(wiki_root, f"{slug}.md")
    archive_dir = os.path.join(wiki_root, "archive", category)
    dest_path = os.path.join(archive_dir, f"{slug}.md")

    if not os.path.exists(source_path):
        print(f"Error: Page '{slug}' not found at {source_path}")
        return False

    os.makedirs(archive_dir, exist_ok=True)
    shutil.move(source_path, dest_path)
    print(f"Archived '{slug}' to {dest_path}")

    # Reindex
    reindex_script = os.path.join(os.path.dirname(__file__), "reindex.py")
    if os.path.exists(reindex_script):
        subprocess.run([sys.executable, reindex_script, wiki_root])
    
    return True

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage: archive.py <wiki-root> <slug> <category>")
        sys.exit(1)
    
    success = archive_page(sys.argv[1], sys.argv[2], sys.argv[3])
    sys.exit(0 if success else 1)
