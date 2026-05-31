import os
import sys
import shutil
import subprocess

def restore_page(wiki_root, slug):
    wiki_root = os.path.expanduser(wiki_root)
    archive_root = os.path.join(wiki_root, "archive")
    
    # Find the file in the archive
    found_path = None
    for root, dirs, files in os.walk(archive_root):
        if f"{slug}.md" in files:
            found_path = os.path.join(root, f"{slug}.md")
            break
    
    if not found_path:
        print(f"Error: Archived page '{slug}' not found under {archive_root}")
        return False

    dest_path = os.path.join(wiki_root, f"{slug}.md")
    shutil.move(found_path, dest_path)
    print(f"Restored '{slug}' to {dest_path}")

    # Reindex
    reindex_script = os.path.join(os.path.dirname(__file__), "reindex.py")
    if os.path.exists(reindex_script):
        subprocess.run([sys.executable, reindex_script, wiki_root])
    
    return True

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: restore.py <wiki-root> <slug>")
        sys.exit(1)
    
    success = restore_page(sys.argv[1], sys.argv[2])
    sys.exit(0 if success else 1)
