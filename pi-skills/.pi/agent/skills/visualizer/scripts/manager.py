#!/usr/bin/env python3
"""Manages the artifact manifest and regenerates the gallery index."""

import os
import json
import datetime
import argparse


def update_manifest(output_dir: str, new_artifact: dict) -> list:
    manifest_path = os.path.join(output_dir, 'manifest.json')
    artifacts = []

    if os.path.exists(manifest_path):
        with open(manifest_path, 'r') as f:
            try:
                artifacts = json.load(f)
            except json.JSONDecodeError:
                artifacts = []

    # Update or add
    found = False
    for i, a in enumerate(artifacts):
        if a.get('path') == new_artifact['path']:
            artifacts[i] = new_artifact
            found = True
            break

    if not found:
        artifacts.insert(0, new_artifact)

    with open(manifest_path, 'w') as f:
        json.dump(artifacts, f, indent=2)

    return artifacts


def render_gallery(output_dir: str, artifacts: list, template_path: str):
    if not os.path.exists(template_path):
        print(f"Warning: Gallery template not found at {template_path}")
        return

    with open(template_path, 'r') as f:
        template = f.read()

    rendered = template.replace('{{artifacts_json}}', json.dumps(artifacts))

    gallery_path = os.path.join(output_dir, 'index.html')
    with open(gallery_path, 'w') as f:
        f.write(rendered)

    print(f"✓ Gallery updated: {gallery_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Update artifact manifest and regenerate gallery")
    parser.add_argument("--output-dir", required=True,
                        help="Directory containing artifacts and index.html")
    parser.add_argument("--template-file", required=True,
                        help="Path to gallery HTML template")
    parser.add_argument("--path", required=True,
                        help="Filename of the artifact (relative to output-dir)")
    parser.add_argument("--title", required=True,
                        help="Artifact title")
    parser.add_argument("--description", default="",
                        help="Artifact description")
    parser.add_argument("--template-type", default="generic",
                        help="Template type used (generic, explainer, comparison, etc.)")
    parser.add_argument("--important", action="store_true",
                        help="Mark as featured")
    parser.add_argument("--tags", default="",
                        help="Comma-separated tags")

    args = parser.parse_args()

    tags = [t.strip() for t in args.tags.split(',') if t.strip()]

    # Add template type as a tag for filtering
    if args.template_type and args.template_type not in tags:
        tags.append(args.template_type)

    # Format type label for display
    type_labels = {
        'generic': 'Document',
        'explainer': 'Explainer',
        'comparison': 'Comparison',
        'plan': 'Plan',
        'report': 'Report',
        'deck': 'Deck',
        'kanban': 'Kanban',
        'custom': 'Custom',
    }
    type_label = type_labels.get(args.template_type, args.template_type.title())

    new_artifact = {
        "path": args.path,
        "title": args.title,
        "description": args.description,
        "important": args.important,
        "date": datetime.date.today().isoformat(),
        "type": args.template_type,
        "typeLabel": type_label,
        "tags": tags,
    }

    artifacts = update_manifest(args.output_dir, new_artifact)
    render_gallery(args.output_dir, artifacts, args.template_file)
