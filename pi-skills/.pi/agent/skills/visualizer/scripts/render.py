#!/usr/bin/env python3
"""Render Markdown to an interactive HTML artifact using purpose-built templates.

Template types (--type):
  generic    - Universal document (default, auto-detects when unsure)
  explainer  - Research digest, tech catchup, learning guide, feature deep-dive
  comparison - Job search, options analysis, approach comparison
  plan       - Implementation plan, roadmap, project plan
  report     - Status report, incident post-mortem, weekly update
  deck       - Slide presentation
  kanban     - Triage board, prioritization, task board

Auto-detection: if --type is omitted, the script scans the markdown
content and picks the best template. Override with --type.
"""

import os
import sys
import argparse
import subprocess
import re

TEMPLATE_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'templates')
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
MANAGER = os.path.join(SCRIPT_DIR, 'manager.py')
GALLERY_TEMPLATE = os.path.join(os.path.dirname(SCRIPT_DIR), 'assets', 'gallery.html')

TEMPLATES = {
    'generic':    'generic.html',
    'explainer':  'explainer.html',
    'comparison': 'comparison.html',
    'plan':       'plan.html',
    'report':     'report.html',
    'deck':       'deck.html',
    'kanban':     'kanban.html',
}

TYPE_KEYWORDS = {
    'explainer': [
        r'\b(TL;DR|how\s+(to|does|.*works)|explainer|deep.?dive|research|catch-?up|digest|learning|guide|reference|overview of)\b',
        r'\b(FAQ|gotchas|step\s+\d|under the hood|internals|architecture)\b',
        r'\b(feature explainer|concept explainer|tutorial|walkthrough)\b',
        r'\b(tech.*catch-?up|weekly.*catch-?up|catch-?up.*week|news.*catch-?up)\b',
        r'\b(on this page|files read|key takeaways|bottom line|what you need to know)\b',
    ],
    'comparison': [
        r'\b(vs\.?|versus|compared?|comparison|trade-?off|pros?.?cons|benchmark|alternative)\b',
        r'\b(side.by.side|scorecard|option \d|approach \d|candidate)\b',
        r'\b(job.*search|listing|role\s+\d|position\s+\d|opportunity)\b',
        r'\b(tier\s+[123]|best match|stack match|identical stack|top pick|strong match)\b',
        r'\b(apply now|view role|blue card|salary range|remote.*germany)\b',
        r'\b(\d+\s*(roles?|positions?|listings?|opportunit(?:y|ies)))\b',
    ],
    'plan': [
        r'\b(implementation plan|roadmap|milestone|ship.*slice|delivery plan|project plan)\b',
        r'\b(week\s+\d|phase\s+\d|sprint\s+\d|slice\s+\d|timeline)\b',
        r'\b(risks?\s*(&|and)\s*mitigations?|decision needed|surfaces touched|packages?)\b',
    ],
    'report': [
        r'\b(status.*(report|update)|weekly.*update|incident report|post-?mortem|SEV-?\d)\b',
        r'\b(shipped|highlights|velocity|carryover|root cause|action items?)\b',
        r'\b(what shipped|impact.*(requests?|users?)|on.?call|mitigated.*\d|resolved.*\d)\b',
    ],
    'deck': [
        r'\b(slide|presentation|deck|keynote|talk|lightning)\b',
        r'\b(introduction slide|thank you|Q&A|next steps.*slide)\b',
    ],
    'kanban': [
        r'\b(kanban|triage|backlog|board|prioritization|kanban board|task board)\b',
        r'\b(ticket|issue.*board|priority.*assignee|estimate.*\d+\s*(pt|point|pts))\b',
        r'^##\s+(Now|Next|Later|Cut)\s*$',
        r'##\s+(Now|Next|Later|Cut)\s*\n',
    ],
}

def detect_type(content: str) -> str:
    """Auto-detect the best template type from markdown content."""
    scores = {}
    for ttype, patterns in TYPE_KEYWORDS.items():
        score = 0
        for pattern in patterns:
            matches = len(re.findall(pattern, content, re.IGNORECASE | re.MULTILINE))
            score += matches * 2
        if score > 0:
            scores[ttype] = score

    if not scores:
        return 'generic'

    # Return the type with the highest score
    return max(scores, key=scores.get)


def render(md_path: str, template_path: str, output_path: str,
           title: str, subtitle: str = "", description: str = "",
           important: bool = False):
    """Render markdown to HTML using the given template."""

    if not os.path.exists(md_path):
        print(f"Error: Markdown file not found at {md_path}", file=sys.stderr)
        sys.exit(1)

    if not os.path.exists(template_path):
        print(f"Error: Template file not found at {template_path}", file=sys.stderr)
        sys.exit(1)

    with open(md_path, 'r') as f:
        content = f.read()

    with open(template_path, 'r') as f:
        template = f.read()

    # Escape content for JS template literal
    escaped_content = content.replace('\\', '\\\\').replace('`', '\\`').replace('$', '\\$')

    rendered = template.replace('{{content}}', escaped_content)
    rendered = rendered.replace('{{title}}', title)
    rendered = rendered.replace('{{subtitle}}', subtitle or '')

    os.makedirs(os.path.dirname(output_path) or '.', exist_ok=True)
    with open(output_path, 'w') as f:
        f.write(rendered)

    print(f"✓ Rendered: {output_path}")

    # Update gallery
    if os.path.exists(MANAGER) and os.path.exists(GALLERY_TEMPLATE):
        output_dir = os.path.dirname(output_path) or '.'
        filename = os.path.basename(output_path)
        template_name = os.path.basename(template_path).replace('.html', '')

        cmd = [
            sys.executable, MANAGER,
            "--output-dir", output_dir,
            "--template-file", GALLERY_TEMPLATE,
            "--path", filename,
            "--title", title,
            "--description", description,
            "--template-type", template_name,
        ]
        if important:
            cmd.append("--important")

        try:
            subprocess.run(cmd, check=True)
        except Exception as e:
            print(f"Warning: Failed to update gallery index: {e}", file=sys.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Render Markdown to an interactive HTML artifact")
    parser.add_argument("md", help="Path to the input Markdown file")
    parser.add_argument("--output", required=True, help="Path to the output HTML file")
    parser.add_argument("--title", default="Artifact", help="Title of the document")
    parser.add_argument("--subtitle", default="", help="Subtitle / context label")
    parser.add_argument("--description", default="", help="Description for the gallery")
    parser.add_argument("--important", action="store_true",
                        help="Mark as featured in gallery")
    parser.add_argument("--type", dest="template_type", default=None,
                        choices=list(TEMPLATES.keys()),
                        help="Template type (auto-detected if omitted)")
    parser.add_argument("--template", default=None,
                        help="Path to a specific HTML template (overrides --type)")

    args = parser.parse_args()

    # Determine template
    if args.template:
        template_path = args.template
        template_type = 'custom'
    else:
        if args.template_type:
            template_type = args.template_type
        else:
            # Auto-detect
            with open(args.md, 'r') as f:
                content = f.read()
            template_type = detect_type(content)
            print(f"→ Auto-detected type: {template_type}")

        template_filename = TEMPLATES.get(template_type)
        if not template_filename:
            print(f"Error: Unknown template type '{template_type}'", file=sys.stderr)
            sys.exit(1)

        template_path = os.path.join(TEMPLATE_DIR, template_filename)

    # Generate subtitle if not provided
    subtitle = args.subtitle
    if not subtitle:
        if template_type in TEMPLATES:
            labels = {
                'explainer': 'Research & Learning',
                'comparison': 'Exploration & Comparison',
                'plan': 'Implementation Plan',
                'report': 'Report',
                'deck': 'Presentation',
                'kanban': 'Triage Board',
                'generic': 'Interactive Artifact',
            }
            subtitle = labels.get(template_type, '')

    render(
        md_path=args.md,
        template_path=template_path,
        output_path=args.output,
        title=args.title,
        subtitle=subtitle,
        description=args.description,
        important=args.important,
    )
