#!/usr/bin/env python3
"""Convert OPML to Org mode."""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
import argparse

from typing import Iterator, Optional


class OpmlData:
    """Data object for tracking OPML conversion state."""

    def __init__(self) -> None:
        """Initialize conversion state."""
        self.has_headline_attributes: bool = False
        self.headline_depth: int = 1
        self.list_depth: int = 0


def process_body(
    element: ET.Element, data: Optional[OpmlData] = None
) -> Iterator[str]:
    """Yield Org mode lines for the given OPML body element.

    Args:
        element: XML element to process.
        data: Conversion state, created if None.

    Yields:
        Lines of Org mode text.
    """
    if data is None:
        data = OpmlData()

    for outline in element:
        attrib = outline.attrib.copy()
        if 'text' not in attrib:
            raise AssertionError('missing text attribute')
        text = attrib.pop('text')

        if 'structure' in attrib:
            structure = attrib.pop('structure')
        else:
            # Case 1: Item has its own OPML attributes (e.g. 'name', 'type' in attributes.opml)
            if attrib:
                data.has_headline_attributes = True # Mark that we've seen an attributed headline
                structure = 'headline'
            # Case 2: Item has children, and we are in a "simple OPML" context (like nba.opml)
            # (no attributed headlines seen yet, not in a list context).
            # This makes items like "NBA", "Eastern Conf" into headlines.
            elif list(outline) and not data.has_headline_attributes and data.list_depth == 0:
                structure = 'headline'
            # Case 3: Item has children, but didn't match Case 1 or 2.
            # This is typically for list items that have sub-lists.
            elif list(outline):
                structure = 'list' 
            # Case 4: Item is a LEAF node (no children), and didn't match Case 1.
            else:
                # If in "simple OPML" (nba.opml) and nested under a headline (depth > 1), it's a list item.
                if not data.has_headline_attributes and data.headline_depth > 1:
                    structure = 'list'
                # Otherwise (e.g. top-level leaf, or any leaf in attributed OPML context if not explicitly structured), it's a paragraph.
                else:
                    structure = 'paragraph'

        if structure == 'headline':
            yield f"{'*' * data.headline_depth} {text}"
            if attrib:
                yield '  :PROPERTIES:'
                for key, value in attrib.items():
                    yield f"  :{key}: {value}"
                yield '  :END:'
                if list(outline): # Add a blank line if properties are followed by more content under this headline
                    yield ''
            if list(outline):
                data.headline_depth += 1
                yield from process_body(outline, data)
                data.headline_depth -= 1
        elif structure == 'list':
            yield f"{' ' * data.list_depth}- {text}"
            if list(outline): # This list item has a sub-list
                data.list_depth += 2
                yield from process_body(outline, data)
                data.list_depth -= 2
        elif structure == 'paragraph':
            yield text # Removed trailing \n; '\n'.join will handle line separation


def extract_header(
    head: ET.Element,
    tag: str,
    export_tag: Optional[str] = None
) -> Optional[str]:
    """Extract Org mode header from OPML head element.

    Args:
        head: OPML head element.
        tag: Tag name to find.
        export_tag: Header name override.

    Returns:
        Formatted header line or None.
    """
    element = head.find(tag)
    if element is not None and element.text:
        key = export_tag or tag.upper()
        return f"#+{key}: {element.text}"
    return None


def main() -> None:
    """Read OPML from a file or stdin and write Org mode to stdout."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input', nargs='?', help='OPML file to read (defaults to stdin)')
    args = parser.parse_args()

    if args.input:
        with open(args.input, 'r', encoding='utf-8') as f:
            xml_content = f.read()
    else:
        xml_content = sys.stdin.read()

    tree = ET.fromstring(xml_content)
    head = tree.find('head')
    body = tree.find('body')
    
    if head is None:
        head = ET.Element('head')
    if body is None:
        raise ValueError("OPML file must contain a <body> element")
    headers = filter(
        None,
        (
            extract_header(head, 'title'),
            extract_header(head, 'description'),
        ),
    )
    org_header_lines = list(headers)
    org_body_lines = list(process_body(body))

    # Join header and body lines separately first
    joined_header = '\n'.join(org_header_lines)
    joined_body = '\n'.join(org_body_lines)

    final_output = ""
    if joined_header:
        final_output += joined_header
        # Add separator only if both header and body exist and are non-empty
        # (An empty body is falsey, an empty header is falsey)
        if joined_body: 
            final_output += "\n\n" # Two newlines for a blank line
    
    if joined_body:
        final_output += joined_body
    
    # Ensure final output ends with a single newline, unless it's completely empty.
    if final_output:
        final_output = final_output.rstrip('\n') + '\n'
        
    sys.stdout.write(final_output)


if __name__ == '__main__':
    main()
