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
            
            if list(outline):
                # Add blank line after properties if we have both properties and children
                if attrib:
                    yield ''
                data.headline_depth += 1
                yield from process_body(outline, data)
                data.headline_depth -= 1
                # Add blank line after headline section if we're at depth 1 or 2
                if data.headline_depth <= 2:
                    yield ''
        elif structure == 'list':
            yield f"{' ' * data.list_depth}- {text}"
            if list(outline): # This list item has a sub-list
                data.list_depth += 2
                yield from process_body(outline, data)
                data.list_depth -= 2
        elif structure == 'paragraph':
            # For paragraphs under headlines, indent with 2 spaces
            # Check if text already contains line breaks, if so preserve them
            if '\n' in text:
                for line in text.split('\n'):
                    if line.strip():
                        yield f"  {line.strip()}"
                    else:
                        yield ""
            else:
                # For single-line text, wrap it
                wrapped_text = wrap_text(text, width=68)  # 70 - 2 for indentation
                for line in wrapped_text.split('\n'):
                    if line.strip():
                        yield f"  {line}"
                    else:
                        yield ""
            # Add blank line after each paragraph
            yield ""


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


def wrap_text(text: str, width: int = 70) -> str:
    """Wrap text to specified width, with custom logic for better line breaks."""
    # Split on existing newlines first
    lines = text.split('\n')
    wrapped_lines = []
    
    for line in lines:
        if len(line.strip()) == 0:
            wrapped_lines.append('')
        else:
            line = line.strip()
            if len(line) <= width:
                wrapped_lines.append(line)
            else:
                # Try to find good break points
                # Look for patterns like "-- " followed by text
                if ' -- ' in line:
                    # Try to break after the second "--"
                    parts = line.split(' -- ')
                    if len(parts) >= 3:  # At least two "--" separators
                        # Try breaking after the second "--"
                        first_part = ' -- '.join(parts[:2]) + ' --'
                        if len(first_part) <= width:
                            wrapped_lines.append(first_part)
                            remaining = ' '.join(parts[2:])
                            if remaining:
                                # Recursively wrap the remaining text
                                remaining_wrapped = wrap_text(remaining, width)
                                wrapped_lines.extend(remaining_wrapped.split('\n'))
                            continue
                
                # Fall back to word-based wrapping
                words = line.split()
                current_line = []
                current_length = 0
                
                for word in words:
                    word_length = len(word) + (1 if current_line else 0)
                    
                    if current_length + word_length <= width:
                        current_line.append(word)
                        current_length += word_length
                    else:
                        if current_line:
                            wrapped_lines.append(' '.join(current_line))
                        current_line = [word]
                        current_length = len(word)
                
                if current_line:
                    wrapped_lines.append(' '.join(current_line))
    
    return '\n'.join(wrapped_lines)


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
    # Check if this looks like an NBA-style document (has description)
    has_description = head.find('description') is not None and head.find('description').text
    
    headers = []
    if has_description:
        headers.append('#+STARTUP: indent')
    
    headers.extend(filter(
        None,
        (
            extract_header(head, 'title'),
            extract_header(head, 'description'),
        ),
    ))
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
        # Remove any trailing empty lines, then add exactly one newline
        final_output = final_output.rstrip() + '\n'
        
    sys.stdout.write(final_output)


if __name__ == '__main__':
    main()
