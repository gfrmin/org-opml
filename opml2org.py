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
            if attrib or (not data.has_headline_attributes
                          and data.list_depth == 0):
                if attrib:
                    data.has_headline_attributes = True
                structure = 'headline'
            elif list(outline):
                structure = 'list'
            else:
                structure = 'paragraph'

        if structure == 'headline':
            yield f"{'*' * data.headline_depth} {text}"
            if attrib:
                yield ':PROPERTIES:'
                for key, value in attrib.items():
                    yield f":{key}: {value}"
                yield ':END:\n'
            if list(outline):
                data.headline_depth += 1
                yield from process_body(outline, data)
                data.headline_depth -= 1
        elif structure == 'list':
            yield f"{' ' * data.list_depth}- {text}"
            if list(outline):
                data.list_depth += 2
                yield from process_body(outline, data)
                data.list_depth -= 2
        elif structure == 'paragraph':
            yield f"{text}\n"


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
    head, body = tree  # type: ignore[tuple-item]
    headers = filter(
        None,
        (
            extract_header(head, 'title'),
            extract_header(head, 'description'),
        ),
    )
    org_header = '\n'.join(headers)
    org_body = '\n'.join(process_body(body))
    sys.stdout.write(f"{org_header}\n\n{org_body}\n")


if __name__ == '__main__':
    main()
