"""
Writes the help from files into the appropriate format, whether for in-program
help or for man pages.
"""

from dataclasses import dataclass
from enum import Enum
from glob import glob
from typing import Dict, List
import sys


class TextKind:
    HEADING = 0
    TEXT = 1


@dataclass
class Text:
    kind: TextKind
    content: str


def read_help_files(help_file) -> Dict[str, Text]:
    topics = {}
    topic = None
    blocks = []
    current_block = None

    # Files are stored at the top level in the help directory.
    with open(help_file) as file:
        lines = file.readlines()
        for line in lines:
            if line.startswith('#'):
                # Start a new topic
                topic = line[1:].strip()
                blocks = []
                if topic in topics:
                    raise f"Found a duplicate topic {topic}"
                topics[topic] = blocks
                current_block = None
            elif line.isspace() or len(line) == 0:
                # End the current block
                current_block = None
                pass
            else:
                if current_block:
                    current_block.content += ' '
                    current_block.content += line.strip()
                else:
                    current_block = Text(kind=TextKind.TEXT, content=line.strip())
                    blocks.append(current_block)
    return topics


def write_ada_topics(file_path):
    with open(file_path, 'w') as file:
        # Write package heading
        file.write("package SP.Help.Topics is\n")

        # Write end of package
        file.write("end SP.Help.Topics;")


def write_ada_help_pages(path_base):
    help_spec_file = f"{path_base}.ads"

    # Topics get written to the .ads
    write_ada_topics(help_spec_file)

    # Descriptions get written to the .adb
    help_file_path = f"{path_base}.adb"
    with open(help_file_path) as file:
        file.write("Hello")


def write_as_ada_text(blocks: List[Text]):
    for block in blocks:
        print("SP.Help.Block (")

        # Write internals
        print(f'"{block.content}"')

        # Close
        print(");")
        print()


def main():
    # TODO: Generate man pages
    #write_ada_help_pages('src/common/sp-help-topics')

    for file in sys.argv[1:]:
        topics = read_help_files(file)

        # Write the topic pages.
        for topic, blocks in topics.items():
            print(topic)
            print(f'SP.Help.Header("{topic}")')
            print()
            write_as_ada_text(blocks)

        # Write the topic specs.
        for topic, blocks in topics.items():
            print(topic)

if __name__ == '__main__':
    main()



