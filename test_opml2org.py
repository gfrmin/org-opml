#!/usr/bin/env python3
"""
Test script for opml2org.py using example files.
"""

import subprocess
import sys
from pathlib import Path

# Assuming opml2org.py is in the same directory as this test script (project root)
OPML2ORG_SCRIPT = Path(__file__).parent / "opml2org.py"
EXAMPLES_DIR = Path(__file__).parent / "examples"

TEST_CASES = [
    "attributes",
    "nba",
]

def run_test(test_name: str) -> bool:
    """
    Runs a single test case.

    Args:
        test_name: The base name of the example files (e.g., "attributes").

    Returns:
        True if the test passes, False otherwise.
    """
    opml_file = EXAMPLES_DIR / f"{test_name}.opml"
    expected_org_file = EXAMPLES_DIR / f"{test_name}.org"

    if not opml_file.exists():
        print(f"ERROR: OPML file not found: {opml_file}")
        return False
    if not expected_org_file.exists():
        print(f"ERROR: Expected Org file not found: {expected_org_file}")
        return False

    print(f"Testing {test_name}: {opml_file} -> {expected_org_file} ... ", end="")

    try:
        # Ensure opml2org.py is executable or use python3 interpreter explicitly
        if not OPML2ORG_SCRIPT.is_file():
            print(f"ERROR: opml2org.py script not found at {OPML2ORG_SCRIPT}")
            return False

        cmd = [sys.executable, str(OPML2ORG_SCRIPT), str(opml_file)]
        result = subprocess.run(cmd, capture_output=True, text=True, check=True, encoding='utf-8')
        
        actual_org_content = result.stdout
        expected_org_content = expected_org_file.read_text(encoding='utf-8')

        # Normalize whitespace, especially trailing newlines, for comparison
        if actual_org_content.strip() == expected_org_content.strip():
            print("PASS")
            return True
        else:
            print("FAIL")
            print("Expected:")
            print("--------------------")
            print(expected_org_content.strip())
            print("--------------------")
            print("Actual:")
            print("--------------------")
            print(actual_org_content.strip())
            print("--------------------")
            return False
    except subprocess.CalledProcessError as e:
        print("FAIL (script execution error)")
        print(f"Command: {' '.join(e.cmd)}")
        print(f"Return code: {e.returncode}")
        print(f"Stdout: {e.stdout}")
        print(f"Stderr: {e.stderr}")
        return False
    except Exception as e:
        print(f"FAIL (unexpected error: {e})")
        return False

def main():
    """
    Runs all test cases.
    """
    if not OPML2ORG_SCRIPT.exists():
        print(f"Critical ERROR: opml2org.py script not found at {OPML2ORG_SCRIPT}")
        print("Please ensure opml2org.py is in the project root directory.")
        sys.exit(1)

    passed_count = 0
    failed_count = 0

    for test_name in TEST_CASES:
        if run_test(test_name):
            passed_count += 1
        else:
            failed_count += 1
    
    print("\n--- Test Summary ---")
    print(f"Total tests: {len(TEST_CASES)}")
    print(f"Passed: {passed_count}")
    print(f"Failed: {failed_count}")

    if failed_count > 0:
        sys.exit(1)
    else:
        sys.exit(0)

if __name__ == "__main__":
    main()
