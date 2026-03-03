#!/usr/bin/env python3
"""
Build the project with multiple GHC versions in parallel.
Each build runs in a temporary directory with a copy of the git-tracked files.
"""
import json
import subprocess
import tempfile
import shutil
import os
import sys
import threading
from pathlib import Path
from multiprocessing import Pool

# ANSI color codes
BOLD_GREEN = "\033[1;32m"
BOLD_RED = "\033[1;31m"
CYAN = "\033[36m"
YELLOW = "\033[33m"
MAGENTA = "\033[35m"
BLUE = "\033[34m"
GREEN = "\033[32m"
RESET = "\033[0m"

# Load GHC versions from JSON file
def load_ghc_versions():
    """Load GHC versions from ghc-versions.json."""
    script_dir = os.path.dirname(os.path.abspath(__file__))
    json_path = os.path.join(script_dir, "ghc-versions.json")
    with open(json_path, 'r') as f:
        return json.load(f)

GHC_VERSIONS = load_ghc_versions()

# Colors for each GHC version prefix
GHC_COLORS = [CYAN, YELLOW, MAGENTA, BLUE, GREEN]

# Lock for thread-safe printing
print_lock = threading.Lock()


def get_git_files(repo_path):
    """Get list of files tracked by git."""
    result = subprocess.run(
        ["git", "ls-files"],
        cwd=repo_path,
        capture_output=True,
        text=True,
        check=True
    )
    return [f for f in result.stdout.strip().split('\n') if f]


def copy_repo_to_temp(repo_path, temp_dir):
    """Copy all git-tracked files to temporary directory."""
    files = get_git_files(repo_path)
    for file in files:
        src = os.path.join(repo_path, file)
        dst = os.path.join(temp_dir, file)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copy2(src, dst)


def stream_output(pipe, prefix, color):
    """Stream output from a pipe with a colored prefix."""
    for line in iter(pipe.readline, ''):
        if line:
            with print_lock:
                print(f"{color}[{prefix}]{RESET} {line}", end='', flush=True)


def build_with_ghc(ghc_version, repo_path, color):
    """Build the project with a specific GHC version."""
    prefix = f"GHC {ghc_version}"
    with print_lock:
        print(f"{color}[{prefix}]{RESET} Starting build")

    with tempfile.TemporaryDirectory() as temp_dir:
        # Copy repository to temp directory
        copy_repo_to_temp(repo_path, temp_dir)

        # Run cabal build with streaming output
        process = subprocess.Popen(
            ["cabal", "build", "all", "--ghc-options=-Werror", "-w", f"ghc-{ghc_version}"],
            cwd=temp_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1
        )

        # Stream output
        stream_output(process.stdout, prefix, color)

        # Wait for process to complete
        returncode = process.wait()

        if returncode == 0:
            with print_lock:
                print(f"{BOLD_GREEN}✓ {prefix}: Build successful{RESET}")

            # Run tests
            with print_lock:
                print(f"{color}[{prefix}]{RESET} Running tests")

            # Set up environment with HC variable
            test_env = os.environ.copy()
            test_env["HC"] = f"ghc-{ghc_version}"

            test_process = subprocess.Popen(
                ["cabal", "run", "unittests", "-w", f"ghc-{ghc_version}", "--", "--hide-successes", "-j8"],
                cwd=temp_dir,
                env=test_env,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1
            )

            # Stream test output
            stream_output(test_process.stdout, prefix, color)

            # Wait for tests to complete
            test_returncode = test_process.wait()

            if test_returncode == 0:
                with print_lock:
                    print(f"{BOLD_GREEN}✓ {prefix}: Tests passed{RESET}")
                return ghc_version, True, True
            else:
                with print_lock:
                    print(f"{BOLD_RED}✗ {prefix}: Tests failed{RESET}")
                return ghc_version, True, False
        else:
            with print_lock:
                print(f"{BOLD_RED}✗ {prefix}: Build failed{RESET}")
            return ghc_version, False, False


def main():
    repo_path = os.path.dirname(os.path.abspath(__file__))

    print(f"Building with GHC versions: {', '.join(GHC_VERSIONS)}")
    print(f"Repository: {repo_path}\n")

    # Run builds in parallel
    args = [
        (ghc_version, repo_path, GHC_COLORS[i % len(GHC_COLORS)])
        for i, ghc_version in enumerate(GHC_VERSIONS)
    ]

    with Pool() as pool:
        results = pool.starmap(build_with_ghc, args)

    # Print summary
    print("\n=== Build Summary ===")
    for ghc_version, build_success, test_success in results:
        if build_success and test_success:
            status = f"{BOLD_GREEN}✓ BUILD + TESTS PASSED{RESET}"
        elif build_success:
            status = f"{YELLOW}✓ BUILD PASSED{RESET} {BOLD_RED}✗ TESTS FAILED{RESET}"
        else:
            status = f"{BOLD_RED}✗ BUILD FAILED{RESET}"
        print(f"GHC {ghc_version}: {status}")


if __name__ == "__main__":
    main()
