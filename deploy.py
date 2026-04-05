#!/usr/bin/env python3
"""Release automation for this repository's Cabal package.

The script expects a clean worktree on ``release/v<version>``, with no
existing local or remote tag for the current package version. The release
branch must already exist before this script runs.

It then performs preflight git checks and runs:

* ``cabal check``
* ``cabal build all``
* ``cabal run unittests -- --hide-successes``
* ``cabal sdist``
* ``cabal haddock lib:<package> --haddock-for-hackage``
* ``git tag -a v<version>``
* ``git push`` for the release branch and tag
* ``cabal upload --publish`` for the source tarball and docs tarball
"""

import argparse
import os
import shlex
import subprocess
import sys
import tempfile
import traceback
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent
RELEASE_BRANCH_PREFIX = "release/v"
TAG_PREFIX = "v"
REMOTE = "origin"


class ReleaseError(RuntimeError):
    pass


def log(message: str) -> None:
    print(message, flush=True)


def step(message: str) -> None:
    log(f"\n==> {message}")


def format_command(command: list[str]) -> str:
    redacted: list[str] = []
    hide_next = False
    for part in command:
        if hide_next:
            redacted.append("<HACKAGE_TOKEN>")
            hide_next = False
            continue
        redacted.append(part)
        if part in {"--token", "-t"}:
            hide_next = True
    return " ".join(shlex.quote(part) for part in redacted)


def run(
    command: list[str],
    capture_output: bool = False,
    env: dict[str, str] | None = None,
    check: bool = True,
) -> subprocess.CompletedProcess[str]:
    log(f"$ {format_command(command)}")
    try:
        return subprocess.run(
            command,
            cwd=REPO_ROOT,
            env=env,
            text=True,
            stdout=subprocess.PIPE if capture_output else None,
            stderr=subprocess.PIPE if capture_output else None,
            check=check,
        )
    except subprocess.CalledProcessError as error:
        if capture_output:
            if error.stdout:
                sys.stdout.write(error.stdout)
            if error.stderr:
                sys.stderr.write(error.stderr)
        raise ReleaseError(
            f"Command failed with exit code {error.returncode}: {format_command(command)}"
        ) from error
    except OSError as error:
        raise ReleaseError(
            f"Failed to start command: {format_command(command)}"
        ) from error


def print_command(command: list[str]) -> None:
    log(f"$ {format_command(command)}")


def find_cabal_file() -> Path:
    cabal_files = sorted(REPO_ROOT.glob("*.cabal"))
    if len(cabal_files) != 1:
        raise ReleaseError("Expected exactly one .cabal file in the repository root.")
    return cabal_files[0]


def parse_package(cabal_file: Path) -> tuple[str, str]:
    name = ""
    version = ""

    for line in cabal_file.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith("name:") and not name:
            name = stripped.split(":", 1)[1].strip()
        if stripped.startswith("version:") and not version:
            version = stripped.split(":", 1)[1].strip()

    if not name or not version:
        raise ReleaseError(f"Could not parse name/version from {cabal_file.name}.")

    return name, version


def current_branch() -> str:
    branch = run(
        ["git", "branch", "--show-current"], capture_output=True
    ).stdout.strip()
    if not branch:
        raise ReleaseError("Detached HEAD is not supported.")
    return branch


def dirty_paths() -> list[str]:
    status = run(
        ["git", "status", "--porcelain"], capture_output=True
    ).stdout.splitlines()
    paths: set[str] = set()
    for line in status:
        if not line:
            continue
        path_text = line[3:]
        if " -> " in path_text:
            old_path, new_path = path_text.split(" -> ", 1)
            paths.add(old_path)
            paths.add(new_path)
        else:
            paths.add(path_text)
    return sorted(paths)


def require_preflight(
    release_branch: str,
    tag_name: str,
) -> str:
    paths = dirty_paths()
    if paths:
        raise ReleaseError("Worktree must be clean. Dirty paths: " + ", ".join(paths))

    branch = current_branch()
    if branch != release_branch:
        raise ReleaseError(
            f"Current branch must be '{release_branch}', found '{branch}'."
        )

    if (
        run(
            ["git", "show-ref", "--verify", "--quiet", f"refs/tags/{tag_name}"],
            check=False,
        ).returncode
        == 0
    ):
        raise ReleaseError(f"Local tag '{tag_name}' already exists.")

    if run(
        ["git", "ls-remote", "--tags", REMOTE, f"refs/tags/{tag_name}"],
        capture_output=True,
        check=False,
    ).stdout.strip():
        raise ReleaseError(f"Remote tag '{tag_name}' already exists.")

    return branch


def compiler() -> str:
    return os.environ.get("HC", "ghc")


def release_env() -> dict[str, str]:
    env = os.environ.copy()
    env.setdefault("HC", compiler())
    return env


def source_tarball_path(artifacts_dir: Path, package: str, version: str) -> Path:
    return artifacts_dir / f"{package}-{version}.tar.gz"


def docs_tarball_path(artifacts_dir: Path, package: str, version: str) -> Path:
    return artifacts_dir / "docs-build" / f"{package}-{version}-docs.tar.gz"


def docs_haddock_command(
    package: str,
    compiler_name: str,
    builddir: Path,
) -> list[str]:
    return [
        "cabal",
        "haddock",
        f"lib:{package}",
        f"--builddir={builddir}",
        "--haddock-for-hackage",
        "-w",
        compiler_name,
    ]


def require_hackage_token(
    hackage_token: str | None,
    dry_run: bool,
) -> tuple[str, list[str]]:
    warnings: list[str] = []
    if hackage_token:
        return hackage_token, warnings
    if dry_run:
        warnings.append("Pass --hackage-token before publishing to Hackage.")
        return "<HACKAGE_TOKEN>", warnings
    raise ReleaseError("Pass --hackage-token before publishing to Hackage.")


def upload_command(
    token: str,
    tarball: Path,
    documentation: bool,
) -> list[str]:
    command = [
        "cabal",
        "upload",
        "--publish",
        "--token",
        token,
    ]
    if documentation:
        command.append("--documentation")
    command.append(str(tarball))
    return command


def dry_run(package: str, version: str, hackage_token: str | None) -> int:
    release_branch = f"{RELEASE_BRANCH_PREFIX}{version}"
    tag_name = f"{TAG_PREFIX}{version}"
    env = release_env()
    compiler_name = env["HC"]
    artifacts_dir = Path(f"/tmp/{package}-{version}-release-XXXXXX")

    step("Collecting release metadata")
    log(f"Package: {package}")
    log(f"Version: {version}")
    log(f"Compiler: {compiler_name}")
    log(f"Artifacts (planned): {artifacts_dir}")

    warnings: list[str] = []

    try:
        branch = require_preflight(release_branch, tag_name)
        log(f"Current branch: {branch}")
    except ReleaseError as error:
        warnings.append(str(error))

    token, token_warnings = require_hackage_token(hackage_token, True)
    warnings.extend(token_warnings)

    source_tarball = source_tarball_path(artifacts_dir, package, version)
    docs_tarball = docs_tarball_path(artifacts_dir, package, version)

    step("Planned side-effecting commands")
    print_command(["cabal", "check"])
    print_command(["cabal", "build", "all", "-w", compiler_name])
    print_command(
        ["cabal", "run", "unittests", "-w", compiler_name, "--", "--hide-successes"]
    )
    print_command(["cabal", "sdist", "-o", str(artifacts_dir)])
    print_command(
        docs_haddock_command(package, compiler_name, artifacts_dir / "docs-build")
    )
    print_command(["git", "tag", "-a", tag_name, "-m", f"Release {package} {version}"])
    print_command(["git", "push", "--set-upstream", REMOTE, release_branch])
    print_command(["git", "push", REMOTE, f"refs/tags/{tag_name}:refs/tags/{tag_name}"])
    print_command(upload_command(token, source_tarball, False))
    print_command(upload_command(token, docs_tarball, True))

    step("Dry run complete")
    log("Read-only checks ran. The commands above were not executed.")

    if warnings:
        step("Preflight warnings")
        for warning in warnings:
            log(f"Warning: {warning}")
        return 1

    return 0


def release(package: str, version: str, hackage_token: str | None) -> int:
    release_branch = f"{RELEASE_BRANCH_PREFIX}{version}"
    tag_name = f"{TAG_PREFIX}{version}"
    env = release_env()
    compiler_name = env["HC"]

    step("Collecting release metadata")
    log(f"Package: {package}")
    log(f"Version: {version}")
    log(f"Compiler: {compiler_name}")

    require_preflight(release_branch, tag_name)
    artifacts_dir = Path(
        tempfile.mkdtemp(prefix=f"{package}-{version}-release-", dir="/tmp")
    )
    log(f"Artifacts: {artifacts_dir}")
    source_uploaded = False
    docs_uploaded = False

    try:
        step("Running cabal check")
        run(["cabal", "check"], env=env)

        step("Building")
        run(["cabal", "build", "all", "-w", compiler_name], env=env)

        step("Running tests")
        run(
            [
                "cabal",
                "run",
                "unittests",
                "-w",
                compiler_name,
                "--",
                "--hide-successes",
            ],
            env=env,
        )

        step("Creating source distribution")
        run(["cabal", "sdist", "-o", str(artifacts_dir)])

        step("Creating docs distribution")
        run(
            docs_haddock_command(package, compiler_name, artifacts_dir / "docs-build"),
            env=env,
        )

        step(f"Creating tag {tag_name}")
        run(["git", "tag", "-a", tag_name, "-m", f"Release {package} {version}"])

        step("Pushing release branch")
        run(["git", "push", "--set-upstream", REMOTE, release_branch])

        step("Pushing tag")
        run(["git", "push", REMOTE, f"refs/tags/{tag_name}:refs/tags/{tag_name}"])

        token, _ = require_hackage_token(hackage_token, False)
        source_tarball = source_tarball_path(artifacts_dir, package, version)
        docs_tarball = docs_tarball_path(artifacts_dir, package, version)

        step("Uploading source package")
        run(upload_command(token, source_tarball, False), env=env)
        source_uploaded = True

        step("Uploading docs package")
        run(upload_command(token, docs_tarball, True), env=env)
        docs_uploaded = True
    except Exception as error:
        log("")
        traceback.print_exception(error)
        if source_uploaded or docs_uploaded:
            log("Warning: Hackage upload cannot be rolled back by this script.")
        log("Warning: Git branch and tag state were left in place for manual cleanup.")
        log(f"Artifacts kept at {artifacts_dir}")
        return 1

    step("Release complete")
    log(f"Release branch: {release_branch}")
    log(f"Tag: {tag_name}")
    log(f"Artifacts: {artifacts_dir}")
    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Release the package in this repository."
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Run read-only checks and print the commands that would be executed.",
    )
    parser.add_argument(
        "--hackage-token",
        help="Hackage token to use for cabal upload.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        package, version = parse_package(find_cabal_file())
        if args.dry_run:
            return dry_run(package, version, args.hackage_token)
        return release(package, version, args.hackage_token)
    except ReleaseError as error:
        traceback.print_exception(error)
        return 1


if __name__ == "__main__":
    sys.exit(main())
