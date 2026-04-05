#!/usr/bin/env python3
"""Release automation for this repository's Cabal package.

The script expects a clean worktree on ``main``, with no existing local or
remote release branch/tag for the current package version.

It then performs preflight git checks and runs:

* ``cabal build all``
* ``cabal run unittests -- --hide-successes``
* ``cabal sdist``
* ``cabal haddock lib:<package> --haddock-for-hackage``
* ``git checkout -b release/v<version>``
* ``git tag -a v<version>``
* ``git push`` for the release branch and tag
* ``cabal upload --publish`` for the source tarball and docs tarball

On failure after creating git state, it tries to delete the pushed tag/branch
and remove the local tag/branch again.
"""

import argparse
import os
import shlex
import subprocess
import sys
import tempfile
import traceback
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent
EXPECTED_BRANCH = "main"
RELEASE_BRANCH_PREFIX = "release/v"
TAG_PREFIX = "v"
REMOTE = "origin"


class ReleaseError(RuntimeError):
    pass


@dataclass
class State:
    original_branch: str
    release_branch: str
    tag_name: str
    branch_created: bool = False
    tag_created: bool = False
    remote_branch_pushed: bool = False
    remote_tag_pushed: bool = False
    source_uploaded: bool = False
    docs_uploaded: bool = False


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
    if branch != EXPECTED_BRANCH:
        raise ReleaseError(
            f"Current branch must be '{EXPECTED_BRANCH}', found '{branch}'."
        )

    if (
        run(
            ["git", "show-ref", "--verify", "--quiet", f"refs/heads/{release_branch}"],
            check=False,
        ).returncode
        == 0
    ):
        raise ReleaseError(f"Local branch '{release_branch}' already exists.")

    if (
        run(
            ["git", "show-ref", "--verify", "--quiet", f"refs/tags/{tag_name}"],
            check=False,
        ).returncode
        == 0
    ):
        raise ReleaseError(f"Local tag '{tag_name}' already exists.")

    if run(
        ["git", "ls-remote", "--heads", REMOTE, f"refs/heads/{release_branch}"],
        capture_output=True,
        check=False,
    ).stdout.strip():
        raise ReleaseError(f"Remote branch '{release_branch}' already exists.")

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


def rollback(state: State) -> None:
    step("Rolling back")

    if state.remote_tag_pushed:
        run(["git", "push", REMOTE, f":refs/tags/{state.tag_name}"], check=False)

    if state.remote_branch_pushed:
        run(["git", "push", REMOTE, "--delete", state.release_branch], check=False)

    if state.branch_created:
        run(["git", "checkout", state.original_branch], check=False)

    if state.tag_created:
        run(["git", "tag", "-d", state.tag_name], check=False)

    if state.branch_created:
        run(["git", "branch", "-D", state.release_branch], check=False)


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
    print_command(["cabal", "build", "all", "-w", compiler_name])
    print_command(
        ["cabal", "run", "unittests", "-w", compiler_name, "--", "--hide-successes"]
    )
    print_command(["cabal", "sdist", "-o", str(artifacts_dir)])
    print_command(
        docs_haddock_command(package, compiler_name, artifacts_dir / "docs-build")
    )
    print_command(["git", "checkout", "-b", release_branch])
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

    original_branch = require_preflight(release_branch, tag_name)
    state = State(
        original_branch=original_branch,
        release_branch=release_branch,
        tag_name=tag_name,
    )
    artifacts_dir = Path(
        tempfile.mkdtemp(prefix=f"{package}-{version}-release-", dir="/tmp")
    )
    log(f"Artifacts: {artifacts_dir}")

    try:
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

        step(f"Creating release branch {release_branch}")
        run(["git", "checkout", "-b", release_branch])
        state.branch_created = True

        step(f"Creating tag {tag_name}")
        run(["git", "tag", "-a", tag_name, "-m", f"Release {package} {version}"])
        state.tag_created = True

        step("Pushing release branch")
        run(["git", "push", "--set-upstream", REMOTE, release_branch])
        state.remote_branch_pushed = True

        step("Pushing tag")
        run(["git", "push", REMOTE, f"refs/tags/{tag_name}:refs/tags/{tag_name}"])
        state.remote_tag_pushed = True

        token, _ = require_hackage_token(hackage_token, False)
        source_tarball = source_tarball_path(artifacts_dir, package, version)
        docs_tarball = docs_tarball_path(artifacts_dir, package, version)

        step("Uploading source package")
        run(upload_command(token, source_tarball, False), env=env)
        state.source_uploaded = True

        step("Uploading docs package")
        run(upload_command(token, docs_tarball, True), env=env)
        state.docs_uploaded = True
    except Exception as error:
        rollback_error: Exception | None = None
        try:
            rollback(state)
        except Exception as nested_error:
            rollback_error = nested_error
        log("")
        traceback.print_exception(error)
        if rollback_error is not None:
            traceback.print_exception(rollback_error)
        if state.source_uploaded or state.docs_uploaded:
            log("Warning: Hackage upload cannot be rolled back by this script.")
        log(f"Artifacts kept at {artifacts_dir}")
        return 1
    run(["git", "checkout", original_branch])

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
