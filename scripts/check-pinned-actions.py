#!/usr/bin/env python3
"""Fail if any GitHub Action or reusable workflow is referenced by a mutable ref.

Tags (@v4) and branches (@main) can be repointed by whoever controls the
action's repo, silently changing what runs in our CI. Only a full commit SHA
is immutable. So every `uses:` must be one of:

  ./path/to/local/action                     (local; exempt)
  owner/repo[/path]@<40-hex commit sha>      (remote action or reusable workflow)
  docker://image[:tag]@sha256:<64-hex>       (Docker image, pinned by digest)

Usage:
  scripts/check-pinned-actions.py            # check .github/ of this repo
  scripts/check-pinned-actions.py FILE...    # check specific files

Uses only the Python standard library, so it runs on a stock GitHub runner or
locally. Rather than a full YAML parser, it scans lines for `uses:` keys,
skipping comments and block scalars (`run: |`). It fails closed: a `uses`
written in a form it doesn't understand (e.g. a flow mapping `{uses: ...}`)
is reported as an error rather than ignored.
"""

import os
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# A `uses` key in block style: `uses: x`, `- uses: x`, `"uses": x`.
USES_LINE = re.compile(r"""^(?P<indent>\s*(?:-\s+)*)["']?uses["']?\s*:(?:\s+(?P<value>.*))?$""")
# A `uses` key in a flow mapping, e.g. `- { uses: x }`; not supported, so flagged.
FLOW_USES = re.compile(r"""[{,]\s*["']?uses["']?\s*:""")
# A line whose value starts a block scalar (`run: |`, `- >-`), and the column its key starts at.
BLOCK_SCALAR = re.compile(r"""^(?P<indent>\s*(?:-\s+)*)(?P<key>[^#\s][^#]*:\s+)?[|>][-+0-9]*\s*(?:#.*)?$""")

SHA1 = re.compile(r"^[0-9a-f]{40}$")
REMOTE = re.compile(r"^(?P<repo>[^/@\s]+/[^@\s]+)@(?P<ref>[^@\s]+)$")
DOCKER_DIGEST = re.compile(r"^docker://[^@\s]+@sha256:[0-9a-f]{64}$")

REMOTE_FIX = (
    "pin it to a full commit SHA, e.g. `uses: owner/repo@<40-hex-sha> # v1.2.3`; "
    "find the SHA with `gh api repos/OWNER/REPO/commits/REF --jq .sha`"
)
DOCKER_FIX = (
    "pin it by digest, e.g. `uses: docker://image:tag@sha256:<digest>`; "
    "find the digest with `docker buildx imagetools inspect image:tag`"
)


def workflow_files(root):
    """The files GitHub reads action references from."""
    gh = root / ".github"
    files = [p for ext in ("yml", "yaml") for p in (gh / "workflows").glob(f"*.{ext}")]
    files += [p for ext in ("yml", "yaml") for p in (gh / "actions").glob(f"**/action.{ext}")]
    return sorted(files)


def unquote(raw):
    """The scalar value of `raw`, minus quotes and any trailing `# comment`."""
    raw = raw.strip()
    if raw[:1] in ("'", '"'):
        end = raw.find(raw[0], 1)
        return raw[1:end] if end > 0 else raw
    return re.split(r"\s#", raw, maxsplit=1)[0].strip()


def check_value(value):
    """None if `value` is an acceptable `uses:` target, else an error message."""
    if value.startswith("./"):
        return None
    if "${{" in value:
        return "is an expression, so its pin can't be verified; " + REMOTE_FIX
    if value.startswith("docker://"):
        return None if DOCKER_DIGEST.match(value) else "is a Docker image not pinned by digest; " + DOCKER_FIX
    m = REMOTE.match(value)
    if not m:
        return "is not of the form owner/repo[/path]@ref; " + REMOTE_FIX
    if not SHA1.match(m["ref"]):
        return f"uses ref '{m['ref']}', which is not a full 40-character commit SHA; " + REMOTE_FIX
    return None


def check_file(path):
    """Yield (line number, offending text, error message) for each bad `uses:` in `path`."""
    block_indent = None  # Set while inside a block scalar: content is indented more than this.
    for lineno, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        indent = len(line) - len(line.lstrip())
        if block_indent is not None:
            if not line.strip() or indent > block_indent:
                continue
            block_indent = None
        if not line.strip() or line.lstrip().startswith("#"):
            continue

        if m := USES_LINE.match(line):
            value = unquote(m["value"] or "")
            if not value:
                yield lineno, "uses:", "has no value on the same line; write `uses: <ref>` on one line"
            elif err := check_value(value):
                yield lineno, f"uses: {value}", err
        elif m := BLOCK_SCALAR.match(line):
            # `key: |` content is indented past the key; `- |` content past the dash.
            block_indent = len(m["indent"]) if m["key"] else indent
        elif FLOW_USES.search(line):
            yield lineno, line.strip(), "is a `uses` in a flow mapping, which this check can't parse; write it as a block-style `uses: <ref>` line"


def main(argv):
    files = [Path(a) for a in argv[1:]] or workflow_files(REPO_ROOT)
    in_actions = os.environ.get("GITHUB_ACTIONS") == "true"
    errors = 0
    for path in files:
        try:
            shown = path.resolve().relative_to(REPO_ROOT)
        except ValueError:
            shown = path
        for lineno, text, err in check_file(path):
            errors += 1
            print(f"{shown}:{lineno}: `{text}` {err}")
            if in_actions:
                print(f"::error file={shown},line={lineno},title=Unpinned action::`{text}` {err}")
    if errors:
        print(f"\n{errors} unpinned action reference(s). See 'Pinning GitHub Actions' in README.md.")
        return 1
    print(f"OK: all action references in {len(files)} file(s) are pinned.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
