---
name: nbreview
description: This skill should be used when the user asks to "review this notebook", "nbreview", "check my notebook", or requests a Jupyter notebook review. Produces a findings-only report covering header metadata, knobs, structure, content, execution state, figure quality, privacy/secrets, and Python environment for a single `.ipynb` file.
---

# Jupyter Notebook Review

Review a single Jupyter notebook. The path is usually provided as an argument.

## If no path is provided

Guess the most likely notebook (e.g. the one modified in `git status`, or the only `.ipynb` in the directory), then ask the user to confirm before reviewing. Do not start the review until confirmed.

## Before reviewing

- The notebook may be open in Jupyter. Check for unsaved modifications or on-disk changes before reading, and never edit without re-reading first.
- Read the notebook top to bottom once before writing any findings.

## What to check

### 1. Header metadata (first markdown cell)

- **Title**, **author**, and **year**.
- One sentence stating the notebook's purpose — what it produces, explores, or answers. Examples:
  - "Cleans the raw enrollment export into a tidy per-student table."
  - "Plots DFW rates over time for intro CS."
  - "Answers: did DFW rates change after the 2020 curriculum revision?"
- Input files read and output files written. Keep it short; summarize if many.
- Staleness indicators: a cell near the top that prints the **execution timestamp** and the **git commit hash + dirty status**. A minimal pattern (place `subprocess` and `datetime` with the other imports):

  ```python
  print(f"Executed: {datetime.datetime.now():%Y-%m-%d %H:%M}")
  sha = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD'], text=True).strip()
  dirty = subprocess.check_output(['git', 'status', '--porcelain'], text=True).strip()
  print(f"Git: {sha}{' (dirty)' if dirty else ''}")
  ```
- Imports are collected in a single code cell near the top, not scattered through the notebook.

### 2. Knobs (tunable parameters)

- The **important knobs** — the variables a reader is most likely to want to change and re-run the notebook with different values for (e.g. the course/term being analyzed, a headline threshold, the primary input/output path) — are collected in a **single named-variables cell near the top**, preceded by a markdown cell that describes each one: what it controls, valid values, and units where applicable.
- "Important" is a judgment call, not a checklist. A useful test: would changing this value alone produce a different, still-sensible run of the notebook? If yes, it probably belongs up top. If it only makes sense alongside edits to a specific cell's logic, it can live where it's used.
- Variables defined later in the notebook, at their point of use, are **fine** — don't flag a variable just because it lives mid-notebook. Only flag when an important knob is hidden mid-notebook in a way that forces a reconfigurer to hunt for it, or when the same value is hard-coded in several places that would need to change together.
- If the notebook uses randomness in computations that feed results, a **seed** is one of the important knobs and its effect is noted. Do not flag `.sample()` / `.head()` / similar calls used only for a quick visual spot-check — reproducibility of a glance doesn't matter.

### 3. Structure

- A short markdown cell precedes each code cell (rare exceptions OK).
  - Markdown opens with a human-readable one-liner. Extended content goes under a bold **Details:** label so readers can skim.
- Notebook is not excessively long — recommend splitting if it is.
- Notebooks in the directory are numbered in the order a reader should execute them (e.g. `01-…`, `02-…`).
- A `README` exists in the directory.
- Long-running cells (more than ~30 seconds) are called out in the preceding markdown so a reader knows what to expect.

### 4. Content

- Summary and analysis markdown is **in sync** with current code outputs (numbers, figures, tables). Flag any claim that contradicts or is not supported by the visible output.
- Summary/analysis references specific values from outputs, so a future reader can tell whether the text has gone stale relative to a re-run.
- Code takes advantage of libraries. Flag long, hand-rolled blocks that a standard library call would replace.
- Comments appear only where the **why** is non-obvious. Flag comments that merely restate what the code does.
- **Readability beats DRY.** A notebook is read top-to-bottom, and a cell should be understandable without scrolling away to chase a definition. **Do not recommend hoisting** constants, label lists, format strings, column selections, or short helpers out of their use site into a distant "setup" or "knobs" cell just to eliminate duplication — that separates the reader from the context they need. Knobs are for things a *reader* would want to change; working constants that belong to one analysis block should stay in that block.
- **Parallel analysis blocks** (e.g. the same model/plot repeated across several DVs, IVs, groups, or thresholds) are a legitimate notebook pattern. Each block sits next to its own markdown heading and output, and is easier to read, re-run, and tweak in isolation than a loop over a helper. **Duplicated boilerplate inside these blocks is part of the pattern** — do not flag it, and do not suggest factoring out the shared lines. The test: if removing the duplication would force a reader to scroll elsewhere to understand any single block in isolation, leave it alone.
- Functions longer than a few lines, or reused across notebooks, should live in a `.py` module rather than being defined (or copy-pasted) inline. (This is the opposite case from the one above: a genuine helper with a name lives better in a module; duplicated boilerplate inside parallel blocks lives better in place.)
- Paths are relative to the notebook or held in a config variable — no absolute paths like `/Users/you/…`.

### 5. Execution state

- "Restart & Run All" completes without error.
- Cell execution counts are monotonic (`[1]`, `[2]`, `[3]`, …) — no out-of-order numbers indicating cells were run piecemeal.
- Outputs committed to the notebook reflect the latest code (or, if the repo convention is to strip outputs, all outputs are cleared — pick one and flag deviations).

### 6. Figures

- Axes have labels **with units** where applicable.
- Font sizes are legible at the size the figure will be viewed/printed.
- A title or caption states what the figure shows.
- Legends are present when more than one series is plotted.
- Color palette is colorblind-safe.

### 7. Privacy and secrets

- No PII: student names, IDs, emails, or other identifying fields in committed outputs, sample data, or markdown.
- No secrets: API keys, passwords, connection strings, tokens. If the notebook needs one, it should be loaded from an environment variable or a gitignored config file.

### 8. Environment

- A Python virtual environment is in use, with a `requirements.txt` that has **pinned** version numbers.

## Reporting

Produce a **findings-only** report — skip items that pass, list only issues.

- Group findings by the section headings above (Header metadata, Knobs, Structure, Content, Execution state, Figures, Privacy and secrets, Environment).
- For each finding, include:
  - A short description of the issue.
  - A reference to the cell (cell index, or a short quoted snippet if the index isn't obvious).
  - A suggested fix in one line.
- If there are no findings in a section, omit the section entirely.
- End the report with:

  > Reply with a list of fixes to apply, or "fix all" to apply everything.

Do not edit the notebook during the review. Wait for the user's reply before applying any fix.
