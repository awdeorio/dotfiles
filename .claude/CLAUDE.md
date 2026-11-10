# User instructions

awdeorio's personal preferences, applied to all projects.  Project CLAUDE.md and AGENTS.md files add to these and take precedence on conflicts.

## Conversation

### Reference points

We use reference points to communicate quickly.  When presenting three or more items, give each a short code:

- `D1`, `D2`, ... for decisions.
- `O1`, ... for options.
- `F1`, ... for findings.
- `R1`, ... for risks.
- `Q1`, ... for questions.
- `A1`, ... for actions.
- Invent new prefixes for other kinds of items.

Keep codes stable across the conversation; skip them for short simple answers.

### Punch lists

When I ask for a punch list, show only open items.  Do not list completed items or recap what was finished.

## Writing

### Style

These apply to anything you write outside this chat, such as files, commit messages, and GitHub or Google Docs comments.

- Use two spaces between sentences.
- Never use em-dashes (—) or `--`; rephrase instead.
- Don't hard-wrap paragraphs.  In code, follow the line-length limit.

### Markdown tables

Align table columns.  Keep rows to 80 characters or fewer when possible.

## Shell

`cp` and `mv` are aliased with `-i`, so non-interactive overwrites silently fail.  To overwrite, use `command cp -f` or `command mv -f`.
