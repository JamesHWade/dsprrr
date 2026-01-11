# Claude Code Commands for dsprrr

This directory contains custom commands for Claude Code that automate common workflows.

## Available Commands

### `/create-reviewed-pr`

Automates PR creation with quality gates and comprehensive code review.

**What it does:**
1. Pre-flight checks (branch validation, uncommitted changes)
2. Runs project quality gates (formatting, linting, R CMD check, pkgdown)
3. Comprehensive automated code review via pr-review-toolkit
4. Blocks/warns based on severity of issues found
5. Creates PR only when safe to proceed

**Usage:**
```
/create-reviewed-pr
```

See `commands/create-reviewed-pr.md` for full documentation.

## Sharing Across Projects

To use these commands in other projects:

**Option 1: Copy to other projects**
```bash
# From another project directory
cp /path/to/dsprrr/.claude/commands/create-reviewed-pr.md .claude/commands/
```

**Option 2: Create a symlink**
```bash
# From another project directory
mkdir -p .claude/commands
ln -s ~/Projects/dsprrr/.claude/commands/create-reviewed-pr.md .claude/commands/
```

**Option 3: Template for new projects**
When creating a new R package, copy the entire `.claude/` directory:
```bash
cp -r ~/Projects/dsprrr/.claude ~/Projects/new-project/
```

## Why Project-Level Commands?

Commands in `.claude/commands/` are:
- ✅ **Version controlled** - committed to the repo
- ✅ **Project-specific** - can be customized per project
- ✅ **Team-shareable** - other contributors get them automatically
- ✅ **Immediately available** - no plugin installation needed
- ✅ **Simple** - just markdown files

## Adding New Commands

1. Create a new `.md` file in `.claude/commands/`
2. Add frontmatter with `name` and `description`
3. Write the command implementation
4. The command will be automatically available as `/command-name`

Example:
```markdown
---
name: my-command
description: Does something useful
---

# My Command

When invoked, do the following:
1. Step one
2. Step two
```

Then use: `/my-command`
