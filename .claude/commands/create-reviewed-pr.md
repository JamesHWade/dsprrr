---
name: create-reviewed-pr
description: Create a PR with automated code review
---

# Create PR with Automated Review

This command automates the complete PR creation workflow:
1. Runs quality gates (formatting, linting, R CMD check)
2. Runs comprehensive automated PR review
3. Creates PR only if no critical issues found

## Workflow

**Step 1: Quality Gates**
```bash
# Format code
air format .

# Lint code
jarl check --fix

# Run R CMD check
Rscript -e "devtools::check()"

# Build pkgdown
Rscript -e "pkgdown::build_site(preview = FALSE)"
```

**Step 2: Automated Review**

Run comprehensive PR review:
```
/pr-review-toolkit:review-pr all
```

**Step 3: Evaluate Results**

If review finds **critical issues**:
- ❌ STOP - Do not create PR
- Fix critical issues first
- Re-run review to verify
- Then retry PR creation

If review finds **important issues**:
- ⚠️ WARN - Consider fixing before PR
- Ask user whether to proceed or fix first

If only **suggestions** or no issues:
- ✅ PROCEED - Safe to create PR

**Step 4: Create PR**

Once approved, create PR with:
```bash
gh pr create --title "..." --body "..."
```

Include review summary in PR description if helpful.

## Usage

Simply run:
```
/create-reviewed-pr
```

The assistant will:
1. Check git status to ensure changes are committed
2. Run all quality gates in sequence
3. Run automated PR review
4. Evaluate results and advise on next steps
5. Create PR if approved, or stop if critical issues found

## Options

You can customize by providing arguments:
```
/create-reviewed-pr --skip-review    # Skip review, just run quality gates
/create-reviewed-pr --review-only    # Only run review, don't create PR
```

## Implementation

When this command is invoked, execute these steps in order:

### Step 1: Pre-flight Checks
- Verify on a feature branch (not main)
- Check for uncommitted changes
- Ensure all changes are committed
- Check if PR already exists for this branch

### Step 2: Run Quality Gates
Execute each quality gate and stop if any fail:

```bash
# 1. Format check
air format .
if [ $? -ne 0 ]; then
  echo "❌ Formatting failed - please fix and retry"
  exit 1
fi

# 2. Lint check
jarl check --fix
if [ $? -ne 0 ]; then
  echo "❌ Linting failed - please fix and retry"
  exit 1
fi

# 3. R CMD check
Rscript -e "devtools::check()"
if [ $? -ne 0 ]; then
  echo "❌ R CMD check failed - please fix and retry"
  exit 1
fi

# 4. Build pkgdown
Rscript -e "pkgdown::build_site(preview = FALSE)"
if [ $? -ne 0 ]; then
  echo "⚠️ pkgdown build failed - continuing anyway"
fi
```

### Step 3: Automated PR Review

Run comprehensive review using pr-review-toolkit:
```
/pr-review-toolkit:review-pr all
```

Wait for review to complete, then analyze results.

### Step 4: Decision Logic

Based on review results:

**If Critical Issues Found:**
```
❌ BLOCKING: Cannot create PR

Critical issues must be fixed:
- [List critical issues]

Actions required:
1. Fix the critical issues
2. Commit the fixes
3. Re-run /create-reviewed-pr

PR creation aborted.
```

**If Important Issues Found:**
```
⚠️ WARNING: Important issues detected

Important issues found:
- [List important issues]

Recommendations:
- [List recommendations]

Do you want to:
1. Fix these issues first (recommended)
2. Create PR anyway and address in review
3. Cancel

[Wait for user choice]
```

**If Only Suggestions or Clean:**
```
✅ READY: No blocking issues

Review summary:
- Critical: 0
- Important: 0
- Suggestions: N

Proceeding with PR creation...
```

### Step 5: Create PR

If approved to proceed:

1. Get PR title and description from user (or auto-generate from commits)
2. Include review summary in PR description
3. Execute: `gh pr create --title "..." --body "..."`
4. Report PR URL to user

## Example Session

```
User: /create-reviewed-pr