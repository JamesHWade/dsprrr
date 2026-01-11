---
name: create-reviewed-pr
description: Create a PR with automated quality gates and code review
---

# Create PR with Automated Review

Automates the complete PR creation workflow with quality gates and comprehensive code review.

## Workflow

### Step 1: Pre-flight Checks

Before starting:
- ✅ Verify on a feature branch (not main/master)
- ✅ Check for uncommitted changes
- ✅ Ensure all changes are committed
- ✅ Check if PR already exists for this branch

If any pre-flight check fails, stop and report the issue.

### Step 2: Run Quality Gates

Automatically detect and run project-specific quality gates:

**For R packages:**
```bash
# Format code
air format .

# Lint code
jarl check --fix

# R CMD check
Rscript -e "devtools::check()"

# Build pkgdown (if _pkgdown.yml exists)
Rscript -e "pkgdown::build_site(preview = FALSE)"
```

**For Python projects:**
```bash
# Format with black (if pyproject.toml or .black config exists)
black .

# Lint with ruff (if ruff.toml or pyproject.toml exists)
ruff check --fix .

# Type check with mypy (if mypy.ini or pyproject.toml exists)
mypy .

# Run tests
pytest
```

**For JavaScript/TypeScript:**
```bash
# Format with prettier (if .prettierrc exists)
npm run format || npx prettier --write .

# Lint with eslint (if .eslintrc exists)
npm run lint || npx eslint --fix .

# Type check (if TypeScript)
npm run type-check || npx tsc --noEmit

# Run tests
npm test
```

**For Go projects:**
```bash
# Format
go fmt ./...

# Lint
golangci-lint run

# Run tests
go test ./...
```

**Auto-detection logic:**
1. Check for project markers (e.g., DESCRIPTION, package.json, go.mod, pyproject.toml)
2. Run appropriate quality gates for detected project type
3. If quality gate fails, stop and report error
4. If quality gate not found, warn but continue

### Step 3: Automated PR Review

Run comprehensive code review using pr-review-toolkit:

```
/pr-review-toolkit:review-pr all
```

This runs multiple specialized review agents:
- **code-reviewer**: General code quality, bugs, best practices
- **pr-test-analyzer**: Test coverage and quality (if tests changed)
- **silent-failure-hunter**: Error handling issues (if error handling changed)
- **comment-analyzer**: Comment accuracy (if comments/docs added)
- **type-design-analyzer**: Type design quality (if types added)

Wait for all reviews to complete, then analyze results.

### Step 4: Decision Logic

Based on review results, decide whether to proceed:

**❌ If Critical Issues Found:**
```
BLOCKING: Cannot create PR

Critical issues must be fixed before creating PR:
- [List each critical issue with file:line]

Required actions:
1. Fix the critical issues listed above
2. Commit the fixes
3. Re-run /create-reviewed-pr

PR creation aborted.
```

**⚠️ If Important Issues Found:**
```
WARNING: Important issues detected

Important issues found:
- [List each important issue with file:line]

Suggestions:
- [List suggestions if any]

These issues are not blocking but should be addressed.

Do you want to:
1. Fix these issues first (recommended)
2. Create PR anyway and address in review
3. Cancel

[Wait for user choice via AskUserQuestion]
```

**✅ If Only Suggestions or Clean:**
```
READY: No blocking issues found

Review summary:
- Critical issues: 0
- Important issues: 0
- Suggestions: N
- Overall: Safe to proceed

Proceeding with PR creation...
```

### Step 5: Create PR

If approved to proceed:

1. **Get PR details** from user (or auto-generate):
   - Title: Auto-generate from commit messages or ask user
   - Description: Include:
     - Summary of changes
     - Review results summary (if helpful)
     - Link to any related issues
     - Test plan if applicable

2. **Execute PR creation:**
   ```bash
   gh pr create --title "..." --body "..."
   ```

3. **Report success:**
   ```
   ✅ PR created successfully!

   URL: https://github.com/user/repo/pull/123

   Review summary included in PR description.
   Next steps:
   - Wait for CI to complete
   - Address any CI failures
   - Request review from team
   ```

## Usage

Basic usage:
```
/create-reviewed-pr
```

The command will:
1. Auto-detect project type
2. Run appropriate quality gates
3. Run comprehensive code review
4. Guide you through any issues
5. Create PR when safe

## Options

You can customize behavior with arguments:

```
/create-reviewed-pr --skip-gates      # Skip quality gates, only review
/create-reviewed-pr --skip-review     # Skip review, only quality gates
/create-reviewed-pr --review-only     # Run review but don't create PR
/create-reviewed-pr --force          # Create PR even with warnings
```

## Configuration

Projects can configure quality gates in `.prworkflow.json`:

```json
{
  "quality_gates": [
    "npm run format",
    "npm run lint",
    "npm test"
  ],
  "skip_gates": ["build"],
  "review_aspects": ["code", "tests", "errors"]
}
```

If no config file exists, auto-detection is used.

## Examples

**Example 1: R Package (Clean Review)**
```
User: /create-reviewed-pr