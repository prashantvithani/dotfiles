---
name: complexity-analyzer
description: Measure and report code complexity (cyclomatic, cognitive, nesting, coupling/cohesion) for a diff, file, or function, and classify each hotspot as Critical/Important/Nit with file:line and the measured score. READ-ONLY — never edits code. Use whenever asked to assess complexity, find hard-to-maintain / hard-to-test code, review a branch or PR for maintainability, gate a merge on complexity, or answer "how complex is this / where are the hotspots". Also use as the measurement half of complexity-optimizer and as the maintainability lens of branch-review.
---

# Complexity Analyzer

Measure complexity, locate the hotspots, and report them as verifiable findings.
This skill **only measures and reports** — it never modifies code. Refactoring is
`complexity-optimizer`'s job.

## When to use

- A review asks "where is this hard to maintain / test / change?"
- A branch/PR needs a maintainability lens (drops into `branch-review` as a lens).
- Before refactoring, to get baseline scores (invoked by `complexity-optimizer`).
- A merge gate on a complexity threshold.

## Workflow

1. **Scope the target.** A diff (`git diff master...HEAD` — analyze only changed
   functions, not the whole file), a file set, or a named function. When reviewing
   a branch, restrict to changed hunks so noise from untouched code stays out.
2. **Measure.** Prefer a real tool if one is on PATH; otherwise count by hand using
   the rules in `references/metrics.md`. Never guess a score — either run a tool or
   apply the counting rules line by line.
   - Multi-language: `lizard <path>` (cyclomatic + token count + params + length).
   - Python: `radon cc -s`, `radon mi`. JS/TS: ESLint `complexity` rule. Go: `gocyclo`.
   - Clojure/EDN and anything unsupported by a tool: count by hand (metrics.md).
3. **Classify** each hotspot Critical / Important / Nit against the thresholds table
   in `references/metrics.md`.
4. **Emit findings** in the contract below. Report the number, the *reason* it is
   complex (which metric, driven by what — nesting depth, branch count, coupling),
   and the cheapest lever that would move it. Do not propose the edit; name the lever.
5. **Stop.** Output findings only. Do not edit. If the user wants fixes, that is
   `complexity-optimizer` (invoked after they approve).

## Output contract

One row per hotspot, most severe first, then a one-line summary. Every row cites
`file:line` and carries the measured number — a finding with no measurement is not
a finding.

| Severity | Location | Metric (measured) | Why complex | Cheapest lever |
|----------|----------|-------------------|-------------|----------------|
| Critical | `src/foo.clj:120` `parse-row` | cognitive 31, nesting 5 | 4-deep nested `when`/`cond` inside a loop | guard clauses + extract inner cond |
| Important | `src/foo.clj:60` `route` | cyclomatic 14 | 12-branch `case` on a type tag | data-driven dispatch table |
| Nit | `src/bar.clj:12` `norm` | cognitive 11 | one avoidable nesting level | early return |

Summary line: total hotspots by severity + the single highest-leverage target.

When invoked as a `branch-review` lens, use exactly branch-review's
`Critical/Important/Nit` labels (above) so the finding merges into its consolidated
table with no translation.

## Rules

- **Read-only.** Never edit, never stage. Measurement and classification only.
- **Measure, don't assume** (matches the repo's correctness-over-speed rule). Run a
  tool or count by the rules; label any estimate you could not verify as an estimate.
- **Metric is a flag, not a verdict.** A flat 12-arm `case` scores high cyclomatic
  but is readable — say so and rank it low. Goodhart: never recommend splitting a
  cohesive unit purely to lower a number. Rank by *human* difficulty (cognitive +
  nesting + coupling), use cyclomatic mainly as a test-count signal.
- **Essential vs accidental.** If the complexity is inherent to the problem, say it
  is essential and stop — don't manufacture a refactor.

See `references/metrics.md` for definitions, counting rules, thresholds, and tools.
