---
name: complexity-optimizer
description: Refactor code to reduce cognitive and cyclomatic complexity while preserving behavior — guard clauses, extract-method, dispatch tables, De Morgan, decompose-conditional — then prove behavior is unchanged with tests and report before/after scores. Use whenever asked to simplify / flatten / de-nest / make code more readable or testable, to fix a complexity finding, or to act on complexity-analyzer output. WRITES CODE — gated behind approval and a green test run. Pairs with complexity-analyzer (which measures) and slots into branch-review as the fix step after findings are approved.
---

# Complexity Optimizer

Reduce complexity without changing behavior. Measure with `complexity-analyzer`,
apply the smallest set of proven refactorings, prove the behavior is unchanged, and
report what moved.

## When to use

- Acting on a `complexity-analyzer` finding (or a `branch-review` maintainability row).
- A direct "simplify / flatten / de-nest / make this testable" request.
- A hotspot is confirmed **accidental** complexity (essential complexity is not a target).

## Workflow

1. **Get the target and baseline.** Invoke `complexity-analyzer` on the target first
   (`Skill(complexity-analyzer)`) to record before-scores and the driving metric — do
   not re-derive scores here; that skill owns measurement. Skip only if the caller
   already handed you fresh scores (e.g. branch-review passing a finding through).
2. **Confirm it is accidental.** If the complexity is essential (irreducible domain
   logic), say so and stop — do not manufacture a refactor to move a number.
3. **Pick strategies** from `references/strategies.md`, cheapest lever first. Match the
   strategy to the *driving* metric: nesting → guard clauses / early return; long unit
   → extract method; wide branch on a tag → dispatch table / polymorphism; tangled
   boolean → De Morgan + extract predicate.
4. **Gate on approval before writing.** Show the plan (target, strategies, expected
   score delta) and the diff, and wait for go-ahead unless the caller already
   authorized fixes (branch-review's approved-fix step does). Never edit unrelated code
   — leave a TODO instead (repo rule).
5. **Apply the smallest change** that lands the win. One behavior-preserving
   transformation at a time; do not fold in feature changes, renames, or reformatting.
6. **Prove behavior is preserved — hard gate.** Run the covering tests (this repo:
   `LD_LIBRARY_PATH=/usr/local/lib clj -M:dev -e "(require 'clojure.test 'NS)
   (clojure.test/run-tests 'NS)"`; whole suite `bin/test-suite` before a commit that
   touches source). Green is required, not assumed. If no test covers the unit, add a
   characterization test capturing current output *before* refactoring, or say clearly
   that the change is unverified and stop.
7. **Measure after.** Invoke `complexity-analyzer` again for after-scores.
8. **Report**: before → after per metric, the strategies applied, the test result
   (pasted summary), and anything left as a TODO.

## Report format

```
Target:   src/foo.clj:120  parse-row
Before:   cognitive 31 · cyclomatic 14 · nesting 5
After:    cognitive 12 · cyclomatic 14 · nesting 2
Strategy: guard clauses (removed 3 nesting levels) · extracted `classify-row`
Tests:    NS parse-test — 18 assertions, 0 failures, 0 errors
Left:     TODO(foo.clj:150) unrelated dead branch — flagged, not touched
```

Note cyclomatic often *doesn't* drop while cognitive does — that is the point:
flattening improves readability (cognitive) without removing genuine paths
(cyclomatic). Say so rather than chasing the cyclomatic number.

## Rules

- **Behavior preservation is the contract.** A refactor that changes output is a bug,
  not an optimization. Tests green is the gate; prove it, don't claim it.
- **Smallest change first.** Don't introduce new abstraction layers, bridging types,
  or named concepts when the goal is simplification (repo rule). A dispatch table beats
  a new class hierarchy unless the caller asked for more.
- **Don't game the metric.** Never split a cohesive unit, or scatter logic across
  helpers that only ever call each other, just to lower a score — that trades cognitive
  complexity for coupling. Optimize human difficulty, not the number.
- **Stay in scope.** Touch only the target unit; leave unrelated smells as TODOs.
- **Match house style.** Follow the surrounding code's idiom even where you'd choose
  differently; flag a harmful convention rather than forking it silently.

See `references/strategies.md` for the refactoring catalog (before/after + which metric
each moves + caveats).
