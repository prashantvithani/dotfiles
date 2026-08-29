# Refactoring strategy catalog

Behavior-preserving transformations, cheapest lever first. Each entry: what it does,
which metric it moves, and when NOT to use it. Metric definitions live in
`complexity-analyzer/references/metrics.md`.

## 1. Guard clauses / early return — kills nesting
Invert a condition and return/`recur`/`throw` early so the happy path stays flat.
- Moves: **cognitive ↓ (big), nesting ↓**. Cyclomatic unchanged.
- Before: `if valid? { …20 lines… } else { return err }`
- After: `if not valid? return err; …20 lines…`
- Not when: the `else` is trivial and the `if` body is one line (inversion adds noise).

## 2. Extract method / function — shrinks a long or multi-job unit
Pull a cohesive block into a well-named helper.
- Moves: **cognitive ↓** in the caller; total cyclomatic unchanged (moved, not removed).
- Use when: a unit does several nameable things, or an inner block is deeply nested.
- Not when: the extracted piece is used only here AND only makes sense here and the
  name adds nothing — over-extraction trades readability for call-chasing (coupling ↑).

## 3. Replace nested conditional with dispatch table / polymorphism
A wide `if/else-if` or `case` on a type/tag → a data map `{tag → fn}` (or multimethod /
polymorphism). In this repo, `defmulti`/`defmethod` is the idiomatic form.
- Moves: **cyclomatic ↓ (real), cognitive ↓** when the branches were nested.
- Use when: branches dispatch on one discriminator and are open to extension.
- Not when: it's a small, closed, flat `case` — a table there just hides control flow.

## 4. Decompose / name the conditional — tames tangled booleans
Extract a complex predicate into a named boolean (`overdue? = …`); split a compound
`if` into named parts.
- Moves: **cognitive ↓**. Cyclomatic unchanged.
- Before: `if a && (b || c) && !d && e { … }`
- After: `eligible? = a && (b || c) && !d && e; if eligible? { … }`

## 5. De Morgan / simplify boolean logic — removes double negatives
`!(a && b)` → `!a || !b`; collapse `if x then true else false` → `x`; remove redundant
conditions.
- Moves: **cognitive ↓**. Cyclomatic may ↓ if a branch disappears.

## 6. Replace flag/loop accumulation with a pipeline
Imperative loop with a mutable accumulator and inner conditionals →
`map`/`filter`/`reduce` (Clojure: threading `->>` + transducers).
- Moves: **cognitive ↓, nesting ↓, cyclomatic ↓** (loop + branches become combinators).
- Not when: the loop has genuine early-exit / side-effecting order that a pipeline hides.

## 7. Consolidate duplicated conditional
The same branch structure in two places → one function parameterized over the
difference. (Also a `branch-review` reuse-lens finding.)
- Moves: **cyclomatic ↓** total, and removes a drift risk.

## 8. Introduce a small value/record to collapse "data clumps"
Several args always passed together → one map/record. Reduces param count (a lizard
warning) and the branching that validates each arg separately.
- Moves: parameter count ↓, cognitive ↓. Coupling stays local.
- Not when: the goal is simplification and this would add a named type nothing else
  needs — prefer a plain map (repo rule: no new bridging abstractions).

## Anti-patterns — do not "optimize" into these

- **Metric-gaming split.** Cutting a cohesive function into `foo-1`/`foo-2`/`foo-3`
  that only call each other lowers per-unit cognitive score but raises coupling and
  makes the flow unreadable. Net complexity ↑. Refuse it.
- **Premature polymorphism.** Turning a 3-arm flat `case` into a class/multimethod
  hierarchy — more indirection than it removes.
- **Boolean-parameter flags.** Splitting on a `do-x?` boolean arg instead of two clear
  functions.
- **Comment-as-fix.** Leaving the complexity and adding a comment is not a refactor.

## Verification reminder

Every strategy here is behavior-preserving *only if the tests prove it*. Run the
covering tests after each transformation (repo commands in the optimizer SKILL.md).
If none cover the unit, write a characterization test first. No green run → the
refactor is unverified; say so and stop.
