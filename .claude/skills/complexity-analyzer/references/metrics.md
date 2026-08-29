# Complexity metrics: definitions, counting rules, thresholds

Source of truth for what "complexity" means in `complexity-analyzer` and
`complexity-optimizer`. Both skills read this file; keep thresholds here only.

## The metrics

### Cyclomatic complexity (McCabe) — path count / test floor
Independent paths through a unit. `1 + (number of decision points)`.
Count +1 for each: `if`, `else if`/`elif`, ternary, each `case`/`when`/`cond`
branch, `&&`/`||`/`and`/`or` (each extra operand), each loop (`for`/`while`/`doseq`),
each `catch`, each early `return`/`recur` guard.
Best read as **the minimum number of tests to cover the unit**. High cyclomatic on a
flat dispatch (a big `case`) is not itself a readability problem.

### Cognitive complexity (SonarSource) — human difficulty
Models how hard code is to *read*. Two rules:
- **+1** for each break in linear flow: `if`, `else`, ternary, `switch`/`case`,
  loops, `catch`, and a *sequence* of mixed boolean operators (`a && b || c` = +1).
- **+ nesting increment**: each of the above gets **+1 extra per level of nesting**
  it sits inside. A branch at depth 3 costs 3, not 1.
- **No penalty** for a flat `switch`/`case` with many arms, or for extracted helper
  calls. This is the key difference from cyclomatic: nesting is punished, breadth is not.
Cognitive complexity is the best single proxy for maintainability — rank by it first.

### Nesting depth
Max indentation levels of control flow in a unit. The strongest driver of cognitive
complexity and the cheapest to fix (guard clauses / early return).

### Coupling (module scale)
- **Fan-out (Ce, efferent):** how many other modules this one depends on.
- **Fan-in (Ca, afferent):** how many depend on it.
- **Instability** `I = Ce / (Ce + Ca)` ∈ [0,1]. 0 = maximally stable (depended-on,
  depends on nothing), 1 = maximally unstable. Stable modules should be abstract;
  a concrete, volatile module that everything depends on is the danger sign.
- In this repo, watch **backwards namespace dependencies** and a low-level ns that
  reaches up (e.g. `config`/`partition-spec` are meant to sit at the bottom).

### Cohesion (module scale)
Do a unit's parts belong together? Low cohesion (a function/ns doing several
unrelated jobs; fields used by disjoint method sets — high LCOM) predicts change cost
and defects. High cohesion + low coupling is the target.

### Halstead / Maintainability Index
Composite scores (MI blends Halstead volume + cyclomatic + LOC). Useful only as a
*trend* or cross-file comparison, never as an absolute gate. Report if a tool emits
it; don't hand-compute it.

## Thresholds → severity

Per **function/unit** (map to branch-review's labels):

| Severity  | Cognitive | Cyclomatic | Nesting | Also |
|-----------|-----------|------------|---------|------|
| Critical  | > 25      | > 20       | > 4     | long AND deeply nested; a unit no one test can pin down |
| Important | 15–25     | 11–20      | 3–4     | module: unstable + high fan-in, or clearly low cohesion |
| Nit       | 10–15     | 8–10       | 3       | one avoidable nesting level; a boolean that reads backwards |
| (ignore)  | < 10      | < 8        | ≤ 2     | leave it alone |

Thresholds are review triggers, not merge-blockers by themselves. A high number on
**essential** complexity (irreducible domain logic) is reported as essential and not
counted against the author.

## Counting by hand (when no tool supports the language)

Applies to Clojure/EDN and any language without a tool on PATH.

1. Walk the unit top to bottom.
2. Cyclomatic: start at 1; +1 per decision point (list above). For `cond`/`case`,
   count each clause; for `and`/`or`, count each operand beyond the first.
3. Cognitive: +1 per flow break, plus the current nesting level as an extra. Track
   nesting: each enclosing `if`/`when`/`cond`/loop/`let`-with-branching adds a level.
   Threading macros (`->`/`->>`) and flat `let` bindings do **not** add nesting.
4. Nesting: the deepest control-flow indentation reached.
5. Show the arithmetic in the finding's "Why complex" cell (e.g. "cognitive 31 =
   loop(1) + when@2 + cond 3 arms@3+3+3 …") so the score is auditable.

Clojure notes: `cond`/`condp`/`case` arms each count; `some->`/`when-let`/`if-let`
are one decision each; a big flat `case` dispatch is high cyclomatic but low
cognitive — rank it low. `defmulti`/`defmethod` *reduces* per-unit complexity by
turning a branch into dispatch; don't flag a multimethod for "missing" a `case`.

## Tools cheatsheet

| Language | Tool | Command |
|----------|------|---------|
| Many (C/C++/Java/JS/Py/Go/…) | lizard | `lizard -C 15 -L 60 <path>` (warns over thresholds) |
| Python | radon | `radon cc -s <path>` · `radon mi <path>` |
| JS/TS | ESLint | `complexity` + `max-depth` rules |
| Go | gocyclo | `gocyclo -over 10 <path>` |
| Clojure | — | no complexity tool; `clj-kondo` finds smells but not scores → count by hand |

`lizard` does **not** parse Clojure — do not run it on `.clj`/`.edn` and report the
(empty/garbage) output as a result; count those by hand.
