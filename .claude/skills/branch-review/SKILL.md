# .claude/skills/branch-review/SKILL.md
---
name: branch-review
description: Thorough branch-vs-master review across seven lenses
---
Dispatch parallel reviewer subagents against `git diff master...HEAD`, one per lens:
1. Correctness & edge cases
2. Concurrency / crash-safety protocol
3. Reuse & duplication (flag ANY logic that exists in two paths)
4. Test coverage & fixture isolation
5. Performance regressions
6. Complexity & maintainability (invoke the `complexity-analyzer` skill over the diff; use its findings table verbatim)
7. Docs/spec drift

Rules:
- Every finding MUST cite file:line and include a repro or measurement. No unverified root causes.
- Classify Critical / Important / Nit.
- Emit a status line to the user after each agent returns.
- Finish with a single consolidated table, then STOP and ask before fixing.
