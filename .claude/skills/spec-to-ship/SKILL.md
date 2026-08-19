---
name: spec-to-ship
description: Take a Linear ticket from brainstorm through spec, plan, subagent implementation, and per-task commits.
---

Given a Linear ticket ID ($ARGUMENTS):

1. Read the ticket and any linked sub-issues via the Linear MCP. Summarize scope in <=10 lines.
2. BRAINSTORM: propose 2-3 approaches with tradeoffs. Use the codebase's existing vocabulary; do NOT invent new abstractions. STOP and wait for the user to pick one.
3. SPEC: draft section-by-section. After each section, pause for review. Do not record any recommendation as decided until the user says so explicitly.
4. PLAN: numbered tasks, each independently testable and committable.
5. IMPLEMENT: one subagent per task (TDD - failing test first). Then a separate reviewer subagent. Fix findings before moving on.
6. GATE: run the full 75-namespace suite. Paste the summary. Only then commit with explicit `git add <paths>` (never -A).
7. Mirror the spec and outcome back to the Linear issue and close it.

Post a one-line status before and after every step longer than ~2 minutes.
