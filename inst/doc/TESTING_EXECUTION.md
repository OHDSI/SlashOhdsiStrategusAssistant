# Execution Testing

The harness tests deterministic execution-state behavior; it does not execute a real Strategus study against patient data.

## Covered conceptually

- Execution-menu help, status, artifact inventory, exploration listing, unknown-command recovery, and safe exit in both shells.
- Step resolution, optional-step skipping, reset cascades, snapshots, restoration, and artifact inspection.
- Restart recovery: an orphaned `running` step becomes `interrupted`; automatic advance is blocked until explicit retry or reset.
- Completion probes for a successful final Strategus summary and diagnostics SQLite/DuckDB result stores. Ambiguous or invalid stores remain interrupted.

## Manual testing

Validate generated scripts individually: cohort generation, Keeper concept sets and case review, diagnostics, and final incidence or CohortMethod execution. Also test database transactions, output completeness, diagnostics explorer compatibility, real VM logout recovery, and the CohortDiagnostics storage convention installed at the deployment.
