# Agent Guide

## ACP-Assisted Cohort Creation And Keeper State

The incidence and cohort-method shells support a review-gated `create` acquisition path for target, comparator, and outcome cohorts. Keep the following behavior intact:

- Narrative scope confirmation, candidate retrieval, CSV/manifest review, explicit policy approval, and Capr/Circe emission are separate gates. Do not make clinical or concept-policy choices on the user's behalf.
- A candidate limit is a bounded retrieval slice. Required review sessions can request up to 500 candidates; large review packages should remain durable CSV/manifest artifacts rather than terminal dumps.
- Preserve `outputs/phenotype-make-computable/`, `confirmed-scope.json`, review manifests, exact policy approvals, and generated Capr/Circe artifacts as resume state. A later session may need to continue review without ACP session memory.
- Keeper concept-set preparation can precede cohort generation, but case review must wait for generated cohort rows. Profile extraction receives only the bounded per-lane input written to `keeper-case-review/rows/*_profile_input.json`; do not replace the full approved artifact.
- Keeper adjudication requires a non-generic clinical phenotype label. Recover it from explicit role overrides, intent metadata, or saved study state; fail with a user-actionable request if no label exists. Archive earlier generic-label reviews before an intentional rerun.
- ACP validation is technical validation, not clinical validation. The LLM sees only sanitized Keeper summaries; preserve this fail-closed boundary.

## R and HADES environment

This package is tested against a working HADES installation made available through the
project `renv` library. Before diagnosing dependency failures or running package checks,
verify that R is using that library rather than an empty renv cache.

1. Locate the project-specific R library under `renv/library/` for the active platform,
   R version, and architecture. It may be a symbolic link to a shared HADES library.
2. Use it explicitly through `R_LIBS_USER` and start ad-hoc R commands with `--vanilla`.
3. Confirm availability before testing. The key packages include `Strategus`,
   `CohortGenerator`, `CohortIncidence`, `CohortMethod`, `DatabaseConnector`, and
   `PhenotypeLibrary`.

Use a placeholder such as `<renv-library>` rather than assuming a machine-specific path:

```sh
R_LIBS_USER=<renv-library> Rscript --vanilla -e   'stopifnot(requireNamespace("Strategus", quietly = TRUE))'
```

The project `.Rprofile` activates renv. If it redirects R away from the intended working
library during `R CMD build` or `R CMD check`, disable the user profile for that command
and retain the explicit library:

```sh
R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null R_LIBS_USER=<renv-library>   R CMD check --no-manual <package-tarball>
```

Do not run `renv::restore()` or modify the lockfile merely to make a check pass unless the
user explicitly asks to change dependency state.

## Testing

- Use the standard `testthat` entrypoint while developing:

  ```sh
  R_LIBS_USER=<renv-library> Rscript --vanilla -e 'testthat::test_local(".")'
  ```

- Build first, then run `R CMD check --no-manual` against the tarball for an installed
  package test.
- `slashOhdsiAcpClient` is optional. If it is intentionally unavailable, run a second
  check with `_R_CHECK_FORCE_SUGGESTS_=false`; report the missing optional package rather
  than treating that result as a code failure.
- Keep the test suite in `tests/testthat/` and use `tests/testthat.R` as the test runner.
  Prefer tests of workflow-local acquisition artifacts over tests that need network ACP
  access or a live database.

## Package and documentation hygiene

- Run `git diff --check` after edits.
- Roxygen-generated Rd pages belong in `man/`; edit their roxygen comments in `R/`, not
  the generated `.Rd` files.
- User-facing installed guides belong in `inst/doc/`. Use `system.file("doc", ..., package
  = "slashOhdsiStrategusAssistant")` in help text and examples to locate them.
- Demo scripts in `extras/` must work with an installed package: do not depend on
  repository-only helpers, a working-directory-relative phenotype index, or local ACP
  source code.
- `R CMD build` and `R CMD check` create a tarball and `<package>.Rcheck/` directory in
  the repository. Remove only the temporary artifacts created by the current task after
  reporting their results.

## Sandbox note

In this environment, ordinary shell commands may fail because bubblewrap namespaces are
unavailable. If a relevant command fails that way, rerun it with the required elevated
permission flow rather than changing the command semantics to work around the sandbox.
