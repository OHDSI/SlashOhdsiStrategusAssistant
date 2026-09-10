# Testing Strategus Shell Workflows

The package test harness protects deterministic workflow behavior in both the incidence and CohortMethod shells. It is not clinical validation and does not substitute for an air-gapped deployment test.

## Testing map

- [Specification and cohort authoring](TESTING_SPECIFICATION.md)
- [Execution and recovery](TESTING_EXECUTION.md)

Generate a current, concise coverage report from R:
Generate a current coverage report and structured test summary from the package source directory:

```r
slashOhdsiStrategusAssistant::strategusTestingReport(
  projectPath = "/absolute/path/to/slashOhdsiStrategusAssistant",
  outputFile = "testing-coverage.md",
  verbose = TRUE
)
```

`verbose = TRUE` emits one concise result line per test. The Markdown report retains the same scan-friendly test list, current pass/warning/skip/fail summary, stable conceptual coverage map, and manual validation boundaries. Supply the absolute package source path—`testthat::test_local(".")` works only when R is already in the directory that contains `DESCRIPTION`.
```

The report intentionally describes areas of coverage rather than assertion counts, which change as the harness evolves.

## Manual testing remains required

A human-operated environment must test clinical decisions, ACP/MCP connectivity, Atlas/WebAPI imports, database-backed acquisition, real OMOP vocabulary mappings, long-running Strategus execution, and recovery after an actual VM/session interruption.
