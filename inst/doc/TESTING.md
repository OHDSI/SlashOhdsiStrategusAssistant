# Testing Strategus Shell Workflows

The package test harness protects deterministic workflow behavior in both the incidence and CohortMethod shells. It is not clinical validation and does not substitute for an air-gapped deployment test.

## Testing map

- [Specification and cohort authoring](TESTING_SPECIFICATION.md)
- [Execution and recovery](TESTING_EXECUTION.md)

Generate a current, concise coverage report from R:

```r
slashOhdsiStrategusAssistant::strategusTestingReport("testing-coverage.md")
```

Run the package tests with the project R library active:

```r
testthat::test_local(".")
```

The report intentionally describes areas of coverage rather than assertion counts, which change as the harness evolves.

## Manual testing remains required

A human-operated environment must test clinical decisions, ACP/MCP connectivity, Atlas/WebAPI imports, database-backed acquisition, real OMOP vocabulary mappings, long-running Strategus execution, and recovery after an actual VM/session interruption.
