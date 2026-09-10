# Specification Testing

The deterministic harness covers the shell behavior that turns a study request into durable workflow artifacts.

## Covered conceptually

- Study-intent and direct role-statement capture for target, comparator, and one or more outcomes.
- Cohort acquisition, candidate selection, cached recommendations, source fallback, `/back`, help, and invalid-input recovery.
- Phenotype recommendation, candidate previews, direct OHDSI reuse, review-gated CIPHER/narrative conversion, concept review, and Atlas review artifacts.
- Phenotype improvements, including a change for only one of multiple outcomes.
- Incidence time-at-risk/strata configuration and CohortMethod analytic-settings configuration.
- End-to-end no-AI workflows for both shells: local JSON cohort acquisition, default method configuration, generated scripts, and a fail-fast assertion that ACP is never called.
- Durable checkpoints, resume state, and workflow-local cohort artifacts.

## Manual testing

Use real systems to test clinical suitability, source terminology mapping, local vocabulary coverage, ACP model behavior, Atlas/WebAPI import/export, database acquisition, and generated Circe/Capr definitions against local study requirements.
