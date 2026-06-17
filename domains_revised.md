# Domains Design

## Overview

A **domain** is the declared universe of valid values for a filter variable — independent of the actual data. Domains enable:

1. **Vocabulary-driven cohort configuration** — define valid choices from external vocabularies (e.g., OMOP CONCEPT table) without scanning data.
2. **Metadata-only operation** — shinyCohortBuilder can render filter UIs and choices from domains alone, without computing stats from data.
3. **Value validation** — filter values are constrained to the domain; out-of-domain values are rejected or intersected.

## Compatibility

Operable on cohortBuilder @ branch v0.5.0 and shinyCohortBuilder @ branch v0.5.0.

## Storage

### Three-tier inheritance

Domain can be set at three levels. Each level falls back to the one above when not set explicitly:

1. **Source description** — `describe("species of iris", domain = c("setosa", "versicolor", "virginica"))`
2. **Available filters** — `filter("discrete", ..., domain = c("setosa", "virginica"))` stored in `source$available_filters`
3. **Step filters** — `filter("discrete", ..., domain = ...)` attached to a step

Resolution: filter's own domain > available_filters domain > description domain > `NULL` (no constraint).

### S7 property

Add `domain` to the `CbFilter` base class:

```r
CbFilter <- S7::new_class("CbFilter",
  properties = list(
    ...,
    domain = S7::new_property(S7::class_any, default = NULL)
  )
)
```

The `filter()` constructor accepts `domain = NULL`. When `NULL` at filter creation time, domain is resolved lazily from the description/available_filters hierarchy during source initialization.

No changes needed for `update_filter` — it already iterates S7 props and sets matching ones.

## Per-type domain structure

Each filter type has a well-defined domain shape matching its `value` structure:

| Filter type       | Domain structure                          | Example                                      |
|-------------------|-------------------------------------------|----------------------------------------------|
| `discrete`        | Character vector of valid values          | `c("setosa", "versicolor", "virginica")`     |
| `discrete_text`   | Character vector of valid values          | `c("Tokyo", "Berlin", "Lima")`               |
| `range`           | 2-length numeric vector `c(min, max)`     | `c(0, 200)`                                  |
| `date_range`      | 2-length Date vector `c(min, max)`        | `as.Date(c("2000-01-01", "2025-12-31"))`     |
| `datetime_range`  | 2-length POSIXct vector `c(min, max)`     | `as.POSIXct(c("2000-01-01", "2025-12-31"))`  |
| `multi_discrete`  | Named list of character vectors           | `list(color = c("red", "blue"), size = 1:5)` |
| `query`           | `NULL` (not applicable)                   | Domains don't constrain freeform queries     |

## Intersection semantics

When a filter is applied, its value is intersected with its domain via an internal generic:

```r
# Internal; dispatches on filter class
.intersect_domain(filter)
```

Per-type rules:

- **discrete / discrete_text / multi_discrete**: `intersect(value, domain)`. If result is empty, filter matches zero rows (no error — this is a valid "no match" state).
- **range / date_range / datetime_range**: `c(max(value[1], domain[1]), min(value[2], domain[2]))`. If resulting min > max, filter matches zero rows.
- **query**: No intersection. Domain is `NULL` for query filters.

### Validation on `update_filter`

When a filter value is set outside its domain: **warn and intersect**. The filter proceeds with the intersected value. This is forgiving (no hard error) but transparent (warning alerts the caller).

```r
# Pseudocode in cb_filter_data or a pre-filtering hook
effective_value <- .intersect_domain(filter)
if (!identical(effective_value, filter@value)) {
  warning("Filter '", filter@id, "': value trimmed to domain.")
}
```

## Domain vs. stats

These are separate concepts:

| Aspect   | Domain                                     | Stats                                      |
|----------|--------------------------------------------|--------------------------------------------|
| Source   | Declared (vocabulary, description, manual)  | Observed (computed from data)              |
| When     | Known before data access                    | Requires data access                       |
| Mutates  | Only explicitly (update_filter, propagation)| Recomputed on every run_step               |
| Purpose  | Constrain valid choices                     | Show empirical distribution                |

`shape()` gains a `domain` column alongside the existing `stats` column:

```r
tibble::tibble(
  dataset = "person",
  filter = "gender",
  description = list(describe("patient gender")),
  domain = list(c("M", "F", "U")),
  stats = list(list(choices = c(M = 120, F = 135), type = "discrete"))
)
```

shinyCohortBuilder rendering contract:
- If stats are available and step is not pending → use stats for choices/range.
- If stats are unavailable (cache disabled or step pending) → fall back to domain.
- If neither → filter UI is disabled or shows "no metadata available."

## Cache control

Add a `cache` option at the cohort level:

```r
cohort(source, cache = TRUE)  # default: compute stats as today
cohort(source, cache = FALSE) # skip update_cache(); rely on domains
```

When `cache = FALSE`:
- `run_step()` skips `update_cache()`.
- `get_cache()` returns `NULL` for stats.
- shinyCohortBuilder falls back to domains for filter rendering.
- Attrition and data stats features are unavailable (expected).

The existing `cb.source_filters_meta_stats` option continues to control meta-level stats independently.

## Domain propagation

### Static vs. dynamic domains

Two modes, chosen per use case:

**Static domains** (default): Domains are set once (from vocabulary, description, or autofilter) and don't change during the data flow. This is the right mode for:
- OMOP vocabulary-driven filters (all valid ICD codes remain valid regardless of filtering)
- Any case where the domain represents the *ontology*, not the *data state*

**Dynamic domains** (opt-in): Filter values at step n narrow domains at step n+1. This is useful for:
- Progressive narrowing in multi-step exploratory workflows
- Cases where step 2 should only show choices that survived step 1

Dynamic propagation is triggered by a source-layer generic:

```r
.propagate_domains <- function(source, data_object, current_step_filters, next_step_filters, ...) {
  UseMethod(".propagate_domains", source)
}

.propagate_domains.default <- function(source, ...) {
  # No-op: static domains by default
}
```

When implemented (e.g., for tblist), this runs in `run_step()` after filtering + binding, before cache update. It computes new domains from the filtered data and pushes them to the next step's filters.

### Binding keys interaction

Domains do **not** propagate through binding keys. Binding affects the *data* (and therefore stats), but not the declared domain. Rationale:

- Domains represent the valid vocabulary, not the current data state.
- For OMOP: binding cascades person-level filtering across clinical tables, but the valid ICD codes or drug concepts don't change because of that.
- Stats already reflect the bound data, so shinyCohortBuilder still shows the empirical narrowing via stats when available.

If a source type needs domain narrowing after binding, it can implement `.propagate_domains()` to run after `.post_binding()`.

## autofilter integration

`autofilter()` populates domains from two sources, in priority order:

1. **Description domains** — if `describe(domain = ...)` was provided for the variable, use it.
2. **Observed values** — if no description domain, compute from data (as today).

```r
# In autofilter rule (e.g., rule_character):
rule_character <- function(column, name, dataset_name, description_domain = NULL) {
  domain <- description_domain %||% collapse::funique(column)
  list(
    type = "discrete", id = name, name = name,
    variable = name, dataset = dataset_name,
    domain = domain,
    ...
  )
}
```

When `attach_as = "meta"`, the domain is stored on the filter in `source$available_filters`. When a filter is later added to a step, it carries its domain with it.

## Serialization

`get_state()` / `restore()` must round-trip domain. Since domain is an S7 property on `CbFilter`, the existing serialization logic (which iterates S7 props) should handle it — verify with a test case.

## OMOP considerations

For an OMOP source type, domains map naturally to vocabulary tables:

- `CONCEPT` table provides valid concept IDs per domain (Condition, Drug, Procedure, etc.)
- Filter domains are populated from vocabulary at source initialization, not by scanning clinical tables.
- Domains are **static** — the valid ICD-10 codes don't change because you filtered patients.
- Stats (when computed) show which concepts actually appear in the data after filtering.
- Binding cascades through `person_id`; domains are unaffected.

This means an OMOP source can operate in metadata-only mode: define filters with vocabulary-driven domains, render UIs in shinyCohortBuilder, and only hit the database when the user explicitly runs the cohort.

## shinyCohortBuilder notes

1. Show stats (pre/post) only when step is not pending and stats are calculated.
2. Feedback plots visible only when stats are calculated.
3. When domains are available but stats are not: render filter choices from domain (no counts shown).
4. Filter choices from stats are always a subset of domain (stats-based approach limits to observed values; domain-based approach shows the full vocabulary). This is expected and correct — the two serve different purposes.

## Implementation order

1. Add `domain` property to `CbFilter` base class.
2. Add `domain` parameter to `filter()` constructor and all subclass constructors.
3. Implement domain inheritance resolution (describe → available_filters → filter).
4. Implement `.intersect_domain()` per filter type.
5. Wire intersection into `cb_filter_data` methods (or as a pre-filtering step).
6. Add `domain` column to `shape()` output.
7. Update `autofilter()` rules to populate domain.
8. Add `cache` parameter to `cohort()`.
9. Implement `.propagate_domains()` generic + tblist no-op default.
10. Update shinyCohortBuilder to fall back to domain when stats unavailable.
11. Tests for each of the above.
