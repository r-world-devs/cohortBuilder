# Describe the structure of a source

\`shape()\` is an S3 generic that summarizes a source for programmatic
or LLM-based inspection. Called with only a \`source\`, it returns a
structured list \`list(datasets, filters)\` where \`datasets\` maps each
dataset name to its description text and \`filters\` is keyed by filter
id (each entry describing the filter's \`dataset\`, \`type\`,
\`description\`, \`variables\`, and \`domain\`).

## Usage

``` r
shape(source, ...)

# Default S3 method
shape(source, field, subfield, ...)

# S3 method for class 'tblist'
shape(source, field, subfield, domains = TRUE, ...)
```

## Arguments

- source:

  A \`Source\` object.

- ...:

  Extra arguments passed to methods.

- field:

  Optional dataset (or description) name to look up.

- subfield:

  Optional variable name within \`field\` to look up.

- domains:

  When \`TRUE\` (default), each filter entry includes a \`domain\` field
  (its set of valid values). Set to \`FALSE\` to omit domains, e.g. when
  only descriptive metadata is needed and computing domains would be
  wasteful.

## Value

Either a \`list(datasets, filters)\` metadata structure or, when
\`field\` is supplied, the description text for the requested entry.

## Details

Called with a \`field\` (and optional \`subfield\`), it instead performs
a description-text lookup, returning the description stored for that
dataset/variable. This form is used internally by the Cohort
\`show_help()\` method.

## See also

\[describe()\], \[autofilter()\]
