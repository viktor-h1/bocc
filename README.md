# statcram 0.7.2 — Local AI build

This build removes Hermes from the runtime path.

Final architecture:

RStudio -> statcram -> local skill files -> Ollama/Qwen -> JSON interpretation
       -> R validation -> deterministic R statistics -> exam answer

The local model is used ONLY for language interpretation. R performs the statistics.

## Prerequisites

You already created the Ollama model:

    statcram-qwen

Recommended Ollama Modelfile:

    FROM qwen3:4b-instruct
    PARAMETER num_ctx 8192
    PARAMETER temperature 0

R packages `httr2` and `jsonlite` are installed automatically as statcram dependencies.

## Install

Restart R, unzip, then:

```r
remotes::install_local(
  "C:/Users/1/Downloads/statcram_0.7.2/statcram_v070",
  upgrade="never"
)
library(statcram)
packageVersion("statcram")
```

Expected: 0.7.2

## Test connection

```r
ai_status()
```

Expected:

    Local AI ready: statcram-qwen via http://localhost:11434

## Main workflow

```r
load("EXAM.RData")
statcram(mode="auto")
```

Modes:

- `auto`: local Qwen first, deterministic rules if unavailable/invalid
- `ai`: require local Qwen
- `rules`: deterministic parser only

## Inspect a single interpretation

```r
ai_interpret(
  "Report the estimate of the proportion with High loyalty and its standard error.",
  marketing
)
```

Qwen should classify the task, but must not calculate it.

## Skills included

`inst/skills/` contains compact local course instructions covering:

- descriptive/location/dispersion/grouped data
- point estimation and standard errors
- confidence intervals
- hypothesis testing, paired/independent cases, Type II error/power
- random variables, covariance, linear combinations, CLT/sampling distributions
- chi-square GOF and independence
- simple/multiple regression, F/t tests, R2/adjusted R2, prediction, diagnostics, multicollinearity
- recurring past-paper wording patterns

The skills are loaded selectively based on the question so the 4B model does not need a 64K context.


## v0.7.2 hotfix

- Explicitly disables Qwen3 thinking for JSON classification.
- Warms the model once and keeps it loaded for 30 minutes.
- Raises the cold-start timeout from 45 to 120 seconds.
- Caps classifier output at 384 tokens.
- Loads fewer skill modules for non-inferential proportion questions.
- Fixes deterministic fallback selection when the question lists every
  factor level and later asks for one specific level.


## v0.7.2 interpretation safety and editing

This release fixes three failures exposed by a literal past-paper question:

1. A categorical-event confidence interval cannot silently become a mean interval
   for an unrelated numerical column.
2. AI percentages such as `90` are normalized to `0.90`, preventing 9000% displays.
3. Rejecting the proposed interpretation now opens an editor instead of discarding
   the question.

At every question, the review menu is:

1. Proceed
2. Edit interpretation
3. Re-enter/rephrase question
4. Cancel

Typing `n` or `no` is treated as "Edit interpretation."

The validator now checks:
- task/variable type compatibility,
- textual support for selected numerical outcomes,
- categorical levels,
- confidence/alpha scale,
- high-confidence disagreement with the deterministic R cross-check.
