# CONFIDENCE INTERVALS + HYPOTHESIS TESTING RULES

## One mean
Known population sigma -> Z.
Unknown sigma + normal population -> Student t.
Unknown distribution + large sample -> course large-sample normal approximation.
If the question does not state enough, requires_clarification=true.

## Paired means
Same subjects/units measured twice or matched pairs -> paired.
Work on D = one measurement minus the other. Preserve the direction explicitly.

## Two independent means
Different units/groups -> independent.
Cases:
- population variances known -> Z, known_sigma=true
- unknown but explicitly assumed equal -> pooled Student t, variance_case="pooled"
- unknown and not assumed equal -> Welch, variance_case="welch"
- unknown distribution with sufficiently large samples -> course normal approximation, variance_case="large"
If independent means are clear but variance/distribution case is absent, ask.

## Proportions
One-proportion test uses null SE based on p0.
Descriptive SE uses p-hat, not p0.
Two-proportion equality test uses pooled p under H0.
Two-proportion confidence interval uses separate sample proportions in the SE.

## Decisions
p-value < alpha -> reject H0.
p-value >= alpha -> fail to reject H0.
Do not say "accept H0".

## Type II error / power
If question asks probability of not rejecting H0 when a specified alternative is true -> beta / Type II error.
Power = 1-beta.


## Confidence-interval disambiguation examples

- "90% confidence interval for the proportion of High-loyalty customers"
  -> task=`proportion_ci`, variable=the categorical loyalty variable,
     success_level=`High`, confidence=0.90.
- "90% confidence interval for customers with a High loyalty level"
  -> if no numerical response is named and High is a level of a categorical variable,
     treat this as the population proportion with that characteristic.
- "90% confidence interval for mean monthly spending among High-loyalty customers"
  -> task=`mean_ci`, outcome=the numerical spending variable, and the High subgroup may be recorded.
- Never use `mean_ci` without an explicitly supported numerical outcome.
