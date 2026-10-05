# CONFIDENCE INTERVALS (book chapter 6)

Every interval = point estimate -/+ MARGIN OF ERROR, ME = reliability factor x (estimated) SE.
Width = 2 ME; it grows with the confidence level and sigma, and shrinks with n.

Meaning: "we are 95% confident that mu lies in [L, U]" = 95% of the intervals built this way
(over all possible samples) contain mu. NOT "probability 0.95 that this interval contains mu":
the computed interval either contains mu or not.

## Table 6.1: estimator, SE and reliability factor
| parameter | case | SE | factor |
|---|---|---|---|
| mu | sigma known (normal or large n) | sigma/sqrt(n) | z_(a/2) |
| mu | sigma unknown, normal (or large n*) | s/sqrt(n) | t_(n-1, a/2) |
| mu_X - mu_Y indep. | variances known | sqrt(sX^2/nX + sY^2/nY) | z |
| mu_X - mu_Y indep. | unknown, assumed EQUAL (normal or large*) | sqrt(s2pool/nX + s2pool/nY), s2pool = [(nX-1)sX^2 + (nY-1)sY^2]/(nX+nY-2) | t_(nX+nY-2) |
| mu_X - mu_Y indep. | unknown, DIFFERENT (normal or large*) | sqrt(sX^2/nX + sY^2/nY) | t with Welch-Satterthwaite df |
| mu_X - mu_Y paired | sigma_D known (jointly normal or large n) | sigma_D/sqrt(n) | z |
| mu_X - mu_Y paired | unknown | s_D/sqrt(n) | t_(n-1) |
| p | large sample | sqrt(p-hat(1-p-hat)/n) | z |
| p_X - p_Y | large samples | sqrt(pX(1-pX)/nX + pY(1-pY)/nY) | z |
* for large samples (non-normal populations) the z quantile can be used (normal approximation);
  the t quantile is the conservative choice. UBStats prints both rows ("Normal.Approx",
  "Student-t"), and for two means all four (equal / different variances).

Reliability factors: z = 1.645 (90%), 1.96 (95%), 2.576 (99%); t from qt(1 - a/2, df).

## Functions                                       ci_*()  sc(3, 1..5)   UBStats line shown
- ci_mean()   -> CI.mean(x, sigma = , conf.level = )
- ci_prop()   -> CI.prop(x, success = , conf.level = )
- ci_paired() -> CI.diffmean(x, y, type = "paired", sigma.d = , conf.level = )
- ci_2means() -> CI.diffmean(x, y, ...) or CI.diffmean(x, by = group, ...)  (by: alphabetical order!)
- ci_2props() -> CI.diffprop(x, y, success.x = , ...) or CI.diffprop(x, by = group, ...)

## Sample size for a margin of error ME* (or width 2 ME*)        n_mean() n_prop() n_2props()
- mean:  n >= (z sigma / ME*)^2        (sigma unknown: (t s / ME*)^2, approximate)
- proportion: n >= (z 0.5 / ME*)^2 when p is unknown (p = 0.5 is the worst case)
- two proportions, equal n: n >= z^2 [p1(1-p1) + p2(1-p2)] / ME*^2  (0.5 each if unknown)
- always round UP. Careful: a width of 0.01 means ME* = 0.005.

## Confidence intervals alone (6.8)
An interval excluding 0 suggests a difference but does not quantify the risk of a wrong
conclusion: that is the job of hypothesis tests (chapter 7).

# HYPOTHESIS TESTS (book chapter 7)

## Setting up H0 and H1
- H0 = status quo / no effect / "under control"; it is held true unless the data give clear
  evidence against it. H0 ALWAYS contains the equality: mu = mu0, mu <= mu0 or mu >= mu0.
- H1 = what must be PROVEN (effective, higher, below target, different).
- Choose the roles by asking which wrong conclusion is more serious: that conclusion is
  "reject a true H0" (Type I error, probability alpha, fixed in advance: 0.1, 0.05, 0.025, 0.01,
  0.001). Different decision makers can set opposite hypotheses (Example 7.2).
- A one-sided H0 is composite (mu <= 15): critical value and p-value use its BOUNDARY value
  mu0 = 15, the H0 value closest to H1 (largest Type I error probability).
- Two-sided H1 (mu != mu0): both tails critical, alpha/2 in each.

| decision \ truth | H0 true | H1 true |
|---|---|---|
| reject H0 | Type I error (alpha) | correct (power = 1 - beta) |
| fail to reject H0 | correct | Type II error (beta) |

beta(theta) = P(fail to reject | theta in H1); power = 1 - beta. For a fixed n, lowering alpha
raises beta; a larger n lowers beta. alpha, beta and power describe the PROCEDURE over all
samples, never the probability that the decision taken on this sample is wrong.
With sigma unknown, beta cannot be computed before sampling (S varies from sample to sample).
If the "true" value is inside H0 (mu = 10 for H0: mu >= 8): P(reject) = alpha(10) <= alpha and
P(not reject) = 1 - alpha(10) (correct decision).

## p-value
Probability, computed under H0 (at the boundary value), of a statistic at least as extreme as the
observed one in the direction of H1. Two-sided: 2 P(Z > |z_obs|).
p-value < alpha -> reject H0; p-value >= alpha -> fail to reject H0 (never "accept": the data
only do not give sufficient evidence against it). The p-value is the smallest alpha at which H0
is rejected. Rejection region <-> p-value give the same decision.
Large n: tiny differences become "statistically significant" but may be practically irrelevant.
Two-sided test at alpha: reject H0 <-> mu0 outside the 100(1 - alpha)% CI.

## Rejection regions (statistic standardised under H0)
| H1 | sigma known (Z) | sigma unknown, normal (T, n-1 df) | p-value |
|---|---|---|---|
| mu < mu0 | Z < -z_alpha | T < -t_(n-1, alpha) | P(stat < obs) |
| mu > mu0 | Z > z_alpha | T > t_(n-1, alpha) | P(stat > obs) |
| mu != mu0 | abs(Z) > z_(alpha/2) | abs(T) > t_(n-1, alpha/2) | 2 P(stat > abs(obs)) |
On the xbar scale (H1: mu > mu0): reject if xbar > mu0 + z_alpha sigma / sqrt(n).
sigma unknown, large n: normal approximation with s; the t version is the conservative one
(larger critical values, larger p-values). UBStats prints both rows.

## One mean                        test_mean()        sc(4,1)
sigma known -> Z | sigma unknown + normal -> t (n-1 df) | not normal, n large -> Z (CLT).

## One proportion (large n)        test_prop()        sc(4,2)
Z = (p-hat - p0) / sqrt(p0 (1 - p0) / n): the SE uses p0 (UBStats also shows s_X = sqrt(p0(1-p0))).

## Two means: Table 7.1 (statistic under mu_x - mu_y = delta0)
| samples | variances | SE | distribution |
|---|---|---|---|
| independent | known | sqrt(sigma_x^2/n_x + sigma_y^2/n_y) | N(0,1) |
| independent | unknown, equal | sqrt(s_pool^2/n_x + s_pool^2/n_y) | T(n_x+n_y-2) |
| independent | unknown, different | sqrt(s_x^2/n_x + s_y^2/n_y) | T(Welch df) |
| paired | sigma_D known | sigma_D / sqrt(n), sigma_D^2 = sigma_x^2 + sigma_y^2 - 2 sigma_xy | N(0,1) |
| paired | unknown | s_D / sqrt(n), s_D^2 = s_x^2 + s_y^2 - 2 s_xy | T(n-1) |
Large samples: N(0,1) approximation for the unknown-variance rows (t is the conservative choice).
s_pool^2 = [(n_x - 1) s_x^2 + (n_y - 1) s_y^2] / (n_x + n_y - 2).
test_2means() without case shows all four (equal/different x z/t); test_paired(); sc(4,3), sc(4,4).

## Equal variances? Levene test     test_levene()      sc(4,6)
H0: sigma_x^2 = sigma_y^2 (status quo) vs H1: different. UBStats var.test = TRUE: z = |x - median|
in each group, one-way ANOVA F on z, df (1, n_x + n_y - 2). Reject -> use the "different
variances" (Welch) results; not rejected -> equal variances acceptable. Needs the raw data.

## Two proportions                  test_2props()      sc(4,5)
delta0 = 0: pooled p0-hat = (x_x + x_y) / (n_x + n_y), se_0 = sqrt(p0-hat (1 - p0-hat)(1/n_x + 1/n_y)).
delta0 != 0 (e.g. "more than 5 points higher": d0 = 0.05): no pooling,
se = sqrt(p_x(1-p_x)/n_x + p_y(1-p_y)/n_y). CI for p_x - p_y: always unpooled.
A non-significant two-sided test says nothing about the direction; the one-sided test at the
same alpha can reject (critical 1.645 instead of 1.96).

## UBStats (section 7.8)
TEST.mean(x, sigma = NULL, mu0 = 0, alternative = "two.sided", digits = 2, data)
TEST.prop(x, success = NULL, p0 = 0.5, alternative = "two.sided", digits = 2, data)
TEST.diffmean(x, y, type = "independent", sigma.x, sigma.y, mdiff0 = 0, alternative, var.test = FALSE, data)
TEST.diffmean(x, by, ...)                      tests mu_by1 - mu_by2 in ALPHABETICAL / factor order
TEST.diffmean(x, y, type = "paired", sigma.d = NULL, mdiff0 = 0, alternative, data)
TEST.diffprop(x, y, success.x, success.y, pdiff0 = 0, alternative, data)   or (x, by, success.x, ...)
With `by` the difference is level1 - level2 alphabetically (No - Yes): reformulate H1 accordingly
(swap < and >), or use the x / y form. alternative = "less" | "greater" | "two.sided".
p-values below 0.0001 are printed as <0.0001.

## Power / Type II error            power_mean() power_prop() power_2means()   sc(5,1) sc(5,2) sc(5,6)
Find the cut-off(s) under H0 on the estimator scale, then the probability of the acceptance
region under the true value: beta(21) = P(Xbar <= 19.29 | mu = 21).

## Confidence-interval reading
"We are 95% confident that the population mean lies between L and U": 95% of intervals built
this way contain the true parameter. It is not a probability statement about this one interval.
