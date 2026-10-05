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

# HYPOTHESIS TESTS (to be aligned with chapter 7)

## One mean                       ci_mean() / test_mean()
- sigma known                                  -> Z (give sigma =)
- sigma unknown + normal population            -> Student t, df = n - 1 (method = "t")
- unknown distribution + large sample          -> course normal approximation (method = "z")
If the question does not say enough, state the assumption you make.

## Paired means                   ci_paired() / test_paired()
Same subjects/units measured twice, or matched pairs -> work on D = first - second.
Keep the direction explicit (D = after - before: "increase" means mu_D > 0).
If only the two means/SDs and their covariance are given: Var(D) = s1^2 + s2^2 - 2 Cov.

## Two independent means          ci_2means() / test_2means()
Different units in the two groups -> independent.
- population sigmas known                         -> Z           case = "known"
- unknown but explicitly assumed equal            -> pooled t    case = "pooled"   df = n1 + n2 - 2
- unknown and not assumed equal                   -> Welch t     case = "welch"
- unknown distribution, both samples large        -> Z (CLT)     case = "large"

## Proportions                    ci_prop() / test_prop() / ci_2props() / test_2props()
- one-proportion test: SE uses p0
- one-proportion CI and descriptive SE: SE uses p-hat
- two-proportion test of equality: pooled p under H0
- two-proportion CI: separate p-hats in the SE

## Decisions
p-value < alpha -> reject H0.   p-value >= alpha -> fail to reject H0 (never "accept").
Equivalent: statistic in the rejection region (beyond the critical value).

## Type II error / power          power_mean() / power_prop()
beta = P(fail to reject H0 | a specified alternative value is true); power = 1 - beta.
Find the cut-off on the xbar (or p-hat) scale under H0, then the probability under the true value.

## Confidence-interval reading
"We are 95% confident that the population mean lies between L and U": 95% of intervals built
this way contain the true parameter. It is not a probability statement about this one interval.
