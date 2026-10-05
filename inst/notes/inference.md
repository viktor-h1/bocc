# CONFIDENCE INTERVALS + HYPOTHESIS TESTS

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
