# ESTIMATION (book chapter 6)

## Parameter, estimator, estimate (6.1)
- Parameter theta: a characteristic of the POPULATION (mu, sigma, p), unknown.
- Estimator theta-hat = f(X_1, ..., X_n): a RANDOM variable (varies from sample to sample).
- Estimate: the value of the estimator on the sample actually drawn (e.g. xbar = 82).
- Properties of ESTIMATORS (not of single estimates):
  unbiased E(theta-hat) = theta;  bias = E(theta-hat) - theta;  asymptotically unbiased;
  consistent (converges to theta as n grows);  MSE = E[(theta-hat - theta)^2]
  (= variance if unbiased);  more efficient = smaller MSE.
- Standard error = SD of the estimator: the expected deviation of a GENERIC estimate from theta.
  It says nothing about how far THIS estimate is from theta (unknown).

## Mean (6.2)                                                   est_mean()  sc(3,6)
- Xbar is unbiased for mu, Var(Xbar) = sigma^2 / n  ->  SE = sigma / sqrt(n).
- sigma unknown: S^2 = sum (X_i - Xbar)^2 / (n - 1) is unbiased for sigma^2;
  the unadjusted S~^2 (divisor n) is biased: E = (n - 1)/n sigma^2.
  Estimated SE: se = s / sqrt(n).
- From sums: xbar = sum x_i / n,  s^2 = [sum x_i^2 - n xbar^2] / (n - 1).
- n for SE <= SE*: n >= (sigma / SE*)^2  (s in place of sigma: approximate).   n_mean(se = )

## Proportion (6.5)                                             desc_prop()  sc(1,6) / sc(3,6)
- P-hat = mean of Bernoulli(p) draws: unbiased, Var = p(1 - p)/n, estimated se = sqrt(p-hat(1 - p-hat)/n).

## Two populations (6.6-6.7)
- Xbar - Ybar unbiased for mu_X - mu_Y. Independent: Var = sigma_X^2/n_X + sigma_Y^2/n_Y.
  Paired (same units): work on D = X - Y, Var(Dbar) = sigma_D^2 / n,
  sigma_D^2 = sigma_X^2 + sigma_Y^2 - 2 sigma_XY  (estimate s_D^2 = s_X^2 + s_Y^2 - 2 s_XY).
- P-hat_X - P-hat_Y: unbiased, se = sqrt(p_X(1-p_X)/n_X + p_Y(1-p_Y)/n_Y).
