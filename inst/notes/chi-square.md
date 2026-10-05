# CHI-SQUARE TESTS (book section 7.6)

Both tests: statistic = sum over cells of (O - E)^2 / E, approximately chi-square under H0 when
every E >= 5 (some authors: at most 20% of the E below 5 and none below 1). Right-tail test:
reject H0 if chi^2 > chi^2_(df, alpha) = qchisq(1 - alpha, df); p-value = 1 - pchisq(chi^2, df).
Residuals (O - E) / sqrt(E) show which categories / cells deviate and in which direction; the
statistic is the sum of the squared residuals.

## Goodness of fit                    chisq_gof()      sc(6,1)
H0: p_k = p_k0 for every k (fully specified distribution, e.g. uniform 0.25 each)
H1: p_k != p_k0 for at least one k.
E_k = n p_k0;  df = K - 1.
R: chisq.test(x = c(151, 117, 140, 162), p = c(0.25, 0.25, 0.25, 0.25)), or
   chisq.test(table(df$var), p = ...). Without p: equal probabilities.

## Independence                       chisq_indep()    sc(6,2)
H0: p_kj = R_k C_j for every cell (independent, with the sample marginals) vs H1: associated.
E_kj = row total x column total / n;  df = (K - 1)(J - 1).
R: chisq.test(x, y) with two raw vectors, or chisq.test(table) / chisq.test(matrix(..., byrow = TRUE)).
2 x 2 tables: chisq.test() applies Yates' continuity correction, so it does not match the formula;
use chisq.test(..., correct = FALSE).

## Reading the result
- Rejecting independence does NOT mean a strong association: report Cramer's V (ch. 4) and the
  residuals.
- Very large n (e.g. 5007 calls): even negligible deviations are significant; the same shares
  on n = 150 may not be.
- GOF vs independence: one variable vs stated shares -> GOF; two variables -> independence.
