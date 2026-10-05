# ABSOLUTE EXAM RULES

1. Hypotheses are about POPULATION parameters (mu, p, beta), never sample statistics (xbar, p-hat, b).
2. Say "fail to reject H0", never "accept H0".
3. Decision: p-value < alpha -> reject H0; p-value >= alpha -> fail to reject H0.
   Conclude in context: "there is (in)sufficient empirical evidence that ...".
4. Do not assume equal population variances unless the question says so
   ("assume equal variances" -> case = "pooled"; otherwise "welch", or "large" for big non-normal samples).
5. Same units measured twice (before/after, pre/post, matched, same customers/firms) -> PAIRED, not independent.
6. "Report the estimate ... and its standard error" is NOT a test: desc_prop() / desc_summary(). No p0 or mu0.
7. A confidence interval request is not a hypothesis test (but a CI that excludes the null value
   leads to the same decision as the two-sided test at alpha = 1 - confidence).
8. Proportions: the TEST uses p0 in the SE; the CI and the descriptive SE use p-hat;
   the two-proportion TEST uses the pooled p; the two-proportion CI uses separate p-hats.
9. Chi-square: goodness of fit = one variable vs stated shares (df = k - 1);
   independence = two categorical variables (df = (r - 1)(c - 1)). Both are right-tail tests.
10. Regression: interpret each slope "holding the other explanatory variables constant";
    a significant F test does not make every coefficient significant.
11. Always state which case/assumption you use and why (sigma known? normal? n large? paired?).
