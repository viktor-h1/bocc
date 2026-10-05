# PAST-PAPER PATTERNS -> WHAT TO RUN -> WHAT THE ANSWER MUST CONTAIN
(from the 2025/26 midterm, partial and general exams with official solutions)

General rule: short, focused answers in the space of each question. Give the NUMBER, the
FORMULA / analytic expression with the numbers in it, and a one-line INTERPRETATION in context.
When asked, also the R function used (statcram prints it as "in R: ...").

## Describing data
"Sketch a graph to compare the distributions of X across groups"
  -> desc_compare(x, group): side-by-side boxplots. Comment on LOCATION (medians), VARIABILITY
     (IQR, box width), SHAPE (box halves, whisker lengths, outliers -> skewness).
     "Q3 of A < Q1 of B: at least 75% of A lower than at least 75% of B."
"Which measure(s) of central tendency?"  -> mean AND median: a mean far from the median reveals a
     long tail (skewed groups); similar medians with different means = different tails.
"Is a value of 35 extremely low for group A?"  -> desc_compare(x, group, value = 35) or
     desc_summary(x[group == "A"], value = 35): extreme low if below Q1 - 1.5 IQR (R quartiles).
"Range of values of the 90% most standard units"  -> [P5, P95] (desc_summary / desc_compare).
"P10 and P90 and what they say"  -> 10% have at most P10, 10% more than P90, central 80% between.
"Relationship between two numerical variables"  -> desc_cor(): scatterplot first; direction,
     strength (r), LINEARITY (a high |r| is not reliable if the cloud is curved).
"Percentage of A among B and C"  -> desc_crosstab(): CONDITIONAL percentages (row %), never
     joint counts or joint percentages ("it was wrong to answer based on joint counts").

## Data in classes (no raw data -> everything is approximate, uniform within classes)
"Frequency distribution, modal class" -> desc_classes(): modal class = highest DENSITY.
"Graph" -> histogram with DENSITIES on the y-axis when widths differ (report class limits and densities).
"Mean / median" -> mean = sum(m_k p_k); median = l + (0.5 - F_(k-1)) / c_k; say "approximated".
"Upper tail" -> P90 (or P95, P99) = l + (0.9 - F_(k-1)) / c_k: "the 10% with the highest values exceed P90".
"How many units below 30?" -> at_most = 30: Freq(X <= 30) by linear interpolation, x n = approximate count.
"Dispersion of grouped data" -> s^2 = n/(n-1) [sum(m_k^2 p_k) - mean^2]; compare dispersion of
     variables in different units with the CV = s / mean (desc_cv(mean =, sd =)).
"Proportion above 85 in the old table vs the data" -> classes: approximation; raw data: exact.

## Estimation
"Estimate the mean / its standard error"  -> est_mean(): xbar, se = s / sqrt(n) (sigma unknown -> estimated).
"Unbiased estimator / define unbiasedness"  -> E(T) = theta for every theta; Xbar for mu, P-hat for p.
"Is one estimate more reliable?"  -> the SE compares ESTIMATORS (generic estimates), not the
     distance of this specific estimate from the parameter: "no conclusion on the realised estimates".
"The SE tells how far our estimate is from mu" -> only partially correct: expected distance of a
     GENERIC estimate.
"Estimator of a proportion, formula, value, SE" -> desc_prop(): P-hat = sum X_i / n (X_i Bernoulli),
     se = sqrt(p-hat (1 - p-hat) / n).

## Confidence intervals
"Report a 99% CI and interpret"  -> ci_*(): "With a level of confidence of 99% we can conclude that
     the <parameter in context> lies between L and U."
"Contained with probability 0.9?" -> NO: the parameter is fixed; 90% of intervals built this way
     contain it (property of the procedure).
"Without calculations, is p significantly different from 0.3?" -> 0.3 outside the 99% CI -> reject
     H0: p = 0.3 in a two-sided test at alpha = 0.01 (and at any larger alpha).
"Margin of error at 95% instead of 90%" -> larger confidence -> larger ME (wider interval).
"Sample size for width <= 0.09 at 99%" -> n_prop(width = 0.09, conf = 0.99) = 820 (p = 0.5).
"CI for mu_x - mu_y: check the variances first" -> ci_2means(raw data): Levene p-value
     (> 0.05 -> equal variances, pooled rows; < 0.05 -> different, Welch rows).
"Analytical formula of the paired CI with its components" -> ci_paired(): dbar +/- t(n-1) s_D / sqrt(n).

## Tests
"Specify H0 and H1"  -> H0 = status quo / "not effective" / "meets the target", ALWAYS with the
     equality (<=, >=, =); H1 = what must be proven. Explain: "the department is conservative toward
     the hypothesis that the campaign is not effective (more serious error = launching an ineffective one)".
"Rejection region at alpha" -> test_*(): z or t scale AND the xbar / p-hat scale
     (e.g. xbar < 630 - z_0.01 x 120 / sqrt(441) = 616.7066); conclude in context.
"Value of the statistic and p-value; conclusion" -> report both; "since p < alpha we reject H0:
     there is sufficient empirical evidence that ..." / "we do not reject: insufficient evidence".
"Analytical expression of the p-value / R function" -> e.g. P(T_(n-1) > 1.58), 1 - pt(1.58, df);
     chi-square: P(chi2_df > 12.717) = 1 - pchisq(12.717, 2).
"Interpret the p-value; difference from alpha" -> p-value: probability, under H0, of a statistic at
     least as extreme as the observed one (computed from the data); alpha: maximum Type I error
     probability the analyst tolerates, fixed in advance.
"Can we say the probability that the conclusion is correct?" -> NO: the decision is right or wrong;
     alpha / beta / power describe the procedure, not this decision.
"Probability the test concludes X when the true mean is 875 (+ R function)" -> power_mean():
     if 875 is in H1 it is the POWER, P(reject | mu = 875) = 1 - pnorm(cut, 875, sigma/sqrt(n));
     if the value is in H0 it is P(reject) = alpha(value) (e.g. 0.00068), not beta.
"Exceeds by more than 10 / 200 points" -> d0 = 10: test_paired(x, y, d0 = 10, alt = ">") for the
     same units; test_2means(..., d0 =) for independent groups; test_2props(d0 = 0.05).
"Last year only summary numbers, this year raw data" -> test_2means(df$price1, xbar2 = 820,
     s2 = 550, n2 = 500, case = "pooled", alt = ">").
"Are the variances equal?" -> test_levene() / Levene rows of test_2means: H0 sigma2_x = sigma2_y.

## Chi-square
"Associated? Specify H0/H1, statistic, p-value, conclusion" -> chisq_indep(): H0 independent,
     H1 dependent; p-value = P(chi2_((r-1)(c-1)) > X2_obs); interpretation under independence.
"Do the shares match 40/30/30?" -> chisq_gof(x, p = c(0.4, 0.3, 0.3)): expected counts n p_k
     (441 x 0.4 = 176.4 ...), df = K - 1.

## Regression
"Report / interpret the coefficients of a factor"  -> reg_fit(): each dummy = average difference
     from the BASELINE level, holding the other variables constant. Equation with I(X = level).
"Difference between two non-baseline levels" -> b_A - b_B (other variables fixed); its significance
     is not in the output (refit with relevel()).
"Is the model globally significant?" -> F test: H0 beta_1 = ... = beta_k = 0;
     F = (SSR/k) / (SSE/(n-k-1)); p-value; no yes/no answer without the measure.
"Significance of a coefficient; t statistic from the output" -> t = b / se(b); p-value vs alpha
     ("significant at 10% only").
"Age significant in mod1 but not in mod2" -> reg_compare(mod1, mod2): correlated with the added
     variables (close to multicollinearity); in mod1 it also captured their effect.
"Goodness of fit" -> R2 = share of the variability of Y explained by the model with ... ;
     different numbers of predictors -> adjusted R2 (an R2 of 0.75 alone is not enough).
"Add variable Z?" -> adjusted R2 increase + p-value of Z (reg_compare: partial F = t^2).
"CI for the coefficient / for a 10-unit change" -> reg_effect(mod, "Age", change = 10, conf = 0.99).
"Point and interval estimate for a client with ..." -> reg_predict(): CI = AVERAGE response,
     PI = ONE individual (wider: adds individual variability). "Is 70 anomalous?" -> value = 70:
     judge with the PI. Values outside the observed range -> extrapolation, unreliable.
"Homoscedasticity / normality" -> reg_check(): Var(eps_i) = sigma^2, plot(mod, which = 1) or
     which = 3; normality: Q-Q plot (which = 2) and histogram of standardised residuals.
"A NorthWest client spends more" -> wrong: conclusions are on AVERAGES, relative to the baseline
     region, for given values of the other variables.
