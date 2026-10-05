# TWO VARIABLES: BIVARIATE DESCRIPTIVE ANALYSIS (book chapter 4)

Which tool?
- both variables with few values (qualitative or discrete) ...... desc_crosstab()   sc(1,7)
- numerical variable (many values) across the groups of another .. desc_compare()    sc(1,4)
- two numerical variables ....................................... desc_cor()        sc(1,8)

## Joint and conditional distributions (4.1-4.2)                   desc_crosstab()
- Contingency table: joint counts f_kj, joint proportions p_kj = f_kj / n.
- Marginal distributions: row sums R_k (distribution of X), column sums C_j (of Y).
- Conditional distributions (shown with stacked bar charts, every bar has height 1):
    Y | X = x*_k :  Freq(Y = y*_j | X = x*_k) = f_kj / R_k   (rows sum to 1)
    X | Y = y*_j :  Freq(X = x*_k | Y = y*_j) = f_kj / C_j   (columns sum to 1)
  Usually condition on the explanatory variable and look at the response (e.g. Satisf | Reason).
- Conditional summaries: mode, median, quartiles of each conditional distribution
  (ordinal response: put its levels in order first).
- Independence: the conditional distributions of Y|X are all the same (= the marginal of Y),
  equivalently p_kj = R_k C_j for every pair. The more they differ, the stronger the association.
- Chi-square (descriptive): chi2 = sum (f_kj - f*_kj)^2 / f*_kj with f*_kj = n R_k C_j
  (= row total x column total / n). Absolute measure: grows with n, K and J.
- Cramer's V = sqrt( chi2 / (n (min(K, J) - 1)) ), between 0 (independence) and 1 (perfect).
- Simpson's paradox: a relation within every subgroup can disappear or reverse when the
  subgroups are aggregated (confounding factor, e.g. age in vaccination death rates).
  Compare conditional distributions only within homogeneous groups.

## A numerical variable across groups (4.3)                          desc_compare()
- Histograms per group (common classes) or, better, SIDE-BY-SIDE BOXPLOTS.
- Conditional summaries: n, min, Q1, median, mean, Q3, max, sd, selected percentiles (p5, p95...).
- With a continuous variable the conditional distributions always differ a little: the question is
  whether they differ substantially in location, dispersion or shape.

## Two numerical variables (4.4)                                     desc_cor()
- Scatterplot: one point per case; the lines at the means split it into quadrants.
  Concordant pairs (both above / both below the means) -> positive relation; discordant -> negative.
- Covariance (direction, NOT strength; depends on units and dispersion):
    sample      s_XY = sum (x_i - xbar)(y_i - ybar) / (n - 1) = [sum x_i y_i - n xbar ybar] / (n - 1)
    population  sigma_XY = sum (x_i - mu_X)(y_i - mu_Y) / N
    joint table s_XY = n/(n-1) [ sum_k sum_j x*_k y*_j p_kj - xbar ybar ]
- Pearson correlation r_XY = s_XY / (s_X s_Y), between -1 and 1, unit-free: strength of the LINEAR
  relation. |r| = 1: all points on a line; r = 0: no linear relation (uncorrelated).
- Regression line (least squares, minimises SSE = sum (y_i - b0 - b1 x_i)^2):
    b1 = s_XY / s_X^2 = r_XY s_Y / s_X        b0 = ybar - b1 xbar
  b0 = fitted Y when X = 0 (meaningful only if 0 is in range); b1 = average change in Y per unit of X.
  The slope does NOT measure the strength of the relation (it depends on s_Y / s_X).
- Cautions: outliers can inflate or deflate r; a low r does not exclude a strong NON-linear relation
  (inverted U, plateau); subgroups with different relations (structural heterogeneity) distort r;
  correlation is not causation (confounding factors, reverse causality, ecological fallacy);
  do not extrapolate beyond the observed range. ALWAYS look at the scatterplot.

## UBStats (book 4.5)                        statcram prints the matching call under each result
distr.table.xy(x, y, freq = "counts", freq.type = "joint", total = TRUE, data)
  freq.type: "joint", "x|y" (column: x given y), "y|x" (row: y given x); x on the ROWS.
  Classes: breaks.x / breaks.y, or interval.x = TRUE / interval.y = TRUE.
distr.plot.xy(x, y, plot.type, freq = "counts", freq.type = "joint", bw = FALSE, data)
  plot.type = "bars" (bar.type = "stacked" or "beside"; freq / freq.type single values),
  "boxplot" (side-by-side boxplots of the numerical variable by the other one),
  "scatter" (fitline = TRUE adds and prints the regression line; var.c colours the points).
distr.summary.x(x, stats = "summary", by1, by2, data)   conditional summaries of x by one or two
  grouping variables (combinations of by1 and by2).
Base R: cov(x, y), cor(x, y) (use = "complete.obs" with missing values).
