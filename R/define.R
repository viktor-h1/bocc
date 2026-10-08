# ------------------------------------------------------------
# sc_define(): exam-ready definitions of the first-partial terms.
# Every definition has the four parts of a definition answer:
# what it is, the formula, its meaning for the units, one property
# or caveat. The same table feeds the "Definitions" card of the
# answer framework.
# ------------------------------------------------------------

.def <- function(key, term, topic, aliases, what, formula, meaning, property, fn = "") {
  data.frame(key = key, term = term, topic = topic, aliases = paste(aliases, collapse = "|"),
             what = what, formula = formula, meaning = meaning, property = property, fn = fn,
             stringsAsFactors = FALSE)
}

.def_topics <- c("Data and variables", "Describing one variable", "Two variables",
                 "Probability and random variables", "Estimation and confidence intervals")

.defs <- rbind(
  # ---- data and variables ----
  .def("population", "Population", .def_topics[1], c("statistical population"),
       "The whole set of units (people, firms, objects) the study is about.",
       "N = population size",
       "Its summary values (mean mu, proportion p, SD sigma) are the parameters, usually unknown.",
       "The population is fixed; what changes from study to study is the sample drawn from it."),
  .def("sample", "Sample", .def_topics[1], c("observed units"),
       "The subset of n units of the population that are actually observed.",
       "n = sample size",
       "Its summary values (xbar, p-hat, s) are statistics, used to estimate the population parameters.",
       "A random sample (units drawn by chance, independently) lets us generalise to the population; another sample would give different values."),
  .def("parameter", "Parameter and statistic", .def_topics[1], c("statistic", "parameter vs statistic"),
       "A parameter is a numerical summary of the population (mu, sigma, p); a statistic is a summary computed on the sample (xbar, s, p-hat).",
       "mu, sigma, p (population) vs xbar, s, p-hat (sample)",
       "Parameters are fixed and usually unknown; statistics are known once the sample is drawn and are used to estimate them.",
       "A statistic varies from sample to sample, so it has a sampling distribution; a parameter does not vary."),
  .def("variable", "Statistical variable", .def_topics[1], c("variable type", "type of variable", "unit"),
       "A characteristic observed on every unit (one column of the data frame).",
       "qualitative (nominal, ordinal) or quantitative (discrete, continuous)",
       "Its type decides which tables, graphs and summary measures make sense.",
       "An identifier (id, index) is not a statistical variable; number codes (1 = F, 2 = M) do not make a variable numerical.",
       "desc_vars()"),
  .def("nominal", "Qualitative nominal variable", .def_topics[1], c("nominal variable", "categorical", "qualitative"),
       "A variable whose values are categories with no natural order (Channel, District).",
       "frequencies f_k and proportions p_k = f_k / n per category",
       "Only counts and shares per category make sense; the centre is described by the mode.",
       "Graph: pie or bar chart, where the order of the bars means nothing; the median and the mean are not defined.",
       "desc_freq()"),
  .def("ordinal", "Qualitative ordinal variable", .def_topics[1], c("ordinal variable", "ordered"),
       "A variable whose categories have a natural order (Early < MediumTerm < LastMinute; low < medium < high).",
       "cumulative frequencies F_k = p_1 + ... + p_k follow the natural order",
       "Besides the mode, the median and the cumulative shares make sense (\"81% have at most Middle\").",
       "Bars must follow the natural order (in R: factor(x, levels = c(...)), otherwise R sorts alphabetically); distances between levels are not measurable, so no mean.",
       "desc_freq(x, order = c(...))"),
  .def("discrete", "Quantitative discrete variable", .def_topics[1], c("discrete variable"),
       "A numerical variable that takes separated values, usually counts (number of children, items bought).",
       "frequency table of the distinct values x*_k",
       "With few distinct values it is described by a frequency table and a spike plot; mode, median and mean all make sense.",
       "With many distinct values it is treated like a continuous variable (classes, histogram).",
       "desc_freq()"),
  .def("continuous", "Quantitative continuous variable", .def_topics[1], c("continuous variable", "numerical", "quantitative"),
       "A numerical variable that can take any value in an interval (time, price, amount).",
       "classes [l_k, u_k) with widths w_k",
       "It is described with classes, a histogram or a boxplot, and by median, mean, quartiles and SD.",
       "Each value appears about once, so the mode of the values is not useful: use the modal class.",
       "desc_summary(), desc_classes()"),
  # ---- describing one variable ----
  .def("frequency", "Absolute frequency (count)", .def_topics[2], c("absolute frequency", "count", "counts"),
       "The number of units with a given value or category.",
       "f_k, with f_1 + ... + f_K = n",
       "How many units fall in category k.",
       "Counts depend on n: to compare samples or groups of different size, use relative frequencies.",
       "desc_freq()"),
  .def("relative-frequency", "Relative frequency (proportion)", .def_topics[2], c("relative frequency", "proportion", "percentage"),
       "The share of units with a given value or category.",
       "p_k = f_k / n (x 100 for a percentage), with p_1 + ... + p_K = 1",
       "The proportion of units in category k, comparable across samples of different size.",
       "It is a sample share (an estimate of the population proportion p).",
       "desc_freq()"),
  .def("cumulative", "Cumulative relative frequency", .def_topics[2], c("cumulative frequency", "cumulative", "ecdf", "distribution function"),
       "The share of units with a value less than or equal to a given value.",
       "F_k = p_1 + ... + p_k ;  F(x) = Freq(X <= x)",
       "F_k = 0.45 means that 45% of units have a value at most x_k.",
       "Defined only when the values are ordered (ordinal or numerical); F(x) is a non-decreasing step function from 0 to 1.",
       "desc_freq(), desc_classes()"),
  .def("density", "Frequency density", .def_topics[2], c("frequency density", "class density"),
       "The proportion of units per unit of width of a class.",
       "c_k = p_k / w_k  (w_k = width of class k)",
       "How concentrated the data are in each class: the histogram uses densities as heights, so the area of each bar is p_k.",
       "Needed whenever the classes have different widths: the modal class is the one with the highest density, not the highest count.",
       "desc_classes()"),
  .def("histogram", "Histogram", .def_topics[2], character(0),
       "A graph of a numerical variable grouped in classes: adjacent bars, one per class, with area equal to the class proportion.",
       "height = c_k = p_k / w_k ; area = p_k ; total area = 1",
       "It shows the shape of the distribution: where the values concentrate, symmetry or skewness, the tails, possible peaks.",
       "With unequal widths the heights must be densities; with equal widths counts, proportions and densities give the same shape.",
       "desc_classes()"),
  .def("mode", "Mode (modal class)", .def_topics[2], c("modal class", "modal value"),
       "The most frequent value or category; with classes, the class with the highest density.",
       "value with the largest f_k (classes: largest c_k)",
       "The most typical value or category.",
       "Defined for every type of variable (the only centre of a nominal one); there can be several modes; not useful for continuous data with all values distinct.",
       "desc_freq(), desc_classes()"),
  .def("median", "Median", .def_topics[2], c("q2", "second quartile"),
       "The value that splits the ordered data into two halves.",
       "middle value of the ordered data (n odd) or mean of the two middle values (n even); first value with F >= 0.5; classes: Me = l_k + (0.5 - F_(k-1)) / c_k",
       "50% of units have a value at most equal to the median: it is the maximum value reached by the lowest 50%.",
       "Robust: not affected by extreme values. Defined for ordinal and numerical variables, not for nominal ones.",
       "desc_summary(), desc_classes()"),
  .def("mean", "Arithmetic mean", .def_topics[2], c("average", "xbar", "sample mean"),
       "The sum of the values divided by their number.",
       "xbar = (x_1 + ... + x_n) / n ; frequency table: xbar = sum x*_k p_k ; classes: xbar ~ sum m_k p_k (midpoints)",
       "The value every unit would have if the total were shared equally; the centre of gravity of the data (the deviations x_i - xbar sum to 0).",
       "Not robust: attracted by extreme values and long tails (mean > median for right-skewed data). For a 0/1 variable it is the proportion of 1s.",
       "desc_summary(), desc_classes()"),
  .def("quartiles", "Quartiles", .def_topics[2], c("quartile", "q1", "q3", "first quartile", "third quartile"),
       "The three values Q1, Q2 = median and Q3 that split the ordered data into four parts of about 25% each.",
       "classes: Q_p = l_k + (p - F_(k-1)) / c_k for p = 0.25 and 0.75",
       "25% of units have a value at most Q1 and 25% more than Q3: the central 50% lie between Q1 and Q3.",
       "Robust to extreme values; several computing rules exist (R interpolates), so results can differ slightly.",
       "desc_summary(), desc_classes()"),
  .def("percentile", "Percentile", .def_topics[2], c("percentiles", "quantile", "p10", "p90", "decile"),
       "The value P_p with p% of the ordered data at or below it.",
       "classes: P_p = l_k + (p - F_(k-1)) / c_k in the first class with F_k >= p",
       "P10 = 39.46: 10% of units have a value at most 39.46 (the maximum value reached by the lowest 10%), 90% have more.",
       "Extreme percentiles (P5, P10, P90, P95) describe the tails; [P_p, P_(100-p)] contains the central (100 - 2p)% of the units.",
       "desc_summary(x, probs = c(0.1, 0.9))"),
  .def("iqr", "Interquartile range (IQR)", .def_topics[2], c("interquartile range", "interquartile"),
       "The difference between the third and the first quartile.",
       "IQR = Q3 - Q1",
       "The width of the interval containing the central 50% of the units (the length of the box in the boxplot).",
       "Robust to extreme values, unlike the range and the SD; it says nothing about the tails.",
       "desc_summary(), desc_classes()"),
  .def("range", "Range", .def_topics[2], character(0),
       "The difference between the largest and the smallest value.",
       "Range = Max - Min",
       "The width of the interval containing all the data.",
       "Not robust: it depends only on the two most extreme values.",
       "desc_summary()"),
  .def("variance", "Variance", .def_topics[2], c("sample variance", "s2", "s^2"),
       "The average squared deviation of the values from their mean.",
       "s^2 = sum (x_i - xbar)^2 / (n - 1) = n/(n - 1) [sum x_i^2 / n - xbar^2] ; population: sigma^2 = sum (x_i - mu)^2 / N ; classes: n/(n - 1) [sum m_k^2 p_k - xbar^2]",
       "How dispersed the values are around the mean: the larger, the more dispersed.",
       "Measured in squared units, so hard to read: its square root, the SD, is in the unit of the data. Never negative; 0 only if all values are equal.",
       "desc_summary(), desc_classes()"),
  .def("sd", "Standard deviation (SD)", .def_topics[2], c("standard deviation", "s", "std dev"),
       "The square root of the variance.",
       "s = sqrt(s^2)",
       "The typical distance of the values from their mean, in the unit of the data.",
       "Not robust (squared deviations amplify extreme values); it depends on the unit, so use the CV to compare variables with different units or means.",
       "desc_summary(), desc_classes()"),
  .def("cv", "Coefficient of variation (CV)", .def_topics[2], c("coefficient of variation", "relative dispersion"),
       "The standard deviation divided by the absolute value of the mean.",
       "CV = s / |xbar|  (often x 100%)",
       "The dispersion relative to the size of the values: a unit-free measure.",
       "Use it to compare the dispersion of variables measured in different units or with very different means; it has no fixed range.",
       "desc_cv()"),
  .def("boxplot", "Boxplot (five-number summary)", .def_topics[2], c("box plot", "five-number summary", "five number summary"),
       "A graph of Min, Q1, median, Q3 and Max: a box from Q1 to Q3 with the median inside, whiskers to the most extreme regular values, outliers as points.",
       "fences: Q1 - 1.5 IQR and Q3 + 1.5 IQR",
       "It shows location (median), variability (box length = IQR) and shape (box halves, whiskers, side of the outliers) at a glance.",
       "Best for comparing groups side by side; it hides peaks and the detail of the tails (use a histogram for those).",
       "desc_summary(), desc_compare()"),
  .def("outlier", "Outlier (extreme value)", .def_topics[2], c("outliers", "extreme value", "fences", "anomalous value"),
       "A value far from the bulk of the data: below Q1 - 1.5 IQR (extremely low) or above Q3 + 1.5 IQR (extremely high).",
       "lower fence = Q1 - 1.5 IQR ; upper fence = Q3 + 1.5 IQR",
       "A unit unusual compared with the others in the same distribution (use the quartiles of the group the question names).",
       "Outliers pull the mean, the SD, the range and r; the median and the IQR are robust to them.",
       "desc_summary(x, value = ...), desc_compare(x, g, value = ...)"),
  .def("skewness", "Skewness (shape)", .def_topics[2], c("skewed", "symmetry", "symmetric", "shape", "right-skewed", "left-skewed"),
       "Lack of symmetry: a right-skewed distribution has a longer upper tail, a left-skewed one a longer lower tail.",
       "compare Q2 - Q1 with Q3 - Q2, Q1 - Min with Max - Q3, and the mean with the median",
       "In a right-skewed distribution most units have low or moderate values and a few have very high ones.",
       "Evidence: unequal box halves and whiskers, outliers on one side, mean > median (right) or mean < median (left); the mean-median comparison is only an indication.",
       "desc_summary()"),
  # ---- two variables ----
  .def("joint", "Joint distribution", .def_topics[3], c("joint frequency", "contingency table", "cross-tab", "crosstab"),
       "The frequencies of every combination of the categories of two variables (the contingency table).",
       "f_kj ; p_kj = f_kj / n",
       "How many units (or which share of all units) have X = x_k and Y = y_j together.",
       "Joint counts must not be used to compare groups of different size: use the conditional distributions.",
       "desc_crosstab()"),
  .def("marginal", "Marginal distribution", .def_topics[3], c("marginal frequency"),
       "The distribution of one variable alone, read from the totals of a contingency table.",
       "row totals R_k (for X), column totals C_j (for Y)",
       "The share of all units in each category, ignoring the other variable.",
       "Under independence every conditional distribution equals the marginal one.",
       "desc_crosstab()"),
  .def("conditional", "Conditional distribution", .def_topics[3], c("conditional frequency", "conditional percentage", "conditional"),
       "The distribution of one variable among the units with a given value of the other.",
       "Freq(Y = y_j | X = x_k) = f_kj / R_k (each row sums to 1)",
       "\"Among customers with Channel = Aggregator, 51% bought with an average advance\": the right way to compare groups of different size.",
       "Condition on the explanatory variable (the \"among\" part of the question); if the conditional distributions differ, the variables are associated.",
       "desc_crosstab()"),
  .def("independence", "Independence and association", .def_topics[3], c("association", "independent", "associated"),
       "Two variables are independent when the distribution of one is the same whatever the value of the other; otherwise they are associated.",
       "p_kj = (R_k / n) x (C_j / n) for every pair: all conditional distributions are equal to the marginal one",
       "Under independence, knowing X tells nothing about Y; the more the conditional distributions differ, the stronger the association.",
       "Association is not causation (confounding factors, reverse causality).",
       "desc_crosstab()"),
  .def("scatterplot", "Scatterplot", .def_topics[3], c("scatter plot", "scatter"),
       "A graph of two numerical variables: one point per unit, X on the horizontal axis and Y on the vertical one.",
       "point (x_i, y_i) for each unit",
       "It shows the direction, the form (linear or not) and the strength of the relationship, and the outliers.",
       "The fundamental tool for two numerical variables: it tells whether r is a reliable measure (only if the points cluster around a straight line).",
       "desc_cor()"),
  .def("covariance", "Covariance", .def_topics[3], c("cov", "s_xy"),
       "The average product of the deviations of two variables from their means.",
       "s_XY = sum (x_i - xbar)(y_i - ybar) / (n - 1)",
       "Its sign gives the direction of the linear relationship: positive = direct (concordant pairs prevail), negative = inverse.",
       "It depends on the units and the dispersion of the variables, so its size does not measure the strength: use r.",
       "desc_cor()"),
  .def("correlation", "Correlation coefficient (r)", .def_topics[3], c("r", "correlation coefficient", "pearson", "linear correlation"),
       "The covariance divided by the product of the two standard deviations.",
       "r = s_XY / (s_X s_Y), with -1 <= r <= 1",
       "Direction and strength of the LINEAR relationship: |r| close to 1 = points close to a straight line; r close to 0 = no linear relationship.",
       "Unit-free and the same whichever variable is X; reliable only if the scatterplot shows a linear pattern (a curve or outliers make it misleading); correlation is not causation.",
       "desc_cor()"),
  .def("regression-line", "Least-squares regression line", .def_topics[3], c("regression line", "least squares", "slope", "intercept", "fitted line"),
       "The straight line y-hat = b0 + b1 x that minimises the sum of the squared vertical distances of the points from it.",
       "b1 = s_XY / s_X^2 = r s_Y / s_X ; b0 = ybar - b1 xbar",
       "b1 = average change in Y for a one-unit increase in X; b0 = fitted Y when X = 0 (meaningful only if 0 is within the observed range).",
       "It depends on which variable is X and which is Y; the slope does not measure the strength of the relationship; do not extrapolate outside the observed range.",
       "desc_cor()"),
  .def("explanatory", "Explanatory and response variable", .def_topics[3], c("response", "explanatory variable", "response variable", "x and y", "independent variable", "dependent variable"),
       "The explanatory variable X is the one that may influence the other (it often comes first in time); the response Y is the one to be explained or predicted.",
       "X on the horizontal axis, Y on the vertical axis ; Y | X",
       "X is the condition in conditional distributions and the predictor in the regression line.",
       "The order of the words in the question does not decide it, the logic of the situation does (the fare is paid before the bid: X = PaidFare, Y = Bid).",
       "desc_cor(x, y), desc_crosstab(x, y)"),
  # ---- probability and random variables ----
  .def("random-variable", "Random variable", .def_topics[4], c("rv", "random variable"),
       "A numerical quantity whose value depends on the outcome of a random experiment (X = number of items a randomly chosen customer buys).",
       "discrete: p(x) = P(X = x) ; continuous: density f(x), P(a <= X <= b) = area under f",
       "Before the experiment its value is uncertain; it is described by its probability distribution.",
       "Discrete random variables take separated values with probabilities; continuous ones take values in an interval, with P(X = a) = 0.",
       "rv_discrete()"),
  .def("distribution", "Probability distribution", .def_topics[4], c("probability distribution", "pmf", "probability function"),
       "The list of the possible values of a random variable with their probabilities (discrete), or its density (continuous).",
       "p(x) >= 0 and sum p(x) = 1 ; F(x) = P(X <= x)",
       "It says how likely each value, or range of values, is.",
       "It describes the population or the process, not a sample; E(X) and Var(X) summarise it.",
       "rv_discrete()"),
  .def("expected-value", "Expected value E(X)", .def_topics[4], c("expected value", "expectation", "e(x)", "mu"),
       "The probability-weighted average of the values of a random variable.",
       "E(X) = mu = sum x p(x)",
       "The long-run average value over many repetitions of the experiment; it need not be a value X can take.",
       "Linear: E(a + bX) = a + b E(X) and E(aX + bY) = a E(X) + b E(Y), with or without independence.",
       "rv_discrete(), rv_lincomb()"),
  .def("rv-variance", "Variance of a random variable", .def_topics[4], c("var(x)", "variance of a random variable", "sd(x)"),
       "The expected squared deviation of X from its expected value.",
       "Var(X) = sum (x - mu)^2 p(x) = E(X^2) - mu^2 ; SD(X) = sqrt(Var(X))",
       "How far, typically, the values of X fall from mu: the risk or uncertainty of X.",
       "Var(a + bX) = b^2 Var(X); Var(aX + bY) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X, Y), with Cov = 0 if independent: the variances of a difference ADD.",
       "rv_discrete(), rv_lincomb()"),
  .def("normal", "Normal distribution", .def_topics[4], c("normal distribution", "gaussian", "bell curve"),
       "A continuous distribution with a symmetric bell-shaped density, fixed by its mean mu and variance sigma^2: X ~ N(mu, sigma^2).",
       "Z = (X - mu) / sigma ~ N(0, 1)",
       "About 68%, 95% and 99.7% of the values fall within 1, 2 and 3 SDs of the mean.",
       "Mean = median = mode; a linear combination of (jointly) normal variables is normal; in R: pnorm(q, mean, sd) and qnorm(p, mean, sd), with the SD, not the variance.",
       "prob_normal()"),
  .def("standardisation", "Standardisation (z-score)", .def_topics[4], c("z-score", "z score", "standardization", "standardize", "standard normal"),
       "The transformation that expresses a value as a number of standard deviations from the mean.",
       "z = (x - mu) / sigma",
       "z = 1.5 means 1.5 SDs above the mean; it makes values from different distributions comparable.",
       "If X is normal, Z is standard normal N(0, 1), so P(X <= x) = P(Z <= z).",
       "prob_normal()"),
  .def("quantile-dist", "Quantile and critical value", .def_topics[4], c("quantile of a distribution", "critical value", "z_alpha", "quantile-dist"),
       "The value with a given probability below it (the quantile q_alpha) or above it (the critical value x_alpha, the course's right subscript).",
       "P(X <= q_alpha) = alpha ; P(X > x_alpha) = alpha, so x_alpha = q_(1-alpha) ; normal: x_alpha = mu + z_alpha sigma",
       "x_0.10 = q_0.90 is the value exceeded with probability 10% (e.g. by 10% of the customers).",
       "In R qnorm() takes the probability BELOW: x_alpha = qnorm(1 - alpha, mean, sd); z_0.05 = 1.645, z_0.025 = 1.96, z_0.005 = 2.576.",
       "prob_normal(quantile = ...)"),
  .def("random-sample", "Random sample (i.i.d.)", .def_topics[4], c("iid", "i.i.d.", "simple random sample"),
       "n observations X_1, ..., X_n that are independent and identically distributed: each has the population distribution.",
       "E(X_i) = mu, Var(X_i) = sigma^2 for every i, independent",
       "Each unit is drawn by chance and independently of the others, so the sample represents the population.",
       "It is the basis of the sampling distributions: E(Xbar) = mu and Var(Xbar) = sigma^2 / n.",
       "rv_iid()"),
  .def("sampling-distribution", "Sampling distribution", .def_topics[4], c("sampling distribution", "distribution of the sample mean"),
       "The distribution of a statistic (xbar, p-hat) over all possible samples of the same size from the population.",
       "Xbar: E = mu, Var = sigma^2 / n ; P-hat: E = p, Var = p(1 - p) / n",
       "It says how the estimates would vary from sample to sample: P(Xbar > 15) = 0.08 means that about 8% of all possible samples give a mean above 15.",
       "Its standard deviation is the standard error; by the CLT it is approximately normal for large n.",
       "rv_iid(), rv_prop()"),
  .def("clt", "Central Limit Theorem (CLT)", .def_topics[4], c("central limit theorem"),
       "For a random sample of large size n (above about 30), the sample mean is approximately normal whatever the distribution of the population.",
       "Xbar ~ approx. N(mu, sigma^2 / n) ; total ~ approx. N(n mu, n sigma^2) ; P-hat ~ approx. N(p, p(1 - p) / n)",
       "Probabilities about the sample mean, the total or the sample proportion can be computed with the normal distribution.",
       "It concerns the sample mean, not single observations; if the population is normal, Xbar is exactly normal for any n.",
       "rv_iid(), rv_prop()"),
  # ---- estimation and confidence intervals ----
  .def("estimator", "Estimator", .def_topics[5], c("point estimator"),
       "A rule (a function of the sample) used to estimate a parameter, e.g. the sample mean Xbar for mu.",
       "theta-hat = f(X_1, ..., X_n)",
       "Before the sample is drawn it is a random variable, with its own sampling distribution.",
       "Unbiasedness, the standard error and consistency are properties of the estimator, not of a single estimate.",
       "est_mean(), desc_prop()"),
  .def("estimate", "Estimate", .def_topics[5], c("point estimate", "realised estimate"),
       "The value of the estimator computed on the sample actually drawn, e.g. xbar = 56.22.",
       "theta-hat computed on the observed x_1, ..., x_n",
       "Our best guess of the parameter from these data.",
       "It is a fixed number: whether it is close to the parameter is unknown; the SE describes the estimator, not this specific estimate.",
       "est_mean(), desc_prop()"),
  .def("unbiased", "Unbiased estimator", .def_topics[5], c("unbiasedness", "bias", "biased"),
       "An estimator whose expected value equals the parameter, whatever its value.",
       "E(theta-hat) = theta ; bias = E(theta-hat) - theta",
       "On average over all possible samples it hits the parameter: it does not systematically over- or underestimate it.",
       "Xbar for mu, P-hat for p and S^2 (divisor n - 1) for sigma^2 are unbiased; the variance with divisor n is biased. Unbiased does not mean that every estimate is correct.",
       "est_mean()"),
  .def("standard-error", "Standard error (SE)", .def_topics[5], c("standard error", "se", "estimated standard error"),
       "The standard deviation of the sampling distribution of an estimator.",
       "SE(Xbar) = sigma / sqrt(n), estimated by s / sqrt(n) ; SE(P-hat) = sqrt(p(1 - p) / n), estimated by sqrt(p-hat (1 - p-hat) / n)",
       "The expected deviation of a GENERIC estimate from the parameter: a smaller SE means estimates more tightly clustered around the parameter (a more reliable estimator).",
       "It refers to the estimator, not to the distance of the specific realised estimate from the parameter; it shrinks as n grows (with sqrt(n)).",
       "est_mean(), desc_prop()"),
  .def("consistent", "Consistent estimator", .def_topics[5], c("consistency"),
       "An estimator whose distribution concentrates more and more around the parameter as n grows.",
       "e.g. E(Xbar) = mu and Var(Xbar) = sigma^2 / n -> 0 as n grows",
       "With a large enough sample, the estimates are very likely to be close to the parameter.",
       "An unbiased estimator whose variance goes to 0 is consistent (Xbar, P-hat).",
       "est_mean()"),
  .def("efficiency", "Efficiency (MSE)", .def_topics[5], c("mse", "mean squared error", "efficient", "more efficient"),
       "The comparison of estimators by their mean squared error: the more efficient one has the smaller MSE.",
       "MSE = E[(theta-hat - theta)^2] = Var(theta-hat) + bias^2 (= SE^2 if unbiased)",
       "Between two unbiased estimators, the one with the smaller SE is preferred.",
       "It ranks estimators (procedures), not the realised estimates.",
       "est_mean()"),
  .def("confidence-interval", "Confidence interval (CI)", .def_topics[5], c("confidence interval", "ci", "interval estimate"),
       "An interval of values, computed from the sample, that contains the parameter with a stated level of confidence.",
       "estimate +/- reliability factor x SE ; mean: xbar +/- t_(n-1, alpha/2) s / sqrt(n) ; proportion: p-hat +/- z_(alpha/2) sqrt(p-hat (1 - p-hat) / n)",
       "\"With a level of confidence of 95% we can conclude that the <parameter in context> lies between L and U.\"",
       "The parameter is fixed and the interval is random: 95% refers to the procedure (95% of the intervals built this way contain the parameter), not to the probability that this interval contains it.",
       "ci_mean(), ci_prop()"),
  .def("confidence-level", "Confidence level", .def_topics[5], c("confidence level", "level of confidence", "1 - alpha"),
       "The proportion of all possible intervals, built with the same procedure, that contain the parameter.",
       "1 - alpha (e.g. 0.95, factor z_0.025 = 1.96)",
       "The long-run success rate of the method, fixed before sampling.",
       "A higher level gives a larger reliability factor and a wider (less precise) interval.",
       "ci_mean(), ci_prop()"),
  .def("margin-of-error", "Margin of error (ME)", .def_topics[5], c("margin of error", "me", "width", "precision", "sample size"),
       "Half the width of a confidence interval: the reliability factor times the standard error.",
       "ME = z_(alpha/2) x SE (or t_(n-1, alpha/2) x SE) ; width = 2 ME ; required n: mean n >= (z sigma / ME)^2, proportion n >= z^2 p(1 - p) / ME^2 (p = 0.5 if unknown), rounded up",
       "With the stated confidence, the estimate is within ME of the parameter.",
       "It grows with the confidence level and the variability and shrinks as n grows (four times n halves it).",
       "n_mean(), n_prop()"),
  .def("t-distribution", "Student's t distribution", .def_topics[5], c("t distribution", "student t", "t", "degrees of freedom"),
       "A symmetric bell-shaped distribution with heavier tails than the normal, indexed by its degrees of freedom.",
       "T = (Xbar - mu) / (S / sqrt(n)) ~ t_(n-1) for a normal population",
       "It is used for the CI of a mean when sigma is unknown and estimated by s: its quantiles are larger than z, so the intervals are wider.",
       "As the degrees of freedom grow it approaches N(0, 1), so for large n the t and z quantiles almost coincide; in R: qt(1 - alpha/2, n - 1).",
       "prob_t(), ci_mean()")
)

.def_norm <- function(s) trimws(gsub("[^a-z0-9^]+", " ", tolower(s)))

# Rows of .defs matching a query: exact key / term / alias, then partial, then approximate.
.def_find <- function(query) {
  q <- .def_norm(query)
  if (!nzchar(q)) return(integer(0))
  cand <- lapply(seq_len(nrow(.defs)), function(i)
    unique(.def_norm(c(.defs$key[i], .defs$term[i], sub(" \\(.*\\)$", "", .defs$term[i]),
                       regmatches(.defs$term[i], regexpr("(?<=\\()[^)]+(?=\\))", .defs$term[i], perl = TRUE)),
                       strsplit(.defs$aliases[i], "|", fixed = TRUE)[[1]]))))
  hit <- which(vapply(cand, function(cs) q %in% cs, logical(1)))
  if (length(hit)) return(hit)
  if (nchar(q) >= 3) {
    hit <- which(vapply(cand, function(cs) any(grepl(q, cs, fixed = TRUE)), logical(1)))
    if (length(hit)) return(hit)
  }
  # misspellings: whole-name edit distance of at most 20% of the query ("standart deviation")
  if (nchar(q) < 4) return(integer(0))
  d <- vapply(cand, function(cs) min(utils::adist(q, cs)), numeric(1))
  which(d <= max(1, floor(0.2 * nchar(q))))
}

.def_lines <- function(i, width = getOption("width", 80)) {
  d <- .defs[i, ]
  w <- max(40, min(width, 100))
  part <- function(label, txt) if (nzchar(txt)) strwrap(txt, width = w, initial = sprintf("%-12s", label), exdent = 12)
  head <- sprintf("== %s  [%s] ", d$term, d$topic)
  c(paste0(head, strrep("=", max(3, w - nchar(head)))),
    part("What it is", d$what), part("Formula", d$formula), part("Meaning", d$meaning),
    part("Property", d$property), part("Computed by", d$fn))
}

#' Exam-ready definitions of the course terms
#'
#' Prints the definition of a term of the first partial in the four parts of
#' a definition answer: what it is, the formula, what it means for the units,
#' and one property or caveat. Names can be abbreviated or approximate
#' (`"IQR"`, `"interquartile"`, `"standard error"`, `"se"`, `"CLT"`).
#' Without a term, lists every term by topic.
#'
#' @param term One or more terms to define; `NULL` lists them all.
#' @return Invisibly, the lines printed.
#' @examples
#' sc_define("IQR")
#' sc_define(c("estimator", "estimate"))
#' sc_define()
#' @export
sc_define <- function(term = NULL) {
  if (is.null(term)) {
    out <- "Definitions: sc_define(\"<term>\")   (abbreviations work: \"IQR\", \"se\", \"CLT\")"
    for (tp in .def_topics) {
      sub <- .defs[.defs$topic == tp, ]
      out <- c(out, "", tp, sprintf("  %-22s %s", sub$key, sub$term))
    }
    cat(out, sep = "\n")
    return(invisible(out))
  }
  out <- character(0)
  for (t in term) {
    hit <- .def_find(t)
    lines <- if (length(hit) == 1) .def_lines(hit)
      else if (length(hit) > 1) c(sprintf("\"%s\" matches several terms; use one of these:", t),
                                  sprintf("  %-32s %s", sprintf("sc_define(\"%s\")", .defs$key[hit]), .defs$term[hit]))
      else c(sprintf("No definition for \"%s\". sc_define() lists every term; for a whole topic see sc_notes().", t))
    out <- c(out, if (length(out)) "", lines)
  }
  cat(out, sep = "\n")
  invisible(out)
}
