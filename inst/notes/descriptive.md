# DESCRIPTIVE ANALYSIS (book chapters 2-3)

## Types of variables (ch. 1)
- Qualitative (categorical): nominal (no order) or ordinal (ordered levels, e.g. satisfaction).
  Number codes (1 = F, 2 = M; Likert 1-7) do NOT make a variable numerical.
- Quantitative (numerical): discrete (counting, integers) or continuous (measurement).
  Few distinct values -> tables / spike plots; many values -> classes, histograms.
- Measured in classes: only the interval is known (e.g. MonthExp "[50,100)") -> approximations.
- Parameter = summary of the POPULATION; statistic = summary of the SAMPLE.

## Frequency distribution (ch. 2)                                 desc_freq()   sc(1,1)
- K distinct values x*_1 ... x*_K, counts f_k, proportions p_k = f_k / n, percentages 100 p_k.
- Cumulative relative frequency F_k = p_1 + ... + p_k  (only for ordered levels!).
- Ordinal character variables: put the levels in order first
  (factor(x, levels = c(...)) or desc_freq(x, order = c(...))); otherwise R sorts alphabetically.
- F(x) = Freq(X <= x) for ANY real x: a step function, constant between observed values,
  0 below the minimum, 1 from the maximum on (e.g. F(3.5) = F(3) if 3.5 is not observed).
- Plots: pie / bar chart for qualitative (bar chart for ordinal, keep the order; Pareto =
  bars in decreasing frequency); spike plot for numerical with few values; histogram /
  density plot for many values; step diagram for cumulative frequencies.

## Classes / intervals (ch. 2)                                     desc_classes()   sc(1,2)
- Classes must be exhaustive and mutually exclusive: [a, b) closed on the left, open on the
  right, EXCEPT the last class [a, b] (closed).
- Equal width with K classes: w = (Max - Min) / K, starting at Min  (desc_classes(x, breaks = K)).
- Frequency density c_k = p_k / w_k = proportion of cases per unit interval.
- Histogram: rectangle AREA = p_k, so total area = 1. With DIFFERENT widths the histogram MUST
  use densities (heights c_k); a bar plot of proportions for unequal classes is misleading.
- Data measured in classes: proportions inside a class are approximated assuming values are
  spread uniformly within the class:
     Freq(X <= x) ~ Freq(X < l_j) + c_j (x - l_j)     for x in class [l_j, u_j)
  e.g. Freq(X <= 250) ~ Freq(X < 200) + c_5 x 50; half a class = p_k / 2.
- Ogive (cumulative frequency polygon): joins F at the upper limits; the slope of each segment
  is the class density. It assumes a constant rate within each class (approximation).
- Modal class: highest DENSITY (not highest frequency) when widths differ.

## Misleading representations (ch. 2.5)
- Vertical axis not starting at 0 exaggerates differences.
- Pictograms / word clouds exaggerate (area grows faster than the frequency).
- Bar plots for unequal classes ignore widths: use a density histogram.

## Central tendency (ch. 3.2)                                       desc_summary()  sc(1,3)
- Mode: most frequent value, for ANY type of variable; can be multimodal; no mode (useless)
  when all values have the same frequency; weak for continuous data -> modal class (highest density).
- Median: middle value of the ordered data (n odd) or average of the two middle values (n even);
  equivalently the smallest value with F >= 0.5, and for NUMBERS the midpoint with the next value
  when F is exactly 0.5. For ORDINAL data: smallest level with F >= 0.5 (no averaging).
  Not defined for nominal variables.
- Mean: xbar = sum(x_i) / n; from a frequency distribution xbar = sum(x*_k p_k). Centre of gravity:
  sum(x_i - xbar) = 0. Sensitive to extreme values (non-robust); the median is robust.
- Right-skewed: mean > median; left-skewed: mean < median (only an indication).
- Binary 0/1 or logical variable: the mean is the proportion of 1s / TRUEs.
- Grouped / classes (approximations, uniform within classes):
     median = l_k + (0.5 - F_(k-1)) / c_k   in the first class with F_k >= 0.5
     mean   ~ sum(m_k p_k) with midpoints m_k   (NOT computable with an open-ended class;
     the median still is, if it does not fall in the open class)

## Quartiles, percentiles, boxplot (ch. 3.3)
- Q1, Q2 = median, Q3 split the ordered data into 4 blocks of about 25%.
- Several rules exist. R's quantile() default (used by R / UBStats output) interpolates.
  The book's HAND rule: Q_p = smallest value with cumulative frequency >= p (0.25, 0.75);
  the only rule for ordinal data and grouped data. desc_summary() shows both when they differ.
- Grouped / classes: Q_p = l_k + (p - F_(k-1)) / c_k  (e.g. Q1 = 20 + (0.25 - 0.2315)/0.05754).
- Five-number summary: Min, Q1, Q2, Q3, Max -> boxplot (numerical variables only, not ordinal).
- Shape from the boxplot: (Q2 - Q1) vs (Q3 - Q2) and (Q1 - Min) vs (Max - Q3):
  similar -> symmetric; upper parts longer -> right-skewed; lower parts longer -> left-skewed.
- Enhanced boxplot (Tukey): values beyond 1.5 x IQR from the box are EXTREME (outliers);
  whiskers end at the smallest / largest REGULAR values inside [Q1 - 1.5 IQR, Q3 + 1.5 IQR].
- Percentiles P_q: q% of the data below; useful for long tails (P90, P95, P99), P90/P10 ratios.

## Dispersion (ch. 3.4)                                              desc_summary(), desc_cv()
- Range = Max - Min (non-robust). IQR = Q3 - Q1 (robust; width of the box).
- Variance: population sigma^2 = sum(x_i - mu)^2 / N; sample s^2 = sum(x_i - xbar)^2 / (n - 1).
  Short-cut: s^2 = n/(n-1) [ sum(x_i^2)/n - xbar^2 ]. Unit: squared unit of the data.
- SD s = sqrt(s^2): average distance from the mean, same unit as the data.
- Grouped data / classes: s^2 = n/(n-1) [ sum(x*_k^2 p_k) - xbar^2 ]  (midpoints m_k for classes);
  if n is unknown use sum(x*_k^2 p_k) - xbar^2 (n/(n-1) ~ 1 for large n).
- Coefficient of variation CV = s / |xbar| (often as %): unit-free; use it to compare dispersion of
  variables in different units or with very different means. It has no fixed range, so it does not
  say whether ONE distribution is "highly dispersed".
- Changing the unit (minutes -> seconds x 60): mean x 60, SD x 60, variance x 60^2, CV unchanged.
