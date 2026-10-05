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

## Location and dispersion (ch. 3: to be aligned with the book)
- mean, median, mode, quartiles, percentiles; mean vs median and quartile asymmetry -> skewness.
- range, IQR, variance, SD, coefficient of variation CV = s / |mean| (unit-free: use it to compare
  dispersion across different units / very different means).
- Grouped data: mean ~ sum(m_k p_k) with midpoints m_k; quantiles by interpolation within classes.
