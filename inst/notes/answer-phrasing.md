# HOW OFFICIAL ANSWERS ARE PHRASED (works for questions no past paper asked)
(extracted from the official solutions of the 2024/25 and 2025/26 partials and general exams
and of the Module 1 exercises)

statcram prints its "Exam wording" with the same labels (Frame, Tool, Result, Meaning, Conclusion,
Caveat); the Computation is the step-by-step block above it.
options(statcram.labels = FALSE) removes the labels.

## A. Seven moves, always in this order (use the ones the question needs)
1. FRAME       say what is analysed, in statistical terms: the variable and its type, sample or
               population, the conditional distribution ("Bid | Agency", "LeadTime | Channel =
               Aggregator"), the parameter (mu, p, beta).
2. TOOL + WHY  "To <goal>, the appropriate tool is <Y>, because <property of Y>".
               scatterplot -> it shows departures from linearity; CV -> different units / means;
               conditional % -> groups of different size; boxplots side by side -> location,
               variability and shape at a glance; both mean and median -> the tails differ.
3. COMPUTATION formula in symbols -> numbers substituted -> result.
               Q1 - 1.5 IQR = 50.8225 - 1.5 x 11.895 = 32.98.  "It is not sufficient to report
               only the final result."
4. RESULT      the number with its object (and unit): "P10 = 39.46", "r = -0.79".
5. MEANING     what the number says about the units, in the words of the question:
               "10% of clients paid (in relative terms) at most 39.46".
6. CONCLUSION  answer the literal question, verdict first:
               "Therefore 35 is not extreme." / "No conclusions can be drawn." /
               "We reject H0: there is sufficient empirical evidence that ...".
7. CAVEAT      what cannot be said and why: approximation (uniform within classes), reliability
               (r high but not reliable), estimator vs realised estimate, association is not
               causation, sample vs population. Optional "Note:" = substantive hypothesis in context
               ("one may hypothesise that agency customers pay more and bid less").

## B. The verb of the question decides which moves carry the marks
describe / compare ..................... FRAME + MEANING, organised as SHAPE, LOCATION, VARIABILITY
which measure / tool would you use .... TOOL + WHY (property), then the evidence with numbers
determine / calculate / report ........ COMPUTATION + RESULT + MEANING
explain / interpret / what information  MEANING, phrased as a definition ("the value below which ...")
how would you proceed / assess ......... TOOL as ordered steps, then COMPUTATION
can we conclude / is it extreme /
is the statement correct ............... CONCLUSION FIRST, then the rule with numbers, then the reason.
                                         A statement can be "only partially correct: ... However ..."
sketch a graph ......................... name the graph, then what it shows (shape, location, spread)

## C. Phrase bank (fill the slots)
Location / quantiles
  "p% of the units have X at most Pp, and p% more than P(100-p): the central (100-2p)% lie between them."
  "Pp is the maximum value reached by the lowest p% (and P(100-p) the minimum of the highest p%)."
  "The median of A (m) is the maximum value reached by the lowest 50% of A."
Comparing groups (conditional distributions)
  "The distributions of X | G differ in location, variability and shape."
  "A has lower measures of location: its values therefore tend to be lower."
  "A has the smallest IQR: its central 50% is more concentrated than for the other groups."
  "Q3 of A < Q1 of B: at least 75% of A's values are lower than at least 75% of B's."
Shape
  "fairly symmetric" / "right(left)-skewed: longer upper (lower) tail" /
  "strongly left-skewed because of the numerous lower outliers" /
  "the central part is fairly symmetric, although there are some outliers, mainly on the lower side"
Mean vs median
  "mean and median offer a similar description of the centre (symmetric distribution)"
  "the medians are aligned and do not reflect the long tail, which is captured by the means"
  "the median is robust to extreme values; the mean is attracted by the tail"
Outliers
  "An extremely low value is below Q1 - 1.5 IQR = ... ; since v > ..., v is not extreme."
Relationship between numerical variables
  "The variables are linked by an inverse (direct) relationship: as X increases, Y tends to decrease."
  "The relationship is not linear: the fitted line is a compromise, it describes the centre of the
   data but not the tails."
  "r = -0.79 is high in absolute value but not reliable, since the data do not cluster around a
   single straight line."
  "Correlation does not imply causation."
Dispersion
  "To compare the dispersion of variables with different units (or very different means) we need
   the coefficient of variation CV = s / mean."
Conditional distributions / association
  "We are interested in the percentage of <A> conditional on <B>."
  "Among the units with B = b1, x% have A = a; among those with B = b2, y%."
  "It would be wrong to answer with joint counts or joint percentages (the groups differ in size)."
  "If the variables were independent, every conditional share would equal the marginal share."
Grouped data (classes)
  "The values are approximations, assuming a uniform distribution within each class
   (midpoints for the mean)." / "The modal class is the one with the highest DENSITY."
Estimation
  "The estimator of mu is the sample mean, unbiased: E(Xbar) = mu; Var(Xbar) = sigma^2 / n."
  "The exact standard error sigma / sqrt(n) cannot be determined, as sigma is unknown; it is
   estimated by s / sqrt(n) = ..."
  "The standard error refers to the sampling distribution of the estimator: the expected deviation
   of a GENERIC estimate from the parameter, not of the specific realised estimate."
  "The estimator with the smaller SE is more reliable (its estimates are more tightly clustered
   around the parameter); nothing can be said about the realised estimates."
Confidence intervals
  "With a level of confidence of 95% we can conclude that <parameter in context> lies between L and U."
  "The parameter is fixed: 95% refers to the procedure (95% of the intervals built this way
   contain it), not to this interval."
Tests
  "H0: <status quo, with the equality>; H1: <what has to be proven>."
  "Since the p-value (p) is below alpha, we reject H0: there is sufficient empirical evidence
   that ..." / "... we fail to reject H0 (never 'accept'): insufficient evidence that ..."
  "p-value: the probability, under H0, of a statistic at least as extreme as the observed one."
  "alpha, beta and the power describe the procedure, not the decision taken on this sample."
Regression
  "On average, holding the other explanatory variables constant, ..." /
  "relative to the baseline level ..." / "R^2: share of the variability of Y explained by the model."

## D. Language rules
- Use the course notation: Bid | Agency, Freq(...), P(...), H0 / H1, se, Q1 - 1.5 IQR.
- Qualify every claim: sample vs population, estimate vs estimator, generic vs realised,
  approximate vs exact.
- Distributions "tend to": "bids tend to be lower", "on average".
- Every number goes with its object and its meaning; quote the numbers you use.
- Never: "accept H0"; "the probability that the parameter is in the interval"; causal claims from
  association; comparisons on joint counts; a yes/no without the measure that supports it.
