# STAT//CRAM — First-Partial Phrasing Bank

**167 source-adapted answer cases · 10 verbatim solution excerpts · Chapters 1–6**

These are ready-to-write answer templates for the question types and variations supported by your uploaded course materials. They are not magic wording, a guarantee of marks, or a claim to cover every possible future question. Select the matching case and replace bracketed placeholders with the actual population, variable, numbers and units. Use only statements supported by the exercise. The main bank is source-adapted; genuinely verbatim excerpts are separately labelled in Appendix B.

## Scope and source rules

- The first-midterm scope is Lessons 1–14. The dedicated RStudio introduction is omitted here at your request; relevant base-R and UBStats interpretation remains.
- Chapters 1–4: apply the syllabus exclusions, including Chapter 1 §§1.3.1–1.3.2, Chapter 2 §2.4 / density plots / Example 2.5, and Chapter 4 categorical association measures. Histogram frequency density is still included.
- Chapter 5: probability foundations and Bayes are included as prerequisite revision and because you requested extra emphasis. Binomial and Uniform distribution procedures and §5.3.4 are excluded. The within-class uniformity assumption is still included.
- Chapter 6: one-mean and one-proportion point and interval estimation and sample-size planning. Formal hypothesis testing, two-population estimation and regression-model inference are not part of this bank.
- Supplementary estimator definitions are labelled separately because the syllabus's named properties prioritise unbiasedness, asymptotic unbiasedness, variance and standard error.
- This is a study/answer-writing aid, not a claim that all wording must be reproduced verbatim or that any document or software is authorised for consultation during an exam.

## Quick navigation

- [Chapter 1 | Identify what is being analysed](#section-01)
- [Chapter 2 | Frequencies, graphs and classes](#section-02)
- [Chapter 3 | Centre, quantiles, dispersion and outliers](#section-03)
- [Chapter 4 | Joint analysis, association and correlation](#section-04)
- [Chapter 5A | Probability, conditioning and Bayes](#section-05p)
- [Chapter 5B | Random variables and transformations](#section-05r)
- [Chapter 5C | Normal probabilities and quantiles](#section-05n)
- [Chapter 5D | Sampling distributions and the CLT](#section-05s)
- [Chapter 6A | Point estimation and standard errors](#section-06e)
- [Chapter 6B | Confidence intervals: meaning and method](#section-06i)
- [Chapter 6C | Required sample size and R output](#section-06n)
- [Appendix A | Additional estimator definitions in the book](#section-06x)
- [Official-solution excerpts](#official-excerpts)
- [Your personal error log](#personal-error-log)
- [Sources and source-fidelity notes](#sources)

## Read the verb first

**Determine / calculate / report:** State the requested object, show formula → substituted numbers → result where required, then give the result's contextual meaning.

**Which tool / measure / graph?:** Name the tool and the property that makes it appropriate, then support the choice with the data.

**Describe / compare / substantive considerations:** For a distribution: shape, location and variability, with contextual comparisons. For a relationship: direction, form and whether the strength measure is reliable.

**Define / explain / interpret:** State what the quantity is, what it measures in this setting, and the limitation needed to prevent a wrong interpretation.

**Can we conclude / is the statement correct?:** Give the verdict, then the relevant rule and numerical evidence. Qualify what the information cannot establish.

**Sketch / provide a graph:** Supply the actual requested graph or sketch as well as the accompanying explanation; prose alone does not replace the drawing.


## Notation you must not mix up

**x̄ / X̄:** Observed sample mean / random sample-mean estimator; E(X̄)=μ, not necessarily the observed x̄.

**p / P̂ / p̂:** Population proportion / random sample-proportion estimator / its observed estimate. The book also uses P for the estimator; the hat is retained here to make the distinction visible.

**σ / s:** Population standard deviation / observed sample standard deviation.

**SE / estimated SE:** The estimator's standard deviation / a sample-based estimate of that standard deviation.

**z_α and t_(ν,α):** Upper-tail quantiles: qnorm(1−α) and qt(1−α,df=ν), not the default lower-tail quantile at α.

**Source-adapted template:** A ready-to-write sentence written for this bank, grounded in the listed course sources. Not a purported literal official quotation.


<a id="section-01"></a>

## Chapter 1 | Identify what is being analysed

Choose the unit, variable and population before selecting a statistical method.

**Basis:** F §1; B1 §§1.1–1.5; G: types of variables


### 1.01 — Population, sample and statistical unit

**Use when:** “Identify the population / sample / statistical unit.”

**Write:**

> The population of interest consists of all [units within the stated scope]. The sample consists of the [n] units observed. The statistical unit is one [customer / transaction / session / company], because the measurements are recorded separately for each such unit.
**Avoid:** Do not define the population only as a number. Do not interchange customers, orders and sessions.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


### 1.02 — Nominal or ordinal

**Use when:** “Specify the type of the variable.” Select the applicable sentence.

**Write:**

> NOMINAL: [X] is a qualitative nominal variable because its categories have no substantive order.
> ORDINAL: [X] is a qualitative ordinal variable because its categories can be ordered, but the distances between consecutive categories cannot be quantified.
**Avoid:** Numerical codes for categories do not turn them into numerical measurements.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


### 1.03 — Discrete or continuous

**Use when:** A numerical variable must be classified.

**Write:**

> DISCRETE: [X] is quantitative discrete because it records a count and takes separated numerical values.
> CONTINUOUS: [X] is quantitative continuous because it represents a measurement that is modelled as taking any value within an interval.
**Qualification:** Recorded monetary amounts and times are treated according to the course's modelling convention and the exercise.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


### 1.04 — Identifiers and data in classes

**Use when:** An ID or interval label appears as a column.

**Write:**

> IDENTIFIER: [ID] identifies the statistical units; its numerical labels do not measure a substantive characteristic.
> CLASSES: The underlying variable is numerical, but only the interval containing each observation is available. The exact individual measurements cannot be recovered from these labels.
**Avoid:** Do not calculate a mean of customer IDs or treat interval labels as known exact amounts.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


### 1.05 — Parameter versus sample statistic

**Use when:** “Is this value a parameter or a statistic?”

**Write:**

> The value [v] is a sample statistic because it summarises the observed sample. The corresponding population parameter is [μ / σ² / p], which describes the entire population and is not necessarily equal to [v].
**Avoid:** A sample mean is not the expected value of the sample-mean estimator. See 6E.01.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


### 1.06 — Descriptive versus inferential

**Use when:** “What kind of analysis is being performed?”

**Write:**

> DESCRIPTIVE: The statement summarises the observations available in [the sample / the population] without extrapolating beyond them.
> INFERENTIAL: The statement uses sample information to draw conclusions about the population and therefore must account for sampling uncertainty.
**Qualification:** Descriptive statistics is not limited to population data; it also describes samples.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


### 1.07 — Data source and study type

**Use when:** “How were the data obtained?”

**Write:**

> PRIMARY / SECONDARY: The data are [primary, collected directly for this investigation / secondary, obtained from an existing source].
> EXPERIMENT / OBSERVATION: The study is [experimental, because the researcher assigns an intervention / observational, because outcomes are recorded without assigning the conditions being compared].
**Avoid:** Random selection of a sample is not the same operation as random assignment to treatments.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


### 1.08 — Simple random and systematic sampling

**Use when:** A sampling procedure must be named.

**Write:**

> SIMPLE RANDOM: The units are selected at random from the same population using the simple-random-sampling framework of the course.
> SYSTEMATIC: After a random starting position, every [k]th unit is selected. This is systematic sampling, with k = N/n. The ordering must not contain a pattern that conflicts with this interval.

**Formula / R:** `k = N/n`

**Qualification:** Detailed advanced sampling schemes in Chapter 1 §§1.3.1–1.3.2 are outside the stated syllabus.

*Sources: F §1; B1 §§1.1–1.5; G: types of variables*


<a id="section-02"></a>

## Chapter 2 | Frequencies, graphs and classes

Name the graph, explain why, and interpret the frequency with the correct denominator.

**Basis:** F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data


### 2.01 — A graph for a categorical variable

**Use when:** “Which graph is appropriate?”

**Write:**

> NOMINAL: A bar chart or a pie chart is appropriate for comparing the frequencies of the categories of [X]. The ordering of nominal categories has no substantive interpretation.
> ORDINAL: An ordered bar chart is appropriate because it displays the category frequencies while preserving their substantive order.
**Avoid:** A numeric satisfaction code does not justify a histogram.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.02 — A graph for a numerical variable

**Use when:** Select according to the number and nature of observed values.

**Write:**

> FEW DISCRETE VALUES: A spike plot is appropriate because [X] is numerical and takes few distinct values; the horizontal positions preserve the numerical distances.
> MANY VALUES / CONTINUOUS: A histogram is appropriate because grouping [X] into intervals provides a readable description of its distribution.
**Qualification:** Module 1 also notes that a bar chart need not distort distances when the discrete values are equally spaced. The general course choice remains the spike plot.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.03 — Cases are not categories of the analysed variable

**Use when:** You are tempted to put each named unit on the horizontal axis.

**Write:**

> The [states / customers / firms] are the statistical units. The variable whose distribution is being described is [X]. A histogram groups the values of [X] and shows the frequency of units in each interval; it does not display a separate category for every named unit.
**Avoid:** “There are 50 states, therefore the distribution needs 50 bars.”

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.04 — Count, proportion or percentage

**Use when:** “How many?” versus “What proportion / percentage?”

**Write:**

> There are [f] observations satisfying [condition], out of [n] observations in the relevant sample. Their relative frequency is [f/n], so [100f/n]% of the sampled [units] satisfy the condition.

**Formula / R:** `Count = f; proportion = f/n; percentage = 100f/n`

**Avoid:** Report the requested form. A raw proportion of 0.15 is 15%, not 0.15%.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.05 — Raw data: count directly

**Use when:** Individual observations are available.

**Write:**

> Since the individual measurements are available, the required frequency is calculated directly by counting observations satisfying [condition] and dividing by the relevant sample size. No within-class uniformity assumption is required.

**Formula / R:** `R: mean(x > a)       R: 100 * mean(x > a)`

**Qualification:** Handle missing values explicitly and use the number of valid observations; do not group raw values unnecessarily.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.06 — Cumulative frequency and strict inequalities

**Use when:** “At most”, “below”, “above”, or “between”.

**Write:**

> The cumulative relative frequency F(a) is the proportion of observations with X ≤ a. The proportion above a is 1 − F(a). For a < X ≤ b, the proportion is F(b) − F(a). I retain the strict or inclusive endpoints stated in the question.

**Formula / R:** `F(a) = Freq(X ≤ a); Freq(a < X ≤ b) = F(b) − F(a)`

**Avoid:** With discrete/raw data, X < a and X ≤ a need not have the same frequency.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.07 — Equal-width versus unequal-width histograms

**Use when:** “Can counts or proportions be plotted as heights?”

**Write:**

> EQUAL WIDTHS: Counts, proportions and densities give the same histogram shape, differing only by a vertical rescaling.
> UNEQUAL WIDTHS: Frequency densities must be used as heights. Each rectangle's area, rather than its height, represents the class proportion.

**Formula / R:** `wₖ = upperₖ − lowerₖ; cₖ = pₖ/wₖ; area = cₖwₖ = pₖ`

**Qualification:** The total area is 1 for a proportion-density histogram, not for every possible rescaling.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.08 — Modal class

**Use when:** “Report the modal class and the criterion.”

**Write:**

> The modal class is [interval], because it has the highest frequency density, [c]. Since the classes have different widths, comparing their counts or proportions alone would not identify the greatest concentration per unit of the variable.

**Formula / R:** `cₖ = fₖ/(n wₖ)`

**Avoid:** The widest class can have the largest count without being modal.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.09 — Choose between alternative class systems

**Use when:** The exercise shows two classifications.

**Write:**

> Classification [A] is preferable here because it displays [specific feature] more clearly. Narrower classes in the concentrated region and wider classes in the sparse tail can summarise the distribution without losing relevant detail. The choice must be justified by the graphs; unequal widths are not inherently better.
**Qualification:** Do not announce a universally optimal number of classes.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.10 — Recover a frequency from a histogram

**Use when:** A density height and interval width are available.

**Write:**

> The proportion in [interval] is its rectangle area: density × width = [calculation] = [p]. Therefore, approximately [100p]% of the observed [units] are in this interval.

**Formula / R:** `pₖ = cₖwₖ; fₖ = n cₖwₖ`

**Qualification:** If the height was read approximately from a graph, label the recovered value approximate.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.11 — A threshold inside a class

**Use when:** Only grouped data are available; the interval cuts through a class.

**Write:**

> The exact frequency cannot be obtained because individual values within the class are unavailable. Assuming uniformity within [class], the relevant subinterval contains [subwidth/classwidth] of the class frequency. The estimated [percentage / count] is therefore [calculation] = [result].

**Formula / R:** `Estimated subinterval proportion = pₖ × subwidth/wₖ = cₖ × subwidth`

**Avoid:** A fractional estimated count is not a literal fractional customer. Report approximately [rounded count] customers.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.12 — A range spans several classes

**Use when:** Both endpoints may cut through classes.

**Write:**

> Assuming uniformity within each partially included class, I add the estimated frequencies in the two partial classes and the frequencies of all completely included classes. This gives [result] [customers / %]. The result is approximate because the positions within the partial classes are unknown.
**Avoid:** Do not omit a fully included middle class.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.13 — Threshold at a class boundary

**Use when:** Cumulative counts are known for complete classes.

**Write:**

> The required proportion is obtained by adding the frequencies of the complete classes below the boundary and dividing by n. No within-class interpolation is needed for this class total.
**Avoid:** Do not silently equate strict and inclusive empirical counts.

**Qualification:** With [a,b) classes, the cumulative total up to the end of that class counts X < b. If the question distinguishes X ≤ b and observations may equal b, the boundary count is not recoverable from the grouped table alone; under the continuous within-class model it does not affect the calculation.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.14 — An ogive or cumulative curve

**Use when:** “Interpret / construct the cumulative representation.”

**Write:**

> The curve reports the cumulative proportion of observations up to each value. For data known only in classes, the ogive joins cumulative class totals at the boundaries; straight-line interpolation within a class corresponds to assuming uniformity there.

**Formula / R:** `Within class k: F(x) ≈ F_before + cₖ(x − lowerₖ)`

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.15 — Comment on a percentage claim

**Use when:** “Is the manager's statement correct?”

**Write:**

> The statement is [supported / not supported] by the observed data. The frequency satisfying the stated condition is [calculation] = [percentage]%, which is [consistent / inconsistent] with the claimed [percentage]%.
**Avoid:** A descriptive discrepancy is not automatically a formal significance test or a population conclusion.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


### 2.16 — Counts versus relative frequencies across samples

**Use when:** The compared samples or groups have different sizes.

**Write:**

> Because the groups have different sizes, their absolute counts are not directly comparable as measures of relative importance. I compare relative frequencies calculated within each group; [outcome] represents [a]% of [A] and [b]% of [B].
**Qualification:** For two variables, see 4.03–4.06 for the conditional denominator.

*Sources: F §2; B2 §§2.1–2.3, 2.6; M1 Exercises 1.1–1.6; G: grouped data*


<a id="section-03"></a>

## Chapter 3 | Centre, quantiles, dispersion and outliers

Select the measure for the question, not just the one that is easiest to calculate.

**Basis:** F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1


### 3.01 — Mode and representativeness

**Use when:** “Which measure of centre?” for nominal data, or “Report the mode.”

**Write:**

> The mode is [category/value], observed with relative frequency [p]. It is the most frequent category.
> IF DOMINANT: It provides a useful description of the most typical category.
> IF CLOSE TO OTHERS: It is not particularly representative because [other category] has a similar frequency.
**Avoid:** Most frequent does not necessarily mean a majority of observations.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.02 — More than one mode

**Use when:** Several values share the maximum frequency.

**Write:**

> The distribution has [number] modes, [values], because they share the highest observed frequency. No single value is uniquely most frequent.
**Qualification:** For classes use density, not frequency alone; for many distinct numerical values the raw mode may be uninformative.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.03 — Arithmetic mean: raw data or frequency table

**Use when:** “Calculate the mean.”

**Write:**

> The arithmetic mean is the total observed [quantity] divided by the number of [units]. From the frequency table, each value is weighted by how often it occurs. The sample mean is [value] [units], so the observed [units] have an average [X] of [value].

**Formula / R:** `x̄ = Σxᵢ/n = Σx*ₖfₖ/n = Σx*ₖpₖ`

**Avoid:** Do not divide by the number of distinct values. A mean need not itself be an observed value.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.04 — Median: numerical observations

**Use when:** “Determine the median.”

**Write:**

> After ordering the values, I identify where the cumulative relative frequency first reaches or exceeds 0.50. If it exceeds 0.50, the median is that value. If it equals 0.50 exactly at a numerical value, I take the midpoint between that value and the next observed value. Here the median is [value].
**Avoid:** Do not automatically select the upper middle observation for an even sample.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.05 — Median: ordinal observations

**Use when:** The categories have a meaningful order but no measurable distances.

**Write:**

> After arranging the categories in their substantive order, the median is [category], the first category whose cumulative relative frequency reaches at least 0.50. Averaging two category labels would not be meaningful.
**Avoid:** Alphabetical ordering and numerical coding do not supply a substantive order or numerical distances.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.06 — Symmetric distribution: mean versus median

**Use when:** The graph supports approximate symmetry.

**Write:**

> The distribution is approximately symmetric, and the mean [x̄] and median [Me] offer a similar description of its centre. Reporting these measures gives a consistent indication of typical [X].
**Avoid:** Symmetric does not automatically mean Normally distributed.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.07 — Right- or left-skewed distribution

**Use when:** The graph supports the direction of the tail.

**Write:**

> RIGHT-SKEWED: The distribution has a long right tail. Large values pull the mean upwards, while the median is less affected; the median is particularly informative about the centre.
> LEFT-SKEWED: The distribution has a long left tail. Small values pull the mean downwards, while the median provides a more robust summary of the centre.
**Qualification:** Use the observed means/medians where available. Their ordering is supporting evidence, not a proof of shape.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.08 — Why report both mean and median?

**Use when:** “Highlight the main characteristics” or different tail behaviour across groups.

**Write:**

> Both the mean and the median should be reported. The medians describe the central locations, while the means also reflect the influence of the tails. Similar medians can therefore conceal differences that become apparent when comparing the means.
**Avoid:** “Skewed, therefore never report the mean.”

*Sources: F §3; B3 §3.2; P25 Q1.b*


### 3.09 — Mean greater than median: can we conclude skewness?

**Use when:** Only summaries are given, without a graph.

**Write:**

> A mean greater than the median is consistent with a right-skewed distribution, but does not establish the complete shape by itself. I would examine a histogram or boxplot before drawing a definite conclusion about asymmetry.
**Qualification:** Reverse the direction for a mean below the median. Similar mean and median do not guarantee symmetry.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.10 — Quartiles by hand

**Use when:** “Determine Q₁ and Q₃” from ordered raw values or a frequency table.

**Write:**

> Using the course's manual convention, Q₁ is the smallest observed value whose cumulative relative frequency is at least 0.25, and Q₃ is the smallest value reaching at least 0.75. Here Q₁ = [a] and Q₃ = [b].
**Avoid:** Do not import a software interpolation convention. The numerical median's exact-0.50 exception is not an automatic averaging rule for Q₁ and Q₃.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.11 — Interpret quartiles and tied values

**Use when:** “Explain what the quartiles tell us.”

**Write:**

> At least 25% of observations have values at most Q₁ = [a], and at least 75% have values at most Q₃ = [b]. The interval between the quartiles describes the central half of the distribution.
**Qualification:** With ties, the share at or below a quartile can exceed its nominal share. Say 'at least' rather than claiming an exact split.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.12 — Selected percentiles and the central share

**Use when:** “Report P₁₀ and P₉₀ and explain their information.”

**Write:**

> The 10th percentile is [a] and the 90th percentile is [b]. These mark the lower and upper decile cutoffs: approximately 10% of [units] have values at most [a], and approximately 10% have values above [b]. The interval describes the central 80% of [X].
**Qualification:** This wording follows the numerical past-paper example. With discrete ties, use the cumulative-frequency inequalities rather than claiming exact percentages.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.13 — Describe the upper tail or the most standard observations

**Use when:** Select the sentence corresponding to the requested share.

**Write:**

> UPPER TAIL: I report a high percentile such as P₉₀ = [v], which identifies the cutoff for the highest 10% of [X].
> CENTRAL 90%: The interval from P₅ to P₉₅ describes the central 90% of the distribution.
> TOP 15%: The relevant range runs from P₈₅ to the maximum.
**Qualification:** Use approximately for empirical percentile shares when ties or interpolation matter.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.14 — Quantile inside a class

**Use when:** The target cumulative share falls within a finite interval.

**Write:**

> The target cumulative proportion [q] lies in [class], since the cumulative frequency rises from [F_before] to [F_after] there. Assuming uniformity within the class, the estimated quantile is lower bound + ([q] − F_before)/density = [value].

**Formula / R:** `Q(q) ≈ lowerₖ + (q − F_before)/cₖ; cₖ = pₖ/wₖ`

**Avoid:** Do not use the midpoint unless the required interpolation actually gives it.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.15 — Mean from class data

**Use when:** Only class frequencies are known.

**Write:**

> The exact mean is not recoverable from the grouped data. I represent each class by its midpoint and calculate their frequency-weighted mean. The resulting approximate mean is [value], based on the within-class approximation.

**Formula / R:** `mₖ = (lowerₖ + upperₖ)/2; x̄ ≈ Σmₖpₖ`

**Avoid:** Check every midpoint first: the midpoint of [15,30) is 22.5, not 27.5.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.16 — Variance and SD: sample versus population

**Use when:** “Evaluate dispersion” from raw data.

**Write:**

> SAMPLE: I divide the sum of squared deviations from the sample mean by n − 1, obtaining s² = [value]. The sample standard deviation is its square root, s = [value] [units].
> POPULATION: When the observations constitute the complete population, I divide the sum of squared deviations from μ by N.

**Formula / R:** `s² = Σ(xᵢ − x̄)²/(n − 1) = (Σxᵢ² − nx̄²)/(n − 1); σ² = Σ(xᵢ − μ)²/N`

**Avoid:** Unsquared deviations sum to zero. Variance has squared units; SD has the original units.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.17 — Grouped-data variance: show the procedure

**Use when:** “Explicitly show your steps” for class-based SD.

**Write:**

> I calculate the class midpoints and relative frequencies, then their weighted mean and weighted mean of squares. Subtracting the square of the mean and applying n/(n − 1) gives the approximate sample variance. Taking its square root gives s ≈ [value] [units]. The result is approximate because within-class individual values are unavailable.

**Formula / R:** `x̄ ≈ Σmₖpₖ; mean of squares ≈ Σmₖ²pₖ; s² ≈ n/(n − 1) × (Σmₖ²pₖ − x̄²)`

**Avoid:** Mean of squares is not square of the mean. Reporting only software's final SD does not show the requested steps.

*Sources: F §3; B3 §3.4; P25 Q3.a; P24 Ex.1.e*


### 3.18 — An open-ended class or missing sample size

**Use when:** An exact required ingredient is absent.

**Write:**

> OPEN END: The usual midpoint-based mean and variance cannot be calculated from these classes alone because [class] has no finite known midpoint. A quantile may still be estimated if it lies in another class with known boundaries.
> UNKNOWN n: From proportions alone, I cannot apply the exact sample correction n/(n − 1). Omitting it is a large-n approximation and must be identified as such.
**Avoid:** Do not invent an endpoint or sample size.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.19 — Range and IQR

**Use when:** “What do these dispersion measures mean?”

**Write:**

> RANGE: The observed values extend from [minimum] to [maximum], giving a range of [value]. This measure depends entirely on the extremes.
> IQR: Q₃ − Q₁ = [value]. The central 50% of the observations lie between [Q₁] and [Q₃], so this difference measures their central spread.

**Formula / R:** `Range = max − min; IQR = Q₃ − Q₁`

**Qualification:** The IQR is robust but does not summarise the structure of the tails.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.20 — Upper, lower or no outlier

**Use when:** “Is the value [v] extreme?” Use the named subgroup's quartiles.

**Write:**

> UPPER: The upper fence is Q₃ + 1.5IQR = [U]. Since [v] > [U], the observation is an upper outlier.
> LOWER: The lower fence is Q₁ − 1.5IQR = [L]. Since [v] < [L], the observation is a lower outlier.
> REGULAR: Since [L] ≤ [v] ≤ [U], the observation is not classified as an outlier.
**Avoid:** A value exactly on a fence is not beyond it. Do not use whole-sample quartiles for a subgroup question.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.21 — Whiskers versus fences

**Use when:** “Where do the whiskers end?”

**Write:**

> In the enhanced boxplot, the whiskers end at the smallest and largest observed values that are not outliers. The fences are classification thresholds, not necessarily observed values. Here the whiskers end at [regular minimum] and [regular maximum].
**Qualification:** If only the five-number summary is given and the maximum is an outlier, the exact upper whisker cannot generally be recovered without the other observations.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.22 — A proportion of outliers

**Use when:** “Are more than [q]% of observations outliers?”

**Write:**

> I first determine the relevant outlier fence. With raw data, I calculate the proportion of observations beyond that fence and compare it with [q]%. If using a supplied percentile, I compare its position with the fence and justify the share implied by the cumulative distribution.

**Formula / R:** `R: mean(x > upper_fence) * 100`

**Qualification:** P24 Ex.1.b uses P₉₅ above the upper fence to establish more than 5% upper outliers. Check quantile conventions and ties in other data.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.23 — Coefficient of variation: define and interpret

**Use when:** “Interpret CV” or compare structurally different scales.

**Write:**

> The coefficient of variation is CV = s/|x̄| = [value]. It expresses the standard deviation relative to the absolute mean; here the SD is [100CV]% of the absolute mean. CV is unit-free and is appropriate for comparing relative dispersion across [groups / variables] with different scales.

**Formula / R:** `CV = s/|x̄|`

**Avoid:** SD goes on top. CV is not mean/SD, and a single CV has no universal weak/moderate/strong scale.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.24 — Compare absolute and relative dispersion

**Use when:** Select the sentence supported by the SDs and CVs.

**Write:**

> ABSOLUTE: [A] has the larger SD, [s_A] versus [s_B], so its values are more dispersed in the common measurement units.
> RELATIVE: [B] has the larger CV, [CV_B] versus [CV_A], so its variability is greater relative to its mean.
> EQUAL CV: The relative dispersion is the same even though the means and SDs differ.
**Avoid:** Larger SD does not necessarily mean larger CV. CV is undefined at a zero mean.

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


### 3.25 — Can a five-number summary compare means?

**Use when:** Someone infers the mean from medians, quartiles and extremes.

**Write:**

> The summaries establish that [A] has the higher median and [specified quartiles]. They do not determine the arithmetic mean, which depends on all observed values. Therefore, the mean ordering cannot be concluded from these five-number summaries alone.
**Avoid:** “All five displayed values are higher, therefore the mean must be higher.”

*Sources: F §3; B3 §§3.1–3.5; M1 Exercises 1.1(i), 1.4–1.6; P25 Q1, Q3, Q4; P24 Ex.1*


<a id="section-04"></a>

## Chapter 4 | Joint analysis, association and correlation

Most important words: among, conditional, observed, linear, and not necessarily causal.

**Basis:** F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables


### 4.01 — Joint versus marginal frequency

**Use when:** “Report the joint / marginal distribution.”

**Write:**

> JOINT: The proportion with both X = [a] and Y = [b] is the cell count divided by the total sample size, [f/n].
> MARGINAL X: Adding the cells across row [a] gives the frequency of X = [a], irrespective of Y.
> MARGINAL Y: Adding the cells down column [b] gives the frequency of Y = [b], irrespective of X.

**Formula / R:** `pₖⱼ = fₖⱼ/n; Rₖ = Σⱼpₖⱼ; Cⱼ = Σₖpₖⱼ`

**Avoid:** A row total divided by n is marginal, not joint.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.02 — Conditional frequency and reversed conditioning

**Use when:** “Among / given / within [group]”.

**Write:**

> Y GIVEN X: Among [units with X = a], [percentage]% have Y = [b]. I divide the joint count by the total of the X = [a] group.
> X GIVEN Y: Among [units with Y = b], [percentage]% have X = [a]. Here the denominator is the total of the Y = [b] group.

**Formula / R:** `Freq(Y=b | X=a) = pₐᵦ/Rₐ; Freq(X=a | Y=b) = pₐᵦ/Cᵦ`

**Avoid:** Read the row AND column labels. The same numerator can produce different conditional percentages.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.03 — Compare percentages in differently sized groups

**Use when:** “Report the percentage of [outcome] among [A] and [B].”

**Write:**

> We compare the conditional percentages of [outcome] within the two groups. The percentage is [a]% among [A] and [b]% among [B]. Therefore, [outcome] is relatively more frequent in [higher-percentage group].
**Qualification:** With several target categories, add their frequencies within each conditioning group before comparing.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.04 — More cases but a lower conditional percentage

**Use when:** A conclusion is based on joint counts.

**Write:**

> Although [A] contains more observations with [outcome], its group is larger. The relevant conditional percentages are [a]% for [A] and [b]% for [B]. The statement based on counts is therefore [not justified / contradicted by the relative frequencies].
**Avoid:** Two joint percentages over the whole sample are not substitutes for the requested within-group percentages.

*Sources: F §4; P25 Q5; P24 Ex.1.d*


### 4.05 — Independence holds

**Use when:** All conditional distributions agree, or all joint products agree.

**Write:**

> For every pair of categories, the joint relative frequency equals the product of its corresponding marginals. Equivalently, the conditional distributions coincide with the corresponding marginal distribution. Thus X and Y are independent in the observed distribution.

**Formula / R:** `pₖⱼ = RₖCⱼ for every k,j`

**Avoid:** Equal counts across cells are not required. Checking one matching cell is not generally sufficient.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.06 — Independence fails: two valid demonstrations

**Use when:** At least one correctly matched comparison fails.

**Write:**

> JOINT CHECK: For [cell], pₖⱼ = [joint], whereas RₖCⱼ = [product]. Since these differ, independence fails.
> CONDITIONAL CHECK: The share of [Y=b] among [X=a] is [conditional], whereas its marginal share is [marginal]. Since these differ, X and Y are associated in the observed data.
**Avoid:** Do not multiply two complementary conditional probabilities from the same row. Do not claim a population significance result from this descriptive check.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.07 — Conditional bar charts

**Use when:** “Graphically compare categorical distributions.”

**Write:**

> I plot the conditional distributions of [Y] within the categories of [X], using stacked or side-by-side bars. With stacked conditional bars, every group totals 100%, allowing a relative comparison despite unequal group sizes.
**Qualification:** A joint-count chart answers a different question about numbers of units.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.08 — Compare numerical distributions across groups

**Use when:** “Sketch a suitable graph and give substantive considerations.”

**Write:**

> I use side-by-side boxplots of [Y] conditional on [G], on the same numerical scale. The distributions differ in [shape / location / variability]. [A] has [median and quartiles], compared with [B]; its central 50% is [more / less] dispersed because its IQR is [value]. The graph also shows [supported tail/outlier features].
**Qualification:** State what this means for the actual customers, bids, posts or calls; do not merely list summary numbers.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.09 — Similar medians, different spreads or tails

**Use when:** The centres look similar but the distributions differ elsewhere.

**Write:**

> The medians are similar, but the distributions differ in [IQR / tail length / outliers]. Therefore, similar central values do not imply similar distributions. [A]'s central half is [more/less] dispersed, while [specific tail evidence] distinguishes the extremes.
**Avoid:** A larger IQR does not by itself prove a larger standard deviation.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.10 — Separated quartiles

**Use when:** Q₃(A) < Q₁(B) actually holds.

**Write:**

> The third quartile of [A], [value], is below the first quartile of [B], [value]. Therefore, at least 75% of [A]'s observations are lower than at least 75% of [B]'s observations, showing a marked separation in their locations.
**Avoid:** Do not use this strong sentence merely because the medians differ.

*Sources: F §3–4; B3 §3.3.1; P25 Q1.a*


### 4.11 — Choose the tool for two numerical variables

**Use when:** “Discuss the relationship and its strength; specify tools and why.”

**Write:**

> The fundamental tool is the scatterplot, which reveals direction, departures from linearity, outliers and possible subgroups. I then use Pearson's correlation coefficient to summarise the linear association, provided the scatterplot supports that summary.
**Avoid:** Do not judge the relationship from r alone.

*Sources: F §4; B4 §4.4; P25 Q2; P24 Ex.1.c*


### 4.12 — Covariance: calculation and meaning

**Use when:** “Calculate / interpret covariance.”

**Write:**

> I multiply the deviations of X and Y for each matched observation and add those products, dividing by n − 1 for a sample. The covariance is [value], indicating a [positive / negative] direction of linear association. Its magnitude depends on the measurement units, so it is not a standardised strength measure.

**Formula / R:** `s_xy = Σ(xᵢ−x̄)(yᵢ−ȳ)/(n−1) = (Σxᵢyᵢ − nx̄ȳ)/(n−1); population: σ_xy = Σ(xᵢ−μ_x)(yᵢ−μ_y)/N`

**Avoid:** Sum of paired products is not product of the sums of deviations. Covariance is not automatically squared.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.13 — Pearson correlation: sign and strength

**Use when:** The scatterplot is compatible with a linear summary.

**Write:**

> The correlation coefficient is r = [value]. It indicates a [positive / negative] linear association: higher [X] tends to accompany [higher / lower] [Y]. Its absolute value indicates [supported description] of the concentration around a straight line.

**Formula / R:** `r_xy = s_xy/(s_x s_y); −1 ≤ r_xy ≤ 1`

**Qualification:** At r = ±1 the observed relation is perfectly linear; at zero, no linear association is captured. Avoid pretending weak/moderate/strong labels are universal cutoffs.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.14 — Zero, high or outlier-driven correlation

**Use when:** Select the warning supported by the graph.

**Write:**

> ZERO / SMALL r: A weak linear correlation does not rule out a nonlinear relationship or establish independence.
> HIGH r WITH CURVATURE: Despite its large absolute value, r is not a reliable summary of a single linear tendency because the scatterplot shows curvature.
> OUTLIERS: Extreme observations may inflate or deflate r, so the coefficient must be interpreted together with the graph.
*Sources: F §4; B4 §4.4.4; P25 Q2; P24 Ex.1.c*


### 4.15 — Changing units and reversing axes

**Use when:** “What changes if X is rescaled?”

**Write:**

> A positive change of measurement scale changes the numerical covariance and the regression slope but leaves Pearson's correlation unchanged. The correlation describes standardised linear association rather than the units of measurement. Interchanging X and Y also leaves their correlation unchanged.

**Formula / R:** `cov(a+bX,c+dY) = bd cov(X,Y); r unchanged for b,d > 0`

**Qualification:** The regression of Y on X is not generally obtained by simply reversing the regression of X on Y.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.16 — Introduction to the fitted line

**Use when:** A fitted line is supplied or added to the scatterplot.

**Write:**

> The fitted line ŷ = b₀ + b₁x summarises the observed linear tendency. Its slope is the fitted change in Y associated with a one-unit increase in X; its intercept is the fitted value at X = 0. A steep slope does not by itself imply a strong correlation, and the line does not establish causality.
**Qualification:** This sheet covers the syllabus's introductory line only, not regression-model inference or coefficient derivations.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.17 — Causality and a possible confounder

**Use when:** “Does X cause Y?”

**Write:**

> The observed association does not establish a causal relationship. A possible confounding factor is [Z], because it is related to both [X] and [Y] and may help explain the observed association. Comparisons accounting for [Z] are needed before attributing the pattern to [X].
**Avoid:** Naming a variable related only to Y does not explain why it confounds this comparison.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


### 4.18 — Simpson's paradox and subgroup composition

**Use when:** The aggregate comparison reverses or disappears within groups.

**Write:**

> The aggregate relationship [describe] is [reversed / largely absent] within the [Z] subgroups. This can arise because the compared groups contain different proportions of the subgroups, which also have different outcome distributions. The overall comparison is therefore influenced by subgroup composition rather than providing a direct causal conclusion.
**Qualification:** The aggregate rate is a weighted average using each group's own composition. It is not a fake or arithmetically incorrect rate.

*Sources: F §4; B4 §§4.1–4.4; P25 Q1, Q2, Q5; P24 Ex.1.a–d; G: two variables*


<a id="section-05p"></a>

## Chapter 5A | Probability, conditioning and Bayes

Background probability is included deliberately: it is prerequisite material and a personal revision priority.

**Basis:** F §5; B5 §5.1; G: probability / answer framework


### 5P.01 — Define the random experiment and events

**Use when:** Before calculating an event probability.

**Write:**

> The random experiment is [selecting a visitor / observing an outcome]. The sample space contains all possible outcomes. I define A = [event] and B = [event], so the requested probability is [event notation].
**Avoid:** Do not use the same letter ambiguously for an event and a product name. State whether the selection is from a fixed observed dataset or a population model.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.02 — Classical probability

**Use when:** A finite set of elementary outcomes is equally likely.

**Write:**

> Because the elementary outcomes are equally likely, the probability of [A] is the number of outcomes satisfying A divided by the total number of possible outcomes.

**Formula / R:** `P(A) = favourable outcomes / equally likely possible outcomes`

**Avoid:** Two categories do not automatically have probability 1/2 each.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.03 — Complement or neither event

**Use when:** “Not A” or “Neither A nor B”.

**Write:**

> NOT A: The event is Aᶜ, so its probability is 1 − P(A) = [value].
> NEITHER: The event is the complement of A ∪ B. I first calculate the probability of at least one event, then subtract it from one.

**Formula / R:** `P(Aᶜ)=1−P(A); P(neither)=1−P(A)−P(B)+P(A∩B)`

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.04 — At least one event

**Use when:** “A or B”, allowing both.

**Write:**

> The requested event is A ∪ B. Adding the marginal probabilities counts the intersection twice, so I subtract it once: P(A ∪ B) = [calculation] = [value]. This is the probability that at least one of the two events occurs.

**Formula / R:** `P(A∪B)=P(A)+P(B)−P(A∩B)`

**Avoid:** Do not multiply marginals unless independence is justified.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.05 — A but not B; exactly one

**Use when:** “Only A”, “A excluding B”, or “exactly one”.

**Write:**

> ONLY A: I remove the overlap from A, giving P(A) − P(A ∩ B).
> EXACTLY ONE: I add the two non-overlapping events A without B and B without A. The probability is P(A) + P(B) − 2P(A ∩ B).
**Qualification:** For exactly one, explain the two disjoint branches rather than relying on a memorised expression.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.06 — Conditional probability

**Use when:** “A given B” or “A among cases with B”.

**Write:**

> The reference group is B, not the entire sample space. Therefore, P(A | B) = P(A ∩ B)/P(B) = [value]. Among outcomes satisfying B, the probability of A is [100 × value]%.

**Formula / R:** `P(A|B)=P(A∩B)/P(B), with P(B)>0`

**Avoid:** Do not report the joint probability as the conditional probability.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.07 — Reverse the condition

**Use when:** The question asks P(B|A), but gives P(A|B).

**Write:**

> The given probability concerns A among B cases, whereas the requested probability concerns B among A cases. These have different reference groups. I calculate the joint probability and divide by P(A), rather than simply reversing the labels.

**Formula / R:** `P(B|A)=P(A|B)P(B)/P(A), with P(A)>0`

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.08 — Intersection from conditional information

**Use when:** “Both events” when a conditional probability is given.

**Write:**

> Using the multiplication rule, the probability that both events occur is the probability of the conditioning event multiplied by the conditional probability of the other event. Thus P(A ∩ B) = P(A | B)P(B) = [value].
**Avoid:** Independence is not needed for this conditional multiplication rule.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.09 — Independence: observed versus hypothetical

**Use when:** “Are they independent?” or “What if they were independent?”

**Write:**

> CHECK: I compare the supplied P(A ∩ B) with P(A)P(B). Since the quantities are [equal / different], the events are [independent / not independent].
> HYPOTHETICAL: Under independence, with the same marginals, the intersection would be P(A)P(B) = [value], which need not equal the supplied actual intersection.
**Avoid:** Do not reuse the actual overlap when the question explicitly changes the assumption to independence.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.10 — Conditional check of independence

**Use when:** A conditional probability and a marginal are available.

**Write:**

> Knowing B [does / does not] change the probability of A: P(A | B) = [value], compared with P(A) = [value]. Therefore the two events are [dependent / independent].
**Qualification:** The conditioning event must have positive probability. For entire random variables, the analogous equality must hold across all categories.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.11 — Mutually exclusive versus independent

**Use when:** The question asks whether both properties hold.

**Write:**

> Mutually exclusive events cannot occur together, so their intersection is empty. If both have positive probabilities, the product P(A)P(B) is positive while P(A ∩ B)=0, so they are not independent.
**Avoid:** Independence means unchanged probabilities, not inability to occur together.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.12 — Total probability

**Use when:** Group shares and within-group outcome probabilities are given.

**Write:**

> The events E₁,…,Eₖ form a partition: they are mutually exclusive and exhaustive. I obtain the overall probability of B by weighting its probability within each group by that group's probability and summing the contributions.

**Formula / R:** `P(B)=ΣⱼP(B|Eⱼ)P(Eⱼ)`

**Avoid:** Do not average conversion rates without the traffic-share weights. The groups must cover all possibilities without overlap.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.13 — Bayes: which source given the outcome?

**Use when:** “Given a purchase, which channel did the visitor use?”

**Write:**

> We seek P(Eᵢ | B), the probability of [source] given [observed outcome]. Bayes' theorem divides the joint probability of that source and outcome by the total probability of the outcome. The result is [value], so among [outcome cases], [percentage]% belong to [source].

**Formula / R:** `P(Eᵢ|B)=P(B|Eᵢ)P(Eᵢ) / ΣⱼP(B|Eⱼ)P(Eⱼ)`

**Avoid:** A channel's conversion rate and its share of all purchasers have different denominators.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.14 — Bayes with no outcome / a complement

**Use when:** “Which source, given no purchase?”

**Write:**

> The conditioning event is Bᶜ. Within group Eᵢ, the probability of no outcome is 1 − P(B | Eᵢ). The required posterior probability is [group share × within-group failure probability] divided by the total probability of failure.

**Formula / R:** `P(Eᵢ|Bᶜ)=P(Eᵢ)[1−P(B|Eᵢ)]/[1−P(B)]`

**Avoid:** Do not keep the purchase denominator when conditioning on non-purchase.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.15 — With versus without replacement

**Use when:** Sequential draws are described.

**Write:**

> WITHOUT REPLACEMENT: The second draw's probabilities depend on the first outcome, so I update the available counts and use the conditional multiplication rule.
> WITH REPLACEMENT AND INDEPENDENT DRAWS: Replacing the unit leaves the distribution unchanged, and independence allows the corresponding probabilities to be multiplied.
**Avoid:** The draw probabilities do not remain unchanged merely because the sample is called random.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


### 5P.16 — A positive diagnostic result

**Use when:** A mathematical test example gives prevalence and conditional test accuracy.

**Write:**

> Sensitivity is P(positive | condition), not P(condition | positive). The requested probability must account for both true and false positive outcomes and the prevalence of the condition. Bayes' theorem gives [posterior], which may be small when the condition is rare.
**Qualification:** This is the textbook's probability-model reasoning, not medical guidance or a claim about a real diagnostic test.

*Sources: F §5; B5 §5.1; G: probability / answer framework*


<a id="section-05r"></a>

## Chapter 5B | Random variables and transformations

Describe a probability model, not an already observed frequency distribution.

**Basis:** F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables


### 5R.01 — Define a random variable and its support

**Use when:** “Define X and specify its possible values.”

**Write:**

> X is the [numerical outcome] associated with one [randomly selected unit / repetition]. Its support is [set or interval]. It is [discrete, because the possible values are finite or countable / continuous, because the model describes values over a continuum].
**Avoid:** The sample size is not automatically the number of support values.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.02 — Valid probability function

**Use when:** “Is this a probability distribution?”

**Write:**

> VALID: All probabilities are non-negative and their sum is one, so the table defines a valid discrete probability function.
> INVALID: The table is not valid because [negative probability / total different from one].

**Formula / R:** `p_X(x)=P(X=x); p_X(x)≥0; Σₓp_X(x)=1`

**Qualification:** If a missing probability is requested, obtain it from the requirement that the total equals one.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.03 — CDF at a non-support value

**Use when:** “Find F_X(a)” when a is between possible values.

**Write:**

> F_X(a) is P(X ≤ a). I add only the probabilities of support values not exceeding a. Between consecutive support values, the CDF stays constant; I do not jump forward to include the next larger outcome.
**Avoid:** Your earlier F(40) calculation incorrectly included the €50 outcome.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.04 — Discrete versus continuous endpoints

**Use when:** “Does > versus ≥ matter?”

**Write:**

> DISCRETE: P(X ≥ a) = P(X > a) + P(X = a), so the endpoint may change the probability.
> CONTINUOUS: P(X = a)=0, so including or excluding one endpoint does not change the probability under the continuous model.
**Qualification:** The continuous density height at a is not the probability P(X=a).

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.05 — Expected value

**Use when:** “Calculate and interpret E(X).”

**Write:**

> The expected value is the probability-weighted mean of the possible outcomes: E(X) = [calculation] = [value] [units]. It describes the average outcome over many comparable repetitions, not the outcome guaranteed for one individual.

**Formula / R:** `E(X)=μ_X=Σₓx p_X(x)`

**Avoid:** E(X) need not belong to the support. Do not simply average the possible values.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.06 — Variance and standard deviation of a random variable

**Use when:** A full probability distribution is supplied.

**Write:**

> I first calculate E(X²), then subtract [E(X)]² to obtain the variance. The standard deviation is the square root of that variance. It describes dispersion of the possible [outcomes] around their expected value.

**Formula / R:** `E(X²)=Σₓx²p_X(x); Var(X)=E(X²)−[E(X)]²; SD(X)=√Var(X)`

**Avoid:** There is no sample n−1 correction when evaluating a specified probability distribution.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.07 — Bernoulli indicator

**Use when:** One success/failure outcome is coded 1/0.

**Write:**

> The indicator X follows a Bernoulli distribution with success probability p. Since X equals one on success and zero otherwise, E(X)=p and Var(X)=p(1−p). The expected value is the probability of success, not a fractional outcome for an individual.

**Formula / R:** `E(X)=1·p+0·(1−p)=p; E(X²)=p; Var(X)=p−p²`

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.08 — Joint, marginal and conditional probabilities

**Use when:** A joint probability table is given.

**Write:**

> A joint cell gives the probability of a pair of outcomes. Summing over the other variable gives a marginal probability. For a conditional probability, I divide the relevant joint probability by the marginal probability of the conditioning event.

**Formula / R:** `p_X(x)=Σᵧp_XY(x,y); p_Y(y)=Σₓp_XY(x,y); p_X|Y(x|y)=p_XY(x,y)/p_Y(y)`

**Avoid:** A joint probability table is not already a table of within-row percentages.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.09 — Expected product and covariance

**Use when:** “Calculate E(XY), covariance or correlation.”

**Write:**

> I calculate E(XY) by weighting each product xy by its joint probability. Covariance is E(XY) − E(X)E(Y). Dividing covariance by the two standard deviations gives the correlation coefficient.

**Formula / R:** `E(XY)=ΣₓΣᵧxy p_XY(x,y); σ_XY=E(XY)−μ_Xμ_Y; ρ_XY=σ_XY/(σ_Xσ_Y)`

**Avoid:** E(XY)=E(X)E(Y) is guaranteed under independence, not for every joint distribution.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.10 — Obtain covariance from correlation

**Use when:** Correlation and two SDs are given.

**Write:**

> Rearranging the definition of correlation gives Cov(X,Y)=ρ_XY σ_X σ_Y. This converts the standardised relationship into the covariance required in the linear-combination variance formula.

**Formula / R:** `Cov(X,Y)=ρ_XY σ_X σ_Y`

**Avoid:** Use the SDs, not the variances, in this product.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.11 — Linear transformation

**Use when:** Y = a + bX.

**Write:**

> The expected value changes to a + bE(X). Adding a fixed constant shifts all outcomes equally and does not change dispersion. Multiplying by b multiplies the variance by b² and the standard deviation by |b|.

**Formula / R:** `E(Y)=a+bμ_X; Var(Y)=b²σ_X²; SD(Y)=|b|σ_X`

**Avoid:** Do not multiply variance by b or by |b|. The absolute value belongs to SD.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.12 — Proof of the transformation formulas

**Use when:** “Show / derive the expected value and variance.”

**Write:**

> Using the probability weights, E(a+bX)=Σₓ(a+bx)p_X(x)=aΣₓp_X(x)+bΣₓxp_X(x)=a+bμ_X. Also, Y−E(Y)=b(X−μ_X), so Var(Y)=E[b²(X−μ_X)²]=b²Var(X). The continuous case uses integrals in place of sums.
**Qualification:** This provides the actual derivation rather than citing linearity as its own proof.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.13 — A linear combination with dependence

**Use when:** T = aX + bY; covariance or correlation is known.

**Write:**

> Linearity of expectation does not require independence, so E(T)=aμ_X+bμ_Y. For the variance, the covariance term must be included: Var(T)=a²σ_X²+b²σ_Y²+2abσ_XY. Taking the square root gives the SD in the units of T.

**Formula / R:** `Var(aX+bY)=a²σ_X²+b²σ_Y²+2abσ_XY`

**Avoid:** Do not add SDs. Preserve the signs of a and b in the covariance term.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.14 — Independent sum or difference

**Use when:** Independence is explicitly justified.

**Write:**

> Because the variables are independent, their covariance is zero. Their variances add for both X+Y and X−Y, since the coefficient of Y is squared. Expected values retain the plus or minus sign.

**Formula / R:** `E(X−Y)=μ_X−μ_Y; Var(X−Y)=σ_X²+σ_Y² under independence`

**Avoid:** Do not subtract the variance of Y merely because the outcome is X−Y.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.15 — Can the combination's probability be calculated?

**Use when:** Only moments are supplied, or joint Normality is specified.

**Write:**

> MOMENTS ONLY: The information determines the mean and variance but not the requested probability without a distributional assumption.
> JOINT NORMALITY: Since X and Y are jointly Normal, the linear combination is Normal with the calculated mean and variance. Independent Normal variables also satisfy this condition.
**Avoid:** Two Normal marginal distributions alone do not guarantee joint Normality.

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


### 5R.16 — Standardisation versus Normality

**Use when:** “Does standardising make the variable Normal?”

**Write:**

> Standardisation gives Z=(X−μ_X)/σ_X, with mean zero and variance one when σ_X is finite and positive. It does not change a non-Normal distribution into a Normal one. Z is standard Normal when X is Normal.

**Formula / R:** `E(Z)=0; Var(Z)=1; X Normal ⇒ Z ~ N(0,1)`

*Sources: F §5; B5 §§5.2–5.3 excluding the syllabus's excluded distributions and §5.3.4; G: random variables*


<a id="section-05n"></a>

## Chapter 5C | Normal probabilities and quantiles

The threshold, tail and standard deviation must match the random variable in the question.

**Basis:** F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT


### 5N.01 — Read the Normal parameters

**Use when:** X ~ N(μ,σ²) is given.

**Write:**

> The variable follows a Normal distribution with mean [μ] and variance [σ²]. Its standard deviation is √[σ²] = [σ]. I use the standard deviation, not the variance, in the R argument sd.

**Formula / R:** `Z=(X−μ)/σ ~ N(0,1)`

**Avoid:** N(80,100) means SD 10, not 100. If 1.2 is explicitly an SD, do not take its square root again.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.02 — Lower tail

**Use when:** “Below”, “at most”, or “does not exceed a”.

**Write:**

> The requested probability is P(X ≤ a). Standardising gives P(Z ≤ (a−μ)/σ), evaluated with the Normal cumulative distribution. Under the model, [percentage]% of [individual outcomes] do not exceed [a].

**Formula / R:** `R: pnorm(a, mean=mu, sd=sigma)`

**Avoid:** The first argument is a numerical threshold, not a logical expression such as X <= a.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.03 — Upper tail

**Use when:** “Above”, “exceeds”, or “greater than a”.

**Write:**

> The requested probability is P(X > a). Since the cumulative distribution gives the probability to the left, I subtract it from one. Under the model, [percentage]% of [outcomes] exceed [a].

**Formula / R:** `R: 1 - pnorm(a, mean=mu, sd=sigma)`

**Avoid:** A z-score such as 2 is a threshold, not a probability.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.04 — Between or outside two values

**Use when:** “Between a and b” or “outside that range”.

**Write:**

> BETWEEN: I subtract the cumulative probability at a from the cumulative probability at b.
> OUTSIDE: I add the probability below a and above b, or subtract the between-probability from one. The result is [value] under the stated Normal model.

**Formula / R:** `Between: pnorm(b,mu,sigma)-pnorm(a,mu,sigma)`

**Avoid:** For the Normal model, whether endpoints are included does not affect the answer.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.05 — Value exceeded by the upper α share

**Use when:** “Only [100α]% exceed this value.”

**Write:**

> We seek c with P(X>c)=α, equivalently P(X≤c)=1−α. Thus c=μ+z_α σ. Under this model, [100α]% of [units] have X above [c].

**Formula / R:** `z_α=qnorm(1−α); R: qnorm(1-alpha, mean=mu, sd=sigma)`

**Avoid:** The book's z subscript is a RIGHT-tail probability; R's default qnorm argument is a LEFT-tail probability.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.06 — Value exceeded with high probability

**Use when:** “Total exceeds c with probability 90%.”

**Write:**

> We seek c such that P(X>c)=0.90, so P(X≤c)=0.10. The required value is the 10th percentile, not the 90th percentile.

**Formula / R:** `R: qnorm(0.10, mean=mu, sd=sigma)`

**Qualification:** The wording is about probability above the threshold, not the percentage written after qnorm.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.07 — Central probability interval

**Use when:** “Find the central 100(1−α)% of individual outcomes.”

**Write:**

> I leave α/2 in each tail and use the lower α/2 quantile and upper 1−α/2 quantile. Under the Normal model, a randomly selected [individual outcome] lies between these endpoints with probability 1−α.

**Formula / R:** `[μ − z_(α/2)σ, μ + z_(α/2)σ]; R: qnorm(c(alpha/2,1-alpha/2),mu,sigma)`

**Avoid:** This is an interval for individual outcomes, not a confidence interval for an unknown mean.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.08 — pnorm versus qnorm

**Use when:** Explain or select the R operation.

**Write:**

> pnorm takes a threshold and returns the cumulative probability below it. qnorm takes a cumulative probability and returns its threshold. By default both refer to the lower tail.

**Formula / R:** `pnorm(value,mu,sigma) → probability; qnorm(probability,mu,sigma) → value`

**Avoid:** 0.05 is 5%; 0.5 is 50%; 10 is not a valid probability input.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


### 5N.09 — Probability cannot be inferred from μ and σ

**Use when:** The distribution of one generic observation is unspecified.

**Write:**

> The mean and variance describe the centre and spread but do not determine the entire probability distribution. Therefore the requested probability for one individual observation cannot be calculated from these moments alone.
**Qualification:** A large numerical value of expected sales is not a sample size that automatically permits the CLT.

*Sources: F §5; B5 §§5.2.4–5.2.5; G: Normal distribution and CLT*


<a id="section-05s"></a>

## Chapter 5D | Sampling distributions and the CLT

Always name the object: one observation, all observations individually, a sum, an average, or a proportion.

**Basis:** F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT


### 5S.01 — Independent and identically distributed

**Use when:** “State the sampling assumptions.”

**Write:**

> I treat X₁,…,Xₙ as independent observations from the same population distribution, with finite mean μ and variance σ². Identically distributed means the same probability model, not equal realised values. Independence is a separate condition.
**Qualification:** Samples may be i.i.d. within different subgroups without following the same distribution across those subgroups.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.02 — Expected value and SD of a total

**Use when:** “Total spending / total demand from n units.”

**Write:**

> The total is S=ΣXᵢ. Its expected value is nμ, and independence makes its variance the sum of the individual variances, nσ². Hence SD(S)=σ√n. Across possible samples of [n] units, total [X] varies around [nμ] with SD [σ√n].

**Formula / R:** `E(S)=nμ; Var(S)=nσ²; SD(S)=σ√n`

**Avoid:** SD of the sum is not nσ. No Normality assumption is needed for these moment calculations.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.03 — Expected value and SD of a mean

**Use when:** “Average spending / mean outcome for n units.”

**Write:**

> The sample mean is X̄=S/n, so E(X̄)=μ and Var(X̄)=σ²/n. Its standard deviation is σ/√n. Across possible samples of [n] units, their means fluctuate around the population mean with SD [value].

**Formula / R:** `E(X̄)=μ; Var(X̄)=σ²/n; SD(X̄)=σ/√n`

**Avoid:** Divide the variance by n and the SD by √n. These formulas do not require Normality.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.04 — Exact Normal sampling distribution

**Use when:** The population is Normal and observations are independent.

**Write:**

> Since the population is Normal and the observations are independent, the sample mean is exactly Normal with mean μ and variance σ²/n. The total is exactly Normal with mean nμ and variance nσ². This holds even for a small sample.

**Formula / R:** `X̄ ~ N(μ,σ²/n); S ~ N(nμ,nσ²)`

**Avoid:** n < 30 does not prevent the exact Normal calculation when population Normality is given.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.05 — Use the CLT

**Use when:** The population is not assumed Normal, but n is sufficiently large.

**Write:**

> Assuming i.i.d. observations with finite mean and variance, the Central Limit Theorem allows the sampling distribution of [the mean / the total] to be approximated by a Normal distribution for this sufficiently large sample. I use its own mean and variance, not the individual variable's spread.

**Formula / R:** `X̄ approximately N(μ,σ²/n); S approximately N(nμ,nσ²)`

**Qualification:** The book's typical n > 30 guideline depends on population shape; it is not a universal guarantee. Finite sample variance is not proof of finite population variance.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.06 — Interpret a probability about an average

**Use when:** “Calculate P(X̄>a) and interpret.”

**Write:**

> Using the [exact Normal model / CLT approximation], P(X̄>a) = [probability]. Thus, in approximately [percentage]% of all possible random samples of [n] [units], the sample average [X] exceeds [a].

**Formula / R:** `R: 1 - pnorm(a, mean=mu, sd=sigma/sqrt(n))`

**Avoid:** Do not say [percentage]% of individual customers exceed a. Complete the probability calculation after identifying the distribution.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.07 — Interpret a probability or quantile of the total

**Use when:** Demand or revenue is summed across n units.

**Write:**

> PROBABILITY: The probability that total [X] for [n] units exceeds [a] is [value].
> QUANTILE: The threshold [c] covers the total demand with probability [q] under the [exact / approximate] model.

**Formula / R:** `P(S>a): 1-pnorm(a,n*mu,sigma*sqrt(n)); q-quantile: qnorm(q,n*mu,sigma*sqrt(n))`

**Avoid:** If converting a total to an average, divide the threshold by n as well.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.08 — All individuals versus their average

**Use when:** “Each / all n exceed a” is not “their mean exceeds a”.

**Write:**

> ALL INDIVIDUALS: Independence gives P(X₁>a,…,Xₙ>a)=[P(X>a)]ⁿ.
> AVERAGE: The event X̄>a concerns the group's mean; individual values below a can be offset by others above a. It requires the sampling distribution of the mean.
**Avoid:** Do not use a power of a tail probability for a sample-mean question.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.09 — No distribution justified for a small sample

**Use when:** Non-Normal/unspecified population and insufficient sample size.

**Write:**

> The sample is not large enough to invoke the course's Normal approximation automatically, and a Normal population distribution has not been specified. The mean and variance can be calculated, but the requested Normal probability or quantile is not justified from the supplied information.
**Avoid:** Do not assume a Normal population just because μ and σ are supplied.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.10 — Sample proportion with known p

**Use when:** A population success probability is given; predict sample behaviour.

**Write:**

> The sample proportion is the mean of i.i.d. Bernoulli indicators. It has expected value p and variance p(1−p)/n. For a sufficiently large sample, its Normal approximation uses SD √[p(1−p)/n]. The resulting probability concerns proportions across samples, not a single customer's outcome.

**Formula / R:** `P̂ approximately N(p,p(1−p)/n); R: 1-pnorm(a,p,sqrt(p*(1-p)/n))`

**Qualification:** The book discusses several practical large-count checks, including sufficiently large np and n(1−p). Convergence is poorer near p=0 or 1.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


### 5S.11 — What larger n does and does not do

**Use when:** “Does a larger sample improve precision or change the population?”

**Write:**

> For the same population, increasing n reduces the SD of the sample mean or proportion and concentrates its sampling distribution around the parameter. It does not make the individual observations less variable or change a skewed population into a Normal one, and it does not guarantee that every particular larger-sample estimate is closer to the parameter.
**Avoid:** Distinguish repeated-sampling behaviour from one realised sample.

*Sources: F §5; B5 §5.4; P24 Ex.1.f; G: Normal distribution and CLT*


<a id="section-06e"></a>

## Chapter 6A | Point estimation and standard errors

Parameter, estimator and estimate are different objects. This is where your wording matters most.

**Basis:** F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors


### 6E.01 — Parameter, estimator and estimate

**Use when:** “Identify the parameter / estimator / observed estimate.”

**Write:**

> The parameter is [μ / p], the fixed but unknown population [mean / proportion] of [context]. The estimator is [X̄ / P̂], a random function of the sample. Its observed realisation is [x̄ / p̂] = [value], which is the point estimate.
**Avoid:** Do not write point estimate = expected value of estimator. E(X̄)=μ, whereas x̄ is the observed number.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.02 — Mean: point estimate and unbiasedness

**Use when:** “Estimate the mean and state a property.”

**Write:**

> I estimate the population mean [context] using the sample mean X̄. It is unbiased because E(X̄)=μ. From the observed sample, the point estimate is x̄=[value] [units].

**Formula / R:** `X̄=ΣXᵢ/n; x̄=Σxᵢ/n`

**Avoid:** Unbiasedness does not mean this particular estimate equals or is necessarily close to μ.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.03 — Proportion: point estimate and unbiasedness

**Use when:** “Estimate the population proportion.”

**Write:**

> Define Xᵢ=1 for [success] and Xᵢ=0 otherwise. The sample proportion P̂=ΣXᵢ/n estimates p. Since E(Xᵢ)=p, E(P̂)=np/n=p, so the estimator is unbiased. The observed estimate is p̂=[success count]/[n]=[value].
**Avoid:** The estimator divides sampled successes by sample size, not by all population units.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.04 — Define unbiasedness and bias

**Use when:** A general estimator's quality is discussed.

**Write:**

> The estimator is unbiased if its expected value equals the parameter for every relevant parameter value and sample size. Its bias is E(θ̂ₙ)−θ. A [positive / negative] bias means systematic [overestimation / underestimation] across samples.

**Formula / R:** `Bias(θ̂ₙ)=E(θ̂ₙ)−θ`

**Avoid:** A property of an estimator is not a claim that every realised estimate has that error.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.05 — Sample variance and asymptotic unbiasedness

**Use when:** “Why n−1?” or compare variance estimators.

**Write:**

> The sample variance with denominator n−1 is unbiased for σ². Using denominator n instead gives expected value ((n−1)/n)σ², so it underestimates on average. Its bias tends to zero as n increases, making that unadjusted estimator asymptotically unbiased.

**Formula / R:** `E(S²)=σ²; E(S̃²)=((n−1)/n)σ²`

**Avoid:** Do not infer that any particular adjusted sample variance must exceed the true population variance.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.06 — SE of a mean, σ known

**Use when:** “Calculate and interpret the standard error.”

**Write:**

> Since the population SD σ is known, SE(X̄)=σ/√n=[value] [units]. This is the standard deviation of the sample-mean estimator across all possible random samples of size [n]. In the book's wording, it describes the expected deviation of a generic estimate from the population mean.
**Avoid:** Do not describe it as the observed error |x̄−μ|.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.07 — SE of a mean, σ unknown

**Use when:** The exercise supplies sample SD s, not population SD σ.

**Write:**

> The exact standard error cannot be evaluated because σ is unknown. I estimate it using the sample SD: estimated SE(X̄)=s/√n=[value] [units]. This estimates the variability of generic sample means around the population mean across random samples of size [n].

**Formula / R:** `Estimated SE(X̄)=s/√n`

**Avoid:** s itself measures individual-observation dispersion; it is not the standard error.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.08 — Normality is not needed merely to calculate an SE

**Use when:** “Are distributional assumptions necessary?” for point estimation or SE.

**Write:**

> The sample mean's expected value and variance follow from the i.i.d. assumptions and finite population moments, regardless of population shape. Therefore, Normality is not required merely to calculate the point estimate and the standard error. It becomes relevant when using an exact Normal or Student's t interval or calculating sampling probabilities.
**Avoid:** Do not impose n>30 simply to calculate σ/√n or s/√n.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.09 — SE of a proportion: known p versus estimated p

**Use when:** Distinguish probability prediction from estimation.

**Write:**

> KNOWN p: The standard error is √[p(1−p)/n].
> UNKNOWN p: Since p is unknown, I estimate the standard error as √[p̂(1−p̂)/n]=[value]. This describes the estimated dispersion of generic sample proportion estimates, not the actual error of this sample's p̂.

**Formula / R:** `Estimated variance = p̂(1−p̂)/n; estimated SE = √[p̂(1−p̂)/n]`

**Avoid:** Take the square root. A value such as 0.0163 represents 1.63 percentage points, not 0.0163 percentage points.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.10 — An SE is claimed to be the actual error

**Use when:** “Our estimate differs from the parameter by SE.”

**Write:**

> The statement is incorrect. The standard error describes the sampling variability of the estimator across possible samples; it does not give the actual distance between the particular observed estimate and the unknown parameter. This specific estimate may be closer to or farther from the parameter.
**Avoid:** Do not replace the unsupported claim with 'the actual error is probably exactly SE'.

*Sources: F §6; B6 Theory & Examples 6.1; P25 Q6.b*


### 6E.11 — Which estimate is more reliable?

**Use when:** The question refers to particular realised estimates.

**Write:**

> We cannot determine which particular observed estimate is closer to its population parameter because those parameters are unknown. We can compare the estimators: the one with the smaller [estimated] standard error produces more tightly concentrated estimates across repeated samples.
**Avoid:** Answer the object asked for: estimator precision is not the accuracy of a realised estimate.

*Sources: F §6; B6 §6.2; P25 Q6.b*


### 6E.12 — Larger sample, different observed mean

**Use when:** “Is the new estimate necessarily better?”

**Write:**

> The larger sample gives an estimator with a smaller standard error, for the same population variability. However, it does not guarantee that the new realised estimate is closer to the true parameter than the old estimate. That comparison cannot be made without knowing the parameter.
**Qualification:** This is the secret-sale example's larger-sample-versus-smaller-sample distinction.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.13 — Sample size for a target SE

**Use when:** “Standard error at most e”, not a margin of error.

**Write:**

> I require σ/√n ≤ [e]. Solving gives n ≥ (σ/e)², so the minimum integer sample size is [ceiling result]. Under the stated known population SD, this achieves the specified theoretical standard error.

**Formula / R:** `n_min=ceiling((sigma/target_SE)^2)`

**Avoid:** No confidence-level quantile belongs in a pure target-SE calculation.

*Sources: F §6; B6 §§6.1–6.2, 6.5; P25 Q6.a–b; G: point estimation and standard errors*


### 6E.14 — Missing observations and the target population

**Use when:** R reports n and missing values separately.

**Write:**

> The estimation uses [n] valid observations, so that is the sample size in the standard error. The estimate refers to [population actually represented by these observations]. Excluding missing values does not by itself justify extending the conclusion to a different group such as abandoned calls.
**Avoid:** Do not use the dataframe's total row count when fewer complete observations were analysed.

*Sources: F §6; B6 Example 6.1 (call-centre application)*


<a id="section-06i"></a>

## Chapter 6B | Confidence intervals: meaning and method

Select the assumptions first, then the interval formula and the appropriate interpretation.

**Basis:** F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a


### 6I.01 — Interpret an observed mean interval

**Use when:** “Interpret the [C]% CI in context.”

**Write:**

> We are [C]% confident that the population mean [variable, units and population] lies between [L] and [U].
**Avoid:** Do not replace population mean with individual observations or with the already-known sample mean.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.02 — Interpret an observed proportion interval

**Use when:** The parameter is a proportion.

**Write:**

> We are [C]% confident that the population proportion of [units] who [success condition] lies between [100L]% and [100U]%.
**Qualification:** Specify the eligible population and the success category. Keep proportions and percentages on a consistent scale.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.03 — What the confidence level means

**Use when:** “Explain 95% confidence.”

**Write:**

> Across repeated random samples of the same size, approximately 95% of intervals constructed using this procedure would contain the true population parameter. The parameter is fixed; the interval endpoints vary with the sample.
**Qualification:** For an exact method, the coverage probability is 0.95. For a Normal approximation, the nominal 95% coverage is approximate.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.04 — Reject the 95%-probability interpretation

**Use when:** “There is a 95% probability that μ is in this observed interval.”

**Write:**

> In the book's confidence-interval framework, the population parameter is fixed and the observed interval is also fixed. That interval either contains the parameter or does not. The 95% probability of coverage refers to the random interval procedure across possible samples, not to a probability assigned to the parameter inside these observed endpoints.
**Avoid:** Do not generalise this into 'probability can only discuss future events'.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.05 — Does the CI contain 95% of observations?

**Use when:** A CI for a mean is confused with a range for individuals.

**Write:**

> No. This confidence interval concerns the unknown population mean, not the distribution of individual observations. A probability interval for individual values uses the individual variable's distribution and spread, not the standard error of the sample mean.
**Avoid:** Neither '95% of customers lie in this CI' nor '95% of possible sample means lie in this observed CI' is its confidence interpretation.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.06 — Point estimate, ME and interval width

**Use when:** The interval is symmetric around its point estimate.

**Write:**

> The point estimate is the midpoint (L+U)/2. The margin of error is half the width, (U−L)/2. The total width is U−L, which equals twice the margin of error.

**Formula / R:** `Centre=(L+U)/2; ME=(U−L)/2; width=2ME`

**Avoid:** These midpoint statements apply to the symmetric intervals being used here, not arbitrary interval procedures.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.07 — Mean, known σ, exactly Normal population

**Use when:** One i.i.d. sample; σ known; Normal population assumption.

**Write:**

> Since the population is Normal and σ is known, (X̄−μ)/(σ/√n) follows the standard Normal distribution exactly. The [C]% interval is x̄ ± z_(α/2)σ/√n = [L,U]. This procedure has exact nominal coverage under the assumptions.

**Formula / R:** `z_(α/2)=qnorm(1−α/2); ME=z_(α/2)σ/√n`

**Avoid:** A small n does not invalidate this exact Normal-population result.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.08 — Mean, known σ, large sample without Normality

**Use when:** A sufficiently large i.i.d. sample with finite population variance.

**Write:**

> The CLT justifies an approximately Normal sampling distribution for X̄. With known σ, I use x̄ ± z_(α/2)σ/√n. The resulting interval has approximately the stated confidence level; individual observations need not be Normal.

**Formula / R:** `R: xbar + c(-1,1)*qnorm(1-alpha/2)*sigma/sqrt(n)`

**Avoid:** Do not claim exact coverage merely because n exceeds a guideline.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.09 — Mean, unknown σ, Normal population

**Use when:** Use Student's t.

**Write:**

> Because σ is unknown, I estimate the standard error by s/√n. Under the Normal population assumption, the studentised sample mean follows Student's t with n−1 degrees of freedom. The interval is x̄ ± t_(n−1,α/2)s/√n = [L,U].

**Formula / R:** `Reliability factor=qt(1-alpha/2,df=n-1)`

**Avoid:** It is the studentised statistic, not X̄ itself, that follows Student's t.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.10 — The exercise says approximately Normal

**Use when:** A small-sample t or z interval is proposed under approximate Normality.

**Write:**

> The stated approximate Normality supports using the [Normal / Student's t] procedure. Its exact distributional result would require exact population Normality, so the interval should be regarded as approximately valid under the given assumption rather than guaranteed to have exact coverage.
**Qualification:** Do not ignore the word approximately when explicitly asked about exact coverage.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.11 — Mean, unknown σ, large non-Normal sample

**Use when:** Normal approximation is available; t is the book's wider alternative.

**Write:**

> For a sufficiently large i.i.d. sample with finite population variance, the CLT and the consistency of the sample variance justify using s/√n with a Normal reliability factor. A t reliability factor may also be adopted as the book's conservative, slightly wider choice. Its exact t distribution, however, requires a Normal population.
**Avoid:** Wider does not establish guaranteed nominal coverage for every non-Normal finite sample.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.12 — Mean, small sample, no Normality assumption

**Use when:** “Can we construct the interval with the available information?”

**Write:**

> The sample is small, the population distribution is unspecified, and population Normality has not been assumed. Therefore, the course's exact [Normal / Student's t] procedure and its large-sample approximation are not automatically justified. Additional distributional assumptions would be needed.
**Avoid:** Do not say no confidence interval of any kind is mathematically possible; state the limitation of the course method.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.13 — Why t is wider than z in this comparison

**Use when:** Same sample size, confidence level and numerical standard-error term.

**Write:**

> When σ is estimated, the denominator of the studentised statistic varies across samples. Student's t accounts for this additional uncertainty through heavier tails and a larger reliability factor. With the same numerical standard error, this yields a larger margin of error and a wider interval than the Normal factor.
**Qualification:** If s and the assumed known σ differ numerically, compare both the reliability factors and the SEs; the interval-width ordering is not automatic.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.14 — Degrees of freedom and convergence to Normal

**Use when:** “How many df?” or “Why do the methods become similar?”

**Write:**

> The one-sample t procedure uses n−1 degrees of freedom. As the degrees of freedom increase, the t distribution approaches the standard Normal distribution, so the reliability factors and interval widths become increasingly similar.

**Formula / R:** `95%: qt(0.975,n−1); 99%: qt(0.995,n−1)`

**Avoid:** Use the lower-tail probability 1−α/2, not α/2, for the positive upper reliability factor.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.15 — Two UBStats rows

**Use when:** Output contains Normal.Approx and Student-t.

**Write:**

> Both rows use the same point estimate and estimated standard error. Normal.Approx uses a z reliability factor; Student-t uses t with n−1 degrees of freedom and is slightly wider. With a Normal population, the t result is exact. Without that assumption but with large n, the Normal approximation is justified and the book permits t as a conservative alternative.
**Qualification:** State the row selected and justify it before interpreting its actual endpoints. The printed t row is not evidence that the population is Normal.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.16 — Proportion: choose the estimator and check approximation

**Use when:** An i.i.d. success/failure sample is available.

**Write:**

> The observed sample proportion is p̂=[successes]/n. For the large-sample Normal interval, I check that there are sufficiently many successes and failures. Here they are [n p̂] and [n(1−p̂)], supporting [or not supporting] the approximation.
**Qualification:** The book discusses multiple practical count thresholds (e.g. above 10 or 15). Name the criterion being used; these are guidelines, not a theorem that every n>30 proportion works.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.17 — Construct the proportion interval

**Use when:** The Normal approximation is appropriate.

**Write:**

> The estimated SE is √[p̂(1−p̂)/n]=[SE]. The reliability factor is z_(α/2)=[z], giving ME=[z×SE]. Therefore the approximate [C]% confidence interval for the population proportion is [p̂−ME,p̂+ME]=[L,U].

**Formula / R:** `R: phat + c(-1,1)*qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)`

**Avoid:** Use a z factor, not Student's t. In a CI the unknown p in the SE is replaced by p̂.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.18 — Raise confidence with the same data

**Use when:** 95% versus 99%, for example.

**Write:**

> Holding the sample data and method fixed, a higher confidence level requires a larger reliability factor. The margin of error and interval width therefore increase. Higher confidence is obtained at the expense of precision.

**Formula / R:** `Width=2 × reliability factor × (estimated) SE`

**Avoid:** A wider interval does not make the point estimate more accurate.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.19 — Increase sample size at fixed confidence

**Use when:** “What happens to precision and confidence?”

**Write:**

> For the same population variability, increasing n reduces the standard error and generally narrows the interval. This improves precision without changing the nominal confidence level. It does not guarantee that the particular new interval contains the parameter.
**Qualification:** For known σ and fixed z, multiplying n by four halves the width. With estimated variability, realised widths also depend on the new sample.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.20 — Reference value is inside the interval

**Use when:** “Does the parameter differ from c?” or “Is c compatible?”

**Write:**

> The value [c] lies inside the [C]% confidence interval, so it is compatible with the data at that confidence level. The interval does not provide sufficient evidence to conclude that the population [mean / proportion] differs from [c]. This does not prove that the parameter equals [c].
**Qualification:** This is interval-based reasoning; no hypothesis-test mechanics are added to the first-partial bank.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.21 — Reference value is outside, or a threshold is crossed

**Use when:** Choose the version supported by the endpoints.

**Write:**

> ABOVE: The entire interval lies above [c], supporting a population parameter greater than [c] at the stated confidence level.
> BELOW: The entire interval lies below [c], supporting a parameter below [c].
> STRADDLES: The interval includes values both below and above [c], so it does not establish on which side the parameter lies.
**Avoid:** Do not claim certainty, or conclusions at every confidence level, from one interval.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


### 6I.22 — Overlapping intervals for two separate groups

**Use when:** Two individual-parameter intervals are shown.

**Write:**

> These intervals describe the two population parameters separately. Their overlap or separation should not be treated as a substitute for constructing the appropriate interval for their difference. I can report their locations and precision, but a formal difference procedure is separate material.
**Qualification:** Two-population estimation is assigned to the second partial. Do not import its formulas into this bank.

*Sources: F §6; B6 §§6.3–6.5; G: confidence intervals; PJ26 Q1.a*


<a id="section-06n"></a>

## Chapter 6C | Required sample size and R output

Separate target SE, target margin of error and target total width.

**Basis:** F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size


### 6N.01 — Mean: margin of error, σ known

**Use when:** “ME at most e” with a stated confidence level.

**Write:**

> I require z_(α/2)σ/√n ≤ e. Solving gives n ≥ (z_(α/2)σ/e)². Rounding upwards, at least [n] observations are required for the desired precision under the stated procedure and known σ.

**Formula / R:** `R: ceiling((qnorm(1-alpha/2)*sigma/target_ME)^2)`

**Avoid:** Keep full precision until the ceiling operation.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.02 — Width rather than margin of error

**Use when:** “CI width / length at most W”.

**Write:**

> The required total width is [W], so the target margin of error is W/2. I substitute that half-width into the sample-size formula and round the resulting sample size upwards.

**Formula / R:** `ME*=W*/2; for known-σ mean: n_min=ceiling((2*z*sigma/W*)²)`

**Avoid:** Putting total width into the ME slot gives a sample four times too small.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.03 — Mean: σ unknown at the planning stage

**Use when:** A pilot sample gives s.

**Write:**

> Because σ is unknown, the required sample size can only be planned approximately using an available sample SD and a stated reliability factor. The result does not guarantee the target realised margin of error, since the new sample SD may differ and the t factor depends on the degrees of freedom.

**Formula / R:** `Book's plug-in planning form: n* ≳ (t × s / target_ME)²`

**Qualification:** State which pilot SD and factor are used. Do not label this the exact minimum for every future sample.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.04 — Proportion: conservative planning

**Use when:** No prior proportion is used; a confidence level and ME are specified.

**Write:**

> The expression p(1−p) is maximised at p=0.5, where it equals 0.25. Using that maximum gives the conservative requirement n ≥ (z_(α/2)×0.5/e)². Rounding up gives at least [n] units.

**Formula / R:** `R: ceiling((qnorm(1-alpha/2)*0.5/target_ME)^2)`

**Qualification:** The conservative bound concerns the margin of error of this Normal-formula interval; it does not turn approximate coverage into exact coverage.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.05 — Proportion: use an existing estimate for planning

**Use when:** The exercise explicitly supplies or requests a planning proportion.

**Write:**

> Using the available planning value p̃ gives n ≥ z² p̃(1−p̃)/e². This is a plug-in planning calculation and depends on the assumed proportion. It is not the worst-case guarantee provided by the conservative p=0.5 calculation.

**Formula / R:** `n_plan=ceiling(z²*p_tilde*(1-p_tilde)/target_ME²)`

**Avoid:** Do not call a plug-in result conservative for every possible population proportion.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.06 — Percentage points and integer rounding

**Use when:** The precision target is stated as a percentage.

**Write:**

> A margin of error of [3] percentage points is [0.03] on the proportion scale. I use this decimal value throughout the calculation and round the required sample size up, because rounding down may fail the target.
**Avoid:** Do not use 3 where the formula expects 0.03. A multiplicative factor of 1/2 is positive; 'a decrease of 50%' describes the change.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.07 — Read CI.mean output

**Use when:** “Explain the columns and conclude.”

**Write:**

> n is the number of valid observations; xbar is the point estimate; sigma_X is a supplied population SD, while s_X denotes a sample SD; SE/se is the standard error or its estimate; Lower and Upper are the confidence limits. I identify the variance setting and method row, then interpret those endpoints about the population mean.
**Qualification:** Unknown σ produces Normal.Approx and Student-t alternatives in the textbook. See 6I.15.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.08 — Read CI.prop output

**Use when:** “Explain the output for the specified success category.”

**Write:**

> n is the number of valid observations, phat is the sample proportion of the specified success, s_X summarises variation in the binary variable, se is the estimated standard error of the sample proportion, and Lower/Upper are its confidence limits. The conclusion concerns the population proportion of [success] among [eligible units].
**Avoid:** Do not use s_X as the SE. Check the success category and missing-value treatment.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


### 6N.09 — Explain R without replacing the statistical answer

**Use when:** R performs the computation.

**Write:**

> The command calculates [requested probability / quantile / confidence limits] using [distribution], with [mean/SD or df] chosen for [statistical object]. Its result is [number]. In context, [interpretation].
**Qualification:** When the prompt says 'show your steps', include formula, substituted numbers and result. R code alone is not a contextual conclusion.

*Sources: F §6; B6 §§6.2, 6.4, 6.5, 6.10; G: confidence intervals and sample size*


<a id="section-06x"></a>

## Appendix A | Additional estimator definitions in the book

The syllabus expressly names unbiasedness, asymptotic unbiasedness, variance and SE. These additional properties occur in the referenced Chapter 6.1.2 and in your notes.

**Basis:** F §6; B6 §6.1.2; G: definitions


### 6X.01 — Consistency

**Use when:** “Define consistency” in a textbook-properties question.

**Write:**

> An estimator is consistent if the probability of an error larger than any fixed positive tolerance tends to zero as sample size increases. This concerns concentration around the parameter as n grows, not an assurance that a particular estimate is correct.

**Formula / R:** `P(|θ̂ₙ−θ|>ε) → 0 for every ε>0`

**Qualification:** Keep this distinct from unbiasedness, which concerns the estimator's expected value.

*Sources: F §6; B6 §6.1.2; G: definitions*


### 6X.02 — Mean squared error and efficiency

**Use when:** “Compare estimators by MSE.”

**Write:**

> The MSE is the expected squared difference between the estimator and the parameter. Under the book's MSE comparison criterion, the estimator with smaller MSE is more efficient. For unbiased estimators, MSE equals variance, so a smaller SE gives a smaller MSE.

**Formula / R:** `MSE(θ̂)=E[(θ̂−θ)²]=Var(θ̂)+Bias(θ̂)²`

**Avoid:** Lower MSE ranks procedures, not the accuracy of two particular observed estimates.

*Sources: F §6; B6 §6.1.2; G: definitions*


### 6X.03 — MSE-consistency

**Use when:** The book asks how the estimator improves as n increases.

**Write:**

> The estimator is MSE-consistent if its MSE tends to zero. For the unbiased sample mean, MSE(X̄)=σ²/n, which tends to zero as n increases. This gives the increasing concentration of the estimator around μ.

**Formula / R:** `MSE(X̄)=σ²/n → 0`

**Qualification:** No unsupported general converse between consistency and unbiasedness is needed in an exam answer.

*Sources: F §6; B6 §6.1.2; G: definitions*


<a id="official-excerpts"></a>

## Appendix B — Verbatim solution excerpts

These are exact word excerpts from your supplied transcription, with formatting markup removed. They are not newly written templates and retain the source's wording. Their numerical conclusions belong to the original exercises.

### Q1 | Choosing side-by-side boxplots

**Source:** P24, Exercise 1(a1)

> To compare the distributions of Impressions for the subgroups identified by Paid we rely on side-by-side boxplots

### Q2 | Explain what group differences mean

**Source:** P25, Question 1.a

> The three distributions differ in shape, location, and variability.

### Q3 | Why the scatterplot comes first

**Source:** P25, Question 2

> To assess the relationship between two variables, the fundamental tool is the scatterplot, which allows us to highlight any departures from a linear relationship, and to determine how reliable the correlation coefficient is for assessing the strength of a linear relationship.

### Q4 | Conditional, not joint, percentages

**Source:** P25, Question 5

> We are interested in the percentage of clients who bought their ticket with an average advance conditional on two levels of Channel.

### Q5 | The wrong denominator warning

**Source:** P25, Question 5

> Note that it was wrong to answer based on joint counts or percentages (72 vs 76 or 11% vs 11%)

### Q6 | Estimator versus realised estimate

**Source:** P25, Question 6.b

> We can therefore say that the first estimator is more reliable, since its estimates are (estimated to be) more tightly clustered around the population mean; however, we cannot draw conclusions about the reliability of specific realised estimates or about their distance from the corresponding parameter.

### Q7 | Single-proportion CI interpretation

**Source:** PJ26, Question 1.a

> with a level of confidence 99% we can conclude that the proportion of interest (customers who apply for a loan for reasons related to business) lies between 0.15 and 0.24.

### Q8 | State why a grouped-data result is approximate

**Source:** M1, Exercise 1.1(i)

> The values obtained for mean and median are approximate because they are obtained from the data aggregated into classes and not from the raw data. Specifically, we approximate their values under the assumption of uniform distribution for data grouped in the same interval.

### Q9 | Unequal histogram widths

**Source:** M1, Exercise 1.1(d)

> In the second case, using frequencies other than the densities would be incorrect since the classes have different width and therefore the information that provides the aggregate frequency at the interval level must account for the width of the class.

### Q10 | Compare dispersion across units

**Source:** P25, Question 3.b

> To compare the dispersion of two variables with different unit of measurement, we need to consider the coefficient of variation:


<a id="personal-error-log"></a>

## Appendix C — Your personal error log

### “Point estimate = expected value of estimator.”

The point estimate is the observed value x̄ or p̂. The expectations are E(X̄)=μ and E(P̂)=p, with the parameters unknown.

**Revisit:** 6E.01–03

### “The SE is how far my actual estimate is from μ.”

The SE describes estimator variability across samples; it does not reveal |x̄−μ| for this sample.

**Revisit:** 6E.06–12

### “I need Normality to calculate an SE.”

For the mean's SE, i.i.d. observations and finite population variance suffice. Distributional assumptions matter for specific interval or probability procedures.

**Revisit:** 6E.08

### “Variance of P̂ is its SE.”

Estimated variance is p̂(1−p̂)/n. Estimated SE is the square root.

**Revisit:** 6E.09

### “0.24/900 = 2.6667.”

0.24/900 = 0.00026667; the estimated SE is about 0.01633. Check the scale before interpreting.

**Revisit:** 6E.09

### “√400 = 40.”

√400 = 20. A correct formula still needs an arithmetic check.

**Revisit:** 6E.07

### “A 95% CI contains the parameter with 95% probability.”

The coverage probability belongs to the random interval procedure. Say: We are 95% confident that [parameter] lies between [L] and [U].

**Revisit:** 6I.01–04

### “Probability is only ex ante, never ex post.”

That statement is too broad. Explain the fixed-parameter and fixed-observed-interval distinction for this framework specifically.

**Revisit:** 6I.04

### “The t interval is always wider.”

With the same confidence level and numerical SE, the t factor is larger. Different s and σ values can change the comparison.

**Revisit:** 6I.13

### “n>30 makes the population Normal.”

CLT concerns sums and means, and the approximation depends on the population shape and assumptions.

**Revisit:** 5S.04–09

### “Independent means equal cells.”

Check joint = product of the corresponding marginals; or compare a conditional distribution with its marginal.

**Revisit:** 4.05–06; 5P.09–11

### Wrong joint cell / wrong conditioning group.

Read both row and column labels. The event after the conditioning bar supplies the denominator.

**Revisit:** 4.01–04

### “CV = mean/SD.”

CV = SD/absolute mean. SD goes on top; larger CV means greater relative dispersion.

**Revisit:** 3.23–24

### A class midpoint entered as 27.5 for [15,30).

Check (15+30)/2=22.5 before calculating the weighted mean or variance.

**Revisit:** 3.15–18

### “Use the largest class count for the modal class.”

With unequal widths, compare densities pₖ/wₖ.

**Revisit:** 2.07–10

### “The boxplot whisker ends at the fence.”

It ends at the most extreme regular observed value.

**Revisit:** 3.20–21

### “Variance scales by b; or just take an absolute value.”

Variance scales by b²; SD scales by |b|; shifts affect only the mean.

**Revisit:** 5R.11–14

### “pnorm is the right tail; z_α is qnorm(α).”

pnorm is lower-tail by default. The book's z_α is qnorm(1−α).

**Revisit:** 5N.02–08

### Answer stops after identifying the method.

Where requested, finish the calculation, show R, state units and give the contextual conclusion.

**Revisit:** Front-page verb decoder


<a id="sources"></a>

## Sources

**S:** Uploaded syllabus (6).pdf, Statistics 30001, 2026/27: detailed schedule pp. 2–5 and first-midterm scope p. 7.

**F:** ASM_Framework_Knowledge.md, §§1–6: course notation, formulas, procedures and interpretation rules.

**B1:** Applied Statistical Methods (Piccarreta, Tonini, Trentini, Egea 2025), Chapter 1, stats1_ocred.pdf.

**B2:** Applied Statistical Methods, Chapter 2, stats2_ocred.pdf.

**B3:** Applied Statistical Methods, Chapter 3, stats3_ocred.pdf.

**B4:** Applied Statistical Methods, Chapter 4, stats4_ocred.pdf.

**B5:** Applied Statistical Methods, Chapter 5, stats5_ocred.pdf.

**B6:** Applied Statistical Methods, Chapter 6, stats6_ocred.pdf.

**M1:** Stats_past_papers_verbatim.md, Module 1: Graphs and Central Tendency Measures, questions and supplied official solutions.

**P25:** Stats_past_papers_verbatim.md, First midterm, 14 October 2025, Shift 1 Version A, Bidding.

**P24:** Stats_past_papers_verbatim.md, First partial, 22 October 2024, Shift 2 Version A, Metrics2.

**PJ26:** Stats_past_papers_verbatim.md, Second partial, 9 January 2026, Version A, Question 1.a. Used only for single-proportion CI wording.

**G:** Stats_1st_partial_answer_guide.md, topic cards, verb decoder, definitions and Part 4 phrasing bank. This is a study aid, not itself an official marking scheme.


## Source fidelity and limits

- Source priority: the uploaded syllabus determines first-partial scope; the ASM framework supplies clean formulas and notation; the book supplies definitions and qualifications; the supplied exam transcription supplies the separately quoted solutions.
- Appendix B quotes are exact word excerpts from the supplied Markdown transcription with presentation markup removed. They have not been independently checked against the original exam PDF images. Their original wording is retained rather than silently repaired.
- An official answer for one dataset is not a universal rule. The templates therefore state conditions such as equal SEs when comparing t versus z, sufficient sample sizes for approximations, and the effects of ties on percentile interpretations.
- The source guide sometimes uses shorthand such as CV=s/mean in positive-mean examples. This bank explicitly uses the framework's general formula CV=s/|x̄|.
- The supplied materials discuss more than one rule of thumb for Normal approximation of proportions. No single cutoff is presented here as a universally exact theorem.
- This bank does not reproduce the disputed printed sample-size arithmetic discussed earlier in the chat. Use the stated formula and target (SE, ME or total width), retaining full precision and rounding the final n upwards.
- Some previous messages labelled source-adapted or hypothetical examples as if they were official. This file labels all main answer cases as source-adapted and reserves 'verbatim' for the checked excerpts in Appendix B.