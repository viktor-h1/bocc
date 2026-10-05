# POINT ESTIMATION / STANDARD ERROR RULES

The course distinguishes parameter (population), estimator (random variable) and estimate (number).

Population mean mu:
- estimator: sample mean Xbar
- sigma known:   SE(Xbar) = sigma / sqrt(n)
- sigma unknown: estimated SE(Xbar) = s / sqrt(n)

Population proportion p:
- estimator: sample proportion p-hat = x / n
- Var(p-hat) = p(1 - p)/n is unknown because p is unknown
- estimated SE(p-hat) = sqrt[p-hat (1 - p-hat) / n]        -> desc_prop()

Critical distinction:
- "Report/estimate the proportion and its standard error" -> desc_prop(), NOT a test (no p0).
- "Is the proportion greater than 0.25?" -> test_prop(p0 = 0.25, alt = ">").

Larger n reduces the standard error. Sample-size questions ask for the n that keeps the SE or the
margin of error below a target -> n_mean() / n_prop() (always round UP).
