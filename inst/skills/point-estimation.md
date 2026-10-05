# POINT ESTIMATION / STANDARD ERROR RULES

The course distinguishes parameter, estimator, and estimate.

Population mean:
- estimator: sample mean
- if population sigma known: SE(Xbar)=sigma/sqrt(n)
- if sigma unknown: estimated SE(Xbar)=s/sqrt(n)

Population proportion:
- estimator: sample proportion p-hat=x/n
- Var(P-hat)=p(1-p)/n is unknown because p is unknown
- estimated SE(P-hat)=sqrt[p-hat(1-p-hat)/n]

Critical distinction:
"Report/estimate the proportion and its standard error" -> descriptive_proportion_se.
Do NOT request p0.
"Is the proportion greater than 0.25?" -> one_proportion_test and null_value=0.25.

Larger n reduces standard error. Sample-size questions may ask for n needed to keep SE or margin of error below a target.
