# PAST-PAPER WORDING PATTERNS

Treat these as intent patterns, not as assumptions.

"Report the estimate of the proportion ... and specify how the corresponding standard error is estimated"
-> descriptive_proportion_se, NOT a hypothesis test.

"Assess whether average ... differs significantly between ..."
-> infer mean comparison structure; paired/independent and variance assumptions must come from wording/data design.

"Assume equal population variances"
-> variance_case="pooled".

"Same states/customers/firms before and after"
-> paired=true.

"Are the two categorical variables associated?"
-> chisq_independence.

"Do observed category shares agree with / follow specified shares?"
-> chisq_gof.

"Model is globally significant but individual variables are insignificant"
-> possible multicollinearity discussion, not logical contradiction.

"Predict the average/expected value for cases with X=..."
-> prediction_mean_response.

"Predict one new individual/case"
-> prediction_individual.

"Compare models with different numbers of predictors"
-> compare_models_adjusted_r2.
