# PAST-PAPER WORDING PATTERNS -> WHAT TO RUN

"Report the estimate of the proportion ... and specify how the corresponding standard error is estimated"
  -> desc_prop()                                     NOT a test

"Assess whether average ... differs significantly between ..."
  -> two means: same units twice? test_paired(); different units? test_2means(case = ...)

"Assume equal population variances"                   -> case = "pooled"
"Same states / customers / firms before and after"    -> test_paired()
"Are the two categorical variables associated?"       -> chisq_indep()
"Do observed shares agree with / follow ...?"         -> chisq_gof(p = ...)
"Interval classes, frequency density, modal class"    -> desc_classes()
"p90 / p95 thresholds, outliers"                      -> desc_summary()
"Compare dispersion, different units"                 -> desc_cv()
"Probability of not rejecting H0 when mu = ..."       -> power_mean()  (Type II error)
"Model globally significant but variables not"        -> reg_check(): multicollinearity, not a contradiction
"Predict the average/expected value for ..."          -> reg_predict(): CI for the mean response
"Predict one new individual/case"                     -> reg_predict(): prediction interval
"Compare models with different numbers of predictors" -> reg_compare(): adjusted R2
"Test whether the slope equals 1"                     -> reg_test(term = ..., value = 1)
