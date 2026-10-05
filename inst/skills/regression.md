# REGRESSION RULES

Simple/multiple linear regression:
Y is dependent variable. X variables are explanatory.

Coefficient:
- intercept: fitted/estimated Y when all X=0, only substantively meaningful if 0 is meaningful
- simple slope: average fitted change in Y for +1 X
- multiple slope: average fitted change in Y for +1 X holding all other explanatory variables constant

Individual t test:
H0: beta_j=0 (unless another null stated)
uses coefficient estimate / SE and model residual df.

Global F test:
H0: all slope coefficients = 0
H1: at least one slope coefficient !=0.
A significant F test says at least one explanatory variable contributes; it does not say every coefficient is significant.

R2:
proportion of sample variability in Y explained by fitted model.
Adjusted R2 is preferred for comparing models with different numbers of predictors.

Residual standard error:
estimated residual/error SD; typical residual scale in Y units.

Multicollinearity warning signs in course:
- strong pairwise correlation, typically |r| >= 0.7
- global F significant while individual t tests weak
- unexpected signs
- coefficients/SE/significance change sharply after adding predictors
Do not call F a contradiction to t tests.

Prediction:
- confidence interval for mean response is narrower
- prediction interval for one individual is wider because it includes individual outcome variability

Diagnostics before prediction:
- residuals vs fitted: linearity / structural pattern and spread
- Normal Q-Q: approximate normality of errors
- residual spread: homoskedasticity
- standardized residuals, leverage, Cook's distance: unusual/influential observations
