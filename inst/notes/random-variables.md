# PROBABILITY AND RANDOM VARIABLES (book chapter 5)

## Events and probability (5.1)                                 prob_events()  sc(2,1)
- Sample space Omega (all simple events); event A = subset; complement A^c; union A or B;
  intersection A and B; mutually exclusive: A and B = empty; exhaustive: union = Omega.
- Classical P(A) = favourable / possible outcomes (equally likely); frequentist = long-run
  relative frequency; empirical = observed proportion (an estimate); subjective = degree of belief.
- Axioms -> P(A^c) = 1 - P(A);  P(A or B) = P(A) + P(B) - P(A and B);  A in B -> P(A) <= P(B).
- Conditional: P(A | B) = P(A, B) / P(B).  Multiplication rule: P(A, B) = P(A | B) P(B);
  P(E1, ..., En) = P(E1) P(E2 | E1) P(E3 | E1, E2) ...
- Independence: P(A, B) = P(A) P(B), i.e. P(A | B) = P(A).
- Sampling WITHOUT replacement changes the population (e.g. 75/250 then 74/249); WITH
  replacement (or a very large population) the draws are independent.

## Total probability and Bayes (5.1.2-5.1.3)                      prob_bayes()  sc(2,2)
- Partition E_1..E_n (exhaustive, mutually exclusive): P(B) = sum P(B | E_i) P(E_i).
- Bayes: P(E_k | B) = P(B | E_k) P(E_k) / P(B).
- Tests: sensitivity = P(T | D), specificity = P(T^c | D^c); with a rare disease a positive test
  can still mean a low P(D | T) (1/2000, 0.99, 0.98 -> P(D | T) = 0.0242).

## Discrete random variables (5.2.1)                              rv_discrete()  sc(2,3)
- p_X(x) = P(X = x);  E(X) = mu = sum x p(x);  Var(X) = sum (x - mu)^2 p(x) = E(X^2) - mu^2.
- F_X(x) = P(X <= x).  Quantile q_alpha = smallest value with F(q_alpha) >= alpha.
- Notation: q_alpha = x_(1-alpha) (right subscript = probability ABOVE the value).

## Bernoulli and Binomial (5.2.2)                                  prob_binom()  sc(2,4)
- Bernoulli(p): X = 1 (success) with prob. p, 0 otherwise; E = p, Var = p (1 - p).
- Binomial(m, p): successes in m INDEPENDENT trials with the same p:
     p(x) = C(m, x) p^x (1 - p)^(m - x),  C(m, x) = m! / (x! (m - x)!);  E = m p,  Var = m p (1 - p).
- R: dbinom(x, m, p) = P(X = x); pbinom(q, m, p) = P(X <= q); qbinom(alpha, m, p).
  P(X > 7) = 1 - pbinom(7, ...);  P(X >= 2) = 1 - pbinom(1, ...);  P(X < 5) = pbinom(4, ...).

## Continuous random variables (5.2.3-5.2.4)
- Density f(x) >= 0, total area 1, P(a <= X <= b) = area = F(b) - F(a); P(X = a) = 0 (< and <= equal).
- Uniform(a, b): f = 1/(b - a);  E = (a + b)/2;  Var = (b - a)^2 / 12;  q_alpha = a + (b - a) alpha.
                                                                prob_unif()    sc(2,5)
- Normal N(mu, sigma^2): symmetric bell, mean = median = mode.   prob_normal()  sc(2,6)
  R: pnorm(q, mean, sd) = P(X <= q);  qnorm(alpha, mean, sd) = q_alpha  (sd, not the variance!).
- Standardisation Z = (X - mu)/sigma ~ N(0, 1);  x_alpha = mu + z_alpha sigma;  z_(1-alpha) = -z_alpha.
- z: 1.2816 (90%), 1.6449 (95%), 1.96 (97.5%), 2.3263 (99%).
- Empirical rule: within 1, 2, 3 sigma: 68%, 95%, 99.7%; beyond 3 sigma = rare / outliers.

## Linear transformations and combinations (5.2.5, 5.3.3-5.3.4)    rv_lincomb()  sc(2,9)
- Y = a + bX: E(Y) = a + b mu_X;  Var(Y) = b^2 sigma_X^2;  SD(Y) = |b| sigma_X.  Normal stays normal.
- aX + bY: E = a mu_X + b mu_Y;  Var = a^2 sigma_X^2 + b^2 sigma_Y^2 + 2ab sigma_XY,  sigma_XY = rho sigma_X sigma_Y.
- sum a_i X_i: E = sum a_i mu_i;  Var = sum a_i^2 sigma_i^2 + 2 sum_(i<j) a_i a_j sigma_ij.
- Probabilities need the distribution: (jointly) normal -> the combination is normal.
- Portfolio: diversifying with negatively correlated assets lowers the SD.

## Joint distribution of two discrete r.v.s (5.3)                  rv_joint()  sc(2,10)
- p_XY(x, y); marginals p_X(x) = sum_y p_XY(x, y); conditionals p_(Y|X)(y | x) = p_XY(x, y) / p_X(x).
- Cov(X, Y) = E(XY) - mu_X mu_Y;  Corr = Cov / (sigma_X sigma_Y).
- Independent: p_XY(x, y) = p_X(x) p_Y(y) for all pairs -> Cov = Corr = 0 (not the converse,
  except for jointly normal variables).

## Sums and means of iid r.v.s; Central Limit Theorem (5.4)          rv_iid()  sc(2,11)
- X_1..X_n iid with mean mu, variance sigma^2:
     sum: E = n mu, Var = n sigma^2;   mean Xbar: E = mu, Var = sigma^2 / n.
- If X is normal: exactly normal.  Sum of iid Bernoulli(p) = Binomial(n, p).
- CLT: for n large (typically > 30) sum ~ N(n mu, n sigma^2) and Xbar ~ N(mu, sigma^2/n),
  WHATEVER the distribution of X.  Not applicable to ONE observation (n = 1).
- Proportion of successes p-hat ~ N(p, p(1 - p)/n); number of successes ~ N(np, np(1 - p))
  (n p (1 - p) large enough; the book uses no continuity correction).    rv_prop()  sc(2,12)
