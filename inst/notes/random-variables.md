# RANDOM VARIABLES / SAMPLING RULES

Discrete RV:
E(X)=sum x p(x)
Var(X)=sum (x-mu)^2 p(x)

Linear transformation Y=a+bX:
E(Y)=a+bE(X)
Var(Y)=b^2 Var(X)
SD(Y)=|b| SD(X)

Two-variable linear combination T=aX+bY+c:
E(T)=aE(X)+bE(Y)+c
Var(T)=a^2 Var(X)+b^2 Var(Y)+2ab Cov(X,Y)
Cov(X,Y)=rho SD(X) SD(Y)

If relevant variables are jointly normal, linear combinations are normal.

IID:
E(sum)=n mu
Var(sum)=n sigma^2
E(Xbar)=mu
Var(Xbar)=sigma^2/n
SE(Xbar)=sigma/sqrt(n)

CLT/normal sampling approximation is a distributional justification, not a license to guess missing assumptions.
