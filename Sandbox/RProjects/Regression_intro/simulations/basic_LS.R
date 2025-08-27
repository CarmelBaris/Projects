####  basic Least Squares analysis + inference under the normal model  ####


set.seed(1)  # for reproducibility

n <- 40
p <- 4

# generate some X matrix
mu <- c(1,1,2,2)
A <- round(matrix(rnorm(p^2,sd = 2),nrow = p),1)
Z <- matrix(rnorm(n*p),nrow = p)
X.t <- mu + A%*%Z
X <- t(X.t)
X <- cbind(1,X) #add col of 1's

dim(X)
head(X)

# set the true coefficient vector beta
beta <- c(-1,.1,3,1.5,.3) # vector in R^{p+1}
print(beta)
eta <- X %*% beta # vector in R^n (=E[Y] under the linear model)
epsilon <- rnorm(n)  #sigma^2=1
Y <- eta + epsilon # vector in R^n

# automatic LS fitting
x0 <- X[,1]
x1 <- X[,2]
x2 <- X[,3]
x3 <- X[,4]
x4 <- X[,5]
y <- Y
mydata <- data.frame(y,x0,x1,x2,x3,x4)
head(mydata)

fm <- lm(Y ~ x1 + x2 + x3 + x4, data = mydata) # main function. Intercept automatically added by default (use 'Y~x1+x2+x3+x4 - 1,data=mydata' for LS with no intercept)
summary(fm)

head( model.matrix(fm) ) # X matrix
fm$coefficients # beta-hat
head( fm$fitted.values ) # Yhat
head( fm$residuals ) # e = Y-Yhat

# manual analysis
V <- solve( t(X) %*% X )  # V = (X^t X)^{-1}
beta.hat <- V %*% t(X) %*% Y
beta.1.hat <- beta.hat[2] # extract second coordinate of beta.hat
e <- Y - X %*% beta.hat
sigsq.hat <- sum(e^2)/(n-p-1)
sqrt(sigsq.hat) # sigma-hat # compare to "Residual standard error" in automatiuc output

v.1 <- V[2,2] # extract (2,2) entry of V # in class: V_{11}

var.hat <- sigsq.hat * v.1 # estimated standard error of beta.1.hat
sqrt(var.hat) # compare to value in "Std. Error"

( T.1 <- beta.1.hat/sqrt(var.hat) ) # compare to value in "t value"
( pval <- 2* (1-pt(T.1, df = n-p-1)) ) # compare to value in "Pr(>|t|)"


## simulation: 10^4 runs of the same experiment

beta.1 <- beta[2] #recall: what we denote by beta_1 is actually the *second* coordinate of the vector beta

nsim <- 10^4  # number of simulation rounds
beta.1.hat <- rep(NA,nsim)  # for each simulation round, this will record the estimate of beta.1 
T.star.1 <- rep(NA,nsim)  # t-statistic for beta.1 (this is the "studentized" version of beta.1.hat)
T.1 <- rep(NA,nsim) # t-statistic for beta.1 computed under the null hypothesis
CI.l <- rep(NA,nsim) # lower limit of the CI
CI.u <- rep(NA,nsim) # upper limit of the CI
is.covering <- rep(NA,nsim) # indicator for the event "CI includes beta.1"
pval <- rep(NA,nsim) # pvalue for testing H_0:beta.1=0
is.covering.zero <- rep(NA,nsim) # indicator for the event "CI includes zero"

for (i in 1:nsim){
  epsilon <- rnorm(n)  #sigma^2=1
  Y <- eta + epsilon # eta = Xbeta
  
  beta.hat <- V %*% t(X) %*% Y # LS estimator
  beta.1.hat[i] <- beta.hat[2]
  e <- Y - X %*% beta.hat
  sigsq.hat <- sum(e^2)/(n-p-1)
  v.1 <- V[2,2]
  
  alpha <- .05
  var.hat <- sigsq.hat * v.1
  T.star.1[i] <- (beta.1.hat[i] - beta.1) / sqrt(var.hat)
  T.1[i] <- abs(beta.1.hat[i])/sqrt(var.hat)
  pval[i] <- 2* (1-pt(T.1[i], df = n-p-1)) # note: the p-value is computed using T.1 (null distribution), not T.star.1
  CI.l[i] <- beta.1.hat[i] - qt(1-alpha/2,df = n-p-1) * sqrt(var.hat)
  CI.u[i] <- beta.1.hat[i] + qt(1-alpha/2,df = n-p-1) * sqrt(var.hat)
  is.covering[i] <- (beta.1 < CI.u[i]) & (CI.l[i] < beta.1)
  is.covering.zero[i] <- (0 < CI.u[i]) & (CI.l[i] < 0)
}

beta.1
head( cbind(beta.1.hat, CI.l, CI.u, is.covering, is.covering.zero) )
mean(is.covering)  # we expect ~0.95
hist(pval, breaks = 100, freq = FALSE)
abline(h=1,col='red')
mean(pval<.05) # estimate of rejection prob = power of the test (if beta.1=0, this is expect to be approxmately 0.05)
# verify duality:  {p-val < alpha} if and only if {beta.1 not in CI}
cbind(pval < alpha, !is.covering.zero)[1:20,] 


# inspect empirical distribution of beta.1.hat and T.star.1

# beta.1.hat
h <- hist(beta.1.hat, breaks = 50)
abline(v=beta.1,col='black', lwd=3, lty=2)

# add normal density
xfit <- seq(min(beta.1.hat), max(beta.1.hat), length.out = 100)
yfit <- dnorm(xfit, mean=beta.1, sd=sqrt(V[2,2]))
yfit <- yfit * diff(h$mids[1:2]) * length(beta.1.hat)
lines(xfit,yfit, col='b lack', lwd=1.5)

# T.star.1
h <- hist(T.star.1, breaks = 50)

# add t density
xfit <- seq(min(T.star.1), max(T.star.1), length.out = 100)
yfit <- dt(xfit, df = n-p-1)
yfit <- yfit * diff(h$mids[1:2]) * length(beta.1.hat)
lines(xfit,yfit, col='black', lwd=1.5)
# compare to standard normal density
lines(xfit,dnorm(xfit)*diff(h$mids[1:2]) * length(beta.1.hat), col='red', lwd=1.5,lty=2)

legend('topright', legend=c(expression(t[35]), 'N(0,1)'), col=c("black", "red"), lty=c(1,2), lwd=c(1.5,1.5), bty='n', cex=1.25, y.intersp=1.5)



