source("C:/Users/user/Documents/Carmel/RProjects/Principles_Micha/submissions/paper2 - inferential stats/spotify_data_cleanup.R")

# ++++++++++++++++++++++++ #
# INTRO
# ++++++++++++++++++++++++ #

# Y dance, explained by changes in X
# X valence
m <- min(valence)
W <- valence - m

# ++++++++++++++++++++++++ #
# GIVEN ASSUMPTIONS
# ++++++++++++++++++++++++ #

# X := W + min(valence), and because linear transformation preserves the distribution model:
# X~Gamma(α x.shape, λ x.rate)

# Y~N(μ, σ^2)

# W := valence - min(valence)
# W~Gamma(α w.shape, λ w.rate)


# ++++++++++++++++++++++++ #
# POINT ESTIMATORS
# ++++++++++++++++++++++++ #

# Y estimators using MOM
y.mu.mom <- mean(dance)
y.var.mom <- mean(dance^2) - (mean(dance))^2
y.sd.mom <- sqrt(y.var.mom)

# Y estimators using MLE
y.mu.mle <- mean(dance)
y.var.mle <- var(dance) # == unbiased estimator, sum((dance-mean(dance))^2) * (1/((n=3969)-1))
y.sd.mle <- sd(dance) # == sqrt(var(dance))

# --------------

# W estimators using MOM
w.rate <- mean(W)/(mean(W^2)-(mean(W))^2)
w.shape <- w.rate * mean(W) # == (mean(W))^2/(mean(W^2)-(mean(W))^2)

# --------------

# X estimators using MOM
x.rate <- mean(valence)/(mean(valence^2)-(mean(valence))^2)
x.shape <- x.rate * mean(valence) # = 6.394555 + (mean(valence))^2/(mean(valence^2)-(mean(valence))^2)


# ++++++++++++++++++++++++ #
# QUANTILES
# ++++++++++++++++++++++++ #

# Y distribution quantiles vs empirical quantiles
a <- qnorm(p=c(0.1, 0.5, 0.75, 0.9), mean=y.mu.mle, sd=y.sd.mle) #Y
b <- quantile(dance, probs=c(0.1, 0.5, 0.75, 0.9)) #Y
cat("\n DI qnorm", a)
cat("\n DI quantile", b)
max.diff.y <- max(abs(diff(a-b)))
min.diff.y <- min(abs(diff(a-b)))
change.y <- ((a-b)/a)*100
cat("\n DI change percentage", change.y, "%")
cat("\n DI change mean",  round(mean(change.y) * 100, 3), "%")
#qqnorm(y=a, main="Danceability Index Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles, plot.it=TRUE, datax=TRUE")
#qqgamma(y=a, main="Danceability Index Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles, plot.it=TRUE, datax=TRUE")

# X distribution quantiles vs empirical quantiles
c <- qgamma(p=c(0.1, 0.5, 0.75, 0.9), shape=x.shape, rate=x.rate) #X
d <- quantile(valence, probs=c(0.1, 0.5, 0.75, 0.9)) #X
cat("\n VI qgamma", c)
cat("\n VI quantile", d)
max.diff.x <- max(abs(diff(c-d)))
min.diff.x <- min(abs(diff(c-d)))
change.x <- ((c-d)/c)
cat("\n VI change percentage", change.x, "%")
cat("\n VI change mean",  round(mean(change.x) * 100, 3), "%")

# W distribution quantiles vs empirical quantiles
e <- qgamma(p=range(0,0.99), shape=w.shape, rate=w.rate) #W
f <- quantile(W, probs=range(0,0.99)) #W
cat("\n W qgamma", e)
cat("\n W quantile", f)
max.diff.w <- max(abs(diff(e-f)))
min.diff.w <- min(abs(diff(e-f)))
change.w <- ((e-f)/e)*100
cat("\n W change percentages", change.w, "%")
cat("\n W change mean",  round(mean(change.w) * 100, 3), "%")

# ++++++++++++++++++++++++ #
# CONFIDENCE INTERVALS
# ++++++++++++++++++++++++ #

# Y expectancy CI
expectancy_bounds <- function (x, cl){
  n <- length(x)
  df <- n-1
  p <- 1 - (1-cl)/2

  cat("\n For a confidence level of:",paste0(cl*100,"%"))
  cat("\n CI lower bound (left border): ", mean(x) - sd(x) * qt(p, df) / sqrt(n))
  cat("\n CI upper bound (right border): ", mean(x) + sd(x) * qt(p, df) / sqrt(n))
}
expectancy_bounds(x=dance, cl=0.97)

# Y variance CI
variance_bounds <- function (x, cl){
  var <- var(x)
  df <- length(x)-1
  u.p <- (1-cl)/2
  l.p <- 1- u.p

  l.cl <- sqrt((df*var) / qchisq(p=l.p, df=df))
  u.cl <- sqrt((df*var) / qchisq(p=u.p, df=df))

  cat("\n For a confidence level of:",paste0(cl*100,"%"))
  cat("\n CI lower bound (left border): ", paste0(round(l.cl, 3), "^2"))
  cat("\n CI upper bound (right border): ", paste0(round(u.cl, 3), "^2"))
}

variance_bounds(x=dance, cl=0.92)


# ++++++++++++++++++++++++ #
# HYPOTHESIS TESTS
# ++++++++++++++++++++++++ #

med <- median(valence)
A <- subset(x=c(dance, valence), subset=valence >= med)
B <- subset(x=c(dance, valence), subset=valence < med)
v.A <- var(A)
v.B <- var(B)
mu.A <- mean(A)
mu.B <- mean(B)

alpha <- 0.03
k <- length(A)
j <- length(B)
s <- sqrt( (v.A/k) + (v.B/j) )
delta <- (mu.A - mu.B)
c <- delta + qnorm(1-alpha) * s
obs <- delta / s

cat("\n var(A) =", round(v.A, 3), "\n var(B) =", round(v.B, 3),
    "\n mean(A) =", round(mu.A, 3), "\n mean(B) =", round(mu.B, 3),
    "\n s(Delta) =", s, "\n Delta =", delta, "\n Observed =", obs, "\ Critical value =", c
)


d <- length(dance)
v <- length(valence)
w <- length(W)
#plot((1:d-1)/(d-1), sort(dance), type="l", main = "Quantiles for Danceability Index", xlab = "Sample Fraction", ylab = "Sample Quantile")
#plot((1:w-1)/(w-1), sort(dance), type="l", main = "Quantiles for random variable W", xlab = "Sample Fraction", ylab = "Sample Quantile")
#plot((1:v-1)/(v-1), sort(dance), type="l", main = "Quantiles for Valence Index", xlab = "Sample Fraction", ylab = "Sample Quantile")

quantiles <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99)

y.norm <- qnorm(p=quantiles, mean=y.mu.mle, sd=y.sd.mle) #Y
y.quant <- quantile(dance, probs=quantiles) #Y
x.gam <- qgamma(p=quantiles, shape=x.shape, rate=x.rate) #X
x.quant <- quantile(valence, probs=quantiles) #X
w.gam <- qgamma(p=quantiles, shape=w.shape, rate=w.rate) #W
w.quant <- quantile(W, probs=quantiles) #W


dance.xlim <- range(0, y.quant)
dance.ylim <- range(0, y.norm)
qqplot(a, b,
xlim = dance.ylim, ylim = dance.ylim,
xlab = "Sample Quantiles",
ylab = "Theoretical Quantiles",
main = "Danceability Index (DI units)")
abline(a=0, b=1, lty="dotted")

valence.xlim <- range(0, 99)
valence.ylim <- range(0, 99)
qqplot(c, d,
xlim = valence.ylim, ylim = valence.ylim,
xlab = "Sample Quantiles",
ylab = "Theoretical Quantiles",
main = "Valence Index (VI units)")
abline(a=0, b=1, lty="dotted")

w.xlim <- range(0, 1)
w.ylim <- range(0, 1)
qqplot(e, f,
xlim = w.ylim, ylim = w.ylim,
xlab = "Sample Quantiles",
ylab = "Theoretical Quantiles",
main = "W (Negative Shift of VI)")
abline(a=0, b=1, lty="dotted")