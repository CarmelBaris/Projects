# for calculating P[Q>q], set lower.tail to FALSE
# otherwise, calculates P[Q≤q]

?pnorm

#Q1
# x~N(5,10^2), find P(X>2) assuming even dist s.t. mean==expectancy
Q1 <- pnorm(q=2, mean=5, sd=10, lower.tail=FALSE)


#Q2
# x~Pois(5), find P(x≤2)
Q2 <- ppois(q = 2, lambda = 5) # P[Q≤q]

#Q3
# x~exp(4.5), find P(X>(2/4.5))
Q3 <- pexp(q = 2/4.5, rate = 4.5, lower.tail = FALSE) # P[Q>q]

# data for Q5 till Q9
lengths_mm = c(17, 3, 10, 9, 6, 13, 18, 5, 5)

#Q4
c <- integer()
?dexp
f_Q1 <- function(t){(31/7)*dexp(t, rate=7)}
integrate(f1, -Inf, Inf)
#https://stackoverflow.com/questions/66577481/newton-raphson-function-in-r-eli5/66578139#66578139

#Q5, Q6, Q7
Q5 <- mean(lengths_mm)
Q6 <- median(lengths_mm)
Q7 <- sd(lengths_mm)

#Q8, Q9
Q8 <- Q5/10 #linear transformation of mean
Q9 <- Q7/10 #linear transformation of sd

#Q10
observation = 9
mean = 24
sd = 10
Q10 <- (observation-mean)/sd

#Q11
sample_lens_mm_q11 <- c(3 , 10 , 9 , 6 , 13 , 18 , 5 , 5)
req_quantiles_q11 <- c(0.1,0.7)
quantile(sample_lens_mm_q11, req_quantiles_q11)

#Q12
dat_q12 <- c(-0.52,  3.92,  2.45,  9.21,  3.82,  5.23, -1.07,  8.00, -2.78,  0.48,  3.63, 3.67,  1.59,  8.18,  3.86,
             
             2.79,  0.28,  6.58, -3.40, 19.17, -0.07,  8.35,  4.83,  8.41, -2.70, -0.07, -2.04,  4.62, -5.10,  4.73)

hist(dat_q12, breaks = 5)

