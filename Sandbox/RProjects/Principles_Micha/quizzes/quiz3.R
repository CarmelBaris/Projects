
#Q4-Q7 Given distribution describing affect of cigarettes on Blood.Pressure:
Blood.Pressure=c(86,145,198,129,168,90,122,60,142,110,153,110,116,168,181,210,130,70,188,196)
cigarettes=c(4,13,10,10,15,4,9,1,11,6,8,3,8,18,11,7,4,2,19,15)

fit.Blood.Cigar <- lm(formula = Blood.Pressure ~ cigarettes)

# Q4: Find growth coefficient
Blood.Cigar.growth.coefficient <- fit.Blood.Cigar$coefficients[2]

# test.growth.coefficient <- round(cov(cigarettes, Blood.Pressure)/var(cigarettes), 3)

# Q5: Find prediction for x=11
# y.hat = y.predicted = a*x+b

#   first, find intercept coefficient
Blood.Cigar.intercept <- fit.Blood.Cigar$coefficients[1]
#   calculate prediction by running in console:
# Blood.Cigar.growth.coefficient*11 + Blood.Cigar.intercept

Blood.Cigar.stats <- summary(fit.Blood.Cigar)
# Q6: Find Multiple R-Squared
Blood.Cigar.RSquared <- Blood.Cigar.stats$r.squared

#Q7: Find RMSE 

# https://www.investopedia.com/ask/answers/012615/whats-difference-between-rsquared-and-adjusted-rsquared.asp#:~:text=What%20Is%20the%20Difference%20Between,and%20R%2Dsquared%20does%20not.