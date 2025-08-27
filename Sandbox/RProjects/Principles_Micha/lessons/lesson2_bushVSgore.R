# they had a complicated voting ballot, and the claim was
# that supporters of Al Gore accidentally voted for a diff candidate, Buchannan

library(UsingR) # when we launch R, run this line to load the UsingR package
data(florida) # the file type is <data> and it isn't yet saved in the workspace
attach(florida) # this line launches the file, and allows searching within it
?view

# politically, we expect a high correlation btw Bush voters & Buchannan voters
cor(BUCHANAN, BUSH)
# the coefficients received: a = 45.3, b=0.0049

# the meaning of b=0.0049
# for each +1 vote to Bush, we get +0.0049 
# for each 10K votes to Bush, we get +0.49 votes to BUCHANAN

# in certain regions, for every 45 votes for Bush, we get +0.49 votes to BUCHANAN

lm(BUCHANAN ~ BUSH)

plot(BUSH, BUCHANAN, col='blue', pch=20, cex=1.5)
abline(45.29, 0.0049, lwd=2)

# possible prediction of the num of votes who persumably went mistakenly to 
# BUCHANAN instead of AL GORE
45.29 + 0.0049*152846
3407-794
sum(BUSH-GORE) # which led Bush to win

# in the regression plot, we notice two extreme outliers
# 1. below the regression line, seems like there's both a deviation from
#    the x-axis (explainable by the size of the county)
#    but also in the y-axis which is weird
# 2. another at ~ (150000, 35000)
#    big deviation in the y-axis 
#    

# OK, let's assume the assumption is correct
# then it should hold true if we apply it to all counties
# but we know that the mean(y.hat) should be equal to mean(y.i.observed)
# and therefore it wouldn't have affected the results!!
# This is the danger of statistics 1.0


# Danger of statistics 2.0
# graph plot till 1996
# fit a regression line to 
# conclusion: in the future, women will run faster than men
# the danger? statisticians can't imply from existing data to the future
#             if 
# such extrapolation ignores the multi-factorial nature of the data
# when the years & enviornment change, the distribution of the parameters change

# Ym = Bm + AmX
# Yf = Bf + AfX
# they claimed that for the same corresponding x value, we can conclude that
# Bm + AmX = Bf + AfX
# => Yf = Yf


