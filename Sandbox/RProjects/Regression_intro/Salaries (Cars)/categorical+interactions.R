
####  categorical variables ####

# install package containing the dataset
install.packages("car")

# load dataset
salaries <- carData::Salaries
head(salaries)

# extract variable names and types
names(salaries)
str(salaries)

# view dummy coding for the factor 'sex' and 'rank'
contrasts(salaries$sex)
contrasts(salaries$rank)

# frequency table for sex
table(salaries$sex)

## regress y = salary on sex + yrs.service
fm <- lm(salary ~ sex + yrs.service, data = salaries)
summary(fm)

# obtain X matrix
head( model.matrix(fm) )

## add another factor: regress y = salary on sex + rank + yrs.service
# frequency table for rank*sex
table(salaries$rank, salaries$sex)

fm1 <- lm(salary ~ sex + rank + yrs.service, data = salaries)
summary(fm1)$coef
# the general intercept 76612.810 is the 'effective' intercept for feamale assistant professor 
# the 'effective' intercept for a female professor will be 76612.810 + 48980.224


####  interactions  ####
head(salaries)
# regress salary on yrs.service
fm0 <- lm(salary ~ yrs.service, data=salaries)
summary(fm0)
fm0$coefficients
# plot
par(mfrow=c(1,3))

par(mar=c(3.2, 3.2, 4.2, 1.2), mgp=c(1.25, .5, 0), las=0)
plot(salaries$yrs.service, salaries$salary, type='n', ylim = c(85000,150000), xaxt='n', yaxt='n', ylab = 'salary', xlab = 'yrs.service', cex.lab=1.5, main='w/o sex')
abline(fm0, col='black', lwd=1.5)

# regress salary on yrs.service + sex, no interaction
fm1 <- lm(salary ~ yrs.service + sex, data=salaries)
summary(fm1)
fm1$coefficients
plot(salaries$yrs.service, salaries$salary, type='n', ylim = c(85000,150000), xaxt='n', yaxt='n', ylab = 'salary', xlab = 'yrs.service', cex.lab=1.5, main='w/ sex, w/o interaction')
abline(92356.9467, 747.6121, col='black', lwd=1.5)
abline(92356.9467 + 9071.8000, 747.6121, col='black', lwd=1.5)

# regress salary on yrs.service + sex, with interaction
fm2 <- lm(salary ~ yrs.service + sex + yrs.service:sex, data=salaries)
summary(fm2)
fm2$coefficients
plot(salaries$yrs.service, salaries$salary, type='n', ylim = c(85000,150000), xaxt='n', yaxt='n', ylab = 'salary', xlab = 'yrs.service', cex.lab=1.5, main='w/ sex, w/ interaction')
abline(82068.5087 + 20128.6258, 1637.2997-931.7363, lwd=1.5) #males
abline(82068.5087 , 1637.2997, lwd=1.5) #females

