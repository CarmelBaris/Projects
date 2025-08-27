source("C:/Users/user/Documents/Carmel/RProjects/Principles_Micha/submissions/paper2 - inferential stats/spotify_data_cleanup.R")

# ++++++++++++++++++++++++ #
# DICTIONARY
# ++++++++++++++++++++++++ #

# X valence
# Y dance, explained by changes in X
m <- min(dance)
W <- dance - m

# ++++++++++++++++++++++++ #
# GIVEN ASSUMPTIONS
# ++++++++++++++++++++++++ #

# X = W + min(dance), and because linear transformation preserves the distribution model:
# X~Gamma(α x.shape, λ x.rate)
# Y~N(μ, σ^2)
# W~Gamma(w.shape, w.rate)

# ++++++++++++++++++++++++ #
# BASIC MEASURES
# ++++++++++++++++++++++++ #

#standard deviations
sd_dance <- sd(dance)
sd_valence <- sd(valence)

# ++++++++++++++++++++++++ #
# POINT ESTIMATORS
# ++++++++++++++++++++++++ #

# Y dance
mu.y <- mean(dance)
sigma.squared.y <- var(dance)

# W = valence + min(valence)
lambda.w <- mean(W)/(mean(W^2)-(mean(W))^2)
# lambda.w.alternative <- mean(W)/var(W)
alpha.w <- (mean(W))^2/(mean(W^2)-(mean(W))^2)
# alpha.w.alternative <- (mean(W))^2/var(W)

# X valence
theta.x <- mean(valence)/(mean(valence^2)-(mean(valence))^2)
beta.x <- (mean(valence))^2/(mean(valence^2)-(mean(valence))^2)

# X valence
theta.x.alternative <- (mean(W)+m)/(mean(W^2)-(mean(W))^2)
beta.x.alternative <- mean(W)*theta.x.alternative + m*theta.x.alternative




