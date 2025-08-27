
ex4q1 <- function(n_sim, n_conv, a, b){
  XY <- matrix(data = NA, nrow = n_sim, ncol = 2)
  X <- rnorm(1, mean = b[1]/a[1], sd = sqrt(1/a[1]))
  for (i in seq(n_sim)) {
    Y <- rnorm(1, mean = b[2]/(X^2 + a[2]), sd = 1/sqrt(X^2 + a[2]))
    X <- rnorm(1, mean = b[1]/(Y^2 + a[1]), sd = 1/sqrt(Y^2 + a[1]))
    XY[i,] <- c(X, Y)
  }
  return(XY)
}

ex4q2 <- function(data, mu, tau, r, lam, n_sim, n_conv){
  M_Y <- matrix(data = NA, nrow = n_sim, ncol = 2)
  X <- 0
  Y <- rgamma(1,r + length(data)/2,lam + (sum(data^2)*0.5))
  for(i in seq(n_conv)){
    X <- rnorm(1,mean=(mu/tau^2 + sum(data)*Y)/(1/tau^2 + length(data)*Y),sd=sqrt(1/(1/tau^ 2 + length(data)*Y)))
    Y <- rgamma(1,r + length(data)/2,lam + 0.5*sum((data-X)^2))
  }
  M_Y[1,] <- c(X, Y)
  for(i in seq(2, n_sim)){
    X <- rnorm(1,mean=(mu/tau^2 + sum(data)*M_Y[i - 1,2])/(1/tau^2 + length(data)*M_Y[i - 1,2]),sd=sqrt(1/(1/tau^ 2 + length(data)*M_Y[i - 1,2])))
    Y <- rgamma(1,r + length(data)/2,lam + 0.5*sum((data - X)^2))
    M_Y[i,] <- c(X, Y)
  }
  return(M_Y)
}
