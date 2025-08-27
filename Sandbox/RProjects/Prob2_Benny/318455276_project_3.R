
ex3q1 = function(n, mu, sig, r){
  l = as.numeric(length(mu))
  Sigma = matrix(data=NA,nrow = length(mu),ncol=length(mu))
  for(i in 1:l){
    for(j in 1:l){
      Sigma[i,j] = sig^2*r^abs(i-j)
    }
  }
  Sig_eigen = eigen(Sigma)
  U = Sig_eigen$vectors
  Lam = diag(sqrt(Sig_eigen$values))
  Sig_root_eigen = U %*% Lam %*% t(U)
  X = matrix(rnorm(n*l),l,n)
  Y = Sig_root_eigen %*% X
  Y = sweep(Y,1,mu,"+")
  return(t(Y))
}

ex3q2 = function(data, statistic, R){
  df = data.frame(t0=statistic(data),t=numeric(R))
  for(i in 1:R){
    df$t[i] = statistic(mvrnorm(n=nrow(data), mu=colMeans(data), Sigma=var(data)))
  }
  return(df)
}
