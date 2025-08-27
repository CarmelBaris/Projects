set.seed(seed=2)
n=100000
groups=100
x=runif(n,0,1)
OBS = as.vector(as.table(table(cut(x,breaks = 100))))
T.obs = sum((OBS-(n/groups))^2/(n/groups))
T.obs
T = qchisq(0.95,groups-1)
'TRUE - reject, False - not reject'
c(T<T.obs)
