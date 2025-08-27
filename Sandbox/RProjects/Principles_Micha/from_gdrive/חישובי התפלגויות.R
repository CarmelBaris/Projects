'חישובי התפלגויות'
'ber(p) - p(X=0/1) --> x=0/1,n=1,p=0.5'
dbinom(7,10,0.5)
'bin(n,p) - p(X=x) --> x=8,n=10,p=0.5'
dbinom(2,3,0.7)+dbinom(3,3,0.7)
'geo(p) - p(X=x) --> x=8,p=0.5'
dgeom(6,0.1)
'pois(p) - p(X=x) --> x=2,p=0.5'
dpois(2,0.5)
'exp(p) - p(X=x) --> x=2,p=0.5'
dexp(2,0.5)
'תכונת חוסר הזיכרון exp(p) - p(X>x) --> x=2,p=0.5'
dexp(2,0.5)/0.5
'norm(p) - p(X=x) --> x=5,mean=6,sd=4 (variance=4^2)'
dnorm(5,6,4)
'LogNorm(p) - p(X=x) --> x=5000,mean=10,sd=2'
qlnorm(0.9,10,2)
'chiSqured(p) --> p=1-(alpha=0.05)=0.95,df=k-1'
qchisq(0.95,3)