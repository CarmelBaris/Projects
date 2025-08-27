
# Question from tirgul 4:

samp<-c(55,65,72,73,56,42,72,57,46,77,58,37,38,31,80,31,79,35,51,58,39,54,40,39,36,
        78,61,79,76,46,37,51,53,70,62,59,52,63,45,60,32,43,47,77,46,77,46,35,79,61)
# Moments:
mue.hat<-mean(samp)
sig2.hat<-mean(samp^2)-(mean(samp))^2

# compute p(x>48):
z<-(48-mue.hat)/sqrt(sig2.hat)

1-pnorm(z)






