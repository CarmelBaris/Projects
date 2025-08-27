

ex1q1 <- function(n, prob){
  u <- runif(n) 
  return(ceiling(log(1 - u) / log(1 - prob)))
}


ex1q2 <- function(n, k, th){
    f <- function(x) {
      return((1 + x)^-(k + 1))
    }
    
    max_f <- f(0)
    samples <- numeric(n)
    
    i <- 1
    while(i <= n) {

      x_proposal <- runif(1, min = 0, max = th)
      
      u <- runif(1)
      
      if(u <= f(x_proposal) / max_f) {
        samples[i] <- x_proposal
        i <- i + 1
      }
    }
    
    return(samples)
  }
  
