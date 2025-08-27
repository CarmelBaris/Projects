

ex2q1 = function(n){
  coordinate_pairs = matrix(NA,n,2)
  colnames(coordinate_pairs) = c("X","Y")
  
  for(pair_index in seq(n)){
    valid_pair = FALSE
    while(!valid_pair){
      pair_attempt = runif(2,0,1)
      if(sum(pair_attempt^2)<1){
        random_check_value = runif(1)
        valid_pair = (random_check_value<=sqrt(1-sum(pair_attempt^2)))
      }
    }
    coordinate_pairs[pair_index,] = pair_attempt
  }
  return(data.frame(coordinate_pairs))
}

ex2q2 = function(alpha, n.copy){
  sample_values = sapply(1:n.copy, function(sample_index){
    random_values = runif(length(alpha))
    sample_value = mean(sin(sum(random_values*alpha)))
    return(sample_value)
  }) 
  standard_error = sd(sample_values)/sqrt(length(alpha))
  return(list("value" = mean(sample_values), "error" = standard_error))
}
