
## +++++++++ WEEK 3
binReads = function(read_locs, last_read, bin_size) {
  browser()
  last_read = as.numeric(last_read)
  num_bins = ceiling(last_read / bin_size)
  reads_binned = numeric(num_bins)
  for (r in seq_along(read_locs)) {
    binxK = 1 + floor((read_locs[r] - 1) / bin_size)
    reads_binned[binxK] = reads_binned[binxK] + 1
  }
  return(reads_binned)
}

## +++++++++ WEEK 4

binBases = function(ATGC_locs, last_read, bin_size=5000) {
  #' Prepares a matrix of dimension (nbins x 4) s.t. 
  #'   each col is a different letter-base (A,T,C,G)
  #'   each row is a different bin of size 2K letter-bases each
  #'   (col,row) counts occurences of letter-base [...] in bin [...]
  
  N = last_read
  n_bins = ceiling(N / bin_size)
  let_count = matrix(nrow = n_bins, ncol = 4)
  letters = c("A", "T", "C", "G")
  
  for (i in 1:n_bins) {
    start_bin = 1 + (i - 1) * bin_size
    end_bin = i * bin_size
    for (let in 1:4) {
      let_count[i, let] = sum(ATGC_locs[start_bin:end_bin] == letters[let], na.rm = TRUE)
    }
  }
  
  return(let_count)
}

loadRData = function(fileName) {
  #' Helper for loading and returing file for storing as variable:
  #'  > myFileName = loadRData(fileName)
  #'  > myFileName
  #' @param "myFile.RData" or "myFile.rda"
  loaded_objects = NULL  # Initialize an empty object
  load(fileName, envir = environment())  # Load the object(s) into the current environment
  loaded_objects = ls()[ls() != "fileName" & ls() != "loaded_objects"]  # Get the names of loaded objects
  if (length(loaded_objects) == 1) {
    loaded_object = get(loaded_objects)  # Get the loaded object if there's only one
  } else {
    loaded_object = mget(loaded_objects)  # Get all loaded objects as a list
  }
  return(loaded_object)  # Return the loaded object(s)
}


## +++++++++ WEEK 5
fitMetrics = function(actual, predicted) {
  #' computes metrics for evaluating goodness of fit and prediction
  r_sqr = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  mae = mean(abs(actual - predicted))
  mse = mean((actual - predicted)^2)
  mspe = mean((actual - predicted)^2)
  return(list(R_sqr = round(r_sqr, 2),
              MAE = round(mae, 2),
              MSE = round(mse, 2),
              MPSE = round(mspe, 2) ## @need to update, rn same as MSE...
  ))
}

check_proportions = function(train, test, expected_train_ratio) {
  total = length(train) + length(test)
  train_ratio = length(train) / total
  test_ratio = length(test) / total
  list(train_ratio = train_ratio, test_ratio = test_ratio, expected_train_ratio = expected_train_ratio)
}

check_overlaps = function(train_indices, test_indices) {
  overlaps = intersect(train_indices, test_indices)
  length(overlaps) == 0  # Should be TRUE if there are no overlaps
}

check_data_size = function(train, test, original_data) {
  total_train_test = length(train) + length(test)
  original_length = length(original_data)
  total_train_test == original_length  # Should be TRUE if the data size is maintained
}

fit_bspline = function(train_data, test_data, knots) {
  
  n = length(train_data)
  
  x_train = 1:n
  
  x_test = (n + 1):(n + length(test_data))
  
  
  
  # Standardize x values
  
  x_train_scaled = scale(x_train)
  
  x_test_scaled = scale(x_test, 
                        
                        center = attr(x_train_scaled, "scaled:center"), 
                        
                        scale = attr(x_train_scaled, "scaled:scale"))
  
  
  
  # Fit b-spline regression model
  
  spline_model = lm(train_data ~ bs(x_train_scaled, knots = knots))
  
  
  
  # Predict on test data
  
  pred_train = predict(spline_model, 
                       
                       newdata = data.frame(x_train_scaled = x_train_scaled))
  
  pred_test = predict(spline_model, 
                      
                      newdata = data.frame(x_train_scaled = x_test_scaled))
  
  
  
  # Calculate fit metrics
  
  metrics_train = fitMetrics(train_data, pred_train)
  
  metrics_test = fitMetrics(test_data, pred_test)
  
  
  
  list(model = spline_model, pred_train = pred_train, pred_test = pred_test, 
       
       metrics_train = metrics_train, metrics_test = metrics_test)
  
}



plot_bspline_results = function(train_data, test_data, pred_train, pred_test, title) {
  
  n = length(train_data)
  
  x_train = 1:n
  
  x_test = (n + 1):(n + length(test_data))
  
  
  
  plot(1:(length(train_data) + length(test_data)), c(train_data, test_data), 
       
       type = 'n', main = title, xlab = "Index", ylab = "Read Counts", 
       
       ylim = range(c(train_data, test_data, pred_train, pred_test), na.rm = TRUE))
  
  lines(x_train, train_data, col = 'blue', lwd = 2)
  
  lines(x_test, test_data, col = 'green', lwd = 2)
  
  lines(x_train, pred_train, col = 'red', lwd = 2, lty = 2)
  
  lines(x_test, pred_test, col = 'orange', lwd = 2, lty = 2)
  
  legend("topright", lty = c(1, 1, 2, 2), lwd = 2, 
         
         legend = c("Train Data", "Test Data", "pred Train", "pred Test"), 
         
         col = c("blue", "green", "red", "orange")
         
  )}
