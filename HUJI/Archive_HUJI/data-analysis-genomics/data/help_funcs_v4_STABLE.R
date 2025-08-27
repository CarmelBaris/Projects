## +++++++++ LECTURE 3
binReads = function(read_locs, last_read, bin_size) {
  last_read = as.numeric(last_read)
  num_bins = ceiling(last_read / bin_size)
  reads_binned = numeric(num_bins)
  for (i in seq_along(read_locs)) {
    bin_i = 1 + floor((read_locs[i] - 1) / bin_size)
    reads_binned[bin_i] = reads_binned[bin_i] + 1
  }
  return(reads_binned)
}



## +++++++++ LECTURE 4
binBases = function(ATGC_locs, last_read, bin_size=5000) {
  #' Prepares a matrix of dimensions (num_bins x 4) s.t. 
  #'   each col is a different letter-base (A,T,C,G)
  #'   each row is a different bin of letter-bases
  #'   (col,row) counts occurrences of letter-base [...] in bin [...]
  
  n_bins = ceiling(last_read / bin_size)
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

## +++++++++ COURTESY OF THE WWW COMMUNITY
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

loadPacks = function(my_packs) {
  #' Helper for installing & loading packages
  for (somepackage in my_packs) {
    somepackage_text = as.character(somepackage)
    if (!require(somepackage_text, character.only = TRUE)) {
      install.packages(somepackage_text)
      library(somepackage_text, character.only = TRUE)
    }
  }
}

removeOutliersPaired = function(x, y) {
  q1_x = quantile(x, 0.25)
  q3_x = quantile(x, 0.75)
  iqr_x = q3_x - q1_x
  lower_bound_x = q1_x - 1.5 * iqr_x
  upper_bound_x = q3_x + 1.5 * iqr_x
  
  q1_y = quantile(y, 0.25)
  q3_y = quantile(y, 0.75)
  iqr_y = q3_y - q1_y
  lower_bound_y = q1_y - 1.5 * iqr_y
  upper_bound_y = q3_y + 1.5 * iqr_y
  
  mask = (x > lower_bound_x & x < upper_bound_x) & (y > lower_bound_y & y < upper_bound_y)
  return(list(x = x[mask], y = y[mask]))
}

# Function to compute metrics for evaluating goodness of fit and prediction
fitMetrics = function(actual, predicted) {
  r_sqr = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  mae = mean(abs(actual - predicted))
  mse = mean((actual - predicted)^2)
  mspe = mean((actual - predicted)^2)
  return(list(R_sqr = round(r_sqr, 2),
              MAE = round(mae, 2),
              MSE = round(mse, 2),
  ))
}


# Function to mimic createDataPartition from `caret` package
# splits data into train & test sets, and preserves distribution of target var. 
createDataPartition = function(y, times = 1, p = 0.5, list = TRUE,
                               groups = min(5, length(y))) {
  if (length(y) < 2) stop("y must have at least 2 elements")
  if (groups < 1) stop("groups must be greater than 0")
  
  # Determine the number of rows in each group
  cuts = cut(y, breaks = unique(quantile(y, probs = seq(0, 1, length = groups))), include.lowest = TRUE)
  
  # Create partitions
  out = vector("list", times)
  for (j in 1:times) {
    trainIndices = numeric(0)
    testIndices = numeric(0)
    for (lvl in levels(cuts)) {
      lvlIndices = which(cuts == lvl)
      size = floor(length(lvlIndices) * p)
      if (size == 0) size = 1 # Ensure at least one element in train set
      sampled = sample(lvlIndices, size)
      trainIndices = c(trainIndices, sampled)
      testIndices = c(testIndices, setdiff(lvlIndices, sampled))
    }
    if (list) {
      out[[j]] = list(train = trainIndices, test = testIndices)
    } else {
      out[[j]] = data.frame(Resample1 = trainIndices)
    }
  }
  if (!list && times > 1) {
    out = do.call(cbind, out)
  }
  return(out)
}



## +++++++++ LECTURE 3
binStuff = function(input, size = 20e+03){
  #' @param size num of single units to be binned as one (default 20K)
  #' 
  output = numeric(ceiling(length(input)/size) )
  dat = ceiling(input / size)
  for (i in 1:length(dat)){
    output[dat[i]] = output[dat[i]] + 1
  }
  output
  
}


getReadLine = function(read_locs, beg_region, end_region) {
  #' Function for creating one-dimensional vector of coverage. 
  #' Per location, counts fragments s.t their start_read begins there.
  line = rep(0, end_region - beg_region + 1)
  for (loc in read_locs) {
    if (loc >= beg_region && loc <= end_region) {
      line[loc - beg_region + 1] = line[loc - beg_region + 1] + 1
    }
  }
  return(line)
}


read_count_region = function(reads_file, beg_region, end_region) {
  chr1_reads = data.table::fread(reads_file)
  colnames(chr1_reads) = c("Chrom", "Loc", "FragLen")
  
  N = end_region - beg_region + 1
  read_line = numeric(N)
  
  first_read = min(which(chr1_reads$Loc >= beg_region))
  r = first_read
  while (chr1_reads$Loc[r] <= end_region) {
    read_line[chr1_reads$Loc[r] - beg_region + 1] = read_line[chr1_reads$Loc[r] - beg_region + 1] + 1
    r = r + 1
  }
  return(read_line)
}




