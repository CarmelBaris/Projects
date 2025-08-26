
## ++++++++++++++ help_funcs.R


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

binBases = function(ATGC_locs, last_read, bin_size) {
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

