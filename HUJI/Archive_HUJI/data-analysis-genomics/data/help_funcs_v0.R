getReadLine = function(locations, beg_region, end_region) {
  #' Function for creating one-dimensional vector
  #'  of counts per location aka coverage
  line = rep(0, end_region - beg_region + 1)
  for (loc in locations) {
    if (loc >= beg_region && loc <= end_region) {
      bin_indx = loc - beg_region + 1
      line[bin_indx] = line[bin_indx] + 1
    }
  }
  return(line)
}


countFragmentsPerBin <- function(locations, cell_size, num_cells, start_location) {
  #' Function to count the number of fragments per bin
  bins <- rep(0, num_cells)
  for (loc in locations) {
    if (loc >= start_location && loc < start_location + region_size) {
      bin_index <- ceiling((loc - start_location + 1) / cell_size)
      bins[bin_index] <- bins[bin_index] + 1
    }
  }
  return(bins)
}


#' 
#' countFragmentsPerBin = function (input, size = 20e+03) {
#'   #' Function for binning data
#'   #' @param size how many single units to be aggregated to one bin (default 20K) 
#'   output = numeric(ceiling(length(input) / size))
#'   dat = ceiling(input / size)
#'   for (i in 1: length(dat)) {
#'     output[dat[i]] = output[dat[i]] + 1
#'   }
#'   output
#'   
#' }


countCGInCell <- function(bases, cell_size) {
  base_chunks <- strsplit(bases, '')[[1]]
  length_base_chunks <- length(base_chunks)
  
  num_rows <- ceiling(length_base_chunks / cell_size)
  padded_length <- num_rows * cell_size
  if (padded_length > length_base_chunks) {
    base_chunks <- c(base_chunks, rep("N", padded_length - length_base_chunks))
  }
  
  base_matrix <- matrix(base_chunks, ncol = cell_size, byrow = TRUE)
  cg_counts <- rowSums(base_matrix == "C" | base_matrix == "G")
  
  return(cg_counts)
}


formatLabels <- function(x) {
  #' Function to format the values
  if (x >= 1e6) {return(paste0(round(x / 1e6, 2), "M"))}
  if (x >= 1e3) {return(paste0(round(x / 1e3, 1), "K"))}
  else {return(as.character(x))
  }
}
