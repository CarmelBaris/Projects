
## +++++++++ COURTESY OF THE WWW COMMUNITY
loadRData = function(fileName) {
  #' Helper for loading and returing file for storing as variable:
  #'  > myFileName = loadRData(fileName)
  #'  > myFileName
  #' @param "myFile.RData" or "myFile.rda"
  loaded_objects <- NULL  # Initialize an empty object
  load(fileName, envir = environment())  # Load the object(s) into the current environment
  loaded_objects <- ls()[ls() != "fileName" & ls() != "loaded_objects"]  # Get the names of loaded objects
  if (length(loaded_objects) == 1) {
    loaded_object <- get(loaded_objects)  # Get the loaded object if there's only one
  } else {
    loaded_object <- mget(loaded_objects)  # Get all loaded objects as a list
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


getReadLine = function(locations, beg_region, end_region) {
  #' Function for creating one-dimensional vector of coverage. 
  #' Per location, counts fragments s.t their start_read begins there.
  line = rep(0, end_region - beg_region + 1)
  for (loc in locations) {
    if (loc >= beg_region && loc <= end_region) {
      line[loc - beg_region + 1] = line[loc - beg_region + 1] + 1
    }
  }
  return(line)
}


read_count_region <- function(reads_file, beg_region, end_region) {
  chr1_reads <- data.table::fread(reads_file)
  colnames(chr1_reads) <- c("Chrom", "Loc", "FragLen")
  
  N <- end_region - beg_region + 1
  read_line <- numeric(N)
  
  first_read <- min(which(chr1_reads$Loc >= beg_region))
  r <- first_read
  while (chr1_reads$Loc[r] <= end_region) {
    read_line[chr1_reads$Loc[r] - beg_region + 1] <- read_line[chr1_reads$Loc[r] - beg_region + 1] + 1
    r <- r + 1
  }
  return(read_line)
}

binReads <- function(locations, xbin_size, last_read) {
  last_read <- as.numeric(last_read)
  NxK <- ceiling(last_read / xbin_size)
  reads_xK <- numeric(NxK)
  for (r in seq_along(locations)) {
    binxK <- 1 + floor((locations[r] - 1) / xbin_size)
    reads_xK[binxK] <- reads_xK[binxK] + 1
  }
  return(reads_xK)
}



## +++++++++ LECTURE 4
binBases = function(chr1_line, last_read, bin_size=5000) {
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
      let_count[i, let] = sum(chr1_line[start_bin:end_bin] == letters[let], na.rm = TRUE)
    }
  }
  
  return(let_count)
}

