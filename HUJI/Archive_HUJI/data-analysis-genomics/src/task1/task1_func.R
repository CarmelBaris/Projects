subSnippet <- function(start_indx=1, cell_size=1000){
  #' Retrieves a subsection of Chromosome 1 from reference genome.
  #' Calculates number of occurrences for each molecular base (A, T, C, G). 
  #' Nucleobases that their decoding is not certain appear in lowercase.
  #' Unknown nucleobases appear as N.
  #'
  #' @param start_indx location of first base in region of interest
  #' @param cell_size number of nucleobases included in analysis region
  
  beg_region = start_indx
  end_region = beg_region + cell_size - 1
  #  chr1_list = unlist(strsplit(chr1_str_10M_30M, split = ""))
  chr1_snippet = chr1_vector[beg_region:end_region]
  return(list(occurrences_table = table(chr1_snippet),
              chr1_snippet = chr1_snippet))
}

