## SETUP
# Make sure that the following files are located within the current working directory:
#  1. chr1_bases.rda
#  2. TCGA-13-0723-01A_lib2_all_chr1.forward
#  3. helpers_file.rmd


# Load required packages
library('data.table')


# Clean the environment
rm(list = ls())

# Set path
# dir_name = "/Users/alevi/Downloads/"
dir_name = "~/Carmel/RProjects/Lab_Benjamini/data/"
# dir_name = "\\Users\\shawn\\Desktop\\"

# If needed, set working directory to path
# setwd(dir_name)
# getwd() # validate


# Load helper functions
helpers_file = file.path(dir_name, "help_funcs.R")
source(helpers_file)
helpers = list(
  loadRData = loadRData,
  binBases = binBases,
  binReads = binReads
)


## DATA
# We consider the data in terms of bins, as defined by the selected bin size. \n
# This means that there is a 1:1 observation-to-bin ratio. \n
# Vector `chr1_line` holds the sequence of nucleobases `ATCG` (`N` is unknown). \n
# Vector `chr1_reads` maps a location to each read, which starts a sequencing fragment.

# Load data of nucleobases (ATGC)
bases_file = file.path(dir_name, "chr1_line.rda")
chr1_bases = helpers$loadRData(bases_file)

# Load reads data
reads_file_all = file.path(dir_name, "TCGA-13-0723-01A_lib2_all_chr1.forward")
chr1_reads = data.table::fread(reads_file_all)
colnames(chr1_reads) = c("Chrom", "Loc", "FragLen")

# Preprocess data
nreads = nrow(chr1_reads)
myLocations = as.numeric(chr1_reads$Loc)
last_read = myLocations[nreads]
binsize = 20000
bases_20K = helpers$binBases(chr1_bases, last_read, binsize)
GC_20K = bases_20K[, 3] + bases_20K[, 4]
if(any(GC_20K>1)){GC_20K = GC_20K / binsize} # convert GC_20K into percentages
reads_20K = helpers$binReads(myLocations, last_read, binsize)

## remove from memory
rm("chr1_bases","chr1_reads")


# Defining logical vectors for filtering out bins (observations).

# outliers of gc percent per bin
zeros_GC = GC_20K == 0 #bins w/o GC content
small_GC = GC_20K <= 0.3 & GC_20K > 0 #low-GC content bins (<30%)
valid_GC = !(zeros_GC | small_GC) #bins w/ at least 30% GC content

# identify outliers of reads count per bin
z_scores = (reads_20K - mean(reads_20K)) / sd(reads_20K)
xtreme_reads = abs(z_scores) > 2

# create new variables without outliers
outliers = (zeros_GC | small_GC | xtreme_reads)
reads_20K_clean = reads_20K[!outliers]
GC_20K_clean = GC_20K[!outliers]


## Save clean data file
save(reads_20K_clean, GC_20K_clean, file = file.path(dir_name, "reads_gc_20K_cln.rda"))
