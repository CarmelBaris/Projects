source("help_funcs.R")

## sanity tests should return 'true' unless explicitly stated otherwise

## ------------------------------------------------
## ---- SANITY TESTS
## ----------------------------------- <- -------------


## ++++ countBASES()
# 
# prep:
nreads = nrow(chr1_reads)
last_read = chr1_reads$Loc[nreads]
bases_5K = countBases(chr1_line, last_read) 
GC_5K = bases_5K[, 3] + bases_5K[, 4] 
# 
# sanity:
sum(GC_5K) == table(chr1_line)[3] + table(chr1_line)[4]



