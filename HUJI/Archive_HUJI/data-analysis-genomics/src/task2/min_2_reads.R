```{r echo=TRUE}

# Part 2: Histogram excluding x values of 0 and 1
filtered_loc_line = pmin(loc_line,4000) # 4K cutoff


# Exclude x values of 0 and 1
# filtered_loc_line <- pmax(loc_line,1)
# filtered_loc_line <- loc_line[loc_line != 0 & loc_line != 1]
cutoff <- hist(filtered_loc_line, breaks = 0:40 - 0.5, plot = FALSE)
y_lim_cutoff <- c(0, max(cutoff$counts) * 1.1)

# Plot the histogram without 0 and 1 x values
plot(cutoff, ylim = y_lim_cutoff, yaxt = "n", 
     main = "Number of Bins That Have At Least Two Reads",
     xlab = "Total Count of Reads Per Given Bin", ylab = "Count of Bins")

y_ticks_cutoff <- axTicks(2)

formatted_y_ticks_cutoff <- sapply(y_ticks_cutoff, format_labels)

axis(2, at = y_ticks_cutoff, labels = formatted_y_ticks_cutoff)

formatted_counts_cutoff <- sapply(cutoff$counts, format_labels)

text(cutoff$mids,
     cutoff$counts,
     formatted_counts_cutoff,
     pos = 3, cex = 0.5)

```


```{r message=FALSE, warning=FALSE, include=FALSE}
## Binning reads into 10K bp (incl last_read)

#Preparing vector of read counts in bins of 10K
last_read = as.numeric(chr1_reads[nrow(chr1_reads),"Loc"])
N10K = ceiling(last_read/10000)

reads_10K= numeric(N10K)
for (r in 1:nrow(chr1_reads)){
  biN10K = 1+floor((chr1_reads$Loc[r]-1)/10000) 
  reads_10K[biN10K] = reads_10K[biN10K]+1 
}

```
