library('ggplot2')
library('data.table')
library('tidyr')
library('dplyr')
library('tictoc')

# Load the data (Group A)
reads_file = '~/Carmel/RProjects/Lab_Benjamini/data/TCGA-13-0723-01A_lib2_all_chr1.forward'

chr1_reads = fread(reads_file) 
colnames(chr1_reads) = c("Chrom","Loc","FragLen")

# Function to get read line
getReadLine = function(locations, beg_region, end_region) {
  line = rep(0, end_region - beg_region + 1)
  for (loc in locations) {
    if (loc >= beg_region && loc <= end_region) {
      line[loc - beg_region + 1] = line[loc - beg_region + 1] + 1
    }
  }
  return(line)
}

## Compare the expected distribution versus the actual distribution

# get read line of actual data
locations = chr1_reads$Loc
beg_region = 1
end_region = 2e07

read_line = getReadLine(locations, beg_region, end_region)

### Observed distribution of the data

## Read Counts dataframe

# Yuval's code
# read_hist = hist(read_line,breaks = 0:40-0.5)
# obs_prob = read_hist$density



# Filter out values for 0-5 and aggregate 5+ as 5+
line_filtered = ifelse(read_line > 5, 5, read_line)

# Initialize observed counts (numeric vector) with names for 0 to 5 and "5+"
obs_cnt_singl <- setNames(numeric(7), c(as.character(0:5), "5+"))

# Calculate counts for 0 to 5
tic()
obs_cnt_singl[as.character(0:5)] <- table(factor(line_filtered, levels = 0:5))
toc()

# Calculate and add the "5+" count
obs_cnt_singl["5+"] <- sum(read_line > 5)

# Round the counts to 2 decimal places and display the result
obs_cnt_singl <- round(obs_cnt_singl, 2)

# observed counts of single reads (unbinned)
# =========================================================================== #
#        0        1        2        3        4        5       5+ 
# 18279693  1610109   103067     6187      581      363      228 
# =========================================================================== #



### Theoretical Poission distribution
# Calculate the expected distribution for a uniform Poisson model


# Mean of observed counts, same as reg_lambda=sum(read_line)/Nsingle
lambda = mean(read_line)  
Nsingle = length(read_line)


# Calculate expected counts for 0-5 and aggregate the rest as 5+
exp_pois_upto5 = dpois(0:5, lambda) * Nsingle
exp_pois_plus5 = sum(dpois(6:max(read_line), lambda)) * Nsingle
expected_counts = c(exp_pois_upto5, exp_pois_plus5)

expected_counts = round(expected_counts, 2) 
names(expected_counts) = c(as.character(0:5), "5+") # add column names

# expected_counts
# =========================================================================== #
#           0           1         2         3        4       5        5+ 
# 18241434.43  1678880.52  77259.27   2370.23    54.54    1.00      0.02 
# =========================================================================== #





### Comparing the counts in a table:

# Function to format the values
format_labels <- function(x) {
  if (x >= 1e6) {return(paste0(round(x / 1e6, 2), "M"))}
  if (x >= 1e3) {return(paste0(round(x / 1e3, 1), "K"))} 
  else {return(as.character(x))
  }
}

# Round the counts to 2 decimal places and display the result
obs_cnt_singl <- round(obs_cnt_singl, 2)
expected_counts <- round(expected_counts, 2)

# Create a data frame
count_df <- data.frame(
  pois_val = c("expected", "observed"),
  "0" = c(format_labels(expected_counts[1]), format_labels(obs_cnt_singl[1])),
  "1" = c(format_labels(expected_counts[2]), format_labels(obs_cnt_singl[2])),
  "2" = c(format_labels(expected_counts[3]), format_labels(obs_cnt_singl[3])),
  "3" = c(format_labels(expected_counts[4]), format_labels(obs_cnt_singl[4])),
  "4" = c(format_labels(expected_counts[5]), format_labels(obs_cnt_singl[5])),
  "5" = c(format_labels(expected_counts[6]), format_labels(obs_cnt_singl[6])),
  "5+" = c(format_labels(expected_counts[7]), format_labels(obs_cnt_singl[7]))
)
print(count_df, right = TRUE)


# Graphical comparison:
# Custom function to format the values
format_values <- function(x) {
  ifelse(x >= 1000000, paste0(round(x / 1000000, 1e6), "M"), as.character(x))
  # ifelse(x >= 1000, paste0(round(x / 1000, 1e3), "K"), as.character(x))
  
}

# Create a data frame for the graphical comparison
comparison_df <- data.frame(
  Count = names(obs_cnt_singl),
  Observed = as.numeric(obs_cnt_singl),
  Expected = as.numeric(expected_counts)
)

# Reshape data to long format using pivot_longer
comparison_df_long <- comparison_df %>%
  pivot_longer(cols = c("Observed", "Expected"), names_to = "Type", values_to = "Value")

# Plot
ggplot(comparison_df_long, aes(x = Count, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red")) +
  labs(title = "Observed vs Expected Poisson Distribution",
       y = "Counts",
       x = "Read Counts") +
  theme_minimal() +
  scale_y_continuous(labels = format_values)
