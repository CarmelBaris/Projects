library('ggplot2')
library('data.table')
library('tidyr')
library('dplyr')
library('tictoc')

reads_file = 'C:/Users/user/Documents/Carmel/Projects/HUJI/Archive_HUJI/data-analysis-genomics/data/TCGA-13-0723-01A_lib2_all_chr1.forward'
chr1_reads = fread(reads_file) 
colnames(chr1_reads) = c("Chrom","Loc","FragLen")

getReadLine = function(locations, beg_region, end_region) {
  line = rep(0, end_region - beg_region + 1)
  for (loc in locations) {
    if (loc >= beg_region && loc <= end_region) {
      line[loc - beg_region + 1] = line[loc - beg_region + 1] + 1
    }
  }
  return(line)
}

locations = chr1_reads$Loc
beg_region = 1
end_region = 2e07

read_line = getReadLine(locations, beg_region, end_region)

line_filtered = ifelse(read_line > 5, 5, read_line)

obs_cnt_singl <- setNames(numeric(7), c(as.character(0:5), "5+"))

obs_cnt_singl[as.character(0:5)] <- table(factor(line_filtered, levels = 0:5))
obs_cnt_singl["5+"] <- sum(read_line > 5)

lambda = mean(read_line)  
Nsingle = length(read_line)

# Calculate probabilities instead of counts
obs_prob_singl <- obs_cnt_singl / Nsingle
exp_pois_upto5 = dpois(0:5, lambda)
exp_pois_plus5 = sum(dpois(6:max(read_line), lambda))
expected_prob = c(exp_pois_upto5, exp_pois_plus5)

format_labels <- function(x) {
  if (x >= 1e6) {return(paste0(round(x / 1e6, 2), "M"))}
  if (x >= 1e3) {return(paste0(round(x / 1e3, 1), "K"))} 
  else {return(as.character(x))
  }
}

obs_prob_singl <- round(obs_prob_singl, 5)
expected_prob <- round(expected_prob, 5)

prob_df <- data.frame(
  pois_val = c("expected", "observed"),
  "0" = c(format_labels(expected_prob[1]), format_labels(obs_prob_singl[1])),
  "1" = c(format_labels(expected_prob[2]), format_labels(obs_prob_singl[2])),
  "2" = c(format_labels(expected_prob[3]), format_labels(obs_prob_singl[3])),
  "3" = c(format_labels(expected_prob[4]), format_labels(obs_prob_singl[4])),
  "4" = c(format_labels(expected_prob[5]), format_labels(obs_prob_singl[5])),
  "5" = c(format_labels(expected_prob[6]), format_labels(obs_prob_singl[6])),
  "5+" = c(format_labels(expected_prob[7]), format_labels(obs_prob_singl[7]))
)

print(prob_df, right = TRUE)

format_values <- function(x) {
  ifelse(x >= 1000000, paste0(round(x / 1000000, 1e6), "M"), as.character(x))
}

comparison_df <- data.frame(
  Count = names(obs_prob_singl),
  Observed = as.numeric(obs_prob_singl),
  Expected = as.numeric(expected_prob)
)

comparison_df_long <- comparison_df %>%
  pivot_longer(cols = c("Observed", "Expected"), names_to = "Type", values_to = "Value")

ggplot(comparison_df_long, aes(x = Count, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red")) +
  labs(title = "Observed vs Expected Poisson Distribution Probabilities",
       y = "Probabilities",
       x = "Read Counts") +
  theme_minimal() +
  scale_y_continuous(labels = format_values)

