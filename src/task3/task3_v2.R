library('ggplot2')
library('data.table')
library('tidyr')
library('dplyr')
library('tictoc')

reads_file = '~/Carmel/RProjects/Lab_Benjamini/data/TCGA-13-0723-01A_lib2_all_chr1.forward'
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

obs_cnt_singl <- round(obs_cnt_singl, 2)



lambda = mean(read_line)  
Nsingle = length(read_line)


exp_pois_upto5 = dpois(0:5, lambda) * Nsingle
exp_pois_plus5 = sum(dpois(6:max(read_line), lambda)) * Nsingle
expected_counts = c(exp_pois_upto5, exp_pois_plus5)

expected_counts = round(expected_counts, 2) 
names(expected_counts) = c(as.character(0:5), "5+") # add column names


format_labels <- function(x) {
  if (x >= 1e6) {return(paste0(round(x / 1e6, 2), "M"))}
  if (x >= 1e3) {return(paste0(round(x / 1e3, 1), "K"))} 
  else {return(as.character(x))
  }
}

obs_cnt_singl <- round(obs_cnt_singl, 2)
expected_counts <- round(expected_counts, 2)

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


format_values <- function(x) {
  ifelse(x >= 1000000, paste0(round(x / 1000000, 1e6), "M"), as.character(x))
  # ifelse(x >= 1000, paste0(round(x / 1000, 1e3), "K"), as.character(x))
  
}

comparison_df <- data.frame(
  Count = names(obs_cnt_singl),
  Observed = as.numeric(obs_cnt_singl),
  Expected = as.numeric(expected_counts)
)

comparison_df_long <- comparison_df %>%
  pivot_longer(cols = c("Observed", "Expected"), names_to = "Type", values_to = "Value")

ggplot(comparison_df_long, aes(x = Count, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red")) +
  labs(title = "Observed vs Expected Poisson Distribution",
       y = "Counts",
       x = "Read Counts") +
  theme_minimal() +
  scale_y_continuous(labels = format_values)
