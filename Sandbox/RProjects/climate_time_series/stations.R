
## Path & Data
setwd("/Users/user/Documents/Carmel/RProjects/climate_time_series")
a_to_m <- read.csv("./data/precipitation_comb_stations_a_m.csv")
n_to_z <- read.csv("./data/precipitation_comb_stations_n_z.csv")

combined_data = rbind(a_to_m, n_to_z)
combined_data <- combined_data[order(combined_data$Station, combined_data$Year, combined_data$Month, combined_data$Day),]

stations = unique(combined_data$Station)

