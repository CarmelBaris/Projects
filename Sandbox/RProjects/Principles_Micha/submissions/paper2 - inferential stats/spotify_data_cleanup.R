# ++++++++++++++++++++++++ #
# PREPARING THE DATA
# ++++++++++++++++++++++++ #

#read data
songs <- read.csv("/Users/user/Documents/Carmel/RProjects/Principles_Micha/data/mine/spotify1921to2020.csv")
dance.origin <- songs$danceability
valence.origin <- songs$valence

#before cleanup
total.origin <- length(dance.origin)

#only danceable songs from years 2019 and 2020
songs.new <- subset(x=songs, subset=(dance.origin>0 #danceable
           & songs$year > 2018 & songs$year < 2021)) #2019 to 2020
two_years <- length(songs.new)
dance <- songs.new$danceability
valence <- songs.new$valence
removed <- total.origin - length(dance) # number of songs removed
