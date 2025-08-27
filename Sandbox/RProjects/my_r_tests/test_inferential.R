source("C:/Users/user/Documents/Carmel/RProjects/my_r_funcs/inferential.R")

# +++++++++++++ EXAMPLE RUNS +++++++++++++
# Answer to question 1:
sampleSize(conf.prcnt=0.9, sd=0.5, L0=0.19)

# Answer to question 2:
sampleSize(conf.prcnt=0.95, sd=3.06, L0=0.5)

# Answer to question 3:
ciExpectancy(conf.prcnt = 0.8, mode="LEN",
             sd.x = 16, n.x = 58)

# Answer to question 4:
ciExpectancy(conf.prcnt = 0.94,
             avg.x = 58.6, sd.x = 7, n.x = 60,
             avg.y = 54.6, sd.y = 16, n.y = 58)

# Answer to question 5:
ciExpectancy(conf.prcnt = .9, is.var.est = "UN",
             avg.x = 155, sd.x = sqrt(261), n.x = 12)

# Answer to question 6:
ciExpectancy(conf.prcnt = .95, mode = "UP", is.var.est = "UN",
             avg.x = 155, sd.x = sqrt(261), n.x = 12,
             avg.y = 455, sd.y = sqrt(79), n.y = 12)

ciExpectancy(conf.prcnt = .95, mode = "LO", is.var.est = 1,
             avg.x = 11100, sd.x = sqrt(1500), n.x = 100,
             avg.y = 7500, sd.y = sqrt(800), n.y = 150)
# Answer to question 6:
