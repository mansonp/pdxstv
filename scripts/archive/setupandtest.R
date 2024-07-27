library(pacman)
p_load(vote, dplyr)

# Reference: https://journal.r-project.org/archive/2021/RJ-2021-086/RJ-2021-086.pdf


data("food_election")


# Plurality
random_plurality <- 1 * (randomballot == 1 & !is.na(randomballot))

plurality(random_plurality)
invalid.votes(plurality(random_plurality))

stv(random_plurality, nseats=3)


# with the bias sample

data_plurality <- 1 * (dataedit == 1 & !is.na(dataedit))

plurality(data_plurality)

biasedrandom <- dataedit %>% select(-BallotID, -Precinct)
stv(biasedrandom, nseats=3)

stv.biased <- stv(biasedrandom, nseats=3, eps = 1, digits = 0, constant.quota = TRUE) # Note the quota can adapt to exhaustion

plot(stv.biased)
image (stv.biased, all.pref = TRUE)
image (stv.biased, proportion=FALSE)


# With Simulated Data

rankings_df_plural <- 1 * (rankings_df == 1 & !is.na(rankings_df))
plurality(rankings_df_plural)

rankings_df_processed <- rankings_df %>% select(-id)
stv(rankings_df_processed, nseats=3, constant.quota = TRUE)
