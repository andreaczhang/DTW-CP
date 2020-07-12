# double check the split

library(magrittr)
library(purrr)
library(dplyr)
library(tictoc)
library(caret)


dataPath_rev_sep <- 'path/to/data/sep/'
sep_dinfo <- read.csv(paste0(dataPath_rev_sep, 'cohortInfo_sepAll.csv'))
sep_processed7days <- readRDS(file = paste0(dataPath_rev_sep, 'processed7day_sep.RData'))



# ------ select the proper window ------ # 
# 2 weeks 
below7dayindex <- which(sep_dinfo$dischtime_hours<= 168) 

dinfo7day <- sep_dinfo[below7dayindex, ]

# match the ID, double check outcomes 
all.equal(paste0('icustay_', dinfo7day$icustay_id), 
          names(sep_processed7days))





# --------- partition the static data ----------- # 
id_day1 <- which(dinfo2weeks$dischtime_hours <= 24)
id_day2 <- which(dinfo2weeks$dischtime_hours <= 48)
id_day3 <- which(dinfo2weeks$dischtime_hours <= 72)
id_day4 <- which(dinfo2weeks$dischtime_hours <= 96)
id_day5 <- which(dinfo2weeks$dischtime_hours <= 120)
id_day6 <- which(dinfo2weeks$dischtime_hours <= 144)
id_day7 <- which(dinfo2weeks$dischtime_hours <= 168)

dinfo_day1 <- dinfo2weeks[id_day1, ]
dinfo_day2 <- dinfo2weeks[id_day2, ]
dinfo_day3 <- dinfo2weeks[id_day3, ]
dinfo_day4 <- dinfo2weeks[id_day4, ]
dinfo_day5 <- dinfo2weeks[id_day5, ]
dinfo_day6 <- dinfo2weeks[id_day6, ]
dinfo_day7 <- dinfo2weeks[id_day7, ]






# ----- split into 10 ------ # 
# for each day, create 10 random splits of train and test
# then save the train, test IDs into its own folder 
# this will be used for creating the baseline data too

trteid_day1 <- list()
trteid_day2 <- list()
trteid_day3 <- list()
trteid_day4 <- list()
trteid_day5 <- list()
trteid_day6 <- list()
trteid_day7 <- list()


set.seed(1)
for(s in 1:10){
  trteid_day1[[s]] <- trteIDgenerator_sepsis(outcometable = dinfo_day1)
  trteid_day2[[s]] <- trteIDgenerator_sepsis(outcometable = dinfo_day2)
  trteid_day3[[s]] <- trteIDgenerator_sepsis(outcometable = dinfo_day3)
  trteid_day4[[s]] <- trteIDgenerator_sepsis(outcometable = dinfo_day4)
  trteid_day5[[s]] <- trteIDgenerator_sepsis(outcometable = dinfo_day5)
  trteid_day6[[s]] <- trteIDgenerator_sepsis(outcometable = dinfo_day6)
  trteid_day7[[s]] <- trteIDgenerator_sepsis(outcometable = dinfo_day7)

}


# note that these correspond to their own dinfo_dayx, it is NOT icustay id!
# therefore need to link their icustay id, outcome together
# also keep the dischtime_hours, to check the length of stay


day1_split_sepsis <- map(1:10, function(x){
  matchID_eachsplit(trainID = trteid_day1[[x]]$trainID, 
                    testID = trteid_day1[[x]]$testID, 
                    dinfoDF = dinfo_day1)
})

day2_split_sepsis <- map(1:10, function(x){
  matchID_eachsplit(trainID = trteid_day2[[x]]$trainID, 
                    testID = trteid_day2[[x]]$testID, 
                    dinfoDF = dinfo_day2)
})


day3_split_sepsis <- map(1:10, function(x){
  matchID_eachsplit(trainID = trteid_day3[[x]]$trainID, 
                    testID = trteid_day3[[x]]$testID, 
                    dinfoDF = dinfo_day3)
})


day4_split_sepsis <- map(1:10, function(x){
  matchID_eachsplit(trainID = trteid_day4[[x]]$trainID, 
                    testID = trteid_day4[[x]]$testID, 
                    dinfoDF = dinfo_day4)
})


day5_split_sepsis <- map(1:10, function(x){
  matchID_eachsplit(trainID = trteid_day5[[x]]$trainID, 
                    testID = trteid_day5[[x]]$testID, 
                    dinfoDF = dinfo_day5)
})


day6_split_sepsis <- map(1:10, function(x){
  matchID_eachsplit(trainID = trteid_day6[[x]]$trainID, 
                    testID = trteid_day6[[x]]$testID, 
                    dinfoDF = dinfo_day6)
})


day7_split_sepsis <- map(1:10, function(x){
  matchID_eachsplit(trainID = trteid_day7[[x]]$trainID, 
                    testID = trteid_day7[[x]]$testID, 
                    dinfoDF = dinfo_day7)
})



names(day1_split_sepsis) <- paste0('split', 1:10)
names(day2_split_sepsis) <- paste0('split', 1:10)
names(day3_split_sepsis) <- paste0('split', 1:10)
names(day4_split_sepsis) <- paste0('split', 1:10)
names(day5_split_sepsis) <- paste0('split', 1:10)
names(day6_split_sepsis) <- paste0('split', 1:10)
names(day7_split_sepsis) <- paste0('split', 1:10)


# saveRDS(day1_split_sepsis, paste0(dataPath_rev_sep, 'day1_sep_10splits/alloutcomes_sep.RData'))
# saveRDS(day2_split_sepsis, paste0(dataPath_rev_sep, 'day2_sep_10splits/alloutcomes_sep.RData'))
# saveRDS(day3_split_sepsis, paste0(dataPath_rev_sep, 'day3_sep_10splits/alloutcomes_sep.RData'))
# saveRDS(day4_split_sepsis, paste0(dataPath_rev_sep, 'day4_sep_10splits/alloutcomes_sep.RData'))
# saveRDS(day5_split_sepsis, paste0(dataPath_rev_sep, 'day5_sep_10splits/alloutcomes_sep.RData'))
# saveRDS(day6_split_sepsis, paste0(dataPath_rev_sep, 'day6_sep_10splits/alloutcomes_sep.RData'))
# saveRDS(day7_split_sepsis, paste0(dataPath_rev_sep, 'day7_sep_10splits/alloutcomes_sep.RData'))


# ========== examine the training and test cohort dimensions ======== # 
day1_split_sepsis <- readRDS(paste0(dataPath_rev_sep, 'alloutcomes_sep_day1.RData'))
day2_split_sepsis <- readRDS(paste0(dataPath_rev_sep, 'alloutcomes_sep_day2.RData'))
day3_split_sepsis <- readRDS(paste0(dataPath_rev_sep, 'alloutcomes_sep_day3.RData'))
day4_split_sepsis <- readRDS(paste0(dataPath_rev_sep, 'alloutcomes_sep_day4.RData'))
day5_split_sepsis <- readRDS(paste0(dataPath_rev_sep, 'alloutcomes_sep_day5.RData'))
day6_split_sepsis <- readRDS(paste0(dataPath_rev_sep, 'alloutcomes_sep_day6.RData'))
day7_split_sepsis <- readRDS(paste0(dataPath_rev_sep, 'alloutcomes_sep_day7.RData'))









