# keep spliting, now AKI cohort 
# the difference is only that I sample 350/150 control/cases 
# 50 random sets for each day 

library(magrittr)
library(purrr)
library(dplyr)
library(tictoc)
# library(caret)


#### data 
dataPath_rev_aki <- 'path/to/data/aki/'
aki_dinfo <- read.csv(paste0(dataPath_rev_aki, 'cohortInfo_akiAll.csv'))
aki_processed7days <- readRDS(file = paste0(dataPath_rev_aki, 'processed7day_aki.RData'))



# ------ select the proper window ------ # 
# 7 days

below7dayindex <- which(aki_dinfo$dischtime_hours<= 168) 

dinfo7day <- aki_dinfo[below7dayindex, ]
# dinfo7day %>% nrow # 3705

# match the ID, double check outcomes 
all.equal(paste0('icustay_', dinfo7day$icustay_id), 
          names(aki_processed7days))




# --------- partition the static data ----------- # 

id_day1_aki <- which(dinfo7day$dischtime_hours <= 24)
id_day2_aki <- which(dinfo7day$dischtime_hours <= 48)
id_day3_aki <- which(dinfo7day$dischtime_hours <= 72)
id_day4_aki <- which(dinfo7day$dischtime_hours <= 96)
id_day5_aki <- which(dinfo7day$dischtime_hours <= 120)
id_day6_aki <- which(dinfo7day$dischtime_hours <= 144)
id_day7_aki <- which(dinfo7day$dischtime_hours <= 168)


dinfo_day1_aki <- dinfo7day[id_day1_aki, ]
dinfo_day2_aki <- dinfo7day[id_day2_aki, ]
dinfo_day3_aki <- dinfo7day[id_day3_aki, ]
dinfo_day4_aki <- dinfo7day[id_day4_aki, ]
dinfo_day5_aki <- dinfo7day[id_day5_aki, ]
dinfo_day6_aki <- dinfo7day[id_day6_aki, ]
dinfo_day7_aki <- dinfo7day[id_day7_aki, ]




# ----- split into 50 ------ # 
# for each day, query 50 random sets (different from sepsis!)
# use controled outcome (70/30%, 350 alive 150 dead)
# then save the train, test IDs into its own folder 
# this will be used for creating the baseline data too

# tryday1 <- trteIDgenerator_aki(outcometable = dinfo_day1_aki)

trteid_day1_aki <- list()
trteid_day2_aki <- list()
trteid_day3_aki <- list()
trteid_day4_aki <- list()
trteid_day5_aki <- list()
trteid_day6_aki <- list()
trteid_day7_aki <- list()



set.seed(1)
for(s in 1:50){
  # the function here is different from sepsis.
  trteid_day1_aki[[s]] <- trteIDgenerator_aki(outcometable = dinfo_day1_aki)
  trteid_day2_aki[[s]] <- trteIDgenerator_aki(outcometable = dinfo_day2_aki)
  trteid_day3_aki[[s]] <- trteIDgenerator_aki(outcometable = dinfo_day3_aki)
  trteid_day4_aki[[s]] <- trteIDgenerator_aki(outcometable = dinfo_day4_aki)
  trteid_day5_aki[[s]] <- trteIDgenerator_aki(outcometable = dinfo_day5_aki)
  trteid_day6_aki[[s]] <- trteIDgenerator_aki(outcometable = dinfo_day6_aki)
  trteid_day7_aki[[s]] <- trteIDgenerator_aki(outcometable = dinfo_day7_aki)
}


# now link back to the icustay id and outcomes 
# note that the IDs names are slightly different from sepsis
 
day1_split_aki <- map(1:50, function(x){
  matchID_eachsplit(trainID = trteid_day1_aki[[x]]$randindex_tr, 
                    testID = trteid_day1_aki[[x]]$randindex_te,  
                    dinfoDF = dinfo_day1_aki)
})


day2_split_aki <- map(1:50, function(x){
  matchID_eachsplit(trainID = trteid_day2_aki[[x]]$randindex_tr, 
                    testID = trteid_day2_aki[[x]]$randindex_te,  
                    dinfoDF = dinfo_day2_aki)
})


day3_split_aki <- map(1:50, function(x){
  matchID_eachsplit(trainID = trteid_day3_aki[[x]]$randindex_tr, 
                    testID = trteid_day3_aki[[x]]$randindex_te,  
                    dinfoDF = dinfo_day3_aki)
})

day4_split_aki <- map(1:50, function(x){
  matchID_eachsplit(trainID = trteid_day4_aki[[x]]$randindex_tr, 
                    testID = trteid_day4_aki[[x]]$randindex_te,  
                    dinfoDF = dinfo_day4_aki)
})

day5_split_aki <- map(1:50, function(x){
  matchID_eachsplit(trainID = trteid_day5_aki[[x]]$randindex_tr, 
                    testID = trteid_day5_aki[[x]]$randindex_te,  
                    dinfoDF = dinfo_day5_aki)
})

day6_split_aki <- map(1:50, function(x){
  matchID_eachsplit(trainID = trteid_day6_aki[[x]]$randindex_tr, 
                    testID = trteid_day6_aki[[x]]$randindex_te,  
                    dinfoDF = dinfo_day6_aki)
})


day7_split_aki <- map(1:50, function(x){
  matchID_eachsplit(trainID = trteid_day7_aki[[x]]$randindex_tr, 
                    testID = trteid_day7_aki[[x]]$randindex_te,  
                    dinfoDF = dinfo_day7_aki)
})




# ======= sanity check ====== # 

names(day1_split_aki) <- paste0('split', 1:50)
names(day2_split_aki) <- paste0('split', 1:50)
names(day3_split_aki) <- paste0('split', 1:50)
names(day4_split_aki) <- paste0('split', 1:50)
names(day5_split_aki) <- paste0('split', 1:50)
names(day6_split_aki) <- paste0('split', 1:50)
names(day7_split_aki) <- paste0('split', 1:50)


saveRDS(day1_split_aki, paste0(dataPath_rev_aki, 'day1_aki_50splits/alloutcomes_aki_day1.RData'))
saveRDS(day2_split_aki, paste0(dataPath_rev_aki, 'day2_aki_50splits/alloutcomes_aki_day2.RData'))
saveRDS(day3_split_aki, paste0(dataPath_rev_aki, 'day3_aki_50splits/alloutcomes_aki_day3.RData'))
saveRDS(day4_split_aki, paste0(dataPath_rev_aki, 'day4_aki_50splits/alloutcomes_aki_day4.RData'))
saveRDS(day5_split_aki, paste0(dataPath_rev_aki, 'day5_aki_50splits/alloutcomes_aki_day5.RData'))
saveRDS(day6_split_aki, paste0(dataPath_rev_aki, 'day6_aki_50splits/alloutcomes_aki_day6.RData'))
saveRDS(day7_split_aki, paste0(dataPath_rev_aki, 'day7_aki_50splits/alloutcomes_aki_day7.RData'))



