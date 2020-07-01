rm(list=ls())

library(magrittr)
library(tidyverse)

inputDataRawThur = read.csv("ella_thursday-trials_adjusted_workerId.csv",
                            header = TRUE,
                            na.strings = c("", " ", "NA"))
reserveDataThur = read.csv("ella_thursday-trials_adjusted_workerId.csv",
                           header = TRUE,
                           na.strings = c("", " ", "NA"))
inputDataRawFri = read.csv("ella_friday-trials.csv",
                           header = TRUE,
                           na.strings = c("", " ", "NA"))
reserveDataFri = read.csv("ella_friday-trials.csv",
                          header = TRUE,
                          na.strings = c("", " ", "NA"))
infoThur = read.csv("ella_thursday-subject_information.csv",
                   header = TRUE,
                   na.strings = c("", " ", "NA"))
systemThur = read.csv("ella_thursday-system.csv",
                     header = TRUE,
                     na.strings = c("", " ", "NA"))
workerThur = read.csv("ella_thursday-workerids.csv",
                     header = TRUE,
                     na.strings = c("", " ", "NA"))
infoFri = read.csv("ella_friday-subject_information.csv",
                        header = TRUE,
                        na.strings = c("", " ", "NA"))
systemFri = read.csv("ella_friday-system.csv",
                    header = TRUE,
                    na.strings = c("", " ", "NA"))
workerFri = read.csv("ella_friday-workerids.csv",
                     header = TRUE,
                     na.strings = c("", " ", "NA"))


# non-native English speakers and confused participants
excluded <- c(13, 31, 41, 47, 85)

workerThur$anon_workerid <- workerThur$anon_workerid+95
systemThur$workerid <- systemThur$workerid+95
infoThur$workerid <- infoThur$workerid+95

systemData <- rbind(systemFri, systemThur)
# systemData <- subset(systemData, ! systemData$workerid %in% excluded)
infoData <- rbind(infoFri, infoThur)
infoData$education <- factor(infoData$education)
#infoData <-  subset(infoData, ! infoData$workerid %in% excluded)
workeridData <- rbind(workerFri, workerThur)
# workeridData <-  subset(workeridData, ! workeridData$anon_workerid %in% excluded)

inputDataRawThur <- subset(inputDataRawThur, select = -X)
inputDataRaw <- rbind(inputDataRawFri, inputDataRawThur)

inputDataFilled <- inputDataRaw %>% fill(guessRank0, .direction = "up") %>% fill(guessRank1, .direction = "up") %>% fill(guessRank2, .direction = "up") %>% fill(simRank0, .direction = "up") %>% fill(simRank1, .direction = "up") %>% fill(simRank2, .direction = "up") %>% fill(evalNum, .direction = "up") %>% fill(certainty, .direction = "up")
inputDataTotal <- inputDataFilled[complete.cases(inputDataFilled[ , "blockNr"]),]

allData <-merge(inputDataTotal, infoData, by="workerid")

allDataCleaned <- subset(allData, select = -c(slide_number, trial_type, name))
# lowerBound <- mean(allDataCleaned$Answer.time_in_minutes)-(2*sd(inputData$Answer.time_in_minutes))

#timeexclude 1st quartile
# dataSummary <- summary(allDataCleaned$Answer.time_in_minutes)
# allDataCleaned <- allDataCleaned[!(allDataCleaned$language == "Urdu" | allDataCleaned$language == "Italian" | allDataCleaned$assess != "Yes" | allDataCleaned$Answer.time_in_minutes < dataSummary[2]),] 

#no time excluded
allDataCleaned <- allDataCleaned[!(allDataCleaned$language == "Urdu" | allDataCleaned$language == "Italian" | allDataCleaned$assess != "Yes"),] 


write.csv(systemData, 'ella_total_systemData.csv')
write.csv(infoData, 'ella_total_infoData.csv')
write.csv(workeridData, 'ella_total_workeridData.csv')
write.csv(inputDataRaw,'ella_total_Raw_trials.csv')
write.csv(inputDataFilled,'ella_total_filled_Raw_trials.csv')
write.csv(inputDataTotal,'ella_total_trials.csv')
write.csv(allData, 'ella_total_allDataMerged.csv')
write.csv(allDataCleaned, 'ella_total_allDataCleaned.csv')
