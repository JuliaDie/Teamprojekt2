library(magrittr)
library(tidyverse)


setwd("C:/Users/tizma/Google Drive/University/Teamprojekt/Teamprojekt2-master/Utt-Pref-Choice-And-Quiz-master/Submiterator-master")

#read data files
datab1 <- read.csv("teamprojekt_010720-trials.csv",
                   header = TRUE,
                   na.strings = c("", " ", "NA")) 

datab2 <- read.csv("teamprojekt_030720-trials.csv",
                   header = TRUE,
                   na.strings = c("", " ", "NA"))

infob1 <- read.csv("teamprojekt_010720-subject_information.csv",
                   header = TRUE,
                   na.strings = c("", " ", "NA"))

infob2 <- read.csv("teamprojekt_030720-subject_information.csv",
                   header = TRUE,
                   na.strings = c("", " ", "NA"))

answersq1b1 <- read.csv("teamprojekt_010720-answers_quiz1.csv")
answersq2b1 <- read.csv("teamprojekt_010720-answers_quiz2.csv")
answersq3b1 <- read.csv("teamprojekt_010720-answers_quiz3.csv")

answersq1b2 <- read.csv("teamprojekt_030720-answers_quiz1.csv")
answersq2b2 <- read.csv("teamprojekt_030720-answers_quiz2.csv")
answersq3b2 <- read.csv("teamprojekt_030720-answers_quiz3.csv")


timesb1 <- datab1[,c("workerid","Answer.time_in_minutes")]

timesidb2 <- datab2[seq(1,588,12),c("workerid","Answer.time_in_minutes")]

timesb2 <- datab2[seq(1,588,12), "Answer.time_in_minutes"]



#combine trialdata
datab1$workerid <- datab1$workerid+49

trialdata <- rbind(datab2, datab1)


#combine infodata
infob1$workerid <- infob1$workerid+49

infodata <- rbind(infob2,infob1)




#combine quiz answers

answersb1 <- cbind(answersq1b1,  answersq2b1$answers_quiz2,answersq3b1$answers_quiz3)

answersb2 <- cbind(answersq1b2, answersq2b2$answers_quiz2,answersq3b2$answers_quiz3)

#swap columns
answersb2 <- answersb2[,c(2,1,3,4)]

answersb1$workerid <- answersb1$workerid+49

names(answersb1) <- c("workerid", "answers_quiz1", "answers_quiz2", "answers_quiz3")

names(answersb2) <- c("workerid", "answers_quiz1", "answers_quiz2", "answers_quiz3")


#combine quiz answer batches
answers <- rbind(answersb2, answersb1)



#exclude weird times(3,13,14,30,47), non natice english speakers(7 and 44) and confused participants(6,12,49)
excluded <- c(3,13,14,30,38,47,7,44,6,12,49,51,24)

trialdata <- subset(trialdata, ! trialdata$workerid %in% excluded)

infodata <- subset(infodata, !infodata$workerid %in% excluded)

answers <- subset(answers, !answers$workerid %in% excluded)



#Fill trialdata
trialDataFilled <- trialdata %>% fill(guessRank0, .direction = "up") %>% fill(guessRank1, .direction = "up") %>% fill(guessRank2, .direction = "up") %>% fill(simRank0, .direction = "up") %>% fill(simRank1, .direction = "up") %>% fill(simRank2, .direction = "up") %>% fill(evalNum, .direction = "up") %>% fill(certainty, .direction = "up")
trialDataTotal <- trialDataFilled[complete.cases(trialDataFilled[ , "blockNr"]),]


#merge infodata and trialdata
allData <- merge(trialDataTotal, infodata, by = "workerid")

#get rid of unnecessary columns
allDataCleaned <- subset(allData, select = -c(slide_number, trial_type, name))


write.csv(trialdata, "teamprojekt_trialdata.csv")
write.csv(infodata, "teamprojekt_infodata.csv")
write.csv(answers, "teamprojekt_answers.csv")
write.csv(allData, "teamprojekt_allData.csv")
write.csv(allDataCleaned, "teamprojekt_allDataCleaned.csv")