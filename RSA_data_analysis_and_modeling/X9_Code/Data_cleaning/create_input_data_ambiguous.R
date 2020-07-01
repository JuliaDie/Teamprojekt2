rm(list = ls())
source("Looped_testing.R")

# ambiguousWorkerWOFirstBlock <- subset(ambiguityUsedWOFirstBlock, ambiguityUsedWOFirstBlock[,2]>75)[,1]
# inputDataAmbiguousWOFirstBlock <- subset(inputData, workerid %in% ambiguousWorkerWOFirstBlock)
    
ambiguousWorker <- subset(ambiguityUsed, ambiguityUsed[,2]>75)[,1]
inputDataAmbiguous <- subset(inputData, workerid %in% ambiguousWorker)
inputDataCondensedAmbiguous <- subset(inputDataCondensed, workerid %in% ambiguousWorker)
inputDataCondensedAmbiguousEqual <- subset(inputDataCondensedAmbiguous, evalNum == evalNumModel)
inputDataCondensedEqual <- subset(inputDataCondensed, evalNum == evalNumModel)
# inputDataCondensedAmbiguousCompare <- subset(inputDataCondensedAmbiguous, select = c(normResponse0,preferencesPrior1, normResponse1,preferencesPrior2, normResponse2, preferencesPrior3))
# inputDataCondensedAmbiguousEqualCompare <- subset(inputDataCondensedAmbiguousEqual, select = c(normResponse0,preferencesPrior1, normResponse1,preferencesPrior2, normResponse2, preferencesPrior3))
 
# inputDataCondensedAmbiguousCompare
response0 <- subset(inputDataCondensedAmbiguous, select = c(normResponse0, preferencesPrior1))
response1 <- subset(inputDataCondensedAmbiguous, select = c(normResponse1, preferencesPrior2))
response2 <- subset(inputDataCondensedAmbiguous, select = c(normResponse2, preferencesPrior3))
colnames(response0) <- c("normResponse", "preferencesPrior")
colnames(response1) <- c("normResponse", "preferencesPrior")
colnames(response2) <- c("normResponse", "preferencesPrior")
inputDataCondensedAmbiguousCompare <- rbind(response0, response1, response2)

# inputDataCondensedAmbiguousEqualCompare
response0 <- subset(inputDataCondensedAmbiguousEqual, select = c(normResponse0, preferencesPrior1))
response1 <- subset(inputDataCondensedAmbiguousEqual, select = c(normResponse1, preferencesPrior2))
response2 <- subset(inputDataCondensedAmbiguousEqual, select = c(normResponse2, preferencesPrior3))
colnames(response0) <- c("normResponse", "preferencesPrior")
colnames(response1) <- c("normResponse", "preferencesPrior")
colnames(response2) <- c("normResponse", "preferencesPrior")
inputDataCondensedAmbiguousEqualCompare <- rbind(response0, response1, response2)

# inputDataCondensedCompare
response0 <- subset(inputDataCondensed, select = c(normResponse0, preferencesPrior1))
response1 <- subset(inputDataCondensed, select = c(normResponse1, preferencesPrior2))
response2 <- subset(inputDataCondensed, select = c(normResponse2, preferencesPrior3))
colnames(response0) <- c("normResponse", "preferencesPrior")
colnames(response1) <- c("normResponse", "preferencesPrior")
colnames(response2) <- c("normResponse", "preferencesPrior")
inputDataCondensedCompare <- rbind(response0, response1, response2)

write.csv(inputDataAmbiguous,'ella_total_ambiguous.csv')
write.csv(inputDataCondensedAmbiguous,'ella_condensed_ambiguous.csv')
# write.csv(inputDataAmbiguousWOFirstBlock,'ella_total_ambiguous_wo_first_block.csv')