## Code for model predictions with parameters set to default ##

############################################################################################
iterative12 <- 1   ###########################################################################
############################################################################################
# 1 iterative 
# 2 non-iterative

#parSetting <- 2
library("dplyr")

source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_UttChoiceOptimization_iterative.R")

# Data file from Ella

x9data = read.csv( 
#                  "X9_Data/ella_total_allDataCleaned.csv", 
                    "X9_Data/ella_coded_data.csv", # File with object codes and order of objects
                   header = TRUE, 
                   na.strings = c("", " ", "NA"))

# adding feature property codes (which feature was uttereed, which features were questioned)
# uttFeat <- ifelse(x9data$utterance=="green" | x9data$utterance=="red" | x9data$utterance=="blue", 3,
#                   ifelse(x9data$utterance=="solid" | x9data$utterance=="striped" | x9data$utterance=="polka-dotted", 2, 1))
# x9data$uttFeat <- uttFeat

targetFeat <- x9data$targetFeatureNum
targetObject <- x9data$targetObject
object2 <- x9data$object2
object3 <- x9data$object3

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x9data$X),3)

for(i in c(1:length(x9data$X))) {
  subjectResponses[i,1] <- x9data$normResponse0[i] + 1e-100
  subjectResponses[i,2] <- x9data$normResponse1[i] + 1e-100
  subjectResponses[i,3] <- x9data$normResponse2[i] + 1e-100
  #  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3]) 
  # Ella already normalized the data
}

## Writing subject responses into a 9 column data table ##

subjectResponsesOrdered <- matrix(NA ,length(x9data$X),9)
for(i in c(1:length(x9data$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(targetFeat[i]-1)*3)] <- subjectResponses[i,j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x9data$workerid
idMax <- max(workerIDs)
llWorkers12 <- matrix(0,length(unique(workerIDs)), 5)
paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 5)

#################################################

# if(iterative12 == 1) {
#   paramsWorkers12 <- as.matrix(read.csv("X9_Data/x9Params_Lambda_indOpt.csv"))
#   llWorkers12 <- as.matrix(read.csv("X9_Data/x9KLDivs_simpleRSA_indOpt_iterative.csv"))
# }else {
#   paramsWorkers12 <- as.matrix(read.csv("X9_Data/x9Params_simpleRSA_indOpt_nonIterative.csv"))
#   llWorkers12 <- as.matrix(read.csv("X9_Data/x9KLDivs_simpleRSA_indOpt_nonIterative.csv"))
# }
# 
# llWorkers12 <- llWorkers12[,c(2:ncol(llWorkers12))]
# paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

### 
# determining the model predictions after worker-specific model parameter optimization!
#constellationCode <- matrix(0,length(x9data$X),6)
#uniqueCCode <- rep(0, length(x9data$X))
#postListMat1Opt <- matrix(0,length(x9data$X),9)
#postListMat2Opt <- matrix(0,length(x9data$X),9)
posteriorUtterances <- matrix(0, length(x9data$X),9)
pickedMostAmbiguous_model <- rep(NA, length(x9data$X))
pickedMostAmbiguous_baseline <- rep(NA, length(x9data$X))
posteriorUtterancesIndependent <- matrix(0, length(x9data$X),9)


logLik <- rep(0,length(x9data$X))

workerID <- -1
utterance <- rep(0, length(x9data$X))
preferences <- matrix(0, length(x9data$X), 9)
for(i in c(1:length(x9data$X))) {
  utterance <- match(as.character(x9data$utterance[i]),allUtterancesNew1) ## polka-dotted!!!
  currentObjects <- c(targetObject[i],object2[i],object3[i]) 
  relevantUtterances <- determineValidUtterances(currentObjects) 
  pickedUtterance <- match(utterance, relevantUtterances)  
  utterancePrior <- rep(0,length(relevantUtterances))
  targetFeature <- targetFeat[i]
  irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
  validUtterances <- relevantUtterances[-irrelevantIndices] 
  utterancePriorShort <- rep (1/length(validUtterances),length(validUtterances)) 
  utterancePrior[-irrelevantIndices] <- utterancePriorShort 

#  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  # uc <- 0
  # for(j in c(1:6)) {
  #   uc <- (uc * 10) + constellationCode[i,j]
  # }
  # uniqueCCode[i] <- uc
  # if(workerID != x9data$workerid[i]) {
  #   workerID <- x9data$workerid[i]
  #   # all three optimized    params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
  #   params1 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(2)]
  #   params2 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(3)]
  #   params12 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(4:5)]
  #   # print(params)
  # }
 
  
    #  if( (i-1)%%4 == 0) {
    #   priorPrefAll_1 <- getPreferencesPrior(x9data[i,"targetFeatureNum"])
    #   priorPrefAll_2 <- getPreferencesPrior(x9data[i,"targetFeatureNum"])
    # } # uniform focussing on the feature type in question.
    
#    if ((i-1)%%4 == 0){
    preferencesPriorAll <- getPreferencesPrior(targetFeature)
#    }  else preferencesPriorAll <- preferences[i-1,]
    ## Calculating model predictions ##
    
    output <- getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll, currentObjects, 0, 0, 1, targetFeature) 
    
    posteriorUtterances[i,relevantUtterances] <- output[[1]] 
    preferences[i,] <- output[[2]][pickedUtterance,,1] 
    
    ##### See if the subjects picked the most ambiguous utterance the model predicts for each trial ####
    mostAmbiguous_model <- which(posteriorUtterances[i,relevantUtterances] == 
                                 max(posteriorUtterances[i,]))
    pickedMostAmbiguous_model[i] <- pickedUtterance %in% mostAmbiguous_model
    
    x9data$pickedMostAmbiguous_model <- pickedMostAmbiguous_model
    
## Now base model predictions. The model picks the most ambiguous utterance
    
    mapObjToU <- determineObjectToUtterancesMapping(currentObjects)
    
    ambiguous <- determineUtteranceToObjectProbabilities(relevantUtterances, currentObjects, mapObjToU, 0)
    ambiguous[irrelevantIndices,] <- NA
   
   ambigCount <- rep(NA, length(relevantUtterances))
   for (row in c(1:length(relevantUtterances))){
     ambigCount[row] <- length(which(ambiguous[row,] > 0))
   }
   mostAmbiguous_baseline <- which(ambigCount == max(ambigCount)) # error here
   pickedMostAmbiguous_baseline[i] <- pickedUtterance %in% mostAmbiguous_baseline
   x9data$pickedMostAmbiguous_baseline <- pickedMostAmbiguous_baseline
   
    # if(iterative12 == 1) {
    #   if(parSetting == 1) {
    #     postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                                0, 0, priorPrefAll_1)
    #     postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                                abs(params12[1]), abs(params12[2]), priorPrefAll_2)
    #   }else if(parSetting == 2) {
    #     postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                                1, 0, priorPrefAll_1)
    #     postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                                0.854056914004583, 1.68201930715193, priorPrefAll_2)
    #   }      
    #   priorPrefAll_1 <- postListMat1Opt[i,]
    #   priorPrefAll_2 <- postListMat2Opt[i,]
    # }else if(iterative12 == 2) {
    #   if(parSetting == 1) {
    #     postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                 0, 0, priorPrefAll_1)
    #     postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                 abs(params12[1]), abs(params12[2]), priorPrefAll_2)
    #   }else if(parSetting == 2) {
    #     postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                                1, 0, priorPrefAll_1)
    #     postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
    #                                                                                0.336897437687373,1.01378644853853, priorPrefAll_2)
    #   
    #   }
    # }
}

###########
## adding all those values to the x9data table.
# subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
# colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
# x9data <- data.frame(x9data, as.data.frame(subjectResponsesOrdered)) 
# 
# postListMat1Opt <- round(postListMat1Opt, digits=5)
# colnames(postListMat1Opt) <- colnames(postListMat1Opt, do.NULL = FALSE, prefix = "Post1_")
# consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1Opt))
# x9data <- data.frame(x9data, consCodeAndPosteriors) 
# 
# postListMat2Opt <- round(postListMat2Opt, digits=5)
# colnames(postListMat2Opt) <- colnames(postListMat2Opt, do.NULL = FALSE, prefix = "Post2_")
# consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2Opt))
# x9data <- data.frame(x9data, consCodeAndPosteriorsNO) 
# 
# x9data$CCode <- uniqueCCode
# x9data$logLik <- logLik
# 
# if(iterative12 == 1) {
#   if(parSetting == 1) {
#     write.csv(x9data, "X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_iterative.csv")
#   }else if(parSetting == 2) {
#     write.csv(x9data, "X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_andOpt12_iterative.csv")
#   }
# }else if(iterative12 == 2) {
#   if(parSetting == 1) {
#     write.csv(x9data, "X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_nonIterative.csv")
#   }else if(parSetting == 2) {
#     write.csv(x9data, "X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_Opt12_nonIterative.csv")
#   }
# }


summary(pickedMostAmbiguous_model)
ambiguity <- x9data %>% group_by(workerid) %>% tally(pickedMostAmbiguous_model)
sort(ambiguity$n)
hist(ambiguity$n)

summary(pickedMostAmbiguous_baseline)
ambiguity <- x9data %>% group_by(workerid) %>% tally(pickedMostAmbiguous_baseline)
sort(ambiguity$n)
hist(ambiguity$n)

##### Testing the baseline model predictions. This model picks the most ambiguous utterance #####

# currentObjects <- c(1,2,6)
# targetFeature <- 1
# mapObjToU <- determineObjectToUtterancesMapping(currentObjects)
# relevantUtterances <- determineValidUtterances(currentObjects)
# irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
# validUtterances <- relevantUtterances[-irrelevantIndices]
# ambiguous <- determineUtteranceToObjectProbabilities(relevantUtterances, currentObjects, mapObjToU, 0)
# ambiguous[irrelevantIndices,] <- NA
# #### 28.03: Something strange happening below
# ambigCount <- rep(NA, length(validUtterances))
# for (row in c(1:length(relevantUtterances))){
#   ambigCount[row] <- length(which(ambiguous[row,] > 0))
# }