############################################################################################
iterative12 <- 2   ###########################################################################
############################################################################################
# 1 iterative 
# 2 non-iterative

parSetting <- 2

source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_StratUttOptimization_iterative.R") 

# Data file from Ella
x9data = read.csv(
  "X9_Data/ella_total_allDataCleaned.csv",
  header = TRUE,
  na.strings = c("", " ", "NA")
)

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x9data$utterance=="green" | x9data$utterance=="red" | x9data$utterance=="blue", 3,
                  ifelse(x9data$utterance=="solid" | x9data$utterance=="striped" | x9data$utterance=="polka-dotted", 2, 1))
x9data$uttFeat <- uttFeat
targetFeat <- x9data$targetFeatureNum

## adding the 1-27 target and object1, object2 & object3 code.
temp <- x9data$simulatedAnswer
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$targetOC27 <- targetOC27

temp <- x9data$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj1OC27 <- obj1OC27

temp <- x9data$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj2OC27 <- obj2OC27

temp <- x9data$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj3OC27 <- obj3OC27

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x9data$X),3)

for(i in c(1:length(x9data$X))) {
  subjectResponses[i,1] <- x9data$normResponse0[i] + 1e-100
  subjectResponses[i,2] <- x9data$normResponse1[i] + 1e-100
  subjectResponses[i,3] <- x9data$normResponse2[i] + 1e-100
  #  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3]) # Ella already normalized the data
}

## ordering the recorded subject responses such that they can be compared directly 
#   to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x9data$X),9)
for(i in c(1:length(x9data$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(targetFeat[i]-1)*3)] <- subjectResponses[i,j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)

## Reordering objects in input data

targetObject <- rep(NA, length(x9data$X))
object2 <- rep(NA, length(x9data$X))
object3 <- rep(NA, length(x9data$X))

for (i in 1:length(x9data$X)){
  if(targetOC27[i] == obj1OC27[i]){
    targetObject[i] <- targetOC27[i]
    object2[i] <- obj2OC27[i]
    object3[i] <- obj3OC27[i]
  } else if (targetOC27[i] == obj2OC27[i]) 
  {targetObject[i] <- obj2OC27[i]
  object2[i] <- obj1OC27[i]
  object3[i] <- obj3OC27[i]
  } else {
    targetObject[i] <- obj3OC27[i]
    object2[i] <- obj1OC27[i]
    object3[i] <- obj2OC27[i]
  }
}  

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x9data$workerid
idMax <- max(workerIDs)
llWorkers12 <- matrix(0,length(unique(workerIDs)), 5)
paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 5)

##########


## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x9data$workerid
idMax <- max(workerIDs)

#################################################

if(iterative12 == 1) {
  paramsWorkers12 <- as.matrix(read.csv("X9_Data/x9Params_simpleRSA_indOpt_iterative.csv"))
  llWorkers12 <- as.matrix(read.csv("X9_Data/x9KLDivs_simpleRSA_indOpt_iterative.csv"))
}else {
  paramsWorkers12 <- as.matrix(read.csv("X9_Data/x9Params_simpleRSA_indOpt_nonIterative.csv"))
  llWorkers12 <- as.matrix(read.csv("X9_Data/x9KLDivs_simpleRSA_indOpt_nonIterative.csv"))
}

llWorkers12 <- llWorkers12[,c(2:ncol(llWorkers12))]
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]





### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x9data$X),6)
uniqueCCode <- rep(0, length(x9data$X))
postListMat1Opt <- matrix(0,length(x9data$X),9)
postListMat2Opt <- matrix(0,length(x9data$X),9)

logLik <- rep(0,length(x9data$X))
workerID <- -1
for(i in c(1:length(x9data$X))) {
  objectConstellation <- c(targetObject[i],object2[i],object3[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  if(workerID != x9data$workerid[i]) {
    workerID <- x9data$workerid[i]
    # all three optimized    params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
    params1 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(2)]
    params2 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(3)]
    params12 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(4:5)]
    # print(params)
  }
    validUtterances <- determineValidUtterances(objectConstellation)
    
    if( (i-1)%%4 == 0) {
      priorPrefAll_1 <- getPreferencesPrior(x9data[i,"targetFeatureNum"])
      priorPrefAll_2 <- getPreferencesPrior(x9data[i,"targetFeatureNum"])
    } # uniform focussing on the feature type in question.
    
    if(iterative12 == 1) {
      if(parSetting == 1) {
        postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                   0, 0, priorPrefAll_1)
        postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                   abs(params12[1]), abs(params12[2]), priorPrefAll_2)
      }else if(parSetting == 2) {
        postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                   1, 0, priorPrefAll_1)
        postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                   0.854056914004583, 1.68201930715193, priorPrefAll_2)
      }      
      priorPrefAll_1 <- postListMat1Opt[i,]
      priorPrefAll_2 <- postListMat2Opt[i,]
    }else if(iterative12 == 2) {
      if(parSetting == 1) {
        postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                    0, 0, priorPrefAll_1)
        postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                    abs(params12[1]), abs(params12[2]), priorPrefAll_2)
      }else if(parSetting == 2) {
        postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                   1, 0, priorPrefAll_1)
        postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                   0.336897437687373,1.01378644853853, priorPrefAll_2)
      
      }
    }
}

###########
## adding all those values to the x9data table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x9data <- data.frame(x9data, as.data.frame(subjectResponsesOrdered)) 

postListMat1Opt <- round(postListMat1Opt, digits=5)
colnames(postListMat1Opt) <- colnames(postListMat1Opt, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1Opt))
x9data <- data.frame(x9data, consCodeAndPosteriors) 

postListMat2Opt <- round(postListMat2Opt, digits=5)
colnames(postListMat2Opt) <- colnames(postListMat2Opt, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2Opt))
x9data <- data.frame(x9data, consCodeAndPosteriorsNO) 

x9data$CCode <- uniqueCCode
x9data$logLik <- logLik

if(iterative12 == 1) {
  if(parSetting == 1) {
    write.csv(x9data, "X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_iterative.csv")
  }else if(parSetting == 2) {
    write.csv(x9data, "X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_andOpt12_iterative.csv")
  }
}else if(iterative12 == 2) {
  if(parSetting == 1) {
    write.csv(x9data, "X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_nonIterative.csv")
  }else if(parSetting == 2) {
    write.csv(x9data, "X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_Opt12_nonIterative.csv")
  }
}