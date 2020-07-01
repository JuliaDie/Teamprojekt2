

############################################################################################
procType <- 2    ###########################################################################
############################################################################################
# 1 iterative (works)
# 2 non-iterative (doesn't work)

if (procType == 1){
  source("X9_Code/SRSA_StratUtt_X9.R")
  source("CommonCode/SRSA_StratUttOptimization_iterative.R") 
} else {
  source("CommonCode/SRSA_StratUtt.R")
  source("X9_Code/SRSA_StratUttOptimization_X9.R")
}


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
## Starting with simple base model determination:
##
workerIndex <- 1
for(workerID in c(0:idMax)) {
  allIndices <- which(workerIDs == workerID)
  if(length(allIndices)>0) {
    llWorkers12[workerIndex,1] <- workerID
    paramsWorkers12[workerIndex,1] <- workerID
    ## based model -> no change in preferences!
    llWorkers12[workerIndex,2] <- 0 # -2 * length(allIndices) * log(1/3)
    for(i in c(1:length(allIndices))) {
      for(j in c(1:3)) {
        llWorkers12[workerIndex, 2] <- llWorkers12[workerIndex, 2] + 
                                        subjectResponses[allIndices[i],j] * 
                                          (log(subjectResponses[allIndices[i],j]) - log(1/3))
      }
    }
    ## done with this worker -> proceed
    workerIndex <- workerIndex + 1
  }
}

###### Optimizing the log likelihoods globally over iterative function ########

allIndices <- c(1:length(workerIDs))
   
## generating data matrix for the purpose of optimization

dataWorker <- matrix(0, length(allIndices), 9)
dataWorker[,1] <- targetObject[allIndices]
dataWorker[,2] <- object2[allIndices]
dataWorker[,3] <- object3[allIndices]
dataWorker[,4] <- uttFeat[allIndices]
dataWorker[,5] <- targetFeat[allIndices]
dataWorker[,6:8] <- subjectResponses[allIndices,1:3]
dataWorker[,9] <- targetFeat[allIndices]

######################### 1 parameter optimization ###########################

# before optimization:         llWorkers12[1,3] <- RSAModelLL1(c(.2), dataWorker)
optRes1 <- optimize(RSAModelLL1_1simpleRSA, c(0,1e+10), dataWorker)   
optRes2 <- optimize(RSAModelLL1_2simpleRSA, c(0,1e+10), dataWorker)   

######################### 2 parameter optimization ###########################

print("Starting optimization with two free parameters Simple RSA model... ")

optRes2n1 <- optim(c(.2, .2), RSAModelLL2_simpleRSA, method="L-BFGS-B", gr=NULL, dataWorker,
                   lower = c(0,0), upper = c(1e+10,1e+10))

######################### recording results #################################

llWorkers12[1,3] <- optRes1$objective
llWorkers12[1,4] <- optRes2$objective

llWorkers12[1,5] <- optRes2n1$value
   
 ## resulting parameter choice

paramsWorkers12[1,2] <- optRes1$minimum
paramsWorkers12[1,3] <- optRes2$minimum

print("One-parameter optimization")
print(paste("Globally optimized softness value is ", round(paramsWorkers12[1,2],3), sep = ""))
print(paste("Globally optimized obedience value is ", round(paramsWorkers12[1,3],3), sep = ""))

paramsWorkers12[1,4] <- optRes2n1$par[1]
paramsWorkers12[1,5] <- optRes2n1$par[2]

print("Two-parameter optimization ")
print(paste("Globally optimized softness value is ", round(paramsWorkers12[1,4],3), sep=""))
print(paste("Globally optimized obedience value is ", round(paramsWorkers12[1,5],3), sep = ""))   

print(round(llWorkers12[1,],3))
print( round(paramsWorkers12[1,],3))
    
# before optimization:     llWorkers12[workerIndex,7] <- RSAModelLL2(c(.2,.2), dataWorker)
# print(optRes)
## max likelihood parameter choice
## max likelihood parameter choice

#################### writing out result tables ############################################

if(procType == 1) {
  write.csv(llWorkers12, "X9_Data/x9KLDivs_simpleRSA_globalOpt.csv")
  write.csv(paramsWorkers12, "X9_Data/x9Params_simpleRSA_globalOpt.csv")
}else if(procType == 2) {
  write.csv(llWorkers12, "X9_Data/x9KLDivs_simpleRSA_globalOpt_nonIterative.csv")
  write.csv(paramsWorkers12, "X9_Data/x9Params_simpleRSA_globalOpt_nonIterative.csv")
}

################### Calculating model predictions with estimated parameters ##############

if(procType == 1) {
  paramsWorkers12 <- as.matrix(read.csv("X9_Data/x9Params_simpleRSA_globalOpt.csv"))
}else {
  paramsWorkers12 <- as.matrix(read.csv("X9_Data/x9Params_simpleRSA_globalOpt_nonIterative.csv"))
}

#paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]
params1 <- paramsWorkers12[1,3]
params2 <- paramsWorkers12[1,4]
params12 <- paramsWorkers12[1,c(5,6)]



### 
# determining the model predictions after worker-specific model parameter optimization!

constellationCode <- matrix(0,length(x9data$X),6)
uniqueCCode <- rep(0, length(x9data$X))
postListMat1Opt <- matrix(0,length(x9data$X),9)
postListMat2Opt <- matrix(0,length(x9data$X),9)

###########################################

  
for(i in c(1:length(x9data$X))) {
  objectConstellation <- c(targetObject[i],object2[i],object3[i])
  featChoice <- uttFeat[i]
#  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
#  uc <- 0
#  for(j in c(1:6)) {
#    uc <- (uc * 10) + constellationCode[i,j]
#  }
#  uniqueCCode[i] <- uc
  validUtterances <- determineValidUtterances(objectConstellation)
  
  if( (i-1)%%4 == 0) {
    priorPrefAll_1 <- getPreferencesPrior(x9data[i,"targetFeatureNum"])
    priorPrefAll_2 <- getPreferencesPrior(x9data[i,"targetFeatureNum"])
    } # focussing on the feature type in question.
  
  if(procType == 1) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                  0, 0, priorPrefAll_1)
    priorPrefAll_1 <- postListMat1Opt[i,]
   
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                  abs(params1[1]), 0, priorPrefAll_2)
    priorPrefAll_2 <- postListMat2Opt[i,]
 

  }else if(procType == 2) {
   postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  0, 0)
   postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  abs(params1[1]), 0)
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

#x9data$CCode <- uniqueCCode

if(procType == 1) {
  write.csv(x9data, "X9_Data/x9dataAugm_SRSAglobalOpt_fixed00_and_Opt1.csv")
}else if(procType == 2) {
  write.csv(x9data, "X9_Data/x9dataAugm_SRSAglobalOpt_OptPrefObedFixed0_and_Opt1_nonIterative.csv")
}

