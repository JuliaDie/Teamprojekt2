
############################################################################################
procType <- 1   ###########################################################################
############################################################################################
# 1 iterative
# 2 non-iterative

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

#######################
## Optimizing the log likelihoods (maximum log likelihoods for each worker...)
##    1 parameter RSA model optimizations... 
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 8)
    dataWorker[,1] <- targetObject[idICases]
    dataWorker[,2] <- object2[idICases]
    dataWorker[,3] <- object3[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- targetFeat[idICases]
    dataWorker[,6:8] <- subjectResponses[idICases,1:3]
    
    ######################### 1 parameter optimization ###########################
    
    if (procType == 1){
      # before optimization:         llWorkers12[1,3] <- RSAModelLL1(c(.2), dataWorker)
      optRes1 <- optimize(RSAModelLL1_1simpleRSA4TrialsIterative, c(0,1e+10), dataWorker)   
      optRes2 <- optimize(RSAModelLL1_2simpleRSA4TrialsIterative, c(0,1e+10), dataWorker)   
    }else{
      optRes1 <- optimize(RSAModelLL1_1simpleRSA4TrialsIndependent, c(0,1e+10), dataWorker)   
      optRes2 <- optimize(RSAModelLL1_2simpleRSA4TrialsIndependent, c(0,1e+10), dataWorker)   
    }
    
    ######################### recording results ################################# 
    ## 1 param RSA model
    llWorkers12[workerIndex,3] <- optRes1$objective
    llWorkers12[workerIndex,4] <- optRes2$objective
    ## resulting parameter choice
    paramsWorkers12[workerIndex,2] <- optRes1$minimum
    paramsWorkers12[workerIndex,3] <- optRes2$minimum
    ####
    print(llWorkers12[workerIndex,])
    print(paramsWorkers12[workerIndex,])
    ####
    workerIndex <- workerIndex + 1
  }
}

######################### 2 parameter optimization ###########################

## Optimizing the log likelihoods (maximum log likelihoods for each worker...)
print("Starting optimization with two free parameters Simple RSA model... ")
workerIDs <- x9data$workerid
idMax <- max(workerIDs)

# llWorkers12 <- matrix(0,length(unique(workerIDs)), 2)
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 8)
    dataWorker[,1] <- targetObject[idICases]
    dataWorker[,2] <- object2[idICases]
    dataWorker[,3] <- object3[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- targetFeat[idICases]
    dataWorker[,6:8] <- subjectResponses[idICases,1:3]
    
# before optimization:     llWorkers12[workerIndex,7] <- RSAModelLL2(c(.2,.2), dataWorker)
    if (procType == 1){
      # before optimization:         llWorkers12[1,3] <- RSAModelLL1(c(.2), dataWorker)
      optRes12 <- optim(c(.2, .2), RSAModelLL2_simpleRSA4TrialsIterative, method="L-BFGS-B", gr=NULL, dataWorker,
                         lower = c(0,0), upper = c(1e+10,1e+10))
    }else{
      optRes12 <- optim(c(.2, .2), RSAModelLL2_simpleRSA4TrialsIndependent, method="L-BFGS-B", gr=NULL, dataWorker,
                         lower = c(0,0), upper = c(1e+10,1e+10))
    }
    ##########
    # print(optRes)
    ## 2 param RSA model2
    ## max likelihood parameter choice
    llWorkers12[workerIndex,5] <- optRes12$value
    ## max likelihood parameter choice
    paramsWorkers12[workerIndex,4] <- optRes12$par[1]
    paramsWorkers12[workerIndex,5] <- optRes12$par[2]
    ##
    print(llWorkers12[workerIndex,])
    print(paramsWorkers12[workerIndex,])
    workerIndex <- workerIndex + 1
  }
}

## writing out result tables
if(procType == 1) {
write.csv(llWorkers12, "X9_Data/x9KLDivs_simpleRSA_indOpt_iterative.csv")
write.csv(paramsWorkers12, "X9_Data/x9Params_simpleRSA_indOpt_iterative.csv")
} else {
write.csv(llWorkers12, "X9_Data/x9KLDivs_simpleRSA_indOpt_nonIterative.csv")
write.csv(paramsWorkers12, "X9_Data/x9Params_simpleRSA_indOpt_nonIterative.csv")  
}