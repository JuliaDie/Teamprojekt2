source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_UttChoiceOptimization_iterative.R")

x9data = read.csv(
  "X9_Data/ella_coded_data.csv",
  header = TRUE,
  na.strings = c("", " ", "NA")
)

# adding feature property codes (which feature was uttereed, which features were questioned)
#uttFeat <- ifelse(x9data$utterance=="green" | x9data$utterance=="red" | x9data$utterance=="blue", 3,
#                   ifelse(x9data$utterance=="solid" | x9data$utterance=="striped" | x9data$utterance=="polka-dotted", 2, 1))
#x9data$uttFeat <- uttFeat

########## Reading in information from the data table #####

targetFeat <- x9data$targetFeatureNum
targetObject <- x9data$targetObject
object2 <- x9data$object2
object3 <- x9data$object3
utterance <- x9data$utteranceNum

# Which utterance the subjects picked

pickedUtterance <- rep(NA, length(x9data$X))
 for(i in c(1:length(x9data$X))) {
#   utterance[i] <- match(as.character(x9data$utterance[i]),allUtterancesNew1) ## polka-dotted!!!
   currentObjects <- c(targetObject[i],object2[i],object3[i]) 
   relevantUtterances <- determineValidUtterances(currentObjects)
   pickedUtterance[i] <- match(utterance[i], relevantUtterances)  
# #  featChoice <- uttFeat[i]
 }

############ Set up the parameters and KL values matrix ##########

workerIDs <- x9data$workerid
idMax <- max(workerIDs)
logLikWorkers <- matrix(0,length(unique(workerIDs)), 10)
paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 11)

#######################################################
## Starting with simple base model determination:    ##
#######################################################

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    logLikWorkers[workerIndex,1] <- workerID
    logLikWorkers[workerIndex, 2] <- 0
    paramsUttWorkers[workerIndex,1] <- workerID
    
    dataWorker <- matrix(0, length(idICases), 6)
    dataWorker[,1] <- targetObject[idICases]
    dataWorker[,2] <- object2[idICases]
    dataWorker[,3] <- object3[idICases]
#    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- targetFeat[idICases]
    dataWorker[,6] <- pickedUtterance[idICases] 
    
    ## base model -> no change in preferences!
    ## The base model assigns a flat distribution over all possible utterance choices
    for(i in c(1:length(idICases))) { 
      #   logLikWorkers[workerIndex,2] <- 0
      currentObjects <- dataWorker[i,1:3] 
      relevantUtterances <- determineValidUtterances(currentObjects)
      irrelevantIndices <- which(relevantUtterances>(3*(dataWorker[i,5]-1)) & 
                                   relevantUtterances<(3*dataWorker[i,5] + 1))
      validUtterances <- relevantUtterances[-irrelevantIndices]  
      #    print(length(validUtterances))
      logLikWorkers[workerIndex, 2] <- logLikWorkers[workerIndex, 2] - log(1/length(validUtterances))  
      if(is.na(logLikWorkers[workerIndex, 2])) {
        print("Is NA!???")
      } 
    } 
    ## done with this worker -> proceed
    workerIndex <- workerIndex + 1
  }
}


## Optimizing (i.e. minimzing) the negative log likelihood values for each worker...
## starting with 1 parameter RSA model optimizations... 
## data is a matrix with data rows. column structure: 
## [1:tagrget object, 2: object 2, 3: object 3,4:uttered featue, 5: target feature,6: picked utterance]

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 8)
    dataWorker[,1] <- targetObject[idICases]
    dataWorker[,2] <- object2[idICases]
    dataWorker[,3] <- object3[idICases]
#    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- targetFeat[idICases]
    dataWorker[,6] <- pickedUtterance[idICases]
    
    ## 1 param RSA Utt model optimizing for kl-value factor
    optRes1 <- optimize(SimpleRSAModelUttKLDivParamA_iterative, c(0,1e+10), dataWorker) 
    optRes2 <- optimize(SimpleRSAModelUttKLDivParamA_independent, c(0,1e+10), dataWorker) 
    optRes3 <- optimize(SimpleRSAModelUttKLDivParamK_iterative, c(-10,10), dataWorker) 
    optRes4 <- optimize(SimpleRSAModelUttKLDivParamK_independent, c(-10,10), dataWorker)
    optRes2n2iter <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamAK_iterative, method="L-BFGS-B", gr=NULL, dataWorker,
                                             lower = c(0,-10), upper = c(1e+10,10))
    optRes2n2indep <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamAK_independent, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,-10), upper = c(1e+10,10))
    
#### Recording negative log likelihoods ####
    
    logLikWorkers[workerIndex,3] <- SimpleRSAModelUttKLDiv_3params_iterative(dataWorker, 0,0,1) 
    logLikWorkers[workerIndex,4] <- SimpleRSAModelUttKLDiv_3params_independent(dataWorker, 0,0,1) 
    logLikWorkers[workerIndex,5] <- optRes1$objective
    logLikWorkers[workerIndex,6] <- optRes2$objective
    logLikWorkers[workerIndex,7] <- optRes3$objective
    logLikWorkers[workerIndex,8] <- optRes4$objective
    logLikWorkers[workerIndex,9] <- optRes2n2iter$value
    logLikWorkers[workerIndex,10] <- optRes2n2indep$value
#   logLikWorkers[workerIndex,9] <- optRes3$objective < optRes4$objective
    colnames(logLikWorkers) <- c("workerid","uniform","default_param_iter", "default_param_indep",
                                 "iter_Soft","indepSoft",
                                 "iterLambda", "indepLambda", 
                                 "iterSoftLambda", "indepSoftLambda"
    )
#### Recording parameter estimates ####
    
    paramsUttWorkers[workerIndex,4] <- optRes1$minimum
    paramsUttWorkers[workerIndex,5] <- optRes2$minimum
    paramsUttWorkers[workerIndex,6] <- optRes3$minimum
    paramsUttWorkers[workerIndex,7] <- optRes4$minimum
    paramsUttWorkers[workerIndex,8] <- optRes2n2iter$par[1]
    paramsUttWorkers[workerIndex,9] <- optRes2n2iter$par[2]
    paramsUttWorkers[workerIndex,10] <- optRes2n2indep$par[1]
    paramsUttWorkers[workerIndex,11] <- optRes2n2indep$par[2]
    
    print(c("Done with worker ",workerIndex," with worker ID ", workerID))
    print(c(logLikWorkers[workerIndex,], paramsUttWorkers[workerIndex,]))
    # ####
    workerIndex <- workerIndex + 1
  }
}

########### Additional optimization options #################

#   
#   optRes2 <- optimize(SimpleRSAModelUttKLDivParamB_iterative, c(0,1e+10), dataWorker)  
#   logLikWorkers[workerIndex,3] <- optRes1$objective
#   logLikWorkers[workerIndex,4] <- optRes2$objective
#   paramsUttWorkers[workerIndex,2] <- optRes1$minimum
#   paramsUttWorkers[workerIndex,3] <- optRes2$minimum

#    optRes2n1 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamBK, method="L-BFGS-B", gr=NULL, dataWorker,
#                       lower = c(0,-10), upper = c(1e+10,10))
#    
#    optRes3 <- optim(c(.2, .2, 1), SimpleRSAModelUttKLDivParamABK, method="L-BFGS-B", gr=NULL, dataWorker,
#                     lower = c(0,0,-10), upper = c(1e+10,1e+10,10))

## 2 and 3 param RSA model2
## max likelihood parameter choice
#    logLikWorkers[workerIndex,6] <- optRes2n1$value
#    logLikWorkers[workerIndex,7] <- optRes2n2$value
#    logLikWorkers[workerIndex,8] <- optRes3$value
## max likelihood parameter choice
# paramsUttWorkers[workerIndex,5] <- optRes2n1$par[1]
# paramsUttWorkers[workerIndex,6] <- optRes2n1$par[2]
# paramsUttWorkers[workerIndex,9] <- optRes3$par[1]
# paramsUttWorkers[workerIndex,10] <- optRes3$par[2]
# paramsUttWorkers[workerIndex,11] <- optRes3$par[3]
##    

############### Recording output ##########################

write.csv(logLikWorkers, "X9_Data/x9_logLik_indOpt.csv")
write.csv(paramsUttWorkers, "X9_Data/x9Params_Lambda_indOpt.csv")
