##########################################
source("CommonCode/AllUtterancesAndObjects.R")
source("CommonCode/getConstCodeStratUtt.R")


## determines the best information gain utterances based on the valid utterances determined from the currentObjects
getSimpleBestInfGainUttPreferencesIterative <- function(preferencesPriorAll,
                                                        currentObjects, softPrefValue, 
                                                        notObeyInst, klValueFactor, targetFeature) {
  relevantUtterances <- determineValidUtterances(currentObjects)
  mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
  mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances, 
                                                              currentObjects, 
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUtteranceToObjectProbabilities(relevantUtterances, 
                                                                      currentObjects, 
                                                                      mapObjToUtt, 0)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects, 
                                                          softPrefValue, mapUttToObjDeterministic)
  
  preferencesPrior <- preferencesPriorAll[relevantUtterances]
  
  # Define utterance prior excluding utterances of target feature
  utterancePrior <- rep(0,length(relevantUtterances))
  irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
  validUtterances <- relevantUtterances[-irrelevantIndices]
  utterancePriorShort <- rep (1/length(validUtterances),length(validUtterances)) 
  utterancePrior[-irrelevantIndices] <- utterancePriorShort
  
#  preferencesPrior <- rep(1/(length(relevantUtterances)), length(relevantUtterances)+1)
#  preferencesPrior[length(relevantUtterances)+1] = 0
  return( simpleBestInfGainUtteranceWithPrefPriorAll(preferencesPriorAll, relevantUtterances, currentObjects, 
                               mapUttToObjProbs, objectPreferenceSoftPriors, 
                               klValueFactor, targetFeature, utterancePrior) )
}


#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]
SimpleRSAModelUttKLDiv_3params_iterative <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  logLik <- 0
  for(i in c(1:nrow(data))) {
    if( (i-1)%%4 == 0) {
      preferencesPriorAll <- getPreferencesPrior(data[i,5]) # focussing on the feature type in question.
    }
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    targetFeat <- data[i, 5]
    pickedUtterance <- data[i, 6]
    relevantUtterances <- determineValidUtterances(currentObjects)
    irrelevantIndices <- which(relevantUtterances>(3*(data[i,5]-1)) & 
                                 relevantUtterances<(3*data[i,5] + 1))
    validUtterances <- relevantUtterances[-irrelevantIndices]  
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    output <- getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll,currentObjects, abs(par1), 
                                                          abs(par2), par3, targetFeat) 
    bInfGainUttModel[relevantUtterances] <- output[[1]] 
    preferencesPriorAll <- output[[2]][pickedUtterance,,1] 
    ## adding the negative log likelihoods
    logLik <- logLik - log(bInfGainUttModel[relevantUtterances[pickedUtterance]] + 1e-100)
#    print(logLik)
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(logLik)
}

SimpleRSAModelUttKLDivParamA_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(par[1]), 0, 1))
}
SimpleRSAModelUttKLDivParamB_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), 0, params[2]))
}

SimpleRSAModelUttKLDivParamAK.2_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamA.2_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(par[1]), 0.2, 1))
}
SimpleRSAModelUttKLDivParamB.2_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK.2.2_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, 0.2, par[1]))
}
SimpleRSAModelUttKLDivParamK.2.0_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK.2_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK.2_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamAB_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), abs(params[2]), 1))
}
SimpleRSAModelUttKLDivParamABK_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), abs(params[2]), params[3]))
}

SimpleRSAModelUttKLDiv_3params_independent <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  logLik <- 0
  for(i in c(1:nrow(data))) {
    preferencesPriorAll <- getPreferencesPrior(data[i,5]) # focussing on the feature type in question.
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    #    numUtterances <- data[i,4]
    uttFeat <- data[i,4]
    targetFeat <- data[i, 5]
    pickedUtterance <- data[i, 6]
    relevantUtterances <- determineValidUtterances(currentObjects)
    irrelevantIndices <- which(relevantUtterances>(3*(data[i,5]-1)) & 
                                 relevantUtterances<(3*data[i,5] + 1))
    validUtterances <- relevantUtterances[-irrelevantIndices]  
    ## determining the model predictions
    #################      Code below not edited ############
    bInfGainUttModel <- rep(NA, 9)
    output <- getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll,currentObjects, abs(par1), 
                                                          abs(par2), par3, targetFeat) 
    bInfGainUttModel[relevantUtterances] <- output[[1]] 
 #   preferencesPriorAll <- output[[2]][pickedUtterance,,1] 
    ## adding the negative log likelihoods
    logLik <- logLik - log(bInfGainUttModel[relevantUtterances[pickedUtterance]] + 1e-100)
   if (bInfGainUttModel[relevantUtterances[pickedUtterance]] == 0){
      print('#####################')
      print(bInfGainUttModel)
      print(relevantUtterances)
      print(validUtterances)
      print(pickedUtterance)
      print(currentObjects)
      print()
      print(allObjects[currentObjects,])
    }
    
    #    print(logLik)
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(logLik)
}

SimpleRSAModelUttKLDivParamA_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(par[1]), 0, 1))
}
SimpleRSAModelUttKLDivParamB_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), 0, params[2]))
}

SimpleRSAModelUttKLDivParamAK.2_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamA.2_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(par[1]), 0.2, 1))
}
SimpleRSAModelUttKLDivParamB.2_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK.2.2_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, 0.2, par[1]))
}
SimpleRSAModelUttKLDivParamK.2.0_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK.2_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK.2_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamAB_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), abs(params[2]), 1))
}
SimpleRSAModelUttKLDivParamABK_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), abs(params[2]), params[3]))
}


#Testing optimization function
currentObjects <- c(1,2,6)
#currentObjects <- c(26,20,23)
notObeyInst <- 1000
klValueFactor <- 1
softPrefValue <- 1000
targetFeature <- 2
trial <- 1
utt <- 5
obj <- 1
if (trial-1%%4 == 0){
  preferencesPriorAll <- getPreferencesPrior(targetFeature)
}

output <-  getSimpleBestInfGainUttPreferencesIterative(
  preferencesPriorAll, currentObjects,
  softPrefValue, notObeyInst, klValueFactor, targetFeature)
posteriorUtterances <- round(output[[1]],3)
preferencesPriorAll <- round(output[[2]][utt,,obj],3)
