##########################################
source("CommonCode/AllUtterancesAndObjects.R")
source("CommonCode/getConstCodeStratUtt.R")


## determines the best information gain utterances based on the valid utterances determined from the currentObjectConstellation
getSimpleBestInfGainUttPreferences <- function(currentObjectConstellation, softPrefValue, notObeyInst, klValueFactor) {
  validUtterances <- determineValidUtterances(currentObjectConstellation)
  mapObjToUtt <- determineObjectToUtterancesMapping(currentObjectConstellation)
  mapUttToObjProbs <- determineUtteranceToObjectProbabilities(validUtterances, 
                                                              currentObjectConstellation, 
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUtteranceToObjectProbabilities(validUtterances, 
                                                                      currentObjectConstellation, 
                                                                      mapObjToUtt, 0)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjectConstellation, 
                                                          softPrefValue, mapUttToObjDeterministic)
  
  preferencesPrior <- rep(1/(length(validUtterances)+1), length(validUtterances)+1)
#  preferencesPrior <- rep(1/(length(validUtterances)), length(validUtterances)+1)
#  preferencesPrior[length(validUtterances)+1] = 0
  return( simpleBestInfGainUtterance(preferencesPrior, validUtterances, currentObjectConstellation, 
                               mapUttToObjProbs, objectPreferenceSoftPriors, 
                               klValueFactor) )
}


#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]
SimpleRSAModelUttKLDiv_3params <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  klRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getSimpleBestInfGainUttPreferences(currentObjects, par1, par2, par3)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      klRes <- klRes + data[i, 4+validUtterances[j]] * 
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(klRes)
}

SimpleRSAModelUttKLDivParamA <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(par[1]), 0, 1))
}
SimpleRSAModelUttKLDivParamB <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0, params[2]))
}

SimpleRSAModelUttKLDivParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamA.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(par[1]), 0.2, 1))
}
SimpleRSAModelUttKLDivParamB.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK.2.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, 0.2, par[1]))
}
SimpleRSAModelUttKLDivParamK.2.0 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamAB <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), 1))
}
SimpleRSAModelUttKLDivParamABK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), params[3]))
}

