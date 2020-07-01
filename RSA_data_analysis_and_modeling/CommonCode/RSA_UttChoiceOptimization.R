##########################################

source("CommonCode/AllUtterancesAndObjects.R")
source("CommonCode/getConstCodeStratUtt.R")

## determines the best information gain utterances based on the valid utterances determined from the currentObjectConstellation
getBestInfGainUttPreferences <- function(currentObjectConstellation, softPrefValue, notObeyInst, alpha, klValueFactor) {
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
  return( bestInfGainUtterance(preferencesPrior, validUtterances, currentObjectConstellation, 
                               mapUttToObjProbs, objectPreferenceSoftPriors, 
                               alpha, klValueFactor) )
}


#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]
RSAModelUttKLDiv_3params <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getBestInfGainUttPreferences(currentObjects, par1, par2, 1, par3)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      llRes <- llRes + data[i, 4+validUtterances[j]] * 
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(llRes)
}

RSAModelUttKLDivParamA <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, abs(par[1]), 0, 1))
}
RSAModelUttKLDivParamB <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, 0, abs(par[1]), 1))
}
RSAModelUttKLDivParamC <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, 0, 0, abs(par[1])))
}
RSAModelUttKLDivParamBC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, 0, abs(params[1]), abs(params[2])))
}
RSAModelUttKLDivParamAC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), 0, abs(params[2])))
}
RSAModelUttKLDivParamAB <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), 1))
}
RSAModelUttKLDivParamABC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), abs(params[3])))
}

#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]
RSAModelUttKLDiv_4params <- function(data, par1, par2, par3, par4) {
  #   print(c(par1, par2, par3, data))
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getBestInfGainUttPreferences(currentObjects, par1, par2, par3, par4)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      llRes <- llRes + data[i, 4+validUtterances[j]] * 
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(llRes)
}

RSAModelUttKLDivParamD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, 0, 1, params[1]))
}
RSAModelUttKLDivParamD4A.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, .2, 0, 1, params[1]))
}
RSAModelUttKLDivParamD4B.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, .2, 1, params[1]))
}
RSAModelUttKLDivParamD4AB.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, .2, .2, 1, params[1]))
}
RSAModelUttKLDivParamAD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), 0, 1, params[2]))
}
RSAModelUttKLDivParamBD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, abs(params[1]), 1, params[2]))
}
RSAModelUttKLDivParamAD4B.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), .2, 1, params[2]))
}
RSAModelUttKLDivParamBD4A.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, .2, abs(params[1]), 1, params[2]))
}
RSAModelUttKLDivParamCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, 0, abs(params[1]), params[2]))
}
RSAModelUttKLDivParamBCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, abs(params[1]), abs(params[2]), params[3]))
}
RSAModelUttKLDivParamACD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), 0, abs(params[2]), params[3]))
}
RSAModelUttKLDivParamABD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), abs(params[2]), 1, params[3]))
}
RSAModelUttKLDivParamABCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), abs(params[2]), abs(params[3]), params[4]))
}
