##########################################
source("CommonCode/AllUtterancesAndObjects.R")
source("CommonCode/getConstCodeStratUtt.R")


## determines the best information gain utterances based on the valid utterances determined from the currentObjectConstellation
getSimpleBestInfGainUttPreferencesFTF <- function(currentObjectConstellation, featureTypeFocus, softPrefValue, notObeyInst, klValueFactor) {
  validUtterances <- determineValidUtterances(currentObjectConstellation)
  # utterances of the feature type focus are filtered out
  # (e.g. you are interested in color preferences - so you are not allowed to utter colors!)
  # --> validUtterancesFTF correspond to all features present in the current objects except those of the featureTypeFocus!
  validUtterancesFTF <-   validUtterances[which(validUtterances <= ((featureTypeFocus-1)*3) | 
                                                  validUtterances > (featureTypeFocus*3))]
  invalidUtteranceIndices <-   which(validUtterances > ((featureTypeFocus-1)*3) & 
                                                  validUtterances <= (featureTypeFocus*3))
  
  mapObjToUtt <- determineObjectToUtterancesMapping(currentObjectConstellation)
  mapUttToObjProbs <- determineUtteranceToObjectProbabilities(validUtterances, 
                                                              currentObjectConstellation, 
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUtteranceToObjectProbabilities(validUtterances, 
                                                                      currentObjectConstellation, 
                                                                      mapObjToUtt, 0)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjectConstellation, 
                                                          softPrefValue, mapUttToObjDeterministic)
  
  preferencesPrior <- rep(0, length(validUtterances)+1)
  preferencesPrior[invalidUtteranceIndices] <- rep(1/(length(invalidUtteranceIndices)), length(invalidUtteranceIndices) )
  return( simpleBestInfGainUtterance(preferencesPrior, validUtterances, currentObjectConstellation, 
                               mapUttToObjProbs, objectPreferenceSoftPriors, 
                               klValueFactor) )
}


#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]
SimpleRSAModelUttKLDiv_3paramsFTF <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  klRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    featureTypeFocus <- data[i,5]
    validUtterances <- determineValidUtterances(currentObjects)
    # utterances of the feature type focus are filtered out
    # (e.g. you are interested in color preferences - so you are not allowed to utter colors!)
    # --> validUtterancesFTF correspond to all features present in the current objects except those of the featureTypeFocus!
    validUtterancesFTF <-   validUtterances[which(validUtterances <= ((featureTypeFocus-1)*3) | 
                                                      validUtterances > (featureTypeFocus*3))]
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getSimpleBestInfGainUttPreferencesFTF(currentObjects, featureTypeFocus, 
                                                                               par1, par2, par3)
    bInfGainUttModel[validUtterancesFTF] <- bInfGainUttModel[validUtterancesFTF] / (sum(bInfGainUttModel[validUtterancesFTF])) 
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterancesFTF))) {
      klRes <- klRes + data[i, 5+validUtterancesFTF[j]] * 
        (log(data[i, 5+validUtterancesFTF[j]] + 1e-100) - log(bInfGainUttModel[validUtterancesFTF[j]] + 1e-100) )
    }
    #    print(c(data[i, 5+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(klRes)
}

SimpleRSAModelUttKLDivFTFParamA <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(par[1]), 0, 1))
}
SimpleRSAModelUttKLDivFTFParamB <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivFTFParamK <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0, 0, par[1]))
}
SimpleRSAModelUttKLDivFTFParamBK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivFTFParamAK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), 0, params[2]))
}

SimpleRSAModelUttKLDivFTFParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivFTFParamA.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(par[1]), 0.2, 1))
}
SimpleRSAModelUttKLDivFTFParamB.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivFTFParamK.2.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, 0.2, par[1]))
}
SimpleRSAModelUttKLDivFTFParamK.2.0 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, 0, par[1]))
}
SimpleRSAModelUttKLDivFTFParamBK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivFTFParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivFTFParamAB <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), abs(params[2]), 1))
}
SimpleRSAModelUttKLDivFTFParamABK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), abs(params[2]), params[3]))
}

