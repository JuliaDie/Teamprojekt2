##########################################
## 
source("CommonCode/AllUtterancesAndObjects.R")
source("CommonCode/getConstCodeStratUtt.R")

### 
# Determines the speaker's posterior guess of the listener's feature value preferences
# currentObjectConstellation -> vector of three values in {1,...,27} specifing the target and the other two objects
# featureUtt in {1,2,3} specifying which feature is uttered (i.e. shape / texture / or color)
# parameter: softPrefValue in [0,\infty) specifying how much actual feature priorities come into play (the larger the higher the tendency towards uniform likeing)
# parameter: notObeyInst determines if the instruction does not need to be obeyed (0=full obedience: -> infty  =full instruction ignorance)
# parameter: alpha - exponential scaling of speaker to utter that value that maximizes the chance of getting the target object right
determineSpeakerPostListPrefs <- function(currentObjectConstellation, featureUtt, 
                                          softPrefValue, notObeyInst, alpha) {
  validUtterances <- determineValidUtterances(currentObjectConstellation)
  mapObjToUtt <- determineObjectToUtterancesMapping(currentObjectConstellation)
  mapUttToObjProbs <- determineUtteranceToObjectProbabilities(validUtterances, 
                                                              currentObjectConstellation, 
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUtteranceToObjectProbabilities(validUtterances, 
                                                              currentObjectConstellation, 
                                                              mapObjToUtt, 0)
  #  print(mapUttToObjProbs)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjectConstellation,
                                                          softPrefValue, mapUttToObjDeterministic)
  postListPrefs = rep(0,9)
  for(f in c(1:3)) {
    prefPrior <- rep(0, length(validUtterances+1)) # determine preference prior over all valid utterances plus no preference
    relevantIndices <- which(validUtterances>(3*(f-1)) & validUtterances<(3*f+1)) # relevant indices for a particular feature type
    prefPrior[relevantIndices] <- 1/length(relevantIndices) # prior over the indices
    prefPost <- pragmaticSpeaker(which(validUtterances==
                                  allObjectsToUtterancesMappings[currentObjectConstellation[1],featureUtt]),
                          1, prefPrior, validUtterances, currentObjectConstellation, 
                          mapUttToObjProbs, objectPreferenceSoftPriors, alpha)
    for(i in c((1+(f-1)*3):(f*3))) {
      postListPrefs[i] <- 1/3
    }
    for(i in c(1:length(relevantIndices))) {
      postListPrefs[validUtterances[relevantIndices[i]]] <- prefPost[relevantIndices[i]] * length(relevantIndices) / 3 # posterior back into vector of 3x3 values
    }
  }
  return(postListPrefs)
}

####
# Used to determine the log likelihood estimates of RSA model given the model parameters and the data
# params is a 1 value vector specifying one of the three parameters:
#   1: the softPrefValue (default = 0), i.e., the strength of preferring one entity over others
#   2: the non-obedience (default = 0), i.e. strength of not following the utterance in the choice
#   3: alpha value (default = 1) of exponentially scaling factor in the speaker's choice function
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat,
#                                                          7:Q1AnswerV1,V2,V3, 10:Q2AnserV1,V2,V3]
RSAModelLL1_1 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), 0, 1))
}

RSAModelLL1_1_notObey.1 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), .1, 1))
}

RSAModelLL1_2 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, 0, abs(params[1]), 1))
}

RSAModelLL1_3 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, 0, 0, abs(params[1])))
}


####
# Used to determine the log likelihood estimates of RSA model given the model parameters and the data
# params here is 2 value vector, which specifies two of three (n1=not the first, n2=...):
#   1: the softPrefValue (default = 0), i.e., the strength of preferring one entity over others
#   2: the non-obedience (default = 0), i.e. strength of not following the utterance in the choice
#   3: alpha value (default = 1) of exponentially scaling factor in the speaker's choice function
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat,
#                                                          7:Q1AnswerV1,V2,V3, 10:Q2AnserV1,V2,V3]
RSAModelLL2_n1 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, 0, abs(params[1]), abs(params[2])))
}

RSAModelLL2_n2 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), 0, abs(params[2])))
}

RSAModelLL2_n3 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), abs(params[2]), 1))
}

####
# Used to determine the log likelihood estimates of RSA model given the model parameters and the data
# params here is 3 value vector, which specifies all three model values:
#   1: the softPrefValue (default = 0), i.e., the strength of preferring one entity over others
#   2: the non-obedience (default = 0), i.e. strength of not following the utterance in the choice
#   3: alpha value (default = 1) of exponentially scaling factor in the speaker's choice function
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat,
#                                                          7:Q1AnswerV1,V2,V3, 10:Q2AnserV1,V2,V3]
RSAModelLL3 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), abs(params[2]), abs(params[3])))
}


#### actual RSA model Kullback leibler divergence determination
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat,
#                                                          7:Q1AnswerV1,V2,V3, 10:Q2AnserV1,V2,V3]
RSAModelKLDiv3paramsAllValuesConsidered <- function(data, par1, par2, par3) {
  #  print(params)
  klDiv <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ## determining the model predictions
    probModelRes <- determineSpeakerPostListPrefs(currentObjects, uttFeat, par1, par2, par3)
    ## adding the negative log likelihoods
    for(j in c(1:3)) {
      klDiv <- klDiv + data[i, 6+j] * 
        ( log(data[i, 6+j] + 1e-100) - log( probModelRes[j + (data[i, 5]-1)*3] + 1e-100) )
    }
    for(j in c(1:3)) {  
      klDiv <- klDiv + data[i, 9+j] * 
        ( log(data[i, 9+j] + 1e-100) - log( probModelRes[j + (data[i, 6]-1)*3] + 1e-100) )
    }
  }
  return(klDiv)
}


#### actual RSA model Kullback leibler divergence determination
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat,
#                                                          7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered <- function(data, par1, par2, par3) {
  #  print(params)
  klDiv <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    probModelRes <- determineSpeakerPostListPrefs(currentObjects, uttFeat, abs(par1), abs(par2), par3)
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers. 
    ##
    ## answer set 1 with feature type data[i,5]
    relevantIndices <- which(validUtterances>(3*(data[i, 5]-1)) & validUtterances<(3*data[i, 5] + 1)) # relevant indices for a particular feature type
    # normalizing to one. 
    probModelRes[validUtterances[relevantIndices]] <- probModelRes[validUtterances[relevantIndices]] / 
                                                                    (sum(probModelRes[validUtterances[relevantIndices]]) +  1e-100)
    relIndicesRel <- validUtterances[relevantIndices] - ((data[i,5]-1)*3)
    data[i,6+relIndicesRel] <- data[i,6+relIndicesRel] / (sum(data[i,6+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      klDiv <- klDiv + data[i, 6+relIndicesRel[j]] * 
        ( log(data[i, 6+relIndicesRel[j]] + 1e-100) - log(probModelRes[validUtterances[relevantIndices[j]]] + 1e-100) )
    }
    ##
    ## answer set 2 with feature type data[i,6]
    relevantIndices <- which(validUtterances>(3*(data[i, 6]-1)) & validUtterances<(3*data[i, 6] + 1)) # relevant indices for a particular feature type
    # normalizing to one. 
    probModelRes[validUtterances[relevantIndices]] <- probModelRes[validUtterances[relevantIndices]] / 
                                                                     (sum(probModelRes[validUtterances[relevantIndices]]) +  1e-100)
    relIndicesRel <- validUtterances[relevantIndices] - (3*(data[i, 6]-1))
    data[i,9+relIndicesRel] <- data[i,9+relIndicesRel] / (sum(data[i,9+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      klDiv <- klDiv + data[i, 9+relIndicesRel[j] ] * 
        ( log(data[i, 9+relIndicesRel[j] ] + 1e-100) - log(probModelRes[validUtterances[relevantIndices[j]]] + 1e-100) )
    }
  }
  return(klDiv)
}

# data is a matrix with data rows. 
UniformModelKLDiv <- function(data) {
  klDiv <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
#    probModelRes <- determineSpeakerPostListPrefs(currentObjects, uttFeat, par1, par2, par3)
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers. 
    ##
    ## answer set 1 with feature type data[i,5]
    relevantIndices <- which(validUtterances>(3*(data[i, 5]-1)) & validUtterances<(3*data[i, 5] + 1)) # relevant indices for a particular feature type
    # normalizing to one. 
    relIndicesRel <- validUtterances[relevantIndices] - ((data[i,5]-1)*3)
    data[i,6+relIndicesRel] <- data[i,6+relIndicesRel] / (sum(data[i,6+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      klDiv <- klDiv + data[i, 6+relIndicesRel[j]] * 
        ( log(data[i, 6+relIndicesRel[j]] + 1e-100) - log( 1 / length(relevantIndices)) )  
    }
    ##
    ## answer set 2 with feature type data[i,6]
    relevantIndices <- which(validUtterances>(3*(data[i, 6]-1)) & validUtterances<(3*data[i, 6] + 1)) # relevant indices for a particular feature type
    # normalizing to one. 
    relIndicesRel <- validUtterances[relevantIndices] - (3*(data[i, 6]-1))
    data[i,9+relIndicesRel] <- data[i,9+relIndicesRel] / (sum(data[i,9+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      klDiv <- klDiv + data[i, 9+relIndicesRel[j] ] * 
        ( log(data[i, 9+relIndicesRel[j] ] + 1e-100) - log( 1 / length(relevantIndices) ) ) 
    }
  }
  return(klDiv)
}



###
# get matrix of all posteriors of all object constellations and object choices possible.
getPostListPrefsForAllConstellations <- function(softPrefValue=0, nonObedience=0, alpha=1) {
  resultMat <- matrix(0, 1+ (27*27*27*3), 28)
  for(o1 in c(1:27)) {
    print(o1)
    for(o2 in c(1:27)) {
      print(c(o1,o2))
      for(o3 in c(1:27)) {
        for(featChoice in c(1:3)) {
          row = 3 * (27 * ((o1-1) * 27 + (o2-1) ) + (o3-1) ) + featChoice
          objectConstellation <- c(o1,o2,o3)
          resultMat[row,1:3] <- allObjects[o1,]
          resultMat[row,4:6] <- allObjects[o2,]
          resultMat[row,7:9] <- allObjects[o3,]
          resultMat[row,10] <- o1
          resultMat[row,11] <- o2
          resultMat[row,12] <- o3
          resultMat[row,13] <- featChoice
          resultMat[row,14:19] <- getConstellationCode(objectConstellation, featChoice)  
          resultMat[row,20:28] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                                softPrefValue, nonObedience, alpha)
        }
      }
    }
  }
  return(resultMat)
}

# ## this can be flexibly set to any number of objects under consideration
# currentObjects <- c(1,2,3)
# print(c("Objects:",currentObjects))
# determineSpeakerPostListPrefs(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 3, 0, 0, 1)
# currentObjects <- c(1,4,16)
# print(c("Objects:",allObjects[currentObjects,]))
# determineSpeakerPostListPrefs(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 3, 0, 0, 1)
# currentObjects <- c(1,4,7)
# print(c("Objects:",allObjects[currentObjects,]))
# determineSpeakerPostListPrefs(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 3, 0, 0, 1)

#Generating the big table... 
# bigTablePostListPrefs <- getPostListPrefsForAllConstellations()

