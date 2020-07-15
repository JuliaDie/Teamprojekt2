## All Utterances
# All possible utterances (i.e. object features) that can be handled.
# Here, we assume a 3x3 matrix (three feature types with three expressions each)
# allUtterances <- c('cloud', 'circle', 'square', 'solid', 'striped', 'dotted', 'blue', 'red', 'green')
allUtterancesNew1 <- c('cloud', 'circle', 'square', 'solid', 'striped', 'polka-dotted', 'blue', 'red', 'green')
allFeatureTypesNew1 <- c('shape','pattern','color')
allUttMatrix <- matrix(allUtterancesNew1, ncol=3, byrow=TRUE)
##
## All Objects
# all object matrix contains 3^3 types of objects.
# the matrix essentially specifies the 3 feature expressions for each object
# thus, the matrix maps objects to matching utterances
# all Objects implements the strings, 
# allObjectsToUtterancesMappings encodes the index mappings
allObjects <- matrix('',27,3)
allObjectsToUtterancesMappings <- matrix(0,27,3)
for(index in c(1:27)) {
  #  print(c(1+((index-1)%%3), 1+floor(((index-1)%%9)/3), 1+floor((index-1)/9)))
  allObjects[index,1] <- allUttMatrix[1,1+((index-1)%%3)]
  allObjects[index,2] <- allUttMatrix[2,1+floor(((index-1)%%9)/3)]
  allObjects[index,3] <- allUttMatrix[3,1+floor((index-1)/9)]
  allObjectsToUtterancesMappings[index,1] <- 1+((index-1)%%3)
  allObjectsToUtterancesMappings[index,2] <- 4+floor(((index-1)%%9)/3)
  allObjectsToUtterancesMappings[index,3] <- 7+floor((index-1)/9)
}

## 
## The relevant utterances are determined given currentObjects
# valid utterances correspond to all features present in the current objects!
determineValidUtterances <- function(currentObjects) {
  validUtterances <- c()
  for(i in c(1:length(currentObjects))) {
    validUtterances <- c(validUtterances, allObjectsToUtterancesMappings[currentObjects[i],])
  }
  validUtterances <- sort(unique(validUtterances))
  return(validUtterances)
}

determineAllFeaValues <- function(currentObjects) {
  allFeaUtterances <- c()
  for(i in c(1:length(currentObjects))) {
    allFeaUtterances <- c(allFeaUtterances, allObjectsToUtterancesMappings[currentObjects[i],])
  }
  return(allFeaUtterances)
}

###
## No preference is encoded with 4, whereas a specific feature expression preference is encode 
# by the respective index value
# get feature-respective priors returns general feature respective priors for all 3 features
# @deprecated (not used currently!)
getFeatureRespectivePriors <- function(softAddProb) {
  featureRespectivePriors <- list()
  for(i in c(1:3)) { ## for all three features generate a preference matrix
    m <- matrix(0,4,3)
    for(fPref in c(1:3)) {
      m[fPref,fPref] <- 1
      m[fPref,] <- m[fPref,] + softAddProb
      m[fPref,] <- m[fPref,] / sum(m[fPref,])
    }
    m[4,] <- 1/3
    featureRespectivePriors[[i]] <- m
  }
  return(featureRespectivePriors)
}

##
## Determining the specifc mapping of objects to utterances that applies given currentObjects
# mapping current objects to utterances
determineObjectToUtterancesMapping <- function(currentObjects) {
  mapObjToUtt <- matrix(0, length(currentObjects), 3)
  for(i in c(1:length(currentObjects))) {
    mapObjToUtt[i,] <- allObjectsToUtterancesMappings[currentObjects[i],]
  }
  return(mapObjToUtt)
}

##
# Determining the corresponding mappings from all relevant utterances to objects
# parameter notObeyInst determines if the instruction does not need to be obeyed (0=full obedience: -> infty  =full instruction ignorance) 
determineUtteranceToObjectProbabilities <- function(consideredUtterances, currentObjects, 
                                                    mapObjToUtt, notObeyInst) {
  mapUttToObj <- list()
  mapUttToObjProbs <- matrix(notObeyInst, length(consideredUtterances), length(currentObjects))
  for(utt in rep(1:length(consideredUtterances)) ) {
    # determine array of all objects that match the utterance
    mapUttToObj[[utt]] = ((which(mapObjToUtt[,] == consideredUtterances[utt])-1)%%nrow(mapObjToUtt))+1
    for(i in rep(1:length(mapUttToObj[[utt]]))) {
      mapUttToObjProbs[utt,mapUttToObj[[utt]][i]] <- mapUttToObjProbs[utt,mapUttToObj[[utt]][i]] + 1;
    }
    mapUttToObjProbs[utt,] <- mapUttToObjProbs[utt,] / sum(mapUttToObjProbs[utt,])# length(mapUttToObj[[utt]])
  }
  return(mapUttToObjProbs)
}

# gives a full random matrix of all feature value preferences given a certain ratio i.e. c(0, 1/3, 2/3)
getAllUtterancePref <- function(ratio) {
  allUttNum <- c(1:9)
  allUtterancePref <- matrix(allUttNum, length(allUttNum), 3)
  sampledRatio <- c(sample(ratio), sample(ratio), sample(ratio))
  repRatio <- c(ratio, ratio, ratio)
  for(utt in rep(1:length(allUtterancesNew1))){
    allUtterancePref[utt,1] <- utt
    allUtterancePref[utt,2] <- allUtterancesNew1[utt]
    # allUtterancePref[utt,3] <- as.numeric(sampledRatio[utt])
    allUtterancePref[utt,3] <- as.numeric(repRatio[utt])
  }
  return(allUtterancePref)
}
# allUtterancePref <- getAllUtterancePref(c(1e-10, 1e-5, 1))
#allUtterancePref <- getAllUtterancePref(c(1/3, 1/3, 1/3))

# WORK HERE!!!?!?!
# getMapUttToObjToPref <- function(currentObjects, allObjects, allUtterancePref, mapUttToObjProbs, allUtterancesNew1){
#   mapUttToObjToPref <- mapUttToObjProbs
#   for (obj in rep(1:length(currentObjects))){
#     object <- currentObjects[obj]
#     objectSpecific <- allObjects[object,]
#     for (featureValue in rep(1:3)){
#       pref <- allUtterancePref[which(allUtterancePref[,2] == objectSpecific[featureValue]), 3]
#       index <- which(relevantUtterances == which(objectSpecific[object] %in% allUtterancesNew1))
#       print(pref)
#       print(index)
#       print(objectSpecific[featureValue])
#       print(objectSpecific)
#       mapUttToObjToPref[index, obj] <- pref
#     }
#   }
#   return (mapUttToObjToPref)
# }
# 
# mapUttToObjToPref <- getMapUttToObjToPref(currentObjects, allObjects, allUtterancePref, mapUttToObjProbs, allUtterancesNew1)

# get a matrix of all preferences for the relevantUtterances
getMapUttToPref <- function(relevantUtterances, allObjects, allUtterancePref){
  mapUttToPref <- allUtterancePref
  allUtteranceNum <- c(1:9)
  notRelevantUtt <- setdiff(allUtteranceNum, relevantUtterances)
  for (rowNum in rep(1:length(notRelevantUtt))){
    mapUttToPref <- mapUttToPref[-which(mapUttToPref[,1] == notRelevantUtt[rowNum]),]
  }
  return (mapUttToPref)
}

# mapUttToPref <- getMapUttToPref(relevantUtterances, allObjects, allUtterancePref)


##
## Priors on object preferences - automatically derived from considered utterances
#    (i.e. derived from all relevant features)
# type == 0: hard priors; type > 0: soft prior with specified softness
# returns a list of preference priors for all considered features, i.e. utterances, 
# as well as for "no preference" whatsoever, i.e., uniform prior over all three objects
getObjectPreferencePriorsOriginal <- function(consideredUtterances, currentObjects, type, mapUttToObjProbs) {
  objectPreferenceHardPriors <- list()
  for(utt in rep(1:length(consideredUtterances)) ) {
    objectPreferenceHardPriors[[utt]] <- mapUttToObjProbs[utt,]
  }
  objectPreferenceHardPriors[[length(consideredUtterances)+1]] = 
    rep(1/length(currentObjects), length(currentObjects) )
  # soft preferences with uniform choice fusion. 
  softAddProb <- type
  objectPreferenceSoftPriors <- list()
  for(utt in rep(1:(length(consideredUtterances)+1)) ) {
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceHardPriors[[utt]] + softAddProb
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceSoftPriors[[utt]] / sum(objectPreferenceSoftPriors[[utt]])
  }
  return(objectPreferenceSoftPriors)
}

getObjectPreferencePriors <- function(consideredUtterances, currentObjects, type, mapUttToObjProbs, mapUttToPref) {
  objectPreferenceHardPriors <- list()
  for(utt in rep(1:length(consideredUtterances)) ) {
    objectPreferenceHardPriors[[utt]] <- mapUttToObjProbs[utt,]#*as.numeric(mapUttToPref[utt,3])
  }
  #nopreference case
  objectPreferenceHardPriors[[length(consideredUtterances)+1]] = rep(1/length(currentObjects), length(currentObjects) )
  
  #  soft preferences with uniform choice fusion. 
  softAddProb <- type
  objectPreferenceSoftPriors <- list()
  for(utt in rep(1:(length(consideredUtterances)+1)) ) {
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceHardPriors[[utt]] + softAddProb
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceSoftPriors[[utt]] / sum(objectPreferenceSoftPriors[[utt]])
  }
  return(objectPreferenceSoftPriors)
}

# creates a matrix containing all preferences for the object choice of the listener, 
# depending on the present objects and the target feature and the preferences
# of the listener
# targetfeature has to be numeric
# +1 row without preferences
getMapUttToObjToPref <- function(currentObjects, targetFeature, relevantUtterances, allUtterancePref, allObjects, mapUttToPref){
  isUnique <- matrix(FALSE, nrow = length(relevantUtterances)+1, ncol = length(currentObjects))
  mapUttToObjToPref <- matrix(0, nrow = length(relevantUtterances)+1, ncol = length(currentObjects))
  objectSpecific <- matrix("", nrow= 3, ncol = 3)
  for(obj in rep(1:3)){
  objectSpecific[obj,] <- allObjects[currentObjects[obj],]
  }
  countedUttObj <- table(objectSpecific)
  relevantUttWords <- relevantUtterances
  for(utt in rep(1:length(relevantUttWords))){
    relevantUttWords[utt] <- mapUttToPref[utt,2]
    index <- which(objectSpecific == relevantUttWords[utt], arr.ind=TRUE)[,"row"]
    for(ind in rep(1:length(index))){
      isUnique[utt,index[ind]] <- TRUE
    }
  }
  # cat(print(relevantUttWords))
  # cat(print(isUnique))
  for(row in rep(1:length(relevantUtterances))){
    for(col in rep(1:length(currentObjects))){
      if(isUnique[row,col]){
        targetFeatureValue <- objectSpecific[col,targetFeature]
        targetFeatureValuePref <- mapUttToPref[which(mapUttToPref[,2]==targetFeatureValue),3]
        mapUttToObjToPref[row,col] <- targetFeatureValuePref
      }
    }
  }
  # cat(print(isUnique))
  mapUttToObjToPref[length(relevantUtterances)+1,]<- 0.33
  if(targetFeature == "shape" || targetFeature == 1){
    notRelevantUtt <- c(1, 2, 3)
  } else  if(targetFeature == "pattern" || targetFeature == 2){
    notRelevantUtt <- c(4, 5, 6)
  } else  if(targetFeature == "color" || targetFeature == 3){
    notRelevantUtt <- c(7, 8, 9)
  }
  currentNotRelevantUtt <- which(as.numeric(mapUttToPref[,1]) %in% notRelevantUtt)
  for (row in rep(1:length(currentNotRelevantUtt))){
    mapUttToObjToPref[currentNotRelevantUtt[row],] <- 0
  }
  return (mapUttToObjToPref)
}

listenerObjChoice <- function(mapUttToObjToPref, utterance){
  return(which.max(mapUttToObjToPref[utterance,])[1])
}

# targetFeature <- 3
# mapUttToObjToPref <- getMapUttToObjToPref(currentObjects, targetFeature, relevantUtterances, allUtterancePref, allObjects, mapUttToPref)
