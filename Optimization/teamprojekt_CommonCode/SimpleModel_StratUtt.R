##########################################
## 
source("CommonCode/AllUtterancesAndObjects.R")
source("CommonCode/getConstCodeStratUtt.R")

# par is the parameter that determines how more likely the participant likes the features values of the chosen object
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat,
#                                                          7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
choiceBasedModelKLDiv1param <- function(par, data) {
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    modelPredProbs <- getFeaturePrefsChoiceBasedModel(currentObjects, uttFeat, par, par)
    ##
    ft <- data[i,c(5,6)]
    ##
    validUtterances <- which(!is.na(modelPredProbs))
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers. 
    ##
    ## answer set 1 with feature type data[i,5]
    #
    # considered relevant indices in validUtterances array
    for(ftIndex in c(1,2)) {
      dataOffset = 3 + 3 * ftIndex
      relevantIndices <- which(validUtterances>(3*(ft[ftIndex]-1)) & validUtterances<(3*ft[ftIndex] + 1)) # relevant indices for a particular feature type
      # the actual relevant utterances relative to the type of utterance that was chosen (\in {1,2,3})
      relIndicesRel <- validUtterances[relevantIndices] - (3*(ft[ftIndex]-1))
      # normalizing to one
      data[i,dataOffset+relIndicesRel] <- data[i,dataOffset+relIndicesRel] / (sum(data[i,dataOffset+relIndicesRel]) + 1e-100)
      # determining respective KL divergence values
      for(j in c(1:length(relevantIndices))) {
        llRes <- llRes + data[i, dataOffset+relIndicesRel[j]] * 
          ( log(data[i, dataOffset+relIndicesRel[j]] + 1e-100) - log(modelPredProbs[validUtterances[relevantIndices[j]]] + 1e-100)  )  
      }
    }
  }
  return(llRes)
}

# par contains two parameters that determine how more likely the participant likes the features values 
# of the chosen object compared to the others -
#   --- params[1] in the case of two feature values for a particular feature type 
#   --- params[2] in the case of three feature values for a particular feature type
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat,
#                                                          7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
choiceBasedModelKLDiv2params <- function(params, data) {
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    modelPredProbs <- getFeaturePrefsChoiceBasedModel(currentObjects, uttFeat, params[1], params[2])
    ##
    ft <- data[i,c(5,6)]
    ##
    validUtterances <- which(!is.na(modelPredProbs))
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers. 
    ##
    ## answer set 1 with feature type data[i,5]
    #
    # considered relevant indices in validUtterances array
    for(ftIndex in c(1,2)) {
      dataOffset = 3 + 3 * ftIndex
      relevantIndices <- which(validUtterances>(3*(ft[ftIndex]-1)) & validUtterances<(3*ft[ftIndex] + 1)) # relevant indices for a particular feature type
      # the actual relevant utterances relative to the type of utterance that was chosen (\in {1,2,3})
      relIndicesRel <- validUtterances[relevantIndices] - (3*(ft[ftIndex]-1))
      # normalizing to one
      data[i,dataOffset+relIndicesRel] <- data[i,dataOffset+relIndicesRel] / (sum(data[i,dataOffset+relIndicesRel]) + 1e-100)
      # determining respective KL divergence values
      for(j in c(1:length(relevantIndices))) {
        llRes <- llRes + data[i, dataOffset+relIndicesRel[j]] * 
          ( log(data[i, dataOffset+relIndicesRel[j]] + 1e-100) - log(modelPredProbs[validUtterances[relevantIndices[j]]] + 1e-100)  )  
      }
    }
  }
  return(llRes)
}

## determines the choice-based feature preference inference model predictions. 
# parameter specifies the strength of the chosen object's feature values. 
# sets NAs for those feature values that are not presend in the currentObjects. 
getFeaturePrefsChoiceBasedModel <- function(currentObjects, uttFeat, modelParameter2f, modelParameter3f) {
  validUtterances <- determineValidUtterances(currentObjects)
  featurePrefs <- rep(NA,9)
  for(ft in c(1:3)) {
    # considered relevant indices in validUtterances array
    relevantIndicesInValUtt <- which(validUtterances>(3*(ft-1)) & validUtterances<(3*ft + 1)) # relevant indices for a particular feature type
    # the actual relevant utterances relative to the type of utterance that was chosen (\in {1,2,3})
    relevantFeatureValues <- validUtterances[relevantIndicesInValUtt]
    # chosen object's feature value
    objectFeat <- (allObjectsToUtterancesMappings[currentObjects[1],])[ft]
    # relative chosen object's feature value index wrt the relevantIndicesInValUtt array
    relObjectFeat <- which(validUtterances[relevantIndicesInValUtt]==objectFeat)
    if(length(relevantIndicesInValUtt) == 3) { # there are three feature values to choose from for one feature type.
      modelPrefVals <- rep((1 - modelParameter3f) / 2, 3)
      modelPrefVals[relObjectFeat] <- modelParameter3f
    }else if(length(relevantIndicesInValUtt) == 2) { # there are multiple feature values to choose from for one feature type.
        modelPrefVals <- rep((1-modelParameter2f), 2)
        modelPrefVals[relObjectFeat] <- modelParameter2f
    }else{
      modelPrefVals <- c(1)
    }
    featurePrefs <- replace(featurePrefs, relevantFeatureValues, modelPrefVals)
  }
#  # debugging check for unassigned values.   
#  featurePrefsIndices <- !is.na(featurePrefs)
#  if( length( which(featurePrefs[featurePrefsIndices]>1 ))) {
#    print(c(featurePrefs, 999, currentObjects, 999, uttFeat, 999, modelParameter2f, modelParameter3f))
#  }
  return(featurePrefs)
}



# data is a matrix with data rows.
# determining the KL divergence for the relevant indices only, that is, 
# for each trial only for those feature values that are present in the presented three objects in the respective trial.
UniformModelKLDiv <- function(data) {
  llRes <- 0
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
      llRes <- llRes + data[i, 6+relIndicesRel[j]] * 
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
      llRes <- llRes + data[i, 9+relIndicesRel[j] ] * 
        ( log(data[i, 9+relIndicesRel[j] ] + 1e-100) - log( 1 / length(relevantIndices) ) ) 
    }
  }
  return(llRes)
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

