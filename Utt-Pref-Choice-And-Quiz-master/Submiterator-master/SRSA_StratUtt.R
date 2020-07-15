source("AllUtterancesAndObjects.R")

# simple KL divergence function
KLdivergence <- function(p, q) {
  toleranceOffset <- 1e-20
  return(max(0, sum(p * (log(
    (toleranceOffset + p) / (toleranceOffset + q)
  )))))
}

# The simple listener function
# determines the listener's object choice given the
# objects to choose from and its preferences, determining
# P(obj | utt, listener's object preferences)
simpleListener <-
  function(utterance,
           mapUttToObjProbs,
           listenerObjectPreferences) {
    objPosterior <-
      mapUttToObjProbs[utterance, ] * (listenerObjectPreferences + 1e-100)
    if (sum(objPosterior) == 0) {
      return(objPosterior)
    }
    return(objPosterior / sum(objPosterior))
  }

simpleListenerElla <-
  function(utterance,
           mapUttToObjToPref,
           listenerObjectPreferences) {
    objPosterior <-
      as.numeric(mapUttToObjToPref[utterance, ]) * (listenerObjectPreferences + 1e-100)
    if (sum(as.numeric(objPosterior)) == 0) {
      return(as.numeric(objPosterior))
    }
    print(as.numeric(objPosterior) / sum(as.numeric(objPosterior)))
    return(as.numeric(objPosterior) / sum(as.numeric(objPosterior)))
  }

#
# simpleListenerMy <- function(utterance, allObjectsToUtterancesMappings, currentObjects, allUtterancePref) {
#   possibleObjects <- which(allObjectsToUtterancesMappings[currentObjects,] == utterance,arr.ind=TRUE)[,1]
#   possibleObjFeatureV <- allObjectsToUtterancesMappings[currentObjects,][possibleObjects,]
#   possibleObjPref <- possibleObjFeatureV
#   for(pos in c(1:length(possibleObjFeatureV))) {
#     featureValue <- possibleObjFeatureV[pos]
#     possibleObjPref[pos] <- allUtterancePref[featureValue,3]
#   }
#   print("simpleListener")
#   print(possibleObjPref)
#   if(is.na(possibleObjPref[1])){
#     print("No valid utterance")
#     return (c(0,0,0,0,0,0,0,0,0))
#   }
#   if(sum(as.numeric(possibleObjPref ))==0) {
#       return(as.numeric(possibleObjPref ) )
#     }
#     return(as.numeric(possibleObjPref ) / sum(as.numeric(possibleObjPref )))
#   # objPosterior <- mapUttToObjProbs[utterance,] * (listenerObjectPreferences + 1e-100)
#   # print(objPosterior)
#   # if(sum(objPosterior)==0) {
#   #   return(objPosterior)
#   # }
#   # return(objPosterior / sum(objPosterior))
# }

# The simple pragmatic speaker considers all "imaginable" (i.e. implemented)
# preference distributions over objects of the listener.
# Starting with a prior assumption over the possible listener's preferences.
# It then infers the posterior over these preferences given the listener
# makes a particular object choice.
# utterance is an index refering to one of the relevantUtterances
# i.e. P(listener's feature value preferences | utterance, object choice by the listener, prior over preferences)
simplePragmaticSpeakerOri <-
  function(utterance,
           obj,
           preferencesPrior,
           relevantUtterances,
           currentObjects,
           mapUttToObjProbs,
           objectPreferenceSoftPriors) {
    prefPost <- rep(0, length(relevantUtterances) + 1)
    for (pref in c(1:length(preferencesPrior))) {
      # prior over the preferences the speaker is interested in
      if (preferencesPrior[pref] > 0) {
        pp <-
          simpleListener(utterance,
                         mapUttToObjProbs,
                         objectPreferenceSoftPriors[[pref]])
        prefPost[pref] <- pp[obj] * preferencesPrior[pref]
      }
    }
    if (sum(prefPost) == 0) {
      # no evidence for any preferences... -> no inference
      return(prefPost)
    }
    return(prefPost / sum(prefPost))
  }

simplePragmaticSpeaker <-
  function(utterance,
           obj,
           preferencesPriorAll,
           relevantUtterances,
           currentObjects,
           mapUttToObjProbs,
           objectPreferenceSoftPriors) {
    #cat("preferencesPriorAll", preferencesPriorAll, "\n")
    preferencesPrior <- preferencesPriorAll[relevantUtterances]
    prefPost <- rep(0, length(relevantUtterances) + 1)
    for (pref in c(1:length(preferencesPrior))) {
      # prior over the preferences the speaker is interested in
      if (preferencesPrior[pref] > 0) {
        pp <-
          simpleListener(utterance,
                         mapUttToObjProbs,
                         objectPreferenceSoftPriors[[pref]])
        #cat("pp", print(pp))
        prefPost[pref] <- pp[obj] * preferencesPrior[pref]
        #cat("pp[obj], preferencesPrior[pref]", pp[obj], preferencesPrior[pref])
        # cat("prefPost", prefPost)
        #cat("preferencesPrior", preferencesPrior)
        #cat(typeof(preferencesPrior[pref]))
      }
    }
    for (pos in c(1:length(relevantUtterances))) {
      preferencesPriorAll[relevantUtterances[pos]] <- prefPost[pos]
    }
    if (sum(preferencesPriorAll) == 0) {
      # no evidence for any preferences... -> no inference
      return(preferencesPriorAll)
    }
    return(preferencesPriorAll / sum(preferencesPriorAll))
    # if(sum(prefPost) == 0) { # no evidence for any preferences... -> no inference
    #   return(prefPost)
    # }
    # return(prefPost / sum(prefPost))
  }

#
# simplePragmaticSpeaker <- function(utterance, obj, preferencesPrior, #has to have the length of allUtterancesNew1
#                                    relevantUtterances, currentObjects,
#                                    objectPreferenceSoftPriors, allUtterancePref, mapUttToObjToPref) {
#   for(pref in c(1:length(preferencesPrior))) {# prior over the preferences the speaker is interested in
#     #print(preferencesPrior)
#     if(preferencesPrior[pref] > 0) {
#       # pp <- simpleListenerElla(utterance, mapUttToObjToPref, objectPreferenceSoftPriors[[pref]])
#       # pp <-  simpleListener(utterance, mapUttToObjProbs, objectPreferenceSoftPriors[[pref]])
#       pp <- simpleListener(utterance, allObjectsToUtterancesMappings, currentObjects, allUtterancePref)
#       preferencesPrior[pref] <- pp[obj] * preferencesPrior[pref]
#       print(pp)
#     }
#   }
#   #print(preferencesPrior)
#   if(sum(preferencesPrior) == 0) { # no evidence for any preferences... -> no inference
#     return(preferencesPrior)
#   }
#   return(preferencesPrior / sum(preferencesPrior))
# }


# Speaker utterance prior function (i.e. prior utterance preferences of the speaker)
getSpeakerUtteranceUniformPrior <- function(relevantUtterances) {
  return(rep(1. / length(relevantUtterances), length(relevantUtterances)))
}



# The ultimate function that determines the utterance preferences of a
# speaker, who wants to learn about the listener's preferences.
# That is, the speaker considers all relevant utterances given the currentObjects,
#   considers all prior feature value preferences (of the listener)
#   NOTE: this can be manipulated to make the speaker for example focus on one particular feature type preference
#         by setting the other feature value preferences to zero!
#   and all possible object choices
#   infers the resulting posterior feature value preferences of the listener in the particular scenario via simple RSA
#   computes the KL diverenve between the expected prior and inferred posterior feature value preferences
#   and finally determines the utility value for the considered utterance in the imagined scenario,
#    adding this utility to all scearios for each considered utterance.
# Essentially, U(utterances | listener's object preference priors) is computed.
# The utility is determined as the expected information gain between prior and posterior of the
#    determined listener's object preferences.
# @param preferencesPrior = probability mass over all feature values present in the scenario plus a "no preference" case
simpleBestInfGainUtterance <-
  function(preferencesPrior,
           relevantUtterances,
           currentObjects,
           mapUttToObjProbs,
           objectPreferenceSoftPriors,
           klValueFactor = 1) {
    postUttGPrefPrior <- rep(0, length(relevantUtterances))
    utterancePrior <-
      getSpeakerUtteranceUniformPrior(relevantUtterances) # prior over speaker utterances
    #
    for (utt in c(1:length(relevantUtterances))) {
      # evaluating the usage of a particular utterance utt
      prefPostAll <- rep(0, length(preferencesPrior))
      for (pref in c(1:length(preferencesPrior))) {
        # prior over the preferences the speaker is interested in
        ### What is the likelihood that this particular preference prior is the correct one?
        prefPost <- 0
        for (obj in c(1:length(currentObjects))) {
          if (mapUttToObjProbs[utt, obj] > 0) {
            if (preferencesPrior[pref] > 0) {
              # only pay attention to preferences with non-zero probability
              featurePrefsPosterior <-
                simplePragmaticSpeaker(
                  utt,
                  obj,
                  preferencesPrior,
                  relevantUtterances,
                  currentObjects,
                  mapUttToObjProbs,
                  objectPreferenceSoftPriors
                )
              # print(objPrefPosterior)
              KLvalue <-
                KLdivergence(preferencesPrior, featurePrefsPosterior)
              
              # log-likelihood interpretation of KLvalue:
              prefPost <-
                prefPost + mapUttToObjProbs[utt, obj] * utterancePrior[utt] *
                preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj] *
                exp(klValueFactor * KLvalue)
              
            }
          }
        }
        if (prefPost > 0) {
          prefPostAll[pref] <- prefPost
        }
      }
      postUttGPrefPrior[utt] <- sum(prefPostAll)
    }
    if (sum(postUttGPrefPrior) == 0)
      # no gain from any utterance...
      return(rep(1 / length(relevantUtterances), length(relevantUtterances)))
    return(postUttGPrefPrior / sum(postUttGPrefPrior))
  }



getAllObjectCodes <- function(allObjects, allUtterancesNew1) {
  allObjectCodes <- c(rep("000":length(allObjects[, 1])))
  for (shape in c(1:length(allObjects[, 1]))) {
    shapeNo <- which(allUtterancesNew1 == allObjects[shape, 1])
    allObjectCodes[shape] <- shapeNo * 100
  }
  for (texture in c(1:length(allObjects[, 2]))) {
    textureNo <- which(allUtterancesNew1 == allObjects[texture, 2]) - 3
    allObjectCodes[texture] <-
      allObjectCodes[texture] + (textureNo * 10)
  }
  for (color in c(1:length(allObjects[, 3]))) {
    colorNo <- which(allUtterancesNew1 == allObjects[color, 3]) - 6
    allObjectCodes[color] <- allObjectCodes[color] + colorNo
  }
  return(allObjectCodes)
}
#which(allObjectCodes%in%c(111,331,113))

getPreferencesPrior <- function(targetFeature) {
  preferencesPrior <- c(rep(0, 9))
  index <- targetFeature * 3
  indices <- c(index, index - 1, index - 2)
  preferencesPrior[indices] <- 1
  return(preferencesPrior / sum(preferencesPrior))
}

evaluate <-
  function(allUtterancePref,
           preferencesPrior,
           targetFeature) {
    index <- targetFeature * 3
    indices <- c(index - 2, index - 1, index)
    tarFeaPref <- allUtterancePref[indices, ]
    if (length(preferencesPrior) > 3){
    tarFeaPrefPrior <- preferencesPrior[indices]
    } else {tarFeaPrefPrior <- preferencesPrior}
    prefRank <-
      order(as.numeric(tarFeaPref[, 3]))#, ties.method = "first")
   # cat("prefRank", prefRank)
    prefPriorRank <-
      order(tarFeaPrefPrior) #, ties.method = "first")
   # cat("prefPriorRank", prefPriorRank)
    
    # if (prefRank == prefPriorRank){
    #   evalNum <- 3
    # } else if (prefRank == c(prefPriorRank[1],prefPriorRank[3],prefPriorRank[2]) || prefRank == c(prefPriorRank[2],prefPriorRank[1],prefPriorRank[3])){
    #   evalNum <- 2
    # }else if (prefRank == c(prefPriorRank[2],prefPriorRank[3],prefPriorRank[1]) || prefRank == c(prefPriorRank[3],prefPriorRank[1],prefPriorRank[2])){
    #   evalNum <- 1
    # }else if (prefRank == c(prefPriorRank[3],prefPriorRank[2],prefPriorRank[1])){
    #   evalNum <- 0
    # }
    
    if (identical(prefRank, prefPriorRank)) {
      evalNum <- 3
    } else if (identical(prefPriorRank, c(prefRank[1], prefRank[3], prefRank[2])) ||
               identical(prefPriorRank, c(prefRank[2], prefRank[1], prefRank[3]))) {
      evalNum <- 2
    } else if (identical(prefPriorRank, c(prefRank[2], prefRank[3], prefRank[1])) ||
               identical(prefPriorRank, c(prefRank[3], prefRank[1], prefRank[2]))) {
      evalNum <- 1
    } else if (identical(prefPriorRank, c(prefRank[3], prefRank[2], prefRank[1]))) {
      evalNum <- 0
    }
    return(evalNum)
  }


## Tests 1:
# notObeyInst <- 1e-10
# softPrefValue <- 0.1
# currentObjects <- c(1,9, 19)
# relevantUtterances <- determineValidUtterances(currentObjects)
# mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
# mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# mapUttToPref <- getMapUttToPref(relevantUtterances, allObjects, allUtterancePref)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects, softPrefValue, mapUttToObjProbs, mapUttToPref)
# allUtterancePref <- getAllUtterancePref(c(1e-10, 1e-5, 1))
# allUtterancePref
# simplePragmaticSpeaker(4, 1, c(0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                  mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
# preferencesPrior <- c(0, 0, 0, .2, .2, .2, 0, 0, 0)
# utterance <- 2
# obj <- 2
# preferencesPrior <- simplePragmaticSpeaker(utterance, obj, preferencesPrior, relevantUtterances, currentObjects,objectPreferenceSoftPriors, allUtterancePref, mapUttToObjToPref) # NON compliant listener...
# preferencesPrior <- simplePragmaticSpeaker(utterance, obj, preferencesPrior,relevantUtterances, currentObjects, mapUttToObjProbs, objectPreferenceSoftPriors)
# preferencesPrior
# # # Tests 2:
# # notObeyInst <- 0.1
# # softPrefValue <- .01
# # currentObjects <- c(1,2,6)
# # relevantUtterances <- determineValidUtterances(currentObjects)
# # mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
# mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
#                                                         softPrefValue, mapUttToObjProbs)
# # simpleBestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects,
# #                                 mapUttToObjProbs, objectPreferenceSoftPriors)
# simpleBestInfGainUtterance(c(0, 0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
# round(simpleBestInfGainUtterance(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6, 0), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors), 3) # sanity check - definite prior, no inf. gain possible
#
# # kldFact <- (c(0:200)-100)/2
# # kldRes <- matrix(0,length(kldFact),6)
# # for(i in c(1:length(kldFact))) {
# #   kldRes[i,] <- round(bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), relevantUtterances, currentObjects,
# #                              mapUttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i]), 3) # sanity check - definite prior, no inf. gain possible
# # }
# # plot(kldFact, kldRes[,1], ylim = c(0:1))
# # lines(kldFact, kldRes[,2], col="black")
# # lines(kldFact, kldRes[,3], col="grey")
# # lines(kldFact, kldRes[,4], col="yellow")
# # lines(kldFact, kldRes[,5], col="orange")
# # lines(kldFact, kldRes[,6], col="blue")
# #
# # bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), relevantUtterances, currentObjects,
# #                      mapUttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i])
# #
# # round(pragmaticSpeaker(4, 1, c(.1666, .1666, .1666, .1666, .1666, .1666, 0),
# #                              relevantUtterances, currentObjects, mapUttToObjProbs,
# #                              objectPreferenceSoftPriors, alpha), 3)
#
#
