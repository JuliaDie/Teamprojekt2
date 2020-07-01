source("CommonCode/AllUtterancesAndObjects.R")

# simple KL divergence function- with small offset to tolerate p=0/q=0
KLdivergence <-
  function(p, q) {
    toleranceOffset <- 1e-20
    return(max(0, sum(p * (log(
      (toleranceOffset + p) / (toleranceOffset + q)
    )))))
  }

# The simple listener function determines the listener's object choice given the
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

# The simple pragmatic speaker considers all "imaginable" (i.e. implemented)
# preference distributions over objects of the listener.
# Starting with a prior assumption over the possible listener's preferences.
# It then infers the posterior over these preferences given the listener makes a particular object choice.
#
# utterance is an index refering to one of the relevantUtterances
# obj includes three object indices (numbers between 1 and 27)
# preferencesPrior gives a prior preferences distribution over the available object features and a uniform preference prior
#                     its length is thus the number of features present in the objects plus 1
# relevantUtterances is a vector of all present features
# currentObjects is a vector of three object indices (numbers between 1 and 27), that is, the objects presend in the scenario
# mapUttToObjProbs is a matrix where the rows map each possible utterance, which corresponds to each present feature value, 
#                   to the objects that may be chosen (reflecting the "obedience" parameter and which objects match the respective utterance)
# objectPreferenceSoftPriors is a list of vectors that reflect prior object choice preferences, given a particular feature is preferred
#                             thus, each vector reflects the choice with respect to one of the present featuers.
#                             while the last vector corresponds to "no feature preference"
#
# returns P(listener's feature value preferences | utterance, object choice by the listener, 
#                                                      prior over listener's feature value preferences)
simplePragmaticSpeaker <-
  function(utterance,
           obj,
           preferencesPrior,
           relevantUtterances,
           currentObjects,
           mapUttToObjProbs,
           objectPreferenceSoftPriors) {
    prefPost <- rep(0, length(preferencesPrior)) # NOTE: length(preferencesPrior == length(relevantUtterances) + 1
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
      return(preferencesPrior)
    }
    return(prefPost / sum(prefPost))
  }

# The simple pragmatic speaker considers all "imaginable" (i.e. implemented)
# preference distributions over objects of the listener.
# Starting with a prior assumption over the possible listener's preferences.
# It then infers the posterior over these preferences given the listener makes a particular object choice.
#
# utterance is an index refering to one of the relevantUtterances
# obj includes three object indices (numbers between 1 and 27)
# preferencesPriorAll gives a prior preferences distribution over all (nine) feature values. 
# relevantUtterances is a vector of all present features
# currentObjects is a vector of three object indices (numbers between 1 and 27), that is, the objects presend in the scenario
# mapUttToObjProbs is a matrix where the rows map each possible utterance, which corresponds to each present feature value, 
#                   to the objects that may be chosen (reflecting the "obedience" parameter and which objects match the respective utterance)
# objectPreferenceSoftPriors is a list of vectors that reflect prior object choice preferences, given a particular feature is preferred
#                             thus, each vector reflects the choice with respect to one of the present featuers.
#                             while the last vector corresponds to "no feature preference"
#
# returns P(listener's feature value preferences | utterance, object choice by the listener, 
#                                                      prior over listener's feature value preferences)
simplePragmaticSpeakerWithPrefPriorAll <-
  function(utterance,
           obj,
           preferencesPriorAll,
           relevantUtterances,
           currentObjects,
           mapUttToObjProbs,
           objectPreferenceSoftPriors) {
    #cat("preferencesPriorAll", preferencesPriorAll, "\n")
    preferencesPrior <- preferencesPriorAll[relevantUtterances]
    prefPost <- rep(0, length(relevantUtterances))
    for (pref in c(1:length(relevantUtterances))) {
      # prior over the preferences the speaker is interested in
      if (preferencesPrior[pref] > 0) {
        pp <-
          simpleListener(utterance,
                         mapUttToObjProbs,
                         objectPreferenceSoftPriors[[pref]])
        #print( cat("pp: ", pp))
        prefPost[pref] <- pp[obj] * preferencesPrior[pref]
        #cat("pp[obj], preferencesPrior[pref]", pp[obj], preferencesPrior[pref])
        # cat("prefPost", prefPost)
        #cat("preferencesPrior", preferencesPrior)
        #cat(typeof(preferencesPrior[pref]))
      }
    }
    if (sum(prefPost) == 0) { # no evidence for any preferences... -> no inference
      return(preferencesPriorAll)
    }
    # normalizing relevant posterior preferences such that the sum is equal to their prior probability mass 
    #   sum(preferencesPrior) is the probability mass of the full prior that we are "entitled" to redistribute because it concerns the features present in the trial
    #   prefPost / sum(prefPost) is the normalized posterior, so that the updated vector sums up to 1
    #   when we multiply, we redistribute the mass we are entitled to according to the prefPost we calculated above
    prefPost <- sum(preferencesPrior) * prefPost / sum(prefPost)
    # replacing the relevant old prior preferences values in preferencesPriorAll with their posteriors (which become the new priors)
    preferencesPriorAll[relevantUtterances] <- prefPost
    #
    return(preferencesPriorAll / sum(preferencesPriorAll))
  }


# Speaker utterance prior function (i.e. prior utterance preferences of the speaker)
getSpeakerUtteranceUniformPrior <- function(relevantUtterances) {
  return(rep(1. / length(relevantUtterances), length(relevantUtterances)))
}

# returns a uniform prior over the three features of the specified feature type 
#  targetFeature is a value between 1 and 3, specifying which feature  type is considered (for preferences)

getPreferencesPrior <- function(targetFeature) {
  preferencesPrior <- c(rep(0, 9))
  index <- targetFeature * 3
  indices <- c(index-2, index - 1, index)
  preferencesPrior[indices] <- 1
  return(preferencesPrior / sum(preferencesPrior))
}

# The ultimate function that determines the utterance preferences of a
# speaker, who wants to learn about the listener's preferences.
# That is, the speaker considers all relevant utterances given the currentObjects,
#   considers all prior feature value preferences (of the listener)
#   NOTE: this can be manipulated to make the speaker for example focus on one particular feature type preference
#         by setting the other feature value preferences to zero!
#   and all possible object choices
#   infers the resulting posterior feature value preferences of the listener in the particular scenario via simple RSA
#   computes the KL difference between the expected prior and inferred posterior feature value preferences
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
              #            print(preferencesPrior)
              #            print(featirePrefsPosterior)
              KLvalue <-
                KLdivergence(preferencesPrior, featurePrefsPosterior)
              
              # log-likelihood interpretation of KLvalue:
              prefPost <- prefPost +  mapUttToObjProbs[utt, obj] *
                objectPreferenceSoftPriors[[pref]][obj] *
                utterancePrior[utt] *  preferencesPrior[pref] *
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


######### Iterative utterance choice function ###########

simpleBestInfGainUtteranceWithPrefPriorAll <-
  function(preferencesPriorAll,
           relevantUtterances,
           currentObjects,
           mapUttToObjProbs,
           objectPreferenceSoftPriors,
           klValueFactor = 1,
           targetFeature,
           utterancePrior) {
    postUttGPrefPrior <- rep(0, length(relevantUtterances))
    preferencesPrior <- preferencesPriorAll[relevantUtterances]
    
    # posterior preferences over feature values: 3 dimensional array for simulated preferences
    #rows:utterances, columns: preferences, blocks:objects
    featurePrefsPosteriorAll <- array(0, c(length(relevantUtterances), length(preferencesPriorAll),3))
    # utterancePrior <-
    #   getSpeakerUtteranceUniformPrior(relevantUtterances) # prior over speaker utterances
    #
    for (utt in c(1:length(relevantUtterances))) {
      # evaluating the usage of a particular utterance utt
      prefPostAll <- rep(0, length(preferencesPrior))
      for (pref in c(1:length(preferencesPrior))) {
        # prior over the preferences the speaker is interested in
        ### What is the likelihood that this particular preference prior is the correct one?
        prefPost <- 0
        for (obj in c(1:length(currentObjects))) {
          if (utterancePrior[utt] > 0){
           if (mapUttToObjProbs[utt, obj] > 0) {
            if (preferencesPrior[pref] > 0) {
              # only pay attention to preferences with non-zero probability
              featurePrefsPosterior <-
                simplePragmaticSpeakerWithPrefPriorAll(
                  utt, obj, preferencesPriorAll, relevantUtterances,
                  currentObjects, mapUttToObjProbs, objectPreferenceSoftPriors
                )
              #            print(preferencesPrior)
              #            print(featirePrefsPosterior)
              KLvalue <-
                KLdivergence(preferencesPriorAll, featurePrefsPosterior)
              featurePrefsPosteriorAll[utt,,obj] <- featurePrefsPosterior
              #       return(featurePrefsPosteriorAll)
              # log-likelihood interpretation of KLvalue:
              prefPost <- prefPost +  mapUttToObjProbs[utt, obj] *
                objectPreferenceSoftPriors[[pref]][obj] *
                utterancePrior[utt] *  preferencesPrior[pref] *
                exp(klValueFactor * KLvalue)
            }
           }
          }
        }
        #        preferencesPriorAll <- featurePrefsPosterior
        
        if (prefPost > 0) {
          prefPostAll[pref] <- prefPost
        }
      }
      postUttGPrefPrior[utt] <- sum(prefPostAll)
    }
    
    ## Defining returns ##
    
    output1 <- list(utterancePrior,  featurePrefsPosteriorAll)
    posterior <- postUttGPrefPrior / sum(postUttGPrefPrior)
    output2 <- list(posterior, featurePrefsPosteriorAll)
    #    return(rep(1 / length(relevantUtterances), length(relevantUtterances)))
    if (sum(postUttGPrefPrior) == 0){# no gain from any utterance...
      return(output1)} # if no learning occurs, use uniform prior over available utterances. 
                      # Available utterances correspond to present feature values excluding utterances for target feature
                      # If the target feature is shape, 'square', 'circle', and 'cloud' are not available
    #return(postUttGPrefPrior / sum(postUttGPrefPrior))
    return(output2)
  }


###################################################

# ## Tests 1:
# notObeyInst <- 1e-10
# softPrefValue <- 0.1
# currentObjects <- c(1,2,3)
# relevantUtterances <- determineValidUtterances(currentObjects)
# mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
# mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
#                                                         softPrefValue, mapUttToObjProbs)
# #pragmaticSpeaker <- function(utterance, obj, preferencesPrior,
# #                             relevantUtterances, currentObjects, mapUttToObjProbs,
# #                             objectPreferenceSoftPriors) {
# simplePragmaticSpeaker(4, 1, c(0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                  mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
# simplePragmaticSpeaker(4, 1, c(.2, .2, .2, .2, .2, 0), relevantUtterances, currentObjects,
#                       mapUttToObjProbs, objectPreferenceSoftPriors) # NON compliant listener...
# simplePragmaticSpeakerWithPrefPriorAll(4, 1, c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9), relevantUtterances, currentObjects,
#                       mapUttToObjProbs, objectPreferenceSoftPriors) # NON compliant listener...

#
# # Tests 2:
 notObeyInst <- 0 
 softPrefValue <- 0.01
 currentObjects <- c(1,2,6)
 targetFeature <- 1
 klValueFactor <- 1
 relevantUtterances <- determineValidUtterances(currentObjects)
 mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
 mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
                                                             currentObjects,
                                                             mapObjToUtt, notObeyInst)
 objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
                                                         softPrefValue, mapUttToObjProbs)
 preferencesPriorAll <- getPreferencesPrior(1)
 
 utterancePrior <- rep(0,length(relevantUtterances))
 irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
 validUtterances <- relevantUtterances[-irrelevantIndices]
 utterancePriorShort <- rep (1/length(validUtterances),length(validUtterances)) 
 utterancePrior[-irrelevantIndices] <- utterancePriorShort

# # simpleBestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects,
# #                                 mapUttToObjProbs, objectPreferenceSoftPriors)
 simpleBestInfGainUtterance(c(0, 0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
                     mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
 round(simpleBestInfGainUtterance(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6, 0), relevantUtterances, currentObjects,
                     mapUttToObjProbs, objectPreferenceSoftPriors), 3) # sanity check - definite prior, no inf. gain possible

# ### Testing iterative utterance choice function ###
#
 simpleBestInfGainUtterance(c(1/3, 1/3, 1/3, 0, 0, 0), relevantUtterances, currentObjects,
                                  mapUttToObjProbs, objectPreferenceSoftPriors)

simpleBestInfGainUtteranceWithPrefPriorAll(preferencesPriorAll, relevantUtterances, 
                                           currentObjects, mapUttToObjProbs, objectPreferenceSoftPriors,
                                            klValueFactor, targetFeature, utterancePrior) #

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
