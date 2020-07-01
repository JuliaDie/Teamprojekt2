source("CommonCode/AllUtterancesAndObjects.R")

# simple KL divergence function 
KLdivergence <- function(p, q) {
  toleranceOffset <- 1e-20
  return(max(0, sum(p * (log( (toleranceOffset + p) / (toleranceOffset + q) ) ) ) ) )
}

#
# Modeling speaker and listener in the setting defined above.
#
# Literal listener function according to assigned listener's object preferences. 
# returns P(obj | utt, listener's object preferences)
literalListener <- function(utterance, listenerObjectPreferences, mapUttToObjProbs) {
  objPosterior <- mapUttToObjProbs[utterance,] # * (listenerObjectPreferences + 1e-100)
  if(sum(objPosterior)==0) {
    return(objPosterior)
  }
  return(objPosterior / sum(objPosterior))
}

# Speaker utterance prior function (i.e. prior utterance preferences of the speaker)
getSpeakerUtterancePriors <- function(relevantUtterances) {
  return(rep(1./length(relevantUtterances), length(relevantUtterances) ) )
}

# Speaker function -> return P(utt | obj, listener's object preferences) 
# Prefers to make those utterances that increase the likelihood of the listern
# to pick pich that particular object. 
# parameter alpha: exponential factor to increase the benefit of maximizing the most likely outcome
speaker <- function(obj, listenerObjectPreferences, relevantUtterances, mapUttToObjProbs, alpha) {
  priors <- getSpeakerUtterancePriors(relevantUtterances)  # prior over speaker utterances
  pugobj = rep(0, length(relevantUtterances)) 
  for (i in c(1:length(relevantUtterances))) { # considering all possible utterances
    #
    ll <- literalListener(i, listenerObjectPreferences, mapUttToObjProbs) # P_L0(obj | utterance i)
    #
    if(ll[obj]==0) {
      pugobj[i] <- 0
    }else{
      pugobj[i] <- exp(alpha * log(ll[obj]) - 0 ) * priors[i]
    }
    #print(c(i, psugs))
  }
  if(sum(pugobj) == 0) {
    return(pugobj)
  }
  return(pugobj / sum(pugobj) )
}

# The pragmatic listener function samples over the speaker 
# determining
# P(obj | utt, listener's object preferences)
# It essentially infers which object it should pick under a certain literal listener preference
pragmaticListener <- function(utterance, listenerObjectPreferences, 
                              relevantUtterances, currentObjects, mapUttToObjProbs, alpha) {
  pogutt <- rep(0, length(currentObjects))
  for(i in c(1:length(currentObjects))) {
    #    print(c("speaker with utt:", utterance, speaker(i, listenerObjectPreferences, relevantUtterances)))
    pogutt[i] <- speaker(i, listenerObjectPreferences, relevantUtterances, mapUttToObjProbs, alpha)[utterance] *
      (listenerObjectPreferences[i] + 1e-100) ## slight offset to prevent 0 cases    
  }
  #  print(c("PL:", pogutt, "LOP:", listenerObjectPreferences))
  if(sum(pogutt)==0) {
    return(pogutt)
  }
  return (pogutt / sum(pogutt))
}

# This pragmatic speaker considers all "imaginable" (i.e. implemented) 
# preference distributions over objects of the listener.
# Starting with a prior assumption over the possible listener's preferences. 
# It then infers the posterior over these preferences given the listener
# makes a particular object choice. 
# utterance is an index refering to one of the relevantUtterances 
# i.e. P(listener's feature value preferences | utterance, object choice by the listener, prior over preferences)
pragmaticSpeaker <- function(utterance, obj, preferencesPrior, 
                             relevantUtterances, currentObjects, mapUttToObjProbs,
                             objectPreferenceSoftPriors, alpha) {
  prefPost <- rep(0, length(relevantUtterances)+1)
  for(pref in c(1:length(preferencesPrior))) { # prior over the preferences the speaker is interested in
    if(preferencesPrior[pref] > 0) {
      #      print(c(utterance, objectPreferenceSoftPriors[[pref]]))
      pp <- pragmaticListener(utterance, objectPreferenceSoftPriors[[pref]],
                                          relevantUtterances, currentObjects, mapUttToObjProbs, alpha)
      prefPost[pref] <- pp[obj] * preferencesPrior[pref]
#      print(c(pref, pp, preferencesPrior))
    }
    #    print(c("PrefPost[]:", pref, prefPost[pref]))
  }
#  print(c("PrefPost:",prefPost))
#  print(c(utterance, obj, preferencesPrior, "rel", 
#          relevantUtterances,currentObjects))
#  print(mapUttToObjProbs)
#  print(objectPreferenceSoftPriors)
  if(sum(prefPost) == 0) { # no evidence for any preferences... -> keep believing in the prior!
    return(prefPost)
  }
  return(prefPost / sum(prefPost))
}




# The ultimate function that determines the utterance preferences of a rather 
#   "informed", "pragmatic" speaker considering essentially all possible scenarios. 
# That is, hypothetically, all utterances are considered, and the resulting
#   inferred listener's object preferences are computed assuming the listener
#   picks a certain object and has certain object preferences. 
# Essentially, U(utt | listener's object preference priors) is computed. 
# The utility is determined as the information gain between prior and posterior of the 
#    determined listener's object preferences.
# 
bestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects, 
                                 mapUttToObjProbs, objectPreferenceSoftPriors, alpha=1,
                                 klValueFactor=1) {
  postUttGPrefPrior <- rep(0, length(relevantUtterances))
  utterancePrior <- getSpeakerUtterancePriors(relevantUtterances) # prior over speaker utterances
  #
  for(utt in c(1:length(relevantUtterances))) { # evaluating the usage of a particular utterance utt
    prefPostAll <- rep(0, length(preferencesPrior))
    for(pref in c(1:length(preferencesPrior))) { # prior over the preferences the speaker is interested in
      ### What is the likelihood that this particular preference prior is the correct one?
      prefPost <- 0
      for(obj in c(1:length(currentObjects)) ) {
        if(mapUttToObjProbs[utt,obj] > 0) {
          if(preferencesPrior[pref] > 0) { # only pay attention to preferences with non-zero probability
            objPrefPosterior <- pragmaticSpeaker(utt, obj, preferencesPrior, 
                                                 relevantUtterances, currentObjects, 
                                                 mapUttToObjProbs, objectPreferenceSoftPriors, alpha)
            # print(objPrefPosterior)
            KLvalue <- KLdivergence(preferencesPrior, objPrefPosterior)
 #           if(KLvalue > 0) {
              # log-likelihood interpretation of KLvalue:
              prefPost <- prefPost + mapUttToObjProbs[utt,obj] * utterancePrior[utt] * 
                                              preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj] *
                                     exp(klValueFactor * KLvalue)
#              prefPost <- prefPost + exp( log(mapUttToObjProbs[utt,obj] * utterancePrior[utt] * 
#                                                preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj]) +
#                                            klValueFactor * KLvalue)
              
              #              print(c(utt,KLvalue, exp(klValueFactor * KLvalue), prefPost))
              # direct likelihood interpretation of KLvalue
#                            prefPost<- prefPost + mapUttToObjProbs[utt,obj] * utterancePrior[utt] * 
#                              preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj] * klValueFactor * KLvalue
            }
            #            prefPost <- prefPost + KLvalue #utterancePrior[utt] *
            #              objPrefPosterior[pref] * 
            #             objectPreferenceSoftPriors[[pref]][obj]
 #         }
        }
      }
      if(prefPost > 0) {
        prefPostAll[pref] <- prefPost
      }
    }
    postUttGPrefPrior[utt] <- sum(prefPostAll)
  }
  if(sum(postUttGPrefPrior) == 0) # no gain from any utterance... 
    return( rep(1/length(relevantUtterances), length(relevantUtterances)) )
  return(postUttGPrefPrior / sum(postUttGPrefPrior))
}

# ## Tests 1:
# notObeyInst <- 1e-10
# softPrefValue <- 0.1
# alpha <- .1
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
# #                             objectPreferenceSoftPriors, alpha) {
# pragmaticSpeaker(4, 1, c(0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects, 
#                  mapUttToObjProbs, objectPreferenceSoftPriors, alpha) # sanity check - definite prior, no inf. gain possible 
# pragmaticSpeaker(4, 1, c(.2, .2, .2, .2, .2, 0), relevantUtterances, currentObjects, 
#                  mapUttToObjProbs, objectPreferenceSoftPriors, alpha) # NON compliant listener... 
# 
## Tests 2:
# notObeyInst <- 0
# softPrefValue <- .01
# alpha <- 1
# currentObjects <- c(1,2,6)
# relevantUtterances <- determineValidUtterances(currentObjects)
# mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
# mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
#                                                         softPrefValue, mapUttToObjProbs)
#bestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects,
#                                 mapUttToObjProbs, objectPreferenceSoftPriors, alpha)
#bestInfGainUtterance(c(0, 0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors, alpha) # sanity check - definite prior, no inf. gain possible
#round(bestInfGainUtterance(c(.2, .2, .2, .2, .2, .2, 0), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors, alpha, -10), 3) # sanity check - definite prior, no inf. gain possible
# 
# kldFact <- (c(0:200)-100)/2
# kldRes <- matrix(0,length(kldFact),6)
# for(i in c(1:length(kldFact))) {
#   kldRes[i,] <- round(bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), relevantUtterances, currentObjects,
#                              mapUttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i]), 3) # sanity check - definite prior, no inf. gain possible
# }
# plot(kldFact, kldRes[,1], ylim = c(0:1))
# lines(kldFact, kldRes[,2], col="black")
# lines(kldFact, kldRes[,3], col="grey")
# lines(kldFact, kldRes[,4], col="yellow")
# lines(kldFact, kldRes[,5], col="orange")
# lines(kldFact, kldRes[,6], col="blue")
# 
# bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), relevantUtterances, currentObjects,
#                      mapUttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i])
# 
# round(pragmaticSpeaker(4, 1, c(.1666, .1666, .1666, .1666, .1666, .1666, 0), 
#                              relevantUtterances, currentObjects, mapUttToObjProbs,
#                              objectPreferenceSoftPriors, alpha), 3)
# 
  
