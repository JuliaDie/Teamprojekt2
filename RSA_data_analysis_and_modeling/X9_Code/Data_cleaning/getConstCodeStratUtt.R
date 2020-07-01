source("AllUtterancesAndObjects.R")

# an object constellation is a 3-value vector \in {1,...,27]}^3
# the first object is the one that is chosen
# chosenFeature: specifies which one of the three feature values of the chosen object was used
#                as the utterance.
getConstellationCode <- function(objectConstellation, chosenFeature) {
  allFeatures <- c(allObjectsToUtterancesMappings[objectConstellation[1],],
                   allObjectsToUtterancesMappings[objectConstellation[2],],
                   allObjectsToUtterancesMappings[objectConstellation[3],])
  ## feature order
  featureOrder <- c(1:3)
  ##
  if(chosenFeature!=1) {
    featureOrder[chosenFeature] <- 1
    featureOrder[1] <- chosenFeature
  }
  ## object order
  objectOrder <- c(1:3)
  ## determining the number of matches wrt first, i.e., chosen object feature values
  matchF1 <- length(which(allFeatures == allFeatures[featureOrder[1]]))
  matchF2 <- length(which(allFeatures == allFeatures[featureOrder[2]]))
  matchF3 <- length(which(allFeatures == allFeatures[featureOrder[3]]))
  ## determining if feature values of the two others match (2=match)
  matchF1_23 <- ifelse(allFeatures[featureOrder[1]+3]==allFeatures[featureOrder[1]+6],2,1)
  matchF2_23 <- ifelse(allFeatures[featureOrder[2]+3]==allFeatures[featureOrder[2]+6],2,1)
  matchF3_23 <- ifelse(allFeatures[featureOrder[3]+3]==allFeatures[featureOrder[3]+6],2,1)
  ## 
  # print("Initialization of allFeatures, featureOrder, objectOrder, and feature matches:")
  # print(allFeatures)
  # print(featureOrder)
  # print(objectOrder)
  # print(c(matchF1, matchF1_23, matchF2, matchF2_23, matchF3, matchF3_23))
  ##
  ## Resulting Code String initialization and first values... 
  resultCode <- rep(0, 6)
  resultCode[1] <- matchF1  
  resultCode[2] <- matchF1_23 # redundant if matchF1==3 or ==2; if matchF1==1 then value indidates 1/2 if the other two do not match / match
  if(matchF1 == 2) {# exceptional case -> assure that second feature is the matching one 
    if(allFeatures[featureOrder[1]] == allFeatures[featureOrder[1]+6]) {
      # swap second and third object, such that the second object is the one that matches in the first feature
      objectConstellation <- replace(objectConstellation, c(2,3), objectConstellation[c(3, 2)]) 
      objectOrder <- replace(objectOrder, c(2,3), objectOrder[c(3,2)])
      allFeatures <-  replace(allFeatures, c(4:9), allFeatures[c(7:9,4:6)])
    }
  }
  # print("Result after handling the first feature:")
  # print(allFeatures)
  # print(featureOrder)
  # print(objectOrder)
  # print(resultCode)
  ##
  ## time to determine the second two values...
  #
  # -> first, possibly adjust feature order such that it is descending... 
  if(matchF2 < matchF3 | (matchF2 == matchF3 & matchF2_23 < matchF3_23)) {
    featureOrder <- replace(featureOrder, c(2,3), featureOrder[c(3,2)])
    temp <- matchF2
    matchF2 <- matchF3
    matchF3 <- temp
    temp <- matchF2_23
    matchF2_23 <- matchF3_23
    matchF3_23 <- temp
  }
  ##
  resultCode[3] <- matchF2  
  resultCode[4] <- matchF2_23 # redundant if matchF1==3; if matchF1==1 then value indidates 1/2 if the other two do not match / match
  if(matchF2 == 2) {# exceptional case -> assure proper object order...
    if(matchF1 == 2) { # object order alread defined -> need to distinguish 2a from 2b (i.e. Code 21/22)
      if(matchF3 == 2) { ## extreme case of all being 2s... might swap feature order}
        if(allFeatures[featureOrder[2]] == allFeatures[featureOrder[2]+6]
           & allFeatures[featureOrder[3]] == allFeatures[featureOrder[3]+3]) { 
          # case where once second and once third object matches with first in the other two features
          # assure that the second feature is the one that matches with the second object
          # since this is not the case currently -> swap the features!
          featureOrder <- replace(featureOrder, c(2,3), featureOrder[c(3,2)])
          temp <- matchF2
          matchF2 <- matchF3
          matchF3 <- temp
          temp <- matchF2_23
          matchF2_23 <- matchF3_23
          matchF3_23 <- temp
        }
      }
      # finally, for the "normal" 2,2 case, need to record the result code
      if(allFeatures[featureOrder[2]] == allFeatures[featureOrder[2]+6]) {
        resultCode[4] <- 2
      }
    }else{
      if(allFeatures[featureOrder[2]] == allFeatures[featureOrder[2]+6]) {
        # swap second and third object, such that the second object is the one that matches in the second feature
        objectConstellation <- replace(objectConstellation, c(2, 3), objectConstellation[c(3, 2)])      
        objectOrder <- replace(objectOrder, c(2,3), objectOrder[c(3,2)])
        allFeatures <-  replace(allFeatures, c(4:9), allFeatures[c(7:9,4:6)])
      }
    }
  }
  # print("Result after handling the second feature:")
  # print(allFeatures)
  # print(featureOrder)
  # print(objectOrder)
  # print(resultCode)
  resultCode[5] <- matchF3  
  resultCode[6] <- matchF3_23 # redundant if matchF1==3; if matchF1==1 then value indidates 1/2 if the other two do not match / match
  if(matchF3 == 2) {
    if(matchF1 != 2 & matchF2 != 2) { # final swap object case 
      if(allFeatures[featureOrder[3]] == allFeatures[featureOrder[3]+6]) {
        # swap second and third object, such that the second object is the one that matches in the third feature
        objectConstellation <- replace(objectConstellation, c(2, 3), objectConstellation[c(3, 2)])      
        objectOrder <- replace(objectOrder, c(2,3), objectOrder[c(3,2)])
        allFeatures <-  replace(allFeatures, c(4:9), allFeatures[c(7:9,4:6)])
      }
    }else{ ## order is already defined... thus, need to encode if second or third object matches with first (i.e. target) object in this case
      if(allFeatures[featureOrder[3]] == allFeatures[featureOrder[3]+6]) {
        resultCode[6] <- 2
      }
    }  
  }
  # print("Final result after handling the third feature:")
  # print(c("af:",allFeatures))
  # print(c("fo:",featureOrder))
  # print(c("oo:",objectOrder))
  # print(c("oc:",objectConstellation))
  #  print(resultCode)
  res <- list(resultCode, featureOrder, objectOrder)  
  return(res)
} 

# an object constellation is a 3-value vector \in {1,...,27]}^3
# this is the method that determines the utterance choice constellation code as well as the 
# resulting feature order dependent on the individual feature value ambiguities. 
getUtteranceChoiceConstellationCode <- function(objectConstellation) {
#  print(c("START with : ", objectConstellation))
  allFeatures <- c(allObjectsToUtterancesMappings[objectConstellation[1],],
                   allObjectsToUtterancesMappings[objectConstellation[2],],
                   allObjectsToUtterancesMappings[objectConstellation[3],])
  allUniqueFeatures <- sort(unique(allFeatures))
  #
  numShared <- rep(0,3) # maximum number of objects carying one feature value type
  for(i in c(1:3)) {
    numShared[i] = 4 - length(which(allUniqueFeatures > (i-1)*3 & allUniqueFeatures < 1+i*3))
  }
  # numShared specifies now the maximum number of feature values shared by each feature type
  # i.e. 1 means that all three feature values are present, 2 means that two are present, 3 means that only one value is present (shared by all three)
  allFeatNums <- rep(0,9) # vector of feature value occurrences for each object's feature values
  for(i in c(1:9)) {
    allFeatNums[i] <- length(which(allFeatures == allFeatures[i]))
  }
#  print(c(allFeatures,allFeatNums,allUniqueFeatures,numShared))
  ## feature order
  featureOrder <- c(1:3)
  ## object order
  objectOrder <- c(1:3)
  ## result code
  resultCode <- ""
  
  if(min(numShared)==3) {
    ## three common values (i.e. identical objects)
    resultCode <- "333"
  }else if(max(numShared)==1) {
    ## no identical features, all different values!
    resultCode <- "111"
  }else if(max(numShared)==3) { # at least one common feature value in all three features(but not three) ...
    if(length(which(numShared==3))==2) {
      # exactly (332 / 331) two features share identical values across all thee objects. 
      onetwoIndex <- which(numShared < 3)
      featureOrder[c(3,onetwoIndex)] <- featureOrder[c(onetwoIndex,3)]
      ### moving object with the lonely third feature value to the last position in the order
      if(length(which(numShared==2)) == 1) {
        if(length(which(allFeatures == allFeatures[featureOrder[2]]))==1) {
          # moving first object to last position
          objectOrder[c(1,3)] <- objectOrder[c(3,1)]
        }else if(length(which(allFeatures == allFeatures[featureOrder[3]]))==1) {
          # moving second object to last position
          objectOrder[c(2,3)] <- objectOrder[c(3,2)]
        }
        resultCode <- "332"
      }else{
        resultCode <- "331"# when the last feature is different in all three, there is no need to rearrange the objects.
      }
    }else{
      #### length(which(numShared==3))==1
      ## moving shared feature value to the first position
      threeIndex <- which(numShared==3)
      featureOrder[c(1,threeIndex)] <- featureOrder[c(threeIndex,1)] # swapping the feature order
      numShared[c(1,threeIndex)] <- numShared[c(threeIndex,1)]
      if(max(numShared[c(2,3)])==1) {
        ## done since both other features' feature values are uniqe to all objects
        resultCode <- "311"
      }else{
        # "322a/322b/321 case"
        #### move the object that shares a feature value with both others to position 1
        if(allFeatNums[featureOrder[2]]==2 & allFeatNums[featureOrder[3]]==2) {
          ## first one is the one that shares with both others.
        }else if(allFeatNums[3+featureOrder[2]]==2 & allFeatNums[3+featureOrder[3]]==2) {
          ## object order change...: move this object to the first position.
          objectOrder[c(1,2)] <- objectOrder[c(2,1)]
          allFeatNums[c(1,2,3,4,5,6)] <- allFeatNums[c(4,5,6,1,2,3)]
          allFeatures[c(1,2,3,4,5,6)] <- allFeatures[c(4,5,6,1,2,3)]
        }else if(allFeatNums[6+featureOrder[2]]==2 & allFeatNums[6+featureOrder[3]]==2) {
          ## object order change...: move this object to the first position.
          objectOrder[c(1,3)] <- objectOrder[c(3,1)]
          allFeatNums[c(1,2,3,7,8,9)] <- allFeatNums[c(7,8,9,1,2,3)]
          allFeatures[c(1,2,3,7,8,9)] <- allFeatures[c(7,8,9,1,2,3)]
        }
        ### 322a cases... need to make sure that the second object shares two values wth the first one.
        if(allFeatNums[6+featureOrder[2]]==2 & allFeatNums[6+featureOrder[3]]==2) {
          ## object order change...: move this object to the second position.
          objectOrder[c(2,3)] <- objectOrder[c(3,2)]
          allFeatNums[c(4,5,6,7,8,9)] <- allFeatNums[c(7,8,9,4,5,6)]
          allFeatures[c(4,5,6,7,8,9)] <- allFeatures[c(7,8,9,4,5,6)]
          resultCode <- "322a"
        }else if(allFeatNums[3+featureOrder[2]]==2 & allFeatNums[3+featureOrder[3]]==2) {
          # second object is 2-2, that is, shares both double values with the first one...
          # (third one has two unique values)
          resultCode <- "322a"
        }else if(min(numShared)==2) {
            resultCode <- "322b"
        }else{
          # case 321 ... move the object that does not have the 2-shared value feature to position three.
          if( ! (allFeatures[featureOrder[2]]==allFeatures[3+featureOrder[2]] | 
                 allFeatures[featureOrder[3]]==allFeatures[3+featureOrder[3]]) ) {
            objectOrder[c(2,3)] <- objectOrder[c(3,2)]
            allFeatNums[c(4,5,6,7,8,9)] <- allFeatNums[c(7,8,9,4,5,6)]
            allFeatures[c(4,5,6,7,8,9)] <- allFeatures[c(7,8,9,4,5,6)]
          }
          resultCode <- "321"
        }
      }
    }
  }else if(max(numShared)==2) {
    # 22a2a / 22a2b / 22b2b / 221 / 211
    # feature values shared by two and or one objects.
    if(length(which(numShared==1))==2) { ## two objects share one feature value, the other ones are unique.
      #211 case 
      ## move the feature type that shares feature values to the front
      twoIndex <- which(numShared==2)
      featureOrder[c(1,twoIndex)] <- featureOrder[c(twoIndex,1)] # swapping the feature order moving the shared feature type to the front
      ## move the object that does not share feature values with the other two to the back (third object)
      if(all(allFeatNums[1:3]==c(1,1,1))) { ## first object is the one with unique values -> reorder to being the third object
        objectOrder[c(1,3)] <- objectOrder[c(3,1)]
        allFeatNums[c(1,2,3,7,8,9)] <- allFeatNums[c(7,8,9,1,2,3)]
        allFeatures[c(1,2,3,7,8,9)] <- allFeatures[c(7,8,9,1,2,3)]
      }else if(all(allFeatNums[4:6]==c(1,1,1))) {## second object is the own with unique feature values -> reorder it to being the third object
        objectOrder[c(2,3)] <- objectOrder[c(3,2)]
        allFeatNums[c(4,5,6,7,8,9)] <- allFeatNums[c(7,8,9,4,5,6)]
        allFeatures[c(4,5,6,7,8,9)] <- allFeatures[c(7,8,9,4,5,6)]
      }
      resultCode <- "211"
    }else if(min(numShared)==1) {
      # 22a1 / 22b1 -> object constellation examples: 147|148|259 / 147|158|249
      oneSharedIndex <- which(numShared==1)
      featureOrder[c(3,oneSharedIndex)] <- featureOrder[c(oneSharedIndex,3)] # swapping the feature order moving the un-shared feature type to the back
      if(all(allFeatNums[1:3]==c(1,1,1))) {
        objectOrder[c(1,3)] <- objectOrder[c(3,1)]
        allFeatNums[c(1,2,3,7,8,9)] <- allFeatNums[c(7,8,9,1,2,3)]
        allFeatures[c(1,2,3,7,8,9)] <- allFeatures[c(7,8,9,1,2,3)]
        resultCode <- "22a1"
      }else if(all(allFeatNums[4:6]==c(1,1,1))) {
        objectOrder[c(2,3)] <- objectOrder[c(3,2)]
        allFeatNums[c(4,5,6,7,8,9)] <- allFeatNums[c(7,8,9,4,5,6)]
        allFeatures[c(4,5,6,7,8,9)] <- allFeatures[c(7,8,9,4,5,6)]
        resultCode <- "22a1"
      }else if(all(allFeatNums[7:9]==c(1,1,1))) {
        resultCode <- "22a1"
      }else if(sum(allFeatNums[7:9])==5) {
        objectOrder[c(1,3)] <- objectOrder[c(3,1)]
        allFeatNums[c(1,2,3,7,8,9)] <- allFeatNums[c(7,8,9,1,2,3)]
        allFeatures[c(1,2,3,7,8,9)] <- allFeatures[c(7,8,9,1,2,3)]
        resultCode <- "22b1"
      }else if(sum(allFeatNums[4:6])==5) {
        objectOrder[c(1,2)] <- objectOrder[c(2,1)]
        allFeatNums[c(1,2,3,4,5,6)] <- allFeatNums[c(4,5,6,1,2,3)]
        allFeatures[c(1,2,3,4,5,6)] <- allFeatures[c(4,5,6,1,2,3)]
        resultCode <- "22b1"
      }else if(sum(allFeatNums[1:3])==5) {
        resultCode <- "22b1"
      }
    }else if(length(which(numShared==2))==3) {
      # 22a2a / 22a2b / 22b2b -> object constellation examples: 147,147,258 / 147,148,257 / 147,158,257
      if(sum(allFeatNums[1:3])==3) {
        objectOrder[c(1,3)] <- objectOrder[c(3,1)]
        allFeatNums[c(1,2,3,7,8,9)] <- allFeatNums[c(7,8,9,1,2,3)]
        allFeatures[c(1,2,3,7,8,9)] <- allFeatures[c(7,8,9,1,2,3)]
        resultCode <- "22a2a"
      }else if(sum(allFeatNums[4:6])==3) {
        objectOrder[c(2,3)] <- objectOrder[c(3,2)]
        allFeatNums[c(4,5,6,7,8,9)] <- allFeatNums[c(7,8,9,4,5,6)]
        allFeatures[c(4,5,6,7,8,9)] <- allFeatures[c(7,8,9,4,5,6)]
        resultCode <- "22a2a"
      }else if(sum(allFeatNums[7:9])==3) {
        resultCode <- "22a2a"
      }else{
        # 22a2b / 22b2b
        # moving the one with all features shared to the front
        foundSix <- FALSE
        if(sum(allFeatNums[1:3])==6) {
          foundSix <- TRUE
        }else if(sum(allFeatNums[4:6])==6) {
          objectOrder[c(1,2)] <- objectOrder[c(2,1)]
          allFeatNums[c(1,2,3,4,5,6)] <- allFeatNums[c(4,5,6,1,2,3)]
          allFeatures[c(1,2,3,4,5,6)] <- allFeatures[c(4,5,6,1,2,3)]
          foundSix <- TRUE
        }else if(sum(allFeatNums[7:9])==6) {
          objectOrder[c(1,3)] <- objectOrder[c(3,1)]
          allFeatNums[c(1,2,3,7,8,9)] <- allFeatNums[c(7,8,9,1,2,3)]
          allFeatures[c(1,2,3,7,8,9)] <- allFeatures[c(7,8,9,1,2,3)]
          foundSix <- TRUE
        }
        ##
        if(sum(allFeatNums[7:9])==5 & foundSix) { # moving the one with 2 shared features to second
          objectOrder[c(2,3)] <- objectOrder[c(3,2)]
          allFeatNums[c(4,5,6,7,8,9)] <- allFeatNums[c(7,8,9,4,5,6)]
          allFeatures[c(4,5,6,7,8,9)] <- allFeatures[c(7,8,9,4,5,6)]
        }
        if(foundSix) {
          resultCode <- "22a2b"
          # we are at an allFeatNums constellation of 222,221,112 here!!!
          featSharedO13Index <- which(allFeatNums[c(7:9)]==2) # determine with feature is shared by the first and third object.
          featureOrder[c(3,featSharedO13Index)] <- featureOrder[c(featSharedO13Index,3)] # move that feature to the third position 
        }else{
          resultCode <- "22b2b" # ring of feature constellation.
          # move 221 to the front - i.e., the object that shares two values.
          if(all(allFeatNums[1:3] == c(2,2,1))) {
            ;
          }else if( all(allFeatNums[4:6] == c(2,2,1))) {
            objectOrder[c(1,2)] <- objectOrder[c(2,1)]
            allFeatNums[c(1,2,3,4,5,6)] <- allFeatNums[c(4,5,6,1,2,3)]
            allFeatures[c(1,2,3,4,5,6)] <- allFeatures[c(4,5,6,1,2,3)]
          }else if( all(allFeatNums[7:9] == c(2,2,1))) {
            objectOrder[c(1,3)] <- objectOrder[c(3,1)]
            allFeatNums[c(1,2,3,7,8,9)] <- allFeatNums[c(7,8,9,1,2,3)]
            allFeatures[c(1,2,3,7,8,9)] <- allFeatures[c(7,8,9,1,2,3)]
          }
          # finally, move 212 onto second
          if( all(allFeatNums[4:6] == c(2,1,2))) {
          }else if( all(allFeatNums[7:9] == c(2,1,2))) {
            objectOrder[c(2,3)] <- objectOrder[c(3,2)]
            allFeatNums[c(4,5,6,7,8,9)] <- allFeatNums[c(7,8,9,4,5,6)]
            allFeatures[c(4,5,6,7,8,9)] <- allFeatures[c(7,8,9,4,5,6)]
          }
        }
      }
    }
  }else{
    print("ERROR -- case not covered!???? ")
    print(objectConstellation)      
  }
  ###########################################
  ### finally, determining actual feature value order. 
  featureValueOrder <- rep(-1, length(allUniqueFeatures))
  featureValueOrder[1] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[1]],])[featureOrder[1]]  
  index <- 2
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[2]],])[featureOrder[1]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} # new feature value added
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[3]],])[featureOrder[1]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} # new feature value added
  # second feature... 
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[1]],])[featureOrder[2]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} # new feature value added
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[2]],])[featureOrder[2]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} # new feature value added
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[3]],])[featureOrder[2]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} # new feature value added
  # third feature...  
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[1]],])[featureOrder[3]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} # new feature value added
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[2]],])[featureOrder[3]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} # new feature value added
  featureValueOrder[index] <- (allObjectsToUtterancesMappings[objectConstellation[objectOrder[3]],])[featureOrder[3]]  
  if(length(which(featureValueOrder[1:(index-1)]==featureValueOrder[index])) == 0) {index <- index+1} else {featureValueOrder <- featureValueOrder[c(1:(length(featureValueOrder)-1))]} # new feature value added
  ## done :-)
#  print(c(numShared, allFeatNums))
  res <- list(resultCode, featureOrder, objectOrder, featureValueOrder)  
  return(res)
} 


# getUtteranceChoiceConstellationCode(c(1,5,12))


# codeFrame <- matrix(NA, 27**3, 11)
# colnames(codeFrame) <- c("o1", "o2", "o3", "code", "fOrder1", "fOrder2", "fOrder3", "oOrder1", "oOrder2", "oOrder3")
# codeFrame <- as.data.frame(codeFrame)
# codeFrame$objectNames <- matrix(0,27**3,9)
# for(i in c(1:27**3)) {
#   t <- i-1
#   codeFrame[[1]][i] <- 1+floor(t/729)
#   codeFrame[[2]][i] <- 1+floor((t%%729)/27)
#   codeFrame[[3]][i] <- 1+t%%27
#   res <- getUtteranceChoiceConstellationCode(c(codeFrame[[1]][i],codeFrame[[2]][i],codeFrame[[3]][i]))
#   codeFrame[[4]][i] <- res[[1]]
#   codeFrame[[5]][i] <- res[[2]][1]
#   codeFrame[[6]][i] <- res[[2]][2]
#   codeFrame[[7]][i] <- res[[2]][3]
#   codeFrame[[8]][i] <- res[[3]][1]
#   codeFrame[[9]][i] <- res[[3]][2]
#   codeFrame[[10]][i] <- res[[3]][3]
# }
# for(i in c(1:27**3)) {
#   t <- i-1
#   codeFrame[[1]][i] <- 1+floor(t/729)
#   codeFrame[[2]][i] <- 1+floor((t%%729)/27)
#   codeFrame[[3]][i] <- 1+t%%27
#   codeFrame$objectNames[i,] <- c(allObjects[codeFrame[[1]][i],],allObjects[codeFrame[[2]][i],],allObjects[codeFrame[[3]][i],])
# }  


