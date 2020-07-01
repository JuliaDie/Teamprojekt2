source("CommonCode/AllUtterancesAndObjects.R")

############## Functions from Ella's code #################################

evaluate <-
  function(allUtterancePref,
           preferencesPrior,
           targetFeature) {
    index <- targetFeature * 3
    indices <- c(index - 2, index - 1, index)
    tarFeaPref <- allUtterancePref[indices, ]
    if (length(preferencesPrior) > 3) {
      tarFeaPrefPrior <- preferencesPrior[indices]
    } else {
      tarFeaPrefPrior <- preferencesPrior
    }
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


isAmbiguous <-
  function(allPresentFeaValues,
           utteranceGeneral,
           currentObjects,
           targetFeatureNum) {
    ambiguous <- FALSE
    utteranceWord <- allUtterancesNew1[utteranceGeneral]
    currentObjectsUtterances <- allObjects[currentObjects, ]
    # if(str_count(allPresentFeaValues, toString(utteranceGeneral))>1){
    if (sum(allPresentFeaValues == utteranceGeneral) > 1) {
      ambiguous <- TRUE
    }
    if (ambiguous) {
      possibleObjectIndex <-
        which(currentObjectsUtterances == utteranceWord, arr.ind = TRUE)[, 1]
      possibleObjects <-
        currentObjectsUtterances[possibleObjectIndex, ]
      possibleObjectTarFeaValue <-
        possibleObjects[, targetFeatureNum]
      if (!length(unique(possibleObjectTarFeaValue)) > 1) {
        ambiguous <- FALSE
      }
    }
    return(ambiguous)
  }