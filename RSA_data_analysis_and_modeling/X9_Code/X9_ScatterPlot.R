source("CommonCode/RSA_StratUtt.R")
source("CommonCode/getConstCodeStratUtt.R")

############################################
# simple RSA
############################################

#x9data <- read.csv("X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_iterative.csv")
x9data <- read.csv("X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_andOpt12_iterative.csv")

#x9data <- read.csv("X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_nonIterative.csv")
#x9data <- read.csv("X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_Opt12_nonIterative.csv")


# Take only the data from the last trial in each block

filterTrials <- 4 # if greater zero then only those %% filterTrials trials are taken
doAverageAmbiguityClasses <- FALSE

if (filterTrials > 0){
  trial4indices <- which((((x9data$X)-1)%%4) == (filterTrials-1))
  x9data <- x9data[trial4indices,]
}


#########################################################
# adding feature property codes (which feature was uttereed, which features were questioned)

uttFeat <- ifelse(x9data$utterance=="green" | x9data$utterance=="red" | x9data$utterance=="blue", 3,
                  ifelse(x9data$utterance=="solid" | x9data$utterance=="striped" | x9data$utterance=="polka-dotted", 2, 1))
x9data$uttFeat <- uttFeat
targetFeat <- x9data$targetFeatureNum


## adding the 1-27 target and object1, object2 & object3 code.
temp <- x9data$simulatedAnswer
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$targetOC27 <- targetOC27

temp <- x9data$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj1OC27 <- obj1OC27

temp <- x9data$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj2OC27 <- obj2OC27

temp <- x9data$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj3OC27 <- obj3OC27

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x9data$X),3)

for(i in c(1:length(x9data$X))) {
  subjectResponses[i,1] <- x9data$normResponse0[i] + 1e-100
  subjectResponses[i,2] <- x9data$normResponse1[i] + 1e-100
  subjectResponses[i,3] <- x9data$normResponse2[i] + 1e-100
  #  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3]) # Ella already normalized the data
}

## ordering the recorded subject responses such that they can be compared directly 
#   to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x9data$X),9)
for(i in c(1:length(x9data$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(targetFeat[i]-1)*3)] <- subjectResponses[i,j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)

## Reordering objects in input data

targetObject <- rep(NA, length(x9data$X))
object2 <- rep(NA, length(x9data$X))
object3 <- rep(NA, length(x9data$X))

for (i in 1:length(x9data$X)){
  if(targetOC27[i] == obj1OC27[i]){
    targetObject[i] <- targetOC27[i]
    object2[i] <- obj2OC27[i]
    object3[i] <- obj3OC27[i]
  } else if (targetOC27[i] == obj2OC27[i]) 
  {targetObject[i] <- obj2OC27[i]
  object2[i] <- obj1OC27[i]
  object3[i] <- obj3OC27[i]
  } else {
    targetObject[i] <- obj3OC27[i]
    object2[i] <- obj1OC27[i]
    object3[i] <- obj2OC27[i]
  }
}  



###########
## filtering for only the RELEVANT feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DataPost_1", colnames(x9data)) - 1
modelGuessIndexM1 <- grep("^Post1_1", colnames(x9data)) - 1
modelGuessIndexM2 <- grep("^Post2_1", colnames(x9data)) - 1

# for(i in c(1:nrow(x9data))) {
#   currentObjects <- c(targetObject[i], object2[i], object3[i])
#   relevantFeatures <- c((x9data$targetFeatureNum)*3 - 2,(x9data$targetFeatureNum)*3 - 1, (x9data$targetFeatureNum)* 3)
#   #  validUtterances <- determineValidUtterances(currentObjects)
#   #  for(j in c(1:3)) { # iterating over the three feature types
#   #  relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
#   #  valUttRel <- validUtterances[relevantIndices]
#   sumSG <- 0
#   sumMG <- 0
#   sumMG2 <- 0
#   for(x in c(1:length(relevantFeatures))) {
#     sumSG <- sumSG + x9data[[relevantFeatures[x]+subjectGuessIndexM1]][i] # column numbers of feature values present
#     sumMG <- sumMG + x9data[[relevantFeatures[x]+modelGuessIndexM1]][i]
#     sumMG2 <- sumMG2 + x9data[[relevantFeatures[x]+modelGuessIndexM2]][i]
#   }
#   if(!is.na(sumSG)) { # only for present feature values
#     for(x in c(1:length(relevantFeatures))) {
#       x9data[[relevantFeatures[x]+subjectGuessIndexM1]][i] <- x9data[[relevantFeatures[x]+subjectGuessIndexM1]][i] /
#         (sumSG + 1e-100)
#     }
#   }
#   for(x in c(1:length(relevantFeatures))) {
#     x9data[[relevantFeatures[x]+modelGuessIndexM1]][i] <- x9data[[relevantFeatures[x]+modelGuessIndexM1]][i] /
#       (sumMG + 1e-100)
#     x9data[[relevantFeatures[x]+modelGuessIndexM2]][i] <- x9data[[relevantFeatures[x]+modelGuessIndexM2]][i] /
#       (sumMG2 + 1e-100)
#   }
#   # setting the non-represented values to NA
#   for(v in c(1:9)) {
#     if(length(which(relevantFeatures == v)) == 0) {
#       x9data[[subjectGuessIndexM1 + v]][i] <- NA
#       x9data[[modelGuessIndexM1 + v]][i] <- NA
#       x9data[[modelGuessIndexM2 + v]][i] <- NA
#     }
#   }
# } #iterating over feature types
# 
# 


subjectGuessIndex <- grep("^DataPost_1", colnames(x9data))
modelGuessIndex1 <- grep("^Post1_1", colnames(x9data))
modelGuessIndex4 <- grep("^Post2_1", colnames(x9data))

if(doAverageAmbiguityClasses) {
  ## now determining the constellation code. 
  constellationCode <- matrix(0,length(x9data$X),6)
  uniqueCCode <- rep(0, length(x9data$X))
  featureOrder <- matrix(0, length(x9data$X), 3)
  objectOrder <- matrix(0, length(x9data$X), 3)
  for(i in c(1:length(x9data$X))) {
    objectConstellation <- c(targetObject[i],object2[i],object3[i]) 
    featChoice <- uttFeat[i]
    cc <- getConstellationCode(objectConstellation, featChoice)
    constellationCode[i,] <- cc[[1]]
    featureOrder[i,] <- cc[[2]]
    objectOrder[i,] <- cc[[3]]
    ## one-column code
    uc <- 0
    for(j in c(1:6)) {
      uc <- (uc * 10) + constellationCode[i,j]
    }
    uniqueCCode[i] <- uc
  }
  # feature order specified reordering of standard order (i.e. shape=1, texture=2, color=3)
  # object order specifies reordering of presented object order
  
  for(i in c(1:length(x9data$X))) {
    # reordering the feature order
    x9data[i,] <- replace(x9data[i,], c(subjectGuessIndex:(subjectGuessIndex+8)),  
                          x9data[i, c( (subjectGuessIndex + (featureOrder[i,1]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,1]-1)*3),
                                       (subjectGuessIndex+ (featureOrder[i,2]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,2]-1)*3),
                                       (subjectGuessIndex+ (featureOrder[i,3]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,3]-1)*3) )])
    x9data[i,] <- replace(x9data[i,], c(modelGuessIndex1:(modelGuessIndex1+8)),  
                          x9data[i, c( (modelGuessIndex1 + (featureOrder[i,1]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,1]-1)*3),
                                       (modelGuessIndex1+ (featureOrder[i,2]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,2]-1)*3),
                                       (modelGuessIndex1+ (featureOrder[i,3]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,3]-1)*3) )])
    x9data[i,] <- replace(x9data[i,], c(modelGuessIndex4:(modelGuessIndex4+8)),  
                          x9data[i, c( (modelGuessIndex4 + (featureOrder[i,1]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,1]-1)*3),
                                       (modelGuessIndex4+ (featureOrder[i,2]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,2]-1)*3),
                                       (modelGuessIndex4+ (featureOrder[i,3]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,3]-1)*3) )])
    ## now rearranging the individual feature values dependent on the object order (first object is the chosen one!)
    objectConstellation <- c(targetObject[i],object2[i],object3[i])
    objectCReordered <- replace(objectConstellation, c(1:3), objectConstellation[objectOrder[i,]])
    # Reordering of objects: target object stays in place, sometimes affects the ordering of 2nd and 3rd objects
    
    for(j in c(1:3)) {
      featValOrder <- rep(0,3)
      targetFeatureValue <- allObjectsToUtterancesMappings[objectCReordered[1],featureOrder[i,j]]
      featValOrder[1] <-  targetFeatureValue # target feature value comes first
      #    featValOrder[1] <-  targetFeat # something going on with feature ordering ...
      featValIndex <- 2
      for(k in c(2:3)) {
        objectFeatureValue <- allObjectsToUtterancesMappings[objectCReordered[k],featureOrder[i,j]]
        if(length(which(featValOrder==objectFeatureValue))==0) { # feature not included yet
          featValOrder[featValIndex] <- objectFeatureValue
          featValIndex <- featValIndex + 1
        }
      }
      if(featValIndex < 4) { # not all feature values assigned, yet -> fill up
        for(featVal in c(((featureOrder[i,j]-1)*3+1):((featureOrder[i,j]-1)*3+3))) {
          if(length(which(featValOrder==featVal))==0) { # feature not included yet
            featValOrder[featValIndex] <- featVal
            featValIndex <- featValIndex + 1
          }
        }
      }
      featValOrder <- featValOrder - (featureOrder[i,j]-1)*3 - 1
      ### now featValOrder specifies the feature value reordering for order feature with index j
      # reordering the feature value order of ordered feature j 
      x9data[i,] <- replace(x9data[i,], c(((j-1)*3 + subjectGuessIndex):(2+((j-1)*3 + subjectGuessIndex))),  
                            x9data[i, subjectGuessIndex + ((j-1)*3 + featValOrder)]) 
      x9data[i,] <- replace(x9data[i,], c(((j-1)*3 + modelGuessIndex1):(2+((j-1)*3 + modelGuessIndex1))),  
                            x9data[i, modelGuessIndex1 + ((j-1)*3 + featValOrder)]) 
      x9data[i,] <- replace(x9data[i,], c(((j-1)*3 + modelGuessIndex4):(2+((j-1)*3 + modelGuessIndex4))),  
                            x9data[i, modelGuessIndex4 + ((j-1)*3 + featValOrder)]) 
    }
  }
  x9data$CCode <- uniqueCCode
  #write.csv(x9data, "X9_Data/x9dataModelOptimizedSorted.csv")
  
  x9data <- x9data[order(x9data$CCode),]
  myCCodes <- unique(x9data$CCode)
  avDataMatrix <- matrix(0,length(myCCodes),19)
  dataPointIndex <- 0
  workerData <- 0
  rsaModel <- 0
  rsaModel2 <- 0
  for(i in c(1:length(myCCodes))) {
    cc <- myCCodes[i]
    cases <- which(x9data$CCode == cc)
    allPilotDataCases <- x9data[cases,]
    for(j in c(1:9)) {
      specCases <- which(is.na(allPilotDataCases[,subjectGuessIndex-1+j]) == FALSE)
      if(length(specCases) > 0) {
        #      if(mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)]) < 1/3 - 1e-5
        #         | mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)]) > 1/3 + 1e-5) {
        dataPointIndex <- dataPointIndex + 1
        workerData[dataPointIndex] <- mean(allPilotDataCases[specCases,(subjectGuessIndex-1+j)])
        rsaModel[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex1-1+j)])
        rsaModel2[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)])
        #      }
      }
    }
  }
  ### DONE with averaging over ambiguity classes (if selected) 
}else{ 
  ### generative worker and model data for all trials
  allPilotDataCases <- x9data
  workerData <- matrix(0,nrow(allPilotDataCases),3)
  rsaModel <- matrix(0,nrow(allPilotDataCases),3)
  rsaModel2 <- matrix(0,nrow(allPilotDataCases),3)
  targetFeatures <-  c( (x9data$targetFeatureNum)*3 - 2, (x9data$targetFeatureNum)*3 - 1, (x9data$targetFeatureNum)*3)
  relevatTargetFeatureIndices <- matrix(targetFeatures,nrow(x9data),3)
  for(row in c(1:nrow(allPilotDataCases))) {
    workerData[row,] <- as.vector(t(allPilotDataCases[row, (relevatTargetFeatureIndices + subjectGuessIndex - 1)[row,]]))
    rsaModel[row,] <- as.vector(t(allPilotDataCases[row, (relevatTargetFeatureIndices + modelGuessIndex1 - 1)[row,]]))
    rsaModel2[row,] <- as.vector(t(allPilotDataCases[row, (relevatTargetFeatureIndices + modelGuessIndex4 - 1)[row,]]))
  }
  workerData <- as.vector(workerData)
  rsaModel <- as.vector(rsaModel)
  rsaModel2 <- as.vector(rsaModel2)
}


### plot with default parameters ###
plot(rsaModel, workerData)
abline(lm(formula = rsaModel~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel~workerData)
summary(model)
confint(model)

### plot after Optimization ###

plot(rsaModel2, workerData)
abline(lm(formula = rsaModel2~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel2,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel2~workerData)
summary(model)
confint(model)




