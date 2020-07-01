source("CommonCode/AllUtterancesAndObjects.R")

x9data = read.csv(
  "X9_Data/ella_total_allDataCleaned.csv",
  header = TRUE,
  na.strings = c("", " ", "NA")
)

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

x9data$targetObject <- targetObject
x9data$object2 <- object2
x9data$object3 <- object3

utterance <- match(as.character(x9data$utterance),allUtterancesNew1)
x9data$utteranceNum <- utterance

write.csv(x9data, "X9_Data/ella_coded_data.csv")
