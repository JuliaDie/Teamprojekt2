setwd("~/Documents/GitHub/prior_inference/RSA_2019_08")

#source("CommonCode/RSA_StratUtt.R")
#source("CommonCode/getConstCodeStratUtt.R")

### Creating a data frame with all model predictions and human data for experiment 4 ###

## Before you do it make sure that all the files with data are commented out in X4_AvScatterPlot.R ##
## Also comment out source files for the X4_AvScatterPlot.R. That reduces running time (or not...)

##################################### 1 ################################################

x9data <- read.csv("X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_iterative.csv") ###
source("X9_Code/X9_ScatterPlot.R")

########################################################################################



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "iterative"
data$softness <- 0
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m1 <-data
m1$Nr <- 1
remove(data)



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "iterative"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m2 <-data
m2$Nr <- 2
remove(data)

############################### 2 #####################################################

x9data <- read.csv("X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_andOpt12_iterative.csv") ###
source("X9_Code/X9_ScatterPlot.R")

########################################################################################



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "iterative"
data$softness <- 1
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m3 <-data
m3$Nr <- 3
remove(data)



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "iterative"
data$softness <- "globally_opt"
data$obedience <- "globally_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m4 <-data
m4$Nr <- 4
remove(data)


############################### 3 #####################################################

x9data <- read.csv("X9_Data/x9dataAugm_SRSAindOpt_fixed00_andOpt12_nonIterative.csv") ###
source("X9_Code/X9_ScatterPlot.R")

########################################################################################



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "non-iterative"
data$softness <- 0
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m5 <-data
m5$Nr <- 5
remove(data)



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "non-iterative"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m6 <-data
m6$Nr <- 6
remove(data)

############################### 4 #####################################################

x9data <- read.csv("X9_Data/x9dataAugm_SRSAglobalOpt_fixed10_Opt12_nonIterative.csv") ###
source("X9_Code/X9_ScatterPlot.R")

########################################################################################



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "non-iterative"
data$softness <- 1
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m7 <-data
m7$Nr <- 7
remove(data)



data <- matrix(NA,length(workerData),5)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","type")
data$type <- "non-iterative"
data$softness <- "globally_opt"
data$obedience <- "globally_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m8 <-data
m8$Nr <- 8
remove(data)




## Big data set ####

full <- rbind(m1,m2,m3,m4,m5,m6,m7,m8)
write.csv(full,"X9_Plots/for_scatterplots_updated.csv")
