library("dplyr")
library("ggplot2")

################################################## X 9 Prior inference ##################################

# Import full csv table

setwd("~/Documents/GitHub/prior_inference/RSA_2019_08/X9_Plots")
full <- read.csv("for_scatterplots_updated.csv")
head(full)

## See what models we have ##

stats <- full %>%
    group_by(softness,obedience,type, Nr) %>%
    summarise(n_occur = n())
ordered <- stats[order(stats$Nr),] # Sort the summary table
print.data.frame(ordered)

##########################################################################################################

# Subset data for plotting. Fill in the template

# plotData <- subset(full, softness ==  & obedience ==  & cross_validated == " " & type == "") # for simple models
# plotData <- subset(full, softness ==  & obedience ==  & alpha == & cross_validated == " " & type == "") # for full models

# Softness, obedience, and alpha are numeric
# Cross_validated can be "yes" or "no"
# Type can be "simpleRSA" or "fullRSA"
# Nr is model number in the order they are listed in the scatterplot file. 

##########################################################################################################

### Subsets for figures in the paper. Pick one plotData subset, then run the define variables part
### Then generate the plot either with ggplot or a simple plot



# for m1
plotData <- subset(full, Nr == 1) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Iterative \n Non-optimized")+
#  labs(title = bquote(atop
 #                     (.(type) ~"," ~ r^2 == .(r2),
 #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
 #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)


# for m2
plotData <- subset(full, Nr == 2) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Iterative \n Indivdually-optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)



# for m3
plotData <- subset(full, Nr == 3) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Iterative \n Fixed at 1_0")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)


# for m4
plotData <- subset(full, Nr == 4) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Iterative \n Globally-optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model_4 <- lm(formula = plotData$model~plotData$workerData)
summary(model)

plotData$workerid <- rep(1:95)
lmer_model_4 <- lmer(plotData$workerData~plotData$model +(1|plotData$workerid))

# for m5
plotData <- subset(full, Nr == 5) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Non-iterative \n Non-optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)


# for m6
plotData <- subset(full, Nr == 6) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Non-iterative \n Individually-optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)


# for m7
plotData <- subset(full, Nr == 7) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Non-iterative \n Fixed at 1_0")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

# for m8
plotData <- subset(full, Nr == 8) 
plotData$workerid <- rep(1:95)
lmer_model_8 <- lmer(plotData$workerData~plotData$model +(1|plotData$workerid))

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, color = "gray24") +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 9) +
  labs(title = "Non-iterative \n Globally-optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)

ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model_8 <- lm(formula = plotData$model~plotData$workerData)
summary(model)




#########################################################################################################


