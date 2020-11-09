library(ggplot2)
library(lme4)
library(lmerTest)
library(hydroGOF)
library(tidyr)
library(dplyr)
library(Rmisc)

#setwd("~/Desktop/adjs!/kids-adjectives/experiments/1-kids-subjectivity/Submiterator-master")
#setwd("~/git/kids-adjectives/experiments/1-kids-subjectivity/Submiterator-master/")


num_round_dirs = 4
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/scopeTVJT-fixed.csv', sep='')) %>% #'round1/kids-subjectivity.csv')) %>% #for just 1
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","number","item","slide_number","context","response", "justification", "language"))
unique(d$language)

length(unique(d$workerid)) 
head(d)

## remove non-English speakers 
d = d[d$language!="Russian"&d$language!="",]
length(unique(d$workerid)) 

##Principled removal of participants
controltrials = subset(d, item %in% c('control1','control2','control3'))
contr1trial = subset(controltrials, item %in% c('control1'))
contr2trial = subset(controltrials, item %in% c('control2'))
contr3trial = subset(controltrials, item %in% c('control3'))
#descriptive stats to remove participants
qplot(contr1trial$response, geom="histogram")
qplot(contr2trial$response, geom="histogram")
qplot(contr3trial$response, geom="histogram")
contMean1 = mean(contr1trial$response)
contsd1 = sd(contr1trial$response)*2
contMean2 = mean(contr2trial$response)
contsd2 = sd(contr2trial$response)*2
contMean3 = mean(contr3trial$response)
contsd3 = sd(contr3trial$response)*2

## remove participants who failed control trials
to_omit1 = subset(contr1trial, contr1trial$response > (contMean1 + contsd1))
d = subset(d, !workerid %in% c(unique(to_omit1$workerid)))
to_omit2 = subset(contr2trial, contr2trial$response < (contMean2 - contsd2))
d = subset(d, !workerid %in% c(unique(to_omit2$workerid)))
to_omit3 = subset(contr3trial, contr3trial$response > (contMean3 + contsd3))
d = subset(d, !workerid %in% c(unique(to_omit3$workerid)))
length(unique(d$workerid))

## determine number of observations from each condition (less from cond2)
relevant_data = subset(d, item %in% c('frog','butterflies','lions','dinosaurs'))
table(relevant_data$context, relevant_data$number)
relevant_data$item = factor(relevant_data$item)

## Make histograms for each condition, and get data on control trials for removal
twowith_data = subset(relevant_data, number == "two" & context == "with")
twowithout_data = subset(relevant_data, number == "two" & context == "without")
fourwith_data = subset(relevant_data, number == "four" & context == "with")
fourwithout_data = subset(relevant_data, number == "four" & context == "without")

##Checking if animal counts were different
twowithout_animals = aggregate(response~item, mean, data=twowithout_data)
twowith_animals = aggregate(response~item, mean, data=twowith_data)
fourwith_animals = aggregate(response~item, mean, data=fourwith_data)
fourwithout_animals = aggregate(response~item, mean, data=fourwithout_data)

###Checking animal differences by item by condition
###rows: butterflies, dinosaurs, frogs, lions
###columns: twowithout, twowith, fourwithout, fourwith
animals = matrix(0,4,4);
animals[,1] = twowithout_animals$response
animals[,2] = twowith_animals$response
animals[,3] = fourwithout_animals$response
animals[,4] = fourwith_animals$response

table(relevant_data$item, relevant_data$context, relevant_data$number)

# ##checking to see if animal type matters for cond4 (it maybe does!)
# cond4_animals = aggregate(response~trial_num,mean, data=cond4_data)
# ggplot(cond4_data, aes(cond4_data$response)) + 
#   geom_histogram(color="black", binwidth = .033333) +
#   facet_grid(. ~ trial_num)



##Histograms of the 4 conditions
#cond1 - two frogs
qplot(twowith_data$response, geom="histogram")
#cond2 - two frogs w/ contrast
qplot(twowithout_data$response, geom="histogram")
#cond3 - four frogs
qplot(fourwith_data$response, geom="histogram")
#cond4 - four frogs w/ contrast
qplot(fourwithout_data$response, geom="histogram")


## calculate average response by condition
agg_resp = matrix(0,4,1);
agg_resp[1,1] = mean(twowithout_data$response)
agg_resp[2,1] = mean(twowith_data$response)
agg_resp[3,1] = mean(fourwithout_data$response)
agg_resp[4,1] = mean(fourwith_data$response)

##doing the LMM
library(nlme)
library(lme4)
par(mfrow = c(1,4))
hist(twowith_data$response, col = "grey", main = "TwoWith", xlab = NULL, breaks=20)
hist(twowithout_data$response, col = "grey", main = "TwoWithout", xlab = NULL, breaks=20)
hist(fourwith_data$response, col = "grey", main = "FourWith", xlab = NULL, breaks=20)
hist(fourwithout_data$response, col = "grey", main = "FourWithout", xlab = NULL, breaks=20)

##This works (greg showed me the syntax)
newLM = lmer(relevant_data$response ~ relevant_data$number * 
     relevant_data$context + (1+number+context|workerid)+(1+number+context|item),data=relevant_data)
summary(newLM)
subsetLM = lmer(response ~ 
               context + (1|workerid) + (1|item),data=relevant_data[relevant_data$number=="four",])
summary(subsetLM)
anova(newLM)

glht(newLM)

#predicting repsonse by number and context, randomly fixing workerid and item.

#, test = adjusted(type = "bonferroni"))

summary(LM)

par(mfrow = c(2,1))
plot(LM)

##for running the ANOVA - works
new_thing = aggregate(relevant_data$response,
                      by = list(relevant_data$workerid, relevant_data$number, relevant_data$context),
                      FUN = 'mean')

colnames(new_thing) = c("WorkerID","number", "context","response")

new_thing = new_thing[order(new_thing$WorkerID),]
head(new_thing)

new_thing.aov = with(new_thing, 
                     aov(new_thing$response ~ new_thing$number * new_thing$context + Error(new_thing$WorkerID/(new_thing$number * new_thing$context))))

summary(new_thing.aov)

TukeyHSD(new_thing.aov, "relevant_data$number")
posthoc = TukeyHSD(x=new_thing.aov, 'relevant_data$number', conf.level=0.95)

summary(glht(lme_velocity, linfct=mcp(Material = "Tukey")), test = adjusted(type = "bonferroni"))


##Getting a graph with error bars - not sure if this is correct
sum = summarySE(relevant_data,
                measurevar = "response",
                groupvars = c("number","context"))

#weird plot
ggplot(sum, aes(x=number,
                y=response,
                color=context)) +
  geom_errorbar(aes(ymin=response-se,
                    ymax=response+se),
                width=.2, size=0.7) +
  geom_point(shape=15, size=4) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust = 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "red"))

##Normal bar plot
##bootstrap 95% confidence intervals
#bootstrap confidence intervals
ggplot(sum, aes(x=number,
                y=response,
                fill=context,
                ymax=response+se,
                ymin=response-se)) +
  geom_bar(stat="identity", position = "dodge", width=0.7)+
  geom_bar(stat="identity", position = "dodge",
           colour="black",width=0.7,
           show.legend=FALSE) +
  scale_y_continuous(breaks = seq(0,1,.25),
                     limits = c(0,1),
                     expand = c(0,0)) +
  scale_fill_manual(name = "Count type" , 
                    values = c('grey80', 'grey30'), 
                    labels = c("with", 
                               "without"))  +
  geom_errorbar(position=position_dodge(width=0.7), 
                width=0.0, size=0.5, color="black")  +
  labs(x = "number",
       y = "response")  +
  ## ggtitle("Main title") + 
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5)
  )


  






## average score on control trials (most people correctly rejected)
agg_controls = aggregate(response~condition,mean,
                     data=subset(d, trial_num %in% c('4','6','8') ))

###Checking for duplicate Ids - there are none
ids = read.csv('WorkerIDs.csv')
length(unique(ids$Actual.ID))
n_occur <- data.frame(table(ids$Actual.ID))


## write to CSV files NAME THEM
#write.csv(agg_adj,"../results/.csv")
#write.csv(agg_class,"../results/.csv")

