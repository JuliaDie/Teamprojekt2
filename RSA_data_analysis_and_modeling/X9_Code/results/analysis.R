library(ggplot2)
library(reshape2)
library(lme4)
library(dplyr)

setwd("~/git/prior_inference/experiments/3-pilot-utterance-choice/Submiterator-master/")

source("../results/helpers.r")

num_round_dirs = 10
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/pilot-utterance-choice.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","item","slide_number","condition","language", "pref1",  "response1","pref2","response2","pref3","response3","pref4","response4","pref5","response5","pref6","response6","pref7","response7","pref8","response8","pref9","response9","obj1","obj2","obj3","ambiguous","numFeatures"))

# re-factorize
d[] <- lapply( d, factor) 

unique(d$language)

# only look at "español" as the native language
d = d[d$language=="english"|d$language=="English"|d$language=="English "|d$language=="ENGLISH",]

length(unique(d$workerid)) ## n=82

summary(d)

#write.csv(d,"../results/3-pilot-utterance-choice.csv")


## class plot
d_s = bootsSummary(data=t, measurevar="response", groupvars=c("class"))
# save data for aggregate plot
#write.csv(d_s,"~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/faultless.csv")

class_plot <- ggplot(d_s, aes(x=reorder(class,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("\nadjective class") +
  ylim(0,1) +
  theme_bw()
class_plot
#ggsave("../results/class_plot.pdf",height=3)

agr_pred = aggregate(response~predicate*class,data=t,mean)

#write.csv(agr_pred,"../results/pred-subjectivity.csv")