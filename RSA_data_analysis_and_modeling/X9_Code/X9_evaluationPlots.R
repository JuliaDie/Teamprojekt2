source("X9_Code/Looped_testing.R")
  
roundingDigits <- 2
bothTables <-
    rbind(data.frame(table(inputDataCondensed$evalNum)), data.frame(table(inputDataCondensed$evalNumModel)))
  HumanOrModel = rep(c("Human", "Model"), each = length(bothTables$Freq) /
                       2)
  
tabledEvalNum <- data.frame(bothTables, HumanOrModel)
  totalTrialsWOFirstBlock <-
    sum(tabledEvalNum$Freq[HumanOrModel == "Human"])
  tabledEvalNum$relativeFreq <-
    round(tabledEvalNum$Freq / totalTrialsWOFirstBlock, digits = roundingDigits)
  
  
learningProcessDataWithFirstBlock <-
    data.frame(
      inputData$trialNum,
      inputData$blockNr,
      inputData$evalNum,
      inputData$evalNumModel
    )
  learningProcessData <-
    subset(learningProcessDataWithFirstBlock, inputData.blockNr != 0)
  summary(learningProcessData)
  humanLearningProcess <-
    as.data.frame(
      table(
        learningProcessData$inputData.trialNum,
        learningProcessData$inputData.evalNum
      )
    )
  humanLearningProcess$relativeFreq <-
    round(humanLearningProcess$Freq / totalTrialsWOFirstBlock,
          digits = roundingDigits)
  
  humanLearnProcPlot <-
    ggplot(data = humanLearningProcess, aes(x = Var1, y = relativeFreq, fill =
                                              Var2)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(
      aes(label = relativeFreq),
      vjust = -0.3,
      color = "black",
      position = position_dodge(0.9),
      size = 2
    ) +
    labs(
      title = "Human learning process, first block excluded\n",
      x = "Trial",
      y = "Frequency",
      fill = "Evaluation\nNumber\n"
    ) + coord_cartesian(ylim = c(0, 0.65))
  
  
modelLearningProcess <-
    as.data.frame(
      table(
        learningProcessData$inputData.trialNum,
        learningProcessData$inputData.evalNumModel
      )
    )

modelLearningProcess$relativeFreq <-
    round(modelLearningProcess$Freq / totalTrialsWOFirstBlock,
          digits = roundingDigits)
  
modelLearnProcPlot <-
    ggplot(data = modelLearningProcess, aes(x = Var1, y = relativeFreq, fill =
                                              Var2)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(
      aes(label = relativeFreq * 100),
      vjust = -0.3,
      color = "black",
      position = position_dodge(0.9),
      size = 2
    ) +
    labs(
      title = "Model learning process, first block excluded\n",
      x = "Trial",
      y = "Frequency",
      fill = "Evaluation\nNumber\n"
    ) + coord_cartesian(ylim = c(0, 0.65))
  
  
  # ____________________________________________________________________________________________________________________________
bothTablesWithFirstBlock <-
    rbind(data.frame(table(inputDataCondensed$evalNum)), data.frame(table(inputDataCondensed$evalNumModel)))
  HumanOrModel = rep(c("Human", "Model"),
                     each = length(bothTablesWithFirstBlock$Freq) /
                       2)
  HumanOrModelWithFirstBlock = rep(c("Human", "Model"),
                                   each = length(bothTablesWithFirstBlock$Freq) / 2)
  tabledEvalNumWithFirstBlock <-
    data.frame(bothTablesWithFirstBlock, HumanOrModelWithFirstBlock)
  
  totalTrialsWithFirstBlock <-
    sum(tabledEvalNumWithFirstBlock$Freq[HumanOrModel == "Human"])
  tabledEvalNumWithFirstBlock$relativeFreq <-
    round(tabledEvalNumWithFirstBlock$Freq / totalTrialsWithFirstBlock,
          digits = roundingDigits)
  
  
evalNumCompairPlotWithFirstBlock <-
    ggplot(data = tabledEvalNumWithFirstBlock,
           aes(x = Var1, y = relativeFreq, fill = HumanOrModelWithFirstBlock)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(
      aes(label = relativeFreq * 100),
      vjust = -0.3,
      color = "black",
      position = position_dodge(0.9),
      size = 2
    ) +
    labs(
      title = "Learning success compared, first block included\n",
      x = "Evaluation Number",
      y = "Frequency",
      fill = ""
    ) + coord_cartesian(ylim = c(0, 0.65))
  
  ggsave(
    filename = "evalNumCompairPlotWithFirstBlock.png" ,
    plot = evalNumCompairPlotWithFirstBlock,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )
  
  # __________________________________________________________________________
  
  for (datasetNum in c(1:3)) {
    if (datasetNum == 1) {
      dataset <- inputData
    } else if (datasetNum == 2) {
      dataset <- inputDataAmbiguous
    } else if (datasetNum == 3) {
      dataset <- inputDataNonAmbiguous
    }
    total <- length(dataset$workerid) %/% maxTrialNum
    learningProcessDataWithFirstBlock <-
      data.frame(dataset$trialNum,
                 dataset$blockNr,
                 dataset$evalNum,
                 dataset$evalNumModel)
    summary(learningProcessDataWithFirstBlock)
    colnames(learningProcessDataWithFirstBlock) <-
      c("trialNum", "blockNr", "evalNum", "evalNumModel")
    humanLearningProcessWithFirstBlock <-
      as.data.frame(
        table(
          learningProcessDataWithFirstBlock$trialNum,
          learningProcessDataWithFirstBlock$evalNum
        )
      )
    humanLearningProcessWithFirstBlock$relativeFreq <-
      round(humanLearningProcessWithFirstBlock$Freq / total,
            digits = roundingDigits)
    
    modelLearningProcessWithFirstBlock <-
      as.data.frame(
        table(
          learningProcessDataWithFirstBlock$trialNum,
          learningProcessDataWithFirstBlock$evalNumModel
        )
      )
    modelLearningProcessWithFirstBlock$relativeFreq <-
      round(modelLearningProcessWithFirstBlock$Freq / total,
            digits = roundingDigits)
    
    
    #_____________Line Plot learning trajectory all in one______________________________________________________________________________________
    
    
humanLearningProcessWithFirstBlock$datattype <- "human"
modelLearningProcessWithFirstBlock$datatype <- "model"
    
    colnames(humanLearningProcessWithFirstBlock) <-
      c("trialNum",
        "evalNum",
        "humanFreq",
        "relativeFreq",
        "datatype")
    colnames(modelLearningProcessWithFirstBlock) <-
      c("trialNum",
        "evalNum",
        "modelFreq",
        "relativeFreq",
        "datatype")
    
    
    titles <-
      c(
        "Learning Trajectory compared \nAll data",
        "Learning Trajectory compared \nOnly data from subjects who picked ambiguous utterances",
        "Learning Trajectory compared \nOnly data  from subjects who didn't pick ambiguous utterances"
      )
    
    
learningProcessLinePlot <- ggplot() +
      # blue plot
      geom_line(
        data = humanLearningProcessWithFirstBlock,
        aes(
          x = trialNum,
          y = relativeFreq,
          color = evalNum,
          group = evalNum,
          linetype = datatype
        )
      ) +
      # red plot
      geom_line(
        data = modelLearningProcessWithFirstBlock,
        aes(
          x = trialNum,
          y = relativeFreq,
          color = evalNum,
          group = evalNum,
          linetype = datatype
        )
      ) +
      coord_cartesian(ylim = c(0, 0.8))  +
      labs(
        title = titles[datasetNum],
        x = "Trial Number",
        y = "Frequency [%]",
        color = "Evaluation\nNumber\n",
        group = "Evaluation\nNumber\n",
        linetype = ""
      )
    
    ggsave(
      filename = paste(
        "learningProcessLinePlot",
        as.character(datasetNum),
        ".png" ,
        sep = ""
      ) ,
      plot = learningProcessLinePlot,
      width = 20,
      height = 20,
      units = "cm",
      dpi = 700
    )
  }
  
  #____________________________non-ambiguous vs ambiguous block -> evaluation number___________________________________________________________________________
  
humanAmbBlockEval <-
    as.data.frame(table(
      inputDataCondensed$ambiguousUtteranceCount,
      inputDataCondensed$evalNum
    ))
modelAmbBlockEval <-
    as.data.frame(
      table(
        inputDataCondensed$ambiguousUtteranceCount,
        inputDataCondensed$evalNumModel
      )
    )
  colnames(humanAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  colnames(modelAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  
ambiguityBlockEvaluation <- ggplot() +
    geom_line(
      humanAmbBlockEval,
      mapping = aes(
        x = evaluationNumber,
        y = Frequency,
        color = ambiguousUtteranceCount,
        group = ambiguousUtteranceCount
      )
    ) +
    geom_line(
      modelAmbBlockEval,
      mapping = aes(
        x = evaluationNumber,
        y = Frequency,
        color = ambiguousUtteranceCount,
        group = ambiguousUtteranceCount
      ),
      linetype = "dashed"
    ) + labs(
      title = "learning success depending on how many ambiguous \nutterances were picked in the block",
      x = "Evaluation Number",
      y = "Frequency",
      color = "Ambiguous \nUtterances \nper Block\n",
      group = "Ambiguous \nUtterances \nper Block\n"
    )
  
  ggsave(
    filename = "ambiguityBlockEvaluation.png" ,
    plot = ambiguityBlockEvaluation,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )
  
  #_______________RELATIVE_____________non-ambiguous vs ambiguous block -> evaluation number___________________________________________________________________________
  
  
  tabeledAmbUttCount <-
    summary(inputDataCondensed$ambiguousUtteranceCount)
  
  humanAmbBlockEval <-
    as.data.frame(table(
      inputDataCondensed$ambiguousUtteranceCount,
      inputDataCondensed$evalNum
    ))
  colnames(humanAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  humanAmbBlockEval$relativeFrequency <- humanAmbBlockEval$Frequency
  
  modelAmbBlockEval <-
    as.data.frame(
      table(
        inputDataCondensed$ambiguousUtteranceCount,
        inputDataCondensed$evalNumModel
      )
    )
  colnames(modelAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  modelAmbBlockEval$relativeFrequency <- modelAmbBlockEval$Frequency
  
  for (index in c(1:5)) {
    ambCount <- index - 1
    ambCountFreq <- tabeledAmbUttCount[[index]]
    for (row in c(1:length(humanAmbBlockEval$ambiguousUtteranceCount))) {
      if (humanAmbBlockEval$ambiguousUtteranceCount[row] == ambCount) {
        humanAmbBlockEval$relativeFrequency[row] <-
          humanAmbBlockEval$Frequency[row] / ambCountFreq
        modelAmbBlockEval$relativeFrequency[row] <-
          modelAmbBlockEval$Frequency[row] / ambCountFreq
      }
    }
  }
  
  
ambiguityBlockEvaluationRelative <- ggplot() +
    geom_line(
      modelAmbBlockEval,
      mapping = aes(
        x = evaluationNumber,
        y = relativeFrequency,
        color = ambiguousUtteranceCount,
        group = ambiguousUtteranceCount
      ),
    ) + labs(
      title = "learning success depending on how many ambiguous \nutterances were picked in the block",
      x = "Evaluation Number",
      y = "relative Frequency [%]",
      color = "Ambiguous \nUtterances \nper Block\n",
      group = "Ambiguous \nUtterances \nper Block\n"
    )
  
  ggsave(
    filename = "ambiguityBlockEvaluationRelative.png" ,
    plot = ambiguityBlockEvaluationRelative,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )
  
#______________________________Scatterplot Model&Human compared_______________________________________________
  
condensedComparePlot <-
    ggplot(inputDataCondensedCompare,
           mapping = aes(preferencesPrior, normResponse)) + # January 23, 2020, flipped axes
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Last trials of block \nAll data \n",
         x = "Model Predictions",
         y = "Human Data")

condensedAmbiguousComparePlot <-
    ggplot(inputDataCondensedAmbiguousCompare,
           mapping = aes(normResponse, preferencesPrior)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Last trials of block \nData from subjects who picked ambiguous utterances\n",
         x = "Human predictions",
         y = "Model Predictions")
  
condensedAmbiguousEqualComparePlot <-
    ggplot(
      inputDataCondensedAmbiguousEqualCompare,
      mapping = aes(normResponse, preferencesPrior)
    ) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Last trials of block \nData from subjects who picked ambiguous utterances \nEqual Evaluation Number",
         x = "Human predictions",
         y = "Model Predictions")
  
comparePlots <-
    arrangeGrob(
      condensedComparePlot,
      condensedAmbiguousComparePlot,
      condensedAmbiguousEqualComparePlot,
      ncol = 3
    )
  
  ggsave(
    filename = "comparePlots.png",
    plot = comparePlots,
    width = 40,
    height = 14,
    units = "cm",
    dpi = 700
  )
  
  # ____________________________________AMBIGUITY USE_________________________________________________________
  
ambiguityUsedArranged <- as.data.frame(table(ambiguityUsed[, 2]))
  ambiguityUsePlot <-
    ggplot(ambiguityUsedArranged, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", width = 0.2) +
    labs(
      title = "Use of ambiguous utterances (of 16 Trials)\n",
      x = "Number of Trials with ambiguous utterances",
      y = "Participants using ambiguous utterances (of 100)",
      fill = "Evaluation\nNumber\n"
    ) +
    coord_flip()
  
  ggsave(
    filename = "ambiguity.png",
    plot = ambiguityUsePlot,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )


##############################################################################
#____________________________stat testing_____________________________________
##############################################################################

library(ordinal)

# p > 0.05 => no learning effect
#         Estimate Std. Error z value Pr(>|z|)
# blockNr  0.12546    0.09453   1.327    0.184
model <-
  clmm(evalNum ~ blockNr + (1 | workerid), data = inputDataCondensed)
summary(model)

# no effect either
#                        Estimate Std. Error z value Pr(>|z|)
# Answer.time_in_minutes  0.02682    0.03966   0.676    0.499
model <-
  clmm(evalNum ~ Answer.time_in_minutes + (1 |
                                             workerid), data = inputDataCondensed)
summary(model)

# big effect!
# Estimate Std. Error z value Pr(>|z|)
# certainty   2.4671     0.5639   4.375 1.22e-05 ***
model <-
  clmm(evalNum ~ certainty + (1 | workerid), data = inputDataCondensed)
summary(model)

# big effect"
# Estimate Std. Error z value Pr(>|z|)
# certainty   2.3298     0.3762   6.193  5.9e-10 ***
model <-
  clmm(ambiguousUtteranceCount ~ certainty + (1 |
                                                workerid), data = inputDataCondensed)
summary(model)

# effect!
# Estimate Std. Error z value Pr(>|z|)
# blockNr 0.364284   0.003463   105.2   <2e-16 ***
model <-
  clmm(ambiguousUtteranceCount ~ blockNr + (1 |
                                              workerid), data = inputDataCondensed)
summary(model)
