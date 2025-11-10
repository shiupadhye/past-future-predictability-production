library(tidyverse)
library(ggpubr)
library(MuMIn)
library(ggplot2)
library(reshape2)
library(viridis)
library(lmtest)
library(doParallel)
library(patchwork)
library(relaimpo)
library(dominanceanalysis)

scores <- read_csv('SWBD_LexSubsAnalysisData.csv')
lexSubsAnnotated <- read_csv('SWBD_lexicalSubstitutions_samePOS_categorized.csv')


scores <- scores %>%
  mutate(relBwPred = infillBw_logProb - infillFw_logProb,
         uncondPMI = infillBw_logProb - unigram_logProb,
         condPMI = infill_logProb - infillFw_logProb)

scores <- scores %>%
  mutate(isProduced = case_when(
    type == 'error' ~ TRUE,
    type != 'error' ~ FALSE
  ))

scores <- merge(scores,lexSubsAnnotated[,c('subsID','category')],on='subsID')

scores <- scores %>%
  filter(!(subsID %in% c(138, 985)))

glm.isProduced.baseline <- glm(isProduced ~ unigram_logProb + infillFw_logProb + NoisySemDist + NoisyPhonDist, family = 'binomial',data=scores)
summary(glm.isProduced.baseline)

glm.isProduced.bw <- glm(isProduced ~ unigram_logProb + infillFw_logProb + infillBw_logProb + NoisySemDist + NoisyPhonDist, family = 'binomial',data=scores)
summary(glm.isProduced.bw)

glm.isProduced.uncondPMI <- glm(isProduced ~ unigram_logProb + infillFw_logProb + uncondPMI + NoisySemDist + NoisyPhonDist, family = 'binomial',data=scores)
summary(glm.isProduced.uncondPMI)

glm.isProduced.relBw <- glm(isProduced ~ unigram_logProb + infillFw_logProb + relBwPred + NoisySemDist + NoisyPhonDist, family = 'binomial',data=scores)
summary(glm.isProduced.relBw)

glm.isProduced.condPMI <- glm(isProduced ~ unigram_logProb + infillFw_logProb + condPMI + NoisySemDist + NoisyPhonDist, family = 'binomial',data=scores)
summary(glm.isProduced.condPMI)

glm.isProduced.both <- glm(isProduced ~ unigram_logProb + infillFw_logProb + relBwPred + condPMI + NoisySemDist + NoisyPhonDist, family = 'binomial',data=scores)
summary(glm.isProduced.both)

# LR test
lrtest(glm.isProduced.baseline,glm.isProduced.relBw)
lrtest(glm.isProduced.baseline,glm.isProduced.condPMI)
lrtest(glm.isProduced.baseline,glm.isProduced.both)

lrtest(glm.isProduced.relBw,glm.isProduced.condPMI)
lrtest(glm.isProduced.relBw,glm.isProduced.both)
lrtest(glm.isProduced.condPMI,glm.isProduced.both)

# BIC
BIC(glm.isProduced.baseline,glm.isProduced.relBw)
BIC(glm.isProduced.baseline,glm.isProduced.condPMI)
BIC(glm.isProduced.baseline,glm.isProduced.both)



