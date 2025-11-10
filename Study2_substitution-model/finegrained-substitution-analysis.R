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


## Fine-grained error analysis
sem.error.scores <- scores %>%
  filter(category == 'semantic')

phon.error.scores <- scores %>%
  filter(category == 'phonological')

ms.error.scores <- scores %>%
  filter(category == 'morphosyntactic')

mixed.error.scores <- scores %>%
  filter(category == 'mixed')


### semantic errors
m.relbw.semScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + relBwPred  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=sem.error.scores)
summary(m.relbw.semScores)

m.condPMI.semScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + condPMI  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=sem.error.scores)
summary(m.condPMI.semScores)

### phonological errors
m.relbw.phonScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + relBwPred  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=phon.error.scores)
summary(m.relbw.phonScores)

m.condPMI.phonScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + condPMI  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=phon.error.scores)
summary(m.condPMI.phonScores)

### mixed errors
m.relbw.mixedScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + relBwPred  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=mixed.error.scores)
summary(m.relbw.mixedScores)

m.condPMI.mixedScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + condPMI  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=mixed.error.scores)
summary(m.condPMI.mixedScores)

### morphosyntactic errors
m.relbw.msScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + relBwPred  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=ms.error.scores)
summary(m.relbw.msScores)

m.condPMI.msScores <- glm(isProduced ~ unigram_logProb + infillFw_logProb + condPMI  + NoisySemDist + NoisyPhonDist, family = 'binomial',data=ms.error.scores)
summary(m.condPMI.msScores)

