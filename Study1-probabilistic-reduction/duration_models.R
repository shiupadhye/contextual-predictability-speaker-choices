library(car)
library(tidyverse)
library(lme4)
library(ggsignif)
library(ggpubr)
library(lmtest)
library(patchwork)
library(cowplot)
library(emmeans)
library(relaimpo)
library(partR2)
library(ggeffects)
library(lmerTest)
library(dominanceanalysis)

scores <- read_csv('SWBD_DurAnalysisData.csv')
scores <- scores %>% filter(Class != 'OTHER')

# define backward predictability variants
scores$relative_bwPred <- scores$infillBw_logProb - scores$infillFw_logProb
scores <- scores %>% rename(condPMI = infill_pmiFP)

# Comparison of Futue Context Predictability Measures
m.baseline <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb  + (1|speaker_id), data = scores)
summary(m.baseline)

m.bw <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + infillBw_logProb + (1|speaker_id), data = scores)
summary(m.bw)

m.relBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + relative_bwPred + (1|speaker_id), data = scores)
summary(m.relBwPred)

m.condPMI <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + condPMI + (1|speaker_id), data = scores)
summary(m.condPMI)

m.both <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + relative_bwPred + condPMI + (1|speaker_id), data = scores)
summary(m.both)


# Vif for backward predictability model
vif(m.bw)
kappa(m.bw)
cor(scores$infillFw_logProb, scores$infillBw_logProb)

# Model comparisons
## Compare against baseline
# ll
lrtest(m.baseline,m.relBwPred)
lrtest(m.bw,m.relBwPred)
lrtest(m.baseline,m.condPMI)
lrtest(m.baseline,m.both)
# AIC
BIC(m.baseline,m.relBwPred)
BIC(m.bw,m.relBwPred)
BIC(m.baseline,m.condPMI)
BIC(m.baseline,m.both)

## Compare variants measures
lrtest(m.bw,m.deltaBwPred)
lrtest(m.deltaBwPred,m.condPMI)
## Compare single-variant models with both
lrtest(m.deltaBwPred,m.both)
lrtest(m.condPMI,m.both)



