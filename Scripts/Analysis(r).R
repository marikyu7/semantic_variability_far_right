library(ggplot2)
library(betareg)
library(lmtest)
library(lme4)
library(MuMIn)
library(mitml)
library(sandwich)
library(ggpubr)
library(effsize)
library(rcompanion)
library(DescTools)
library(rstatix)


# Load Dataframes ####
f <- file.choose()
semantic_variation <- read.csv(f)
semantic_variation$forum <- as.factor(semantic_variation$forum)
semantic_variation$word.type <- semantic_variation$word
semantic_variation$word.type[semantic_variation$word.type == 'immigr' | 
                               semantic_variation$word.type == 'nation'] <- 'land'
semantic_variation$word.type[semantic_variation$word.type == 'peopl' | 
                               semantic_variation$word.type == 'race' | 
                               semantic_variation$word.type == 'white' | 
                               semantic_variation$word.type == 'black'] <- 'race'

semantic_variation$word.type <- as.factor(semantic_variation$word.type)
semantic_variation$forum <- as.factor(semantic_variation$forum)
semantic_variation$ID <- as.factor(semantic_variation$ID)
semantic_variation$user.1 <- as.factor(semantic_variation$user.1)
semantic_variation$ID <- as.factor(semantic_variation$ID)

semantic_variation$forum <- relevel(semantic_variation$forum, ref = 'RC')
semantic_variation$word.type <- relevel(semantic_variation$word.type, ref = 'land')

semantic_variation$cosine.similarity <- (1 - semantic_variation$cosine.similarity)
semantic_variation$neighborhood.overlap <- (1 - semantic_variation$neighborhood.overlap)

forum_core <- semantic_variation[semantic_variation$user.2=='wiki',]
forum_core = subset(forum_core, select = -c(user.2, inter.intra) )
forum_core$forum <- droplevels(forum_core$forum)
Q <- quantile(forum_core$word.frequency, probs=c(.01, .99), na.rm = FALSE)
forum_core<- subset(forum_core, forum_core$word.frequency > (Q[1]) & forum_core$word.frequency < (Q[2]))

intra_forum <- semantic_variation[semantic_variation$inter.intra=='intra',]
intra_forum$forum <- droplevels(intra_forum$forum)
Q <- quantile(intra_forum$word.frequency, probs=c(.01, .99), na.rm = FALSE)
intra_forum<- subset(intra_forum, intra_forum$word.frequency > (Q[1]) & intra_forum$word.frequency < (Q[2]))

inter_intra_dif <- semantic_variation[semantic_variation$inter.intra == 'intra'| 
                                      semantic_variation$inter.intra == 'inter',]
inter_intra_dif$category <- paste(inter_intra_dif$forum, inter_intra_dif$inter.intra, sep="_")
inter_intra_dif$category <- as.factor(inter_intra_dif$category)
inter_intra_dif$category <- relevel(inter_intra_dif$category, ref = '_inter')
Q <- quantile(intra_forum$word.frequency, probs=c(.01, .99), na.rm = FALSE)
inter_intra_dif<- subset(inter_intra_dif, inter_intra_dif$word.frequency > (Q[1]) & inter_intra_dif$word.frequency < (Q[2]))
remove(Q, f)

# MULTILEVEL REGRESSION ####
## CORE - FORUM ####
## Cosine
FCc_model <- lmer(cosine.similarity ~ forum * word.frequency + forum * word.type  + 
                    (1|ID), 
                  data=forum_core)
r.squaredGLMM(FCc_model)
multilevelR2(FCc_model)
summary(FCc_model)
1.781e-05/(9.775e-04+1.781e-05)
# [1] 0.01789392 -> only 1.7% of the of the total variance of the random effects is attributed to the nested effect.
# aka the random effects are not present and linear mixed modeling is not appropriate == procede with linear model
FCc_model_pvalue <- lme(cosine.similarity~forum * word.frequency + forum * word.type,
                        random=~1|user.1,
                        data=forum_core)
anova(FCc_model_pvalue)

## Overlap
FCo_model <- lmer(neighborhood.overlap ~ forum * word.frequency + forum * word.type  + (1|user.1), 
                  data=forum_core)
r.squaredGLMM(FCo_model)
multilevelR2(FCo_model)
summary(FCo_model)
0.00000/(0.00000+0.03264)
# [1] 0.0 -> 0% of the of the total variance of the random effects is attributed to the nested effect.
# aka the random effects are not present and linear mixed modeling is not appropriate == procede with linear model

## INTRA - FORUM #####
## Cosine
IFc_model <- lmer(cosine.similarity ~ forum*word.frequency + forum*word.type  + (1|ID), 
                  data=intra_forum)
r.squaredGLMM(IFc_model)
multilevelR2(IFc_model)
summary(IFc_model)
0.0/(0.0+0.02587)
# [1] 0.0 -> 0% of the of the total variance of the random effects is attributed to the nested effect.
# aka the random effects are not present and linear mixed modeling is not appropriate == procede with linear model

## Overlap
IFo_model <- lmer(neighborhood.overlap ~ forum*word.frequency + forum*word.type  + (1|ID), 
                  data=intra_forum)
r.squaredGLMM(IFo_model)
multilevelR2(IFo_model)
summary(IFo_model)
0.0/(0.0+0.001158)
# [1] 0.0 -> 0% of the of the total variance of the random effects is attributed to the nested effect.
# aka the random effects are not present and linear mixed modeling is not appropriate == procede with linear model

## INTER - INTRA - DIFFERENCES #####
## Cosine
IIDc_model <- lmer(cosine.similarity ~ category  + word + word.frequency  + (1|ID), data=inter_intra_dif)
r.squaredGLMM(IIDc_model)
multilevelR2(IIDc_model)
summary(IIDc_model)
0.8099/(0.8099+11.4988)
# [1] 0.06579899 -> only 6.6% of the of the total variance of the random effects is attributed to the nested effect.
# aka the random effects are not present and linear mixed modeling is not appropriate == procede with linear model

## Overlap
IIDo_model <- lmer(neighborhood.overlap ~ category  + word + word.frequency  + (1|ID), data=inter_intra_dif)
r.squaredGLMM(IIDo_model)
multilevelR2(IIDo_model)
summary(IIDo_model)
20.08/(20.08+162.13)
# [1] 0.1102025 -> only 11% of the of the total variance of the random effects is attributed to the nested effect.
# aka the random effects are not present and linear mixed modeling is not appropriate == procede with linear model


# BETA REGRESSION ####
## CORE - FORUM ######
### Cosine ####
FCc_model <- betareg(cosine.similarity ~ forum * word.frequency,
                     data = forum_core,
                     link = 'logit')
FCc <- summary(FCc_model)

vcov <- sandwich(FCc_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_coeftest <- coeftest(FCc_model, vcov. = vcov)

### Overlap ####
FCo_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                     data = forum_core,
                     link = 'logit')
FCo <- summary(FCo_model)

vcov <- sandwich(FCo_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_coeftest <- coeftest(FCo_model, vcov. = vcov)

## INTRA - FORUM #####
### Cosine ####
IFc_model <- betareg(cosine.similarity ~ forum * word.frequency,
                     data = intra_forum,
                     link = 'logit')
IFc <- summary(IFc_model)

vcov <- sandwich(IFc_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_coeftest <- coeftest(IFc_model, vcov. = vcov)

### Overlap ####
IFo_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                     data = intra_forum,
                     link = 'logit')
IFo <- summary(IFo_model)

vcov <- sandwich(IFo_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_coeftest <- coeftest(IFo_model, vcov. = vcov)

## INTER - INTRA - DIFFERENCES #####
### Cosine ####
IIDc_model <- betareg(cosine.similarity ~ category * word.frequency,
                     data = inter_intra_dif,
                     link = 'logit')
IIDc <- summary(IIDc_model)

vcov <- sandwich(IIDc_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_coeftest <- coeftest(IIDc_model, vcov. = vcov)

### Overlap ####
IIDo_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                     data = inter_intra_dif,
                     link = 'logit')
IIDo <- summary(IIDo_model)

vcov <- sandwich(IIDo_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_coeftest <- coeftest(IIDo_model, vcov. = vcov)

rm(vcov, robust_se)
# TAB beta regression coefficients ####
models <- list(FCc, FCo, IFc, IFo)
coef <- c('intercept', 'forumSf', 'word.frequency', 'forumSf:word.frequency')
coef_tab <- data.frame(coef)
for (m in models){
  vec <- c()
  for (n in 1:4){
    estimate <- m[1][[1]]$mean[n,1]
    se <- m[1][[1]]$mean[n,2]
    odds.ratio <- exp(estimate)
    low.b <- exp(estimate - 1.96*se)
    high.b <- exp(estimate + 1.96*se)
    cell <- c(round(odds.ratio, 3), ' (', round(low.b, 3), '-', round(high.b, 3), ') ***')
    cell <- paste(cell, sep="", collapse="")
    vec <- append(vec, cell)
  }
  col <- data.frame(vec)
  coef_tab <- cbind(coef_tab, col)
  rm(vec, n, m, estimate, se, odds.ratio, low.b, high.b, cell, col)
}
rm(models, coef)
names(coef_tab) <- c('coef', 'FCc', 'FCo', 'IFc', 'IFo')


IID_models <- list(IIDc, IIDo)
IID_coef <- c('intercept', 'forumRC', 'forumSf', 'word.frequency', 
              'forumRC:word.frequency', 'forumSf:word.frequency')
IID_coef_tab <- data.frame(IID_coef)
for (m in IID_models){
  vec <- c()
  for (n in 1:6){
    estimate <- m[1][[1]]$mean[n,1]
    se <- m[1][[1]]$mean[n,2]
    odds.ratio <- exp(estimate)
    low.b <- exp(estimate - 1.96*se)
    high.b <- exp(estimate + 1.96*se)
    cell <- c(round(odds.ratio, 3), ' (', round(low.b, 3), '-', round(high.b, 3), ') ***')
    cell <- paste(cell, sep="", collapse="")
    vec <- append(vec, cell)
  }
  col <- data.frame(vec)
  IID_coef_tab <- cbind(IID_coef_tab, col)
  rm(vec, n, m, estimate, se, odds.ratio, low.b, high.b, cell, col)
}
rm(IID_models, IID_coef)
names(IID_coef_tab) <- c('coef', 'IIDc', 'IIDo')

coef_TAB <- merge(IID_coef_tab, coef_tab, by = "coef", all.x = T)
rm(IID_coef_tab, coef_tab)
# Mann–Whitney U test ####
## FORUM CORE ####
### Cosine ####
FCc_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf'])
FCc_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'])
FCc_wt <- wilcox.test(cosine.similarity ~ forum,
                      data=forum_core)
FCc_vda <- VD.A(cosine.similarity ~ forum, data=forum_core)
FCc_ci.vda<- vda(cosine.similarity ~ forum, data=forum_core, ci=TRUE)
### Overlap ####
FCo_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf'])
FCo_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'])
FCo_wt <- wilcox.test(neighborhood.overlap ~ forum,
                      data=forum_core)
FCo_vda <- VD.A(neighborhood.overlap ~ forum, data=forum_core)
FCo_ci.vda<- vda(neighborhood.overlap ~ forum, data=forum_core, ci=TRUE)

## INTRA FORUM ####
### Cosine ####
IFc_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf'])
IFc_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'])
IFc_wt <- wilcox.test(cosine.similarity ~ forum,
                      data=intra_forum)
IFc_vda <- VD.A(cosine.similarity ~ forum, data=intra_forum)
IFc_ci.vda<- vda(cosine.similarity ~ forum, data=intra_forum, ci=TRUE)
### Overlap ####
IFo_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf'])
IFo_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'])
IFo_wt <- wilcox.test(neighborhood.overlap ~ forum,
                      data=intra_forum)
IFo_vda <- VD.A(neighborhood.overlap ~ forum, data=intra_forum)
IFo_ci.vda<- vda(neighborhood.overlap ~ forum, data=intra_forum, ci=TRUE)

# Kruskal-Wallis rank sum test ####
## INTER - INTRA - DIFFERENCES ####
### Cosine ####
IIDc_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter'])
IIDc_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'])
IIDc_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra'])
IIDc_kruskal.test <- kruskal.test(cosine.similarity ~ category, data = inter_intra_dif)
IIDc_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,data = inter_intra_dif)
IIDc_wt <- wilcox_test(cosine.similarity ~ category, 
                       p.adjust.method = "bonferroni", data = inter_intra_dif)

### Overlap ####
IIDo_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter'])
IIDo_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'])
IIDo_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra'])
IIDo_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, data = inter_intra_dif)
IIDo_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,data = inter_intra_dif)
IIDo_wt <- wilcox_test(neighborhood.overlap ~ category, 
                       p.adjust.method = "bonferroni", data = inter_intra_dif)

# Median word frequency ####
## FORUM CORE ####
FCwf_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'])
FCwf_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'])

## INTRA FORUM ####
IFwf_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'])
IFwf_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'])

## INTER - INTRA - DIFFERENCES ####
IIDwf_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'])
IIDwf_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'])
IIDwf_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'])



# TAB median + word freq ####
test <- c('forum-core', 'forum-core', 'forum-core', 'forum-core', 
          'intra-group', 'intra-group', 'intra-group', 'intra-group',
          'inter & intra group', 'inter & intra group', 'inter & intra group',
          'inter & intra group', 'inter & intra group', 'inter & intra group')

measure <- c('cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap')

group <- c('Sf', 'RC', 'Sf', 'RC',
           'Sf', 'RC', 'Sf', 'RC',
           'inter', 'Sf', 'RC', 'inter', 'Sf', 'RC')

medians <- list(FCc_Sf.median, FCc_RC.median, FCo_Sf.median, FCo_RC.median,
                IFc_Sf.median, IFc_RC.median, IFo_Sf.median, IFo_RC.median,
                IIDc_inter.median, IIDc_Sf.median, IIDc_RC.median, 
                IIDo_inter.median, IIDo_Sf.median, IIDo_RC.median)
semantic.distance <- c()
for (m in medians){
  a <- c(round(m[1], 3),' (', round(m[2], 3), '-', 
         round(m[3], 3), ')')
  a <- paste(a, sep="", collapse="")
  semantic.distance <- append(semantic.distance, a)
  rm(a, m)
}

frequencies <- list(FCwf_Sf.median, FCwf_RC.median, FCwf_Sf.median, FCwf_RC.median,
                    IFwf_Sf.median, IFwf_RC.median, IFwf_Sf.median, IFwf_RC.median,
                    IIDwf_inter.median, IIDwf_Sf.median, IIDwf_RC.median,
                    IIDwf_inter.median, IIDwf_Sf.median, IIDwf_RC.median)
word.frequency <- c()
for (f in frequencies){
  a <- c(round(f[1], 3),' (', round(f[2], 3), '-', 
         round(f[3], 3), ')')
  a <- paste(a, sep="", collapse="")
  word.frequency <- append(word.frequency, a)
  rm(a, f)
}

es_ci <- list(FCc_ci.vda, FCc_ci.vda, FCo_ci.vda, FCo_ci.vda,
              IFc_ci.vda, IFc_ci.vda, IFo_ci.vda, IFo_ci.vda)
es_ci.IID <- list(IIDc_kruskal.es, IIDc_kruskal.es, IIDc_kruskal.es,
                  IIDo_kruskal.es, IIDo_kruskal.es, IIDo_kruskal.es)
effect.size <- c()
for (e in es_ci){
  a <- c(e[1],' (', e[2], '-', e[3], ')')
  a <- paste(a, sep="", collapse="")
  effect.size <- append(effect.size, a)
  rm(a, e)
}
for (e in es_ci.IID){
  a <- c(round(e[3],3),' (', e[4], '-', e[5], ')')
  a <- paste(a, sep="", collapse="")
  effect.size <- append(effect.size, a)
  rm(a, e)
}

median_TAB <- data.frame(test, measure, group, word.frequency, semantic.distance, effect.size)
View(median_TAB)
rm(medians, frequencies, es, es_ci)
rm(test, measure, group, word.frequency, semantic.distance, effect.size)

rm(FCc_Sf.median, FCc_RC.median, FCo_Sf.median, FCo_RC.median,
   IFc_Sf.median, IFc_RC.median, IFo_Sf.median, IFo_RC.median,
   IIDc_inter.median, IIDc_Sf.median, IIDc_RC.median, 
   IIDo_inter.median, IIDo_Sf.median, IIDo_RC.median)
rm(FCwf_Sf.median, FCwf_RC.median,
   IFwf_Sf.median, IFwf_RC.median,
   IIDwf_inter.median, IIDwf_Sf.median, IIDwf_RC.median)





# PLOTS #####

## CORE - FORUM ####
### Cosine ####
Sf <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
              subset = forum == 'Sf',
              link = "logit")
RC <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
              subset = forum == 'RC',
              link = c("logit"))

FCc_plot <- ggplot(forum_core, aes(x = word.frequency, y = cosine.similarity)) +
  geom_point(size = 1, aes(fill = forum), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(RC, forum_core),
                colour = "RC")) +
  geom_line(aes(y = predict(Sf, forum_core), 
                colour = "Sf")) +
  scale_colour_manual("", values = c("red", "blue")) +
  ggtitle('Users\' comments difference from neutral text - Semantic Distance') +
  xlab("word frequency") + 
  ylab("semantic distance")+
  theme_bw()
FCc_plot

### Overlap ####
Sf <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
              subset = forum == 'Sf',
              link = "logit")
RC <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
              subset = forum == 'RC',
              link = "logit")

FCo_plot <- ggplot(forum_core, aes(x = word.frequency, y = neighborhood.overlap)) +
  geom_point(size = 1, aes(fill = forum), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(RC, forum_core),
                colour = "RC")) +
  geom_line(aes(y = predict(Sf, forum_core), 
                colour = "Sf")) +
  scale_colour_manual("", values = c("red", "blue")) +
  ggtitle('Users\' comments difference from neutral text - Neighborhood Overlap') +
  xlab("word frequency") + 
  ylab("neighborhood overlap")+
  theme_bw()
FCo_plot

## INTRA FORUM ####
### Cosine ####
Sf <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
              subset = forum == 'Sf',
              link = "logit")
RC <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
              subset = forum == 'RC',
              link = "logit")

IFc_plot <- ggplot(intra_forum, aes(x = word.frequency, y = cosine.similarity)) +
  geom_point(size = 1, aes(fill = forum), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(RC, intra_forum),
                colour = "RC")) +
  geom_line(aes(y = predict(Sf, intra_forum), 
                colour = "Sf")) +
  scale_colour_manual("", values = c("red", "blue")) +
  ggtitle('Intra forum variability - Semantic Distance') +
  xlab("word frequency") + 
  ylab("semantic distance")+
  theme_bw()
IFc_plot

### Overlap ####
Sf <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
              subset = forum == 'Sf',
              link = "logit")
RC <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
              subset = forum == 'RC',
              link = "logit")

IFo_plot <- ggplot(intra_forum, aes(x = word.frequency, y = neighborhood.overlap)) +
  geom_point(size = 1, aes(fill = forum), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(RC, intra_forum),
                colour = "RC")) +
  geom_line(aes(y = predict(Sf, intra_forum), 
                colour = "Sf")) +
  scale_colour_manual("", values = c("red", "blue")) +
  ggtitle('Intra forum variability - Neighborhood Overlap') +
  xlab("word frequency") + 
  ylab("neighborhood overlap")+
  theme_bw()
IFo_plot

## INTER - INTRA - FORUM ####
### Cosine ####
Sf <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'Sf_intra',
              link = "logit")
RC <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'RC_intra',
              link = "logit")
inter <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                      subset = category == '_inter' ,
                      link = "logit")

IIDc_plot <- ggplot(inter_intra_dif, aes(x = word.frequency, y = cosine.similarity)) +
  geom_point(size = 1, aes(fill = category), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(RC, inter_intra_dif),
                colour = "RC")) +
  geom_line(aes(y = predict(Sf, inter_intra_dif), 
                colour = "Sf")) +
  geom_line(aes(y = predict(inter, inter_intra_dif), 
                colour = "_inter")) +
  scale_colour_manual("", values = c("red", "blue", 'green')) +
  ggtitle('Inter and Intra group variability - Semantic Distance') +
  xlab("word frequency") + 
  ylab("semantic distance")+
  theme_bw()
IIDc_plot

### Overlap ####
Sf <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'Sf_intra',
              link = "logit")
RC <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'RC_intra',
              link = "logit")
inter <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                 subset = category == '_inter' ,
                 link = "logit")

IIDo_plot <- ggplot(inter_intra_dif, aes(x = word.frequency, y = neighborhood.overlap)) +
  geom_point(size = 1, aes(fill = category), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(RC, inter_intra_dif),
                colour = "RC")) +
  geom_line(aes(y = predict(Sf, inter_intra_dif), 
                colour = "Sf")) +
  geom_line(aes(y = predict(inter, inter_intra_dif), 
                colour = "_inter")) +
  scale_colour_manual("", values = c("red", "blue", 'green')) +
  ggtitle('Inter and Intra group variability - Neighborhood Overlap') +
  xlab("word frequency") + 
  ylab("neighborhood overlap")+
  theme_bw()
IIDo_plot


remove(Sf,RC,inter)

## Overlap-cosine correlation #####
cos <- append(forum_core$cosine.similarity, intra_forum$cosine.similarity)
cos <- append(cos, inter_intra_dif$cosine.similarity)
ov <- append(forum_core$neighborhood.overlap, intra_forum$neighborhood.overlap)
ov <- append(ov, inter_intra_dif$neighborhood.overlap)
cos.ov.df <- data.frame(cos, ov)
reg <- lm(ov~log(cos), data = cos.ov.df)

cos.ov_plot <- ggplot(cos.ov.df, aes(x = log(cos), y = ov)) +
  geom_point(size = 1, color = 'grey', shape = 21) +
  scale_fill_grey() +
  geom_abline(intercept = 1.118, slope = 0.143, color="red", size=0.5)+ 
  ggtitle('Correlation semantic difference measures') +
  xlab("log(semantic distance)") + 
  ylab("neighborhood overlap")
theme_bw()
cos.ov_plot
rm(cos, ov, cos.ov.df)


##### .----------.#######




















