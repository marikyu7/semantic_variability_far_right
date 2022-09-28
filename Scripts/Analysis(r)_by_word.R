# Load Dataframes ####
f <- file.choose()
semantic_variation <- read.csv(f)
semantic_variation$forum <- as.factor(semantic_variation$forum)

semantic_variation$word.type <- as.factor(semantic_variation$word.type)
semantic_variation$forum <- as.factor(semantic_variation$forum)
semantic_variation$ID <- as.factor(semantic_variation$ID)
semantic_variation$user.1 <- as.factor(semantic_variation$user.1)
semantic_variation$ID <- as.factor(semantic_variation$ID)

semantic_variation$forum <- relevel(semantic_variation$forum, ref = 'RC')

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

# CORE - FORUM ####
## Cosine ####
### Beta - regression ####
#### Immigration ####
FCc_immig_model <- betareg(cosine.similarity ~ forum * word.frequency,
                     data = forum_core[forum_core$word == 'immigr',],
                     link = 'logit')
FCc_immig <- summary(FCc_immig_model)

vcov <- sandwich(FCc_immig_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_immig_coeftest <- coeftest(FCc_immig_model, vcov. = vcov)
#### Nation ####
FCc_nation_model <- betareg(cosine.similarity ~ forum * word.frequency,
                            data = forum_core[forum_core$word == 'nation',],
                            link = 'logit')
FCc_nation <- summary(FCc_nation_model)

vcov <- sandwich(FCc_nation_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_nation_coeftest <- coeftest(FCc_nation_model, vcov. = vcov)
#### Black ####
FCc_black_model <- betareg(cosine.similarity ~ forum * word.frequency,
                            data = forum_core[forum_core$word == 'black',],
                            link = 'logit')
FCc_black <- summary(FCc_black_model)

vcov <- sandwich(FCc_black_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_black_coeftest <- coeftest(FCc_black_model, vcov. = vcov)
#### White ####
FCc_white_model <- betareg(cosine.similarity ~ forum * word.frequency,
                           data = forum_core[forum_core$word == 'white',],
                           link = 'logit')
FCc_white <- summary(FCc_white_model)

vcov <- sandwich(FCc_white_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_white_coeftest <- coeftest(FCc_white_model, vcov. = vcov)
#### People ####
FCc_peopl_model <- betareg(cosine.similarity ~ forum * word.frequency,
                           data = forum_core[forum_core$word == 'peopl',],
                           link = 'logit')
FCc_peopl <- summary(FCc_peopl_model)

vcov <- sandwich(FCc_peopl_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_peopl_coeftest <- coeftest(FCc_peopl_model, vcov. = vcov)
#### Race ####
FCc_race_model <- betareg(cosine.similarity ~ forum * word.frequency,
                           data = forum_core[forum_core$word == 'race',],
                           link = 'logit')
FCc_race <- summary(FCc_race_model)

vcov <- sandwich(FCc_race_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_peopl_coeftest <- coeftest(FCc_race_model, vcov. = vcov)

#### Tot ####
FCc_model <- betareg(cosine.similarity ~ forum * word.frequency,
                          data = forum_core,
                          link = 'logit')
FCc <- summary(FCc_model)

vcov <- sandwich(FCc_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCc_peopl_coeftest <- coeftest(FCc_model, vcov. = vcov)
### Mann–Whitney U test ####
#### Immigration ####
FCc_immig_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'immigr'])
FCc_immig_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'
                                                             & forum_core$word == 'immigr'])
FCc_immig_wt <- wilcox.test(cosine.similarity ~ forum,
                      data=forum_core[forum_core$word == 'immigr',])
FCc_immig_vda <- VD.A(cosine.similarity ~ forum, 
                      data=forum_core[forum_core$word == 'immigr',])
FCc_immig_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'immigr',])
#### Nation ####
FCc_nation_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'nation'])
FCc_nation_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'
                                                             & forum_core$word == 'nation'])
FCc_nation_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=forum_core[forum_core$word == 'nation',])
FCc_nation_vda <- VD.A(cosine.similarity ~ forum, 
                      data=forum_core[forum_core$word == 'nation',])
FCc_nation_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'nation',])
#### Black ####
FCc_black_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'black'])
FCc_black_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'
                                                             & forum_core$word == 'black'])
FCc_black_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=forum_core[forum_core$word == 'black',])
FCc_black_vda <- VD.A(cosine.similarity ~ forum, 
                      data=forum_core[forum_core$word == 'black',])
FCc_black_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'black',])
#### White ####
FCc_white_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'white'])
FCc_white_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'
                                                             & forum_core$word == 'white'])
FCc_white_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=forum_core[forum_core$word == 'white',])
FCc_white_vda <- VD.A(cosine.similarity ~ forum, 
                      data=forum_core[forum_core$word == 'white',])
FCc_white_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'white',])
#### People ####
FCc_peopl_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'peopl'])
FCc_peopl_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'
                                                             & forum_core$word == 'peopl'])
FCc_peopl_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=forum_core[forum_core$word == 'peopl',])
FCc_peopl_vda <- VD.A(cosine.similarity ~ forum, 
                      data=forum_core[forum_core$word == 'peopl',])
FCc_peopl_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'peopl',])
#### Race ####
FCc_race_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'race'])
FCc_race_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'
                                                             & forum_core$word == 'race'])
FCc_race_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=forum_core[forum_core$word == 'race',])
FCc_race_vda <- VD.A(cosine.similarity ~ forum, 
                      data=forum_core[forum_core$word == 'race',])
FCc_race_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'race',])

#### Tot ####
FCc_Sf.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'Sf'])
FCc_RC.median <- MedianCI(forum_core$cosine.similarity[forum_core$forum == 'RC'])
FCc_wt <- wilcox.test(cosine.similarity ~ forum,
                           data=forum_core)
FCc_vda <- VD.A(cosine.similarity ~ forum, 
                     data=forum_core)
FCc_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                      data=forum_core)

## Overlap ####
### Beta - regression ####
#### Immigration ####
FCo_immig_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = forum_core[forum_core$word == 'immigr',],
                           link = 'logit')
FCo_immig <- summary(FCo_immig_model)

vcov <- sandwich(FCo_immig_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_immig_coeftest <- coeftest(FCo_immig_model, vcov. = vcov)
#### Nation ####
FCo_nation_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                            data = forum_core[forum_core$word == 'nation',],
                            link = 'logit')
FCo_nation <- summary(FCo_nation_model)

vcov <- sandwich(FCo_nation_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_nation_coeftest <- coeftest(FCo_nation_model, vcov. = vcov)
#### Black ####
FCo_black_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = forum_core[forum_core$word == 'black',],
                           link = 'logit')
FCo_black <- summary(FCo_black_model)

vcov <- sandwich(FCo_black_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_black_coeftest <- coeftest(FCo_black_model, vcov. = vcov)
#### White ####
FCo_white_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = forum_core[forum_core$word == 'white',],
                           link = 'logit')
FCo_white <- summary(FCo_white_model)

vcov <- sandwich(FCo_white_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_white_coeftest <- coeftest(FCo_white_model, vcov. = vcov)
#### People ####
FCo_peopl_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = forum_core[forum_core$word == 'peopl',],
                           link = 'logit')
FCo_peopl <- summary(FCo_peopl_model)

vcov <- sandwich(FCo_peopl_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_peopl_coeftest <- coeftest(FCo_peopl_model, vcov. = vcov)
#### Race ####
FCo_race_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                          data = forum_core[forum_core$word == 'race',],
                          link = 'logit')
FCo_race <- summary(FCo_race_model)

vcov <- sandwich(FCo_race_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_race_coeftest <- coeftest(FCo_race_model, vcov. = vcov)

#### Tot ####
FCo_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                          data = forum_core,
                          link = 'logit')
FCo <- summary(FCo_model)

vcov <- sandwich(FCo_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
FCo_coeftest <- coeftest(FCo_model, vcov. = vcov)

rm(vcov, robust_se)
### Mann–Whitney U test ####
#### Immigration ####
FCo_immig_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'immigr'])
FCo_immig_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'
                                                             & forum_core$word == 'immigr'])
FCo_immig_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=forum_core[forum_core$word == 'immigr',])
FCo_immig_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=forum_core[forum_core$word == 'immigr',])
FCo_immig_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'immigr',])
#### Nation ####
FCo_nation_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf' 
                                                              & forum_core$word == 'nation'])
FCo_nation_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'
                                                              & forum_core$word == 'nation'])
FCo_nation_wt <- wilcox.test(neighborhood.overlap ~ forum,
                             data=forum_core[forum_core$word == 'nation',])
FCo_nation_vda <- VD.A(neighborhood.overlap ~ forum, 
                       data=forum_core[forum_core$word == 'nation',])
FCo_nation_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                        data=forum_core[forum_core$word == 'nation',])
#### Black ####
FCo_black_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'black'])
FCo_black_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'
                                                             & forum_core$word == 'black'])
FCo_black_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=forum_core[forum_core$word == 'black',])
FCo_black_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=forum_core[forum_core$word == 'black',])
FCo_black_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'black',])
#### White ####
FCo_white_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'white'])
FCo_white_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'
                                                             & forum_core$word == 'white'])
FCo_white_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=forum_core[forum_core$word == 'white',])
FCo_white_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=forum_core[forum_core$word == 'white',])
FCo_white_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'white',])
#### People ####
FCo_peopl_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf' 
                                                             & forum_core$word == 'peopl'])
FCo_peopl_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'
                                                             & forum_core$word == 'peopl'])
FCo_peopl_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=forum_core[forum_core$word == 'peopl',])
FCo_peopl_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=forum_core[forum_core$word == 'peopl',])
FCo_peopl_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=forum_core[forum_core$word == 'peopl',])
#### Race ####
FCo_race_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf' 
                                                            & forum_core$word == 'race'])
FCo_race_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'
                                                            & forum_core$word == 'race'])
FCo_race_wt <- wilcox.test(neighborhood.overlap ~ forum,
                           data=forum_core[forum_core$word == 'race',])
FCo_race_vda <- VD.A(neighborhood.overlap ~ forum, 
                     data=forum_core[forum_core$word == 'race',])
FCo_race_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                      data=forum_core[forum_core$word == 'race',])


#### Tot ####
FCo_Sf.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'Sf'])
FCo_RC.median <- MedianCI(forum_core$neighborhood.overlap[forum_core$forum == 'RC'])
FCo_wt <- wilcox.test(neighborhood.overlap ~ forum,
                           data=forum_core)
FCo_vda <- VD.A(neighborhood.overlap ~ forum, 
                     data=forum_core)
FCo_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                      data=forum_core)


#### Median word frequency ####
#### Immigration ####
FCwf_immig_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'
                                                           & forum_core$word == 'immigr'])
FCwf_immig_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'
                                                           & forum_core$word == 'immigr'])
#### Nation ####
FCwf_nation_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'
                                                           & forum_core$word == 'nation'])
FCwf_nation_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'
                                                           & forum_core$word == 'nation'])
#### Black ####
FCwf_black_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'
                                                           & forum_core$word == 'black'])
FCwf_black_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'
                                                           & forum_core$word == 'black'])
#### White ####
FCwf_white_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'
                                                           & forum_core$word == 'white'])
FCwf_white_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'
                                                           & forum_core$word == 'white'])
#### People ####
FCwf_peopl_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'
                                                           & forum_core$word == 'peopl'])
FCwf_peopl_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'
                                                           & forum_core$word == 'peopl'])
#### Race ####
FCwf_race_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'
                                                           & forum_core$word == 'race'])
FCwf_race_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'
                                                           & forum_core$word == 'race'])

#### Tot ####
FCwf_Sf.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'Sf'])
FCwf_RC.median <- MedianCI(forum_core$word.frequency[forum_core$forum == 'RC'])

## TAB median + word freq ####
word <- c('Immigration', '','','', 'Nation', '','','', 'Black', '','','', 
          'White', '','','', 'People', '','','', 'Race', '','','', 'Tot', '','','' )

measure <- c('cosine', 'cosine', 'overlap', 'overlap', 'cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'overlap', 'overlap', 'cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'overlap', 'overlap', 'cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'overlap', 'overlap')

group <- c('Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC',
           'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC',
           'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC',
           'Sf', 'RC', 'Sf', 'RC')

medians <- list(FCc_immig_Sf.median, FCc_immig_RC.median, FCo_immig_Sf.median, FCo_immig_RC.median,
                FCc_nation_Sf.median, FCc_nation_RC.median, FCo_nation_Sf.median, FCo_nation_RC.median,
                FCc_black_Sf.median, FCc_black_RC.median, FCo_black_Sf.median, FCo_black_RC.median,
                FCc_white_Sf.median, FCc_white_RC.median, FCo_white_Sf.median, FCo_white_RC.median,
                FCc_peopl_Sf.median, FCc_peopl_RC.median, FCo_peopl_Sf.median, FCo_peopl_RC.median,
                FCc_race_Sf.median, FCc_race_RC.median, FCo_race_Sf.median, FCo_race_RC.median,
                FCc_Sf.median, FCc_RC.median, FCo_Sf.median, FCo_RC.median)
semantic.distance <- c()
for (m in medians){
  a <- c(round(m[1], 3),' (', round(m[2], 3), '-', 
         round(m[3], 3), ')')
  a <- paste(a, sep="", collapse="")
  semantic.distance <- append(semantic.distance, a)
  rm(a, m)
}

frequencies <- list(FCwf_immig_Sf.median, FCwf_immig_RC.median, FCwf_immig_Sf.median, FCwf_immig_RC.median,
                    FCwf_nation_Sf.median, FCwf_nation_RC.median, FCwf_nation_Sf.median, FCwf_nation_RC.median,
                    FCwf_black_Sf.median, FCwf_black_RC.median, FCwf_black_Sf.median, FCwf_black_RC.median,
                    FCwf_white_Sf.median, FCwf_white_RC.median, FCwf_white_Sf.median, FCwf_white_RC.median,
                    FCwf_peopl_Sf.median, FCwf_peopl_RC.median, FCwf_peopl_Sf.median, FCwf_peopl_RC.median,
                    FCwf_race_Sf.median, FCwf_race_RC.median, FCwf_race_Sf.median, FCwf_race_RC.median,
                    FCwf_Sf.median, FCwf_RC.median, FCwf_Sf.median, FCwf_RC.median)
word.frequency <- c()
for (f in frequencies){
  a <- c(round(f[1], 3),' (', round(f[2], 3), '-', 
         round(f[3], 3), ')')
  a <- paste(a, sep="", collapse="")
  word.frequency <- append(word.frequency, a)
  rm(a, f)
}

es_ci <- list(FCc_immig_ci.vda, FCc_immig_ci.vda, FCo_immig_ci.vda, FCo_immig_ci.vda,
              FCc_nation_ci.vda, FCc_nation_ci.vda, FCo_nation_ci.vda, FCo_nation_ci.vda,
              FCc_black_ci.vda, FCc_black_ci.vda, FCo_black_ci.vda, FCo_black_ci.vda,
              FCc_white_ci.vda, FCc_white_ci.vda, FCo_white_ci.vda, FCo_white_ci.vda,
              FCc_peopl_ci.vda, FCc_peopl_ci.vda, FCo_peopl_ci.vda, FCo_peopl_ci.vda,
              FCc_race_ci.vda, FCc_race_ci.vda, FCo_race_ci.vda, FCo_race_ci.vda,
              FCc_ci.vda, FCc_ci.vda, FCo_ci.vda, FCo_ci.vda)

effect.size <- c()
for (e in es_ci){
  a <- c(e[1],' (', e[2], '-', e[3], ')')
  a <- paste(a, sep="", collapse="")
  effect.size <- append(effect.size, a)
  rm(a, e)
}

FC_median_TAB <- data.frame(word, measure, group, word.frequency, semantic.distance, effect.size)
View(FC_median_TAB)
rm(medians, frequencies, es_ci)
rm(word, measure, group, word.frequency, semantic.distance, effect.size)

## TAB betareg ####
models <- list(FCc_immig, FCc_nation, FCc_black, FCc_white, FCc_peopl, FCc_race, FCc, 
               FCo_immig, FCo_nation, FCo_black, FCo_white, FCo_peopl, FCo_race, FCo)
coef <- c('intercept', 'forumSf', 'word.frequency', 'forumSf:word.frequency')
FC_coef_tab <- data.frame(coef)
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
  FC_coef_tab <- cbind(FC_coef_tab, col)
  rm(vec, n, m, estimate, se, odds.ratio, low.b, high.b, cell, col)
}
rm(models, coef)
names(FC_coef_tab) <- c('coef', 
                        'FCc_immig', 'FCc_nation', 'FCc_black', 'FCc_white', 'FCc_peopl', 'FCc_race',
                        'FCc_TOT',
                        'FCo_immig', 'FCo_nation', 'FCo_black', 'FCo_white', 'FCo_peopl', 'FCo_race',
                        'FCo_TOT')
View(FC_coef_tab)

#### Clean ####
rm(FCc_immig_Sf.median, FCc_immig_RC.median, FCo_immig_Sf.median, FCo_immig_RC.median,
   FCc_nation_Sf.median, FCc_nation_RC.median, FCo_nation_Sf.median, FCo_nation_RC.median,
   FCc_black_Sf.median, FCc_black_RC.median, FCo_black_Sf.median, FCo_black_RC.median,
   FCc_white_Sf.median, FCc_white_RC.median, FCo_white_Sf.median, FCo_white_RC.median,
   FCc_peopl_Sf.median, FCc_peopl_RC.median, FCo_peopl_Sf.median, FCo_peopl_RC.median,
   FCc_race_Sf.median, FCc_race_RC.median, FCo_race_Sf.median, FCo_race_RC.median,
   FCc_Sf.median, FCc_RC.median, FCo_Sf.median, FCo_RC.median)
rm(FCwf_immig_Sf.median, FCwf_immig_RC.median,
   FCwf_nation_Sf.median, FCwf_nation_RC.median, 
   FCwf_black_Sf.median, FCwf_black_RC.median, 
   FCwf_white_Sf.median, FCwf_white_RC.median, 
   FCwf_peopl_Sf.median, FCwf_peopl_RC.median, 
   FCwf_race_Sf.median, FCwf_race_RC.median,
   FCwf_Sf.median, FCwf_RC.median)
rm(FCc_immig_ci.vda, FCo_immig_ci.vda,
   FCc_nation_ci.vda, FCo_nation_ci.vda,
   FCc_black_ci.vda, FCo_black_ci.vda,
   FCc_white_ci.vda, FCo_white_ci.vda,
   FCc_peopl_ci.vda, FCo_peopl_ci.vda,
   FCc_race_ci.vda, FCo_race_ci.vda,
   FCc_ci.vda, FCo_ci.vda)
rm(FCc_immig_vda, FCo_immig_vda,
   FCc_nation_vda, FCo_nation_vda,
   FCc_black_vda, FCo_black_vda,
   FCc_white_vda, FCo_white_vda,
   FCc_peopl_vda, FCo_peopl_vda,
   FCc_race_vda, FCo_race_vda,
   FCc_vda, FCo_vda)
rm(FCc_immig_wt, FCo_immig_wt,
   FCc_nation_wt, FCo_nation_wt,
   FCc_black_wt, FCo_black_wt,
   FCc_white_wt, FCo_white_wt,
   FCc_peopl_wt, FCo_peopl_wt,
   FCc_race_wt, FCo_race_wt,
   FCc_wt, FCo_wt)
rm(FCc_immig_coeftest, FCo_immig_coeftest,
   FCc_nation_coeftest, FCo_nation_coeftest,
   FCc_black_coeftest, FCo_black_coeftest,
   FCc_white_coeftest, FCo_white_coeftest,
   FCc_peopl_coeftest, FCo_peopl_coeftest,
   FCc_race_coeftest, FCo_race_coeftest,
   FCc_coeftest, FCo_coeftest)
rm(FCc_immig, FCo_immig,
   FCc_nation, FCo_nation,
   FCc_black, FCo_black,
   FCc_white, FCo_white,
   FCc_peopl, FCo_peopl,
   FCc_race, FCo_race,
   FCc, FCo)
rm(FCc_immig_model, FCo_immig_model,
   FCc_nation_model, FCo_nation_model,
   FCc_black_model, FCo_black_model,
   FCc_white_model, FCo_white_model,
   FCc_peopl_model, FCo_peopl_model,
   FCc_race_model, FCo_race_model,
   FCc_model, FCo_model)

## PLOTS #####
### Cosine ####
Sf_immig <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
              subset = forum == 'Sf' & word == 'immigr',
              link = "logit")
Sf_nation <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'nation',
                    link = "logit")
Sf_black <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'black',
                    link = "logit")
Sf_white <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'white',
                    link = "logit")
Sf_peopl <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'peopl',
                    link = "logit")
Sf_race <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'race',
                    link = "logit")
Sf <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
              subset = forum == 'Sf',
              link = "logit")

RC_immig <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'immigr',
                    link = "logit")
RC_nation <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                     subset = forum == 'RC' & word == 'nation',
                     link = "logit")
RC_black <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'black',
                    link = "logit")
RC_white <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'white',
                    link = "logit")
RC_peopl <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'peopl',
                    link = "logit")
RC_race <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                   subset = forum == 'RC' & word == 'race',
                   link = "logit")
RC <- betareg(cosine.similarity ~ word.frequency, data = forum_core, 
                   subset = forum == 'RC',
                   link = "logit")

FCc_plot <- ggplot(forum_core, aes(x = word.frequency, y = cosine.similarity)) +
  geom_line(aes(y = predict(Sf_immig, forum_core),
                colour = "immig", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_nation, forum_core), 
                colour = "nation", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_black, forum_core),
                colour = "black", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_white, forum_core), 
                colour = "white", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_peopl, forum_core),
                colour = "peopl", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_race, forum_core), 
                colour = "race", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf, forum_core), 
                colour = "tot", linetype= 'Sf')) +
  geom_line(aes(y = predict(RC_immig, forum_core),
                colour = "immig", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_nation, forum_core), 
                colour = "nation", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_black, forum_core),
                colour = "black", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_white, forum_core), 
                colour = "white", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_peopl, forum_core),
                colour = "peopl", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_race, forum_core), 
                colour = "race", linetype= 'RC')) +
  geom_line(aes(y = predict(RC, forum_core), 
                colour = "tot", linetype= 'RC')) +
  scale_colour_manual("", values = c("red", 'orange', 'green', "blue", 'purple', 'black', 'pink')) +
  ggtitle('Stormfront users\' comments difference from neutral text by word - Semantic Distance') +
  xlab("word frequency") + 
  ylab("semantic distance")+
  theme_bw()
FCc_plot
### Overlap ####
Sf_immig <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'immigr',
                    link = "logit")
Sf_nation <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                     subset = forum == 'Sf' & word == 'nation',
                     link = "logit")
Sf_black <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'black',
                    link = "logit")
Sf_white <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'white',
                    link = "logit")
Sf_peopl <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'Sf' & word == 'peopl',
                    link = "logit")
Sf_race <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                   subset = forum == 'Sf' & word == 'race',
                   link = "logit")
Sf <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                   subset = forum == 'Sf',
                   link = "logit")

RC_immig <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'immigr',
                    link = "logit")
RC_nation <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                     subset = forum == 'RC' & word == 'nation',
                     link = "logit")
RC_black <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'black',
                    link = "logit")
RC_white <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'white',
                    link = "logit")
RC_peopl <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                    subset = forum == 'RC' & word == 'peopl',
                    link = "logit")
RC_race <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                   subset = forum == 'RC' & word == 'race',
                   link = "logit")
RC <- betareg(neighborhood.overlap ~ word.frequency, data = forum_core, 
                   subset = forum == 'RC',
                   link = "logit")

FCo_plot <- ggplot(forum_core, aes(x = word.frequency, y = neighborhood.overlap)) +
  geom_line(aes(y = predict(Sf_immig, forum_core),
                colour = "immig", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_nation, forum_core), 
                colour = "nation", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_black, forum_core),
                colour = "black", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_white, forum_core), 
                colour = "white", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_peopl, forum_core),
                colour = "peopl", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_race, forum_core), 
                colour = "race", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf, forum_core), 
                colour = "tot", linetype= 'Sf')) +
  geom_line(aes(y = predict(RC_immig, forum_core),
                colour = "immig", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_nation, forum_core), 
                colour = "nation", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_black, forum_core),
                colour = "black", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_white, forum_core), 
                colour = "white", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_peopl, forum_core),
                colour = "peopl", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_race, forum_core), 
                colour = "race", linetype= 'RC')) +
  geom_line(aes(y = predict(RC, forum_core), 
                colour = "tot", linetype= 'RC')) +
  scale_colour_manual("", values = c("red", 'orange', 'green', "blue", 'purple', 'black', 'pink')) +
  ggtitle('Stormfront users\' comments difference from neutral text by word - Neighborhood Overlap') +
  xlab("word frequency") + 
  ylab("neighborhood overlap")+
  theme_bw()
FCo_plot

rm(Sf_immig, Sf_nation, Sf_black, Sf_white, Sf_peopl, Sf_race,
   RC_immig, RC_nation, RC_black, RC_white, RC_peopl, RC_race)
### .-------------------. ######
# INTRA - FORUM ####
## Cosine ####
### Beta - regression ####
#### Immigration ####
IFc_immig_model <- betareg(cosine.similarity ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'immigr',],
                           link = 'logit')
IFc_immig <- summary(IFc_immig_model)

vcov <- sandwich(IFc_immig_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_immig_coeftest <- coeftest(IFc_immig_model, vcov. = vcov)
#### Nation ####
IFc_nation_model <- betareg(cosine.similarity ~ forum * word.frequency,
                            data = intra_forum[intra_forum$word == 'nation',],
                            link = 'logit')
IFc_nation <- summary(IFc_nation_model)

vcov <- sandwich(IFc_nation_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_nation_coeftest <- coeftest(IFc_nation_model, vcov. = vcov)
#### Black ####
IFc_black_model <- betareg(cosine.similarity ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'black',],
                           link = 'logit')
IFc_black <- summary(IFc_black_model)

vcov <- sandwich(IFc_black_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_black_coeftest <- coeftest(IFc_black_model, vcov. = vcov)
#### White ####
IFc_white_model <- betareg(cosine.similarity ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'white',],
                           link = 'logit')
IFc_white <- summary(IFc_white_model)

vcov <- sandwich(IFc_white_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_white_coeftest <- coeftest(IFc_white_model, vcov. = vcov)
#### People ####
IFc_peopl_model <- betareg(cosine.similarity ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'peopl',],
                           link = 'logit')
IFc_peopl <- summary(IFc_peopl_model)

vcov <- sandwich(IFc_peopl_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_peopl_coeftest <- coeftest(IFc_peopl_model, vcov. = vcov)
#### Race ####
IFc_race_model <- betareg(cosine.similarity ~ forum * word.frequency,
                          data = intra_forum[intra_forum$word == 'race',],
                          link = 'logit')
IFc_race <- summary(IFc_race_model)

vcov <- sandwich(IFc_race_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_peopl_coeftest <- coeftest(IFc_race_model, vcov. = vcov)

#### Tot ####
IFc_model <- betareg(cosine.similarity ~ forum * word.frequency,
                     data = intra_forum,
                     link = 'logit')
IFc <- summary(IFc_model)

vcov <- sandwich(IFc_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFc_peopl_coeftest <- coeftest(IFc_model, vcov. = vcov)
### Mann–Whitney U test ####
#### Immigration ####
IFc_immig_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf' 
                                                             & intra_forum$word == 'immigr'])
IFc_immig_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'
                                                             & intra_forum$word == 'immigr'])
IFc_immig_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=intra_forum[intra_forum$word == 'immigr',])
IFc_immig_vda <- VD.A(cosine.similarity ~ forum, 
                      data=intra_forum[intra_forum$word == 'immigr',])
IFc_immig_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'immigr',])
#### Nation ####
IFc_nation_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf' 
                                                              & intra_forum$word == 'nation'])
IFc_nation_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'
                                                              & intra_forum$word == 'nation'])
IFc_nation_wt <- wilcox.test(cosine.similarity ~ forum,
                             data=intra_forum[intra_forum$word == 'nation',])
IFc_nation_vda <- VD.A(cosine.similarity ~ forum, 
                       data=intra_forum[intra_forum$word == 'nation',])
IFc_nation_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                        data=intra_forum[intra_forum$word == 'nation',])
#### Black ####
IFc_black_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf' 
                                                             & intra_forum$word == 'black'])
IFc_black_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'
                                                             & intra_forum$word == 'black'])
IFc_black_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=intra_forum[intra_forum$word == 'black',])
IFc_black_vda <- VD.A(cosine.similarity ~ forum, 
                      data=intra_forum[intra_forum$word == 'black',])
IFc_black_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'black',])
#### White ####
IFc_white_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf' 
                                                             & intra_forum$word == 'white'])
IFc_white_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'
                                                             & intra_forum$word == 'white'])
IFc_white_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=intra_forum[intra_forum$word == 'white',])
IFc_white_vda <- VD.A(cosine.similarity ~ forum, 
                      data=intra_forum[intra_forum$word == 'white',])
IFc_white_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'white',])
#### People ####
IFc_peopl_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf' 
                                                             & intra_forum$word == 'peopl'])
IFc_peopl_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'
                                                             & intra_forum$word == 'peopl'])
IFc_peopl_wt <- wilcox.test(cosine.similarity ~ forum,
                            data=intra_forum[intra_forum$word == 'peopl',])
IFc_peopl_vda <- VD.A(cosine.similarity ~ forum, 
                      data=intra_forum[intra_forum$word == 'peopl',])
IFc_peopl_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'peopl',])
#### Race ####
IFc_race_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf' 
                                                            & intra_forum$word == 'race'])
IFc_race_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'
                                                            & intra_forum$word == 'race'])
IFc_race_wt <- wilcox.test(cosine.similarity ~ forum,
                           data=intra_forum[intra_forum$word == 'race',])
IFc_race_vda <- VD.A(cosine.similarity ~ forum, 
                     data=intra_forum[intra_forum$word == 'race',])
IFc_race_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                      data=intra_forum[intra_forum$word == 'race',])

#### Tot ####
IFc_Sf.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'Sf'])
IFc_RC.median <- MedianCI(intra_forum$cosine.similarity[intra_forum$forum == 'RC'])
IFc_wt <- wilcox.test(cosine.similarity ~ forum,
                      data=intra_forum)
IFc_vda <- VD.A(cosine.similarity ~ forum, 
                data=intra_forum)
IFc_ci.vda<- vda(cosine.similarity ~ forum, ci=TRUE,
                 data=intra_forum)

## Overlap ####
### Beta - regression ####
#### Immigration ####
IFo_immig_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'immigr',],
                           link = 'logit')
IFo_immig <- summary(IFo_immig_model)

vcov <- sandwich(IFo_immig_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_immig_coeftest <- coeftest(IFo_immig_model, vcov. = vcov)
#### Nation ####
IFo_nation_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                            data = intra_forum[intra_forum$word == 'nation',],
                            link = 'logit')
IFo_nation <- summary(IFo_nation_model)

vcov <- sandwich(IFo_nation_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_nation_coeftest <- coeftest(IFo_nation_model, vcov. = vcov)
#### Black ####
IFo_black_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'black',],
                           link = 'logit')
IFo_black <- summary(IFo_black_model)

vcov <- sandwich(IFo_black_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_black_coeftest <- coeftest(IFo_black_model, vcov. = vcov)
#### White ####
IFo_white_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'white',],
                           link = 'logit')
IFo_white <- summary(IFo_white_model)

vcov <- sandwich(IFo_white_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_white_coeftest <- coeftest(IFo_white_model, vcov. = vcov)
#### People ####
IFo_peopl_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                           data = intra_forum[intra_forum$word == 'peopl',],
                           link = 'logit')
IFo_peopl <- summary(IFo_peopl_model)

vcov <- sandwich(IFo_peopl_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_peopl_coeftest <- coeftest(IFo_peopl_model, vcov. = vcov)
#### Race ####
IFo_race_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                          data = intra_forum[intra_forum$word == 'race',],
                          link = 'logit')
IFo_race <- summary(IFo_race_model)

vcov <- sandwich(IFo_race_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_race_coeftest <- coeftest(IFo_race_model, vcov. = vcov)

#### Tot ####
IFo_model <- betareg(neighborhood.overlap ~ forum * word.frequency,
                     data = intra_forum,
                     link = 'logit')
IFo <- summary(IFo_model)

vcov <- sandwich(IFo_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IFo_coeftest <- coeftest(IFo_model, vcov. = vcov)

rm(vcov, robust_se)
### Mann–Whitney U test ####
#### Immigration ####
IFo_immig_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf' 
                                                                & intra_forum$word == 'immigr'])
IFo_immig_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'
                                                                & intra_forum$word == 'immigr'])
IFo_immig_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=intra_forum[intra_forum$word == 'immigr',])
IFo_immig_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=intra_forum[intra_forum$word == 'immigr',])
IFo_immig_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'immigr',])
#### Nation ####
IFo_nation_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf' 
                                                                 & intra_forum$word == 'nation'])
IFo_nation_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'
                                                                 & intra_forum$word == 'nation'])
IFo_nation_wt <- wilcox.test(neighborhood.overlap ~ forum,
                             data=intra_forum[intra_forum$word == 'nation',])
IFo_nation_vda <- VD.A(neighborhood.overlap ~ forum, 
                       data=intra_forum[intra_forum$word == 'nation',])
IFo_nation_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                        data=intra_forum[intra_forum$word == 'nation',])
#### Black ####
IFo_black_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf' 
                                                                & intra_forum$word == 'black'])
IFo_black_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'
                                                                & intra_forum$word == 'black'])
IFo_black_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=intra_forum[intra_forum$word == 'black',])
IFo_black_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=intra_forum[intra_forum$word == 'black',])
IFo_black_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'black',])
#### White ####
IFo_white_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf' 
                                                                & intra_forum$word == 'white'])
IFo_white_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'
                                                                & intra_forum$word == 'white'])
IFo_white_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=intra_forum[intra_forum$word == 'white',])
IFo_white_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=intra_forum[intra_forum$word == 'white',])
IFo_white_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'white',])
#### People ####
IFo_peopl_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf' 
                                                                & intra_forum$word == 'peopl'])
IFo_peopl_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'
                                                                & intra_forum$word == 'peopl'])
IFo_peopl_wt <- wilcox.test(neighborhood.overlap ~ forum,
                            data=intra_forum[intra_forum$word == 'peopl',])
IFo_peopl_vda <- VD.A(neighborhood.overlap ~ forum, 
                      data=intra_forum[intra_forum$word == 'peopl',])
IFo_peopl_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                       data=intra_forum[intra_forum$word == 'peopl',])
#### Race ####
IFo_race_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf' 
                                                               & intra_forum$word == 'race'])
IFo_race_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'
                                                               & intra_forum$word == 'race'])
IFo_race_wt <- wilcox.test(neighborhood.overlap ~ forum,
                           data=intra_forum[intra_forum$word == 'race',])
IFo_race_vda <- VD.A(neighborhood.overlap ~ forum, 
                     data=intra_forum[intra_forum$word == 'race',])
IFo_race_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                      data=intra_forum[intra_forum$word == 'race',])


#### Tot ####
IFo_Sf.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'Sf'])
IFo_RC.median <- MedianCI(intra_forum$neighborhood.overlap[intra_forum$forum == 'RC'])
IFo_wt <- wilcox.test(neighborhood.overlap ~ forum,
                      data=intra_forum)
IFo_vda <- VD.A(neighborhood.overlap ~ forum, 
                data=intra_forum)
IFo_ci.vda<- vda(neighborhood.overlap ~ forum, ci=TRUE,
                 data=intra_forum)


#### Median word frequency ####
#### Immigration ####
IFwf_immig_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'
                                                           & intra_forum$word == 'immigr'])
IFwf_immig_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'
                                                           & intra_forum$word == 'immigr'])
#### Nation ####
IFwf_nation_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'
                                                            & intra_forum$word == 'nation'])
IFwf_nation_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'
                                                            & intra_forum$word == 'nation'])
#### Black ####
IFwf_black_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'
                                                           & intra_forum$word == 'black'])
IFwf_black_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'
                                                           & intra_forum$word == 'black'])
#### White ####
IFwf_white_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'
                                                           & intra_forum$word == 'white'])
IFwf_white_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'
                                                           & intra_forum$word == 'white'])
#### People ####
IFwf_peopl_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'
                                                           & intra_forum$word == 'peopl'])
IFwf_peopl_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'
                                                           & intra_forum$word == 'peopl'])
#### Race ####
IFwf_race_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'
                                                          & intra_forum$word == 'race'])
IFwf_race_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'
                                                          & intra_forum$word == 'race'])

#### Tot ####
IFwf_Sf.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'Sf'])
IFwf_RC.median <- MedianCI(intra_forum$word.frequency[intra_forum$forum == 'RC'])

## TAB median + word freq ####
word <- c('Immigration', '','','', 'Nation', '','','', 'Black', '','','', 
          'White', '','','', 'People', '','','', 'Race', '','','', 'Tot', '','','' )

measure <- c('cosine', 'cosine', 'overlap', 'overlap', 'cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'overlap', 'overlap', 'cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'overlap', 'overlap', 'cosine', 'cosine', 'overlap', 'overlap',
             'cosine', 'cosine', 'overlap', 'overlap')

group <- c('Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC',
           'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC',
           'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC', 'Sf', 'RC',
           'Sf', 'RC', 'Sf', 'RC')

medians <- list(IFc_immig_Sf.median, IFc_immig_RC.median, IFo_immig_Sf.median, IFo_immig_RC.median,
                IFc_nation_Sf.median, IFc_nation_RC.median, IFo_nation_Sf.median, IFo_nation_RC.median,
                IFc_black_Sf.median, IFc_black_RC.median, IFo_black_Sf.median, IFo_black_RC.median,
                IFc_white_Sf.median, IFc_white_RC.median, IFo_white_Sf.median, IFo_white_RC.median,
                IFc_peopl_Sf.median, IFc_peopl_RC.median, IFo_peopl_Sf.median, IFo_peopl_RC.median,
                IFc_race_Sf.median, IFc_race_RC.median, IFo_race_Sf.median, IFo_race_RC.median,
                IFc_Sf.median, IFc_RC.median, IFo_Sf.median, IFo_RC.median)
semantic.distance <- c()
for (m in medians){
  a <- c(round(m[1], 3),' (', round(m[2], 3), '-', 
         round(m[3], 3), ')')
  a <- paste(a, sep="", collapse="")
  semantic.distance <- append(semantic.distance, a)
  rm(a, m)
}

frequencies <- list(IFwf_immig_Sf.median, IFwf_immig_RC.median, IFwf_immig_Sf.median, IFwf_immig_RC.median,
                    IFwf_nation_Sf.median, IFwf_nation_RC.median, IFwf_nation_Sf.median, IFwf_nation_RC.median,
                    IFwf_black_Sf.median, IFwf_black_RC.median, IFwf_black_Sf.median, IFwf_black_RC.median,
                    IFwf_white_Sf.median, IFwf_white_RC.median, IFwf_white_Sf.median, IFwf_white_RC.median,
                    IFwf_peopl_Sf.median, IFwf_peopl_RC.median, IFwf_peopl_Sf.median, IFwf_peopl_RC.median,
                    IFwf_race_Sf.median, IFwf_race_RC.median, IFwf_race_Sf.median, IFwf_race_RC.median,
                    IFwf_Sf.median, IFwf_RC.median, IFwf_Sf.median, IFwf_RC.median)
word.frequency <- c()
for (f in frequencies){
  a <- c(round(f[1], 3),' (', round(f[2], 3), '-', 
         round(f[3], 3), ')')
  a <- paste(a, sep="", collapse="")
  word.frequency <- append(word.frequency, a)
  rm(a, f)
}

es_ci <- list(IFc_immig_ci.vda, IFc_immig_ci.vda, IFo_immig_ci.vda, IFo_immig_ci.vda,
              IFc_nation_ci.vda, IFc_nation_ci.vda, IFo_nation_ci.vda, IFo_nation_ci.vda,
              IFc_black_ci.vda, IFc_black_ci.vda, IFo_black_ci.vda, IFo_black_ci.vda,
              IFc_white_ci.vda, IFc_white_ci.vda, IFo_white_ci.vda, IFo_white_ci.vda,
              IFc_peopl_ci.vda, IFc_peopl_ci.vda, IFo_peopl_ci.vda, IFo_peopl_ci.vda,
              IFc_race_ci.vda, IFc_race_ci.vda, IFo_race_ci.vda, IFo_race_ci.vda,
              IFc_ci.vda, IFc_ci.vda, IFo_ci.vda, IFo_ci.vda)

effect.size <- c()
for (e in es_ci){
  a <- c(e[1],' (', e[2], '-', e[3], ')')
  a <- paste(a, sep="", collapse="")
  effect.size <- append(effect.size, a)
  rm(a, e)
}

IF_median_TAB <- data.frame(word, measure, group, word.frequency, semantic.distance, effect.size)
View(IF_median_TAB)
rm(medians, frequencies, es_ci)
rm(word, measure, group, word.frequency, semantic.distance, effect.size)

## TAB betareg ####
models <- list(IFc_immig, IFc_nation, IFc_black, IFc_white, IFc_peopl, IFc_race, IFc, 
               IFo_immig, IFo_nation, IFo_black, IFo_white, IFo_peopl, IFo_race, IFo)
coef <- c('intercept', 'forumSf', 'word.frequency', 'forumSf:word.frequency')
IF_coef_tab <- data.frame(coef)
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
  IF_coef_tab <- cbind(IF_coef_tab, col)
  rm(vec, n, m, estimate, se, odds.ratio, low.b, high.b, cell, col)
}
rm(models, coef)
names(IF_coef_tab) <- c('coef', 
                        'IFc_immig', 'IFc_nation', 'IFc_black', 'IFc_white', 'IFc_peopl', 'IFc_race',
                        'IFc_TOT',
                        'IFo_immig', 'IFo_nation', 'IFo_black', 'IFo_white', 'IFo_peopl', 'IFo_race',
                        'IFo_TOT')
View(IF_coef_tab)

#### Clean ####
rm(IFc_immig_Sf.median, IFc_immig_RC.median, IFo_immig_Sf.median, IFo_immig_RC.median,
   IFc_nation_Sf.median, IFc_nation_RC.median, IFo_nation_Sf.median, IFo_nation_RC.median,
   IFc_black_Sf.median, IFc_black_RC.median, IFo_black_Sf.median, IFo_black_RC.median,
   IFc_white_Sf.median, IFc_white_RC.median, IFo_white_Sf.median, IFo_white_RC.median,
   IFc_peopl_Sf.median, IFc_peopl_RC.median, IFo_peopl_Sf.median, IFo_peopl_RC.median,
   IFc_race_Sf.median, IFc_race_RC.median, IFo_race_Sf.median, IFo_race_RC.median,
   IFc_Sf.median, IFc_RC.median, IFo_Sf.median, IFo_RC.median)
rm(IFwf_immig_Sf.median, IFwf_immig_RC.median,
   IFwf_nation_Sf.median, IFwf_nation_RC.median, 
   IFwf_black_Sf.median, IFwf_black_RC.median, 
   IFwf_white_Sf.median, IFwf_white_RC.median, 
   IFwf_peopl_Sf.median, IFwf_peopl_RC.median, 
   IFwf_race_Sf.median, IFwf_race_RC.median,
   IFwf_Sf.median, IFwf_RC.median)
rm(IFc_immig_ci.vda, IFo_immig_ci.vda,
   IFc_nation_ci.vda, IFo_nation_ci.vda,
   IFc_black_ci.vda, IFo_black_ci.vda,
   IFc_white_ci.vda, IFo_white_ci.vda,
   IFc_peopl_ci.vda, IFo_peopl_ci.vda,
   IFc_race_ci.vda, IFo_race_ci.vda,
   IFc_ci.vda, IFo_ci.vda)
rm(IFc_immig_vda, IFo_immig_vda,
   IFc_nation_vda, IFo_nation_vda,
   IFc_black_vda, IFo_black_vda,
   IFc_white_vda, IFo_white_vda,
   IFc_peopl_vda, IFo_peopl_vda,
   IFc_race_vda, IFo_race_vda,
   IFc_vda, IFo_vda)
rm(IFc_immig_wt, IFo_immig_wt,
   IFc_nation_wt, IFo_nation_wt,
   IFc_black_wt, IFo_black_wt,
   IFc_white_wt, IFo_white_wt,
   IFc_peopl_wt, IFo_peopl_wt,
   IFc_race_wt, IFo_race_wt,
   IFc_wt, IFo_wt)
rm(IFc_immig_coeftest, IFo_immig_coeftest,
   IFc_nation_coeftest, IFo_nation_coeftest,
   IFc_black_coeftest, IFo_black_coeftest,
   IFc_white_coeftest, IFo_white_coeftest,
   IFc_peopl_coeftest, IFo_peopl_coeftest,
   IFc_race_coeftest, IFo_race_coeftest,
   IFc_coeftest, IFo_coeftest)
rm(IFc_immig, IFo_immig,
   IFc_nation, IFo_nation,
   IFc_black, IFo_black,
   IFc_white, IFo_white,
   IFc_peopl, IFo_peopl,
   IFc_race, IFo_race,
   IFc, IFo)
rm(IFc_immig_model, IFo_immig_model,
   IFc_nation_model, IFo_nation_model,
   IFc_black_model, IFo_black_model,
   IFc_white_model, IFo_white_model,
   IFc_peopl_model, IFo_peopl_model,
   IFc_race_model, IFo_race_model,
   IFc_model, IFo_model)

## PLOTS #####
### Cosine ####
Sf_immig <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'immigr',
                    link = "logit")
Sf_nation <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                     subset = forum == 'Sf' & word == 'nation',
                     link = "logit")
Sf_black <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'black',
                    link = "logit")
Sf_white <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'white',
                    link = "logit")
Sf_peopl <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'peopl',
                    link = "logit")
Sf_race <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                   subset = forum == 'Sf' & word == 'race',
                   link = "logit")
Sf <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
              subset = forum == 'Sf',
              link = "logit")

RC_immig <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'immigr',
                    link = "logit")
RC_nation <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                     subset = forum == 'RC' & word == 'nation',
                     link = "logit")
RC_black <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'black',
                    link = "logit")
RC_white <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'white',
                    link = "logit")
RC_peopl <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'peopl',
                    link = "logit")
RC_race <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
                   subset = forum == 'RC' & word == 'race',
                   link = "logit")
RC <- betareg(cosine.similarity ~ word.frequency, data = intra_forum, 
              subset = forum == 'RC',
              link = "logit")

IFc_plot <- ggplot(intra_forum, aes(x = word.frequency, y = cosine.similarity)) +
  geom_line(aes(y = predict(Sf_immig, intra_forum),
                colour = "immig", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_nation, intra_forum), 
                colour = "nation", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_black, intra_forum),
                colour = "black", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_white, intra_forum), 
                colour = "white", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_peopl, intra_forum),
                colour = "peopl", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_race, intra_forum), 
                colour = "race", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf, intra_forum), 
                colour = "tot", linetype= 'Sf')) +
  geom_line(aes(y = predict(RC_immig, intra_forum),
                colour = "immig", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_nation, intra_forum), 
                colour = "nation", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_black, intra_forum),
                colour = "black", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_white, intra_forum), 
                colour = "white", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_peopl, intra_forum),
                colour = "peopl", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_race, intra_forum), 
                colour = "race", linetype= 'RC')) +
  geom_line(aes(y = predict(RC, intra_forum), 
                colour = "tot", linetype= 'RC')) +
  scale_colour_manual("", values = c("red", 'orange', 'green', "blue", 'purple', 'black', 'pink')) +
  ggtitle('Intra forum variability by word - Semantic Distance') +
  xlab("word frequency") + 
  ylab("semantic distance")+
  theme_bw()
IFc_plot
### Overlap ####
Sf_immig <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'immigr',
                    link = "logit")
Sf_nation <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                     subset = forum == 'Sf' & word == 'nation',
                     link = "logit")
Sf_black <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'black',
                    link = "logit")
Sf_white <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'white',
                    link = "logit")
Sf_peopl <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'Sf' & word == 'peopl',
                    link = "logit")
Sf_race <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                   subset = forum == 'Sf' & word == 'race',
                   link = "logit")
Sf <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
              subset = forum == 'Sf',
              link = "logit")

RC_immig <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'immigr',
                    link = "logit")
RC_nation <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                     subset = forum == 'RC' & word == 'nation',
                     link = "logit")
RC_black <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'black',
                    link = "logit")
RC_white <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'white',
                    link = "logit")
RC_peopl <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                    subset = forum == 'RC' & word == 'peopl',
                    link = "logit")
RC_race <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
                   subset = forum == 'RC' & word == 'race',
                   link = "logit")
RC <- betareg(neighborhood.overlap ~ word.frequency, data = intra_forum, 
              subset = forum == 'RC',
              link = "logit")

IFo_plot <- ggplot(intra_forum, aes(x = word.frequency, y = neighborhood.overlap)) +
  geom_line(aes(y = predict(Sf_immig, intra_forum),
                colour = "immig", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_nation, intra_forum), 
                colour = "nation", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_black, intra_forum),
                colour = "black", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_white, intra_forum), 
                colour = "white", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_peopl, intra_forum),
                colour = "peopl", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_race, intra_forum), 
                colour = "race", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf, intra_forum), 
                colour = "tot", linetype= 'Sf')) +
  geom_line(aes(y = predict(RC_immig, intra_forum),
                colour = "immig", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_nation, intra_forum), 
                colour = "nation", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_black, intra_forum),
                colour = "black", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_white, intra_forum), 
                colour = "white", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_peopl, intra_forum),
                colour = "peopl", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_race, intra_forum), 
                colour = "race", linetype= 'RC')) +
  geom_line(aes(y = predict(RC, intra_forum), 
                colour = "tot", linetype= 'RC')) +
  scale_colour_manual("", values = c("red", 'orange', 'green', "blue", 'purple', 'black', 'pink')) +
  ggtitle('Intra forum variability by word - Neighborhood Overlap') +
  xlab("word frequency") + 
  ylab("neighborhood overlap")+
  theme_bw()
IFo_plot

rm(Sf_immig, Sf_nation, Sf_black, Sf_white, Sf_peopl, Sf_race,
   RC_immig, RC_nation, RC_black, RC_white, RC_peopl, RC_race)

### .-------------------. ######
# INTER-INTRA FORUM DIFFERENCES ####
## Cosine ####
### Beta - regression ####
#### Immigration ####
IIDc_immig_model <- betareg(cosine.similarity ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'immigr',],
                           link = 'logit')
IIDc_immig <- summary(IIDc_immig_model)

vcov <- sandwich(IIDc_immig_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_immig_coeftest <- coeftest(IIDc_immig_model, vcov. = vcov)
#### Nation ####
IIDc_nation_model <- betareg(cosine.similarity ~ category * word.frequency,
                            data = inter_intra_dif[inter_intra_dif$word == 'nation',],
                            link = 'logit')
IIDc_nation <- summary(IIDc_nation_model)

vcov <- sandwich(IIDc_nation_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_nation_coeftest <- coeftest(IIDc_nation_model, vcov. = vcov)
#### Black ####
IIDc_black_model <- betareg(cosine.similarity ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'black',],
                           link = 'logit')
IIDc_black <- summary(IIDc_black_model)

vcov <- sandwich(IIDc_black_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_black_coeftest <- coeftest(IIDc_black_model, vcov. = vcov)
#### White ####
IIDc_white_model <- betareg(cosine.similarity ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'white',],
                           link = 'logit')
IIDc_white <- summary(IIDc_white_model)

vcov <- sandwich(IIDc_white_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_white_coeftest <- coeftest(IIDc_white_model, vcov. = vcov)
#### People ####
IIDc_peopl_model <- betareg(cosine.similarity ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'peopl',],
                           link = 'logit')
IIDc_peopl <- summary(IIDc_peopl_model)

vcov <- sandwich(IIDc_peopl_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_peopl_coeftest <- coeftest(IIDc_peopl_model, vcov. = vcov)
#### Race ####
IIDc_race_model <- betareg(cosine.similarity ~ category * word.frequency,
                          data = inter_intra_dif[inter_intra_dif$word == 'race',],
                          link = 'logit')
IIDc_race <- summary(IIDc_race_model)

vcov <- sandwich(IIDc_race_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_peopl_coeftest <- coeftest(IIDc_race_model, vcov. = vcov)

#### Tot ####
IIDc_model <- betareg(cosine.similarity ~ category * word.frequency,
                     data = inter_intra_dif,
                     link = 'logit')
IIDc <- summary(IIDc_model)

vcov <- sandwich(IIDc_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDc_peopl_coeftest <- coeftest(IIDc_model, vcov. = vcov)
### Kruskal-Wallis rank sum test ####
#### Immigration ####
IIDc_immig_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'immigr'])
IIDc_immig_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra' 
                                                              & inter_intra_dif$word == 'immigr'])
IIDc_immig_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'
                                                              & inter_intra_dif$word == 'immigr'])

IIDc_immig_kruskal.test <- kruskal.test(cosine.similarity ~ category, 
                                  data = inter_intra_dif[inter_intra_dif$word == 'immigr',])
IIDc_immig_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,
                                   data = inter_intra_dif[inter_intra_dif$word == 'immigr',])
IIDc_immig_wt <- wilcox_test(cosine.similarity ~ category, p.adjust.method = "bonferroni", 
                       data = inter_intra_dif[inter_intra_dif$word == 'immigr',])

#### Nation ####
IIDc_nation_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'nation'])
IIDc_nation_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'nation'])
IIDc_nation_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'nation'])

IIDc_nation_kruskal.test <- kruskal.test(cosine.similarity ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'nation',])
IIDc_nation_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'nation',])
IIDc_nation_wt <- wilcox_test(cosine.similarity ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'nation',])

#### Black ####
IIDc_black_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter' 
                                                                       & inter_intra_dif$word == 'black'])
IIDc_black_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra' 
                                                                    & inter_intra_dif$word == 'black'])
IIDc_black_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'
                                                                    & inter_intra_dif$word == 'black'])

IIDc_black_kruskal.test <- kruskal.test(cosine.similarity ~ category, 
                                         data = inter_intra_dif[inter_intra_dif$word == 'black',])
IIDc_black_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,
                                          data = inter_intra_dif[inter_intra_dif$word == 'black',])
IIDc_black_wt <- wilcox_test(cosine.similarity ~ category, p.adjust.method = "bonferroni", 
                              data = inter_intra_dif[inter_intra_dif$word == 'black',])

#### White ####
IIDc_white_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'white'])
IIDc_white_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'white'])
IIDc_white_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'white'])

IIDc_white_kruskal.test <- kruskal.test(cosine.similarity ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'white',])
IIDc_white_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'white',])
IIDc_white_wt <- wilcox_test(cosine.similarity ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'white',])

#### People ####
IIDc_peopl_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'peopl'])
IIDc_peopl_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'peopl'])
IIDc_peopl_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'peopl'])

IIDc_peopl_kruskal.test <- kruskal.test(cosine.similarity ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'peopl',])
IIDc_peopl_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'peopl',])
IIDc_peopl_wt <- wilcox_test(cosine.similarity ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'peopl',])

#### Race ####
IIDc_race_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'race'])
IIDc_race_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'race'])
IIDc_race_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'race'])

IIDc_race_kruskal.test <- kruskal.test(cosine.similarity ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'race',])
IIDc_race_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'race',])
IIDc_race_wt <- wilcox_test(cosine.similarity ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'race',])

#### Tot ####
IIDc_inter.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == '_inter'])
IIDc_Sf.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'Sf_intra'])
IIDc_RC.median <- MedianCI(inter_intra_dif$cosine.similarity[inter_intra_dif$category == 'RC_intra'])

IIDc_kruskal.test <- kruskal.test(cosine.similarity ~ category, 
                                       data = inter_intra_dif)
IIDc_kruskal.es <- kruskal_effsize(cosine.similarity ~ category, ci = T,
                                        data = inter_intra_dif)
IIDc_wt <- wilcox_test(cosine.similarity ~ category, p.adjust.method = "bonferroni", 
                            data = inter_intra_dif)

## Overlap ####
### Beta - regression ####
#### Immigration ####
IIDo_immig_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'immigr',],
                           link = 'logit')
IIDo_immig <- summary(IIDo_immig_model)

vcov <- sandwich(IIDo_immig_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_immig_coeftest <- coeftest(IIDo_immig_model, vcov. = vcov)
#### Nation ####
IIDo_nation_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                            data = inter_intra_dif[inter_intra_dif$word == 'nation',],
                            link = 'logit')
IIDo_nation <- summary(IIDo_nation_model)

vcov <- sandwich(IIDo_nation_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_nation_coeftest <- coeftest(IIDo_nation_model, vcov. = vcov)
#### Black ####
IIDo_black_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'black',],
                           link = 'logit')
IIDo_black <- summary(IIDo_black_model)

vcov <- sandwich(IIDo_black_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_black_coeftest <- coeftest(IIDo_black_model, vcov. = vcov)
#### White ####
IIDo_white_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'white',],
                           link = 'logit')
IIDo_white <- summary(IIDo_white_model)

vcov <- sandwich(IIDo_white_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_white_coeftest <- coeftest(IIDo_white_model, vcov. = vcov)
#### People ####
IIDo_peopl_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                           data = inter_intra_dif[inter_intra_dif$word == 'peopl',],
                           link = 'logit')
IIDo_peopl <- summary(IIDo_peopl_model)

vcov <- sandwich(IIDo_peopl_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_peopl_coeftest <- coeftest(IIDo_peopl_model, vcov. = vcov)
#### Race ####
IIDo_race_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                          data = inter_intra_dif[inter_intra_dif$word == 'race',],
                          link = 'logit')
IIDo_race <- summary(IIDo_race_model)

vcov <- sandwich(IIDo_race_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_race_coeftest <- coeftest(IIDo_race_model, vcov. = vcov)

#### Tot ####
IIDo_model <- betareg(neighborhood.overlap ~ category * word.frequency,
                     data = inter_intra_dif,
                     link = 'logit')
IIDo <- summary(IIDo_model)

vcov <- sandwich(IIDo_model, type = "HC1")
robust_se <- sqrt(diag(vcov))
IIDo_coeftest <- coeftest(IIDo_model, vcov. = vcov)

rm(vcov, robust_se)
### Kruskal-Wallis rank sum test ####
#### Immigration ####
IIDo_immig_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'immigr'])
IIDo_immig_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'immigr'])
IIDo_immig_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'immigr'])

IIDo_immig_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'immigr',])
IIDo_immig_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'immigr',])
IIDo_immig_wt <- wilcox_test(neighborhood.overlap ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'immigr',])

#### Nation ####
IIDo_nation_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter' 
                                                                       & inter_intra_dif$word == 'nation'])
IIDo_nation_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra' 
                                                                    & inter_intra_dif$word == 'nation'])
IIDo_nation_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'
                                                                    & inter_intra_dif$word == 'nation'])

IIDo_nation_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, 
                                         data = inter_intra_dif[inter_intra_dif$word == 'nation',])
IIDo_nation_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,
                                          data = inter_intra_dif[inter_intra_dif$word == 'nation',])
IIDo_nation_wt <- wilcox_test(neighborhood.overlap ~ category, p.adjust.method = "bonferroni", 
                              data = inter_intra_dif[inter_intra_dif$word == 'nation',])

#### Black ####
IIDo_black_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'black'])
IIDo_black_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'black'])
IIDo_black_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'black'])

IIDo_black_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'black',])
IIDo_black_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'black',])
IIDo_black_wt <- wilcox_test(neighborhood.overlap ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'black',])

#### White ####
IIDo_white_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'white'])
IIDo_white_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'white'])
IIDo_white_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'white'])

IIDo_white_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'white',])
IIDo_white_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'white',])
IIDo_white_wt <- wilcox_test(neighborhood.overlap ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'white',])

#### People ####
IIDo_peopl_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter' 
                                                                      & inter_intra_dif$word == 'peopl'])
IIDo_peopl_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra' 
                                                                   & inter_intra_dif$word == 'peopl'])
IIDo_peopl_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'
                                                                   & inter_intra_dif$word == 'peopl'])

IIDo_peopl_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, 
                                        data = inter_intra_dif[inter_intra_dif$word == 'peopl',])
IIDo_peopl_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,
                                         data = inter_intra_dif[inter_intra_dif$word == 'peopl',])
IIDo_peopl_wt <- wilcox_test(neighborhood.overlap ~ category, p.adjust.method = "bonferroni", 
                             data = inter_intra_dif[inter_intra_dif$word == 'peopl',])

#### Race ####
IIDo_race_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter' 
                                                                     & inter_intra_dif$word == 'race'])
IIDo_race_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra' 
                                                                  & inter_intra_dif$word == 'race'])
IIDo_race_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'
                                                                  & inter_intra_dif$word == 'race'])

IIDo_race_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, 
                                       data = inter_intra_dif[inter_intra_dif$word == 'race',])
IIDo_race_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,
                                        data = inter_intra_dif[inter_intra_dif$word == 'race',])
IIDo_race_wt <- wilcox_test(neighborhood.overlap ~ category, p.adjust.method = "bonferroni", 
                            data = inter_intra_dif[inter_intra_dif$word == 'race',])

#### Tot ####
IIDo_inter.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == '_inter'])
IIDo_Sf.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'Sf_intra'])
IIDo_RC.median <- MedianCI(inter_intra_dif$neighborhood.overlap[inter_intra_dif$category == 'RC_intra'])

IIDo_kruskal.test <- kruskal.test(neighborhood.overlap ~ category, 
                                  data = inter_intra_dif)
IIDo_kruskal.es <- kruskal_effsize(neighborhood.overlap ~ category, ci = T,
                                   data = inter_intra_dif)
IIDo_wt <- wilcox_test(neighborhood.overlap ~ category, p.adjust.method = "bonferroni", 
                       data = inter_intra_dif)

#### Median word frequency ####
#### Immigration ####
IIDwf_immig_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'
                                                                 & inter_intra_dif$word == 'immigr'])
IIDwf_immig_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'
                                                            & inter_intra_dif$word == 'immigr'])
IIDwf_immig_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'
                                                            & inter_intra_dif$word == 'immigr'])
#### Nation ####
IIDwf_nation_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'
                                                                    & inter_intra_dif$word == 'nation'])
IIDwf_nation_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'
                                                             & inter_intra_dif$word == 'nation'])
IIDwf_nation_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'
                                                             & inter_intra_dif$word == 'nation'])
#### Black ####
IIDwf_black_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'
                                                                    & inter_intra_dif$word == 'black'])
IIDwf_black_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'
                                                            & inter_intra_dif$word == 'black'])
IIDwf_black_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'
                                                            & inter_intra_dif$word == 'black'])
#### White ####
IIDwf_white_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'
                                                                    & inter_intra_dif$word == 'white'])
IIDwf_white_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'
                                                            & inter_intra_dif$word == 'white'])
IIDwf_white_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'
                                                            & inter_intra_dif$word == 'white'])
#### People ####
IIDwf_peopl_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'
                                                                    & inter_intra_dif$word == 'peopl'])
IIDwf_peopl_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'
                                                            & inter_intra_dif$word == 'peopl'])
IIDwf_peopl_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'
                                                            & inter_intra_dif$word == 'peopl'])
#### Race ####
IIDwf_race_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'
                                                                    & inter_intra_dif$word == 'race'])
IIDwf_race_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'
                                                           & inter_intra_dif$word == 'race'])
IIDwf_race_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'
                                                           & inter_intra_dif$word == 'race'])

#### Tot ####
IIDwf_inter.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == '_inter'])
IIDwf_Sf.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'Sf_intra'])
IIDwf_RC.median <- MedianCI(inter_intra_dif$word.frequency[inter_intra_dif$category == 'RC_intra'])

## TAB median + word freq ####
word <- c('Immigration', '','','','','', 'Nation', '','','','','', 'Black', '','','','','', 
          'White', '','','','','', 'People', '','','','','', 'Race', '','','', '','','Tot', '','','','','' )

measure <- c('cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap', 
             'cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap',
             'cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap', 
             'cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap',
             'cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap', 
             'cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap',
             'cosine', 'cosine', 'cosine', 'overlap', 'overlap', 'overlap')

group <- c('inter', 'Sf', 'RC', 'inter','Sf', 'RC', 
           'inter', 'Sf', 'RC', 'inter', 'Sf', 'RC',
           'inter', 'Sf', 'RC', 'inter', 'Sf', 'RC', 
           'inter', 'Sf', 'RC', 'inter', 'Sf', 'RC',
           'inter', 'Sf', 'RC', 'inter', 'Sf', 'RC', 
           'inter', 'Sf', 'RC', 'inter', 'Sf', 'RC',
           'inter', 'Sf', 'RC', 'inter', 'Sf', 'RC')

medians <- list(IIDc_immig_inter.median, IIDc_immig_Sf.median, IIDc_immig_RC.median, 
                IIDo_immig_inter.median, IIDo_immig_Sf.median, IIDo_immig_RC.median,
                IIDc_nation_inter.median, IIDc_nation_Sf.median, IIDc_nation_RC.median, 
                IIDo_nation_inter.median, IIDo_nation_Sf.median, IIDo_nation_RC.median,
                IIDc_black_inter.median, IIDc_black_Sf.median, IIDc_black_RC.median, 
                IIDo_black_inter.median, IIDo_black_Sf.median, IIDo_black_RC.median,
                IIDc_white_inter.median, IIDc_white_Sf.median, IIDc_white_RC.median, 
                IIDo_white_inter.median, IIDo_white_Sf.median, IIDo_white_RC.median,
                IIDc_peopl_inter.median, IIDc_peopl_Sf.median, IIDc_peopl_RC.median, 
                IIDo_peopl_inter.median, IIDo_peopl_Sf.median, IIDo_peopl_RC.median,
                IIDc_race_inter.median, IIDc_race_Sf.median, IIDc_race_RC.median, 
                IIDo_race_inter.median, IIDo_race_Sf.median, IIDo_race_RC.median,
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

frequencies <- list(IIDwf_immig_inter.median, IIDwf_immig_Sf.median, IIDwf_immig_RC.median, 
                    IIDwf_immig_inter.median, IIDwf_immig_Sf.median, IIDwf_immig_RC.median,
                    IIDwf_nation_inter.median, IIDwf_nation_Sf.median, IIDwf_nation_RC.median, 
                    IIDwf_nation_inter.median, IIDwf_nation_Sf.median, IIDwf_nation_RC.median,
                    IIDwf_black_inter.median, IIDwf_black_Sf.median, IIDwf_black_RC.median, 
                    IIDwf_black_inter.median, IIDwf_black_Sf.median, IIDwf_black_RC.median,
                    IIDwf_white_inter.median, IIDwf_white_Sf.median, IIDwf_white_RC.median, 
                    IIDwf_white_inter.median, IIDwf_white_Sf.median, IIDwf_white_RC.median,
                    IIDwf_peopl_inter.median, IIDwf_peopl_Sf.median, IIDwf_peopl_RC.median, 
                    IIDwf_peopl_inter.median, IIDwf_peopl_Sf.median, IIDwf_peopl_RC.median,
                    IIDwf_race_inter.median, IIDwf_race_Sf.median, IIDwf_race_RC.median, 
                    IIDwf_race_inter.median, IIDwf_race_Sf.median, IIDwf_race_RC.median,
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

es_ci <- list(IIDc_immig_kruskal.es, IIDc_immig_kruskal.es, IIDc_immig_kruskal.es, 
              IIDo_immig_kruskal.es, IIDo_immig_kruskal.es, IIDo_immig_kruskal.es,
              IIDc_nation_kruskal.es, IIDc_nation_kruskal.es, IIDc_nation_kruskal.es, 
              IIDo_nation_kruskal.es, IIDo_nation_kruskal.es, IIDo_nation_kruskal.es,
              IIDc_black_kruskal.es, IIDc_black_kruskal.es, IIDc_black_kruskal.es, 
              IIDo_black_kruskal.es, IIDo_black_kruskal.es, IIDo_black_kruskal.es,
              IIDc_white_kruskal.es, IIDc_white_kruskal.es, IIDc_white_kruskal.es, 
              IIDo_white_kruskal.es, IIDo_white_kruskal.es, IIDo_white_kruskal.es,
              IIDc_peopl_kruskal.es, IIDc_peopl_kruskal.es, IIDc_peopl_kruskal.es, 
              IIDo_peopl_kruskal.es, IIDo_peopl_kruskal.es, IIDo_peopl_kruskal.es,
              IIDc_race_kruskal.es, IIDc_race_kruskal.es, IIDc_race_kruskal.es, 
              IIDo_race_kruskal.es, IIDo_race_kruskal.es, IIDo_race_kruskal.es,
              IIDc_kruskal.es, IIDc_kruskal.es, IIDc_kruskal.es, 
              IIDo_kruskal.es, IIDo_kruskal.es, IIDo_kruskal.es)

effect.size <- c()
for (e in es_ci){
  a <- c(round(e[3],3),' (', e[4], '-', e[5], ')')
  a <- paste(a, sep="", collapse="")
  effect.size <- append(effect.size, a)
  rm(a, e)
}

IID_median_TAB <- data.frame(word, measure, group, word.frequency, semantic.distance, effect.size)
View(IID_median_TAB)
rm(medians, frequencies, es_ci)
rm(word, measure, group, word.frequency, semantic.distance, effect.size)

## TAB betareg ####
models <- list(IIDc_immig, IIDc_nation, IIDc_black, IIDc_white, IIDc_peopl, IIDc_race, IIDc, 
               IIDo_immig, IIDo_nation, IIDo_black, IIDo_white, IIDo_peopl, IIDo_race, IIDo)
coef <- c('intercept', 'forumRC', 'forumSf', 'word.frequency', 
          'forumRC:word.frequency', 'forumSf:word.frequency')
IID_coef_tab <- data.frame(coef)
for (m in models){
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
rm(models, coef)
names(IID_coef_tab) <- c('coef', 
                        'IIDc_immig', 'IIDc_nation', 'IIDc_black', 'IIDc_white', 'IIDc_peopl', 'IIDc_race',
                        'IIDc_TOT',
                        'IIDo_immig', 'IIDo_nation', 'IIDo_black', 'IIDo_white', 'IIDo_peopl', 'IIDo_race',
                        'IIDo_TOT')
View(IID_coef_tab)

#### Clean ####
rm(IIDc_immig_inter.median, IIDc_immig_Sf.median, IIDc_immig_RC.median, 
   IIDo_immig_inter.median, IIDo_immig_Sf.median, IIDo_immig_RC.median,
   IIDc_nation_inter.median, IIDc_nation_Sf.median, IIDc_nation_RC.median, 
   IIDo_nation_inter.median, IIDo_nation_Sf.median, IIDo_nation_RC.median,
   IIDc_black_inter.median, IIDc_black_Sf.median, IIDc_black_RC.median, 
   IIDo_black_inter.median, IIDo_black_Sf.median, IIDo_black_RC.median,
   IIDc_white_inter.median, IIDc_white_Sf.median, IIDc_white_RC.median, 
   IIDo_white_inter.median, IIDo_white_Sf.median, IIDo_white_RC.median,
   IIDc_peopl_inter.median, IIDc_peopl_Sf.median, IIDc_peopl_RC.median, 
   IIDo_peopl_inter.median, IIDo_peopl_Sf.median, IIDo_peopl_RC.median,
   IIDc_race_inter.median, IIDc_race_Sf.median, IIDc_race_RC.median, 
   IIDo_race_inter.median, IIDo_race_Sf.median, IIDo_race_RC.median,
   IIDc_inter.median, IIDc_Sf.median, IIDc_RC.median, 
   IIDo_inter.median, IIDo_Sf.median, IIDo_RC.median)
rm(IIDwf_immig_inter.median, IIDwf_immig_Sf.median, IIDwf_immig_RC.median, 
   IIDwf_nation_inter.median, IIDwf_nation_Sf.median, IIDwf_nation_RC.median, 
   IIDwf_black_inter.median, IIDwf_black_Sf.median, IIDwf_black_RC.median, 
   IIDwf_white_inter.median, IIDwf_white_Sf.median, IIDwf_white_RC.median, 
   IIDwf_peopl_inter.median, IIDwf_peopl_Sf.median, IIDwf_peopl_RC.median, 
   IIDwf_race_inter.median, IIDwf_race_Sf.median, IIDwf_race_RC.median, 
   IIDwf_inter.median, IIDwf_Sf.median, IIDwf_RC.median)
rm(IIDc_immig_kruskal.test, IIDo_immig_kruskal.test,
   IIDc_nation_kruskal.test, IIDo_nation_kruskal.test,
   IIDc_black_kruskal.test, IIDo_black_kruskal.test,
   IIDc_white_kruskal.test, IIDo_white_kruskal.test,
   IIDc_peopl_kruskal.test, IIDo_peopl_kruskal.test,
   IIDc_race_kruskal.test, IIDo_race_kruskal.test,
   IIDc_kruskal.test, IIDo_kruskal.test)
rm(IIDc_immig_kruskal.es, IIDo_immig_kruskal.es,
   IIDc_nation_kruskal.es, IIDo_nation_kruskal.es,
   IIDc_black_kruskal.es, IIDo_black_kruskal.es,
   IIDc_white_kruskal.es, IIDo_white_kruskal.es,
   IIDc_peopl_kruskal.es, IIDo_peopl_kruskal.es,
   IIDc_race_kruskal.es, IIDo_race_kruskal.es,
   IIDc_kruskal.es, IIDo_kruskal.es)
rm(IIDc_immig_wt, IIDo_immig_wt,
   IIDc_nation_wt, IIDo_nation_wt,
   IIDc_black_wt, IIDo_black_wt,
   IIDc_white_wt, IIDo_white_wt,
   IIDc_peopl_wt, IIDo_peopl_wt,
   IIDc_race_wt, IIDo_race_wt,
   IIDc_wt, IIDo_wt)
rm(IIDc_immig_coeftest, IIDo_immig_coeftest,
   IIDc_nation_coeftest, IIDo_nation_coeftest,
   IIDc_black_coeftest, IIDo_black_coeftest,
   IIDc_white_coeftest, IIDo_white_coeftest,
   IIDc_peopl_coeftest, IIDo_peopl_coeftest,
   IIDc_race_coeftest, IIDo_race_coeftest,
   IIDc_coeftest, IIDo_coeftest)
rm(IIDc_immig, IIDo_immig,
   IIDc_nation, IIDo_nation,
   IIDc_black, IIDo_black,
   IIDc_white, IIDo_white,
   IIDc_peopl, IIDo_peopl,
   IIDc_race, IIDo_race,
   IIDc, IIDo)
rm(IIDc_immig_model, IIDo_immig_model,
   IIDc_nation_model, IIDo_nation_model,
   IIDc_black_model, IIDo_black_model,
   IIDc_white_model, IIDo_white_model,
   IIDc_peopl_model, IIDo_peopl_model,
   IIDc_race_model, IIDo_race_model,
   IIDc_model, IIDo_model)

## PLOTS #####
### Cosine ####
inter_immig <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == '_inter' & word == 'immigr',
                    link = "logit")
inter_nation <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                     subset = category == '_inter' & word == 'nation',
                     link = "logit")
inter_black <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == '_inter' & word == 'black',
                    link = "logit")
inter_white <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == '_inter' & word == 'white',
                    link = "logit")
inter_peopl <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == '_inter' & word == 'peopl',
                    link = "logit")
inter_race <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                   subset = category == '_inter' & word == 'race',
                   link = "logit")
inter <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
              subset = category == '_inter',
              link = "logit")

Sf_immig <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'immigr',
                    link = "logit")
Sf_nation <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                     subset = category == 'Sf_intra' & word == 'nation',
                     link = "logit")
Sf_black <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'black',
                    link = "logit")
Sf_white <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'white',
                    link = "logit")
Sf_peopl <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'peopl',
                    link = "logit")
Sf_race <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                   subset = category == 'Sf_intra' & word == 'race',
                   link = "logit")
Sf <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'Sf_intra',
              link = "logit")

RC_immig <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'immigr',
                    link = "logit")
RC_nation <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                     subset = category == 'RC_intra' & word == 'nation',
                     link = "logit")
RC_black <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'black',
                    link = "logit")
RC_white <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'white',
                    link = "logit")
RC_peopl <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'peopl',
                    link = "logit")
RC_race <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
                   subset = category == 'RC_intra' & word == 'race',
                   link = "logit")
RC <- betareg(cosine.similarity ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'RC_intra',
              link = "logit")

IIDc_plot <- ggplot(inter_intra_dif, aes(x = word.frequency, y = cosine.similarity)) +
  geom_line(aes(y = predict(inter_immig, inter_intra_dif),
                colour = "immig", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_nation, inter_intra_dif), 
                colour = "nation", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_black, inter_intra_dif),
                colour = "black", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_white, inter_intra_dif), 
                colour = "white", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_peopl, inter_intra_dif),
                colour = "peopl", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_race, inter_intra_dif), 
                colour = "race", linetype= '_inter')) +
  geom_line(aes(y = predict(inter, inter_intra_dif), 
                colour = "tot", linetype= '_inter')) +
  geom_line(aes(y = predict(Sf_immig, inter_intra_dif),
                colour = "immig", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_nation, inter_intra_dif), 
                colour = "nation", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_black, inter_intra_dif),
                colour = "black", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_white, inter_intra_dif), 
                colour = "white", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_peopl, inter_intra_dif),
                colour = "peopl", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_race, inter_intra_dif), 
                colour = "race", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf, inter_intra_dif), 
                colour = "tot", linetype= 'Sf')) +
  geom_line(aes(y = predict(RC_immig, inter_intra_dif),
                colour = "immig", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_nation, inter_intra_dif), 
                colour = "nation", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_black, inter_intra_dif),
                colour = "black", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_white, inter_intra_dif), 
                colour = "white", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_peopl, inter_intra_dif),
                colour = "peopl", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_race, inter_intra_dif), 
                colour = "race", linetype= 'RC')) +
  geom_line(aes(y = predict(RC, inter_intra_dif), 
                colour = "tot", linetype= 'RC')) +
  scale_colour_manual("", values = c("red", 'orange', 'green', "blue", 'purple', 'black', 'pink')) +
  ggtitle('Inter and Intra group variability by word - Semantic Distance') +
  xlab("word frequency") + 
  ylab("semantic distance")+
  theme_bw()
IIDc_plot
### Overlap ####
inter_immig <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                       subset = category == '_inter' & word == 'immigr',
                       link = "logit")
inter_nation <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                        subset = category == '_inter' & word == 'nation',
                        link = "logit")
inter_black <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                       subset = category == '_inter' & word == 'black',
                       link = "logit")
inter_white <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                       subset = category == '_inter' & word == 'white',
                       link = "logit")
inter_peopl <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                       subset = category == '_inter' & word == 'peopl',
                       link = "logit")
inter_race <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                      subset = category == '_inter' & word == 'race',
                      link = "logit")
inter <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                 subset = category == '_inter',
                 link = "logit")

Sf_immig <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'immigr',
                    link = "logit")
Sf_nation <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                     subset = category == 'Sf_intra' & word == 'nation',
                     link = "logit")
Sf_black <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'black',
                    link = "logit")
Sf_white <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'white',
                    link = "logit")
Sf_peopl <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'Sf_intra' & word == 'peopl',
                    link = "logit")
Sf_race <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                   subset = category == 'Sf_intra' & word == 'race',
                   link = "logit")
Sf <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'Sf_intra',
              link = "logit")

RC_immig <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'immigr',
                    link = "logit")
RC_nation <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                     subset = category == 'RC_intra' & word == 'nation',
                     link = "logit")
RC_black <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'black',
                    link = "logit")
RC_white <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'white',
                    link = "logit")
RC_peopl <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                    subset = category == 'RC_intra' & word == 'peopl',
                    link = "logit")
RC_race <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
                   subset = category == 'RC_intra' & word == 'race',
                   link = "logit")
RC <- betareg(neighborhood.overlap ~ word.frequency, data = inter_intra_dif, 
              subset = category == 'RC_intra',
              link = "logit")

IIDo_plot <- ggplot(inter_intra_dif, aes(x = word.frequency, y = neighborhood.overlap)) +
  geom_line(aes(y = predict(inter_immig, inter_intra_dif),
                colour = "immig", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_nation, inter_intra_dif), 
                colour = "nation", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_black, inter_intra_dif),
                colour = "black", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_white, inter_intra_dif), 
                colour = "white", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_peopl, inter_intra_dif),
                colour = "peopl", linetype= '_inter')) +
  geom_line(aes(y = predict(inter_race, inter_intra_dif), 
                colour = "race", linetype= '_inter')) +
  geom_line(aes(y = predict(inter, inter_intra_dif), 
                colour = "tot", linetype= '_inter')) +
  geom_line(aes(y = predict(Sf_immig, inter_intra_dif),
                colour = "immig", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_nation, inter_intra_dif), 
                colour = "nation", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_black, inter_intra_dif),
                colour = "black", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_white, inter_intra_dif), 
                colour = "white", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_peopl, inter_intra_dif),
                colour = "peopl", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf_race, inter_intra_dif), 
                colour = "race", linetype= 'Sf')) +
  geom_line(aes(y = predict(Sf, inter_intra_dif), 
                colour = "tot", linetype= 'Sf')) +
  geom_line(aes(y = predict(RC_immig, inter_intra_dif),
                colour = "immig", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_nation, inter_intra_dif), 
                colour = "nation", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_black, inter_intra_dif),
                colour = "black", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_white, inter_intra_dif), 
                colour = "white", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_peopl, inter_intra_dif),
                colour = "peopl", linetype= 'RC')) +
  geom_line(aes(y = predict(RC_race, inter_intra_dif), 
                colour = "race", linetype= 'RC')) +
  geom_line(aes(y = predict(RC, inter_intra_dif), 
                colour = "tot", linetype= 'RC')) +
  scale_colour_manual("", values = c("red", 'orange', 'green', "blue", 'purple', 'black', 'pink')) +
  ggtitle('Inter and Intra group variability by word - Neighborhood Overlap') +
  xlab("word frequency") + 
  ylab("neighborhood overlap")+
  theme_bw()
IIDo_plot

rm(inter_immig, inter_nation, inter_black, inter_white, inter_peopl, inter_race, inter,
  Sf_immig, Sf_nation, Sf_black, Sf_white, Sf_peopl, Sf_race, Sf,
   RC_immig, RC_nation, RC_black, RC_white, RC_peopl, RC_race, RC)
rm(FCc_plot, FCo_plot, IFc_plot, IFo_plot, IIDc_plot, IIDo_plot)
### .-------------------. #####




















### Plots according to word


