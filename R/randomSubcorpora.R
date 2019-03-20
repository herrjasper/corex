set.seed(20180921)

library(parallel)
library(plyr)
library(psych)
library(RMySQL)
library(ggplot2)
source("fa_definitions.R")

# set sample size PER CORPUS:
samplesize = 70000 #1000 #70000
maxchunksize = 10000 #500 #10000 
replacement = FALSE

# set column names:

colnames <- c("textsigle", "id", "ttrat", "wlen", 
              "slen", "modal", "vv", "vaux", 
              "vfin", "cn", "prep", "inf", "imp", "adv",
              "adj", "subjs", "subji", "conj", "wh",
              "dem", "poss", "neg", "answ", "zuinf",
              "parta", "card", "itj", "nonwrd", "def",
              "indef", "neper", "neloc", "neorg", "emo",
              "dq", "clitindef", "vpast", "vpres",
              "vpressubj", "wpastsubj", "vvpastsubj",
              "pper_1st", "pper_2nd", "pper_3rd", "gen",
              "simpx", "psimpx", "rsimpx", "v2", "vlast",
              "vflen", "esvf", "clausevf", "cmpnd", "unkn",
              "short", "qsvoc", "cnloan", "vvieren",
              "sapos", "pass", "perf", "plu")  

# parallel setup:
no_cores <- detectCores() - 1

connectiondetails_dereko <- c('ukviewer', 'ukviewer', 'dereko', 'gramlinux02')
connectiondetails_decow <- c('decowviewer', 'decowviewer', 'decow', 'gramlinux02')


# get sample from dereko:
cat("Sampling", samplesize, "documens from dereko...\n")
tablename <- "uk_corex_min100"; ndocs <- 16763354; dbname <- 'dereko'
docnums.dereko <- sample.int(ndocs, size = samplesize, replace = replacement)
docchunks.dereko <- chunker(docnums.dereko, maxchunksize = maxchunksize)
df.dereko.neu <- parallel_from_dereko(chunks = docchunks.dereko, selectby = "num")


# get sample from decow:
cat("Sampling", samplesize, "documens from decow16b...\n")
tablename <- "decow16b_meta_min100"; ndocs <- 15119452; dbname <- 'decow'
docnums.decow <- sample.int(ndocs, size = samplesize, replace = replacement)
docchunks.decow <- chunker(docnums.decow, maxchunksize = maxchunksize)
df.decow.neu <- parallel_from_decow(chunks = docchunks.decow, selectby = "num")

# add 'corpus' identifier and rbind data frames:
df.dereko.neu$forum <- 0
df.dereko.neu$corpus <- "dereko"
df.decow.neu$corpus <- "decow"
df.dereko.neu <- df.dereko.neu[,c(2:65, 1)]
df.decow.neu$textsigle <- NA
colnames(df.decow.neu) == colnames(df.dereko.neu)
random.subcorp.neu <- rbind(df.dereko.neu, df.decow.neu)

# export sampled corpora as csv file for further assessment:
write.table(df.dereko.neu, file="/Users/felix/Documents/Konferenzen/2019/dgfs-19-tutorial/randecow/R/random_dereko_70k.csv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(df.decow.neu, file="/Users/felix/Documents/Konferenzen/2019/dgfs-19-tutorial/randecow/R/random_decow_70k.csv", sep = "\t", row.names = FALSE, quote = FALSE)
###############################################################################################

colnames(random.subcorp)

## scale all numerical values to z-scores:
#random.subcorp.scaled <- random.subcorp
random.subcorp[2:62] <- lapply(random.subcorp[,2:62], scale)
head(random.subcorp)


### determine optimal number of factors (scree plots)
# don't use "gen" feature in factor analysis
# (we want to predict the ocurrence of genitives)
parallel<-fa.parallel(random.subcorp[c(2:43,45:62)], fm='minres', fa='fa')
parallel.pa <-fa.parallel(random.subcorp[c(2:43,45:62)], fm='pa', fa='fa')
parallel.pa

### run factor analyses on random.subcorp:

### factoring method: minres
## rotate: oblimin
# 6 factors

fa.n6.minres.oblimin <- fa(random.subcorp[,3:63], nfactors=6, rotate = "oblimin", fm="minres")
print(fa.n6.minres.oblimin, cut = 0.3)
fa.diagram(fa.n6.minres.oblimin, cut = 0.3)

# 12 factors
fa.n12.minres.oblimin <- fa(random.subcorp[,3:63], nfactors=12, rotate = "oblimin", fm="minres")
print(fa.n12.minres.oblimin, cut = 0.3)
fa.diagram(fa.n12.minres.oblimin, cut = 0.3)


## rotate: varimax

# 6 factors
fa.n6.minres.varimax <- fa(random.subcorp[,3:63], nfactors=6, rotate = "varimax", fm="minres")
print(fa.n6.minres.varimax, cut = 0.3)
fa.diagram(fa.n6.minres.varimax, cut = 0.3)

# 12 factors
fa.n12.minres.varimax <- fa(random.subcorp[,3:63], nfactors=12, rotate = "varimax", fm="minres")
print(fa.n12.minres.varimax, cut = 0.3)
fa.diagram(fa.n12.minres.varimax, cut = 0.3)


## rotate: promax
# 6 factors

fa.n6.minres.promax <- fa(random.subcorp[,3:63], nfactors=6, rotate = "promax", fm="minres")
print(fa.n6.minres.promax, cut = 0.3)
fa.diagram(fa.n6.minres.promax, cut = 0.3)

# 12 factors
fa.n12.minres.promax <- fa(random.subcorp[,3:63], nfactors=12, rotate = "promax", fm="minres")
print(fa.n12.minres.promax, cut = 0.3)
fa.diagram(fa.n12.minres.promax, cut = 0.3)


##### factoring method: ml

## rotate: oblimin
# 6 factors

fa.n6.ml.oblimin <- fa(random.subcorp[,3:63], nfactors=6, rotate = "oblimin", fm="ml")
print(fa.n6.ml.oblimin, cut = 0.3)
fa.diagram(fa.n6.ml.oblimin, cut = 0.3)

# 12 factors
fa.n12.ml.oblimin <- fa(random.subcorp[,3:63], nfactors=12, rotate = "oblimin", fm="ml")
print(fa.n12.ml.oblimin, cut = 0.3)
fa.diagram(fa.n12.ml.oblimin, cut = 0.3)


## rotate: varimax
# no warnings/errors here!
# 6 factors
fa.n6.ml.varimax <- fa(random.subcorp[,3:63], nfactors=6, rotate = "varimax", fm="ml")
print(fa.n6.ml.varimax, cut = 0.3)
fa.diagram(fa.n6.ml.varimax, cut = 0.3)

# 12 factors
# no warnings/errors here!
fa.n12.ml.varimax <- fa(random.subcorp[,3:63], nfactors=12, rotate = "varimax", fm="ml")
print(fa.n12.ml.varimax, cut = 0.3)
fa.diagram(fa.n12.ml.varimax, cut = 0.3)

# 18 factors
fa.n18.ml.varimax <- fa(random.subcorp[,3:63], nfactors=18, rotate = "varimax", fm="ml")
print(fa.n18.ml.varimax, cut = 0.3)
fa.diagram(fa.n18.ml.varimax, cut = 0.3)

## rotate: promax
# 6 factors

fa.n6.ml.promax <- fa(random.subcorp[,3:63], nfactors=6, rotate = "promax", fm="ml")
print(fa.n6.ml.promax, cut = 0.3)
fa.diagram(fa.n6.ml.promax, cut = 0.3)

# 12 factors
fa.n12.ml.promax <- fa(random.subcorp[,3:63], nfactors=12, rotate = "promax", fm="ml")
print(fa.n12.ml.promax, cut = 0.3)
fa.diagram(fa.n12.ml.promax, cut = 0.3)



### factoring method: pa

## rotate: oblimin
# 6 factors

fa.n6.pa.oblimin <- fa(random.subcorp[,3:63], nfactors=6, rotate = "oblimin", fm="pa")
print(fa.n6.pa.oblimin, cut = 0.3)
fa.diagram(fa.n6.pa.oblimin, cut = 0.3)

# 12 factors
fa.n12.pa.oblimin <- fa(random.subcorp[,3:63], nfactors=12, rotate = "oblimin", fm="pa")
print(fa.n12.pa.oblimin, cut = 0.3)
fa.diagram(fa.n12.pa.oblimin, cut = 0.3)


## rotate: varimax

# 6 factors
fa.n6.pa.varimax <- fa(random.subcorp[,3:63], nfactors=6, rotate = "varimax", fm="pa")
print(fa.n6.pa.varimax, cut = 0.3)
fa.diagram(fa.n6.pa.varimax, cut = 0.3)

# 12 factors
fa.n12.pa.varimax <- fa(random.subcorp[,3:63], nfactors=12, rotate = "varimax", fm="pa")
print(fa.n12.pa.varimax, cut = 0.3)
fa.diagram(fa.n12.pa.varimax, cut = 0.3)


## rotate: promax ('pa' & 'promax' is the combination used & recommended in Biber 1988)
# 6 factors

fa.n6.pa.promax <- fa(random.subcorp[,3:63], nfactors=6, rotate = "promax", fm="pa")
print(fa.n6.pa.promax, cut = 0.3)
fa.diagram(fa.n6.pa.promax, cut = 0.3)



# 5 factors
fa.n5.pa.promax <- fa(random.subcorp[,c(2:43,45:62)], nfactors=5, rotate = "promax", fm="pa")
print(fa.n5.pa.promax, cut = 0.3)
colnames(fa.n5.pa.promax$loadings) <- c("Factor_3", "Factor_1", "Factor_5", "Factor_2", "Factor_4")
fa.diagram(fa.n5.pa.promax, cut = 0.3)


# 8 factors
fa.n8.pa.promax <- fa(random.subcorp[,c(2:43,45:62)], nfactors=8, rotate = "promax", fm="pa")
print(fa.n8.pa.promax, cut = 0.3)
fa.diagram(fa.n8.pa.promax, cut = 0.3)

# 12 factors
fa.n12.pa.promax <- fa(random.subcorp[,c(2:43,45:62)], nfactors=12, rotate = "promax", fm="pa")
print(fa.n12.pa.promax, cut = 0.3)
fa.diagram(fa.n12.pa.promax, cut = 0.3)


# 7 factors
# no errors/warnings
# don't use "gen" feature in factor analysis
# (we want to predict the ocurrence of genitives)
colnames(random.subcorp)[c(2:43,45:62)]
fa.n7.pa.promax <- fa(random.subcorp[,c(2:43,45:62)], nfactors=7, rotate = "promax", fm="pa")
print(fa.n7.pa.promax, cut = 0.3)
# from https://stackoverflow.com/questions/50494693/change-factor-labels-in-psychfa-or-psychfa-diagram :
colnames(fa.n7.pa.promax.renamed$loadings) <- c("Factor 2", "Factor 1", "Factor 5", "Factor 4", "Factor 7", "Factor 6", "Factor 3")
# pad features with whitespace:
rn <- rownames(fa.n7.pa.promax.renamed$loadings)
rn <- gsub("$"," ", gsub("^", " ",rn))
rownames(fa.n7.pa.promax.renamed$loadings) <- rn
fa.diagram(fa.n7.pa.promax.renamed, cut = 0.3)



##############################

# Biber-style construction of factor scores (p.93-97)
# 1) discard loadings whose absolute value is smaller than 0.35
# 2) use any given feature only on one factor (the one with the greatest loading for that feature)
# 3) for each document: for each factor, add up standardized counts (ie., z-scores) for those features
#    that are salient on that factor (salient means its abs(loading) >= .35 and
#    there is no other factor where this feature has a greater abs(loading)).
#
#    That is, the actual magnitude of the abs(loading) above .35 is irrelevant for the calculation;
#    the only condition is that the feature must not be more salient on another factor.
#

# Filter the factor loadings: set all values to 0 if 
# - either its absolute value is less than a given threshold (eg., 0.35)
# - or there is a greater value for that feature on another factor.

filtered.fa.loadings <- filter_loadings(fa.n7.pa.promax, threshold = 0.35)
# after fixing the filter_loadings function:
filtered.fa.loadings.new <- filter_loadings_new(fa.n7.pa.promax, threshold = 0.35)

# calculate the actual document scores according to the filtered factor loadings:
# make a matrix with a column for the doc-ID and one column for each factor from the fa:

document.scores <- docscores_biber(random.subcorp, c(2:43,45:62), filtered.fa.loadings)
# after fixing the filter_loadings function:
document.scores.new <- docscores_biber(random.subcorp, c(2:43,45:62), filtered.fa.loadings.new)
colnames(document.scores.new) <- c("id", "PA2.new", "PA1.new", "PA5.new", "PA4.new", "PA7.new", "PA6.new", "PA3.new")


# merge document scores with original corpus df:
random.subcorp.with.scores <- merge(x = random.subcorp, y = document.scores, by = "id", all.x = TRUE)
# merge corrected document scores:
random.subcorp.with.scores.forum.meta <- merge(x = random.subcorp.with.scores.forum.meta, y = document.scores.new, by = "id", all.x = TRUE)


hist(random.subcorp.with.scores$PA2,breaks=100)
hist(random.subcorp.with.scores$PA5,breaks=100)
hist(random.subcorp.with.scores$PA3,breaks=100)
hist(random.subcorp.with.scores$PA1,breaks=100)

mean(subset(random.subcorp.with.scores, corpus=="dereko")$PA1)
mean(subset(random.subcorp.with.scores, corpus=="decow")$PA1)

mean(random.subcorp.with.scores[sample(nrow(random.subcorp.with.scores),10000),]$PA1)
head(random.subcorp.with.scores[,63:70])
head(fa.n7.pa.promax$scores)

# for decow documents, get forum info from database:
random.subcorp.decow.id <- subset(random.subcorp.with.scores, corpus=="decow")$id
write.csv(random.subcorp.decow.id, file="random_subcorp_decow_id.csv")
# fetch info from db, then re-import data frame:
random.subcorp.decow.id.forum <- read.csv("random_subcorp_decow_id_forum.csv", header = FALSE, sep="\t")
colnames(random.subcorp.decow.id.forum) <- c("id", "forum")
# merge with existing data frame:
random.subcorp.with.scores.forum <- merge(x = random.subcorp.with.scores, y = random.subcorp.decow.id.forum, by = "id", all.x = TRUE)
# set forum value to "0" for all dereko documents:
random.subcorp.with.scores.forum[which(random.subcorp.with.scores.forum$corpus=="dereko"),]$forum <- "0"
# check result:
xtabs(~forum+corpus, data=random.subcorp.with.scores.forum)



# for dereko documents, get forum info from database:
random.subcorp.dereko.id <- subset(random.subcorp.with.scores, corpus=="dereko")$id
write.csv(random.subcorp.dereko.id, file="random_subcorp_dereko_id.csv")
# fetch info from db, re-import data frame:
random.subcorp.dereko.meta <- read.csv("random_subcorp_dereko_meta.joined", header = TRUE, sep="\t") 
# merge with existing data frame:
random.subcorp.with.scores.forum.meta <- merge(x = random.subcorp.with.scores.forum, y = random.subcorp.dereko.meta, by = "id", all.x = TRUE)

random.subcorp.with.scores.forum.meta$register <- as.factor(random.subcorp.with.scores.forum.meta$register)
random.subcorp.with.scores.forum.meta$region <- as.factor(random.subcorp.with.scores.forum.meta$region)
random.subcorp.with.scores.forum.meta$country <- as.factor(random.subcorp.with.scores.forum.meta$country)
random.subcorp.with.scores.forum.meta$medium <- as.factor(random.subcorp.with.scores.forum.meta$medium)
random.subcorp.with.scores.forum.meta$domain <- as.factor(random.subcorp.with.scores.forum.meta$domain)

# plot distribution of document scores for some factors:

plot(density(subset(random.subcorp.with.scores.forum, forum=="0")$PA1), col="darkorange", lwd=2)
points(density(subset(random.subcorp.with.scores.forum, forum=="1")$PA1), col="darkgreen",lwd=2,type="l")

# only decow-data:
ggplot(subset(random.subcorp.with.scores.forum, corpus=="decow"), aes(PA1, fill = forum)) + geom_density(alpha = 0.4)

cbPalette <- c("#E69F00", "#009E73")
#cbPalette <- c("#111111", "#999999")
# all data:

ggplot(random.subcorp.with.scores.forum, aes(PA1, fill = corpus)) +  scale_fill_manual(values=cbPalette) + geom_density(alpha = 0.6) + labs(x = "Factor score on Factor 1") + labs(title = "Factor 1: Distribution of scores by corpus") #+ labs(caption = "(Based on FA of 140k random docs from DeReKo and DECOW16B)") 
ggplot(random.subcorp.with.scores.forum, aes(PA1, fill = forum)) +  scale_fill_manual(values=cbPalette) + geom_density(alpha = 0.6) + labs(x = "Factor score on Factor 1") + labs(title = "Factor 1: Distribution of scores by document type") #+ labs(caption = "(Based on FA of 140k random docs from DeReKo and DECOW16B)") 
ggplot(random.subcorp.with.scores.forum, aes(PA1, fill = forum)) + scale_fill_manual(values=cbPalette) + geom_histogram(alpha = 0.7, aes(y = ..density..), bins = 500, position = 'identity') + labs(x = "Doc. score on Factor 2 (short, clitindef, itj, emo, qsvoc, pper2nd, ...)") + labs(caption = "(Based on FA of 140k random docs from DeReKo and DECOW16B)") + labs(title = "Factor 2: Distribution of scores by document type")

ggplot(random.subcorp.with.scores.forum, aes(PA3, fill = corpus)) + geom_density(alpha = 0.6) + labs(x = "Doc. score on Factor 3 (vpast, plu)") + labs(title = "Factor 3: Distribution of scores by corpus")

ggplot(random.subcorp.with.scores.forum, aes(PA2, fill = corpus)) + geom_density(alpha = 0.6) + labs(x = "Doc. score on Factor 2 (...)") + labs(title = "Factor 2: Distribution of scores by corpus")

ggplot(random.subcorp.with.scores.forum, aes(PA5, fill = forum)) + geom_density(alpha = 0.6) + labs(x = "Doc. score on Factor 5 (...)") + labs(title = "Factor 5: Distribution of scores by corpus")

ggplot(random.subcorp.with.scores.forum, aes(PA4, fill = corpus)) + geom_density(alpha = 0.6) + labs(x = "Doc. score on Factor 4 (...)") + labs(title = "Factor 4: Distribution of scores by corpus")

ggplot(random.subcorp.with.scores.forum, aes(PA6, fill = corpus)) + geom_density(alpha = 0.6) + labs(x = "Doc. score on Factor 6 (...)") + labs(title = "Factor 6: Distribution of scores by corpus")

ggplot(random.subcorp.with.scores.forum, aes(PA7, fill = corpus)) + geom_density(alpha = 0.6) + labs(x = "Doc. score on Factor 7 (...)") + labs(title = "Factor 7: Distribution of scores by corpus")

#####

# plot distribution of factor scores for other meta data categories (dereko data only):
random.subcorp.with.scores.forum.meta.dereko <- subset(random.subcorp.with.scores.forum.meta, corpus=="dereko")[,c(1,seq(63,77))]
random.subcorp.with.scores.forum.meta.dereko[] <- lapply(random.subcorp.with.scores.forum.meta.dereko, function(x) if(is.factor(x)) factor(x) else x)

random.subcorp.with.scores.forum.meta.dereko[which(random.subcorp.with.scores.forum.meta.dereko$country==2),]$country <- 1
random.subcorp.with.scores.forum.meta.dereko[which(random.subcorp.with.scores.forum.meta.dereko$country==3),]$country <- 1
random.subcorp.with.scores.forum.meta.dereko$country <- factor(random.subcorp.with.scores.forum.meta.dereko$country)

random.subcorp.with.scores.forum.meta.dereko$oral <- random.subcorp.with.scores.forum.meta.dereko$medium==4


plot(density(random.subcorp.with.scores.forum.meta.dereko$PA3), col="darkorange", lwd=2)
points(density(subset(random.subcorp.with.scores.forum.meta, register=="2")$PA3), col="darkgreen",lwd=2,type="l")
points(density(subset(random.subcorp.with.scores.forum.meta, register=="3")$PA3), col="darkred",lwd=2,type="l")

ggplot(random.subcorp.with.scores.forum.meta.dereko, aes(PA2, fill = oral))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 2") + labs(title = "Factor 2: Distribution of scores by document type")
ggplot(random.subcorp.with.scores.forum.meta.dereko, aes(PA5, fill = oral))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 5") + labs(title = "Factor 5: Distribution of scores by document type")
ggplot(random.subcorp.with.scores.forum.meta.dereko, aes(PA4, fill = oral))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 4") + labs(title = "Factor 4: Distribution of scores by document type")
ggplot(random.subcorp.with.scores.forum.meta.dereko, aes(PA7, fill = oral))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 7") + labs(title = "Factor 7: Distribution of scores by document type")
ggplot(random.subcorp.with.scores.forum.meta.dereko, aes(PA6, fill = oral))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 6") + labs(title = "Factor 6: Distribution of scores by document type")
ggplot(random.subcorp.with.scores.forum.meta.dereko, aes(PA3, fill = oral))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 3") + labs(title = "Factor 3: Distribution of scores by document type")


ggplot(random.subcorp.with.scores.forum.meta, aes(PA1.new, fill = forum))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 1") + labs(title = "Factor 1: Distribution of scores by forum")
ggplot(random.subcorp.with.scores.forum.meta, aes(PA2.new, fill = forum))  + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 2") + labs(title = "Factor 2: Distribution of scores by forum")
ggplot(random.subcorp.with.scores.forum.meta, aes(PA5.new, fill = medium)) + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 5") + labs(title = "Factor 5: Distribution of scores by corpus")
ggplot(random.subcorp.with.scores.forum.meta, aes(PA4.new, fill = corpus)) + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 4") + labs(title = "Factor 4: Distribution of scores by corpus")
ggplot(random.subcorp.with.scores.forum.meta, aes(PA7.new, fill = corpus)) + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 7") + labs(title = "Factor 7: Distribution of scores by corpus")
ggplot(random.subcorp.with.scores.forum.meta, aes(PA6.new, fill = corpus)) + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 6") + labs(title = "Factor 6: Distribution of scores by corpus")
ggplot(random.subcorp.with.scores.forum.meta, aes(PA3.new, fill = corpus)) + geom_density(alpha = 0.3) + labs(x = "Factor score on Factor 3") + labs(title = "Factor 3: Distribution of scores by corpus")



str(subset(random.subcorp.with.scores.forum.meta, corpus=="dereko")$register)
xtabs(~country, data = subset(random.subcorp.with.scores.forum.meta, corpus=="dereko"))


## inspect some typical documents with high/low scores on factor1:
summary(random.subcorp.with.scores.forum$PA1)

head(subset((subset(random.subcorp.with.scores.forum, PA1 > 30)), corpus=="decow")[,c(1,65,71)])
subset(random.subcorp.with.scores.forum, id=="02d2d403f2c7b848a910cf1fdcde59c7f07e")[,c("id", "PA1", "forum", "short", "clitindef", "itj", "emo", "unkn", "imp", "qsvoc", "nonwrd","pper_2nd", "adv", "pper_1st")]

# Factor 5:
mean(subset(random.subcorp.with.scores.forum.meta, forum==0)$PA5)
head(subset(subset(random.subcorp.with.scores.forum.meta, PA5 < -10), corpus=="dereko")[,c("textsigle", "PA5", "wlen", "def", "pass", "cn", "cmpnd", "prep", "poss")],10)

# Factor 3:
# get thresholds for most extreme 10% of values:
quantile(random.subcorp.with.scores.forum.meta$PA3, probs = c(.1,.9))
nrow(subset(random.subcorp.with.scores.forum.meta, PA3 > 2.318))
nrow(subset(random.subcorp.with.scores.forum.meta, PA3 <= -1.558))
# get random selection of extreme documents:
subset(subset(random.subcorp.with.scores.forum.meta, PA3 > 2.318), corpus=="dereko")[sample(nrow(subset(subset(random.subcorp.with.scores.forum.meta, PA3 > 2.318), corpus=="dereko")),10),c("textsigle", "PA3", "vpast", "vpres", "plu")]


