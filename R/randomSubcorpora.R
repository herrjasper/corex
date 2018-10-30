set.seed(20180921)

library(parallel)
library(plyr)
library(psych)
library(RMySQL)
source("fa_definitions.R")

# set sample size PER CORPUS:
samplesize = 70000
maxchunksize = 10000 
replacement = FALSE

# set column names:

colnames <- c("id", "ttrat", "wlen", 
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

connectiondetails_dereko <- c('ukviewer', '', 'dereko', 'gramlinux02')
connectiondetails_decow <- c('ukviewer', '', 'decow', 'gramlinux02')


# get sample from dereko:
cat("Sampling", samplesize, "documens from dereko...\n")
tablename <- "uk_corex_min100"; ndocs <- 16763354; dbname <- 'dereko'
docnums.dereko <- sample.int(ndocs, size = samplesize, replace = replacement)
docchunks.dereko <- chunker(docnums.dereko, maxchunksize = maxchunksize)
df.dereko <- parallel_from_dereko(chunks = docchunks.dereko, selectby = "num")


# get sample from decow:
cat("Sampling", samplesize, "documens from decow16b...\n")
tablename <- "decow16b_meta_min100"; ndocs <- 15119452; dbname <- 'decow'
docnums.decow <- sample.int(ndocs, size = samplesize, replace = replacement)
docchunks.decow <- chunker(docnums.decow, maxchunksize = maxchunksize)
df.decow <- parallel_from_decow(chunks = docchunks.decow, selectby = "num")

# add 'corpus' identifier and rbind data frames:
df.dereko$corpus <- rep("dereko", nrow(df.dereko))
df.decow$corpus <- rep("decow", nrow(df.decow))
random.subcorp <- rbind(df.dereko, df.decow)

###############################################################################################

colnames(random.subcorp)

## scale all numerical values to z-scores:
#random.subcorp.scaled <- random.subcorp
random.subcorp[2:62] <- lapply(random.subcorp[,2:62], scale)
head(random.subcorp)


### determine optimal number of factors (scree plots)

parallel<-fa.parallel(random.subcorp[,2:62], fm='minres', fa='fa')
parallel.pa <-fa.parallel(random.subcorp[,3:63], fm='pa', fa='fa')
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

# 7 factors
# no errors/warnings
# don't use "gen" feature in factor analysis
# (we want to predict the ocurrence of genitives)
colnames(random.subcorp)[c(2:43,45:62)]
fa.n7.pa.promax <- fa(random.subcorp[,c(2:43,45:62)], nfactors=7, rotate = "promax", fm="pa")
print(fa.n7.pa.promax, cut = 0.3)
fa.diagram(fa.n7.pa.promax, cut = 0.3)



# 8 factors
fa.n8.pa.promax <- fa(random.subcorp[,3:63], nfactors=8, rotate = "promax", fm="pa")
print(fa.n8.pa.promax, cut = 0.3)
fa.diagram(fa.n8.pa.promax, cut = 0.3)

# 12 factors
fa.n12.pa.promax <- fa(random.subcorp[,3:63], nfactors=12, rotate = "promax", fm="pa")
print(fa.n12.pa.promax, cut = 0.3)
fa.diagram(fa.n12.pa.promax, cut = 0.3)

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

# calculate the actual document scores according to the filtered factor loadings:
# make a matrix with a column for the doc-ID and one column for each factor from the fa:

document.scores <- docscores_biber(random.subcorp, 2:62, filtered.fa.loadings)

# merge document scores with original corpus df:
random.subcorp.with.scores <- merge(x = random.subcorp, y = document.scores, by = "id", all.x = TRUE)

hist(random.subcorp.with.scores$PA2,breaks=100)
hist(random.subcorp.with.scores$PA5,breaks=100)

mean(subset(random.subcorp.with.scores, corpus=="dereko")$PA7)
mean(subset(random.subcorp.with.scores, corpus=="decow")$PA7)

mean(random.subcorp.with.scores[sample(nrow(random.subcorp.with.scores),10000),]$PA3)
head(random.subcorp.with.scores[,66:72])
head(fa.n7.pa.promax$scores)
