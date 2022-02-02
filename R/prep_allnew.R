library("fmsb")
library("lme4")
library("RColorBrewer")

prep.allnew <- read.csv2("/Users/felix/Documents/Papers/corex/casestudies/prepositions/concordance.combined1.100000.csv", header=FALSE, sep="\t", col.names = c("corpus", "id", "sidx", "case", "forum", "match", "target.prep", "next.pos", "next.lemma"))
prep.allnew$id <- as.character(prep.allnew$id)
head(prep.allnew)
table(prep.allnew$corpus,prep.allnew$next.pos)
str(prep.allnew)

# keep only a single concordance line from each document:
prep.allnew  <- prep.allnew[!duplicated(prep.allnew$id),]

# get doc-IDs from dereko documents; format and chunk:
id.dereko <- format_mysql_listquery(subset(prep.allnew, corpus=="dereko")$id)
id.dereko.chunks <- chunker(vec = id.dereko, maxchunksize = 15000)
# get doc-IDs from decow documents; format and chunk:
id.decow <- format_mysql_listquery(subset(prep.allnew, corpus=="decow")$id)
id.decow.chunks <- chunker(vec = id.decow, maxchunksize = 15000)

# retrieve corex data:
dereko.md <- parallel_from_dereko(chunks = id.dereko.chunks, selectby = "id")
decow.md <- parallel_from_decow(chunks = id.decow.chunks, selectby = "id")
combined.md <- rbind.data.frame(dereko.md,decow.md)


# merge concordance df with corex df:
prep.allnew.md <- merge(y = combined.md, x = prep.allnew, by = "id", all.y = TRUE)

# sample n=40,000 concordance lines from each corpus:
prep.allnew.md.sample <- rbind.data.frame(prep.allnew.md[sample(which(prep.allnew.md$corpus=="dereko"), 40000),], prep.allnew.md[sample(which(prep.allnew.md$corpus=="decow"), 40000),])

# inspect distribution of prepositions by corpus (all prepositions per corpus adding up to 1):
barplot(xtabs(~corpus+target.prep, data=prep.allnew.md.sample)/40000, beside=TRUE,col = c("green", "darkorange"), las=2, ylim=c(0,0.4))

# same, but with raw counts, not proportions:
barplot(xtabs(~corpus+target.prep, data=prep.allnew.md.sample), beside=TRUE,col = c("green", "darkorange"), las=2)

##############################################################
# inspect proportion of genitive, per preposition per corpus:

prep.allnew.md.sample.genprops <- xtabs(~corpus+target.prep, data=subset(prep.allnew.md.sample, case=="gen"))/xtabs(~corpus+target.prep, data=prep.allnew.md.sample)
prep.allnew.md.sample.genprops.ordered <- prep.allnew.md.sample.genprops[,order(prep.allnew.md.sample.genprops[1,])]
# same for full data set:
prep.allnew.md.genprops <- xtabs(~corpus+target.prep, data=subset(prep.allnew.md, case=="gen"))/xtabs(~corpus+target.prep, data=prep.allnew.md)
prep.allnew.md.genprops.ordered <- prep.allnew.md.genprops[,order(prep.allnew.md.genprops[1,])]
###############################################################

###############################################################
# plot proportion of genitives:

opar <- (mar=c(5.1,4.1,4.1,2.1))
par(mar=c(7.3,4.1,4.1,2.1))
#barplot(prep.allnew.md.sample.genprops.ordered,  beside=TRUE, las=2, angle=c(45,135), density=c(90,40), cex.names = 1.2, col="black")
barplot(prep.allnew.md.sample.genprops.ordered,  beside=TRUE, las=2, cex.names = 1.2, col=c("black", "lightgray"))
par(xpd=TRUE)
#legend(0, 1, legend=rownames(prep.allnew.md.sample.genprops), cex = 1, fill=TRUE, angle=c(45,135), density=c(90,20), bty = "n")
legend(0, 1, legend=rownames(prep.allnew.md.sample.genprops), cex = 1, fill=c("black", "lightgray"),  bty = "n")

# same for full data set:
barplot(prep.allnew.md.genprops.ordered,  beside=TRUE, las=2, cex.names = 1.2, col=c("black", "lightgray"))
legend(0, 1, legend=rownames(prep.allnew.md.genprops), cex = 1, fill=c("black", "lightgray"),  bty = "n")
################################################################


# get prepositions for which the proportion of genitves is between .1 and .9 
# in at least one of the corpora (i.e., prepositions that show a fair amount of variation at all):
varying.preps <- levels(factor(subset(subset(as.data.frame(prep.allnew.md.sample.genprops), Freq <= .9), Freq >= .1)$target.prep))


# for each concordance line, add document scores from a particular factor analysis (randomSubcorpora.R);
# (do NOT use "gen" feature for predicting occurrence of genitives; we get Nagelkerke R2 = 0.31 wth the gen feature alone...):
# glm.corex.gen <- glm(case ~ gen, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="wegen"), family = binomial)
# summary(glm.corex.gen)
# NagelkerkeR2(glm.corex.gen)

docscores <- docscores_biber(prep.allnew.md.sample, c(10:51,53:70), filtered.fa.loadings.new)
# try with scaled values, for comparison:
prep.allnew.md.sample.scaled <- prep.allnew.md.sample
prep.allnew.md.sample.scaled[, c(10:70)] <-  lapply(prep.allnew.md.sample.scaled[, c(10:70)], scale)
docscores.scaled <- docscores_biber(prep.allnew.md.sample.scaled, c(10:51,53:70), filtered.fa.loadings.new)

# merge document scores with original corpus df:
prep.allnew.md.sample.docscores.new <- merge(x = prep.allnew.md.sample, y = docscores, by = "id", all.x = TRUE)
# do the same with the scaled data:
prep.allnew.md.sample.scaled.docscores.new <- merge(x = prep.allnew.md.sample.scaled, y = docscores.scaled, by = "id", all.x = TRUE)

#################

# get a subset of the concordance, including only the prepositions in varying.preps
# (i.e. "dank", "einschließlich", "entgegen", "gemäß", "mangels", "mitsamt", "mittels", "trotz", "wegen", "zuzüglich")

prep.allnew.md.sample.scaled.docscores.prepselection <- subset(prep.allnew.md.sample.scaled.docscores, target.prep %in% varying.preps)
prep.allnew.md.sample.scaled.docscores.prepselection$target.prep <- factor(prep.allnew.md.sample.scaled.docscores.prepselection$target.prep)
colSums(xtabs(~corpus+target.prep, data=prep.allnew.md.sample.scaled.docscores.prepselection))
barplot(xtabs(~corpus+target.prep, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, case=="gen"))/xtabs(~corpus+target.prep, data=prep.allnew.md.sample.scaled.docscores.prepselection),  beside = TRUE, las=2, col = c("green", "darkorange"))

# summary of proportions of genitive and sample size by preposition and corpus: 
prep.allnew.md.sample.scaled.docscores.prepselection.stats <- cbind(t(xtabs(~corpus+target.prep, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, case=="gen"))/xtabs(~corpus+target.prep, data=prep.allnew.md.sample.scaled.docscores.prepselection)),t(xtabs(~corpus+target.prep, data=prep.allnew.md.sample.scaled.docscores.prepselection)), colSums(xtabs(~corpus+target.prep, data=prep.allnew.md.sample.scaled.docscores.prepselection)))
colnames(prep.allnew.md.sample.scaled.docscores.prepselection.stats) <- c("gen.decow", "gen.dereko", "n.decow", "n.dereko", "n")
prep.allnew.md.sample.scaled.docscores.prepselection.stats


#################
#################

# Modeling with GLMs:
# get names of independent variables:
corex.predictors <- colnames(prep.allnew.md.sample.scaled.docscores.prepselection)[c(10:51,53:70)]
fa.predictors <- colnames(prep.allnew.md.sample.scaled.docscores.prepselection)[71:77]

f.corex <- paste("case", " ~ ", paste(corex.predictors, collapse = " + "))
f.fa <- paste("case", " ~ ", paste(fa.predictors, collapse = " + "))

# produce results.combined, results.dereko, results.decow ...
results.combined <- do_glms(prep.allnew.md.sample.scaled.docscores.prepselection)
results.dereko <- do_glms(prep.allnew.md.sample.scaled.docscores.prepselection, subcorp = "dereko")
results.decow <- do_glms(prep.allnew.md.sample.scaled.docscores.prepselection, subcorp = "decow")

results.combined

# glm for all prepositions:

glm.corex.allprep <- glm(f.corex, data=prep.allnew.md.sample.scaled.docscores.prepselection, family = "binomial")
glm.fa.allprep <- glm(f.fa, data=prep.allnew.md.sample.scaled.docscores.prepselection, family = "binomial")

NagelkerkeR2(glm.corex.allprep)
length(coef(summary(glm.corex.allprep))[,4])
length(which(coef(summary(glm.corex.allprep))[,4] < 0.05))
NagelkerkeR2(glm.fa.allprep)


# glms for all prepositions, response coded as standard/nonstandard:

prep.allnew.md.sample.scaled.docscores.prepselection$nscase <- rep(0,length(prep.allnew.md.sample.scaled.docscores.prepselection$target.prep))

# classify  "entgegen", "gemäß", "mitsamt" with genitive as non-standard case:
prep.allnew.md.sample.scaled.docscores.prepselection$nscase[which(prep.allnew.md.sample.scaled.docscores.prepselection$target.prep %in% c("entgegen", "gemäß", "mitsamt") & prep.allnew.md.sample.scaled.docscores.prepselection$case == "gen")] <- 1
# classify "dank", "einschließlich", "mangels", "mittels", "trotz", "wegen", "zuzüglich" with dative as non-standard case:
prep.allnew.md.sample.scaled.docscores.prepselection$nscase[which(prep.allnew.md.sample.scaled.docscores.prepselection$target.prep %in% c("dank", "einschließlich", "mangels", "mittels", "trotz", "wegen", "zuzüglich") & prep.allnew.md.sample.scaled.docscores.prepselection$case == "dat")] <- 1
prep.allnew.md.sample.scaled.docscores.prepselection$nscase <- as.factor(prep.allnew.md.sample.scaled.docscores.prepselection$nscase)

xtabs(~nscase+case, data=prep.allnew.md.sample.scaled.docscores.prepselection)
# How many instances of non-standard case in the data set?
rowSums(xtabs(~nscase+case, data=prep.allnew.md.sample.scaled.docscores.prepselection))[2]
# ==> 6122 
rowSums(xtabs(~nscase+case, data=prep.allnew.md.sample.scaled.docscores.prepselection))[2]/sum(xtabs(~nscase+case, data=prep.allnew.md.sample.scaled.docscores.prepselection))
# ==> 14.8%

#adapt formula:
f.corex <- paste("nscase", " ~ ", paste(corex.predictors, collapse = " + "))
f.fa <- paste("nscase", " ~ ", paste(fa.predictors, collapse = " + "))

glm.corex.allprep.ns <- glm(f.corex.ns, data=prep.allnew.md.sample.scaled.docscores.prepselection, family = "binomial")
glm.fa.allprep.ns <- glm(f.fa.ns, data=prep.allnew.md.sample.scaled.docscores.prepselection, family = "binomial")

NagelkerkeR2(glm.corex.allprep.ns)
length(which(coef(summary(glm.corex.allprep.ns))[,4] < 0.05))
# 0.28
NagelkerkeR2(glm.fa.allprep.ns)
length(which(coef(summary(glm.fa.allprep.ns))[,4] < 0.05))
summary(glm.fa.allprep.ns)
# 0.24
#
# but most of this rather high R2 is because "wegen" ist very strongly represented in the data set, and it seems to be particularly well predictable 
# from the selected features (i.e., seems to be strongly related to standard vs. non-standard), 
# the only dimension that really sticks out from the FA.
# If "wegen" is discarded from the data set, Nagelkerke R2 drops to 0.08 (COReX) and 0.05 (FA).
#
########################

# inspect models for individual prepositions:

glm.corex.wegen.ns <- glm(f.corex.ns, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="wegen"), family = "binomial")
NagelkerkeR2(glm.corex.wegen.ns)

glm.fa.allprep.ns <- glm(f.fa.ns, data=prep.allnew.md.sample.scaled.docscores.prepselection, family = "binomial")
glm.fa.wegen.ns <- glm(f.fa.ns, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="wegen"), family = "binomial")

NagelkerkeR2(glm.corex.wegen.ns)
NagelkerkeR2(glm.fa.wegen.ns)
NagelkerkeR2(glm.fa.allprep.ns)

########################
# check other predictors

# forum: 

do_glms_open(df = prep.allnew.md.sample.scaled.docscores.prepselection, formula = "nscase ~ forum")


glm.corex.gen <- glm(case ~ gen, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="wegen"), family = binomial)
# summary(glm.corex.gen)
# NagelkerkeR2(glm.corex.gen)

glm.forum.allprep <- glm(case ~ forum, data=prep.allnew.md.sample.scaled.docscores.prepselection, family = binomial)
NagelkerkeR2(glm.forum.allprep)

# Try POS tag of next 
head(prep.allnew.md.sample.scaled.docscores.prepselection)
do_glms_open(prep.allnew.md.sample.scaled.docscores.prepselection, "case ~ next.pos")


# works well for "trotz" (with and adjective following, we get more dative; cf. Duden)
m <- (glm(paste("case", " ~ ", paste(corex.predictors, collapse = " + ")), data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="mangels"), family=binomial))
m <- (glm(paste("case", " ~ ", paste(fa.predictors, collapse = " + ")), data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="mangels"), family=binomial))
#m <- glm(case~ slen + vlast + subjs + emo + neorg + indef + zuinf + neg + conj, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="mangels"), family=binomial)
# zuzüglich does not work. why?

xtabs(~case+corpus, data=subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="zuzüglich"))

table(head(fitted(m), 10000),head(subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="wegen")$next.pos, 10000))
summary(m)
NagelkerkeR2(m)

rownames(coef(summary(m)))

#[,4]
cov.unscaled(m)
table(factor(subset(prep.allnew.md.sample.scaled.docscores.prepselection, target.prep=="trotz")$next.lemma))

######################
# try a mixed model:

#glmm1 <- glmer(paste("case", " ~ (1|target.prep)  + ", paste(corex.predictors, collapse = " + ")), data=prep.allnew.md.sample.scaled.docscores.prepselection, family=binomial)

######################

# plot coefficient estimates from individual models:

# get a matrix of all coefficient estimates with associated p-value of < .05, and between 5 and -5: 
coeffs <- do_glms_coeffs(prep.allnew.md.sample.scaled.docscores.prepselection, paste("nscase", " ~ ", paste(corex.predictors, collapse = " + ")))
# remove the intercept:
coeffs<-coeffs[2:nrow(coeffs),]

#rownames(coeffs)

# get maximum and minimum coefficient 
x.lower <- min(coeffs[!is.na(coeffs)]) * 1.05
x.upper <- max(coeffs[!is.na(coeffs)]) * 1.05



dotchart(rep(-100, length(rownames(coeffs))), xlim=c(x.lower, x.upper), lcolor = "gray", 
         cex.lab = .3,
         labels = rownames(coeffs),
         cex = .8
)
lines(c(0,0), c(0,nrow(coeffs)+1), col="gray", lty=1)


llty <- 1
lpch <- 20
lcex <- 1.3 # 0.75
llwd <- 1.5
plotcolors <- c(brewer.pal(7,"Set1"), "khaki", "olivedrab1", "purple3")
pchsymbols <- c(0:4,6:8,11,12)

for (i in 1:nrow(coeffs)) {
#  cat("---------", rownames(coeffs)[i], "--------------\n")
  for (j in 1:ncol(coeffs)){
    #cat(colnames(coeffs)[j],"\t")
    color <- plotcolors[j]
    #cat(color, "\n")
  points(coeffs[i,j], i, col = color, pch = lpch, cex = lcex)
#   points(coeffs[i,j], i, col = "black", pch = pchsymbols[j], cex = lcex)
  }
}
legend(-4.6, 60.5, legend=colnames(coeffs), cex = .7, fill=plotcolors, ncol=2)
#legend(-4.6, 60.5, legend=colnames(coeffs), cex = .8, pch=pchsymbols, ncol=2)

######################################
