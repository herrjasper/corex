# redo analysis form prep_allnew.R,
# but using only the 10 prepositions that show
# substantial variation in case assignment (>= 10% non-standard alternat);
# and with a stratified sample of max. 4000 tokens per preposition per corpus
# (less tokens from "zuzüglich" and "mangels")

library("fmsb")
library("RColorBrewer")
library(plyr)
library(DAMisc)

stratpreps <- read.csv2("/Users/felix/Documents/Papers/corex/casestudies/prepositions/combined.concordance.varyingpreps.4000.csv", header=FALSE, sep="\t", col.names = c("corpus", "id", "sidx", "case", "forum", "match", "target.prep", "next.pos", "next.lemma"))
stratpreps$id <- as.character(stratpreps$id)
stratpreps$forum <- as.factor(stratpreps$forum)
stratpreps$match <- as.character(stratpreps$match)

##############################################

dative.preps <- c("mitsamt", "gemäß", "entgegen")
genitive.preps <- c("mangels", "wegen", "dank", "zuzüglich", "trotz", "mittels", "einschließlich")

# create a variable for "non-standard case":
stratpreps$nscase <- ifelse(stratpreps$target.prep %in% dative.preps, ifelse(stratpreps$case=="gen", 1, 0),ifelse(stratpreps$case=="dat", 1,0))
stratpreps$nscase <- as.factor(stratpreps$nscase)

# keep only a single concordance line from each document:
# nrow(stratpreps)
# [1] 75329
stratpreps  <- stratpreps[!duplicated(stratpreps$id),]
# nrow(stratpreps)
# [1] 73922

#############################################

# add COReX data
# parallel retrieval does not work currenty; split ID list in twi halves:
# get doc-IDs from dereko documents; format and chunk:
id.dereko <- format_mysql_listquery(subset(stratpreps, corpus=="dereko")$id)
id.dereko.chunks1 <- chunker(vec = id.dereko[1:20001], maxchunksize = 40000)
id.dereko.chunks2 <- chunker(vec = id.dereko[20001:length(id.dereko)], maxchunksize = 40000)

# get doc-IDs from decow documents; format and chunk:
id.decow <- format_mysql_listquery(subset(stratpreps, corpus=="decow")$id)
id.decow.chunks1 <- chunker(vec = id.decow[1:20000], maxchunksize = 40000)
id.decow.chunks2 <- chunker(vec = id.decow[20001:length(id.decow)], maxchunksize = 40000)

# set up connection details:
# parallel setup:
no_cores <- detectCores() - 1

connectiondetails_dereko <- c('ukviewer', 'ukviewer', 'dereko', 'gramlinux02')
connectiondetails_decow <- c('decowviewer', 'decowviewer', 'decow', 'gramlinux02')

# retrieve corex data:
dereko.md1 <- parallel_from_dereko(chunks = id.dereko.chunks1, selectby = "id")
dereko.md2 <- parallel_from_dereko(chunks = id.dereko.chunks2, selectby = "id")
decow.md1 <- parallel_from_decow(chunks = id.decow.chunks1, selectby = "id")
decow.md2 <- parallel_from_decow(chunks = id.decow.chunks2, selectby = "id")
combined.md <- rbind.data.frame(dereko.md, dereko.md2, decow.md, decow.md2)

# scale COReX values:
combined.md[, c(2:ncol(combined.md))] <-  lapply(combined.md[, c(2:ncol(combined.md))], scale)

# merge concordance df with corex df:
stratpreps.md <- merge(y = combined.md, x = stratpreps, by = "id", all.y = TRUE)

########################################################

# inspect distribution of prepositions by corpus (all prepositions per corpus adding up to 1):
barplot(xtabs(~corpus+target.prep, data=stratpreps.md), beside=TRUE,col = c("black", "lightgray"), las=2)
legend(1, 4300, legend=c("decow", "dereko"), cex = 1, fill=c("black", "lightgray"),  bty = "n")

# plot proportion of genitive by preposition and corpus:
stratpreps.md.genprops <- xtabs(~corpus+target.prep, data=subset(stratpreps.md, case=="gen"))/xtabs(~corpus+target.prep, data=stratpreps.md)
stratpreps.md.genprops.ordered <- stratpreps.md.genprops[,order(stratpreps.md.genprops[1,])]

opar <- (mar=c(5.1,4.1,4.1,2.1))
par(mar=c(7.3,4.1,4.1,2.1))

barplot(stratpreps.md.genprops.ordered, ylim=c(0,1), beside=TRUE, las=2, cex.names = 1.2, col=c("black", "lightgray"), main="Proportion of genitives by corpus and preposition")
legend(0, 1, legend=rownames(prep.allnew.md.genprops), cex = 1, fill=c("black", "lightgray"),  bty = "n")

# plot proportion of nscase by preposition and corpus:
stratpreps.md.nscaseprops <- xtabs(~corpus+target.prep, data=subset(stratpreps.md, nscase=="1"))/xtabs(~corpus+target.prep, data=stratpreps.md)
stratpreps.md.nscaseprops.ordered <- stratpreps.md.nscaseprops[,order(stratpreps.md.nscaseprops[1,])]

barplot(stratpreps.md.nscaseprops.ordered, ylim=c(0,.4), beside=TRUE, las=2, cex.names = 1.2, col=c("black", "lightgray"), main="Proportion of non-standard case by corpus and preposition")
legend(0, .4, legend=rownames(prep.allnew.md.genprops), cex = 1, fill=c("black", "lightgray"),  bty = "n")







################################################################

# add factor scores for every document, using filtered factor loadings from a specific a FA model:

docscores <- docscores_biber(stratpreps.md, c(11:ncol(stratpreps.md)), filtered.fa.loadings.new)
stratpreps.md.scores <- merge(x = stratpreps.md, y = docscores, by = "id", all.x = TRUE)


################################################################

# summary of proportions of genitive and sample size by preposition and corpus: 

stratpreps.stats <- cbind(t(xtabs(~corpus+target.prep, data=subset(stratpreps.md.scores, case=="gen"))/xtabs(~corpus+target.prep, data=stratpreps.md.scores)),t(xtabs(~corpus+target.prep, data=stratpreps.md.scores)), colSums(xtabs(~corpus+target.prep, data=stratpreps.md.scores)))
colnames(stratpreps.stats) <- c("gen.decow", "gen.dereko", "n.decow", "n.dereko", "n")
stratpreps.stats

# summary of proportions of non-standard case sample size by preposition and corpus: 

stratpreps.stats2 <- cbind(round(t(xtabs(~corpus+target.prep, data=subset(stratpreps.md.scores, nscase=="1"))/xtabs(~corpus+target.prep, data=stratpreps.md.scores)),2),t(xtabs(~corpus+target.prep, data=stratpreps.md.scores)), colSums(xtabs(~corpus+target.prep, data=stratpreps.md.scores)))
colnames(stratpreps.stats2) <- c("nscase.decow", "nscase.dereko", "n.decow", "n.dereko", "n.total")
stratpreps.stats2


################################################################
################################################################

# Modelling

# GLMs:
# get names of independent variables:
corex.predictors <- colnames(stratpreps.md.scores)[11:70]
fa.predictors <- colnames(stratpreps.md.scores)[71:77]

f.corex <- paste("nscase", " ~ ", paste(corex.predictors, collapse = " + "))
f.fa <- paste("nscase", " ~ ", paste(fa.predictors, collapse = " + "))

f.corex.global <- paste("nscase", " ~ target.prep + ", paste(corex.predictors, collapse = " + "))
f.fa.global <- paste("nscase", " ~ target.prep + ", paste(fa.predictors, collapse = " + "))

#f.corex <- "nscase ~ corpus"
#f.fa <- ("nscase ~ forum")



# produce results.combined, results.dereko, results.decow ...
results.combined <- do_glms(stratpreps.md.scores)


####################################################

# plot R2 and PRE:

plot(seq(1,10), results.combined$R2.FA, col="blue", pch=19, ylim=c(0,0.5), xaxt="n", xlab="", main = "Nagelkerke R2 by model and preposition")
axis(1, at=1:10, labels=results.combined$Prep,las=2)
points(seq(1,10), results.combined$R2.COReX, col="red", pch=19)
legend(1,.5, legend=c("GLM with COReX predictors", "GLM with FA predictors"), cex = 1, fill=c("red", "blue"), ncol=1, bty="n")

plot(seq(1,10), results.combined$PRE.FA, col="blue", pch=19, ylim=c(0,0.35), xaxt="n", xlab="", ylab = "PRE", main="Proportional reduction in error by model and preposition")
axis(1, at=1:10, labels=results.combined$Prep,las=2)
points(seq(1,10), results.combined$PRE.COReX, col="red", pch=19)
legend(1,.35, legend=c("GLM with COReX predictors", "GLM with FA predictors"), cex = 1, fill=c("red", "blue"), ncol=1, bty="n")

m <- matrix(ncol = 2,  c(results.combined$R2.COReX, results.combined$R2.FA))
colnames(m) <- c("R2.COReX","R2.FA")        
rownames(m) <- results.combined$Prep        
barplot(t(m), beside = TRUE, col=c("red", "blue"), main = "Nagelkerke R2 by model and preposition", las=2)
legend(1,.5, legend=c("GLM with COReX predictors", "GLM with FA predictors"), cex = 1, fill=c("red", "blue"), ncol=1, bty="n")

m <- matrix(ncol = 2,  c(results.combined$PRE.COReX, results.combined$PRE.FA))
colnames(m) <- c("PRE.COReX","PRE.FA")        
rownames(m) <- results.combined$Prep        
barplot(t(m), beside = TRUE, col=c("red", "blue"), main = "PRE by model and preposition", las=2)
legend("topleft", legend=c("GLM with COReX predictors", "GLM with FA predictors"), cex = 1, fill=c("red", "blue"), ncol=1, bty="n")

####################################################
####################################################

# plot coefficient estimates from individual models:

# get a matrix of all coefficient estimates with associated p-value of < .05, and between 5 and -5: 
coeffs <- do_glms_coeffs(stratpreps.md.scores, paste("nscase", " ~ ", paste(corex.predictors, collapse = " + ")), threshold = 0.001)
# remove the intercept:
coeffs<-coeffs[2:nrow(coeffs),]
coeffs <- coeffs[fac5.predictors,]


# get maximum and minimum coefficient 
x.lower <- min(coeffs[!is.na(coeffs)]) * 1.05
x.upper <- max(coeffs[!is.na(coeffs)]) * 1.05



dotchart(rep(-100, length(rownames(coeffs))),
         xlim = c(x.lower, x.upper),
         ylim = c(0,12),
         lcolor = "gray", 
         cex.lab = .3,
         labels = rownames(coeffs),
         cex = .8,
         main = "GLM coefficient estimates for selected COReX features, by preposition",
         sub = "Features associated with Factor 5"
)

lines(c(0,0), c(0,nrow(coeffs)+1), col="gray", lty=1)


llty <- 1
lpch <- 20
lcex <- 1.3 # 0.75
llwd <- 2
#plotcolors <- c(brewer.pal(7,"Set1"), "khaki", "olivedrab1", "purple3")
plotcolors <- c(brewer.pal(8,"Dark2"), brewer.pal(11, "RdYlGn")[1], brewer.pal(9,"YlOrRd")[6])
pchsymbols <- c(0:4,6:8,11,12)

for (i in 1:nrow(coeffs)) {
  #  cat("---------", rownames(coeffs)[i], "--------------\n")
  for (j in 1:ncol(coeffs)){
    #cat(colnames(coeffs)[j],"\t")
    color <- plotcolors[j]
    #cat(color, "\n")
    #points(coeffs[i,j], i, col = color, pch = lpch, cex = lcex)
    points(coeffs[i,j], i, col = plotcolors[j], pch = pchsymbols[j], cex = lcex, lwd=llwd)
  }
}
#legend(-0.65, 58, legend=colnames(coeffs), cex = .7, fill=plotcolors, ncol=2)
#legend(-0.65, 15, legend=colnames(coeffs), cex = .7, fill=plotcolors, ncol=2)
# all facs:
#legend(-0.65, 15, legend=colnames(coeffs), cex = .7, pch=pchsymbols, ncol=2)

# fac 5:
#legend(-0.6, nrow(coeffs)+.7, legend=colnames(coeffs), cex = .7, pch=pchsymbols, ncol=2)
# fac 1:
legend("topleft", legend=colnames(coeffs), cex = .7, pch=pchsymbols, ncol=2, text.width=.1)

#########################################

# group corex predictors by factors:

fac1.predictors <- rownames(filtered.fa.loadings[(filtered.fa.loadings[,'PA1'] != 0),])
fac2.predictors <- rownames(filtered.fa.loadings[(filtered.fa.loadings[,'PA2'] != 0),])
fac3.predictors <- rownames(filtered.fa.loadings[(filtered.fa.loadings[,'PA3'] != 0),])
fac4.predictors <- rownames(filtered.fa.loadings[(filtered.fa.loadings[,'PA4'] != 0),])
fac5.predictors <- rownames(filtered.fa.loadings[(filtered.fa.loadings[,'PA5'] != 0),])
fac6.predictors <- rownames(filtered.fa.loadings[(filtered.fa.loadings[,'PA6'] != 0),])
fac7.predictors <- rownames(filtered.fa.loadings[(filtered.fa.loadings[,'PA7'] != 0),])


# Try POS tag of next token as predictor:
do_glms_open(stratpreps.md.scores, "nscase ~ next.pos")



