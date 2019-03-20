library(dplyr)
# get identifiers of old (2016) "derecow" documents: 
# with dereko, use textsigle:
derekogac.textsigle <- read.csv(file="/Users/felix/Documents/Konferenzen/2016/gac2016/corpusstudies/prepositions/metadb/min100tokens/derekogac_min100_meta.TEXTSIGLE", header = FALSE, sep = "\t")
colnames(derekogac.textsigle) <- "textsigle"
length(derekogac.textsigle$textsigle)

# with decow, use id:
decow16.ID <- read.csv(file="/Users/felix/Documents/Konferenzen/2016/gac2016/corpusstudies/prepositions/metadb/min100tokens/de16mini_min100_meta-IDs", header = FALSE, sep = "\t")
colnames(decow16.ID) <- "id"
length(decow16.ID$id)
head(decow16.ID)


maxchunksize=15000

# get information from new corex db.
# dereko:
dereko.textsiglen.f <- format_mysql_listquery(derekogac.textsigle$textsigle)
siglen.chunks <- split(dereko.textsiglen.f, ceiling(seq_along(dereko.textsiglen.f)/maxchunksize))

dereko.meta.neu <- parallel_from_dereko(chunks = siglen.chunks, selectby = "textsigle")
dereko.meta.neu$corpus <- as.factor(rep("dereko", nrow(dereko.meta.neu)))

# decow:
decow.ids.f <- format_mysql_listquery(decow16.ID$id)
decow.ids.chunks <- chunker(decow.ids.f, maxchunksize = 15000)

# test
#corpussample_naked(decow.ids.chunks$`1`, connectiondetails_decow, tablename="decow16b_meta_min100", selectby = "id")
decow16.meta.neu <- parallel_from_decow(chunks = decow.ids.chunks, selectby = "id")
decow16.meta.neu$corpus <- as.factor(rep("decow", nrow(decow16.meta.neu)))

colnames(decow16.meta.neu) ==  colnames(dereko.meta.neu)

# bind both dataframes together:
derecow.meta <- rbind(dereko.meta.neu, decow16.meta.neu)
# delete 'sentc' (number of sentences, not normalised):
#derecow.meta <- derecow.meta[,c(1:4,6:ncol(derecow.meta))]

colnames(derecow.meta)

########################### run factor analysis:

## scale all numerical values to z-scores:
derecow.meta.scaled <- derecow.meta
colnames(derecow.meta.scaled)
nrow(derecow.meta.scaled)
derecow.meta.scaled[3:64] <- lapply(derecow.meta.scaled[,3:64], scale)
head(derecow.meta.scaled)

parallel<-fa.parallel(derecow.meta.scaled[,3:64], fm='minres', fa='fa')
# Call: fa.parallel(x = derecow.meta.scaled[, 3:63], fm = "minres", fa = "fa")
# Parallel analysis suggests that the number of factors =  21  and the number of components =  NA 


# # try psych::fa
# fa.n12 <- fa(derecow.meta.scaled[,3:63], nfactors=12, rotate="promax")
# print(fa.n12$loadings, cutoff = 0.3)
# 
# fa.n16 <- fa(derecow.meta.scaled[,3:63], nfactors=16, rotate = "oblimin",fm="minres")
# print(fa.n16$loadings,cutoff = 0.3)
# fa.diagram(fa.n16, cutoff = 0.3)
# fa.n16
# fa.sort(fa.n16)
# fa.organize(fa.n16,c(7,1,2)) 
# 
# fa.n16 <- fa(derecow.meta.scaled[,3:63], nfactors=16, rotate = "oblimin",fm="pa")
# 
# 
# fa.n21 <- fa(derecow.meta.scaled[,3:63], nfactors=21, rotate = "oblimin",fm="minres")
# print(fa.n21$loadings,cutoff = 0.3)
# 
# # Get the factor scores for each document:
# fa.n12.scores <- data.frame(fa.n12$scores)
# 
# # Get the documents' IDs:
# docids <- data.frame(meta[,1])
# 
# # bind them together in a single data frame:
# fa.n12.scores <- cbind.data.frame(docids, fa.n12.scores)
# head(fa.n12.scores)
# # rename column:
# colnames(fa.n12.scores)[1] <- "id"
# 
# head(fa.n12.scores)
# 


################## this new #################

derecow.meta.scaled.fa.n7.pa.promax <- fa(derecow.meta.scaled[,3:64], nfactors=7, rotate = "promax", fm="pa")
print(derecow.meta.scaled.fa.n7.pa.promax, cut = 0.3)
fa.diagram(derecow.meta.scaled.fa.n7.pa.promax, cut = 0.3)

derecow.filtered.fa.loadings <- filter_loadings(derecow.meta.scaled.fa.n7.pa.promax, threshold = 0.35)
derecow.filtered.fa.loadings

derecow.document.scores <- docscores_biber(derecow.meta.scaled, 3:64, derecow.filtered.fa.loadings) 

derecow.meta.scaled.with.scores <- merge(x = derecow.meta.scaled, y = derecow.document.scores, by = "id", all.x = TRUE)

head(derecow.meta.scaled.with.scores)[66:72]

head(derecow.meta.scaled.fa.n7.pa.promax$scores)

docscore_means_by_corpus(derecow.meta.scaled.with.scores, 66:72, 65)


levels(derecow.meta.scaled.with.scores[,65])

       
# also, try $scores from the fa model:
derecow.meta.scaled.with.modelscores <- cbind.data.frame(derecow.meta.scaled, derecow.meta.scaled.fa.n7.pa.promax$scores)
derecow.meta.scaled.fa.n7.pa.promax.modelscores <- derecow.meta.scaled.with.modelscores[c(1,66:72)]
colnames(derecow.meta.scaled.fa.n7.pa.promax.modelscores) <- c("id", "m.PA1", "m.PA4", "m.PA5", "m.PA2", "m.PA7", "m.PA3", "m.PA6")
colnames(derecow.meta.scaled.fa.n7.pa.promax.modelscores) 


mean(subset(derecow.meta.scaled.with.scores, corpus=="dereko")$PA2)
mean(subset(derecow.meta.scaled.with.scores, corpus=="dereko")$PA2)


############# alternative fa with 21 factors:

derecow.meta.scaled.fa.n21.pa.promax <- fa(derecow.meta.scaled[,3:64], nfactors=21, rotate = "promax", fm="pa")
fa.diagram(derecow.meta.scaled.fa.n21.pa.promax, cut = 0.3)
derecow.filtered.fa.21.loadings <- filter_loadings(derecow.meta.scaled.fa.n21.pa.promax, threshold = 0.35)
derecow.document.scores.21 <- docscores_biber(derecow.meta.scaled, 3:64, derecow.filtered.fa.21.loadings) 

colnames(derecow.document.scores.21) <- gsub("PA", "n21PA", colnames(derecow.document.scores.21))
head(derecow.document.scores.21)
