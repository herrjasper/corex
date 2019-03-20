# Fit models for individual prepositions. Discard prepositions that show no variation wrt. "case":


# "ausweislich":
prepconc.new.withnewdata.withscores.allvaried <- subset(prepconc.new.withnewdata.withscores, targetprep!="ausweislich")
prepconc.new.withnewdata.withscores.allvaried$targetprep <- factor(prepconc.new.withnewdata.withscores.allvaried$targetprep)
levels(prepconc.new.withnewdata.withscores.allvaried$targetprep)


f.corex <- paste("case", " ~ ", paste(corex.predictors.nogen, collapse = " + "))
f.fa <- paste("case", " ~ ", paste(fa.predictors, collapse = " + "))


results <- matrix(NA, ncol = 6, nrow = length(levels(prepconc.new.withnewdata.withscores.allvaried$targetprep)))
colnames(results) <- c("Prep", "Total", "Gen", "Prop.gen", "R2.COReX", "R2.FA")
count <- 0
for (l in levels(prepconc.new.withnewdata.withscores.allvaried$targetprep)){
count <- count + 1
dataset <- subset(prepconc.new.withnewdata.withscores.allvaried, targetprep==l)
m.corex <- glm(f.corex, family = binomial, data=dataset)
m.fa <- glm(f.fa, family = binomial, data=dataset)

results[count,] <- c(l, nrow(dataset), nrow(subset(dataset, case=="gen")), round(nrow(subset(dataset, case=="gen"))/nrow(dataset),3), round(NagelkerkeR2(m.corex)$R2,3), round(NagelkerkeR2(m.fa)$R2,3))
cat(l,":\n")
cat("\tData points:\t", nrow(dataset), "\n")
cat("\t% genitive:\t", (nrow(subset(dataset, case=="gen"))/nrow(dataset))*100, "\n")
cat("\tCOReX:\t", NagelkerkeR2(m.corex)$R2, "\n")
cat("\tFA:\t", NagelkerkeR2(m.fa)$R2, "\n")
}
results <- as.data.frame(results)

results <- results[order(results$Prop.gen),]

results.combined <- results
results.cow <- results
results.dereko <- results

colnames(results.cow) <- gsub('^', 'cow.', colnames(results.cow))
colnames(results.dereko) <- gsub('^', 'dereko.', colnames(results.dereko))

tmp <- cbind.data.frame(results.combined, results.cow, results.dereko)

colnames(tmp)[1] <- "Prep"
results.comparison <- tmp[,c(1:6,8:12,14:18)]
# turn factors into floats:
results.comparison[,2:15] <- lapply(results.comparison[,2:15], as.character)
results.comparison[,2:15] <- lapply(results.comparison[,2:15], as.numeric)

# assess case variability per preposition per corpus;
# choose only prepositions for which the non-modal case category accounts for more than 10% of the data:
results.comparison[which((results.comparison$dereko.Prop.gen < .9 & results.comparison$dereko.Prop.gen > .1)|(results.comparison$cow.Prop.gen < .9 & results.comparison$cow.Prop.gen > .1)),c(1:4,7:9,12:14)]
validpreps <- results.comparison[which((results.comparison$dereko.Prop.gen < .9 & results.comparison$dereko.Prop.gen > .1)|(results.comparison$cow.Prop.gen < .9 & results.comparison$cow.Prop.gen > .1)),c(1:4,7:9,12:14)]$Prep
validpreps <- factor(validpreps)

validpreps
nrow(subset(prepconc.new.withnewdata.withscores, targetprep %in% validpreps))
nrow(prepconc.new.withnewdata.withscores)

# m.corex <- glm(f.corex, family = binomial, data=subset(prepconc.new.withnewdata.withscores.allvaried, targetprep=="trotz"))
# cat("\tNagelkerke R^2 COReX:\t", NagelkerkeR2(m.corex)$R2, "\n")
# m.fa <- glm(f.fa, family = binomial, data=subset(prepconc.new.withnewdata.withscores.allvaried, targetprep=="trotz"))
# cat("\tNagelkerke R^2 FA:\t", NagelkerkeR2(m.fa)$R2, "\n")
# 
# summary(m.fa)
# summary(m.corex)
# 
# dereko.samt <- subset(subset(prepconc.new.withnewdata.withscores.allvaried, targetprep=="samt"), corpus=="dereko")
# dereko.samt$targetprep <- factor(dereko.samt$targetprep)
# table(dereko.samt$case)
# glm(f.corex, family = binomial, data=dereko.samt)
# head(dereko.samt)

sum(subset(prepconc.new.withnewdata.withscores.allvaried, corpus=="cow")$tokc)
nrow(subset(prepconc.new.withnewdata.withscores.allvaried, corpus=="dereko"))
