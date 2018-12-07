library(cluster)
library(factoextra)
library(scales)

n.samples       <- 25
n.sampsize      <- 2500
distmet         <- "euclidean"
traclev         <- 1
save.persistent <- T

crxcols <- c("id", "tokc", "ttrat", "wlen", "sentc", "slen", "modal", "vv",
             "vaux", "vfin", "cn", "prep", "inf", "imp", "adv",
             "adj", "subjs", "subji", "conj", "wh", "dem", "poss",
             "neg", "answ", "zuinf", "parta", "card", "itj",
             "nonwrd", "def", "indef", "neper", "neloc", "neorg",
             "emo", "dq", "clitindef", "vpast", "vpres", "vpressubj",
             "wpastsubj", "vvpastsubj", "pper_1st", "pper_2nd", "pper_3rd",
             "gen", "simpx", "psimpx", "rsimpx", "v2", "vlast",
             "vflen", "esvf", "clausevf", "cmpnd", "unkn", "short",
             "qsvoc", "cnloan", "vvieren", "sapos", "pass", "perf",
             "plu")

crxcols.base <- c("ttrat", "wlen", "slen", "modal", "vv",
                  "vaux", "vfin", "cn", "prep", "inf", "imp", "adv",
                  "adj", "subjs", "subji", "conj", "wh", "dem", "poss",
                  "neg", "answ", "zuinf", "parta", "card", "itj",
                  "nonwrd", "def", "indef", "neper", "neloc", "neorg")
crxcols.style <- c("emo", "dq", "clitindef", "short", "qsvoc", "sapos",
                   "vvieren", "cnloan", "unkn", "cmpnd")
crxcols.morpho <- c("vpast", "vpres", "vpressubj",
                    "wpastsubj", "vvpastsubj", "pper_1st", "pper_2nd", "pper_3rd",
                    "gen")
crxcols.syntax <- c("simpx", "psimpx", "rsimpx", "v2", "vlast",
                    "vflen", "esvf", "clausevf",
                    "pass", "perf", "plu")



#crx <- read.csv2(file = "decow16a01_clean.tsv", sep = "\t", quote = "", col.names = crxcols)
#corexdata <- random.subcorp
corexdata <- df.decow

crx.base <- corexdata[, crxcols.base]
#for (i in 1:ncol(crx.base)) crx.base[,i] <- as.numeric(as.character(crx.base[,i]))

crx.style <- corexdata[, crxcols.style]
#for (i in 1:ncol(crx.style)) crx.style[,i] <- as.numeric(as.character(crx.style[,i]))

crx.morpho <- corexdata[, crxcols.morpho]
#for (i in 1:ncol(crx.morpho)) crx.morpho[,i] <- as.numeric(as.character(crx.morpho[,i]))

crx.syntax <- corexdata[, crxcols.syntax]
#for (i in 1:ncol(crx.syntax)) crx.syntax[,i] <- as.numeric(as.character(crx.syntax[,i]))


set.seed(8423)


# === BASIC ===

k.base <- 2
crx.base.clara <- clara(crx.base, k = k.base, metric = distmet, samples = n.samples, sampsize = n.sampsize, trace = traclev)

if (save.persistent) pdf("silh_base.pdf")
  plot(silhouette(crx.base.clara), col = 1, border = 1, main = "Silhouette: COReX Base features")
if (save.persistent) dev.off()

if (save.persistent) pdf("clust_base.pdf")
  viz.base <- fviz_cluster(crx.base.clara, stand = FALSE, geom = "point", pointsize = 1, main = "COReX Clusters: Base Features")
  print(viz.base)
if (save.persistent) dev.off()

if (save.persistent) sink("cluster.txt", append = T)
  cat("\n\n BASE CATEGORIES MEDOIDS\n")
  crx.base[crx.base.clara$i.med,]
if (save.persistent) sink()



# === STYLE ===

k.style <- 3
crx.style.clara <- clara(crx.style, k = k.style, metric = distmet, samples = n.samples, sampsize = n.sampsize, trace = traclev)

if (save.persistent) pdf("silh_style.pdf")
  plot(silhouette(crx.style.clara), col = 1, border = 1, main = "Silhouette: COReX Style")
if (save.persistent) dev.off()

if (save.persistent) pdf("clust_style.pdf")
  viz.style <- fviz_cluster(crx.style.clara, stand = FALSE, geom = "point", pointsize = 1, main = "COReX Clusters: Style")
  print(viz.style)
if (save.persistent) dev.off()

if (save.persistent) sink("cluster.txt", append = T)
  cat("\n\n STYLE MEDOIDS\n")
  crx.style[crx.style.clara$i.med,]
if (save.persistent) sink()

# === MORPHOLOGY ===

k.morpho <- 4
crx.morpho.clara <- clara(crx.morpho, k = k.morpho, metric = distmet, samples = n.samples, sampsize = n.sampsize, trace = traclev)

if (save.persistent) pdf("silh_morpho.pdf")
  plot(silhouette(crx.morpho.clara), col = 1, border = 1, main = "Silhouette: COReX Morphology")
if (save.persistent) dev.off()

if (save.persistent) pdf("clust_morpho.pdf")
  viz.morpho <- fviz_cluster(crx.morpho.clara, stand = FALSE, geom = "point", pointsize = 1, main = "COReX Clusters: Morphology")
  print(viz.morpho)
if (save.persistent) dev.off()

if (save.persistent) sink("cluster.txt", append = T)
  cat("\n\n MORPHOLOGY MEDOIDS\n")
  crx.morpho[crx.morpho.clara$i.med,]
if (save.persistent) sink()


# === SYNTAX ===

k.syntax <- 3
crx.syntax.clara <- clara(crx.syntax, k = k.syntax, metric = distmet, samples = n.samples, sampsize = n.sampsize, trace = traclev)

if (save.persistent) pdf("silh_syntax.pdf")
  plot(silhouette(crx.syntax.clara), col = 1, border = 1, main = "Silhouette: COReX Syntax")
if (save.persistent) dev.off()

if (save.persistent) pdf("clust_syntax.pdf")
  viz.syntax <- fviz_cluster(crx.syntax.clara, stand = FALSE, geom = "point", pointsize = 1, main = "COReX Clusters: Syntax")
  print(viz.syntax)
if (save.persistent) dev.off()

if (save.persistent) sink("cluster.txt", append = T)
  cat("\n\n SYNTAX MEDOIDS\n")
  crx.syntax[crx.syntax.clara$i.med,]
if (save.persistent) sink()



# Helpers for standardization.
normstd <- function(x, m, s) {
  r <- (m - x)/s
  r
}
normstd.row <- function(r, m, s) {
  nr <- lapply(r, function(x) normstd(x, m, s))
  nr
}

# Make standardized version of df.
crx.base.norm <- crx.base
for (i in 1:length(crx.base.norm)) {
  crx.base.norm[,i] <- unlist(normstd.row(crx.base.norm[,i], mean(crx.base.norm[,i]), sd(crx.base.norm[,i])))
}
crx.style.norm <- crx.style
for (i in 1:length(crx.style.norm)) {
  crx.style.norm[,i] <- unlist(normstd.row(crx.style.norm[,i], mean(crx.style.norm[,i]), sd(crx.style.norm[,i])))
}
crx.syntax.norm <- crx.syntax
for (i in 1:length(crx.syntax.norm)) {
  crx.syntax.norm[,i] <- unlist(normstd.row(crx.syntax.norm[,i], mean(crx.syntax.norm[,i]), sd(crx.syntax.norm[,i])))
}
crx.morpho.norm <- crx.morpho
for (i in 1:length(crx.morpho.norm)) {
  crx.morpho.norm[,i] <- unlist(normstd.row(crx.morpho.norm[,i], mean(crx.morpho.norm[,i]), sd(crx.morpho.norm[,i])))
}

# Get ggplot colors for n categories:
# show_col(hue_pal()(2))

if (save.persistent) pdf("dist_base.pdf")
par(mar = c(4,6,4,4))
barplot(as.matrix(crx.base.norm[crx.base.clara$i.med, c(1, 2, 7, 12, 13, 18, 21, 28, 29, 30, 32)]),
        beside = T, horiz = T, las = 1, col = hue_pal()(k.base),
        main = "COReX: Base features distributions in medoids\n(standardized/normalized)"
        )
if (save.persistent) dev.off()

if (save.persistent) pdf("dist_morpho.pdf")
par(mar = c(4,8,4,4))
barplot(as.matrix(crx.morpho.norm[crx.morpho.clara$i.med, c(1, 2, 6, 7, 8, 9)]),
        beside = T, horiz = T, las = 1, col = hue_pal()(k.morpho),
        main = "COReX: Morphological features distributions in medoids\n(standardized/normalized)"
        )
if (save.persistent) dev.off()

if (save.persistent) pdf("dist_style.pdf")
par(mar = c(4,6,4,4))
barplot(as.matrix(crx.style.norm[crx.style.clara$i.med, c(1, 3, 4, 5, 7, 8, 10)]),
        beside = T, horiz = T, las = 1, col = hue_pal()(k.style),
        main = "COReX: Style features distributions in medoids\n(standardized/normalized)"
        )
if (save.persistent) dev.off()

if (save.persistent) pdf("dist_syntax.pdf")
par(mar = c(4,6,4,4))
barplot(as.matrix(crx.syntax.norm[crx.syntax.clara$i.med[c(1, 3)], c(2, 3, 6, 7, 8, 9, 10)]),
        beside = T, horiz = T, las = 1, col = hue_pal()(k.syntax)[c(1,3)],
        main = "COReX: Syntax features distributions in medoids\n(standardized/normalized)"
        )
if (save.persistent) dev.off()

#crx$cluster.base       <- as.factor(crx.base.clara$clusterin)
#crx$cluster.morphology <- as.factor(crx.morpho.clara$clustering)
#crx$cluster.syntax     <- as.factor(crx.syntax.clara$clustering)
#crx$cluster.style      <- as.factor(crx.style.clara$clustering)

#sem$cluster.semantics  <- as.factor(sem.clara$clustering)
