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
#
# from Stackoverflow:

filter_loadings_old <- function(fa.output, threshold = 0.35){
  Y <- t(apply(fa.output$loadings, 1, function(z){1 * (z == max(z) & abs(z) >= threshold)}))
  return(Y)
} 

filter_loadings <- function(fa.output, threshold = 0.35){
  Y <- t(apply(fa.output$loadings, 1, function(z){sign(z) * (abs(z) == max(abs(z)) & abs(z) >= .35)} ))
  return(Y)
} 




docscores_biber <- function(corpus_df, corpus_columns_used_in_fa, filtered_loadings){
  # calculate the actual document scores according to the filtered factor loadings
  
  # check if feature names are the same:
  if(length(which((rownames(filtered_loadings) == colnames(corpus_df[,corpus_columns_used_in_fa]))==FALSE)) > 0){
    stop("Column names from corpus data frame not equal to row names from filtered loadings.\n")}
  else {
    
    # make a matrix with a column for the doc-ID and one column for each factor from the fa:
    m <- matrix(NA,nrow=nrow(corpus_df[,corpus_columns_used_in_fa]), ncol=(ncol(filtered_loadings)+1))
    m[,1] <- corpus_df$id
    colnames(m) <- c("id", colnames(filtered_loadings))
    
    # multiply a column of filtered factor loadings with each row (document) in the corpus data frame,
    # and save the sum as the document score for that factor; repeat for all factors:
    system.time(for(i in 1:ncol(filtered_loadings)){
      cat("Now calculating document scores for factor:", colnames(filtered_loadings)[i], "\n")
      m[,i+1] <- round(rowSums(sweep(corpus_df[,corpus_columns_used_in_fa],MARGIN=2,filtered_loadings[,i],`*`)),3)
    })
    m <- as.data.frame(m)
    m[,2:ncol(m)] <- lapply(m[,2:ncol(m)], as.character)
    m[,2:ncol(m)] <- lapply(m[,2:ncol(m)], as.numeric)
    return(m)
  }
}


docscore_means_by_corpus <- function(corpus_df, factorcols, byfactorcol){
  thelevels <- levels(corpus_df[,byfactorcol])
  cat(thelevels, "\n")
  for (i in factorcols){
    cat("Factor ", colnames(corpus_df)[i], "\n")
    cat("\t", thelevels, "\n\t")
    for (level in thelevels){
      cat(mean(subset(corpus_df, corpus_df[,byfactorcol]==level)[,i]), " ")
    }
    cat("\n")
  }
}
  
#  mean(subset(derecow.meta.scaled.with.scores, corpus=="dereko")$PA2)
#  mean(subset(derecow.meta.scaled.with.scores, corpus=="dereko")$PA2)
  

# do_glms <- function(df, subcorp=""){
#   results <- matrix(NA, ncol = 6, nrow = length(levels(df$target.prep)))
#   colnames(results) <- c("Prep", "Total", "Gen", "Prop.gen", "R2.COReX", "R2.FA")
#   count <- 0
#   for (l in levels(df$target.prep)){
#     count <- count + 1
#     dataset <- subset(df, target.prep==l)
#     if(nchar(subcorp)>0){
#     dataset <- subset(dataset, corpus==subcorp)  
#     }
#     m.corex <- glm(f.corex, family = binomial, data=dataset)
#     m.fa <- glm(f.fa, family = binomial, data=dataset)
#     
#     results[count,] <- c(l, nrow(dataset), nrow(subset(dataset, case=="gen")), round(nrow(subset(dataset, case=="gen"))/nrow(dataset),3), round(NagelkerkeR2(m.corex)$R2,3), round(NagelkerkeR2(m.fa)$R2,3))
#     cat(l,":\n")
#     cat("\tData points:\t", nrow(dataset), "\n")
#     cat("\t% genitive:\t", (nrow(subset(dataset, case=="gen"))/nrow(dataset))*100, "\n")
#     cat("\tCOReX:\t", NagelkerkeR2(m.corex)$R2, "\n")
#     cat("\tFA:\t", NagelkerkeR2(m.fa)$R2, "\n")
#   }
#   results <- as.data.frame(results)
#   return(results)
# }