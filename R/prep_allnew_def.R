do_glms <- function(df, subcorp=""){
  results <- matrix(NA, ncol = 6, nrow = length(levels(df$target.prep)))
  colnames(results) <- c("Prep", "Total", "Gen", "Prop.gen", "R2.COReX", "R2.FA")
  count <- 0
  for (l in levels(df$target.prep)){
    count <- count + 1
    dataset <- subset(df, target.prep==l)
    if(nchar(subcorp)>0){
      dataset <- subset(dataset, corpus==subcorp)  
    }
    m.corex <- glm(f.corex, family = binomial, data=dataset)
    m.fa <- glm(f.fa, family = binomial, data=dataset)
    
    results[count,] <- c(l, nrow(dataset), nrow(subset(dataset, nscase==1)), round(nrow(subset(dataset, nscase==1))/nrow(dataset),3), round(NagelkerkeR2(m.corex)$R2,3), round(NagelkerkeR2(m.fa)$R2,3))
    cat(l,":\n")
    cat("\tData points:\t", nrow(dataset), "\n")
    cat("\t% genitive:\t", (nrow(subset(dataset, case=="gen"))/nrow(dataset))*100, "\n")
    cat("\tCOReX:\t", NagelkerkeR2(m.corex)$R2, "\n")
    cat("\tFA:\t", NagelkerkeR2(m.fa)$R2, "\n")
  }
  results <- as.data.frame(results)
  return(results)
}


do_glms_open <- function(df, formula, subcorp=""){
  results <- matrix(NA, ncol = 5, nrow = length(levels(df$target.prep)))
  colnames(results) <- c("Prep", "Total", "Gen", "Prop.gen", "R2")
  count <- 0
  for (l in levels(df$target.prep)){
    count <- count + 1
    dataset <- subset(df, target.prep==l)
    if(nchar(subcorp)>0){
      dataset <- subset(dataset, corpus==subcorp)  
    }
    
    m <- glm(formula, family = binomial, data=dataset)
    
    results[count,] <- c(l, nrow(dataset), nrow(subset(dataset, case=="gen")), round(nrow(subset(dataset, case=="gen"))/nrow(dataset),3), round(NagelkerkeR2(m)$R2,3))
    cat(l,":\n")
    cat("\tData points:\t", nrow(dataset), "\n")
    cat("\t% genitive:\t", (nrow(subset(dataset, case=="gen"))/nrow(dataset))*100, "\n")
    cat("\tR^2:\t", NagelkerkeR2(m)$R2, "\n")
  }
  results <- as.data.frame(results)
  return(results)
}


do_glms_coeffs <- function(df, formula, subcorp=""){
  results <- matrix(NA, ncol = length(levels(df$target.prep)), nrow = 61)
  colnames(results) <- levels(df$target.prep)
  count <- 0
  for (l in levels(df$target.prep)){
    count <- count + 1
    dataset <- subset(df, target.prep==l)
    if(nchar(subcorp)>0){
      dataset <- subset(dataset, corpus==subcorp)  
    }
    
    m <- glm(formula, family = binomial, data=dataset)
    
    coeffs <- coef(summary(m))[,c(1,4)]
    for(i in 1:nrow(coeffs)){
      if(coeffs[i,2] < 0.05 & abs(coeffs[i,1]) < 5){
        results[i, count] <- coeffs[i,1]
      }
    }
  }
  rownames(results) <- rownames(coef(summary(m)))
  results <- as.data.frame(results)
  return(results)
}
