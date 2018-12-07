myeval <-function(model){
  predictions <- ifelse(predict(model,type="response")>.5,1,0)
  iscorrect <- sum(diag(table(model$y,predictions)))/length(predictions)
  baseline <- max(c(length(which(model$y==1))),length(which(model$y==0)))/length(model$y)
  difference <- iscorrect - baseline
  restable <- matrix(data=c(round(iscorrect*100,2),round(baseline*100,2),round(difference*100,2)), byrow=FALSE, nrow=1, ncol=3)
  colnames(restable) <- c("Model","Baseline", "Diff")
  return(restable)
}
