auto.chisq <- function(x,y){
  results <- chisq.test(x,y,correct = F)
  if (min(results$expected) >= 1) {
    #frequency analysis assumption fit
    if ((length(results$expected[results$expected > 5])/length(results$expected)) > 0.8) {
      #over 20% cells have expected fq >5
      cat("test assumptions are met, Chi square test selected \n")
      cat("\nObserved frequencies: \n")
      print(results$observed)
      cat("\nExpected frequencies: \n")
      print(results$expected)
      cat("\nResults:\n")
      print(results)

    }else if ((length(results$expected[results$expected > 5])/length(results$expected)) < 0.8){
      #"less than 20% of cells have expected fq >5, Fisher's Exact test selected"
      cat("Chi square test assumption is not met, Fisher's exact test selected \n")
      cat("\nObserved frequencies: \n")
      print(results$observed)
      cat("\nExpected frequencies: \n")
      print(results$expected)
      cat("\nResults:\n")
      print(fisher.test(x,y))
    }
  }else{
    cat("\nExpected frequencies: \n")
    print(results$expected)
    warning("cell(s) with expected frequency < 1 exists, cannot conduct test, merging of categories required")
  }
}
