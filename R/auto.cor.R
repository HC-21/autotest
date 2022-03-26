auto.cor <- function(x,y,
                      xname=deparse(substitute(x)),
                      yname=deparse(substitute(y))){

  plot(x,y,xlab = xname,ylab = yname)
  readline("Showing scatter plot of the variables, return to see next graph:")

  xqq<-paste("Q-Q Plot of",xname)
  yqq<-paste("Q-Q Plot of",yname)
  xnorm<-paste("Showing qq plot of",xname,", return to see next graph:")
  ynorm<-paste("Showing qq plot of",yname,", return to see next graph:")

  qqnorm(x,main = xqq)
  qqline(x)
  readline(xnorm)

  qqnorm(y,main = yqq)
  qqline(y)
  readline(ynorm)

  cat("Are all the assumptions met?: \n if true, return 1 for Pearson's coefficient,  \n otherwise return 0 for Spearman's rank coefficient")
  ans <- readline("Answer:")
  print(ans)
  if(ans == "T"|ans == 1){
    print(cor.test(x, y, method = "p"))
  }else if(ans == "F"|ans == 0){
    print(cor.test(x,y,method = "s"))
  }
}
