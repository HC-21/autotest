auto.anova <- function(formula,data=NULL,
                       xname=substitute(x),
                       yname=substitute(y)){
  require(gplots)
  require(FSA)
  #format data
  if (is.null(data)==F) {
    y<-all.vars(formula)[1]
    x<-all.vars(formula)[2]
    d<-deparse(substitute(data))
    formula<-formula(paste(d,"$",y,"~",d,"$",x))
  }

  ano.model<-aov(formula)
  par(mfrow=c(2,2))
  plot(ano.model)
  par(mfrow=c(1,1))
  assumption<-readline("does the model fit assumption? reply T or F :")
  if (assumption=="T"|assumption==1) {
    gplots::plotmeans(formula,connect = 1,xlab = xname, ylab = yname)
    print(anova(ano.model))
    cat(" \n ")
    print(TukeyHSD(ano.model))
  }else if (assumption=="F"|assumption==2) {
    boxplot(formula,xlab = xname, ylab = yname)
    print(kruskal.test(formula))
    print(FSA::dunnTest(formula))
  }
}
