require(gplots)
require(FSA)
require(tidyverse)
require(car)
require(Rmisc)

# Rmisc::CI
# FSA::dunnTest
# car::leveneTest
# gplots::plotCI
# gplots::plotmeans

auto.anova <- function(formula,data=NULL,
                       xname=NULL,
                       yname=NULL){
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
    gplots::plotmeans(formula,connect = 1)
    print(anova(ano.model))
    cat(" \n ")
    print(TukeyHSD(ano.model))
  }else if (assumption=="F"|assumption==2) {
    boxplot(formula)
    print(kruskal.test(formula))
    print(FSA::dunnTest(formula))
  }
}

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

auto.corr <- function(x,y,
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

auto.lm<- function(formula, data=NULL,xname=substitute(x),yname=substitute(y)){
  if (is.null(data)==F) {
    y<-all.vars(formula)[1]
    x<-all.vars(formula)[2]
    d<-deparse(substitute(data))
    formula<-formula(paste(d,"$",y,"~",d,"$",x))
  }

  d<-all.vars(formula)[1]
  y<-all.vars(formula)[2]
  x<-all.vars(formula)[3]
  dx<-paste(d,"$",x)
  dy<-paste(d,"$",x)

  print(list(dx,dy))

  m<-lm(formula)
  par(mfrow=c(2,2))
  plot(m)
  par(mfrow=c(1,1))
  readline("displaying diagnostic plot, n\
           return to display scatter plot with linear relationship plot:")
  plot(formula,xlab=xname,ylab = yname)+abline(lm(formula))
  print(summary(m))
}

auto.t <- function(x, ...) UseMethod("auto.t")
require(tidyverse)
require(gplots)
require(car)
require(Rmisc)


auto.t.default <- function(x, y = NULL,mu = 0, paired = FALSE, var.equal = FALSE,xname = as.character(deparse(substitute(x))),yname = as.character(deparse(substitute(y))),...)
{
  if (is.null(y)) {
    #One sample----
    qqnorm(x)
    qqline(x)
    cat(paste("are",xname,"normally distributed?\n If true, Return 1, If false, Return 0:"))
    ansnorm<- readline("answer:")
    if (ansnorm==1|ansnorm=="T") {
      t.test(x=x,mu=mu)
      cat("displaying mean plot with confidence interval and a horizontal line at mu")
      gplots::plotCI(mean(x),uiw = Rmisc::CI(x)[1]-Rmisc::CI(x)[2],xlab = NULL, ylab = yname)
      abline(h=mu)
    }else if (ansnorm==0|ansnorm=="F") {
      boxplot(x)
      wilcox.test(x=x,mu=mu)
    }
  }else if (paired) {
    #Paired----
    diff<- y-x
    qqnorm(diff)
    qqline(diff)
    cat("displaying QQ plot of sample differences, are they normally distributed?\n If true, Return 1, If false, Return 0:")
    ans<-readline("Answer:")
    if(yname==as.character(deparse(substitute(y)))){
      yname<-"sample difference"
    }
    if (ans==1|ans=="T") {
      print(t.test(x,y,paired = T))
      gplots::plotCI(mean(diff),uiw = Rmisc::CI(diff)[1]-Rmisc::CI(diff)[2],xlab = NULL, ylab = yname
                     # ,ylim = c(0,0)
      )
      abline(h=0)
    }else if (ans==0|ans=="F") {
      cat("Sign Test Results:")
      print(binom.test(length(diff[diff>0]),length(diff)))
      boxplot(diff, ylab = yname)
    }
  }else if (paired == F) {
    #Unpaired----
    qqnorm(x)
    qqline(x)
    readline("displaying the QQ plot of the first vector, return to display the next graph:")
    qqnorm(y)
    qqline(y)
    readline("displaying the QQ plot of the second vector, return to display the next graph:")
    cat("Are both variables normally distributed? n/ If true, return 1, if false, return 0")
    normal <- readline("Answer:")
    if (normal == 1|normal=="T") {
      #leven's test for equal variance
      #need to transform x,y into a categorized data frame
      df <- data.frame(cond=c(rep("x", times=length(x)),
                              rep("y", times=length(y))),
                       rating=c(x, y))
      df$cond <- as.factor(df$cond)
      l<-car::leveneTest(df$rating~df$cond)
      if (l$`Pr(>F)`[1]>=0.05) {
        #student's t
        print(t.test(x, y, var.equal = T))
        gplots::plotmeans(df$rating~df$cond)
      }else{
        #Welche's t
        print(t.test(x, y))
        gplots::plotmeans(df$rating~df$cond)
      }
    }else if (normal == 0|normal=="F"){
      #wilcox
      print(wilcox.test(x, y))
      boxplot(x,y)
    }else{
      return("Error:incorrect answer to question, please try again")
    }
  }
}


auto.t.formula <-   function(formula,data=NULL,xname=xnamet,yname=ynamet,...)
{
  #Format data ----
  if (is.null(data)) {
    f<-formula
    v<-all.vars(f)
    d<-v[1]
    ynamet<-v[2]
    xnamet<-v[3]
    print(f)
  }else if (is.null(data)==F) {
    as.character(data)
    v<-all.vars(formula)
    d<-as.character(deparse(substitute(data)))
    ynamet<-v[1]
    xnamet<-v[2]
    f<-paste(d,"$",ynamet,"~",d,"$",xnamet)
    f<-as.formula(f)
    print(f)
  }
  #variable and levels
  xcom <- paste(d,"$",xnamet)
  ycom <- paste(d,"$",ynamet)

  x<-eval(parse(text=xcom))
  y<-eval(parse(text=ycom))

  #diagnostics, tests & visualisations----
  cat1<-deparse(substitute(levels(x)[1]))
  cat2<-deparse(substitute(levels(x)[2]))
  rl1<-paste("showing the QQ plot of",ynamet,"when",xnamet,"is",levels(x)[1],", return to see the next graph")
  qqnorm(y[levels(x)==1])
  qqline(y[levels(x)==1])
  readline(rl1)

  rl2<-paste("showing the QQ plot of",ynamet,"when",xnamet,"is",levels(x)[2],", return to see the next graph")
  qqnorm(y[levels(x)==2])
  qqline(y[levels(x)==2])
  readline(rl2)

  cat("are both groups normally distributed, return 1 if True, 0 if False")
  ans<-readline("Answer:")

  if (ans==1|ans=="T") {
    l<-car::leveneTest(f)
    if (l$`Pr(>F)`[1]>=0.05) {
      #student's t
      t<-t.test(f,var.equal = T)
      gplots::plotmeans(f,xlab = xname,ylab = yname)
      print(l)
      print(t)
    }else{
      #Welche's t
      t<-t.test(f,var.equal = F)
      gplots::plotmeans(f,xlab = xname,ylab = yname)
      print(l)
      print(t)
    }
  }else if (ans==0|ans=="F") {
    w<-wilcox.test(f)
    boxplot(f,xlab = xname,ylab = yname)
    print(w)
  }
}
