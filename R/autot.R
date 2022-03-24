require(gplots)
require(FSA)
require(tidyverse)
require(car)
require(Rmisc)

auto.t <- function(x, ...) UseMethod("auto.t")

auto.t.default <- function(x, y = NULL,mu = 0, paired = FALSE,xname = as.character(deparse(substitute(x))),yname = as.character(deparse(substitute(y))),...)
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
