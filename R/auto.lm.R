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

  m<-lm(formula)
  par(mfrow=c(2,2))
  plot(m)
  par(mfrow=c(1,1))
  readline("displaying diagnostic plot, return to display scatter plot with linear relationship plot:")
  plot(formula,xlab=xname,ylab = yname)+abline(lm(formula))
  print(summary(m))
}
