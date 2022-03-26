\name{auto.t}
\alias{auto.t}
\title{autometic selection, execution and visualization of t test and its parametric alternatives}
\usage{
auto.t(x,...)

## Default S3 method:
auto.t(x, y = NULL, mu = 0, paired = FALSE, xname = as.character(deparse(substitute(x))), yname = as.character(deparse(substitute(y))), ...)

## S3 method for class 'formula'
auto.t(formula, data, xname=xnamet,yname=ynamet,...)
}
\arguments{
  \item{x}{a numeric vector}

  \item{y}{an optional numeric vector}

  \item{mu}{a numeric value for one sample t test}

  \item{paired}{a logical indicator for if the vectors are paired for paired t-test or sign test}

  \item{formula}{a formula in the for of y~x, where y and x are numeric vectors}

  \item{data}{to specify the dataframe that contains the vectors in the formula}

  \item{xname}{a character string specify the name of the x variable and axis label}

  \item{yname}{a character string specify the name of the y variable and axis label}
}
\author{
Tsz Hong Chan
}
\description{
automatically select,conduct and visualize the results of t-tests and its non-parametric alternatives(e.g. Wilcox's rank sum test and sign test).

Return 1 or 0 to the answer prompts for an interactive test for normality and select the appropriate inferential tests.

For one sample analysis , one sample t-test or Wilcoxon signed rank test will be executed

For paired data, paired t-test or sign test will be executed

For unpaired data, Wilcox ranked sum test or t-tests will be used.

To maximize the power of t-tests, Leven's test for equal variance will be automatically conducted, Student's t-test will be selected if the samples are of equal variance, Welch t-test will be selected otherwise.
}
\examples{
#one sample t test or Wilcoxon signed rank test
auto.t(sleep$extra[sleep$group==1],yname = "change in hour of sleep")

#paired t test or sign test
sleep2 <- reshape(sleep, direction = "wide",
                  idvar = "ID", timevar = "group")

auto.t(sleep2$extra.1,sleep2$extra.2,paired = T,yname = "difference in effect on hour of sleep")

#two sample t test or wilcoxon ranked sum test
auto.t(extra ~ group, data = sleep,yname = "change in hour of sleep")
#or
auto.t(sleep$extra[sleep$group==1],sleep$extra[sleep$group==2],xname = "group",yname = "change in hour of sleep")
}
