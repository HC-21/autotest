\name{auto.chisq}
\alias{auto.chisq}
\title{autometic selection, execution and visualization of Chi square test or Kruskal- Wallis test
}
\description{
Autometically select and percform a Pearson's Chi-square test or a Fisher's Exact test
}
\usage{
auto.chisq(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a categorical vector or a contingency table data frame
}
  \item{y}{
  an optional categorical vector
  }
}
\details{
test selection is dependent on expected value, if more than 80 percent of cells exceeds the expected value of 5, chi square test is used
}
\value{
The expected value, observed value and test result will be returned
}

\author{
Tsz Hong Chan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

##Borrowing tests from Rbase chisq.test

##Chi square test with contingency table dataframe
## From Agresti(2007) p.39
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
auto.chisq(M)

##test with two categorical vectors
iris$PetalWidthCategory<-ifelse(iris$Petal.Width>mean(iris$Petal.Width),"long","short")

auto.chisq(iris$Species,iris$PetalWidthCategory)

##For contingency table with less than more than 20 percent of cells ##that has less than an expected value of 5

cow<-matrix(c(15,6,7,322),nr=2,)
auto.chisq(cow)



}
