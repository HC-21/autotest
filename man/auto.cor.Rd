\name{auto.cor}
\alias{auto.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatic selection, execution and visualization of correlation between paired samples
}
\description{
Plot scatter plots, QQ plots and select between Pearson's or Spearman's correlation coefficient tests
}
\usage{
auto.cor(x, y, xname = deparse(substitute(x)), yname = deparse(substitute(y)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a continuous vector
}
  \item{y}{
a continuous vector
}
  \item{xname}{
name vector x and x axia of scatter plot
}
  \item{yname}{
name vector y and y axia of scatter plot
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tsz-Hong Chan
}
\note{
#current version does not support formula arguments yet, please stay tuned to the package updates
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#
auto.cor(subset(iris,iris$Species=='setosa')$Sepal.Length,subset(iris,iris$Species=='setosa')$Sepal.Width,xname = "Sepal Length",yname = "Sepal Width")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
