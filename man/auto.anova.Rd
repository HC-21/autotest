\name{auto.anova}
\alias{auto.anova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
automatic selection, execution & visualization of one-way analysis of variance
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Conduct residual analysis of the anova model and select between anova or a kruskal-wallis ranked sum test, mean plots or boxplots visualizations will be plotted respectively
}
\usage{
auto.anova(formula, data = NULL, xname = substitute(x), yname = substitute(y))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
a formula in the form of y~x, where y is a continuous response variable and x is a categorical predictor variable
}
  \item{data}{
%%     ~~Describe \code{data} here~~
specifying the dataframe or matrix containing the variables of the formula}
  \item{xname}{
a character string to specify the name of the x variable and x axis
}
  \item{yname}{
%%     ~~Describe \code{yname} here~~
a character string to specify the name of the y variable and y axis
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
%%  ~~who you are~~
Tsz-Hong Chan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#specifying data argument
auto.anova(Sepal.Length~Species,data = iris,xname = "Species", yname = "Sepal Length")

#without specifying data argument
auto.anova(iris$Sepal.Length~iris$Species,xname = "Species", yname = "Sepal Length")

#return F or 0 during residual analysis to execute a kruskal-wallis ranked sum test anyways

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
