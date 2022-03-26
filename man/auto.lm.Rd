\name{auto.lm}
\alias{auto.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
automatic linear regression
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
conduct linear regression, residual analysis and visualze the result with one function
}
\usage{
auto.lm(formula, data = NULL, xname = substitute(x), yname = substitute(y))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
formula with two continuous variables in the format of y~x where y is the response variable and x is the predictor variable
}
  \item{data}{
specifying the dataframe containing the variables of the formula
}
  \item{xname}{
a character string to specify the name of the x variable and x axis
}
  \item{yname}{
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

auto.lm(Sepal.Length~Sepal.Width,subset(iris,iris$Species=="setosa"),xname = "Sepal Width",yname = "Sepal length")

#note that using subset() will distort the extraction of variable name and manually correcting plot axis name with xname and yname arguments is highly recommended

#without specifying data argument
auto.lm(Orange$circumference~Orange$age)
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
