\name{resolve}
\alias{resolve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
resolve an arbitrary key according to the context policy
}
\description{
Resolve an arbitrary key according to the context policy.
}
\usage{
resolve(sym)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym}{
string expected to map as a key to a value in the
global .AnnoContexts structure
}
}
\details{
first key's value is matched and value returned
}
\value{
NA if no match, otherwise whatever was mapped in the context
}
%\references{
%%% ~put references to the literature/web site here ~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
example(setContexts)
resolve("BRCA1")
resolve("cg13869341")
resolve("ENSG00000183696")
resolve("ENSG00000183696.9")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }% use one of  RShowDoc("KEYWORDS")
