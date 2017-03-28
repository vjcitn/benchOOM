\name{setContexts}
\alias{setContexts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
establish global context list for resolving arbitrary identifiers
}
\description{
establish global context list for resolving arbitrary identifiers
}
\usage{
setContexts(conlist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conlist}{
a list of named vectors or functions
}
}
\details{
the list defines an ordering on mappings from symbols to values
}
\value{
side effect is to define .AnnoContexts in .GlobalEnv
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
if (require(erma) && require(yriMulti) && require(EnsDb.Hsapiens.v75)) {
 data(banovichSE)
 c1 = sapply(c("BRCA1", "BRCA2", "ESRRA"), 
        function(x) range(genemodel(x)))
 c2 = rowRanges(banovichSE[1:4,])
 c3 = function(sym) {
       sel = select(EnsDb.Hsapiens.v75,
          keys=sym, keytype="GENEID", columns=
          c("SEQNAME", "GENESEQSTART", "GENESEQEND"))[1,]
       if (is.na(sel$GENESEQEND)) return(NA)
       GRanges(sel$SEQNAME, IRanges(sel$GENESEQSTART, sel$GENESEQEND))
       }
 setContexts(list(c1,c2,c3))
 .contexts
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
