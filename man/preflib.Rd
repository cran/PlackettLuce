% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preflib.R
\name{preflib}
\alias{preflib}
\alias{read.soc}
\alias{read.soi}
\alias{read.toc}
\alias{read.toi}
\alias{as.aggregated_rankings.preflib}
\title{Read Preflib Election Data Files}
\usage{
read.soc(file)

read.soi(file)

read.toc(file)

read.toi(file)

\method{as.aggregated_rankings}{preflib}(x, ...)
}
\arguments{
\item{file}{An election data file, conventionally with extension \code{.soc},
\code{.soi}, \code{.toc} or \code{.toi} according to data type.}

\item{x}{An object of class \code{"preflib"}.}

\item{...}{Additional arguments passed to \code{\link[=as.rankings]{as.rankings()}}: \code{freq},
\code{input} or \code{items} will be ignored with a warning as they are set
automatically.}
}
\value{
A data frame of class \code{"preflib"} with first column \code{Freq},
giving the frequency of the ranking in that row, and remaining columns
\code{Rank 1}, \ldots, \code{Rank r} giving the items ranked from first to
last place in that ranking. Ties are represented by vector elements in list
columns. The data frame has an attribute \code{"items"} giving the labels
corresponding to each item number.
}
\description{
Read orderings from \code{.soc}, \code{.soi}, \code{.toc} or \code{.toi} file types storing
election data as defined by
\href{https://www.preflib.org/}{\{PrefLib\}: A Library for Preferences}.
}
\details{
The file types supported are
\describe{
\item{.soc}{Strict Orders - Complete List}
\item{.soi}{Strict Orders - Incomplete List}
\item{.toc}{Orders with Ties - Complete List}
\item{.toi}{Orders with Ties - Incomplete List}
}
Note that the file types do not distinguish between types of incomplete
orderings, i.e. whether they are a complete ranking of a subset of items
(as supported by \code{\link[=PlackettLuce]{PlackettLuce()}}) or top-\eqn{n} rankings of \eqn{n} items
from the full set of items (not currently supported by \code{\link[=PlackettLuce]{PlackettLuce()}}).

The numerically coded orderings and their frequencies are read into a
data frame, storing the item names as an attribute. The
\code{as.aggregated_rankings} method converts these to an
\code{\link[=aggregate.rankings]{"aggregated_rankings"}} object with the items labelled
by the item names.

A Preflib file may be corrupt, in the sense that the ordered items do not
match the named items. In this case, the file can be read in as a data
frame (with a warning) using the corresponding \verb{read.*} function, but
\code{as.aggregated_rankings} will throw an error.
}
\note{
The Netflix and cities datasets used in the examples are from
Bennet and Lanning (2007) and Caragiannis et al (2017) respectively. These
data sets require a citation for re-use.
}
\examples{

# strict complete orderings of four films on Netflix
netflix <- read.soc(system.file("extdata", "netflix.soc",
                                package = "PlackettLuce"))
head(netflix)
attr(netflix, "items")

head(as.aggregated_rankings(netflix))

# strict incomplete orderings of 6 random cities from 36 in total
cities <- read.soi(system.file("extdata", "cities.soi",
                                package = "PlackettLuce"))

# complete orderings with ties of 30 skaters
skaters <- read.toc(system.file("extdata", "skaters.toc",
                                package = "PlackettLuce"))

# incomplete orderings with ties: most important qualities for success
# from 20 in total
qualities <- read.toi(system.file("extdata", "education_qualities.toi",
                      package = "PlackettLuce"))

# alternatively read from a url
# - can take a little while depending on speed of internet connection

\dontrun{
# incomplete orderings with ties: most important qualities for success
# from 20 in total
preflib <- "https://www.preflib.org/static/data/"
qualities2 <- read.toi(file.path(preflib, "education/00032-00000007.toi"))
all.equal(qualities, qualities2)
}
}
\references{
Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
\emph{Proceedings of Third International Conference on Algorithmic Decision
Theory (ADT 2013)}. Lecture Notes in Artificial Intelligence, Springer.

Caragiannis, I., Chatzigeorgiou, X, Krimpas, G. A., and Voudouris, A. A.
(2017) Optimizing positional scoring rules for rank aggregation.
In \emph{Proceedings of the 31st AAAI Conference on Artificial Intelligence}.

Bennett, J. and Lanning, S. (2007) The Netflix Prize.
\emph{Proceedings of The KDD Cup and Workshops}.
}
