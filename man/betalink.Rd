% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betalink.r
\name{betalink}
\alias{betalink}
\title{beta-diversity of two networks}
\usage{
betalink(n1, n2, bf = B01)
}
\arguments{
\item{n1}{network 1 (as an igraph object)}

\item{n2}{network 2 (as an igraph object)}

\item{bf}{any function to measure beta-diversity between two sets}
}
\value{
a list with components S, OS, WN, and ST. While interpreting
the output, it is important to consider that ST is strongly constrained by
the values of S (the species composition dissimilarity). ST is only really
meaningful when the values of S are "intermediate"; a good example is when
the networks have been sampled along a gradient, and a more or less equal
proportion of the species show turnover from one step to the next. In the
situations where S is either really high or really low, the values of ST
are constrained and should no be given importance. The values of OS and WN,
and how they relate to S, have more informative value.
}
\description{
measures the beta-diversity between two networks
}

