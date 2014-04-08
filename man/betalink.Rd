\name{betalink}
\alias{betalink}
\title{beta-diversity of two networks}
\usage{
betalink(n1, n2, bf = B01)
}
\arguments{
  \item{n1}{network 1 (as an igraph object)}

  \item{n2}{network 2 (as an igraph object)}

  \item{bf}{any function to measure beta-diversity between
  two sets}
}
\value{
a list with components S, OS, WN, ST, and contrib
}
\description{
measures the beta-diversity between two networks
}

