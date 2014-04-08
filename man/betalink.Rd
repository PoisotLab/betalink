\name{betalink}
\alias{betalink}
\title{beta-diversity of two networks}
\usage{
betalink(w1, w2, bf = B01)
}
\arguments{
  \item{w1}{network 1 (as a matrix)}

  \item{w2}{network 2 (as a matrix)}

  \item{bf}{any function to measure beta-diversity between
  two sets}
}
\value{
a list with components S, OS, WN, ST, and contrib
}
\description{
measures the beta-diversity between two networks
}

