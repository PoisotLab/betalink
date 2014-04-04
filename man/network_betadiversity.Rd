\name{network_betadiversity}
\alias{network_betadiversity}
\title{Components of beta-diversity for a list of networks}
\usage{
network_betadiversity(W, ...)
}
\arguments{
  \item{W}{a list of networks}

  \item{...}{additional arguments to be passed to
  \link{betalink}}
}
\value{
WN The whole network beta-diversity

OS The overlapping species beta-diversity

S The species beta-diversity

ST The difference between WN and OS

contrib ST expressed as a proportion of WN
}
\description{
Given a list of networks, returns the pairwise
beta-diversity components
}

