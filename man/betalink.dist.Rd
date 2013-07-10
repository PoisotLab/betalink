\name{betalink.dist} 
\Rdversion{1.1} 
\alias{betalink.dist} 
\title{Distances between networks}
\description{Returns the distance matrix between all networks from a list}
\usage{ 
betalink.dist = function(W,...)
}

\arguments{ 
\item{W}{A list of webs}
\item{...}{Further instructions to be passed to \kbd{betalink}}
}

\value{
	A table with the components of network dissimilarity
}

\details{}

\references{---}

\examples{W <- generate.metaweb(10,10,60)
	real <- extract.localwebs(W,4,4,0.9)
	betalink.dist(real)}
