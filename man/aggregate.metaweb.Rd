\name{aggregate.metaweb} 
\Rdversion{1.1} 
\alias{aggregate.metaweb} 
\title{Aggregate different networks into a single metaweb}
\description{Take a list of networks, ad returns the adjacency matrix of the metaweb, and the probability to observe each link based on the number of times it occured when the two species were present.}
\usage{ 
aggregate.metaweb(W)
}

\arguments{ 
\item{W}{A list of matrices}
}

\value{
	\item{web}{The adjacency matrix of the metaweb}
	\item{template}{The link occurence probability matrix, needed by the \kbd{null.metaweb} function}
}

\details{Both matrices are named suing the rows and columns names of the matrices in the \kbd{W} list}

\references{---}

\examples{W <- generate.metaweb(10,10,60)
	real <- extract.localwebs(W,4,4,0.9)
	Meta <- aggregate.metaweb(real) 
	image(Meta$template)}
