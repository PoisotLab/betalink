\name{generate.metaweb} 
\Rdversion{1.1} 
\alias{generate.metaweb} 
\title{Generate a random metaweb}
\description{Returns a random metaweb of size \kbd{U} by \kbd{L}, with a total of \kbd{k} links. The network is filled so that no species have a degree of 0.}
\usage{ 
generate.metaweb(U,L,k)
}

\arguments{ 
\item{U}{Number of upper-trophic levels organisms}
\item{L}{Number of lower-trophic levels organisms}
\item{k}{Number of links to put in the network}
}

\value{
	\item{}{A list of \kbd{reps} networks generated using the \kbd{null.connectance} model}
}
 
\details{For extremely low values of \kbd{k}, this function may need a long time to assign one link to each species. Species in \kbd{W} are named.}

\examples{W <- generate.metaweb(10,10,60)
	image(W)}