\name{null.connectance} 
\Rdversion{1.1} 
\alias{null.connectance} 
\title{Null model based on connectance}
\description{Returns a given number of pseudo-random network simulated from number of links in the network.}
\usage{ 
null.connectance(template,reps)
}

\arguments{ 
\item{template}{A probability matrix}
\item{reps}{Number of networks to generate} 
}

\value{
	\item{}{A list of \kbd{reps} networks with the same rows and colums names than the template -- species keep identical names}
}
 
\details{---}

\references{Fortuna, M. A., & Bascompte, J. 2006. Habitat loss and the structure of plant-animal mutualistic networks. Ecology Letters 9: 281-286.}

\examples{} 