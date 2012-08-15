\name{null.from_template} 
\Rdversion{1.1} 
\alias{null.from_template} 
\title{Null model based on the observed metaweb}
\description{Returns a given number of pseudo-random network simulated from the probability to observe each link in the realizations of the metaweb.}
\usage{ 
null.from_template(w,template,reps)
}

\arguments{ 
\item{w}{The network to replicate (which can be a realization)}
\item{template}{A probability matrix (which can be the metaweb)}
\item{reps}{Number of networks to generate}
}

\value{
	\item{I}{A list of \kbd{reps} networks generated using the \kbd{null.connectance} model}
	\item{II}{A list of \kbd{reps} networks generated using the \kbd{null.degrees} model}
	\item{MW}{A list of \kbd{reps} networks generated using the \kbd{null.metaweb} model}
}
 
\details{---}

\references{---}

\examples{}