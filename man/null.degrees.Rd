\name{null.degrees} 
\Rdversion{1.1} 
\alias{null.degrees} 
\title{Null model based on species' degrees}
\description{Returns a given number of pseudo-random network simulated from the marginal probability that two species interac.}
\usage{ 
null.degrees(template,reps)
}

\arguments{ 
\item{template}{A probability matrix}
\item{reps}{Number of networks to generate} 
}

\value{
	\item{}{A list of \kbd{reps} networks with the same rows and colums names than the template -- species keep identical names}
}
 
\details{---}

\references{Bascompte, J., Jordano, P., Melian, C. J., & Olesen, J. M. 2003. The nested assembly of plant-animal mutualistic networks. Proceedings of the National Academy of Sciences 100: 9383-9387.}

\examples{} 
