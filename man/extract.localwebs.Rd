\name{extract.localwebs} 
\Rdversion{1.1} 
\alias{extract.localwebs} 
\title{Exctract two random realizations of a metaweb}
\description{Sub-sample two local realizations of a single metaweb, with a given probability that the links will be lost in either of the realizations}
\usage{ 
extract.localwebs(template,u,l,i)
}

\arguments{ 
\item{template}{A metaweb to serve as a template}
\item{u}{Number of upper trophic level species in the realizations}
\item{l}{Number of lower trophic level species in the realizations}
\item{i}{Probability that each link will be conserved when extracting the realization}
}

\value{
	\item{m1}{A list of \kbd{reps} networks generated using the \kbd{null.connectance} model}
	\item{m2}{A list of \kbd{reps} networks generated using the \kbd{null.connectance} model}
}

\examples{W <- generate.metaweb(10,10,60)
	image(W)
	real <- extract.localwebs(W,4,4,0.9)}
