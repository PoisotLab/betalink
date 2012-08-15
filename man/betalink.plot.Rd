\name{betalink.plot} 
\Rdversion{1.1} 
\alias{betalink.plot} 
\title{Beta-gram of two networks}
\description{Draw a beta-gram of two networks}
\usage{ 
betalink.plot(m1,m2,by.unique=TRUE)
}

\arguments{ 
\item{m1}{First web}
\item{m2}{Second web}
\item{by.unique}{Boolean telling whether to plot the network so as to reflect beta-diversity (\kbd{TRUE}) or nestedness (\kbd{FALSE}).}
}

\value{
	Returns a plot
}

\details{The beta-gram is a way to visualize beta-diversity of networks, introduced in Poisot et al. (xxxx). Species unique to network 1 are coloured in green, species unique to network 2 are colord in blue. Interactions between shared species are in grey if they are conserved between the two webs, and in orange otherwise. The proposition of each colors allows a rapid inspection of which events (changes in links, changes in specieos composition, both) are responsible for differences between the networks. The option \kbd{by.unique=FALSE} sorts the network to relfect its nestedness, but the color code is conserved.}

\references{---}

\examples{W <- generate.metaweb(10,10,60)
	real <- extract.localwebs(W,4,4,0.9)
	betalink.plot(real$m1,real$m2)}
