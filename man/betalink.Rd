\name{betalink} 
\Rdversion{1.1} 
\alias{betalink} 
\title{Beta-link between two networks}
\description{Sub-sample two local realizations of a single metaweb, with a given probability that the links will be lost in either of the realizations}
\usage{ 
betalink(w1,w2,bf=B01)
}

\arguments{ 
\item{w1}{A metaweb to serve as a template}
\item{w2}{Number of upper trophic level species in the realizations}
\item{bf}{The measure of beta-diversity to use, following the nomenclature of Kollef et al. (2003). See details.}
}

\value{
	\item{U}{The \eqn{\beta_U}{beta U} partition of beta-diversity, i.e. compositional turnover in the upper trophic level.}
	\item{L}{The \eqn{\beta_L}{beta L} partition of beta-diversity, i.e. compositional turnover in the lower trophic level.}
	\item{S}{The \eqn{\beta_S}{beta S} partition of beta-diversity, i.e. compositional turnover in the whole network.}
	\item{OS}{The \eqn{\beta_{OS}}{beta OS} partition of beta-diversity, i.e. turnover in links established between shared species.}
	\item{WN}{The \eqn{\beta_{WN}}{beta WN} partition of beta-diversity, i.e. turnover in links established in the whole network.}
	\item{ST}{The \eqn{\beta_{ST}}{beta ST} partition of beta-diversity, defined in eq. ZZ of Poisot et al. (wwww).}
	\item{contrib}{The contribution of species turnover to network turnover, expressed as a proportion.}
}

\details{The precise meaning of each partition is explained in Tbale YY of Poisot et al. (xxxx).

Measures of \eqn{\beta}{beta} diversity follow the nomenclature of Koleff et al. (2003). The 24 measures presented in Table 1 are implemented, and the code of each function is \kbd{B} followed by a two-digits code (corresponding to the row in Koleff et al. table). The default measure is \eqn{\beta_{sim}}{beta-sim}, i.e. \kbd{B22}.}

\references{Canard, E. F., Mouquet, N., Gravel, D., Krasnov, B. R., Poulin, R., & Mouillot, D. Neutrality predicts the spatial patterns of network beta diversity. in prep.

Koleff, P., Gaston, K. J., & Lennon, J. J. 2003. Measuring beta diversity for presence-absence data. Journal of Animal Ecology 72: 367-382.}

\examples{W <- generate.metaweb(10,10,60)
	real <- extract.localwebs(W,4,4,0.9)
	print(betalink(real$m1,real$m2))}