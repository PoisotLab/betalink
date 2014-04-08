\name{prepare_networks}
\alias{prepare_networks}
\title{Prepare networks}
\usage{
prepare_networks(w, mode = "directed")
}
\arguments{
  \item{w}{A list of network matrices}

  \item{mode}{the mode of interaction (see
  \code{?graph.adjacency})}
}
\description{
Taking a list of networks as matrices, returns a list of
igraph objects
}
\examples{
data(clownfishes)
networks <- prepare_networks(clownfishes, "undirected")
networks[[1]]
}

