\name{prepare_networks}
\alias{prepare_networks}
\title{Prepare networks}
\usage{
prepare_networks(w, directed = TRUE)
}
\arguments{
  \item{w}{A list of network matrices}

  \item{directed}{whether the edges are directed or not}
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

