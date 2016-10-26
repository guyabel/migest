\name{block.sum}
\alias{block.sum}
\title{
Sum of a Block Within a Matrix
}
\description{
Returns of a sum of a block within a \code{matrix}. This function is predominantly intended to be used within the \code{\link{ipf2.b}} routine.
}
\usage{
block.sum(block = NULL, m = NULL, bid = NULL)
}
\arguments{
  \item{block}{
The block to summed. To be matched against the matrix in \code{bid}.
}
  \item{m}{
The complete matrix of values.
}
  \item{bid}{
A matrix of the same dimensions of \code{m} used to identify blocks.
}
}
\details{
Returns a numeric value of the sum of a single block.
}
\value{
Matrix of values, where \code{x} is repeated in each block.
}
\author{
Guy J. Abel
}
\seealso{
\code{\link{ipf2.b}}
}
\examples{
m <- matrix(100:220, nrow = 11, ncol = 11)
b <- block.matrix(1:16, c(2,3,4,2))
block.sum(1, m, b)
block.sum(4, m, b)
block.sum(16, m, b)
}
