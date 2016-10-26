\name{block.matrix}
\alias{block.matrix}
\title{
Create a Block Matrix with Non-Uniform Block Sizes.
}
\description{
Creates a \code{matrix} with differing size blocks
}
\usage{
block.matrix(x = NULL, b = NULL, byrow = FALSE, dimnames = NULL)
}
\arguments{
  \item{x}{
A vector of numbers to fill each block
}
  \item{b}{
The size of the square matrices that lie on the diagonal of the matrix.
}
  \item{byrow}{
logical. If \code{FALSE} (the default) the matrix blocks are filled by columns, otherwise the matrix blocks are filled by rows.
}
  \item{dimnames}{
A dimnames attribute for the matrix: NULL or a vector of the same length of \code{b} giving the basis of row and column names.
}
}
\details{
Returns a \code{matrix} with blocks of sizes dictated by the \code{b} argument. Each block is filled with the same value. 
}
\value{
Matrix of values, where \code{x} is repeated in each block.
}
\author{
Guy J. Abel
}
\seealso{
\code{\link{ipf2.b}}, \code{\link{block.matrix}}
}
\examples{
block.matrix(x = 1:16, b = c(2,3,4,2), dimnames = LETTERS[1:4])
}
