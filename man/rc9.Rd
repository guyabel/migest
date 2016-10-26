\name{rc9}
\alias{rc9}
\title{
Rogers-Castro Migration Schedule
}
\description{
Provides the Rogers-Castro schedule,
\deqn{ M(x) = a_{1} \exp[-\alpha_{1}x] + a_{2} \exp [ \alpha_{2}(x-\mu_{2})- \exp [ \lambda_{2}(x-\mu_{2}) ] ] +c}
for a given set of parameters and ages.
}
\usage{
rc9(x, param = NULL, scaled = TRUE)
}
\arguments{
  \item{x}{
A vector of numbers
}
  \item{param}{
A list with names match those of the schedule
}
  \item{scaled}{
Scale estimates to sum to one across all ages, x.
}
}
\details{
Returns the M(x) values from the Rogers-Castro schedule of age specific migration rate. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The \code{param} argument must be a list with correct names for each parameter. See for example the \code{\link{rc9.fund}} object for an example of the naming convention.
}
\value{
Vector of values of M(x)
}
\references{
Rogers, A., and L. J. Castro. (1981). Model Migration Schedules. \emph{IIASA Research Report 81} RR-81-30
}
\author{
Guy J. Abel
}
\seealso{
\code{\link{rc9.fund}}
}
\examples{
# single age groups
x <- 1:100
m <- rc9(x, param = rc9.fund)
plot(x, m, type="l")

# 5 year age groups
m <- rc9(x, param = rc9.fund)
plot(x, m, type="l")
}
