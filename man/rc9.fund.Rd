\name{rc9.fund}
\alias{rc9.fund}
\docType{data}
\title{
Fundamental Parameters for Rogers-Castro Migration Schedule
}
\description{
Set of fundamental parameters suggested by Rogers and Castro (1981).
}
\usage{data(rc9.fund)}
\details{
A \code{list} of the parameters required by the \code{\link{rc9}} function:
\deqn{ a_{1} = 0.02 }
\deqn{ \alpha_{1} = 0.1 }
\deqn{ a_{2} = 0.06 }
\deqn{ \alpha_{2} = 0.1 }
\deqn{ \mu_{2} = 20 }
\deqn{ \lambda_{2} = 0.4 }
\deqn{ c = 0.003 }
Can be copied and altered to illustrate the effect of parameters.
}
\references{
Rogers, A., and L. J. Castro. (1981). Model Migration Schedules. \emph{IIASA Research Report 81} RR-81-30
}
\examples{
# check format
str(rc9.fund)

# single age groups
x <- 1:100
rc9(x, rc9.fund)
plot(x, rc9(x, rc9.fund), type="l")

# alter to see the effect of mu2
p1 <- rc9.fund
p1$mu2 <- 30
plot(x, rc9(x, rc9.fund), type="l")
lines(x, rc9(x, p1), lty=2)
}
\keyword{datasets}
