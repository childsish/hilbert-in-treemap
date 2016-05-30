\name{hilbertintreemap}
\alias{hilbertintreemap}
\title{Pseudo-Hilbert curves in tree maps}
\usage{

}
\description{
Plots pseudo-Hilbert curves in tree maps.
}
\examples{
par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i")
plot(0, type='n', xlim=c(0, 512), ylim=c(0, 512))
lines(hilbert(512))
}
