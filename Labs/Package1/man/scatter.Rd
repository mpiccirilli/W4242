\name{scatter}
\alias{scatter}
\title{scatter}
\usage{
  scatter(data, x, y)
}
\arguments{
  \item{data}{\code{\link{data.frame}} holding your data}

  \item{x}{Name of column for the x-axis as a character}

  \item{y}{Name of the column for the y-axis as a charater}
}
\value{
  A ggplot object that resembles a scatterplot
}
\description{
  build scatter plots
}
\details{
  Builds a ggplot2 scatterplot based on the data and x,y
}
\examples{
scatter(iris, "Sepal.Length", "Sepal.Width")
}
\author{
  Michael Piccirilli
}

