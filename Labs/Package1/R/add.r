#required packages - devtools, roxygen2

#' @title add
#' @description Adds two numbers
#' @details Adds two numbers.  these can be vectors or matrices or otherwise compatible objects
#' @author Michael Piccirilli
#' @aliases add
#' @export add
#' @param x A vector of numbers
#' @param y A vector of numbers
#' @return The addition of \code{x} and \code{y}
#' @examples 
#' add(1,1)
#' add(1:3, 4:6)

#after this is all written, go to Console and type "document()" to export this and create the help file automatically
#then once you run it, go to the NAMESPACE table, see the function was added
#then go to the consule again, and run "check()"
#make sure Latek package is installed, otherwise PDFs won't be built
#then type "build()"

add <- function(x,y)
{
  return(x+y)
}

#' @title scatter
#' @description build scatter plots
#' @details Builds a ggplot2 scatterplot based on the data and x,y
#' @aliases scatter
#' @author Michael Piccirilli
#' @export scatter
#' @param data \code{\link{data.frame}} holding your data
#' @param x Name of column for the x-axis as a character
#' @param y Name of the column for the y-axis as a charater
#' @return A ggplot object that resembles a scatterplot
#' @import ggplot2
#' @examples 
#' scatter(iris, "Sepal.Length", "Sepal.Width")

#on the Description tab, use the Import: line to call ggplot2 for this one particular function
# if you use the Depend: line, then once the user uses the "scatter" function
#they will then be able to use ggplot2 for the remainder of their R session

scatter <- function(data, x,y)
{
  ggplot(data,aes_string(x=x, y=y)) + geom_point()
}

#now run document()
#and check()
#then if it passed, run build()
# then after you build, you can type in the console: load_all() which mimics the 
#process of installing the package, so you can run the functions in the package to test 
#your work.
