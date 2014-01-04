
#include <Rccp.h>

using namespace Rcpp;

//[[Rcpp::export]]

NumericVector vector_add(NumericVector x, NumericVector y)
{
  // declare result vector
  NumericVector result(x.size());
  
  for(int i = 0; i<x.size(); ++i)
  {
    result[i] = x[i] + y[i];
  }
  return(result)
}

//' @title Vector_add2
//' @description Add two vectors
//' @details Adding two vecotrs with C++
//' @authors Michael Piccirilli
//' @aliases vector_add2
//' @param x A vector of numbers
//' @param y A vector of numbers
//' @return A vector containing the addition of x and y
//' @useDynLib Package1
// [[Rcpp::export]]
NumericVector vector_add2(NumericVector x, NumericVector y)
{
  return(x+y);
}


