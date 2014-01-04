#Assignment 2, Problem 2
theXmatrix <- matrix(c(-1, -2, 1, 4, 0, -6, -8, -7, 9), nrow=3,ncol=3)
theYmatrix <- matrix(1:9,3,3)
theBvector <- matrix(1:3,3)
theXmatrix %*% theYmatrix
theYmatrix%*%theXmatrix
theXmatrix%*%theBvector
t(theXmatrix)
solve(theXmatrix)
theQR <- qr(theXmatrix)
theQR$rank
