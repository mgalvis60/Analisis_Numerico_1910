tril1 <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = FALSE)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
  }
  return(M)
}

tril1M <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = TRUE)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
  }
  return(M)
}

A=matrix(c(4,2,1,3,1,2,4,5,6),nrow=3)
A

tril1(A, k=0)
tril1M(A, k=0)
