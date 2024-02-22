############################################################################
#         R FUNCTIONS FOR MATRIX POWERS
#
#         Copied by Jeffrey S. Rosenthal, probability.ca, 2009, from:
#         https://stat.ethz.ch/pipermail/r-help/2007-May/131330.html
#
# The base package of the statistical software R does not seem to include
# built-in functions for computing powers of matrices.  So, I provide
# them here, copied from the above-mentioned web page.
############################################################################
# SOURCE: https://web.archive.org/web/20240222012202/http://www.probability.ca/jeff/comp/matpow.R
# ZB downloaded and web archived on 2024-02-21.
############################################################################

# a simple version, by Ron E. VanNimwegen:
matpow <- function(mat,n){
  ans <- mat
  for ( i in 1:(n-1)){
    ans <- mat %*% ans
  }
  return(ans)
}

# a faster version, by Alberto Monteiro
matpowfast <- function(mat, n)
{
  if (n == 1) return(mat)
  result <- diag(1, ncol(mat))
  while (n > 0) {
    if (n %% 2 != 0) {
      result <- result %*% mat
      n <- n - 1
    }
    mat <- mat %*% mat
    n <- n / 2
  }
  return(result)
}

# example:
# mymat = matrix( c(1,0.98,0.98,1), nrow=2 )
# print( matpow(mymat,5) );
# print( matpowfast(mymat,5) );
