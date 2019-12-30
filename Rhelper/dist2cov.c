#include <R.h>
#include <Rinternals.h>
#include <R_ext/Arith.h>
#include <Rmath.h>
#include <float.h>
#include <math.h>

#define min(a,b) (a<b?a:b)

SEXP dist2covexp(SEXP x, SEXP dim, SEXP theta)
{
  // changes the matrix 'x' in place. WARNING!
  
  R_len_t i, j, d1 = INTEGER(dim)[0], d2 = INTEGER(dim)[1];
  double *rx = REAL(x), *rtheta = REAL(theta);
 
  for(i = 0; i < d1; i++) {
    for(j = 0; j < d2; j++) {
      if(rx[i + d1*j] == 0.0)
	rx[i + d1*j] = rtheta[1] + rtheta[2];
      else
	rx[i + d1*j] = rtheta[1] * exp(-rx[i + d1*j] / rtheta[0]);
    }
  }
  return(R_NilValue);
}
