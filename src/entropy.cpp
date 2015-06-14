#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// Calculates Shannon entropy for a list of values. Because the log of zero is
// undefined, a fractional pseudocount is added to each value. This pseudocount
// is set to 1/ # of values.  
//
// @param xs A vector of values to calculate the entropy for
// @return The calculated entropy value
//[[Rcpp::export]]
NumericVector rcpp_entropy(NumericVector xs) {

  NumericVector p = clone(xs);
  float pseudocount = 1.0 / xs.size();
  double sumxs = 0.0;
  double entropy = 0.0;

  for(int i = 0; i < xs.size() ; i++) {
  	sumxs += xs[i];
  }
					  
  for(int i = 0; i < xs.size() ; i++) {
  	p[i] = (xs[i] + (1.0/xs.size())) / (sumxs + 1.0);
  }

  for(int i = 0; i < p.size() ; i++) {
	entropy += -1 * p[i] * log2(p[i]);
  }
  
  return wrap(entropy);
}
