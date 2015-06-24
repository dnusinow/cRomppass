#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Normalizes a vector of WD scores for a given normalization factor
//
// @param xs A vector of unnormalized WD scores
// @param norm.factor A number between 0 and 1 corresponding to the quantile of the xs to normalize to. Defaults in the comppass function to the 98th percentile.
// @return A vector of WD scores divided by whatever score is the percentile specified by norm.factor
//[[Rcpp::export]]
NumericVector normalize_wd(NumericVector xs, double norm_factor) {
  std::vector<double> fxs;		// Filtered xs
  NumericVector::iterator it;
  std::vector<double>::iterator vit;

  for(it = xs.begin() ; it != xs.end() ; ++it) {
	if(! (NumericVector::is_na(*it) || *it == R_NaN)) {
	  fxs.push_back(*it);
	}
  }

  
  // Calculate the quantiles for the entire set of wd scores. Just use R.
  Rcpp::Function quantile("quantile");
  NumericVector qs = quantile(wrap(fxs), norm_factor);

  std::vector<double> nwds;		// Normalized wd's

  for(it = xs.begin() ; it != xs.end() ; ++it) {
	nwds.push_back(*it / qs[0]);
  }
  
  return wrap(nwds);
}
