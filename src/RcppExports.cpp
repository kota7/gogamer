// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// PruneSgf
std::string PruneSgf(std::string x, bool keep_first);
RcppExport SEXP sgf_PruneSgf(SEXP xSEXP, SEXP keep_firstSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type keep_first(keep_firstSEXP);
    __result = Rcpp::wrap(PruneSgf(x, keep_first));
    return __result;
END_RCPP
}
// GetTransition
Rcpp::IntegerMatrix GetTransition();
RcppExport SEXP sgf_GetTransition() {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    __result = Rcpp::wrap(GetTransition());
    return __result;
END_RCPP
}
