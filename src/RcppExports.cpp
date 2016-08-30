// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// gogame_test
void gogame_test(int m);
RcppExport SEXP gogamer_gogame_test(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    gogame_test(m);
    return R_NilValue;
END_RCPP
}
// get_movenumber
std::vector<int> get_movenumber(std::vector<bool> hasmove, std::vector< std::vector<unsigned int> > children, bool onebased);
RcppExport SEXP gogamer_get_movenumber(SEXP hasmoveSEXP, SEXP childrenSEXP, SEXP onebasedSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<bool> >::type hasmove(hasmoveSEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<unsigned int> > >::type children(childrenSEXP);
    Rcpp::traits::input_parameter< bool >::type onebased(onebasedSEXP);
    __result = Rcpp::wrap(get_movenumber(hasmove, children, onebased));
    return __result;
END_RCPP
}
// prune_sgf
std::string prune_sgf(std::string sgf, bool keepfirst);
RcppExport SEXP gogamer_prune_sgf(SEXP sgfSEXP, SEXP keepfirstSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::string >::type sgf(sgfSEXP);
    Rcpp::traits::input_parameter< bool >::type keepfirst(keepfirstSEXP);
    __result = Rcpp::wrap(prune_sgf(sgf, keepfirst));
    return __result;
END_RCPP
}
// sgftree_test
void sgftree_test(std::string sgf);
RcppExport SEXP gogamer_sgftree_test(SEXP sgfSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::string >::type sgf(sgfSEXP);
    sgftree_test(sgf);
    return R_NilValue;
END_RCPP
}
// make_sgftree
Rcpp::List make_sgftree(std::string sgf, bool bynode);
RcppExport SEXP gogamer_make_sgftree(SEXP sgfSEXP, SEXP bynodeSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::string >::type sgf(sgfSEXP);
    Rcpp::traits::input_parameter< bool >::type bynode(bynodeSEXP);
    __result = Rcpp::wrap(make_sgftree(sgf, bynode));
    return __result;
END_RCPP
}
// get_transitions
Rcpp::DataFrame get_transitions(unsigned int boardsize, std::vector<bool> ismove_vec, std::vector<unsigned int> x_vec, std::vector<unsigned int> y_vec, std::vector<unsigned int> color_vec);
RcppExport SEXP gogamer_get_transitions(SEXP boardsizeSEXP, SEXP ismove_vecSEXP, SEXP x_vecSEXP, SEXP y_vecSEXP, SEXP color_vecSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< unsigned int >::type boardsize(boardsizeSEXP);
    Rcpp::traits::input_parameter< std::vector<bool> >::type ismove_vec(ismove_vecSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int> >::type x_vec(x_vecSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int> >::type y_vec(y_vecSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int> >::type color_vec(color_vecSEXP);
    __result = Rcpp::wrap(get_transitions(boardsize, ismove_vec, x_vec, y_vec, color_vec));
    return __result;
END_RCPP
}
// get_transitiontree
Rcpp::List get_transitiontree(Rcpp::ListOf<Rcpp::DataFrame> data, std::vector< std::vector<int> > children, unsigned int boardsize);
RcppExport SEXP gogamer_get_transitiontree(SEXP dataSEXP, SEXP childrenSEXP, SEXP boardsizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::ListOf<Rcpp::DataFrame> >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<int> > >::type children(childrenSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type boardsize(boardsizeSEXP);
    __result = Rcpp::wrap(get_transitiontree(data, children, boardsize));
    return __result;
END_RCPP
}
// tree_compressor
Rcpp::List tree_compressor(std::vector< std::vector<unsigned int> > children, bool onebased);
RcppExport SEXP gogamer_tree_compressor(SEXP childrenSEXP, SEXP onebasedSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<unsigned int> > >::type children(childrenSEXP);
    Rcpp::traits::input_parameter< bool >::type onebased(onebasedSEXP);
    __result = Rcpp::wrap(tree_compressor(children, onebased));
    return __result;
END_RCPP
}
