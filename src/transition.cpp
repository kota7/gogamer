#include <Rcpp.h>
#include <vector>
#include "gogame.h"


//' Obtains the transition of board configuration
//' @param boardsize integer of the board size
//' @param ismove_vec logical vector indicating moves, as opposed to setup
//' @param x_vec integer vector of x coordinate (horizontal)
//' @param y_vec integer vector of y coordinate (vertical)
//' @param color_vec integer vector of color vector
//' @return \code{data.frame}
//' @keywords internal
//[[Rcpp::export]]
Rcpp::DataFrame get_transitions(
  unsigned int boardsize,
  std::vector<bool> ismove_vec,
  std::vector<unsigned int> x_vec, std::vector<unsigned int> y_vec,
  std::vector<unsigned int> color_vec)
{
  Gogame gg(boardsize);

  // isMoves, locations, colors must have the same size
  unsigned int n = ismove_vec.size();
  for (unsigned int i = 0; i < n; i++)
    gg.Play(color_vec[i], x_vec[i], y_vec[i], ismove_vec[i]);

  return GetTransitions(gg);
}


//' Obtains the transition of board configuration
//' @param data   A list of data.frames, each contains
//' @keywords internal
//[[Rcpp::export]]
Rcpp::List get_transitiontree(
    Rcpp::ListOf<Rcpp::DataFrame> data,
    std::vector< std::vector<int> > children, unsigned int boardsize)
{
  Gogame gg(boardsize);


  Rcpp::List out;
  out["test"] = data[0]["color"];
  return out;
}




/***R
sgf <- readLines("tests/sample/joseki.sgf") %>% paste0(collapse = "")
tree <- gogamer:::make_sgftree(sgf)
parsed <- gogamer:::parse_sgfnode(tree$data)
compressor <- gogamer:::tree_compressor(tree$children)
moves <- lapply(compressor$indices, function(i) parsed$moves[i]) %>%
  lapply(dplyr::bind_rows)
children <- compressor$children
gogamer:::get_transitiontree(moves, children, 19)
*/