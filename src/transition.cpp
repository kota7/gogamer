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
//' @export
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

  return gg.GetTransitions();
}



