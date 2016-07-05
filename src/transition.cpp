#include <Rcpp.h>
#include <vector>
#include "gogame.hpp"


//' Obtains the transition of board configuration
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
  int n = ismove_vec.size();
  for (int i = 0; i < n; i++)
  {
    gg.Play(color_vec[i], x_vec[i], y_vec[i], ismove_vec[i]);
  }
  return gg.GetTransitions();
}



