#include <Rcpp.h>
#include <vector>
#include "gogame.h"
#include "validate.h"

Rcpp::DataFrame get_transitions(
    unsigned int boardsize,
    std::vector<bool> ismove_vec,
    std::vector<unsigned int> x_vec, std::vector<unsigned int> y_vec,
    std::vector<unsigned int> color_vec);
Rcpp::ListOf<Rcpp::DataFrame> get_transitiontree(
    Rcpp::ListOf<Rcpp::DataFrame> data,
    std::vector< std::vector<unsigned int> > children, unsigned int boardsize,
    bool onebased);
void GetTransitionTreeRecursive(
    int currentNode, int parentPositionn, Gogame &gg,
    Rcpp::List &out,
    const Rcpp::ListOf<Rcpp::DataFrame> &data,
    const std::vector< std::vector<unsigned int> > &children);


//[[Rcpp::export]]
Rcpp::DataFrame get_transitions(
    unsigned int boardsize,
    std::vector<bool> ismove_vec,
    std::vector<unsigned int> x_vec, std::vector<unsigned int> y_vec,
    std::vector<unsigned int> color_vec)
{
  // Obtains the transition of board configuration
  //
  // Args:
  //   boardsize  : integer of the board size
  //   ismove_vec : logical vector indicating moves, as opposed to setup
  //   x_vec      : integer vector of x coordinate (horizontal)
  //   y_vec      : integer vector of y coordinate (vertical)
  //   color_vec  : integer vector of color vector
  //
  // Returns:
  //   data.frame
  //
  // Assumes:
  //   isMoves, locations, colors must have the same size

  Gogame gg(boardsize);

  unsigned int n = ismove_vec.size();
  for (unsigned int i = 0; i < n; i++)
    gg.Play(color_vec[i], x_vec[i], y_vec[i], ismove_vec[i]);

  return GetTransitionsAsDF(gg);
}


//[[Rcpp::export]]
Rcpp::ListOf<Rcpp::DataFrame> get_transitiontree(
    Rcpp::ListOf<Rcpp::DataFrame> data,
    std::vector< std::vector<unsigned int> > children, unsigned int boardsize,
    bool onebased = true)
{
  // Obtains the transition of board configuration with branching
  //
  // Args:
  //   data     : a list of data.frames. Each dataframe has columns named as
  //              'color', 'x', 'y', 'ismove'
  //   children : a vecrot of int vectors of children pointers
  //   boardsize: integer of boardsize
  //
  // Returns:
  //   a list of dataframes, same size as data.

  // make children indices zeo-based, if needed
  if (onebased) {
    for (unsigned int i = 0; i < children.size(); i++)
      for (unsigned int j = 0; j < children[i].size(); j++)
        children[i][j]--;
  }
  // check the validity of input
  bool check = ValidateChildren(children);
  if (!check) Rcpp::stop("Invalid 'children' input");

  Gogame gg(boardsize);
  Rcpp::List out(data.size());
  GetTransitionTreeRecursive(0, -1, gg, out, data, children);
  return out;
}


void GetTransitionTreeRecursive(
    int currentNode, int parentPosition, Gogame &gg,
    Rcpp::List &out,
    const Rcpp::ListOf<Rcpp::DataFrame> &data,
    const std::vector< std::vector<unsigned int> > &children)
{
  Rcpp::Rcout << "nodeid " << currentNode << "\n";
  Rcpp::IntegerVector color = data[currentNode]["color"];
  Rcpp::IntegerVector x = data[currentNode]["x"];
  Rcpp::IntegerVector y = data[currentNode]["y"];
  Rcpp::LogicalVector ismove = data[currentNode]["ismove"];

  // store the move number before playing
  // then get back to the position after going through all children
  int previous_movenumber = gg.GetMoveNumber();
  for (unsigned int i = 0; i < color.size(); i++)
    gg.Play(color[i], x[i], y[i], ismove[i]);
  //gg.Summary();

  // obtain the transition vector of this node
  // then compile the dataframe for this node and store in the out
  // while doing so, ignore elements up to parentPosition
  // since they belong to the parent node
  std::vector<Transition> tt = gg.GetTransitions();
  int n = tt.size() - parentPosition - 1;
  Rcpp::IntegerVector movevec(n);
  Rcpp::IntegerVector xvec(n);
  Rcpp::IntegerVector yvec(n);
  Rcpp::IntegerVector vvec(n);
  int j;
  for (unsigned int i = 0; i < n; i++)
  {
    j = parentPosition + i + 1;
    movevec[i] = tt[j].movenumber;
    xvec[i] = tt[j].x;
    yvec[i] = tt[j].y;
    vvec[i] = tt[j].value;
  }
  Rcpp::DataFrame tmp = Rcpp::DataFrame::create(
    Rcpp::Named("move") = movevec,
    Rcpp::Named("x") = xvec,
    Rcpp::Named("y") = yvec,
    Rcpp::Named("value") = vvec
  );
  out[currentNode] = tmp;


  // go to children node
  // children node should receive updated parentPosition,
  // which equals the size of tt
  for (unsigned int i = 0; i < children[currentNode].size(); i++)
  {
    GetTransitionTreeRecursive(
      children[currentNode][i], tt.size()-1, gg, out, data, children);
  }
  // revert the gogame position
  gg.GobackTo(previous_movenumber);
}



/***R
library(magrittr)
sgf <- readLines("tests/sample/joseki.sgf") %>% paste0(collapse = "")
tree <- gogamer:::make_sgftree(sgf)
parsed <- gogamer:::parse_sgfnode(tree$data)
compressor <- gogamer:::tree_compressor(tree$children)
parsed$moves$id2 <- compressor$indexmap[parsed$moves$id]
parsed$moves <- dplyr::arrange_(parsed$moves, ~id, ~dplyr::desc(ismove))
n2 <- length(compressor$parent)
moves <- lapply(seq(n2), function(i) dplyr::filter_(parsed$moves, ~id2 == i))
gogamer:::get_transitiontree(moves, compressor$children, 19, TRUE)
*/
