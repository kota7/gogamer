#include <Rcpp.h>
#include <vector>
#include "gogame.h"
#include "validate.h"

Rcpp::DataFrame get_transitions(
    unsigned int boardsize,
    std::vector<bool> ismove_vec,
    std::vector<unsigned int> x_vec, std::vector<unsigned int> y_vec,
    std::vector<unsigned int> color_vec);

Rcpp::DataFrame get_transitiontree(
    unsigned int boardsize, std::vector<bool> ismove_vec,
    std::vector<unsigned int> x_vec, std::vector<unsigned int> y_vec,
    std::vector<unsigned int> color_vec, std::vector<int> nodeid_vec,
    std::vector< std::vector<unsigned int> > children,  bool onebased);

void GetTransitionTreeRecursive(
    int currentNode, int parentPositionn, Gogame &gg,
    std::vector<Transition> &transitionStack, std::vector<int> &nodeidNew,
    const std::vector<bool> &ismove_vec,
    const std::vector<unsigned int> &x_vec,
    const std::vector<unsigned int> &y_vec,
    const std::vector<unsigned int> &color_vec,
    const std::vector<int> &nodeid_vec,
    const std::vector< std::vector<unsigned int> > &children,
    const std::vector<int> &nodeStart);


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
  {
    gg.Play(color_vec[i], x_vec[i], y_vec[i], ismove_vec[i]);
  }
  // compile data frame to return
  std::vector<Transition> tt = gg.GetTransitions();
  n = tt.size();
  // although move, x, y are all nonnegative,
  // they should be declared as int type, so that
  // they can be passed to R properly
  // R does not recognize unsigned int type, and
  // regard it as double
  std::vector<int> movevec(n);
  std::vector<int> xvec(n);
  std::vector<int> yvec(n);
  std::vector<int> vvec(n);

  for (unsigned int i = 0; i < n; i++)
  {
    movevec[i] = tt[i].movenumber;
    xvec[i] = tt[i].x;
    yvec[i] = tt[i].y;
    vvec[i] = tt[i].value;
  }
  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("move") = movevec,
    Rcpp::Named("x") = xvec,
    Rcpp::Named("y") = yvec,
    Rcpp::Named("value") = vvec
  );
  return out;
}


//[[Rcpp::export]]
Rcpp::DataFrame get_transitiontree(
    unsigned int boardsize, std::vector<bool> ismove_vec,
    std::vector<unsigned int> x_vec, std::vector<unsigned int> y_vec,
    std::vector<unsigned int> color_vec, std::vector<int> nodeid_vec,
    std::vector< std::vector<unsigned int> > children,  bool onebased)
{
  // Obtains the transition of board configuration with branching
  //
  // Args:
  //   boardsize    : board size
  //   ismove_vec   : vector indicates move or setup
  //   x_vec, y_vec : x and y coordinate
  //   color_vec    : vector of stone color
  //   nodeid_vec   : vector of nodeid
  //   children     : a vector of int vectors of children pointers
  //   onebased     : indicates one-based index or zero-based
  //
  // Returns:
  //   DataFrame with variables (x, y, value, move, nodeid)
  //
  // Assumes:
  //   nodeid_vec must be clustered
  //   ismove_vec, x_vec, y_vec, color_vec, nodeid_vec are all same size
  //   max(nodeid_vec) < size(children), since children of each node must be
  //   specified

  // make children and nodeid indices zeo-based, if needed
  if (onebased) {
    for (unsigned int i = 0; i < children.size(); i++)
      for (unsigned int j = 0; j < children[i].size(); j++)
        children[i][j]--;
    for (unsigned int i = 0; i < nodeid_vec.size(); i++)
      nodeid_vec[i]--;
  }

  // check validity of input
  // check children
  bool check = ValidateChildren(children);
  if (!check) Rcpp::stop("Invalid 'children' input");
  // check vector size
  unsigned int n = ismove_vec.size();
  if (x_vec.size() != n     || y_vec.size() != n ||
      color_vec.size() != n || nodeid_vec.size() != n) {
    Rcpp::stop("x, y, ismove, color, and nodeid vectors must be the same size");
  }
  // check nodeid
  // nodeid must be clustered, and max(nodeid_vec) < size(children)
  std::vector<bool> nodeFound(children.size());
  for (unsigned int i = 0; i < nodeid_vec.size(); i++)
  {
    if (nodeid_vec[i] >= children.size())
      Rcpp::stop("nodeid is larger than the size of children pointers");
    if (nodeFound[nodeid_vec[i]])
      Rcpp::stop("nodeid vector must be clustered");
    if (i > 0 && nodeid_vec[i] != nodeid_vec[i-1]) {
      // nodeid has been switched. hence this nodeid must not appear again
      nodeFound[nodeid_vec[i-1]] = true;
    }
  }

  Gogame gg(boardsize);
  // store the starting position of each node
  // nodeid_vec.size() indicates that node does not show up
  std::vector<int> nodeStart(children.size(), nodeid_vec.size());
  for (unsigned int i = 0; i < nodeid_vec.size(); i++)
  {
    if (i == 0) {
      nodeStart[nodeid_vec[i]] = i;
    } else if (nodeid_vec[i] != nodeid_vec[i-1]) {
      nodeStart[nodeid_vec[i]] = i;
    }
  }
  // keep the results in these two objects
  // later compile into DataFrame
  std::vector<Transition> transitionStack;
  std::vector<int> nodeidNew;
  GetTransitionTreeRecursive(
    0, -1, gg, transitionStack, nodeidNew,
    ismove_vec, x_vec, y_vec, color_vec, nodeid_vec, children, nodeStart);

  // compile output
  n = transitionStack.size();
  std::vector<int> x(n);
  std::vector<int> y(n);
  std::vector<int> move(n);
  std::vector<int> value(n);
  for (unsigned int i = 0; i < n; i++)
  {
    x[i]     = transitionStack[i].x;
    y[i]     = transitionStack[i].y;
    move[i]  = transitionStack[i].movenumber;
    value[i] = transitionStack[i].value;
    nodeidNew[i]++; // make it one-based
    //Rcpp::Rcout << x[i] << "," << y[i] << "," << move[i] << "," <<
    //  value[i] << "," << nodeidNew[i] << "\n";
  }

  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("move") = move,
    Rcpp::Named("x") = x,
    Rcpp::Named("y") = y,
    Rcpp::Named("value") = value,
    Rcpp::Named("nodeid") = nodeidNew
  );
  return out;
}


void GetTransitionTreeRecursive(
    int currentNode, int parentPosition, Gogame &gg,
    std::vector<Transition> &transitionStack, std::vector<int> &nodeidNew,
    const std::vector<bool> &ismove_vec,
    const std::vector<unsigned int> &x_vec,
    const std::vector<unsigned int> &y_vec,
    const std::vector<unsigned int> &color_vec,
    const std::vector<int> &nodeid_vec,
    const std::vector< std::vector<unsigned int> > &children,
    const std::vector<int> &nodeStart)
{
  // store the move number before playing
  // then get back to the position after going through all children
  int previous_movenumber = gg.GetMoveNumber();


  // find the position where current node starts
  unsigned int i = nodeStart[currentNode];
  //Rcpp::Rcout << "\nnode " << currentNode << " start at " << i << "\n";
  // apply moves until the nodeid switches
  while (i < nodeid_vec.size())
  {
    //Rcpp::Rcout << "(" << color_vec[i] << "," << x_vec[i] << "," <<
    //  y_vec[i] << "," << ismove_vec[i] << "," << nodeid_vec[i] << ") ";
    gg.Play(color_vec[i], x_vec[i], y_vec[i], ismove_vec[i]);
    // this node ends when next node does not exist, or nodeid switches
    if (i + 1 == nodeid_vec.size()) break;
    if (nodeid_vec[i+1] != nodeid_vec[i]) break;
    i++;
  }
  //gg.Summary();

  // obtain the transition vector of this node
  // then add to the end of transitionStack
  std::vector<Transition> tt = gg.GetTransitions();
  int n = tt.size() - parentPosition - 1;
  transitionStack.reserve(transitionStack.size() + n);
  transitionStack.insert(transitionStack.end(),
                         tt.begin() + parentPosition + 1, tt.end());
  // also update the nodeidNew vector
  nodeidNew.resize(nodeidNew.size() + n, currentNode);
  parentPosition = tt.size() - 1;


  // go to children node
  // children node should receive updated parentPosition,
  // which equals the size of tt
  for (unsigned int i = 0; i < children[currentNode].size(); i++)
  {
    GetTransitionTreeRecursive(
      children[currentNode][i], parentPosition, gg, transitionStack, nodeidNew,
      ismove_vec, x_vec, y_vec, color_vec, nodeid_vec, children, nodeStart);
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
parsed$moves$nodeid <- compressor$indexmap[parsed$moves$id]
parsed$moves <- dplyr::arrange_(parsed$moves, ~id, ~dplyr::desc(ismove))
gogamer:::get_transitiontree(
  19, parsed$moves$ismove, parsed$moves$x, parsed$moves$y, parsed$moves$color,
  parsed$moves$nodeid, compressor$children, TRUE)
*/
