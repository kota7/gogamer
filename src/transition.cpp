#include <Rcpp.h>
#include <vector>
#include "gogame.hpp"



struct Move
{
  bool isMove;
  int location;
  int color;
};



Rcpp::IntegerMatrix GetTransition(
  std::vector<bool> isMoves, std::vector<int> locations,
  std::vector<int> colors, int boardsize);
std::vector<Transition> MovesToTransitions(
    std::vector<Move> moves, int boardsize);

//[[Rcpp::export]]
Rcpp::IntegerMatrix GetTransition(
  std::vector<bool> isMoves, std::vector<int> locations,
  std::vector<int> colors, int boardsize)
{
  // isMoves, locations, colors must have the same size
  int n = isMoves.size();
  std::vector<Move> moves(n);
  for (int i = 0; i < n; i++)
  {
    moves[i].isMove = isMoves[i];
    moves[i].location = locations[i];
    moves[i].color = colors[i];
  }
  Rcpp::IntegerMatrix out;
  return out;
}


std::vector<Transition> MovesToTransitions(
    std::vector<Move> moves, int boardsize)
{
  std::vector<Transition> out(1);
  return out;
}




