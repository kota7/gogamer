#include <Rcpp.h>
#include <vector>

struct Move
{
  bool isMove;
  int position;
  int color;
};

struct Transition
{
  int movenumber;
  int position;
  int color;
  bool add;
};


Rcpp::IntegerMatrix GetTransition();
std::vector<Transition> MovesToTransitions(
    std::vector<Move> moves, int boardsize);

//[[Rcpp::export]]
Rcpp::IntegerMatrix GetTransition()
{
  Rcpp::IntegerMatrix out;
  return out;
}


std::vector<Transition> MovesToTransitions(
    std::vector<Move> moves, int boardsize)
{
  std::vector<Transition> out(1);
  return out;
}




