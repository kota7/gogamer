#include <Rcpp.h>
#include <vector>


class Gogame
{
  static const int EM = 0;
  static const int BL = 1;
  static const int WH = 2;
  static const int OB = 3;

  int boardsize;
  std::vector< std::vector<int> > board;

  int b_captured;
  int w_captured;

public:
  Gogame(int s);  // no default constractor. requires board size
  void clear();   // initialize board and prisoners

  void play(int color, int x, int y);

  // for debugging
  void summary();
};


Gogame::Gogame(int s)
{
  boardsize = s;

  // board's size is set to s + 2, so that
  // outer edge is included.
  // hence the board index is accessed by one-base coordinate
  board.resize(s + 2);
  for (int i = 0; i < board.size(); i++) board[i].resize(s + 2);

  // initialize board and set prisoners to zero
  clear();
}

void Gogame::clear()
{
  for (int i = 0; i < board.size(); i++)
  {
    board[0][i] = OB;
    board[board.size() - 1][i] = OB;
    board[i][0] = OB;
    board[i][board.size() - 1] = OB;
  }
  for (int i = 1; i < board.size() - 1; i++)
  {
    for (int j = 1; j < board[i].size() - 1; j++)
      board[j][i] = EM;
  }

  b_captured = 0;
  w_captured = 0;
}


void Gogame::summary()
{
  Rcpp::Rcout << "color definition\n";
  Rcpp::Rcout << " Empty = "<< EM << "\n";
  Rcpp::Rcout << " Black = "<< BL << "\n";
  Rcpp::Rcout << " White = "<< WH << "\n";
  Rcpp::Rcout << " OB    = "<< OB << "\n";


  Rcpp::Rcout << "\nboard configuration\n";
  for (int i = 0; i < board.size(); i++)
  {
    for (int j = 0; j < board[i].size(); j++)
      Rcpp::Rcout << board[j][i] << " ";
    Rcpp::Rcout << "\n";
  }
}


void Gogame::play(int color, int x, int y)
{
  // play a move by color at (x, y)

}



//' test for gogame class
//' @export
// [[Rcpp::export]]
void gogame_test()
{
  Gogame gg(19);
  gg.summary();
}

