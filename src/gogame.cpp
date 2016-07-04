#include <Rcpp.h>
#include <vector>
#include <queue>


struct Point
{
  // a point on the board
  // used in the queue for checking liberties,
  // and storing captured stones
  unsigned int x;
  unsigned int y;

  Point() {}
  Point(unsigned int a, unsigned int b)
  {
    x = a;
    y = b;
  }
};



class Gogame
{
  static const int EM = 0;
  static const int BL = 1;
  static const int WH = 2;
  static const int OB = 3;

  int boardsize;
  std::vector< std::vector<int> > board;

  unsigned int b_captured;
  unsigned int w_captured;

public:
  Gogame(unsigned int s);  // no default constractor. requires board size
  void clear();   // initialize board and prisoners

  void play(int color, unsigned int x, unsigned int y);
  bool check_liberty(Point p,
                     std::queue<Point> &checklist,
                     std::vector< std::vector<bool> > &checked,
                     std::vector< std::vector<bool> > &alive);


  // for debugging
  void summary();
};


Gogame::Gogame(unsigned int s)
{
  boardsize = s;

  // board's size is set to s + 2, so that
  // outer edge is included.
  // hence the board index is accessed by one-base coordinate
  board.resize(s + 2);
  for (unsigned int i = 0; i < board.size(); i++) board[i].resize(s + 2);

  // initialize board and set prisoners to zero
  clear();
}

void Gogame::clear()
{
  for (unsigned int i = 0; i < board.size(); i++)
  {
    board[0][i] = OB;
    board[board.size() - 1][i] = OB;
    board[i][0] = OB;
    board[i][board.size() - 1] = OB;
  }
  for (unsigned int i = 1; i < board.size() - 1; i++)
  {
    for (unsigned int j = 1; j < board[i].size() - 1; j++)
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
  for (unsigned int i = 0; i < board.size(); i++)
  {
    for (unsigned int j = 0; j < board[i].size(); j++)
      Rcpp::Rcout << board[j][i] << " ";
    Rcpp::Rcout << "\n";
  }
}


void Gogame::play(int color, unsigned int x, unsigned int y)
{
  // play a move by color at (x, y)

  // TODO:
  //   check if legal move
  //     suicide move
  //     ko
  // for now, assume moves are valid

  // put the stone temporarily
  board[y][x] = color;


  // enter four adjacent points to the checklist
  // if they are opponent color
  int opponent_color;
  if (color == BL) {
    opponent_color = WH;
  } else {
    opponent_color = BL;
  }


  // check if any opponent stone becomes captured due to this play
  // define input for check_liberty routine
  std::vector< std::vector<bool> > checked;
  std::vector< std::vector<bool> > alive;
  std::queue<Point> checklist;
  std::vector<Point> captured;

  // loop for adjacent points of the current point
  unsigned int xx;
  unsigned int yy;
  for (int k = 0; k < 2; k++)
  {
    for (int l = 0; l < 2; l++)
    {
      // for each loop, checklist should be empty
      xx = x + 2*k - 1;
      yy = y + 2*l - 1;
      if (board[yy][xx] == opponent_color)
        check_liberty(Point(x, y + 1), checklist, checked, alive);
    }
  }



}


bool Gogame::check_liberty(Point p,
                           std::queue<Point> &checklist,
                           std::vector< std::vector<bool> > &checked,
                           std::vector< std::vector<bool> > &alive)
{
  // check if the point p has any liberty (not dead)

  return false;
}




//' test for gogame class
//' @export
// [[Rcpp::export]]
void gogame_test()
{
  Gogame gg(19);
  gg.summary();
}

