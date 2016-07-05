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

  unsigned int b_captured;
  unsigned int w_captured;

  void Play(int color, unsigned int x, unsigned int y);
  bool HasLiberty(unsigned int x, unsigned int y,
                  std::vector< std::vector<bool> > &visited);
  void RemoveChain(unsigned int x, unsigned int y);
  void CheckAndRemove(unsigned int x, unsigned int y);


public:
  Gogame(unsigned int s);  // no default constractor. requires board size
  void Clear();   // initialize board and prisoners


  // functions to be called by outside
  void BPlay(unsigned int x, unsigned int y) { Play(BL, x, y); }
  void WPlay(unsigned int x, unsigned int y) { Play(WH, x, y); }



  // for debugging
  void Summary();
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
  Clear();
}

void Gogame::Clear()
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


void Gogame::Summary()
{
  Rcpp::Rcout << "color definition\n";
  Rcpp::Rcout << " Empty = "<< EM << "\n";
  Rcpp::Rcout << " Black = "<< BL << "\n";
  Rcpp::Rcout << " White = "<< WH << "\n";
  Rcpp::Rcout << " OB    = "<< OB << "\n";

  Rcpp::Rcout << "\ncaptured\n";
  Rcpp::Rcout << " Black = "<< b_captured << "\n";
  Rcpp::Rcout << " white = "<< w_captured << "\n";


  Rcpp::Rcout << "\nboard configuration\n";
  for (unsigned int i = 0; i < board.size(); i++)
  {
    for (unsigned int j = 0; j < board[i].size(); j++)
      Rcpp::Rcout << board[j][i] << " ";
    Rcpp::Rcout << "\n";
  }
}


void Gogame::Play(int color, unsigned int x, unsigned int y)
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

  // loop over the four adjacent point
  unsigned int xx;
  unsigned int yy;
  int increment;
  for (int k = 0; k < 4; k++)
  {
    increment = 2*(k % 2) - 1;
    if (k < 2) {
      xx = x + increment;
      yy = y;
    } else {
      xx = x;
      yy = y + increment;
    }
    Rcpp::Rcout << xx << " " << yy << "\n";
    // if this is a opponent stone,
    // check the liberty of this point,
    // and if it does not have a liberty,
    // remove all stones connected to it
    if (board[yy][xx] == opponent_color)
      CheckAndRemove(xx, yy);
  }

}


void Gogame::CheckAndRemove(unsigned int x, unsigned int y)
{
  // check the liberty of point (x, y)
  // and if it has no liberty, remove all connected stones to (x, y)

  // prepare for a checklist that stores points that have been checked already
  std::vector< std::vector<bool> > visited(board.size());
  for (unsigned int j = 0; j < visited.size(); j++)
  {
    for (unsigned int i = 0; i < visited[j].size(); i++)
    {
      visited[j][i] = false;
    }
  }

  if (!HasLiberty(x, y, visited))
    RemoveChain(x, y);
}


void Gogame::RemoveChain(unsigned int x, unsigned int y)
{
  // remove all stones connected to (x, y) and of the same color

  int color = board[y][x];

  // do nothing if the color is neither black nor white
  if (color != BL && color != WH) return;


  // remove this point
  board[y][x] = EM;
  if (color == BL) {
    b_captured++;
  } else {
    w_captured++;
  }


  // loop over the four adjacent point
  unsigned int xx;
  unsigned int yy;
  int increment;
  for (int k = 0; k < 4; k++)
  {
    increment = 2*(k % 2) - 1;
    if (k < 2) {
      xx = x + increment;
      yy = y;
    } else {
      xx = x;
      yy = y + increment;
    }

    if (board[yy][xx] == color) RemoveChain(xx, yy);
  }
}


bool Gogame::HasLiberty(unsigned int x, unsigned int y,
                        std::vector< std::vector<bool> > &visited)
{
  // return true if the point (x, y) has a liberty

  // assumes that there is a stone at the point
  // if it is neither black nor white, do nothing and return true
  int color = board[y][x];
  if (color != BL && color != WH) return true;

  // mark that this point has been checked already
  // so that the same point is not examined again
  visited[y][x] = true;


  // loop over the four adjacent point
  unsigned int xx;
  unsigned int yy;
  int ccolor;
  int increment;
  for (int k = 0; k < 4; k++)
  {
    increment = 2*(k % 2) - 1;
    if (k < 2) {
      xx = x + increment;
      yy = y;
    } else {
      xx = x;
      yy = y + increment;
    }

    // skip if this point has been already examined
    if (!visited[yy][xx]) {
      ccolor = board[yy][xx];
      // if adjacent point (xx, yy) is...
      //  Empty          ... liberty found, so return true
      //  same color     ... check the liberty of (xx, yy) recursively
      //  OB or opponent ... this is not liberty, do nothing
      if (ccolor == EM) {
        return true;
      } else if (ccolor == color) {
        if (HasLiberty(xx, yy, visited)) return true;
      }
    }
  }
  return false;
}




//' test for gogame class
//' @export
// [[Rcpp::export]]
void gogame_test()
{
  Gogame gg(19);
  gg.Summary();

  gg.BPlay(4, 4); gg.Summary();
  gg.WPlay(4, 5); gg.Summary();
  gg.WPlay(4, 3); gg.Summary();
  gg.WPlay(3, 4); gg.Summary();
  gg.WPlay(5, 4); gg.Summary();

  gg.BPlay(15, 13); gg.Summary();
  gg.BPlay(15, 14); gg.Summary();
  gg.WPlay(15, 12); gg.Summary();
  gg.WPlay(15, 15); gg.Summary();
  gg.WPlay(14, 13); gg.Summary();
  gg.WPlay(14, 14); gg.Summary();
  gg.WPlay(16, 13); gg.Summary();
  gg.WPlay(16, 14); gg.Summary();

}

