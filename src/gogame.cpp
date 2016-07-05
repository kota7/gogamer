#include <Rcpp.h>
#include <vector>
#include "gogame.hpp"



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

  movenumber = 0;
  transitions.resize(0);
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

  Rcpp::Rcout << "\nboard state transitions\n";
  for (unsigned int i = 0; i < transitions.size(); i++)
    Rcpp::Rcout << " " << transitions[i].movenumber << " " <<
      transitions[i].x << " " <<
      transitions[i].y << " " <<
      transitions[i].value << "\n";
  Rcpp::Rcout << "\n\n";
}


void Gogame::Play(int color, unsigned int x, unsigned int y, bool ismove)
{
  // play a move by color at (x, y)

  // TODO:
  //   check if legal move
  //     is it black or white?
  //     is this poition empty?
  //     suicide move
  //     ko
  // for now, assume moves are valid


  // ismove flag indicates if this play is a move or setup
  if (ismove) movenumber++;


  // put the stone temporarily
  board[y][x] = color;

  // and record it in the transitions field
  transitions.push_back(Transition(movenumber, x, y, color));


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
    visited[j].resize(board[j].size());
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

  // and record it in the transition field
  transitions.push_back(Transition(movenumber, x, y, -color));


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



Rcpp::DataFrame Gogame::GetTransitions()
{
  unsigned int n = transitions.size();

  std::vector<unsigned int> movevec(n);
  std::vector<unsigned int> xvec(n);
  std::vector<unsigned int> yvec(n);
  std::vector<int> vvec(n);

  for (unsigned int i = 0; i < n; i++)
  {
    movevec[i] = transitions[i].movenumber;
    xvec[i] = transitions[i].x;
    yvec[i] = transitions[i].y;
    vvec[i] = transitions[i].value;
  }
  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("move") = movevec,
    Rcpp::Named("x") = xvec,
    Rcpp::Named("y") = yvec,
    Rcpp::Named("value") = vvec
  );
  return out;
}




//' test for gogame class
//' @export
// [[Rcpp::export]]
void gogame_test()
{
  Gogame gg(19);
  gg.Summary();

  gg.BPlay(10, 10, false);
  gg.Summary();

  gg.BPlay(4, 4, true);
  gg.Summary();
  gg.WPlay(4, 5, true);
  gg.Summary();
  gg.WPlay(4, 3, true);
  gg.Summary();
  gg.WPlay(3, 4, true);
  gg.Summary();
  gg.WPlay(5, 4, true);
  gg.Summary();

  gg.BPlay(15, 13, true);
  gg.Summary();
  gg.BPlay(15, 14, true);
  gg.Summary();
  gg.WPlay(15, 12, true);
  gg.Summary();
  gg.WPlay(15, 15, true);
  gg.Summary();
  gg.WPlay(14, 13, true);
  gg.Summary();
  gg.WPlay(14, 14, true);
  gg.Summary();
  gg.WPlay(16, 13, true);
  gg.Summary();
  gg.WPlay(16, 14, true);
  gg.Summary();

  gg.WPlay(19, 14, true);
  gg.Summary();
  gg.BPlay(19, 13, true);
  gg.Summary();
  gg.BPlay(19, 15, true);
  gg.Summary();
  gg.BPlay(18, 14, true);
  gg.Summary();

  gg.BPlay(19, 19, true);
  gg.Summary();
  gg.WPlay(19, 18, true);
  gg.Summary();
  gg.WPlay(18, 19, true);
  gg.Summary();



  gg.WPlay(5, 9, true);
  gg.Summary();
  gg.WPlay(6, 10, true);
  gg.Summary();
  gg.WPlay(7, 8, true);
  gg.Summary();
  gg.WPlay(7, 9, true);
  gg.Summary();
  gg.BPlay(5, 8, true);
  gg.Summary();
  gg.BPlay(6, 7, true);
  gg.Summary();
  gg.BPlay(7, 7, true);
  gg.Summary();
  gg.BPlay(8, 8, true);
  gg.Summary();
  gg.BPlay(8, 9, true);
  gg.Summary();
  gg.BPlay(7, 10, true);
  gg.Summary();
  gg.BPlay(6, 9, true);
  gg.Summary();
  gg.WPlay(6, 8, true);
  gg.Summary();
  gg.BPlay(6, 9, true);
  gg.Summary();

  gg.WPlay(10, 11, true);
  gg.Summary();
  gg.WPlay(9, 10, true);
  gg.Summary();
  gg.WPlay(10, 9, true);
  gg.Summary();
  gg.WPlay(11, 10, true);
  gg.Summary();

  gg.BPlay(1, 3, true);
  gg.Summary();
  gg.BPlay(1, 2, true);
  gg.Summary();
  gg.BPlay(1, 4, true);
  gg.Summary();
  gg.WPlay(2, 2, true);
  gg.Summary();
  gg.WPlay(2, 3, true);
  gg.Summary();
  gg.WPlay(2, 4, true);
  gg.Summary();
  gg.WPlay(1, 5, true);
  gg.Summary();
  gg.WPlay(1, 1, true);
  gg.Summary();

}

