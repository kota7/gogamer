#include <Rcpp.h>
#include <vector>
#include "gogame.h"



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

  Rcpp::Rcout << "\nmove number: " << movenumber << "\n";

  Rcpp::Rcout << "\ncaptured\n";
  Rcpp::Rcout << " Black = "<< b_captured << "\n";
  Rcpp::Rcout << " white = "<< w_captured << "\n";


  Rcpp::Rcout << "\nboard configuration\n";
  for (unsigned int i = 0; i < board.size(); i++)
  {
    for (unsigned int j = 0; j < board[i].size(); j++)
      Rcpp::Rcout << board[i][j] << " ";
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


void Gogame::Play(unsigned int color, unsigned int x,
                  unsigned int y, bool ismove)
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
  unsigned int opponent_color;
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

  unsigned int color = board[y][x];

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
  unsigned int color = board[y][x];
  if (color != BL && color != WH) return true;

  // mark that this point has been checked already
  // so that the same point is not examined again
  visited[y][x] = true;


  // loop over the four adjacent point
  unsigned int xx;
  unsigned int yy;
  unsigned int ccolor;
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



void Gogame::GobackTo(int m)
{
  // go back to a certain move number
  //
  // This function does:
  //   turn the board configuration and prisoner counts back to that of
  //     specified move number
  //   truncate transitions after the specified move number
  //   reset the move number
  //   if m is negative, then board is fully initialized
  //
  // Args:
  //   m : move number to go back to
  //
  // Returns:
  //   nothing

  // initialized to zero, which is used when all elements are to be removed
  int nToKeep = 0;
  for (int i = transitions.size() - 1; i >= 0; i--)
  {
    if ((int)transitions[i].movenumber <= m) {
      // you have reached the target move number
      nToKeep = i + 1;
      break;
    } else {
      // revert board configuration
      if (transitions[i].x > 0 && transitions[i].x <= boardsize
            && transitions[i].y > 0 && transitions[i].y <= boardsize) {
        board[transitions[i].y][transitions[i].x] -= transitions[i].value;
        // revert prisoner count
        if (transitions[i].value == -BL) {
          b_captured--;
        } else if (transitions[i].value == -WH) {
          w_captured--;
        }
      }
    }
  }

  // truncate the transitions (keep up to i-th element)
  transitions.resize(nToKeep);
  // set the movenumber
  // but do not change if m is greater than the original move number
  if (m < 0) {
    movenumber = 0;
  } else if (m < (int)movenumber) {
    movenumber = m;
  }
}






// test for gogame class
// [[Rcpp::export]]
void gogame_test(int m = 0)
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

  gg.GobackTo(m);
  gg.Summary();
}

/***R
gogamer:::gogame_test()
gogamer:::gogame_test(-1)
gogamer:::gogame_test(10)
gogamer:::gogame_test(4)
*/
