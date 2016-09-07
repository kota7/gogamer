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
  currentnode = -1;
  transitions.resize(0);
}




void Gogame::Summary()
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
      Rcpp::Rcout << board[i][j] << " ";
    Rcpp::Rcout << "\n";
  }

  Rcpp::Rcout << "\nmove number: " << movenumber;
  Rcpp::Rcout << "\nnodeid     : " << currentnode << "\n";

  Rcpp::Rcout << "\ncaptured\n";
  Rcpp::Rcout << " Black = "<< b_captured << "\n";
  Rcpp::Rcout << " white = "<< w_captured << "\n";


  Rcpp::Rcout << "\nboard state transitions\n";
  for (unsigned int i = 0; i < transitions.size(); i++)
    Rcpp::Rcout << " " <<
      transitions[i].movenumber << " " <<
      transitions[i].nodeid << " " <<
      transitions[i].x << " " <<
      transitions[i].y << " " <<
      transitions[i].value << "\n";
  Rcpp::Rcout << "\n\n";
}

bool Gogame::IsLegal(unsigned int x, unsigned int y, unsigned int color, bool ismove)
{
  // check if this is a legal play
  // currently checks:
  // (a) For moves, color must be black or white
  // (b) If color is black or white, there must not be a stone there
  // (c) If this is a move (not a setup), it must be that either
  //   (i)   this stone has liberty (not a suicide move); or
  //   (ii)  some of the opponent stones can be captured
  // Strictly speaking, for the case of (ii), it must also be that this is not
  // an immediate ko take back, but this part is not checked

  // debug mode
  //return true;

  // move must be colored
  if (ismove && color != BL && color != WH) {
    return false;
  }

  // x and y must be a valid index for board
  // x and y are unsigned int, so no need to check >= 0
  if (x > boardsize + 1 || y > boardsize + 1) {
    return false;
  }

  // can't put a stone on non-empty point if it is a move
  // if it is a set up, then it is okay.
  if ((color == BL || color == WH) && ismove
        && (board[y][x] == BL || board[y][x] == WH)) {
      return false;
  }

  if ((color == BL || color == WH) && ismove &&
      x >= 1 && x <= boardsize && y >= 1 && y <= boardsize) {
    // will check the liberty validity
    // illegal if:
    //   this stone has no liberty and
    //   all opponent stones adjacent to it has liberty

    unsigned int opponent_color;
    if (color == BL) {
      opponent_color = WH;
    } else {
      opponent_color = BL;
    }

    // put stone temporarily
    board[y][x] = color;
    // then check if this point has liberty
    // if it has, good.
    // otherwise check if some of the adjacent stones can be taken

    bool own_liberty = HasLiberty(x, y);
    bool opp_liberty = true;
    // indicates that all adjacent opponent stones have liberty
    // initialized as true (all alive) and update to false when
    // one of the opponent group is found to be dead
    if (!own_liberty) {
      // this stone has no liberty. check the opponent liberty
      // loop over the four adjacent point
      unsigned int xx;
      unsigned int yy;
      int increment;
      std::vector< std::vector<bool> > visited(board.size());
      for (unsigned int j = 0; j < visited.size(); j++)
        visited[j].resize(board[j].size(), false);

      opp_liberty = true;
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
        // and if some of them has no liberty, then
        if (board[yy][xx] == opponent_color) {
          if (!HasLiberty(xx, yy, visited)) {
            opp_liberty = false;
            break;
          }
        }
      }
    }
    // revert the stone
    board[y][x] = EM;
    // if own liberty is false but opp has libery, then
    // it is illegal suicide move
    if (!own_liberty && opp_liberty) return false;
  }

  // passed all test
  return true;
}

void Gogame::AddStone(unsigned int color, unsigned int x, unsigned int y,
                      bool ismove, int nodeid)
{
  // Add a stone of a color at (x, y)
  // may either be play or setup
  // if x or y is out-of-bounds, this is regarded as 'pass' move
  //
  // ismove indicates whether this is a move or setup
  // if setup, the movenumber does not increase

  // ismove flag indicates if this play is a move or setup

  // Rcpp::Rcout << "add stone: " <<
  //   "(" << color << "," << x << "," <<
  //   y << "," << ismove << "," << nodeid << ") ";

  // check validity of color
  if (color != BL && color != WH && color != EM) {
    Rcpp::Rcout << "at nodeid " << nodeid << " ";
    Rcpp::stop("invalid color");
  }
  if (ismove && color == EM) {
    Rcpp::Rcout << "at nodeid " << nodeid << " ";
    Rcpp::stop("empty stone is invalid for moves");
  }
  // check if the stone can be put there
  if (!IsLegal(x, y, color, ismove)) {
    Summary();
    Rcpp::Rcout << "at nodeid " << nodeid <<
      ", tries to play: (x, y, color, ismove) = (" <<
      x << ", " << y << ", " << color << ", " << (int)ismove << ")\n";
    Rcpp::stop("illegal move");
  }


  // update current node member field
  currentnode = nodeid;
  // case for setup move
  if (!ismove) {
    // if this is a setup move, then add stone at the point and
    // no need to check the liberty.
    // but do nothing if the x or y is out of bounds because
    // you cannot add stone there!
    if (x >= 1 && y >= 1 && x <= boardsize && y <= boardsize) {
      // if a stone is already there, then you need to first remove the stone
      if (board[y][x] == BL || board[y][x] == WH) {
        transitions.push_back(Transition(movenumber, x, y,
                                         -board[y][x], currentnode, false));
        board[y][x] = EM;
      }
      // if this new stone is colored, add that stone
      if (color == BL || color == WH) {
        board[y][x] = color;
        transitions.push_back(Transition(movenumber, x, y,
                                         color, currentnode, false));
      }
    }
    return;
  }

  // reaching here means it is a move
  movenumber++;

  // coordinates out of bounds are regarded as pass,
  // append transition, but do nothing afterwards
  // x = y = 0 means this move is a pass
  if (x < 1 || y < 1 || x > boardsize || y > boardsize) {
    transitions.push_back(Transition(movenumber, 0, 0, color, currentnode, true));
    return;
  }

  // put the stone temporarily
  board[y][x] = color;

  // and record it in the transitions field
  transitions.push_back(Transition(movenumber, x, y, color, currentnode, true));


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
  if (!HasLiberty(x, y)) RemoveChain(x, y);
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
  transitions.push_back(Transition(movenumber, x, y, -color, currentnode, false));


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

bool Gogame::HasLiberty(unsigned int x, unsigned int y)
{
  // wrapper for the other function
  // this function prepares visited vector and calls the other function,
  // which checks the liberties recursively

  // initialize visited marker
  std::vector< std::vector<bool> > visited(board.size());
  for (unsigned int j = 0; j < visited.size(); j++)
    visited[j].resize(board[j].size(), false);

  return HasLiberty(x, y, visited);
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



void Gogame::GobackToMove(int m)
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

void Gogame::GobackToNode(int nid)
{
  // go back to a certain nodeid
  //
  // This function does:
  //   turn the board configuration and prisoner counts back to that of
  //     specified nodeid
  //   truncate transitions after the specified nodeid
  //   reset the move number
  //   if nid is negative, then board is fully initialized
  //
  // Args:
  //   nid : nodeid
  //
  // Returns:
  //   nothing
  // initialized to zero, which is used when all elements are to be removed
  int nToKeep = 0;
  for (int i = transitions.size() - 1; i >= 0; i--)
  {
    if (transitions[i].nodeid == nid) {
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
  if (nToKeep == 0)  {
    movenumber = 0;
  } else {
    movenumber = transitions[nToKeep-1].movenumber;
  }
  // set the nodeid
  currentnode = nid;
}




// test for gogame class
// [[Rcpp::export]]
void gogame_test(int m = 0)
{
  Gogame gg(19);
  gg.Summary();

  gg.AddBlackStone(10, 10, false, 1);
  gg.Summary();

  gg.AddBlackStone(4, 4, true, 2);
  gg.Summary();
  gg.AddWhiteStone(4, 5, true, 3);
  gg.Summary();
  gg.AddWhiteStone(4, 3, true, 4);
  gg.Summary();
  gg.AddWhiteStone(3, 4, true, 5);
  gg.Summary();
  gg.AddWhiteStone(5, 4, true, 6);
  gg.Summary();

  gg.GobackToNode(m);
  gg.Summary();

  // some illegal check
  gg.AddBlackStone(16, 16, true, 7);
  //gg.AddWhiteStone(16, 16, true, 7);
}

/***R
gogamer:::gogame_test()
gogamer:::gogame_test(-1)
gogamer:::gogame_test(10)
gogamer:::gogame_test(4)
*/
