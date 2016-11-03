#ifndef GOGAMEHEADERDEF
#define GOGAMEHEADERDEF


#include <Rcpp.h>
#include <vector>


struct Transition
{
  // this structure stores the transition in the board configuration
  // e.g., add a black stone, remove a white stone
  // the absolute value of 'value' indicates color
  // and the sign indicates add/remove

  unsigned int movenumber;
  unsigned int x;
  unsigned int y;
  int value;
  bool ismove;
  int nodeid;
  Transition() {}
  Transition(unsigned int mn, unsigned int xx, unsigned int yy, int val,
             int nid, bool move)
  {
    movenumber = mn;
    x = xx;
    y = yy;
    value = val;
    ismove = move;
    nodeid = nid;
  }

};



class Gogame
{
  // stones and point state marker
  // colors must be positive since in game transition expression,
  // adding a stone is denoted by color, and
  // removing by -color
  static const unsigned int EM = 0;
  static const unsigned int BL = 1;
  static const unsigned int WH = 2;
  static const unsigned int OB = 3;

  int boardsize;
  std::vector< std::vector<unsigned int> > board;

  int b_captured;
  int w_captured;

  unsigned int movenumber; // current move number
  int currentnode;         // current node id
  std::vector<Transition> transitions;

  // function to check liberty
  // the one with visited argument is a recursive function
  // the one without visited argument is a wrapper function for the other,
  // where visited vector is prepared and calls the recursive function
  bool HasLiberty(unsigned int x, unsigned int y);
  bool HasLiberty(unsigned int x, unsigned int y,
                  std::vector< std::vector<bool> > &visited);

  void RemoveChain(unsigned int x, unsigned int y);
  void CheckAndRemove(unsigned int x, unsigned int y);

  // check if you can put stone
  bool IsLegal(unsigned int x, unsigned int y, unsigned int color, bool ismove);

public:
  Gogame(unsigned int s);  // no default constractor. requires board size
  void Clear();      // clear stones and prisoner counts

  void AddStone(unsigned int color, unsigned int x,
            unsigned int y, bool ismove, int nodeid);
  // wrapper for Play
  void AddBlackStone(unsigned int x, unsigned int y, bool ismove, int nodeid)
    { AddStone(BL, x, y, ismove, nodeid); }
  void AddWhiteStone(unsigned int x, unsigned int y, bool ismove, int nodeid)
    { AddStone(WH, x, y, ismove, nodeid); }

  // go back to a certain move number
  void GobackToMove(int m);
  void GobackToNode(int nid);

  // access to elements from outside
  // for returning a big object, return a reference with const modifier
  // so it is read-only access from outside
  unsigned int GetMoveNumber() const { return movenumber; }
  int GetCurrentNode() const { return currentnode; }
  const std::vector<Transition> &GetTransitions() const { return transitions; }

  // friend function to interact with R
  // returns a data frame containing the transition of board configuration
  //friend Rcpp::DataFrame GetTransitionsAsDF(Gogame g);
  // This function has been removed.

  // for debugging
  void Summary();
};


#endif
