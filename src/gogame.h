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

  Transition() {}
  Transition(unsigned int mn, unsigned int a, unsigned int b, int v)
  {
    movenumber = mn;
    x = a;
    y = b;
    value = v;
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
  std::vector<Transition> transitions;

  bool HasLiberty(unsigned int x, unsigned int y,
                  std::vector< std::vector<bool> > &visited);
  void RemoveChain(unsigned int x, unsigned int y);
  void CheckAndRemove(unsigned int x, unsigned int y);


public:
  Gogame(unsigned int s);  // no default constractor. requires board size
  void Clear();   // initialize board and prisoners

  void Play(unsigned int color, unsigned int x, unsigned int y, bool ismove);
  // wrapper for Play
  void BPlay(unsigned int x, unsigned int y, bool ismove)
    { Play(BL, x, y, ismove); }
  void WPlay(unsigned int x, unsigned int y, bool ismove)
    { Play(WH, x, y, ismove); }



  // friend function to interact with R
  // returns a data frame containing the transition of board configuration
  friend Rcpp::DataFrame GetTransitions(Gogame g);

  // for debugging
  void Summary();
};
