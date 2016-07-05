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
  static const int EM = 0;
  static const int BL = 1;
  static const int WH = 2;
  static const int OB = 3;

  int boardsize;
  std::vector< std::vector<int> > board;

  unsigned int b_captured;
  unsigned int w_captured;

  unsigned int movenumber; // current move number
  std::vector<Transition> transitions;

  void Play(int color, unsigned int x, unsigned int y, bool ismove);
  bool HasLiberty(unsigned int x, unsigned int y,
                  std::vector< std::vector<bool> > &visited);
  void RemoveChain(unsigned int x, unsigned int y);
  void CheckAndRemove(unsigned int x, unsigned int y);


  public:
  Gogame(unsigned int s);  // no default constractor. requires board size
  void Clear();   // initialize board and prisoners


  // functions to be called by outside
  void BPlay(unsigned int x, unsigned int y, bool ismove)
    { Play(BL, x, y, ismove); }
  void WPlay(unsigned int x, unsigned int y, bool ismove)
    { Play(WH, x, y, ismove); }



  // for debugging
  void Summary();
};
