#include <Rcpp.h>
#include <string>
#include <algorithm>


// [[Rcpp::export]]
std::vector<int> char_to_coord(std::vector<std::string> charvec)
{
  // convert char to coordinate
  // e.g., a -> 1, z -> 26
  //
  // Args:
  //   charvec : vector of characters of length 1
  //
  // Returns:
  //   integer vector
  //
  // Assumes:
  //   Each entry is length 1

  for (int i = 0; i < charvec.size(); i++)
  {
    if (charvec[i].size() != 1) {
      Rcpp::Rcout << charvec[i] << "\n";
      Rcpp::stop("each element of charvec must be length 1");
    }
  }

  std::vector<int> out(charvec.size());
  for (int i = 0; i < charvec.size(); i++)
    out[i] = charvec[i][0] - 96;

  return out;
}


// [[Rcpp::export]]
Rcpp::DataFrame expand_rectangle(std::vector<std::string> text)
{
  // expand rectangle expression
  // e.g., ab:cc -> ab,    ac,    bb,    bc,    cb,    cc
  //             -> (1,2), (1,3), (2,2), (2,3), (3,2), (3,3)
  //       ab    -> (1,2)
  //
  // Args:
  //   text : character vector
  //
  // Returns:
  //   dataframe with three columns
  //       x, y  : integer vectors of coorinates
  //       index : integer vectors maps each element of x and y to
  //               the index of correponding text
  //

  int x1;
  int x2;
  int y1;
  int y2;
  bool invalid;

  std::vector<int> x_out;
  std::vector<int> y_out;
  std::vector<int> i_out;

  // input check
  for (unsigned int i = 0; i < text.size(); i++)
  {
    if (text[i].size() != 2 && text[i].size() != 5) {
      Rcpp::Rcout << text[i] << "\n";
      Rcpp::stop("unregnized rectangle expression");
    }
    if (text[i].size() == 5 && text[i][2] != ':') {
      Rcpp::Rcout << text[i] << "\n";
      Rcpp::stop("unregnized rectangle expression");
    }
  }

  // main computation
  for (unsigned int i = 0; i < text.size(); i++)
  {
    // char -> int, no cast is needed
    // minus 96 to make 'a' = 1
    x1 = text[i][0] - 96;
    y1 = text[i][1] - 96;
    if (text[i].size() == 5) {
      x2 = text[i][3] - 96;
      y2 = text[i][4] - 96;
    } else {
      x2 = x1;
      y2 = y1;
    }

    // make sure x1 <= x2 and y1 <= y2
    if (x1 > x2) std::swap(x1, x2);
    if (y1 > y2) std::swap(y1, y2);

    //Rcpp::Rcout << x1 << " " << y1 << " " << x2 << " " << y2 << "\n";
    for (int x = x1; x <= x2; x++)
    {
      for (int y = y1; y <= y2; y++)
      {
        x_out.push_back(x);
        y_out.push_back(y);
        i_out.push_back(i + 1);  // add 1 so the index is one-based
      }
    }
  }

  // compile output
  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("x") = x_out,
    Rcpp::Named("y") = y_out,
    Rcpp::Named("index") = i_out
  );
  return out;
}




/*** R
gogamer:::expand_rectangle(c("fa:ic", "cc", "bc:ad"))
gogamer:::char_to_coord(c("a", "z", "`"))
*/
