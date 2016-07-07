#include <Rcpp.h>
#include <string>

std::string PruneSgf(std::string x, bool keep_first);
void PruneRecursive(std::string &x, bool &keep_first);
void FindBranch(std::string &x, int &start, int &end);


//' Remove branches from SGF text
//' @param sgf  Scalar character formatted as SGF
//' @param keep_first  Logical. If TRUE, keep the first branch.
//'   Otherwise, keep the last branch.
//' @return Scalar character of SGF text where branches are removed
//' @export
// [[Rcpp::export]]
std::string prune_sgf(std::string sgf, bool keep_first = true)
{
  PruneRecursive(sgf, keep_first);
  return sgf;
}


void PruneRecursive(std::string &x, bool &keep_first)
{
  int start = 0;
  int end = x.length() - 1;
  FindBranch(x, start, end);
  if (start < 0) return;  // no branch

  std::string trunc = x.substr(0, start);
  if (!keep_first) {
    int new_start = end + 1;
    int new_end = x.length() - 1;
    while (true)
    {
      Rcpp::checkUserInterrupt();
      FindBranch(x, new_start, new_end);
      if (new_start < 0) break;
      start = new_start;
      end = new_end;
      new_start = end + 1;
      end = x.length() - 1;
    }
  }
  std::string child = x.substr(start + 1, end - start - 1);
  x = trunc + child;
  PruneRecursive(x, keep_first);
}



// This function finds the first SGF branch in x
// between start and end positions.
// The starting and ending indices are stored in start and end.
// If x has no branch, then start and end are both equal to -1.
void FindBranch(std::string &x, int &start, int& end)
{
  bool intag = false;
  int open_count = 0;
  for (int i = start; i <= end; i++)
  {
    if (!intag) {
      if (x[i] == '[') {
        if (i == 0) {
          intag = true;
        } else if (x[i-1] != '\\') {
          intag = true;
        }
      } else if (x[i] == '(') {
        if (open_count == 0)
          start = i;
        open_count++;
      } else if (x[i] == ')') {
        if (open_count > 0) {
          open_count--;
          if (open_count == 0) {
            // original parenthesis is closed
            end = i;
            return;
          }
        }
      }
    } else {
      if (x[i] == ']') {
        if (i == 0) {
          intag =false;
        } else if (x[i-1] != '\\') {
          intag = false;
        }
      }
    }
  }
  start = -1;
  end = -1;
  return;
}
