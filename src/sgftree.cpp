#include <Rcpp.h>
#include <vector>
#include <string>
#include "tree.h"

Tree<std::string> get_sgftree(const std::string &sgf)
{
  // parse SGF string and returns a tree of SGF nodes
  // As a special case, where there is no branch in the input,
  // returns a tree with a single node
  Tree<std::string> out;
  unsigned int i0;
  unsigned int opencount = 0;

  // This loop finds the beginning of the first branch
  // Usually, a SGF starts at the first letter, but
  // this handles cases where SGF contains few spaces or
  // junk information before the first parenthesis
  for (unsigned i = 0; i < sgf.size(); i++)
  {
    if (sgf[i] == '(') {
      i0 = i + 1;
      opencount = 1;
      break;
    }
  }

  // Return an empty tree if no branch is found
  if (opencount == 0) return out;

  unsigned int myid = 0;
  int parentid = -1;
  int mystart = i0;
  bool intag = false;   // indicates we are now in tag element
  bool inbranch = true; // indicates we are not in a branch, not in-between
  for (unsigned int i = i0; i < sgf.size(); i++)
  {
    Rcpp::Rcout << "at " << i << ": " << sgf[i] <<
      " myid = " << myid << " parentid = " << parentid << "\n";
    // finish if all node is closed
    if (opencount == 0) break;

    if (!intag && sgf[i] == '(') {
      // new branch opens
      // so, add the current node to the tree
      if (inbranch) out.AddNode(sgf.substr(mystart, i - mystart), parentid);

      opencount++;
      parentid = myid;
      myid = out.size();
      mystart = i + 1;
      inbranch = true;
    } else if (!intag && sgf[i] == ')') {
      // current branch closes
      if (inbranch) out.AddNode(sgf.substr(mystart, i - mystart), parentid);

      opencount--;
      if (opencount == 0) break;

      myid = parentid;
      parentid = out.GetParent(myid);
      inbranch = false;
    } else if (!intag && sgf[i] == '[') {
      // new tag starts
      intag = true;
    } else if (intag && sgf[i] == ']' && sgf[i-1] == '\\') {
      intag = false;
    }
  }

  return out;
}


// test for sgftree function
// [[Rcpp::export]]
void sgftree_test(std::string sgf)
{
  std::vector<unsigned int> c;
  Tree<std::string> t = get_sgftree(sgf);
  Rcpp::Rcout << "get_sgftree() has finished!\n";
  Rcpp::Rcout << "SGF = " << sgf << "\n";
  for (unsigned int i = 0; i < t.size(); i++)
  {
    Rcpp::Rcout << i << " : " << t.Get(i) <<
      "\n  parent = " <<  t.GetParent(i) << "\n  children = ";
    c = t.GetChildren(i);
    for (unsigned int j = 0; j < c.size(); j++)
      Rcpp::Rcout << c[j] << " ";
    Rcpp::Rcout << "\n";
  }
}


/*** R
gogamer:::sgftree_test("(aaa (bbb (ccc)(ddd)) (eee) (fff (ggg)))")
*/
