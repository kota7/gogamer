#include "sgftree.h"


Tree<std::string> MakeSgfBranchTree(const std::string &sgf)
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
    } else if (intag && sgf[i] == ']' && sgf[i-1] != '\\') {
      intag = false;
    }
  }

  return out;
}




Tree<std::string> MakeSgfNodeTree(const std::string &sgf)
{
  Tree<std::string> out;

  // Find the main branch
  unsigned int startindex;
  bool flg = false;
  for (unsigned int i = 0; i < sgf.size(); i++)
  {
    if (sgf[i] == '(') {
      startindex = i+1;
      flg = true;
      break;
    }
  }

  // return an empty tree if no branch is found
  if (!flg) return out;

  // Go to the recursive procedure to fill the tree
  int parentid = -1;
  ParseSgfBranch(sgf, startindex, parentid, out);
  return out;
}


void ParseSgfBranch(
    const std::string &sgf, unsigned int &index,
    int parentid, Tree<std::string> &out)
{
  // find the first node (i.e. ';')
  bool intag = false;
  bool innode = false;
  unsigned int nodestart = index;
  while (index < sgf.size())
  {
    if (!intag && sgf[index] == '[') {
      intag = true;
    } else if (intag && sgf[index] == ']' && sgf[index-1] != '\\') {
      intag = false;
    } else if (!intag && sgf[index] == ';') {
      // current node ends, if any
      if (innode) {
        out.AddNode(sgf.substr(nodestart, index-nodestart), parentid);
        parentid = out.size() - 1;
      }
      // new node starts
      nodestart = index + 1;
      innode = true;
    } else if (!intag && sgf[index] == ')') {
      // branch closed
      // hence, current node ends
      if (innode) {
        out.AddNode(sgf.substr(nodestart, index-nodestart), parentid);
      }
      return;
    } else if (!intag && sgf[index] == '(') {
      // new branch starts
      // hence, current node ends
      if (innode) {
        out.AddNode(sgf.substr(nodestart, index-nodestart), parentid);
        parentid = out.size() - 1;
      }
      index++;
      innode = false;
      ParseSgfBranch(sgf, index, parentid, out);
    }
    index++;
  }
}



// test for sgftree function
// [[Rcpp::export]]
void sgftree_test(std::string sgf)
{
  std::vector<unsigned int> c;
  Tree<std::string> t = MakeSgfBranchTree(sgf);
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

  MakeSgfNodeTree(sgf);
}



// interacting with R
// [[Rcpp::export]]
Rcpp::List make_sgftree(std::string sgf, bool bynode = true)
{
  Tree<std::string> t;
  if (bynode) {
    t = MakeSgfNodeTree(sgf);
  } else {
    t = MakeSgfBranchTree(sgf);
  }

  Rcpp::List out = TreeToList(t);
  return out;
}






/*** R
s1 <- "(;a1 ; a2;a3 (;bbb (;ccc)(;ddd)) (;eee) (;fff (;ggg)))"
s2 <- c("(;GM[1]FF[4]CA[UTF-8]AP[CGoban:3]ST[2]",
        "RU[Japanese]SZ[19]KM[0.00]PW[White]PB[Black];B[qd];W[dc](;B[cp]",
        ";W[pq])(;B[dq];W[pp];B[ce];W[ed];B[ci];W[od](;B[oc](;W[pd]",
        ";B[pc];W[qe];B[nd])(;W[nc];B[pc];W[nd];B[qf];W[jd]))(;B[ld]",
        ";W[pg];B[oe];W[ne];B[of];W[qc];B[qf];W[rd];",
        "B[pc];W[pd];B[qe];W[rc];B[nd])))") %>% paste0(collapse = "")
s3 <- c("(;GM[1]FF[4]CA[UTF-8]AP[CGoban:3]ST[2]",
        "RU[Japanese]SZ[19]KM[0.00]",
        "PW[White]PB[Black]",
        ";B[pd];W[dd];B[dp]C[special characters in comments...",
        "( yay[! )  { foo }  [ bar \\]; \\\\ ])") %>% paste0(collapse = "")


gogamer:::make_sgftree(s1, TRUE)
gogamer:::make_sgftree(s1, FALSE)

gogamer:::make_sgftree(s2, TRUE)
gogamer:::make_sgftree(s2, FALSE)

gogamer:::make_sgftree(s3, TRUE)
gogamer:::make_sgftree(s3, FALSE)
*/
