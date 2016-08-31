fill_tree_structure <- function(parent, children, leaf)
{
  # Guess and fill missing components of tree structure
  #
  # Args:
  #   parent  : integer vector of parent node pointers
  #   children: list of integer vectors of children node pointers
  #   leaf    : integer vector of leaf node pointers
  #
  # Returns:
  #   a list of parent, children, and leaf, where components are best guessed

  check <- c(!is.null(parent), !is.null(children), !is.null(leaf))
  if (!check[1] && !check[2]) {
    # both parent and children missing -> assume only one node
    parent   <- 0L
    children <- list(integer(0))
  } else if (check[1] && !check[2]) {
    # impute children from parent
    children <- parent_to_children(parent)
  } else if (!check[1] && check[2]) {
    # impute parent from children
    parent <- children_to_parent(children)
  }
  if (!check[3]) {
    # impute leaf from children
    leaf <- parent_to_leaf(parent)
  }

  return(list(parent = parent, children = children, leaf = leaf))
}


check_tree_structure <- function(parent, children, leaf)
{
  # validity check for tree comonents
  #
  # Args:
  #   parent  : integer vector
  #   children: list of integer vectors
  #   leaf    : integer vector
  #
  # Returns:
  #   logical

  if (!validate_parent(parent)) {
    cat("invalid parent pointer\n")
    return(FALSE)
  }
  if (!validate_children(children)) {
    cat("invalid children pointer\n")
    return(FALSE)
  }

  ## mutual consitency of parent and children
  if (!identical(parent, children_to_parent(children))) {
    cat("parent and children are not consistent\n")
    return(FALSE)
  }
  if (!identical(children, parent_to_children(parent))) {
    cat("parent and children are not consistent\n")
    return(FALSE)
  }
  ## consistency of leaf with parent
  if (!identical(leaf, parent_to_leaf(parent))) {
    cat("leaf is not consistent with parent")
    return(FALSE)
  }

  ## no problem is detected
  return(TRUE)
}



children_to_parent <- function(children)
{
  # create parent from children
  #
  # Args:
  #   children: list of integer vector of children pointers
  #
  # Returns:
  #   integer vector of parent pointers

  len <- lapply(children, length) %>% unlist()
  index <- Map(rep, seq_along(len), len) %>% unlist()
  parent <- rep(NA_integer_, length(children))
  parent[unlist(children)] <- index
  parent[1] <- 0L
  return(parent)
}

parent_to_children <- function(parent)
{
  # create children from parent
  #
  # Args:
  #   parent: integer vector of parent pointers
  #
  # Returns:
  #   list of integer vectors of children pointers

  children <- lapply(seq_along(parent), function(i) which(i == parent))
  return(children)
}

parent_to_leaf <- function(parent)
{
  # create children from parent
  #
  # Args:
  #   parent: integer vector of parent pointers
  #
  # Returns:
  #   integer vectors of leaf pointers

  leaf <- seq_along(parent) %>% setdiff(parent)
  return(leaf)
}
