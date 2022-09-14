suppressMessages(require(igraph))

#' @title Jaccard Distance
#' @description Calculate Jaccard distance between two vectors
#' @param a A vector.
#' #' @param a A vector.
#' @param b A vector.
#' @return Jaccard distance calculated as the ratio between the intersection and the union of the two vectors.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  jaccard(c('A','B','C','D','E'), c('A','B','C'))
#'  }
#' }
#' @rdname jaccard
#' @export 
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}
#' @title Regulon Redundancy
#' Calculate redundancy between two regulons
#' @param a A vector
#' @param b A vector
#' @return Redundancy calculated as the ratio between the intersection and the minimum length of the two vectors.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#' redundancy(c('A','B','C','D','E'), c('A','B','C'))
#'  }
#' }
#' @rdname redundancy
#' @export 
redundancy <- function(a, b) {
  intersection = length(intersect(a, b))
  min = min(length(a),length(b))
  return (intersection/min)
}


#' @title Range between 0 and 1
#' @description Normalize absolute values of a numeric vector
#' @param x A numeric vector
#' @return Normalized values (x-min(x))/(max(x)-min(x))
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#' range01(c('-50','-5','5','50','100')
#'  }
#' }
#' @rdname range01
#' @export 
range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}




#' @title Split Composite Names
#' @description Split a vector of composite names into a matrix of simple names.
#' @param x character vector
#' @param split character to split each element of vector on, see strsplit
#' @param ... other arguments are passed to strsplit
#' @return Normalized values (x-min(x))/(max(x)-min(x))
#' @details This function is the same as strsplit except that the output value is a matrix instead of a list. 
#' The first column of the matrix contains the first component from each element of x, the second column contains the second components etc.
#'  The number of columns is equal to the maximum number of components for any element of x.
#'   The motivation for this function in the limma package is handle input columns which are composites of two or more annotation fields.
#' @examples 
#' \dontrun{
#' if(interactive()){
#' x = c("2__cc.1","2__cc.2"); strsplit2(x,split="__")
#'  }
#' }
#' @rdname strsplit2
#' @export 
strsplit2 = function (x, split, ...) 
{
  x <- as.character(x)
  n <- length(x)
  s <- strsplit(x, split = split, ...)
  nc <- unlist(lapply(s, length))
  out <- matrix("", n, max(nc))
  for (i in 1:n) {
    if (nc[i]) 
      out[i, 1:nc[i]] <- s[[i]]
  }
  out
}


