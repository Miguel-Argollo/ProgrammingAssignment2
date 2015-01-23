##
##	Coursera - R Programming Course , January, 2015
##
##	This file contains 2 functions
##		makeCacheMatrix: Returns a list with 4 functions:
##  		1. set_matrix - sets the cached variables "mat" with the value of the parametrer matrix  
##                			and "inverted_matrix" with the value "NULL"
##  		2. get_matrix - returns the value of the original matrix cached
##  		3. set_inverted_matrix - caches the value of the inverted matrix
##  		4. get_inverted_matrix - returns the value of the inverted matrix
##
##		cacheSolve: If the matrix associated with the list passed as the parameter has already been inverted,
##  				returns the "cached" inverted matrix; if not, calls "solve" to invert the matrix and 
##  				returns the inverted matrix.
## 

makeCacheMatrix <- function(x = matrix()) {
##
##	This function creates a special "matrix" object that can cache its inverse.
##

  inverted_matrix <- NULL
  set <- function(rmat) {
    mat <<- rmat
    inverted_matrix <<- NULL
  }
  
  get <- function() mat
  set_invm <- function(p1) inverted_matrix <<- p1
  get_invm <- function() inverted_matrix
  
  list(set_matrix = set, 
       get_matrix = get,
       set_inverted_matrix = set_invm,
       get_inverted_matrix = get_invm)
}

cacheSolve <- function(x, ...) {
## 
##	This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##	above. If the inverse has already been calculated (and the matrix has not changed), 
##	then the cachesolve should retrieve the inverse from the cache.
##
		
  inverted_matrix <- x$get_inverted_matrix()
  if(!is.null(inverted_matrix)) {
    message("getting cached data")
    return(inverted_matrix)
  }
  data <- x$get_matrix()
  inverted_matrix <- solve(data, ...)
  x$set_inverted_matrix(inverted_matrix)
  inverted_matrix		
}

