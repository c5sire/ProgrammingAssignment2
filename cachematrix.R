## The following pair of functions calculates the inverse of a square numeric matrix.
## It uses the R scoping rules to cache a potentially long running function. The key
## to this approach is assinging the results to a variable in another R environment.
## This is indicated in R using the operator '<<-'.
##
##
## References: the pair of function is mmodeled after the example given in the 
## programming assigment 2 of the Coursera R course.
##
## Reinhard Simon


## This function creates a special internal cacheable variable to
## store the cached result of the inverse of a matrix. It has a companion function
## 'cacheSolve' that actually calculates that inverse.
##
## @param x a matrix object
## @return a list containing access functions to the cached matrix inverse
## @author Reinhard Simon

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL # variable to hold the inverse of the matrix in another environment
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mi <<- inverse
  getInverse <- function() mi
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function returns the inverse of a matrix using cached results when available.
## If first tries to retrieve any cached matrix inverse from the internal cached 
## variable; if not yet available it calculates, assigns it to the cached variable
## for future uses and returns the result.
##
## @param x a matrix object
## @return a matrix inverse
## @author Reinhard Simon

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Try to get the cached inverse
  mi <- x$getInverse()
  if(!is.null(mi)) { ## if already available ...
    message("getting cached data")
    return(mi) ## return the cached inverse;
  }
  ## otherwise calculate it ...
  data <- x$get()
  mi <- solve(data, ...)
  ## store it for future use and ...
  x$setInverse(mi)
  
  ## return the calculated inverse
  mi
}
