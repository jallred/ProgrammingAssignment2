## Module to invert a matrix. When computing the inversion, store the
## result in a global cache. If the inversion has already been
## computed, returne the cached result.

## create the cache entry to store the result of inverting the matrix
makeCacheMatrix <- function(x) {
  im <- NULL ## the inverted matrix
  
  set <- function(y) {
    x <<-y
    im <<- NULL
  }
  
  get <- function () x
  
  setinversematrix <- function (y) im <<- y
  
  getinversematrix <- function () im
  
  ## list of functions in this module
  list(set = set, 
       get = get, 
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## computes the inverse matrix and caches the result

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  ## if we have already computed the inverse, return the cached result

  ## matrix must be square. we're not checking this per the assignment
  
  ## first, check to see if we have already computed the inverse of 
  ## this matrix. if so, return the inverted matrix from the cache
 
  originalmatrix <- x$get()
  inversematrix <- x$getinversematrix()
  
  if(!is.null(inversematrix)){
    message("getting cached data")
    return(inversematrix)
  }
  
  # otherwise compute the inverse
  matrix1<-x$get()
  inversematrix<-solve(matrix1)			# compute the inverse of the original matrix
  x$setinversematrix(inversematrix)
  inversematrix
}

