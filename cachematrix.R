## This function will cache an inverted matrix into memory for performance
## Created by Dean A Wynkoop


## Create list of functions that will set and get a matrix plus set and get it 
## from cache  

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize cache by setting it to null
  xCache <- NULL
  
  ## Create function to instantiate the matrix and it's cached version  
  set <- function(y) {
    x <<- y
    xCache <<- NULL
  }
  
  ## Create function to retrieve matrix
  get <- function() x
  
  ## Create function to save matrix to cache
  setcache <- function(y) xCache <<- y
  
  ## Create function to retrieve matrix from cache
  getcache <- function() xCache
  
  ## Create a list of functions
  list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## Retrieve an inverted matrix.  
## If in cache, retrieve from cache.
## Otherwise invert it and store it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Check to see if matrix has been cached, if so, return
  matrixInverse <- x$getcache()
  if (!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  
  ## Get matrix
  matrix <- x$get()
  
  ## Invert matrix
  matrixInverse <- solve(matrix)
  
  ## Place inverted matrix into cache
  x$setcache(matrixInverse)
  
  ## Return the newly inverted matrix
  matrixInverse
  
}
