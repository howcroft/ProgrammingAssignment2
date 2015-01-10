## Put comments here that give an overall description of what your
## functions do

## This function has four sub-functions (get, set, setInverse, and getInverse)
## It receives a matrix as input, computes an inverse and stores it in cache.
## Any retrieval after initial computation will return the cached inverse (no invalidation of cache)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function produces the inverse of a matrix received. 
# Only accepts square Matrixes, as solve() only accepts square matrices.
#If one has already been produced it will retrieve this cached version
#Tip: you can test with identiy matrix: e.g. diag(3). Not really interesting though.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
          message("getting cached inverse matrix")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
