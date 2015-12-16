## Create a cache marix object, if the inverse of some matrix is not calculated, then we only
## have to do it once and store it
## set and get can set or get the matrix itself
## setinverse and getinverse can set or get the inverse of some matrix


makeCacheMatrix <- function(x = matrix()) {
  Cacheinverse <- NULL
  set <- function(y) {
    x <<- y
    Cacheinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Cacheinverse <<- inverse
  getinverse <- function() Cacheinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## calculate the inverse of a matrix and store it (If not calculated before)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
