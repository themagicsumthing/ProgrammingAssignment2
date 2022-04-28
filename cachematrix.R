## These two functions allow the inverse of a matrix to be cached, thus saving
## computation time. They make use of the <<- operator to store "x" and "inv"
## in the enclosing environment where they can be accessed by both functions.

## This first function takes a matrix as an argument and returns a list of functions
## on that matrix. set stores a new matrix. get returns the stored matrix.
## setinverse is called by cacheSolve.R and stores the inverse in a variable.
## getinverse returns the stored inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}                             ## Get returns the matrix stored in x.
  setinverse <- function(inverse) {inv <<- inverse} ## setinverse is called by cacheSolve.R.
  getinverse <- function() {inv}                    ## Returns inv (inverse of matrix if calculated yet.)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a list of functions returned by makeCacheMatrix as its
## object, i.e. "x". If the inverse of the matrix ("x$get()") has already been
## calculated, it returns the cached inverse. Otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Returning cached inverse")
    return(inv)
  }else{
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    return(inv)
  }
}
