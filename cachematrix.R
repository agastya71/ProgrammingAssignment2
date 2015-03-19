## R functions to cache the inverse of a matrix, Uses 
## a function to cache the inverse of the matrix - makeCacheMatrix
## and Return a cahced inverse if already calculated (or) if not calculated,
## call the solve function to calculate the inverse and set the 
## inverse in the cached object (created by the call to makeCacheMatrix)
## Steps to invoke this functionality :
##    1. create an object that will store the caching functionality like so:
##        a<-makeCacheMatrix()   (Important to not forget the parantheses)
##    2. Optional Step to create a variable and store a matrix  like so:
##        b<- matrix(c(1,-1/4,-1/4,1),nrow=2,ncol=2) 
##         (Note: Inverse operation only works on a Square matrix, i.e. nrow = ncol)
##    3. Set the matrix in the object created in Step 1. like so :
##         a$set(b)
##    4. Call the cacheSolve function on a, like so :
##         cacheSolve(a)
## The first time cacheSolve is called, since the inverse is not yet calculated, 
## it will calculate it and cache it/store it.
## If the cacheSolve(a) function is executed again (without clearing the 
## System environment), The message "getting cached data" will be displayed
## along with the inverse of the submitted matrix. 
##
## to verify that the solution is indeed the Inverse and correct : perform the following
## step:
##   b%*%cacheSolve(a)
##    This would return a matrix with the diagonal set to 0 and the other diagonal set to 1
##     for the above example :
##          [,1] [,2]
##    [1,]    1    0
##    [2,]    0    1

## Creates the object that will store the inverse of a matrix "x"
## that is set in the object by object$set(matrix) call.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## function that computes and caches the inverse of the matrix
## stored in the object passed in.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
