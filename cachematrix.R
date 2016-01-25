## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix is really a list containing a function to
##1.Set the value of the matrix
##2.Get the value of the matrix
##3.Set the value of the inverse
##4.Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 inve<- NULL
  set<- function(y){
    x<<-y
    inve<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inve<<-inverse
  getinverse <- function()inve
  list(set=set,get=get,
setinverse=setinverse,
getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inve<-x$getinverse()
  if(!is.null(inve)){
    message('getting cached data.')
    return(inve)
  }
  dat<-x$get()
  inve<-solve(dat)
  x$setinverse(inve)
  inve
}
## Example:
##>x=rbind(c(1, -1/2),c(-1/2,1))
##> m=makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]  1.0 -0.5
##[2,] -0.5  1.0
##> cacheSolve(m)
##          [,1]      [,2]
##[1,] 1.3333333 0.6666667
##[2,] 0.6666667 1.3333333

