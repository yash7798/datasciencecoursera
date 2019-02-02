## Put comments here that give an overall description of what your
## functions do

## The following function creates a special "matrix" object that can cache it's inverse.
## It creates a list object from the matirx given as input, containing functions to:
## 1. Set the value of the Matrix
## 2. Get the value of Matrix
## 3. Set the value of the inverse of Matrix
## 4. Get the value of inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
x<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function() x
setinv<-function(inverse) inv<<-inverse
getinv<-function() inv
list(set=set,get=get,setinv=setinv,getinv=getinv)
}



## The following function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated then cacheSolve 
## retrieves the inverse stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

