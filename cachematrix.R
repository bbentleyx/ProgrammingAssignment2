## makeCacheMatrix provides the methods to save an inverse matrix in an
## alternate environment

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(minverse) i <<- minverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve will retrieve the inverse of x by either grabbing it
## from the alternate environment or computing the inverse and saving it

cacheSolve <- function(x, ...){
 i <- x$getinverse()
 if(!is.null(i)){
   message("getting cached data")
   return(i)
 }
 data<-x$get()
 i<-solve(x)
 x$setinverse(i)
 i
}



