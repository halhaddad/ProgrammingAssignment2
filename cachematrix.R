## The following functions cache the inverse of a matrix rather than compute it repeatedly


## The first function creates a special "matrix" that can cache its inverse. It does that by 
## 1. setting the value of the matrix
## 2. getting the value of the matrix
## 3. setting the value of the inverse of the matrix
## 4. getting the value of the inverse if the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function computes the inverse of the special "matrix" by checking first if the 
## inverse has already been calculated and retrieving the inverse from the cache
## if not, then it computes the inverse of the special "matrix returened by the function above

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

