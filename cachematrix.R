## define "makeCacheMatrix" function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## set the value of matrix
  i <- NULL                                 
  set <- function(y) {                      
    x <<- y                                
    i <<- NULL
  }
  
## get the value of matrix
  get <- function() x  
  
## set the value of inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
                  
## get the value of inverse of matrix
  getinverse <- function() i    
  
  ## return a list of four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## define "cacheSolve" function to compute the inverse of the special “matrix” returned by "makeCacheMatrix" function 
## If the inverse has already been calculated, then the inverse will be retrieved
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
## if the inverse matrix already exits, retrieve it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if the inverse matrix does not exit, compute it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## return computed inverse matrix
  i
}
