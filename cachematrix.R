# Assigment: Caching the Inverse of a Matrix
# Write the following functions:
# makeCacheMatrix & cacheSolve
# *******************************************
# * ZUNKA ARENOVITZ --- 22.08.2020          * 
# *******************************************

# makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){ #set the value of the MATRIX
              x <<- y
            inv <<- NULL
      }
      get <- function() x #get the value of the MATRIX
      
      setinverse <- function(inverse) inv <<- inverse #set the value of the INVERSE
      
      getinverse <- function() inv #get the value of the INVERSE
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "MATRIX" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # solve : This generic function solves the equation a %*% x = b for x.
  x$setinverse(inv)
  inv
}
