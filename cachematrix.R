## Functions that cache the inverse of a matrix

## This function creates an object to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    #sol is the inverse of the matrix
    sol <- NULL
    set <- function(y){
      x <<- y
      sol <<- NULL
    }
    get <- function() x
    setinverse <- function(solvematrix) sol <<- solvematrix
    getinverse <- function() sol
  
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Function to compute the inverse of the matrix as returned by the function above

cacheSolve <- function(x, ...) {
       
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getinverse()
        if(!is.null(sol)) {
                 message("getting cached data")
                 return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setinverse(sol)
        return(sol)
}
