## This function creates a special "matrix" object that can cache its inverse.  
# The "matrix" is actually a list object containing a get a set for the matrix
# and a get a set for the matrix's inverse. Setting the matrix will clear the 
# current inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #Function to set the matrix value.  Also, clears any existing 
    #inverse value
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    getinverse <- function() inv
    setinverse <- function(inverse) inv <<- inverse
    #Returns a list object with the methods to modify the cacheable matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ##If the inverse already exists we will return it
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        #Otherwise we will retrieve the matrix value and solve the inverse
        matrix <- x$get()
        inv <- solve(matrix)
        ##Set the inverse so it's cached for next time
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}

#Example matrix with inverse c=rbind(c(1, -1/4), c(-1/4, 1)) 