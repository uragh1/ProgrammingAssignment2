## The objective here is to create an object which caches the inverse of a Matrix once it is
## calculated so that, next time program and fetches from cache if inverse is already calculated 
## 

## This function creates a list having functions to get and set matrix and get and set inverse
## Also demonstrates Lexical Scoping using <<- assignment

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This method calculates inverse of matrix that is assigned in the Special list created in the 
## makeCacheMatrix method and returns the same.
## It first checks if the inverse is aready calculated and if so returns it and skips rest of execution  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        dataMat <- x$get()
        inv <- solve(dataMat, ...)
        x$setInverse(inv)
        inv        
}
