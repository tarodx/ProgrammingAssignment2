## This function creates a list type object with lenght 4.
## Each of the list elements is a function for set/get the
## original or the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize inv matrix: first time it has a NULL value
        inv <- NULL 
        
        ## Define set and get matrix functions
        
        ## Setmatrix
        setmatrix <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        ## Getmatrix: Return the original matrix
        getmatrix <- function() x
        
        ## Define set and get inverse matrix functions
        
        ## Setinverse
        setinverse <- function(solve) inv <<- solve
        
        ## Getinverse: Return the inverse matrix
        getinverse <- function() inv
        
        ## Define the list variable with all the available operations
        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of a defined matrix.
## If it is the first time the function is called, it calculates
## the inverse of the matrix and return its value, and cached it in memory.
## If the function is called more than one times, the function used the
## cache value and return it instead of calculated it again.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        ## Get the inverse of the "x" matrix by calling the getinverse function
        inv <- x$getinverse()
        
        ## If is not null "inv" then return the cached value
        if(!is.null(inv)){
                message("getting cached inverse")
                
                ## If the inverse is cached, return it instead of calculate it.
                return(inv)
        }
        
        ## Get the original "x" matrix by calling the getmatrix function
        data <- x$getmatrix()
        
        ## Set the inverse of the "x" matrix
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        ## Return the inverse of the matrix (if it is not in cache)
        inv
}