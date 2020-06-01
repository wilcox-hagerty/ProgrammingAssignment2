## For this assignment, assume that the matrix supplied is always invertible.
## solve() will return the inverse of a matrix.

## The makeCacheMatrix creates a matrix object, and can cache it's inverse.

## define matrix and set the default mode of "matrix"
makeCacheMatrix <- function(x = matrix()) {
        ## initialize 'i' as NULL. This will eventually store the matrix inverse
        i <- NULL 
        
        ## function to define a new matrix in the parent environment.
        set <- function(y){
                x <<- y
                i <<- NULL  ## if a new matrix is created, reset 'i' to NULL
        }
        
        ## function to return the matrix created by 'set'
        get <- function() x
        
        ## function to set inverse of matrix to variable 'i' in parent env
        ## Here 'solve' is just a variable for the function.
                ## you will need to pass it the solve() function on the matrix. 
        setinverse <- function(solve) i <<- solve
        
        ## function to return value of 'i'
        getinverse <- function() i
        
        ## list of functions set to names.
                ## Allows function to be referenced with '$' operator
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function will return inverse of matrix from makeCacheMatrix function.  
cacheSolve <- function(x, ...) {
        
        ## set 'i' with the getinverse() function from makeCacheMatrix. 
        i <- x$getinverse()
        
        ## If 'i' is not null, it will return the cached inverse. 
        ## If null, it will move to next line.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## next 4 lines will return the 
        ## inverse of the matrix from makeCacheMatrix
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

