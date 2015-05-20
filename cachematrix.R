## ============================================================
## 
## Caching the Inverse of a Matrix
## ____________________________________________________________
##
## Example of use:  
##
##        # Initialise matrix to be inverted
##        x <- matrix(c(1,2,3,4),2,2)
##        
##        # Create a matrix object b that can cache its inverse
##        b <- makeCacheMatrix(x)
##
##        # Compute the inverse of the matrix object b 
##        # (or return cached data after first call)
##        cacheSolve(b)
## ===========================================================

## Create a special "matrix" object that can cache its inverse
##------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        
        # Initialise inverse of x
        inv_x <- NULL
          
        # Get x value
        get <- function() x
        
        # Set inverse of x using the solve function 
        setInv <- function(solve) inv_x <<- solve
        
        # Get inverse of x
        getInv <- function() inv_x
        
        # Return a list containing the 3 functions
        list(get = get,
             setInv = setInv,
             getInv = getInv)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix()
## -------------------------------------------------------------------------
cacheSolve <- function(x, ...) {

        # Get current value of the inverse (expecting matrix or NULL)
        inv_x <- x$getInv()
        
        # If the inverse has already been calculated, 
        # indicate it to the user and return its value          
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        # Else compute the inverse, store it and return its value
        data  <- x$get()
        inv_x <- solve(data, ...)
        x$setInv(inv_x)
        inv_x
        
}
