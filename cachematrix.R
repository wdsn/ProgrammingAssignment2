## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        
        
        ## test the input to ensure the input is a matrix
        
        if(!is.matrix(x)){
                stop("Error: the input is not a matrix")
        }
        
        minv <- NULL ## create minv as a NULL variable
        
        set <- function(y) { ## create the set function to set x & also create the minv as NULL
                
                x <<- y
                minv <<- NULL
                
        }
        
        get <- function() {
                
                x
                
                } ## get the vlaue of x
        
        setinv <- function(inverse) {
                
                minv <<- inverse ## returns the calculed inverse (in cacheSolve function)
                
        }
        
        getinv <- function() { ## returns minv
                
                minv
                
        }
        
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)       
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        minv <- x$getinv() # returns the stored minv
        
        if(!is.null(minv)) { # checks if minv is NULL, if not returns minv
                
                message("getting cached data")
                return(minv)
                        
        }
        
        # solve inverse of matrix
        
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
        
}

