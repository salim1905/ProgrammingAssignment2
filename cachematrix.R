
## This function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        ## "i" will store inverse of matrix, initialize it to NULL in first step
        i <<- NULL
        
        ## set the value of the matrix
        set <<- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse of matrix
        setinverse <- function(inverse) i <<- inverse
        
        # get the value of the inverse of matrix
        getinverse <- function() i
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips the computation. 
## Otherwise, it calculates the invere of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
        ## "i" will store inverse of matrix, load the "i" by get,inverse() function
        i <- x$getinverse()
        
        ## checks to see if the inverse has already been calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## get the data of the matrix and calculate the inverse of the matrix
        data <- x$get()
        i <- solve(data)
        
        ## set the inverse of the matrix by setinverse() function
        x$setinverse(i)
        
        ##return "i"
        i
}