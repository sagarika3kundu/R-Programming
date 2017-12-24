## Caching the inverse of a matrix

##This function makes the special "matrix" which actually is a list of 
##functions used by various purposes of the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function(){
                x        
        }
        setinv <- function(inv){
                m <<- inv
           }
        getinv <- function(){
                m        
        }
        list(set = set, get = get,
             setmean = setinv,
             getmean = getinv)
}


## This function checks whether the inverse exists in the cache.
##If yes, it returns the value
##else calculates inverse and returns the result and also stores the result in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ##using the solve function
        m <- solve(data)
        x$setinv(m)
        m
}
