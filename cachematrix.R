## The functions serve to calculate the inverse of a matrix and either by retrieving the cached result of a previous 
## calculation or by calculating the inverse if no previous result is cached and storing it in chache.



#The function makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


###The  function cacheSolve returns the inverse of a matrix after a  special "vector" has been created with the makeCacheMatrix. 
#It first checks to see if the inverse has been calculated previously . If so, it gets the Inverse 
##from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the 
##value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
