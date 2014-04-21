## This function will create a special matrix, which is really a list containing a function to
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse
##     get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                #initialize the stored inverse value to NULL
    set <- function(y) {                     #set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                      #get the value of the matrix
    setinverse <- function(inv) m <<- inv    #set the inverse
    getinverse <- function() m               #get the inverse
    list(set = set, get = get,               #return a list of the functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'. 
## If it's in cache, it will use that otherwise it will recalculate the values.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()                 #query the x vector's cache         
    if ( !is.null(m) ) {                #if there is cached data
        message("getting cached data") 
        return(m)                       #just return the cache, no computation needed
    }                                   
    data <- x$get()                     #no cache, get data
    m <- solve(data, ...)               #inverse the matrix
    x$setinverse(m)                     #save the result back to x's cache
    m                                   #return the result
}
