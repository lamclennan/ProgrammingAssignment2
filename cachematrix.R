## makeCacheMatrix and cacheSolve work together to create a  special matrix
## and cache its inverse to save compute time.

## makeCacheMatrix takes one matrix input and returns a list of functions
## essentially creating a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ##set  i to null
    i <- NULL
    
    ##create function set taking y as an input
    set <- function(y) {
        ##set x to y in the parent enviroment
        x <<- y
        ## set i to null in the parent enviroment
        i <<- NULL
    }
    
    ## create function get that returns x
    get <- function() x
    
    ## create function setinverse that takes inverse as an input
    ## and set i to inverse in the parent enviroment
    setinverse <- function(inverse) i <<- inverse
    
    ## create function getinverse that returns i
    getinverse <- function() i
    
    ## return a named list of our fuctions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a special matrix created with makeCacheMatrix
## and returns its inverse either from the cache or computes if not cached.

cacheSolve <- function(x, ...) {
    ## get the cached result
    i <- x$getinverse()
    
    ## if i/cached result is not null then
    if(!is.null(i)) {
        message("getting cached data")
        ## Return a cached matrix that is the inverse of 'x' and end
        return(i)
    }
    
    ## set data to the 
    data <- x$get()
    
    ##run solve on  data and assing output to i 
    i <- solve(data, ...)
    
    ## update the cache with the solved inverse
    x$setinverse(i)
    
    ## Return a solved matrix that is the inverse of 'x'
    i
}
