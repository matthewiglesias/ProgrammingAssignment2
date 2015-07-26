## Matthew Iglesias
## 26Jul2015


##This function creates a special "matrix" object that can cache its inverse
##This object contains four functions that enable the special matrix, to
## set its inverse in the cache and query it when necessary. 
## This function use the base structure from the example provided but 
## stores the inverse instead of the mean.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y  
        m <<- NULL 
    }
    get <- function() x 
    setinverse <- function(solve) m <<- solve 
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
}


##This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
## This function use the base structure from the example provided but 
## calculates the inverse instead of the mean.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse() 
    
    #verified if there is a cached inverse value
    # and return cache value if true
    if(!is.null(m)){                    
        message("getting cached data")    
        return(m)                           
    }
    #get the x matrix from the above makeCacheMatrix function
    #calculate the inverse and set its value.
    #return the inverse matrix of x
    data <- x$get()                     
    m <- solve(data, ...)               
    x$setinverse(m) 
    m
}