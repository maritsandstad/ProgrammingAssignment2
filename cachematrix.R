## Functions to store in cache and get from cache (or solve) if not 
## stored, a matrix and its inverse

## makeCacheMatrix produces a list of the functions:
## set : which sets the value of the matrix in cache
## get : which gets the matrix stored in cache
## setInv: which sets the inverse matrix in cache
## getInv: which gets the inverse function from cache
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
        	x <<- y
        	inv <<- NULL 
        	
        }
        ##It is not clear that we strictly need the get function
        ## to fulfill the requirements for the assignement.
        ##However, I kept it for completion.
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function that gets the inverse of a matrix of the type produced
## by makeCacheMatrix.
## If the inverse has been found previously and x has not 
## changed, the inverse is taken from cache.
## Otherwise the (new) inverse is computed and this inverse
## is sent back to be stored in cache by the setInv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
        	message("getting cached inverse")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
