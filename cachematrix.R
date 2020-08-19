
## Create a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get=get, setinv=setinv, getinv=getinv)
}


## Obtain the cached inverse of a matrix if it exists 
## Otherwise calculates solve(matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
