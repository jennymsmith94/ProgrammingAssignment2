## makeCacheMatrix and cacheSolve work together to compute, store and retrieve
## the inverse of a matrix, as well as the original matrix
####################################
## makeCacheMatrix creates a 4-item list that contains the functions "set",
##"get", "setinv", and "getinv". 
##      Ex: yourCachedMatrix<-makeCacheMatrix(yourMatrix)
##      "set": stores the matrix when makeCacheMatrix is initially run (?) 
##      "get": retrieves and prints the matrix after set-up.  
##              Ex: yourCachedMatrix$get()
##      "setinv": sets the cached matrix inverse. Used by cacheSolve
##      "getinv": retrieves and prints the cached matrix inverse
##              Ex: yourCachedMatrix$getinv()
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv<-function(solve) inv<<-solve
        getinv<-function() inv
        list(set=set, get=get,setinv=setinv,getinv=getinv)

}


## cacheSolve checks to see if an inverse is already cached and returns the 
##      cached value if present. If not, it will compute the inverse, set the
##      cached value, and then return the inverse
##      Ex: cacheSolve(yourCachedMatrix)
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
