## These functions' purpose is to calculate the inverse of a matrix
## if it has not yet been calculated, if it has, the function should
## recall it from the cache where it was saved and return it. This
## avoids having to calculate the inverse again.

## This function generates a list containing functions, which intend to
## set and get the value of the matrix, and to set and get the inverse 
## of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set=set, get = get, setinv = setinv, getinv = getinv)
}


##  This function returns the inverse of a matrix, but if first checks on 
## the cache defined abvove "getinv" and returns the last value saved in it
## if the cache is empty then it will calculate the invrse if the function.
## the x argument of this function is the list created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
                message("returning cache data")
                return(i)
        }
        matrix <- x$get()
        m <- solve(matrix,...)
        x$setinv(m)
        m
}
