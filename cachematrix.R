## One function produces a special matrix whilst also being able to cache the inverse of the matrix, and the other will calculate the inverse of the special matrix if 
## it hasn't been done already

## makeCacheMatrix will create a function that has 4 functions in it: set() will set the value of the matrix, get() will get the value of the matrix
## setInv() will set the cache containing the inverse of the matrix, getInv() will get the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
        Invx = NULL
        set <- function(y) {
                x <<- y
                Invx <<- NULL
        }
        get <- function() x
        setInv <- function(Inverse) Invx <<- Inverse
        getInv <- function() Invx
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve will check to see if the inverse of the matrix in "makeCacheMatrix()" has already been calculated, if it has, it will return the cached
## inverse, if not it will calculate it and return this

cacheSolve <- function(x, ...) {
        Invx <- x$getInv()
        if (!is.null(Invx)) {
                message("getting cached data")
                return(Invx)
        }
        getmatrix <- x$get()
        solution <- solve(getmatrix)
        x$setInv(solution)
        solution
}
