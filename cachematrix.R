## These functions are used to print and cache the result of the inverse of squared matrices
## makeCacheMatrix
##	input: matrix
##	output: list of functions (set, get, setsolu, getsolu)
## cacheSolve
##	input: list
##	output: result of the inverse of a squared matrix


## Set the content of a matrix and the inverse of this one in another environment used to cache the## data.
## Get these data. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <- NULL
    }
    
    get <- function() x
    
    setsolu <- function(solu) m <<- solu
    
    getsolu <- function() m
    
    list(set=set, get=get, setsolu=setsolu, getsolu=getsolu)
}


## Check if the inverse of the matrix is in cache. Retreive it if existing or calculating it if is ## not in cache. The result is then printed. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    z <- x$getsolu()
    if(!is.null(z)) {
        message("Getting matrix from cache")
        return(z)
    }
    matrix <- x$get()
    z <- solve(matrix, ...)
    x$setsolu(z)
    z
}
