##cache the inverse of a matrix




##This function creates a matrix object and can cache the inverse
makeCacheMatrix <- function(x = matrix()){
        cache <- NULL
        set <- function(y) {
                x <<- y 
                cache <<- NULL
        } 
        get <- function() x
        setMatrix <- function(inverse) cache <<- inverse
        getInverse <- function() cache
        list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}

##this function computes the inverse of the matrix returned
##cacheSolve calculates the inverse of the matrix created in makeCacheMatrix


cacheSolve <- function(x, ...){
        cache <- x$getInverse()
        if (!is.null(cache)){
                message("getting cached data")
                return(cache)
                
                
        }
        matrix <- x$get()
        
##testing to handle exceptions 
        
        tryCatch( {
                cache <- solve(matrix, ...)
                
        },
        error = function(e) {
                message("Error:")
                message(e)
                return(NA)
        }, 
        warning = function(e){
                message("Warning:")
                message(e)
                return(NA)
        },
        finally = {
                x$setMatrix(cache)
        })
        return(cache)
        }

