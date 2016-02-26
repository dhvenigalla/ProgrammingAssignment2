## Assignment :Caching the inverse of  a Matrix.
## Two funstions that cache the inverse of a matrix


## function 1:makeCacheMatrix.
##This function creates a special Matrix objext that can cache its invers.

makeCacheMatrix <- function(x = matrix()) {
        i <-NULL
        set <- function(y)
        {
                x<<-y
                i <<-NULL
        }
        get <-function()x
        setinverse <-function(inverse) i <<-inverse
        getinverse <-function() i
        list(set =set,
             get=get,
             setinverse =setinverse,
             getinverse=getinverse)
        
}


## This function computes the inverse of teh special Matrix returned by makeChacheMatrix above.
##if inverse has already been calculated ,then cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <-x$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
                
        }
        data <-x$get()
        i<- solve(data,...)
        x$setinverse(i)
        i
}

