## These functions (makeCacheMatrix & cacheSolve)
## are used to cache the inverse of a matrix.

## makeCacheMatrix is a function that creates a special
## matrix object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {  
   m <- NULL 
   set<-function(y){                      ## set the value of the matrix
     x<<-y
     m<<-NULL
   }

   get<-function() x                       ## get the value of the matrix
   setinverse<-function(solve) m <<-solve  ## use the solve function to set the value of the inverse
   getinverse<-function()m                 ## get the value of the inverse
   list(set=set, get=get,                  ## create a list for setting the matrix, getting the matrix
        setinverse=setinverse,             ## seting the inverse and
        getinverse=getinverse)             ## getting the inverse
}


## cacheSolve is a function that computes the inverse of the special matrix from
## makeCacheMatrix. If the inverse has already been calculated in makeCacheMatrix, 
## then cacheSolve gets the cached inverse instead of calculating the inverse again.

cacheSolve <- function(x, ...) {
                           ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){                     ## if m is not null, then the value was cached in makeCacheMatrix
      message("getting cached data")     ## and then returns the cached value of the matrix
      return(m)
    }
    data<-x$get()			## if m is null, the value was not cached. 
    m<-solve(data,...)			## uses solve to determine the inverse value
    x$setinverse(m)			## sets the value of the inverse
    m					## returns the value of the inverse
}
