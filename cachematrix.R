#  Lisa Nelson
#  R Programming 
#  Assignment 2
#  October 2015

# This module contains functions to invert and cache a matrix

# To Run: Enter the following on the command line
# > testMatrix <- matrix (1:4, nrow = 2, ncol = 2)     # create a matrix to test with
# > env <- makeCacheMatrix(testMatrix)                 # set up the environment with functions
# > cacheSolve(env)                                    # request performance of the calculation or pull from cache


#makeCacheMartrix accepts a matrix to invert using the "setinverse" function.
#other functions may be renamed from original, but provide the same functionality (get, set, etc)
makeCacheMatrix <- function(x = matrix()) {       
     m <- NULL             
     set <- function(y) {                            # store the matrix
          x <<- y
          m <<- NULL
     }
     get <- function() x                             
     setinverse <- function(inverse) m <<- solve(x)  #solve is used to find the inverse of a matrix
     getinverse <- function() m                      
     list(set = set, get = get,                      #Return a list of functions
          setinverse = setinverse,
          getinverse = getinverse)
}


#CacheSolve accepts the environment created by makeCacheMatrix
#It checks to see if it has already been invoked and performed the calculation
#If a value for the inversion is already stored, it brings back from cache
#If not, it performs the calculation
#The code is essentially unchanged from the original (functions are renamed)
cacheSolve <- function(x, ...) {
     m <- x$getinverse()        #checks for existing value
     if(!is.null(m)) {          #if it exists, get from cache and return 
          message("getting cached data")
          return(m)
     }
     data <- x$get()            #need to calculate
     m <- inverse(data, ...)    #invert the matrix
     x$setinverse(m)            #save the calculation      
     m                          #Return the matrix
}