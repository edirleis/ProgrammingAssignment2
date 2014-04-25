## code created as partial completion for Coursera class: R programming
## author: edirleis
## matrix inversion requires heavy computation, specially for large size matrices 
## this code caches in memory the solution for the inverse of a matrix 
## 
## The process is done in two functions:
## 1. makeCacheMatrix is reponsible to the value of a x matrix and its corresponding inverse. 
##    It makes these values accessible by a list of functions
## 2. cacheSolve is reponsible to return the solution of the inverse of x
##    when the inverse is needed first we look into the cache, if it is there retrieve it, if not calculate and cache it   

## makeCacheMatrix receives a matrix x and returns a list of functions
## the list of functions are setters and getters for x and the inverse of x
makeCacheMatrix <- function(x = matrix()) {
  #receives x and returns set and get for x, and setinverse and getinverse for the inverse of x
  inverse <- NULL                   #initial state of inverse is NULL
  set <- function(y) {              #function set stores x
    x <<- y
    inverse <<- NULL                #inverse is NULL every time new x is stored
  }
  get <- function() x               #get returns original matrix data
  setinverse <- function(y) {       #stores solved inverted version of matrix
    inverse <<- y
  }  
  getinverse <- function() inverse  #returns the solved version from cache
  list(set = set, get = get,        #a list of functions is returned
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve a matrix x and returns the its inverse
## first it verifies if there is a solution on cache, if not calculate the inverse and store it on cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()          #retrieve inverse data from cache
  if(!is.null(inverse)) {            #if inverse is not null, it has been already solved
    message("getting cached data")   #notify that returning value comes from cached version 
  }
  else { #else means the inverse matrix needs to be calculated and result send to the cache
    
    data <- x$get()                   #get original matrix data out of the cache
    inverse <- solve(data, ...)       #calculate the inverse matrix 
    x$setinverse(inverse)             #send result to the cache
  }
  return(inverse)                     #just return the result
}
