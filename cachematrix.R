## Create a special Matrix "Object" with the following "methods":
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  mt<-NULL
  set <- function(y) {
    x <<- y # Note that "<<-" operator makes assignments one level up 
            # so we are assigning the value to a variable which is
            # defined at the caller's level
    mt <<- NULL
  }
  get <- function() x # Returns the matrix 
  setInverse <- function(solve) mt <<- solve
  getInverse <- function() mt
  list(set=set, get=get, 
       setInverse=setInverse,
       getInverse=getInverse)
}


## cacheSolve computes the inverse of a matrix if it has not been 
## computed yet (and then saves it as part of the caller's instance)
## otherwise returns the cached version 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mt <-x$getInverse() #query the caller's cached result first
  if(!is.null(mt)){
    return(mt) ## if this exists return the cached value
  }
  data <-x$get() ##otherwise get the matrix
  mt <- solve(data, ...) ## compute the inverse with solve()
  x$setInverse(mt) ## store it
  mt ## return it
}
