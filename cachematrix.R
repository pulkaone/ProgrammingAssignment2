# ======================================================================
# Coursera
# R_Programming
# week03 - Assignment (2019-06-03)
# ======================================================================
#-----------------------------------------------------------------------
# Libraries
#-----------------------------------------------------------------------
library(tidyverse)
library(magrittr)


#-------------------------------
# 1) Solution
#-------------------------------
# This makeCacheMatrix() function creates a list  of another functions 
# that are called from the cacheSolve() function :D
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # -- The set() function, from the vector example, is not used in further calls, 
  # so technically unnecessary
  # -- + commented the listing of the set() function within the list below
  
  # set <- function(y){
  #   x <<- y
  #   inv <<- NULL
  # }
  get <- function(){x}
  setinv <- function(solved_inversion){inv <<- solved_inversion}
  getinv <- function(){inv}
  
  
  list(
    #set = set,
    get = get,
    setinv = setinv,
    getinv = getinv)
  
}



cacheSolve <- function(x, ...) {
  # 0.1) Search and attach the cached inverted matrix. 
  # This attachment cannot be successful for the first call of the cacheSolve() function, 
  # because there cannot be anything in cache to be found.
  inv <- x$getinv()
  
  # 0.2) If we already have cached inverted matrix (for at least the second call), then 
  # write a message that we already have it and...
  # ... use it => return the cached inverted matrix, i.e. the end of call/function, 
  # no further code is going to be executed. 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # 1) If we do not have the inverted matrix cached yet, then
  # call the function get() that gets the original matrix...
  data <- x$get()
  # 2) ... Create an inverted matrix by using the solve() function.
  inv <- solve(data, ...)
  # 3) When we have the inverted matrix now, put it in cache,
  # i. e. call the set.inv() function that creates
  # an object 'inv' within the environment of this cacheSolve() function, 
  # so it can be "found in cache" with the repeated call of the cacheSolve() function,
  # => the step 0.1)
  x$setinv(inv)
  # 4) Return the "newly created" inverted matrix
  inv
  
}

# -----------
# Check
# -----------
bm <- matrix(c(1,3,7,2), byrow = FALSE, ncol=2)
bi <- makeCacheMatrix(bm)
cacheSolve(bi)


# -----------
# Check the Check against "simple" solve() function
# -----------
# bm
solve(bm)

