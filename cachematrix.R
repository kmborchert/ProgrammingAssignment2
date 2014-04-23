# Coursera R Programming
# Programming Assignment #2 - Peer Assessment
# Kristen Borchert
# cachematrix.R

# Goal: Write a pair of functions that calculate and cache the inverse of a matrix.
# Function 1: `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
# Function 2: `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.  The inverse is calculated
# using the `solve` function in R

# Approach: Adapt the code from the class examples makeVector and cachemean to
# `makeCacheMatrix` and `cacheSolve`, respectively, swapping the vector for a matrix
# and the mean for solve (inverse matrix).


# `makeCacheMatrix` is composed of a list of functions which gets and sets the values of the matrix, and
# gets and set the values of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv.mat) inv <<- inv.mat
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)    
}


# 'makecacheSolve' will compute the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the 'makecacheSolve`
# will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {                 # This step is checking if the inverse is cached
    message("getting cached data")    
    return(inv)                       # This step returns the inverse from the cache
  }
  data <- x$get()                     # This step gets the matrix from above and...
  inv <- solve(data, ...)             # then computes the inverse matrix.
  x$setinv(inv)                       # This step caches the inverse and..
  inv                                 # returns it.                  
}


