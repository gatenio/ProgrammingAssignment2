#Peer Assessments /Programming Assignment 2: Lexical Scoping
#???????????????????????????????????????????????????????????
## Put comments that give an overall description of what your
## functions do
#-----------------------------------------------------------------------
## Write  comments describing this function-makeCacheMatrix & cacheSolve
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#1.keCacheMatrix: This function creates a special "matrix" object that
#can cache its inverse.
#2.cacheSolve: This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve
#should retrieve the inverse from the cache.
#the code is base on the makeVector() & cachemean() patterns
#===========================================================
#makeVector <- function(x = numeric()) {
#  m <- NULL
#  set <- function(y) {
#    x <<- y
#    m <<- NULL
#  }
#  get <- function() x
#  setmean <- function(mean) m <<- mean
#  getmean <- function() m
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)
#}

#cachemean <- function(x, ...) {
#  m <- x$getmean()
#  if(!is.null(m)) {
#    message("getting cached data")
#    return(m)
#  }
#  data <- x$get()
#  m <- mean(data, ...)
#  x$setmean(m)
#  m
#}

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve<- function(solve) s <<-solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve= getsolve)
}

#----------------------------------------------------------------------------

## The  cacheSolve function, returns the inverse of a matrix  created 
## by makeCacheMatrix().
## If the cached inverse matrix is available, cacheSolve retrieves while if 
## not, it computes the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix which is  inverse of 'x'.Solve() 
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached inverse matrix")
    return(s)
  } else {
    s <- solve(x$get())
    x$setisolve(s)
    return(s)
  }
}

#x <- array(1:4, dim=c(2,2)) # x ivertible matrix
#x
#solve(x)










