# Cachematrix.R defines two functions that will allow to invert a
# matrix and store the result in a cached matrix object
# These functions can be used to avoid computationally expensive 
# repeated re-calculations of the inverse of a matrix when the matrix 
# has not changed. 

# Functions tested using discussion post on Simple test matrices for 
# the lexical scoping programming assignment: https://www.coursera.org/
# learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg


## makeCacheMatrix defines an object that stores an original and 
## inverted matrix. In addition is defines and stores four functions to 
### - set the object (e.g. if the matrix has changed)
### - get the object (i.e. retrieve the original matrix)
### - set_m_inverse (i.e. invert the matrix and store)
### - get_m_inverse (i.e. retrieve the inverted matrix)
## Store function results as a named object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_m_inverse <- function(invert) m <<- solve(x)
  get_m_inverse <- function() m
  list(set = set, get = get,
       set_m_inverse = set_m_inverse,
       get_m_inverse = get_m_inverse)
}


## cacheSolve(object) checks if the matrix inverse was previously
## calculated and retrieve it from cache (if TRUE) or calculates it
## (if FALSE). It uses the matrix and functions stored in the object 
## generated from function makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_m_inverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_m_inverse(m)
  m
}
