## The functions cache inverse of a matrix

##makeCacheMatric basically enters the inverse of a matrix into it
##The function contains set and get functions to  set a matrix through a given matrix and get function returns back the matrix
##On the other hand, set and get inverse do the same for inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mat_inv <<- inverse
  getinv <- function() mat_inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function tries to retrive inverse of the matrix in case it is stored in cache
#In case it is not, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("getting cached result")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinv(mat_inv)
  mat_inv
}
