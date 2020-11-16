#' Class for functions to create and use a cached matrix object, which holds
#' a matrix and potentially its inverse matrix
#' 
#' @description 
#' This class has two functions:
#' * makeCacheMatrix: Function to create a matrix wrapper with functions to access
#' a matrix and its inverse. Inverse is null be default
#' * cacheSolve: Function to cache the inverse matrix in the matrix wrapper and
#' also return the inverse matrix



#' This function creates, a cached matrix wrapper, a matrix container that can 
#' contain a matrix and its inverse matrix
#' 
#' @param x A matrix
#' @return A special matrix wrapper that includes its inverse. The object 
#' contains four functions:
#' * get(): returns matrix
#' * set(x): sets the matrix and nulls the inverse
#' * getinverse(): returns the inverse matrix if calculated, otherwise null
#' * setinverse(x): sets the inverse matrix
#' @examples
#' > matr <- matrix(c(4, 7, 2, 6), ncol=2) # test matrix
#' > matcache <- makeCacheMatrix(matr)
#' > matcache$get()
#'[,1] [,2]
#'[1,]    4    2
#'[2,]    7    6
#'> matcache$getinverse()
#'NULL
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { m <<- inverse }
  getinverse <- function () { m }
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


#' Returns a matrix that is the inverse of the cached matrix objects matrix 
#' value. It also sets the inverse matrix in the cached matrix wrapper for 
#' future use  
#' @param x A cached matrix, see makeCacheMatrix above
#' @return an inverse matrix of the cached matrix
#' @example 
#' > matr <- matrix(c(4, 7, 2, 6), ncol=2) # test matrix
#' > matcache <- makeCacheMatrix(matr)
#' > invmat <- cacheSolve(matcache)
#' > invmat
#'      [,1] [,2]
#'[1,]  0.6 -0.2
#'[2,] -0.7  0.4
#'#' > matcache$get()
#'[,1] [,2]
#'[1,]    4    2
#'[2,]    7    6
#'> matcache$getinverse()
#'      [,1] [,2]
#'[1,]  0.6 -0.2
#'[2,] -0.7  0.4
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  } else {
    message("creating cached inverse matrix")
    data <- x$get()
    m2 <- solve(data, ...)
    x$setinverse(m2)
    m2
  }
}
