#' Class for functions to create and use a cached matrix wrapper object, which holds
#' a matrix and potentially its inverse matrix. For this class assume the matrix
#' is invertible
#' 
#' @description 
#' This class has two functions:
#' * makeCacheMatrix: Function to create a matrix wrapper with functions to access
#' a matrix and its inverse. Inverse matrix is null by default
#' * cacheSolve: Function to cache the inverse matrix in the cached matrix wrapper and
#' also return the inverse matrix



#' This function creates, a cached matrix wrapper, a matrix container that can 
#' contain a matrix and its inverse matrix
#' 
#' @param matrx A matrix
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
#'
#' @export
makeCacheMatrix <- function(matrx = matrix()) {
  inv_matr <- NULL
  set <- function(new_matrx) {
    matrx <<- new_matrx
    inv_matr <<- NULL
  }
  get <- function() { matrx }
  setinverse <- function(inverse) { inv_matr <<- inverse }
  getinverse <- function () { inv_matr }
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


#' Returns a matrix that is the inverse of the cached matrix objects matrix 
#' value. It also sets the inverse matrix in the cached matrix wrapper
#' @param matrx_wrap A cached matrix wrapper object that holds a matrix and its
#' inverse potentially, see makeCacheMatrix above
#' @return an inverse matrix of the cached matrix wrapper's matrix
#' @examples 
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
#'
#' @export
cacheSolve <- function(matrx_wrap, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matx <- matrx_wrap$getinverse()
  if (!is.null(inv_matx)) {
    message("getting cached inverse matrix")
    return(inv_matx)
  } else {
    message("creating cached inverse matrix")
    data <- matrx_wrap$get()
    inv_matx2 <- solve(data, ...)
    matrx_wrap$setinverse(inv_matx2)
    inv_matx2
  }
}
