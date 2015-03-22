##Overall Descirption:
## makeCacheMatrix function is a list that stores a matrix and its inverse
## cacheSolve computes the inverse of the matrix made by using the makeCacheMatrix function.
##---------------------------------------------------

## "makeCacheMatrix" function creates a list of setter/getter functions of an input matrix and its inverse.
## Available functions of the makeCacheMatrix are:
## set: sets value of the matrix.
## get: gets value of the matrix.
## setinv: sets value of the inverse matrix.
## getinv: gets value of the inverse matrix.

##Sample :
# 1) Create a sample invertible square matrix: 
#    smatrix<-rbind(c(1,2,7),c(3/4,5,-2),c(1,4/10,-2))
# 2) Make a cache matrix using makeCacheMatrix: 
#    mat<-makeCacheMatrix(smatrix)
# 3) Get the matrix using get() function:
#    mat$get()
# 4) Get the inverse of the matrix and set it to 'inverse' variable:
#    inverse <-solve(mat$get)
# 5) set the inverse matrix to list object mat
#    mat$setinv(inverse)
# 6) get the cached inverse matrix from list object mat:
#    mat$getinv()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y){ # 'y' is the matrix argument passed into makeCacheMatrix
    x <<-y
    inv <<- NULL
  }
  get <-function ()x
  setinv <-function(solve) inv<<-solve
  getinv <-function() inv
  
  #List of the values of the functions 
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
  
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated  then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv           # Returns a matrix that is the inverse of 'x'
        
}
