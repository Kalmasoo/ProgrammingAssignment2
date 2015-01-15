## Put comments here that give an overall description of what your
## functions do

## First check if the input is matrix or not
## then check if it is square and invertible
## if so, then make the special matrix object

makeCacheMatrix <- function(x = matrix()) {
  
  if(class(x) != "matrix"){
    stop ("Data is not as matrix!")
  }
  if(nrow(x) == ncol(x)){
    if(det(x) == 0)
      stop ("The matrix is NOT invertible! because determinant of the matrix is ZERO. ")
    else {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function(){
        x
      }
      setInverse <- function(inverse){
        inv <<- inverse
      }
      getInverse <- function(){
        inv
      }
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    }
  } else {
    stop ("NOT square matrix :(")
  }
}


## This function inverse the special cached matrix and checked
## if it is cached return its content otherwise inverse it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data...")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
