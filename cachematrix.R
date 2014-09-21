## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

# initialize inverse matrix object  
  inv <- NULL

# define set function, which stores matrix internally AND resets inverse  
  set <- function(y) {
    x <<- y
# any change to the matrix x, which is updated into the matrix object via $set(m)
# also leads to a reset of the inverse matrix
    inv <<- NULL
  }


  get <- function() x

# store value of solve in object inv
  setinverse <- function(solve) inv <<- solve
# returns value of inverse
  getinverse <- function() inv
# defines matrix object as type list: try
# > mo <- makeCacheMatrix( matrix(1:4, nrow=2,ncol=2))
# > class(mo)
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse) 
  
}

## Write a short comment describing this function
# determines the inverse of a matrix via the solve function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

# get the inverse calculated earlier, reading from cache  
  inv <- x$getinverse()

# check if Inv-object is not NULL/empty, and return result
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }  

# inv-object is NULL, hence the following code is executed to solve matrix for inverse
# 1) get the intial matrix from cache
  data <- x$get()
# 2) determine the inverse via the solve function
  inv <- solve(data, ...)
# 3) store result in cache with the setinverse function
  x$setinverse(inv)
# 4) output result
  inv
  
}
