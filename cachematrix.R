# makeCacheMatrix: This function creates a special "matrix"
# object that can cache its inverse
makeCacheMatrix <- function(x = matrix())
{
  # initialization of the invese matrix
  inv <- NULL
  
  # setting the matrix's value
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  # getting matrix's value
  get <- function() x
  
  # setting ind getting inverse's values
  setInverse <- function(inputInverse) inv <<- inputInverse
  getInverse <- function() inv
  
  # returning a list with the functions generated on this function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



# cacheSolve: This function computes the inverse of the special
# "matrix" returned by makeCacheMatrix above. If the inverse has
# already been calculated (and the matrix has not changed), then
# the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...)
{
  # checking that there is no inverse yet
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached inverse")
    return(inv)
  }
  
  # ...continuiing with the process in the oposite case
  inverse <- x$get()
  inv <- solve(inverse, ...)
  
  # placing the inverse matrix and returning the result
  x$setInverse(inv)
  inv
}

