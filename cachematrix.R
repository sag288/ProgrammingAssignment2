#These functions take an input matrix, x, and create a convenient way to store the inverse of x if it has already been computed
#This will save time if the inverse matrix is needed a lot

# creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #this is the cached inverse matrix; there is no cached inverse matrix at creation, so set to null
  cached_inv_mtrx <- NULL
  
  #updates the matrix; this automatically sets cached_inv_mtrx to null
  set <- function(y) {
    x <<- y
    cached_inv_mtrx <<- NULL
  }
  
  #returns the matrix
  get <- function() x
  
  #sets cached_inv_mtrx to new value (inv_mtrx)
  set_inv_mtrx <- function(inv_mtrx) cached_inv_mtrx <<- inv_mtrx
  
  #returns cached_inv_mtrx
  get_inv_mtrx <- function() cached_inv_mtrx
  
  #this is the 'return'; this object is actually a list
  list(set = set, get = get,
       set_inv_mtrx = set_inv_mtrx,
       get_inv_mtrx = get_inv_mtrx)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #get cached_inv_mtrx
  inv_mtrx <- x$get_inv_mtrx()
  
  #if inv_mtrx is not null, then return inv_mtrx (this is a cached value)
  if(!is.null(inv_mtrx)) {
    message("getting cached data")
    return(inv_mtrx)
  }
  
  #get matrix
  data <- x$get()
  
  #solve for inverse of matrix x
  inv_mtrx <- solve(data, ...)
  
  #store inv_mtrx in x$cached_inv_mtrx using set_inv_mtrx 
  x$set_inv_mtrx(inv_mtrx)
  
  #return
  inv_mtrx
}
