## makeCacheMatrix creates a list object with several entrances
## set= get the matrix from parameter, get= you could retrive the set matrix
## getin= you could retrive the inverser matrix, setin= allocate the inverse matrix if posible.

## create matrix and matrix list.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inve) m<<-inve
  
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## give inverse matrix if was not created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  mymatrix <- x$get()
  m <- solve(mymatrix)
  x$setinv(m)
  m  
}
