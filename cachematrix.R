
## Store and retrieve the matrix and the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Check if the inverse is already in cache
##, if not calculate the inverse, store it in cache and 
##returns its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##To test:
##>  m1 <- matrix(c(1,0,5,2,1,6,3,4,0),ncol=3,nrow=3)
##>  m2 <- makeCacheMatrix(m1)
##>  cacheSolve(m2)
##getting cached data
##[,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
