## This program will cach the inverse of a matrix rather than compute it repeatedly

## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
      #  set the value of the matrix
       set <- function(y) {
              x <<- y
              i <<- NULL
       }
       
     # get the value of the matrix
       get <- function() x

     #  set the value of inverse of the matrix
    
       setinverse <- function(inverse) i <<- inverse
    
      # get the value of inverse of the matrix
      
      getinverse <- function() i
      
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    	i <- x$getinverse()
    	if(!is.null(i)) {
        message("Caching data!!")
        return(i)
   	   }
    	data <- x$get()
    	inv <- solve(data)
    	x$setinverse(i)
    	i
}


# testing
  
# > x= rbind(c(10, 3), c(9, 2))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]   10    3
# [2,]    9    2


# > cacheSolve(m)
# NULL

# > cacheSolve(m)
# Caching data!!
# [,1] [,2]
# [1,]   10    3
# [2,]    9    2

