## Matrix inversion is a time consuming computational problem 
## The below  2 functions provide solution to this, by caching the matrix inversions.

## makeCacheMatrix generates a list containing the following functions 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of a matrix
## 4.get the value of the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  
    ## initialize a matrix to NULL
    matrixInv <-NULL
    
    ## define the set function
    set <- function(y) { 
           x <<- y 
         matrixInv <<- NULL 
    } 
    
    
     ## define the get function
     get <- function() x 
     
     ## define the setinverse function for caching
     setinverse <- function(inverse) matrixInv <<- inverse 
     
     ## define the getinverse function
     getinverse <- function() matrixInv 
     
     ## return the function list
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## cacheSolve caches and returns the  inverse of a matrix 
## It first checks if the inverse of the matrix exists in the cache, if yes then returns the inverse.
## If not, it computes the inverse , caches the inverse and then returns it.

## Assumption : the matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    ## get the inverse of the matrix from cache
    matrixInv <- x$getinverse()
    
    ## Check if the inverse of the matrix exists in cache.
    ## if yes , then return the cached inverse and exit the function.
    
    if(!is.null(matrixInv)) {
      message("getting cached data")
      return(matrixInv)
    }
    
    ## If not, get the data 
    data <- x$get()
    
    ## compute the inverse of the matrix by using solve(x) 
    matrixInv <- solve(data)
    
    ## set the cache to the computed inverse matrix
    x$setinverse(matrixInv)
    
    ## return the inverse matrix
    matrixInv
  
}
