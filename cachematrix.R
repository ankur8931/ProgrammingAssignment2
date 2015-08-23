##Name: Coursera User                                Date: 8/22/2015
##Program to calculate inverse of the matrix using caching mechanism

##This function is based on example function provided. It takes as input a matrix
##It has four subparts get,set, getinverse, setinverse
## Precondition is that matrix should be square and non-singular

makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 
 #Solve function is inbuilt function to get inverse
 
 setinverse <- function(solve) m <<- solve
 getinverse <- function() m
 list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## cache solve function will take as input a matrix and if there
## is cached value present for that matrix it will be printed 
## otherwise cached value will be stored and printed on next call to function.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


##Sample Output
#> source('C:/Users/achaud16/ProgrammingAssignment2/cachematrix.R')
#> B=matrix(c(1,12,13,4,8,6,17,4,9), nrow=3, ncol=3)
#> m=makeCacheMatrix(B)
#> cacheSolve(m)
#[,1]        [,2]        [,3]
#[1,] -0.06666667 -0.09166667  0.16666667
#[2,]  0.07777778  0.29444444 -0.27777778
#[3,]  0.04444444 -0.06388889  0.05555556
#> cacheSolve(m)
#getting cached data
#[,1]        [,2]        [,3]
#[1,] -0.06666667 -0.09166667  0.16666667
#[2,]  0.07777778  0.29444444 -0.27777778
#[3,]  0.04444444 -0.06388889  0.05555556
