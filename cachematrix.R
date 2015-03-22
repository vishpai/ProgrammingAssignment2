## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.  Set of functions in this R file is an effort to cache the 
## inveser of the matrix in an object and consume it later to speed up 
## overall processing


## This function creates the inverse of matrix using solve function
## and stores the values in object m

makeCacheMatrix <- function(x = matrix()) 
  {
      m<-NULL
      set<-function(y)
        {
           x<<-y
           m<<-NULL
         }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
         #print (m)
      getmatrix<-function() m
      list(set=set, get=get,
      setmatrix=setmatrix,
      getmatrix=getmatrix)
  }


## This function obtains the inverse of the matrix if it is available in 
## object m if cached value pre-exists. 

cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
      if(!is.null(m))
        {
           message("getting cached data")
           #print (m)
           return(m)
        }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
  }

