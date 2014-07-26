## The below code will use caching to cumpute the invert of a matrix.
# It will get the value from cache if it has been computed already, else compute it and cache it

# This function creates a special matrix containing a function to:
#     - set the value of the matrix
#     - get the value of the matrix
#     - set the invert of the matrix
#     - get the invert of the matrix

makeCacheMatrix <- function(x = matrix()) {

      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
      
}


# The following function calculates the invert of the special "matrixr" created with the above function. 
# However, it first checks to see if the invert has already been calculated. 
#     - If so, it gets the mean from the cache and skips the computation. 
#     - Otherwise, it calculates the invert and sets the value of the inverted matrix in the cache via the setatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}
