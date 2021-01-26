##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                   }
  get<-function() {x}                #function to get matrix x
  setinv<-function(inverse) {inv<<-inverse}
  getinv<-function()  {inv}
      
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This is used to get the cached data

cacheSolve <- function(x, ...) 
  {
  inv<-x$getinv()
  if(!is.null(inv)){                    #checking whether inverse is null
                      message("obtaining cached data")
                      return(inv)                         #returns
  }
  mat <- x$get()
  inv<-solve(mat,...)                  #solves inverse value
  x$setinv(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
