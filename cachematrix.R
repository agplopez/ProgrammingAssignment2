
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


## Write a short comment describing this function

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
