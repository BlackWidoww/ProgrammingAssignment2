## Caches the inverse of a matrix once it has been calculated.

makeCacheMatrix<-function(x=matrix())  {
  m<-NULL
  set<-function(y)  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m <<-solve
  getsolve<-function() m
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

##  Retrieves the cached matrix inverse, else calculates matrix inverse directly

cacheSolve<-function(x,...) {
  m<-x$getsolve()                # Returns cached inverse if available
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()                  # Calculates inverse if not available
  m<-solve(data, ...)
  x$setsolve(m)
}
