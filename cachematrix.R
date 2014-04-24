## Caches the inverse of a matrix once it has been calculated.

makeCacheMatrix<-function(x=matrix())  {      #input a matrix, x.  Output is a list containing set, get,
                                              # getsolve, and setsolve
  m<-NULL                    # creates empty value
  set<-function(y)  {        # inline function "set"
    x<<-y
    m<<-NULL
  }
  get<-function() x          # __$get() returns input matrix   
  setsolve<-function(solve) m <<-solve
  getsolve<-function() m
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

##  Retrieves the cached matrix inverse, else calculates matrix inverse directly

cacheSolve<-function(x,...) {
  m<-x$getsolve()                # Looks up cached inverse if available
  if(!is.null(m)) {
    message("getting cached data")    # Message returned if cache is found
    return(m)                         # Returns cached value
  }
  data<-x$get()                  # Calculates inverse if not available
  m<-solve(data, ...)
  x$setsolve(m)
}
