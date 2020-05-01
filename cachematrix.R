
## this function makes a list consists of getter and setter functions 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                  # sets m equal to null
  set <- function(y) {                       # defines set function
    x <<- y
    m <<- NULL
  }
  get <- function() x                        # defines get function
  setsolve <- function(solve) m <<- solve    # defines setsolve function which assigns the solve in the cache
  getsolve <- function() m                   # defines getsolve function which accesses the solve from the cache
  list(set = set, get = get,                 # lists all the function defined
       setsolve = setsolve,
       getsolve = getsolve)
}


## this function uses the list to call the inverted matrix from cache

cacheSolve <- function(x, ...) {             # takes the list as input
m <- x$getsolve()                            # assigns the solve from list
  if(!is.null(m)) {                          # executes when there is already a solve
    message("getting cached data")
    return(m)
  }
  data <- x$get()                            # assigns the matrix in data
  m <- solve(data, ...)                      # solves the matrix
  x$setsolve(m)                              # assigns the solve in m
  m
}
