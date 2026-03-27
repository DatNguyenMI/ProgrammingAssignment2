## Put comments here that give an overall description of what your
## functions do

## this will store a matrix in x and the inverse is set to Null intiially
## once a new matrix is pass to makeCacheMatrix, the matrix will be stored in x, similarly 
##inverse of the matrix if found can be stored in m

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL 
    set <- function(y) { 
        x <<- y 
        m <<- NULL 
    } 
    get <- function() x 
    setsolve <- function(solve) m <<- solve 
    getsolve <- function() m 
    list(set = set, get = get, 
        setsolve = setsolve, 
        getsolve = getsolve)
}

## cacheSolve takes the matrix stored in MakeCachematrix
## if inverse value is already set, it returns message"getting cached data" and return 
## saved inverse matrix value stored in m
## if inverse value is not set yet, it will get the matrix from x using x$get
## it then calculate inverse of x using function solve and return inverse matrix m
## it will store m in cache via x$setsolve and also return m in the console as inverse
## matrix of x

cacheSolve <- function (x,...) {
    m <- x$getsolve()
    if (!is.null(m)) {
        message ('getting cached data')
        return (m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setsolve (m)
    m
}
        ## Return a matrix that is the inverse of 'x'

