makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <- y
    invrs <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <- inverse
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  invrs <= x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat, ...)
  x$setinverse(invrs)
  invrs
}

#xixibie945, GOGODRDOG, Baescott, martinsamuelm, fcbaker49, taolucks, reek129, Eduardopribeiro, weiyizz, rb715, and 3 more reacted with thumbs up emoji

#@Roslad
#Roslad on 25 Oct 2016
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
 # Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()


#@sunvarg
#sunvarg on 25 Oct 2017
#Assignment: swirl Lesson 2
#makeCacheMatrix creates a list containing a function to
#set/get value of matrix and set/get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}

#The below fn returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  invr <= x$getinverse()
  if(!is.null(invr)) {
    message("Getting Cached Data:-")
    return(invr)
  }
  matrx <- x$get()
  invr <- solve(matrx, ...)
  x$setinverse(invr)
  invr
}


#@rozenv1
#rozenv1 on 5 May 2019
#Week 3 Assignment by Veronica Rozendaal. Purpose is to write a pair of
#functions that cache the inverse of a matrix using Lexical Scope concepts.
#The two functions created are: makeCacheMatrix and cacheSolve
#makeCacheMatrix function creates a special "matrix" object that can cache
#its inverse or store the matrix and its inverse.
#First initialize the two objects x and n1, where x is the argument to be
#passed when testing in makeCacheMatrix for the matrix and n1 will be used
#later in the function for the matrix inverse.
#makeCacheMatrix <- function(x = matrix()) {
  n1<-NULL
  ## define the 4 different behaviors or functions for object of type
  ## makeCacheMatrix as follows:
  ## 1. set() takes an argument named m1 or any object name other
  ## than x. m1 is assumed to be a matrix.
  set<-function(m1){
    ## Use the <<- operator to assign a value to an object in an
    ## environment that is different from the current environment.
    ## When set executes, first, this assigns the input argument
    ## to the x object in the parent environment,
    x<<-m1
    ## and second, this assigns a value of NULL to the n1 object
    ## in the parent environment.
    ## this line, clears any value of n1 that had been cached by a
    ## prior execution of cacheSolve()
    n1<<-NULL
  }
  ## 2. getter for the matrix x:
  get<-function()x
  ## 3 the setter for the inverse n1
  setinverse<-function(solve) n1<<-solve
  ## 4 the getter for the inverse n1
  getinverse<-function()n1
  ## Create the new special "matrix" object by returning a list()
  ## to assign each of these functions as an element within a list ()
  ## and returns it to the parent environment. Here we're naming the
  ## list elements, which allows to use the $ form of the extract
  ## operator to access the functions by name rather than by [[
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)


#Write a short comment describing this function
#cacheSolve requires an argument that is returned by makeCacheMatrix()
#in order to retrieve the inverse from the cached value that is stored in the
#makeCacheMatrix() object's environment. cacheSolve computes the inverse
#of the special "matrix" returned by makeCacheMatrix above.
#if the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## the function attempts to retrieve an inverse from the matrix
## object passed in as the argument. First, it calls the getinverse()
## function on the input object.
n1<-x$getinverse()
## then it checks to see whether the result is NULL. Since
## makeCacheMatrix() sets the cached inverse to NULL whenever a new
## matrix is set into object, if the value here is not equal to NULL,
## we have a valid, cached inverse and can return it to the parent
## environment
if (!is.null(n1)){
message("getting cached data")
return (n1)
}
## if the result of !is.null(n1) is FALSE, cacheSolve() gets the matrix
## from the input object, calculates the solve(), uses the setinverse()
## function on the input object to set the inverse in the input object,
## and then returns the value of the inverse to the parent environment
## by printing the inverse object
data<-x$get()
n1<-solve(data)
x$setinverse(n1)
n1
}

