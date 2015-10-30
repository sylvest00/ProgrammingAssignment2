########################################################## 
##
## R Programming Course- Programming Assignment 2
##
## The goal of programming assignment 2 is to demonstrate
## lexical scoping by accessing a cached value, the
## inverse of a matrix. A repo for the assignment
## (https://github.com/rdpeng/ProgrammingAssignment2) was
## forked, cloned, edited locally, and pushed onto my
## GitHub account (https://github.com/sylvest00).
##
##########################################################


## =======================================================
## makeCacheMatrix creates a special "matrix" object (a
## list) that can cache its inverse, inv. It also allows the
## existing matrix to change after being initialized.
## =======================================================

makeCacheMatrix <- function(x = matrix())
{
  
  inv   <- NULL;                                            #Set inverse to NULL upon initialization
  
  reinitializeMatrix <- function(newMatrix)                 #change matrix "x" after declaring it outside makeCacheMatrix and "resets" the inverse
  {                     
    x   <<- newMatrix;
    inv <<- NULL;
  }
  
  getMatrix   <- function() x                               #gets the matrix via a function to allow easy changes to the list
  setInverse  <- function(inverseVal) inv <<- inverseVal    #sets locally declared inverseVal to the global inv value
  getInverse  <- function() inv                             #gets the inverse via a function to allow for easy access to the list
  
  list(                                                     #make a "matrix object" (a list) that contains all relevent information/ functions for easy manipulation of data
        reinitializeMatrix = reinitializeMatrix,
        getMatrix  = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse
      )
}




## =======================================================
## cacheSolve computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the
## inverse has already been calculated and the matrix
## is unchanged, then cacheSolve retrieves the inverse
## from the cache. Otherwise, the inverse is calculated
## and set to the list "x".
## =======================================================

cacheSolve <- function(x, ...)
{
  inv <- x$getInverse();
  
  if(!is.null(inv))                                         #if the inverse, stored in inv, is not NULL, access inv from the cache & return. Otherwise, calculate the inverse using solve in the lines below
  {
    message("Getting Cached Inverse...");
    return(inv);
  }
  
  xMatrix <- x$getMatrix();                                 #get the matrix stored in list "x"
  inv <- solve(xMatrix);                                    #find the inverse of the matrix
  x$setInverse(inv);                                        #set the inverse, "inv", to the "setInverse" key/ field in list "x"
  inv;                                                      #output inv
}
