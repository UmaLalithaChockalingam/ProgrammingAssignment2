# --------------------------------------------------------------------------------------------------
# Assignment          : Caching the Inverse of a Matrix
# Overall Description : A pair of functions that cache the inverse of a matrix
# --------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------
# Function Name : `makeCacheMatrix`
# Description   : This function creates a special "matrix" object that can cache its inverse.
# Date          : 20 Sep 2014
# Author        : Uma Lalitha Chockalingam
# --------------------------------------------------------------------------------------------------
makecacheMatrix <- function(x = matrix())
{
  ## Initialize the inverse property
  inverseOfMatrix <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inverseOfMatrix<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) inverseOfMatrix<<- inverse #Code to assign and cache Inverse of the matrix 
  
  ## Method to get the inverse of the matrix
  getInverse <- function() inverseOfMatrix
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

# --------------------------------------------------------------------------------------------------
# Function Name : `cacheSolve`
# Description   : This function computes the inverse of the special
#                 "matrix" returned by `makeCacheMatrix` above. If the inverse has
#                 already been calculated (and the matrix has not changed), then 
#                 `cachesolve` retrieves the inverse from the cache.
# Assumtion     : Input is always a invertible matrix.
# Date          : 20 Sep 2014
# Author        : Uma Lalitha Chockalingam
# --------------------------------------------------------------------------------------------------
cacheSolve <- function(x,...)
{
  #Retrive value stored as Inverse of the matrix.
  inverseOfMatrix<- x$getInverse()
  
  #Returns the valid data for Inverse of the matrix stored, If any.
  if(!is.null(inverseOfMatrix)) 
  {
    message("getting cached data")
    return(inverseOfMatrix)
  }
  
  #Matrix data retrived from special object  
  data <- x$get()
  
  #Fresh value for Inverse of matrix is computed and stored in special object
  message("getting fresh data")
  inverseOfMatrix<- solve(data)
  x$setInverse(inverseOfMatrix)
  
  #Computed Inverse of matrix is returned
  inverseOfMatrix
  
}

#--------------------------------------------------------------------------------------------------
# SAMPLE OUTPUT :
#    > Source('D:/R-pwd/LexicalScoping.R')
#     > n
#     [,1] [,2]
#     [1,]    1    3
#     [2,]    2    4
#     > mobj <- makecacheMatrix(n)
#     > cacheSolve(x = mobj)
#     getting fresh data
#     [,1] [,2]
#     [1,]   -2  1.5
#     [2,]    1 -0.5
#     > cacheSolve(x = mobj)
#     getting cached data
#     [,1] [,2]
#     [1,]   -2  1.5
#     [2,]    1 -0.5
#     > 
#-------------------------------------------------------------------------------------------------- 