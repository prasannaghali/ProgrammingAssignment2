###############################################################################
#
# @file: cachematrix.R
# @author: pghali@gmail.com
# @date: 06/16/2015
#
# @function: makeCacheMatrix
# @parameter: matrix that is to be cached along with its inverse
# the default value is NULL.
# @return: Special matrix object consisting of a list of function plus a
# cached matrix and the cached inverse matrix ...
# @intent: This function retuns a list of the following functions:
# set: caches parameter matrix and invalidates any previously computed inverse
# get: retrieves cached matrix (matrix was cached by previous call to set())
# setinv: caches parameter as inverse matrix of the matrix that was previously
# cached via a call to set()
# getinv: retrieves the cached inverse matrix that was previously cached by a
# call to setinv()
#
# @function: cacheSolve
# @parameter: cache_mat_list a list object returned by a call to
# makeCacheMatrix()
# @optional parameters: used to pass additional parameters to mean() such as
# rm.na=TRUE
# @return: the cached inverse matrix of cached matrix object ...
# @intent: This function uses the set of functions in the list cache_mat_list to
# do the following:
# 1. retrieve the cached inverse matrix via a call to cache_mat_list$getinv()
# 2. if the inverse matrix has not been previously computed and then cached, then
# the cached matrix object is retrieved via a call to cache_mat_list$get()
# 3. the inverse matrix of cached matrix object is computed via a call to solve()
# 4. the computed inverse matrix of the matrix object returned by cache_mat_list$get() is
# cached via a call to cache_mat_list$setinv()
# 5. the function terminates by returning the inverse matrix - either by retrieving the
# cached copy or by computing (and then caching) the inverse ...
#
###############################################################################

# The function returns an environment consisting of a matrix (passed as a
# parameter), its inverse, and functions to set and/or retrieve these two
# objects
makeCacheMatrix <- function(cache_mat = matrix()) {
  inv_mat <- NULL # null matrix
  
  # this function caches the matrix supplied as a parameter in the object cache_mat
  # that is defined at outer scope.
  # since the cached matrix is either being assigned for the first time or being updated,
  # the previously cached inverse matrix - if any - is invalidated ...
  set <- function(mat) {
    cache_mat <<- mat
    inv_mat <<- NULL
  }
  
  # this function retrieves the previously cached matrix
  get <- function() {
    return (cache_mat)
  }
  
  # this function uses the parameter - which presumably is the inverse matrix of cache_mat - 
  # to assign to and thereby cache the inverse of cache_mat
  setinv <- function(inv) {
    inv_mat <<- inv
  }
  
  # this function retrieves the cached inverse matrix ...
  getinv <- function() {
    return (inv_mat)
  }
  
  # define a list that contains the following functions:
  # set - caches the parameter matrix and invalidates any previously computed inverse
  # get - retrieves cached matrix (matrix was cached by previous call to set())
  # setinv - caches parameter as inverse matrix of the matrix that was previously
  # cached via a call to set()
  # getinv - retrieves the cached inverse matrix that was previously cached by a
  # call to setinv()
  return (list(set=set, get=get, setinv=setinv, getinv=getinv))
}


# This function uses the functionality and environment of makeCacheMatrix to compute
# - only the first time - the inverse of the matrix cached by makeCacheMatrix().
# Once the inverse is calculated and cached, the computation is skipped and instead
# the cached version is retured.
cacheSolve <- function(cache_mat_list, ...) {
   inv_mat <- cache_mat_list$getinv()
  # if inv_mat is null, inverse matrix is not yet computed and cached;
  # otherwise, the inverse matrix has previously been computed and cached
  if (!is.null(inv_mat)) { 
    message("getting cached data")
    return (inv_mat)
  }
  
  # inverse matrix needs to be computed via a call to solve() and then cached ...
  mat <- cache_mat_list$get()
  inv_mat <- solve(mat)
  cache_mat_list$setinv(inv_mat)
  
  return (inv_mat)
}
