# -----------------------------------------------------------------------------
## cacheMatrix.R - Making matrix inversion faster via caching.
## Author: Maytrix
## Project: Coursera; R-Programming; December 2014
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
## makeCacheMatrix - A cached matrix object.
## 	set - store orginal matrix data
##	get - original matrix data
##	setcache - cache (store) inverted matrix
##	getcache - retrieve inverted matrix
# -----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
	cachem <- NULL
	set <- function (y) {
		x <<- y
		# The matrix has changed. Cache no longer valid.
		cachem <<- NULL
	}
	get <- function() x
	setcache <- function(c) cachem <<- c
	getcache <- function() cachem
	list(set = set, get = get, 
	     setcache = setcache, getcache = getcache)

} # makeCacheMatrix


# -----------------------------------------------------------------------------
## cacheSolve -- Cached Matrix Inversion. Makes solve() efficient with caching.
## Returns cached copy of inverted matrix is available.
##         Otherwise returns a freshly computed inverse.
# -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {

        ## Access cache. 
	cachem <- x$getcache ()
	
	if (!is.null(cachem)){
		# Found a cached copy.
		message ("Cached inverse:")
		return (cachem)
	}

	# Cache is empty. Compute the inverse.
	data <- x$get()
	cachem <- solve (data)
	x$setcache(cachem)
	cachem
} # cacheSolve


