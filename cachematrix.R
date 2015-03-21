
#############################GOAL###############################
# create a place to store matrix (Cache) and its inverse
# Create a function that inverses the stored matrix, unless it is inversed
# Retrieve already inversed matrix from cache (!is.null(of inverse?))
# (s)set matrix, (g)get matrix, (si)set inverse matrix, (gi) get inverse matrix
# store above in object to use in cache solve function
# Return/print inversed matrix

#######################CacheMatrix##############################
##creates a place to store matrix and its inverse
makeCacheMatrix <- function(x = matrix()) { #Square matrix=x  
  inm = NULL #default Cache
  s = function(y) {  #default cache
    x <<- y #Cache Matrix
    inm <<- NULL #Empty cache
  }
  g = function() x
  si = function(inverse) inm <<- inverse 
  gi = function() inm
  list(s=s, g=g, si=si, gi=gi)
}

########################CacheSolve##############################
##Caches matrix if not inversed or recalls already inversed matric from cache
cacheSolve <- function(x, ...) { 
  inm = x$gi()
  if (!is.null(inm)){
    message("retrieve cached matrix")
    return(inm)
  }
  mcache = x$g() #calculate inverse if no cached matrix
  inm = solve(mcache, ...)
  x$si(inm) #cache inverse
  return(inm) #print inverse matirx
  #invisible(inm) #does not print...how to make this work?? 
}

#############################TEST##############################
mat <- matrix(data = c(2,4,6,8), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)

r = rnorm(100)
mat1 = matrix(r, nrow=10, ncol=10)
mat3 <- makeCacheMatrix(mat1)
cacheSolve(mat3)
