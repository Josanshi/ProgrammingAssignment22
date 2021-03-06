#################################################### MOOC Coursera Week 3 #####################################################

## Description (Fran�ais) :
## Ces deux fonctions permettent, pour la premi�re, de cr�er un espace dans la m�moire cache afin de stocker l'inverse d'une 
## matrice inversible dont on sait que le r�sultat nous servira � plusieurs reprises (makeCacheMatrix). La deuxi�me, pour �viter de recalculer
## � chaque fois cette matrice inverse, sert � cr�er une alternative � la fonction solve qui va v�rifier la pr�sence d'une matrice inverse
## d�j� en m�moire cache pour la matrice d�termin� avant d'effectuer l'inversion (cacheSolve). Si c'est le cas alors elle renvoie le r�sultat 
## d�j� en m�moire si ce n'est pas le cas elle va alors stocker ce nouveau r�sultat dans l'espace cr�� par la fonction makeCacheMatrix.
## Ce r�sultat pourra ensuite �tre appel� si on redemande la m�me inversion de matrice pour gagner en temps de calcul.

## Description (English : sorry for my poor english writing, i hope it'll be readable)
## These two functions allow , for the first , to create a space in the cache memory to store the inverse of a
## invertible matrix of which result will be usefull several times (makeCacheMatrix) . The second, to avoid recalculating each time 
## this inverse matrix is used to create an alternative to solve function that will check the presence of an inverse matrix already 
## in cache memory for the determined matrix. If this is the case then it returns the result already in memory if it is not the case 
## it will then store this new result in the space created by the makeCacheMatrix function. This result can then be called if 
## one asks the same matrix inversion in order to gain in computation time.

## makeCacheMatrix Function : Fran�ais
## On exige comme argument une matrice (inversible) puis on initialise un espace permettant de stocker la moyenne (m). La partie set 
## permet de mettre en cache la matrice initiale. La partie get permettra en cas de donn�es non stock�es d'appeler la matrice � inverser.
## La partie setinverse quant � elle, permet de lier la fonction solve � une zone en m�moire cache pour y stocker le r�sultat. La
## partie getinverse permet d'appeler le r�sultat gard� en m�moire qui est soit NULL si la m�moire cache est vide ou qui contient l'inverse
## de la matrice enregistr�e

## makeCacheMatrix Function : English
## A matrix (invertible) is required as an argument then it initializes a space for storing the mean (m). The party set
## can cache the initial data. The party get will call the stored data matrix in case of no inversed matrix data in cache to calculate the inverse.
## The setinverse portion, links the function solve to a memory area in cache to store the result. The
## Getinverse party can call the result kept in memory that is either null if the cache is empty (so the function solve will be called for 
## calculate the inverse matrix) or contains the already stored inverse matrix (so no need to calculate).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve Function : Fran�ais
## La fonction commence par appeler le r�sultat en cache gr�ce � la fonction getinverse. Elle teste ensuite si ce r�sultat est vide = NULL
## ou si il contient un autre �l�ment (la matrice invers�e). Si c'est une matrice invers�e, la fonction affiche le message "getting cached data"
## pour indiquer qu'il n'y a pas eu de nouveau calcul, juste la lecture du cache et retourne la matrice invers� contenu dans le cache.
## Si le cache est vide alors la fonction get recup�re la matrice et la stock dans l'�l�ment data. la fonction solve est appel� pour inverser
## la matrice contenu dans data qui est stock� dans m. Enfin la matrice invers�e est� la fois stock�e dans le cache par la function setinverse
## et elle est donn�e � l'utilisateur dans la console.

##cacheSolve Function : English
## The function starts by calling the cached result through getinverse function. It then tests whether the result is empty = NULL
## Or if it contains an element (the inverse matrix) . If it is an inverse matrix , the function displays "getting cached data"
## To indicate that there was no recalculation just reading the cache , and returns the inverted matrix content in the cache.
## If the cache is empty then the function retrieves the matrix and get the stock in the data element. the function solve is called to reverse
## Contained in the matrix data that is stored in m. Finally, the inverse matrix est� both stored in the cache by function setinverse
## And is given to the user in the console.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## First example for testing functions without using set part

rm(list=ls())

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

a<-matrix(1:4,2,2) #We create a simple inversible matrix example

alpha<-makeCacheMatrix(a) # we create the cache and the function needed for compute the inverse matrix of a and store its in alpha with the matrix a itself

cacheSolve(alpha) # We ask for the inverse matrix of a throught alpha. For the first computing, there is no data in the cache, so cacheSolve compute inverse, print it in the console and store it in the cache memory

cacheSolve(alpha) # we ask again the inverse matrix of a throught alpha. This time, the cache memory contains the inverse matrix, so cacheSolve do not compute the inverse and just return the matrix contained in the cache memory

## Second example using set part

rm(list=ls())

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

alpha<-makeCacheMatrix() # we create the cache and the function needed for compute the inverse of an inversible matrix 

alpha$set(matrix(1:4,2,2)) #We create a simple inversible matrix example and store it in alpha

cacheSolve(alpha) # We ask for the inverse matrix of a throught alpha. For the first computing, there is no data in the cache, so cacheSolve compute inverse, print it in the console and store it in the cache memory

cacheSolve(alpha) # we ask again the inverse matrix of a throught alpha. This time, the cache memory contains the inverse matrix, so cacheSolve do not compute the inverse and just return the matrix contained in the cache memory