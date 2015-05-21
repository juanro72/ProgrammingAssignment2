## THE FUNCTION "makeCacheMatrix" CREATES A LIST "l" WHICH 
## CONTAINS FOUR FUNCTIONS REQUIRED TO SET AND GET A MATRIX
## (PREVIOUSLY DEFINED), STORING TO A CACHE LOCATION, IN 
## PREPARATION FOR OBTAINING ITS CORRESPONDANT INVERSE MATRIX.

## HAVING EXECUTED "makeCacheMatrix", ONE CAN CALL NEXT THE
## FUNCTION "cacheSolve(l)" TO CALCULATE DE INVERSE, OR IF IT
## HAS BEEN ALREADY CALCULATED, RETRIEVE THE RESULTANT FROM
## THE CACHE.

## NOTE:THE INPUT MATRIX MUST BE SQUARE AND INVERTIBLE
##---------------------------------------------------------

      ## "makeCacheMatrix": SEE EXPLANATION BY LINE, BELOW CODE
      
      makeCacheMatrix <- function(x = matrix()) {     ##LINE 1
            m<-NULL                                   ##LINE 2
            set<-function(y)  {                       ##LINE 3
                  x<<-y                               ##LINE 4
                  m<<-NULL                            ##LINE 5
            }
            get<-function()   x                       ##LINE 6
            setsolve<-function(solve)   m<<-solve     ##LINE 7
            getsolve<-function()    m                 ##LINE 8
            l<<-list(set=set, get=get,   
                     setsolve=setsolve, 
                     getsolve=getsolve)               ##LINE 9          
      }

      ##LINE 1: THE FUNCTION LOADS AS ITS ONLY ARGUMENT, A MATRIX.
      ##LINE 2: INITIALIZES EMPTY INVERSE MATRIX.
      ##LS 3-5: DEFINES "set" FUNCTION TO STORE "x"IN A MATRIX 
      ##          AT THE GLOBAL ENVIRONMENT CALLED "y" TO SERVE
      ##          AS CACHE MEMORY FOR THE INPUT MATRIX.
      ##LINE 6: DEFINES "get" FUNCTION TO BE CALLED LATER FOR 
      ##          INVERSE CALCULATION. SEE "cacheMatrix" LINE 6.
      ##LINE 7: CREATES FUNCTION THAT TAKES THE SOLVED MATRIX
      ##          AND ASSIGN IT TO "m". SEE "cacheMatrix" LINE 8.
      ##LINE 8: CREATES FUNCTION THAT RETRIEVES THE vALUE OF "m"
      ##          FOR USE IN  "cacheMatrix" LINE 2.
      ##LINE 9: CREATES THE LIST THAT CONTAINS THE 4 FUNCTIONS.

##---------------------------------------------------------

      ## "cacheSolve": SEE EXPLANATION BY LINE, BELOW CODE
      
      cacheSolve <- function(l, ...) {                ##LINE 1
            m<-l$getsolve()                           ##LINE 2
            if(!is.null(m))   {                       ##LINE 3
                  message("getting cache data")       ##LINE 4
                  return(m)                           ##LINE 5
            }
            data<-l$get()                             ##LINE 6
            m<-solve(data,...)                        ##LINE 7
            l$setsolve(m)                             ##LINE 8
            m                                         ##LINE 9
      }

      ##LINE 1: THE FUNCTION LOADS AS MAIN ARGUMENT, THE LIST OF
      ##          FUNCTIONS "l" FROM "makeCacheMatrix".
      ##LINE 2: RETRIEVES ON "m" ITS PREVIOUS VALUE IF IT EXISTS.
      ##LS 3-5: RETURNS A MESSAGE WHEN INVERTED MATRIX IS FOUND
      ##          ALREADY CACHED. THIS HAPPENS WHEN "m" IS NOT
      ##          EMPTY. THIS ENDS FUNCTION WITH A PRINTED "m".
      ##LS 6-8: INSERTS THE NEW VALUE FOR "m": CALCULATES THE
      ##          INVERSE MATRIX: TAKES "x"(FROM l$get), THEN
      ##          APPLIES "solve" FUNCTION AND  CALLS FUNCTION
      ##          IN "l" THAT TAKES SOLVE AND ASSIGN IT TO "m".
      ##LINE 9: PRINTS "m".


##---------------------------------------------------------