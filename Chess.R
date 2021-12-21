#Function that is created to check is there any attacking prices horizontally to the selected peice
#The pos variable in the parameter resembles the 2d array which is the chess board that contains respective queens positioned.
#x is the horizontal coordinate of the chess board grid while y is the vertical coordinate 
check_horizontal <- function(pos,x,y){
  bool <- FALSE #The bool variable used to determine is there any attacking peices for a particular peice is initialize as false
  for(i in 1:8){ #The iteration of 8 is to obtain the peices column by column vertically 
    if(i!=y){ #If the ith value during iteration is not as the same value as the peices that is being used to check
              # This to obtain the peices to check itself to falsely assume it is attacking itself due to the presence of matrix 1 on itself.
      if(pos[x,i]==1){#If the is a another block of grid value 1 which means there is another queen peice same row as the peice being used to check.
#        cat(x,y , "Attack",x,i," " )
        display_diagonal(x,y,x,i)#Display both the queens that are attacking each other 
        bool <- TRUE # The bool is changed to true if at least there are peices attacking each other 
      }
    }
  }
  return (bool) #The final bool value is returned from the function 
}

#This function is created to check the diagonal moves of the queen 
check_diagonal <-function (pos,x,y){
  bool<- FALSE #The bool variable used to determine is there any attacking peices for a particular peice is initialize as false
    if(y==1){
      bool <- diagonal_column1(pos,x)#Call the column1 diagonal checking function if the queen is in column 1
    }
    if(y==2){
      bool <- diagonal_column2(pos,x)#Call the column2 diagonal checking function if the queen is in column 2
    }
    if(y==3){
      bool <- diagonal_column3(pos,x)#Call the column3 diagonal checking function if the queen is in column 3
    }
    if(y==4){
      bool <- diagonal_column4(pos,x)#Call the column4 diagonal checking function if the queen is in column 4
    }
    if(y==5){
      bool <- diagonal_column5(pos,x)#Call the column5 diagonal checking function if the queen is in column 5
    }
    if(y==6){
      bool <- diagonal_column6(pos,x)#Call the column6 diagonal checking function if the queen is in column 6
    }
    if(y==7){
      bool <- diagonal_column7(pos,x)#Call the column7 diagonal checking function if the queen is in column 7
    }
    if(y==8){
      bool <- diagonal_column8(pos,x)#Call the column8 diagonal checking function if the queen is in column 8
    }
  return(bool) # return the status true if there is at least one queen attacking each other and false otherwise
}

#diagonal_column functions to iterate the possible movement of the queen based on the respective column function call and return true
#if it attacks any queen during its diagonal traversal
diagonal_column1 <- function (pos,x){
  bool <- FALSE #Initialize the status to false 
  if(x==8){ #If the queen is in 8th box of column 1
    a <- 7 #The first coordinate/location to visit is initialize
    b <- 2 #The first coordinate/location to visit is initialize
    for(i in 1:7){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<- TRUE # The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective grid/coordinate/location
      }
      a <- a-1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
  }
  
  if(x==7){#If the queen is in 7th box of column 1
    a <- 6 #The first coordinate/location to visit is initialize
    b <- 2 #The first coordinate/location to visit is initialize
    for(i in 1:6){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE  #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective grid/coordinate/location
      }
      a <- a-1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
    if (pos[8,2]==1){
      bool<- TRUE
      display_diagonal(x,1,a,b)
    }
  }
  
  if(x==6){#If the queen is in 6th box of column 1
    a <- 5 #The first coordinate/location to visit is initialize
    b <- 2 #The first coordinate/location to visit is initialize
    for(i in 1:5){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a-1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
    a <- 7 #The fist coordinate/location to visit in other diagonal direction is initialized
    b <- 2 #The fist coordinate/location to visit in other diagonal direction is initialized
    for(i in 1:2){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a+1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
  }
  
  if(x==5){ #If the queen is in 5th box of column 1
    a <- 4 #The first coordinate/location to visit is initialize
    b <- 2 #The first coordinate/location to visit is initialize
    for (i in 1:4){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a-1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
    a <- 6 #The fist coordinate/location to visit in other diagonal direction is initialized
    b <- 2 #The fist coordinate/location to visit in other diagonal direction is initialized
    for (i in 1:3){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a+1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
  }
  
  if(x==4){ #If the queen is in 4th box of column 1
    a <- 3 #The first coordinate/location to visit is initialize
    b <- 2 #The first coordinate/location to visit is initialize
    for(i in 1:3){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a-1 #to move to the next grid/coordinate
      b <- b+1#to move to the next grid/coordinate
    }
    a <- 5 #The fist coordinate/location to visit in other diagonal direction is initialized
    b <- 2 #The fist coordinate/location to visit in other diagonal direction is initialized
    for(i in 1:4){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a+1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
  }
  
  if(x==3){ #If the queen is in 3rd box of column 1
    a <- 2 #The first coordinate/location to visit is initialize
    b <- 2 #The first coordinate/location to visit is initialize
    for(i in 1:2){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
          bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
          display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
        }
      a <- a-1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
    a <- 4 #The fist coordinate/location to visit in other diagonal direction is initialized
    b <- 2 #The fist coordinate/location to visit in other diagonal direction is initialized
    for(i in 1:5){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a+1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
  }
  
  if(x==2){ #If the queen is in 2nd box of column 1
    a <- 1 #The first coordinate/location to visit is initialize
    b <- 2 #The first coordinate/location to visit is initialize
    if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
      bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
      display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
    }
    a <- 3 #The fist coordinate/location to visit in other diagonal direction is initialized
    b <- 2 #The fist coordinate/location to visit in other diagonal direction is initialized
    for(i in 1:6){ #Iterate the possible diagonal coordinate/location that the queen can attack
      if(pos[a,b]==1){ #If there is any possible queen peice in diagonal attacking the peice that is in check
        bool<-TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a+1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
  }
  
  if(x==1){
    a <- 2 #The first coordinate/location to visit is initialize
    b <- 2  #The first coordinate/location to visit is initialize
    for(i in 1:7){ #If there is any possible queen peice in diagonal attacking the peice that is in check
      if(pos[a,b]==1){
        bool<- TRUE #The status is changed to true to indicate there is a queen piece attacking the peice that is in check
        display_diagonal(x,1,a,b) #calls the display function to display both the queens in attack with their respective coordinate
      }
      a <- a+1 #to move to the next grid/coordinate
      b <- b+1 #to move to the next grid/coordinate
    }
  }
  
  return (bool) # return the status true if there is at least one queen attacking each other in diagonal to any coordinate  in column 1 and false otherwise
}
#The steps is as same as column 1
diagonal_column2 <- function(pos,x){
  bool <- FALSE 
  if(x==8){
    a<-7 
    b<-3
    for(i in 1:6){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,2,a,b)
      }
      a <- a-1
      b <- b+1
    }
    if (pos[7,1]==1){
      bool<- TRUE
      display_diagonal(x,2,7,1)
    }
  }
  
  if(x==7){
    a<-8
    b<-1
    for(i in 1:8){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,2,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    if (pos[8,3]==1){
      bool<- TRUE
      display_diagonal(x,2,8,3)
    }
    else if (pos[6,1]==1){
      bool<- TRUE
      display_diagonal(x,2,6,1)
    }
  }
  
  if(x==6){
    a<-7
    b<-1
    for(i in 1:7){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,2,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    if (pos[5,1]==1){
      bool<- TRUE
      display_diagonal(x,2,5,1)
    }
    else if (pos[7,3]==1){
      bool<- TRUE
      display_diagonal(x,2,7,3)
    }
    else if (pos[8,4]==1){
      bool<- TRUE
      display_diagonal(x,2,8,4)
    }
  }
  
  if(x==5){
    a<-6
    b<-1
    for(i in 1:6){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,2,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    if (pos[4,1]==1){
      bool<- TRUE
      display_diagonal(x,2,4,1)
    }
    else if (pos[6,3]==1){
      bool<- TRUE
      display_diagonal(x,2,6,3)
    }
    else if (pos[7,4]==1){
      bool<- TRUE
      display_diagonal(x,2,7,4)
    }
    else if (pos[8,5]==1){
      bool<- TRUE
      display_diagonal(x,2,8,5)
    }
  }
  
  if(x==4){
    a<-5
    b<-1
    c<-5
    d<-3
    for(i in 1:5){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,2,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    if (pos[3,1]==1){
      bool<- TRUE
      display_diagonal(x,2,3,1)
    }
    for(i in 1:4){
      if(pos[c,d]==1){
        bool <- TRUE
        display_diagonal(x,2,c,d)
      }
      c<-c+1
      d<-d+1
    }
  }
  
  if(x==3){
    a<-4
    b<-1
    c<-4
    d<-3
    for(i in 1:4){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,2,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    if (pos[2,1]==1){
      bool<- TRUE
      display_diagonal(x,2,2,1)
    }
    for(i in 1:5){
      if(pos[c,d]==1){
        bool <- TRUE
        display_diagonal(x,2,c,d)
      }
      c<-c+1
      d<-d+1
    }
  }
  
  if(x==2){
    a<-3
    b<-1
    c<-3
    d<-3
    for(i in 1:3){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,2,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    if (pos[1,1]==1){
      bool<- TRUE
      display_diagonal(x,2,1,1)
    }
    for(i in 1:6){
      if(pos[c,d]==1){
        bool <- TRUE
        display_diagonal(x,2,c,d)
      }
      c<-c+1
      d<-d+1
    }
  }
  
  if(x==1){
    c<-3
    d<-4
    if (pos[2,1]==1){
      bool<- TRUE
      display_diagonal(x,2,2,1)
    }
    else if (pos[2,3]==1){
      bool<- TRUE
      display_diagonal(x,2,2,3)
    }
    for(i in 1:5){
      if(pos[c,d]==1){
        bool <- TRUE
        display_diagonal(x,2,c,d)
      }
      c<-c+1
      d<-d+1
    }
  }
  
  return (bool)
}
#The steps is as same as column 1
diagonal_column3 <- function(pos,x){
  bool <- FALSE 
  if(x==8){
    a<-7 
    b<-4
    for(i in 1:5){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,3,a,b)
      }
      a <- a-1
      b <- b+1
    }
    if (pos[7,2]==1){
      bool<- TRUE
      display_diagonal(x,3,7,2)
    }
    else if (pos[6,1]==1){
      bool<- TRUE
      display_diagonal(x,3,6,1)
    }
  }
  
  if(x==7){
    a<-8
    b<-2
    for(i in 1:7){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,3,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    if (pos[8,4]==1){
      bool<- TRUE
      display_diagonal(x,3,8,4)
    }
    else if (pos[6,2]==1){
      bool<- TRUE
      display_diagonal(x,3,6,2)
    }
    else if (pos[5,1]==1){
      bool<- TRUE
      display_diagonal(x,3,5,1)
    }
  }
  
  if(x==6){
    a<-8
    b<-1
    c<-8
    d<-5
    for(i in 1:8){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,3,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    for(i in 1:5){
      if(i!=3){
        if(pos[c,d]==1){
          bool <- TRUE
          display_diagonal(x,3,c,d)
        }
      }
      c<-c-1
      d<-d-1
    }
  }
  
  if(x==5){
    a<-7
    b<-1
    c<-8
    d<-6
    for(i in 1:7){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,3,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    for(i in 1:6){
      if(i!=4){
        if(pos[c,d]==1){
          bool <- TRUE
          display_diagonal(x,3,c,d)
        }
      }
      c<-c-1
      d<-d-1
    }
  }
  
  if(x==4){
    a<-6
    b<-1
    c<-8
    d<-7
    for(i in 1:6){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,3,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    for(i in 1:7){
      if(i!=5){
        if(pos[c,d]==1){
          bool <- TRUE
          display_diagonal(x,3,c,d)
        }
      }
      c<-c-1
      d<-d-1
    }
  }
  
  if(x==3){
    a<-5
    b<-1
    c<-8
    d<-8
    for(i in 1:5){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,3,a,b)
        }
      }
      a<- a-1
      b<- b+1
    }
    
    for(i in 1:8){
      if(i!=6){
        if(pos[c,d]==1){
          bool <- TRUE
          display_diagonal(x,3,c,d)
        }
      }
      c<-c-1
      d<-d-1
    }
  }
  
  if(x==2){
    a<-4
    b<-1
    c<-7
    d<-8
    for(i in 1:4){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,3,a,b)
        }
        a<- a-1
        b<- b+1
      }
    }
    for(i in 1:7){
      if(i!=6){
        if(pos[c,d]==1){
          bool <- TRUE
          display_diagonal(x,3,c,d)
        }
      }
      c<-c-1
      d<-d-1
    }
  }
  
  if(x==1){
    c<-2
    d<-4
    if (pos[3,1]==1){
      bool<- TRUE
      display_diagonal(x,3,3,1)
    }
    else if (pos[2,2]==1){
      bool<- TRUE
      display_diagonal(x,3,2,2)
    }
    for(i in 1:5){
      if(pos[c,d]==1){
        bool <- TRUE
        display_diagonal(x,3,c,d)
      }
      c<-c+1
      d<-d+1
    }
  }
  
  return(bool)
}
#The steps is as same as column 1
diagonal_column4 <- function(pos,x){
  bool <- FALSE 
  if(x==8){
    a<-7 
    b<-5
    for(i in 1:4){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,4,a,b)
      }
      a <- a-1
      b <- b+1
    }
    a<-7
    b<-3
    for(i in 1:3){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,4,a,b)
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==7){
    a<-8 
    b<-3
    for(i in 1:6){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-5
    for(i in 1:5){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==6){
    a<-8 
    b<-2
    for(i in 1:7){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-6
    for(i in 1:6){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==5){
    a<-8 
    b<-1
    for(i in 1:8){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-7
    for(i in 1:7){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==4){
    a<-7 
    b<-1
    for(i in 1:7){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-8
    for(i in 1:8){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==3){
    a<-6 
    b<-1
    for(i in 1:6){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-7
    b<-8
    for(i in 1:7){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==2){
    a<-5 
    b<-1
    for(i in 1:5){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-6
    b<-8
    for(i in 1:6){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,4,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==1){
    a<-4 
    b<-1
    for(i in 1:3){
      if(pos[a,b]==1){
          bool <- TRUE
          print("lol")
          display_diagonal(x,4,a,b)
        }
      a <- a-1
      b <- b+1
    }
    a<-2
    b<-5
    for(i in 1:4){
        if(pos[a,b]==1){
          bool <- TRUE
          print("lol")
          display_diagonal(x,4,a,b)
        }
      a <- a+1
      b <- b+1
    }
  }
  
  return (bool)
}
#The steps is as same as column 1
diagonal_column5 <- function(pos,x){
  bool <- FALSE 
  if(x==8){
    a<-7 
    b<-6
    for(i in 1:3){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,5,a,b)
      }
      a <- a-1
      b <- b+1
    }
    a<-7
    b<-4
    for(i in 1:4){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,5,a,b)
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==7){
    a<-8 
    b<-4
    for(i in 1:5){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-6
    for(i in 1:6){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==6){
    a<-8 
    b<-3
    for(i in 1:6){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-7
    for(i in 1:7){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==5){
    a<-8 
    b<-2
    for(i in 1:7){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-8
    for(i in 1:8){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==4){
    a<-8 
    b<-1
    for(i in 1:8){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-7
    b<-8
    for(i in 1:7){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==3){
    a<-7 
    b<-1
    for(i in 1:7){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-6
    b<-8
    for(i in 1:6){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==2){
    a<-6 
    b<-1
    for(i in 1:6){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-5
    b<-8
    for(i in 1:5){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,5,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==1){
    a<-5 
    b<-1
    for(i in 1:4){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,5,a,b)
      }
      a <- a-1
      b <- b+1
    }
    a<-4
    b<-8
    for(i in 1:3){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,5,a,b)
      }
      a <- a-1
      b <- b-1
    }
  }
  
  return (bool)
}
#The steps is as same as column 1
diagonal_column6 <- function(pos,x){
  bool <- FALSE 
  if(x==8){
    a<-7 
    b<-7
    for(i in 1:2){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,6,a,b)
      }
      a <- a-1
      b <- b+1
    }
    a<-7
    b<-5
    for(i in 1:5){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,6,a,b)
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==7){
    a<-8 
    b<-5
    for(i in 1:4){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-7
    for(i in 1:7){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==6){
    a<-8 
    b<-4
    for(i in 1:5){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-8
    for(i in 1:8){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==5){
    a<-8 
    b<-3
    for(i in 1:6){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-7
    b<-8
    for(i in 1:7){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==4){
    a<-8 
    b<-2
    for(i in 1:7){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-6
    b<-8
    for(i in 1:6){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==3){
    a<-8 
    b<-1
    for(i in 1:8){
      if(i!=6){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-5
    b<-8
    for(i in 1:5){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==2){
    a<-7 
    b<-1
    for(i in 1:7){
      if(i!=6){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-4
    b<-8
    for(i in 1:4){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,6,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==1){
    a<-6 
    b<-1
    for(i in 1:5){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,6,a,b)
      }
      a <- a-1
      b <- b+1
    }
    a<-3
    b<-8
    for(i in 1:2){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,6,a,b)
      }
      a <- a-1
      b <- b-1
    }
  }
  
  return (bool)
}
#The steps is as same as column 1
diagonal_column7 <- function(pos,x){
  bool <- FALSE 
  if(x==8){
    a<-7
    b<-6
    if(pos[7,8]==1){
      bool <- TRUE
      display_diagonal(x,7,7,8)
    }
    
    for(i in 1:6){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,7,a,b)
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==7){
    a<-8 
    b<-6
    for(i in 1:3){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-8
    b<-8
    for(i in 1:8){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==6){
    a<-8 
    b<-5
    for(i in 1:4){
      if(i!=3){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-7
    b<-8
    for(i in 1:7){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==5){
    a<-8 
    b<-4
    for(i in 1:5){
      if(i!=4){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-6
    b<-8
    for(i in 1:6){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==4){
    a<-8 
    b<-3
    for(i in 1:6){
      if(i!=5){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-5
    b<-8
    for(i in 1:5){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==3){
    a<-8 
    b<-2
    for(i in 1:7){
      if(i!=6){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-4
    b<-8
    for(i in 1:4){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==2){
    a<-8 
    b<-1
    for(i in 1:8){
      if(i!=7){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b+1
    }
    a<-3
    b<-8
    for(i in 1:3){
      if(i!=2){
        if(pos[a,b]==1){
          bool <- TRUE
          display_diagonal(x,7,a,b)
        }
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==1){
    a<-7 
    b<-1
    for(i in 1:6){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,7,a,b)
      }
      a <- a-1
      b <- b+1
    }
    if(pos[2,8]==1){
      bool <- TRUE
      display_diagonal(x,7,2,8)
    }
  }
  
  return (bool)
}
#The steps is as same as column 1
diagonal_column8 <- function(pos,x){
  bool <- FALSE 
  if(x==8){
    a<-7
    b<-7
    for(i in 1:7){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a-1
      b <- b-1
    }
  }
  
  if(x==7){
    a<-6 
    b<-7
    C<-8
    d<-8
    for(i in 1:6){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a-1
      b <- b-1
    }
    if(pos[8,7]==1){
      bool <- TRUE
      display_diagonal(x,8,8,7)
    }
    
  }
  
  if(x==6){
    a<-5 
    b<-7
    for(i in 1:5){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a-1
      b <- b-1
    }
    a<-7
    b<-7
    for(i in 1:2){
      if(pos[a,b]){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a+1
      b <- b-1
    }
  }
  
  if(x==5){
    a<-4 
    b<-7
    for(i in 1:4){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a-1
      b <- b-1
    }
    a<-6
    b<-7
    for(i in 1:3){
      if(pos[a,b]){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a+1
      b <- b-1
    }
  }
  
  if(x==4){
    a<-3 
    b<-7
    for(i in 1:3){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a-1
      b <- b-1
    }
    a<-5
    b<-7
    for(i in 1:4){
      if(pos[a,b]){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a+1
      b <- b-1
    }
  }
  
  if(x==3){
    a<-2 
    b<-7
    for(i in 1:2){
      if(pos[a,b]==1){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a-1
      b <- b-1
    }
    a<-4
    b<-7
    for(i in 1:5){
      if(pos[a,b]){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a+1
      b <- b-1
    }
  }
  
  if(x==2){
    a<-3
    b<-7
    if(pos[1,7]==1){
      bool <- TRUE
    }
    for(i in 1:6){
      if(pos[a,b]){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a+1
      b <- b-1
    }
  }
  
  if(x==1){
    a<-2
    b<-7
    for(i in 1:7){
      if(pos[a,b]){
        bool <- TRUE
        display_diagonal(x,8,a,b)
      }
      a <- a+1
      b <- b-1
    }
  }
  
  return (bool)
  
}

#Function to display the pair of queens that are attacking each other 
#An alphabet is assigned to each column number to display as the output
display_diagonal <- function(x,y,a,b){
  #the alphabets assigned to corresponding value of the y parameter which is the column number 
  if(y==1){
    j <- 'a'
  }
  if(y==2){
    j <- 'b'
  }
  if(y==3){
    j <- 'c'
  }
  if(y==4){
    j <- 'd'
  }
  if(y==5){
    j <- 'e'
  }
  if(y==6){
    j <- 'f'
  }
  if(y==7){
    j <- 'g'
  }
  if(y==8){
    j <- 'h'
  }
  #the alphabets assigned to corresponding value of the b parameter which is the column number 
  if(b==1){
    k<- 'a'
  }
  if(b==2){
    k<- 'b'
  }
  if(b==3){
    k<- 'c'
  }
  if(b==4){
    k<- 'd'
  }
  if(b==5){
    k<- 'e'
  }
  if(b==6){
    k<- 'f'
  }
  if(b==7){
    k<- 'g'
  }
  if(b==8){
    k<- 'h'
  }
  
  cat(x,j , "Attacking",a,k,"\n" ) #The pair of queen attacking is displayed based on the respective alphabet and number that indicates the queen
}


chessboard_mat <- matrix(0, nrow=8 , ncol =8)#Initialize matrix grid to create the chess board that will contain 8 queens position
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
#PLACE THE VECTOR INPUT OF CHESS OVERR HERE 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
solution <- c(3,1,7,5,8,2,4,6) #Input of the 8 queens position
temp<- 0 #A temporary variable initialize to 0 
for( i in 1:8){ #Iteration of index to obtain 8 elements from the input vector
  temp <- solution[i]
  chessboard_mat[temp,i]<-1 # the queens position is marked on the matrix board based on the user input which is in the vector
}

count<-0  #a variable that is used to identify if there is any true value for diagonal and horizontal checking 
print("The chess board with the position of the queens as 1")
chessboard_mat #print the chess board containing the position of the queens
print("Queens that are attacking each other: ")
for (i in 1:8){ #Iteration to check each column 
  
  if(check_horizontal(chessboard_mat,solution[i],i)){
    count<-count+1 #count variable incremented if the is at least a pair queens attacking each other horizontally 
  }
  if(check_diagonal(chessboard_mat,solution[i],i)){
    count<-count+1 #count variable incremented if the is at least a pair queens attacking each other diagonally 
  }
}

#feasible or infeasible is displayed based on the value of the count after the horizontal and diagonal function call return
if(count!=0){
  print("Infeasible Solution")
}
if(count==0){
  print("NONE")
  print("feasible solution")
}
