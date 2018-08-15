#

best <- function(state,outcome){

    # 1. Reads outcomes data 
    rawData <- read.csv("outcome-of-care-measures.csv", colClass = "character")
    
    # 2. Check that state and outcome are valid
   validity <- function(state,outcome){
       # Create "valid" lists to check against
       outcomes <- c("heart attack","heart failure","pneumonia")
       states <- unique(rawData[,7])
       # Stop program if invalid args
         if(!(state %in% states)){  
            stop("invalid state")
            geterrmessage()
        }
        if(!(outcome %in% outcomes)){ 
            stop("invalid outcome")
            geterrmessage()
        }
   }
   validity(state,outcome)
   
   # 3. Specify column to analyse
    if(outcome==outcomes[1]){
        x <- 11 # Heart Attack: rawData[,11]
    } else if (outcome==outcomes[2]){
        x <- 17 # Heart Failure: rawData[,17]
    } else if (outcome==outcomes[3]){
        x <- 23 # Pneumonia: rawData[,23]
    }
   
   # 4A. Splice data to get only hospitals from that state
   stateData <- subset(rawData,rawData[,7]==state)
   
   # 4B. Remove hospitals without data from analysis
   noData <- (stateData[,x]=="Not Available")
   stateData <- droplevels(stateData[!noData,])
    n <- nrow(stateData)
    
   # 5. Return hospital(s) with lowest mortality
   ##ATTEMPT 4
    list<-c()
    lowestLevel <- which.min(stateData[,x])
    lowest <- as.character(stateData[lowestLevel,x])
    get <- function(i){
        check <- stateData[i,2] 
            if(check==lowest){ #error that min is not meaningful for factors
                if(is.null(match(check,list))){
                    list<- c(list,check)
                }
            }
    }
    ans <- lapply(1:n,get) #iterate through rows and stores lowest in a list
    ans
   
   ##ATTEMP2
    # lowest = apply(stateData,2,min)
   # list = c()
   # makeList <- function(data){
   #     if(lowest%in%){
   #          list<- c(list,data[1])
   #     }
   # }
   # apply(stateData,2,makeList)
   # print(list)
    
   ##ATTEMPT1
    # Function to test if mortality = lowest
    # isLowest <- function(...){
    #         if(hospital[,x]==lowest){
    #             return(hospital[,2])
    #         }else(NULL)
    # }
    # isLowest(stateData[,c(2,x)],)
    # 
    #     lowestList <- apply(stateData[,c(2,x)], 1, isLowest)
    # min(lowestList) -> best
    # return(best)
}