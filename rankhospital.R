rankhospital<- function(state,outcome,num = "best"){
    raw <- read.csv("outcome-of-care-measures.csv", colClass = "character")
    data <- as.data.frame(cbind 
                          # Binds selected columns into a matrix, coerce into df 
                          (raw[,2], #hospital
                              raw[,7], #state
                              raw[,11], # heart attack
                              raw[,17], # heart failure
                              raw[,23]), #pneumonia
                          stringAsFactors=FALSE) #so characters are not forced into factors
    colnames(data) <- c("hospital","state", "heart attack", "heart failure", "pneumonia")
    
    # 2. Check validity of arguments 
    outcomes <- c("heart attack","heart failure","pneumonia")
    
    if(!state %in% data[,"state"]){
        stop('invalid state')
    } else if(!outcome %in% outcomes){
        stop('invalid outcome')
    
    } else if (is.numeric(rank)) {
        s <- which(data[,"state"] == state)
        slice <- data[s,]
        sliceN <- as.numeric(slice[, eval(outcome)])
        sliceN <- sliceN[order()] ##STOPPED HERE 
        ls <- sliceN[,"hospital"][rank]
        ans <- ls[,"hospital"][1]
        
    } else if (!is.numeric(rank)) { # best/worse case
        if (rank == "best"){
            ans <- best(state, outcome)
        }else if (rank =="worst"){
            s <- which(data[,"state"] == state)
            slice <- data[s,]
            sliceN <- as.numeric(slice[, eval(outcome)])
            ls <- sliceN[order(sliceN[,eval(outcome)],ts[,"hospital"],decreasing=TRUE),]
            ans <- ls[,"hospital"][1]
        } else { 
        stop('invalid rank')
        }
    }
    return(ans)
}

## NTS
# No need for na.rm because ... 