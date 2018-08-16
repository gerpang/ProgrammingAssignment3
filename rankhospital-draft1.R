rankhospital<- function(state,outcome,rank = "best"){
    raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
        slice[, eval(outcome)] <- as.numeric(slice[, eval(outcome)])
        slice <- slice[order(slice[, eval(outcome)],slice[,"hospital"]),]
        ans <- slice[,"hospital"][rank]

    } else if (!is.numeric(rank)) { # best/worse case
        if (rank == "best"){
            ans <- best(state, outcome)
            
        }else if (rank =="worst"){
            s <- which(data[,"state"] == state)
            slice <- data[s,]
            slice[, eval(outcome)] <- as.numeric(slice[, eval(outcome)])
            slice <- slice[order(slice[, eval(outcome)],slice[,"hospital"],decreasing=TRUE),]
            # print(slice)
            ans <- slice[,"hospital"][1]
        } else { 
        stop('invalid rank')
        }
    }
return(ans)
}

samplerank("NC", "heart attack", "worst")
