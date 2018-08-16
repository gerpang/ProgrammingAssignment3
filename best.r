# best <- function(state,outcome){
#     # 1. Read and structure data 
#     raw <- read.csv("outcome-of-care-measures.csv", colClass = "character")
#     data <- as.data.frame(cbind 
#     # Binds selected columns into a matrix, coerce into df 
#                                 (raw[,2], #hospital
#                                 raw[,7], #state
#                                 raw[,11], # heart attack
#                                 raw[,17], # heart failure
#                                 raw[,23]), #pneumonia
#                         stringAsFactors=FALSE) #so characters are not forced into factors
#     colnames(data) <- c("hospital","state", "heart attack", "heart failure", "pneumonia")
#     
#     # return(data) # check
#     
#     # 2. Check validity of arguments 
#     outcomes <- c("heart attack","heart failure","pneumonia")
#     if(!state %in% data[,"state"]){
#         stop('invalid state')
#     } else if(!outcome %in% outcomes){
#         stop('invalid outcome')
#     } else {
#        # 3. Slice data for specified state and outcome
#         s <- which(data[,"state"] == state)
#             # print(head(s))
#         slice <- data[s,]
#             # print(head(slice))
#         sliceN <- as.numeric(slice[, eval(outcome)])
#             # print(sliceN)   # Stores outcome's row values in a list ..
#         lowest <- min(sliceN,na.rm=TRUE)
#             # print(lowest)   # .. since min does not work on factors 
#         result <- slice[,"hospital"][which(sliceN == lowest)]
#             # print(result) 
#         ans <- result[order(result)]
#             # print(ans)  
#     }
#     return(ans)
# }

best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(!state %in% fd[, "state"]){
        stop('invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {
        si <- which(fd[, "state"] == state)
        ts <- fd[si, ]    # extracting data for the called state
        oi <- as.numeric(ts[, eval(outcome)])
        min_val <- min(oi, na.rm = TRUE)
        result  <- ts[, "hospital"][which(oi == min_val)]
        output  <- result[order(result)]
    }
    return(output)
}

# example output:
best("SC", "heart attack")  