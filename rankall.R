rankall <- function(outcome, num="best"){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[ ,c(2,7,11,17,23)]
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    
    if(class(num) == "numeric" && num > nrow(data)){
        return ("NA")
    }
    
    if(outcome == "heart attack") {
        data = data[ ,c(1,2,3)]
    } else if(outcome == "heart failure") {
        data = data[ ,c(1,2,4)]
    } else if(outcome == "pneumonia") {
        data = data[ ,c(1,2,5)]
    }
    names(data)[3] = "Deaths"
    data[ ,3] = suppressWarnings( as.numeric(data[ ,3]) )
    
    # Remove rows with NA
    data = data[!is.na(data$Deaths), ]
    
    ##Return a data frame with the hospital names and the abbreviated state name 
    bystate = split(data, data$State)
    ans = lapply(bystate, function(rslt, num) {
        # Order by Deaths and then HospitalName
        rslt = rslt[order(rslt$Deaths, rslt$Hospital.Name), ]
        ##data = data[order(data$Deaths, data$Hospital.Name), ]
  
        if(class(num) == "character") {
            if(num == "best") {
                return (rslt$Hospital.Name[1])
            }
            else if(num == "worst") {
                return (rslt$Hospital.Name[nrow(x)])
            }
        }
        else {
            return (rslt$Hospital.Name[num])
        }
    }, num)
    
    #Return data.frame with format
    return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}