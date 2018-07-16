best <- function(state, outcome) {
        ## Read outcome data
        outcome_dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        state.lst <- unique(outcome_dt$State)
        outcomes_pos <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        if ((state %in% state.lst) == F) {
                stop(" invalid state ")
        }
        else if ((outcome %in% names(outcomes_pos)) == F) {
                stop(" invalid outcome ")
        }
        
        state_subset <- subset(outcome_dt, State == state, 
                               select = c("Hospital.Name", colnames(outcome_dt)[outcomes_pos[[outcome]]]))
        state_subset[,2] <- as.numeric(state_subset[,2])
        state_subset <- state_subset[complete.cases(state_subset), ]
        Hospital_Name <- (state_subset$Hospital.Name[which.min(state_subset[,2])])
        return(Hospital_Name)
}