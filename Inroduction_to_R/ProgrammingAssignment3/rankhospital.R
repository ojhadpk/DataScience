rankhospital <- function(state, outcome, num="best") {
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
        hospital_rank <- state_subset[with(state_subset, order(state_subset[,2], Hospital.Name)), ]
        if (num == "best") {
                Hospital_Name = hospital_rank[1,1]
        }
        else if (num == "worst") {
                Hospital_Name = hospital_rank[nrow(hospital_rank), 1]
        }
        else {
                Hospital_Name = hospital_rank[num, 1]
        }
        return(Hospital_Name)
}