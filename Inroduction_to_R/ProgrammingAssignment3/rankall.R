rankall <- function(outcome, num = "best") {
        outcome_dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomes_pos <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        state.lst <- unique(outcome_dt$State)
        if ((outcome %in% names(outcomes_pos)) == F) {
                stop(" invalid outcome ")
        }
        #hospital_subset <- subset(outcome_dt, select = c("Hospital.Name", "State" 
        #                                                 colnames(outcome_dt)[outcomes_pos[[outcome]]]))
        #hospital_subset[,3] <- as.numeric(hospital_subset[,3])
        #hospital_subset <- hospital_subset[complete.cases(hospital_subset), ]
        
        for (state in state.lst) {
                state_subset <- subset(outcome_dt, State == state, 
                                       select = c("Hospital.Name", "State", colnames(outcome_dt)[outcomes_pos[[outcome]]]))
                state_subset[, 3] <- as.numeric(state_subset[, 3])
                state_subset <- state_subset[complete.cases(state_subset), ]
                hospital_rank <- state_subset[with(state_subset, order(state_subset[,3], Hospital.Name)), ]
                Hospital_Name <- data.frame()
                if (num == "best") {
                        Hospital.lst <- rbind(hospital_rank[1,1], hospital_rank[1,2])
                }
                else if (num == "worst") {
                        Hospital.lst <- rbind(hospital_rank[nrow(hospital_rank), 1], hospital_rank[nrow(hospital_rank), 2])
                }
                else {
                        Hospital.lst <- rbind(hospital_rank[num, 1], hospital_rank[num, 2]) 
                }
        }
        Hospital_Name <- rbind(Hospital_Name, Hospital.lst)
        return(Hospital_Name)
}