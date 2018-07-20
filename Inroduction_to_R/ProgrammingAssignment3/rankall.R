
rankall <- function(outcome, num = "best") {
        # Read input CSV file
        outcome_dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Define a list with all possible outcomes with respective column number
        outcomes_pos <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        # Define a list with all unique states
        state.lst <- unique(outcome_dt$State)
        
        # Validity for outcome
        if ((outcome %in% names(outcomes_pos)) == F) {
                stop(" invalid outcome ")
        }
        
        # Define list to store Hospital name and their state
        Hospital.lst <- list()
        state.out <- list()
        
        for (state in sort(state.lst)) {
                
                # Subset of data based on state 
                state_subset <- subset(outcome_dt, State == state, 
                                       select = c("Hospital.Name", "State", colnames(outcome_dt)[outcomes_pos[[outcome]]]))
                
                # Convert mortality rate to numeric class
                state_subset[, 3] <- as.numeric(state_subset[, 3])
                
                # Remove NA values
                state_subset <- state_subset[complete.cases(state_subset), ]
                
                # Order on basis of mortality rate
                hospital_rank <- state_subset[with(state_subset, order(state_subset[,3], Hospital.Name)), ]
                
                # Condition for input rank of hospital
                if (num == "best") {
                        Hospital.lst <- append(Hospital.lst, hospital_rank[1,1])
                        state.out <- append(state.out, state)
                }
                else if (num == "worst") {
                        Hospital.lst <- append(Hospital.lst, hospital_rank[nrow(hospital_rank), 1])
                        state.out <- append(state.out, state)
                }
                else {
                        Hospital.lst <- append(Hospital.lst, hospital_rank[num, 1])
                        state.out <- append(state.out, state)
                }
        }
        
        # Insert hospital name and respective state in a dataframe
        Hospital_Name <- do.call(rbind, Map(data.frame, hospital = Hospital.lst, state = state.out))
        
        # Remove NA values (if no hospital on particular rank)
        Hospital_Name <- Hospital_Name[complete.cases(Hospital_Name), ]
        
        # Return Hospital_Name
        return(Hospital_Name)
}
