# Function to flag groups with multiple species with same generic code
#
# If a group has multiple species within a generic code AND the generic code called, it is not easy to classify the match
# When combined with group_by(tranID, begend, grp) to group by observation group and mutate(...) this function creates a new column
# with "no" or "yes" ... yes = ambiguous group
#

ambiguousGrps.fn <- function(inFrame) { # function should follow group_by that subsets to observation groups
  
  out <- "no"
  generics <- unique(spgroup)
  
  for (i in generics) { # for each generic group ...
    if (nrow(inFrame %>% filter(genericSp == i)) > 1) { # must have at least 2 records in group
      test <- nrow(distinct(inFrame %>% filter((species != i) & (genericSp == i)), species)) # must have at least 2 records called to species
      if (test > 1) {
        test2 <- nrow(inFrame %>%  filter(species == i))
        if (test2 >0) out <- "yes" # must have at least one generic record as well
      }
    }
  }
  
  out
}
