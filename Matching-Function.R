# This function reads in a data frame for all groups where there are bird observations for both observers (f/r)
# It compares the records and classifies each bird observation as follows:
#
# 1. checks for perfect match of species-count-type, classifies as perfectMatch
# 1b. checks scoter and scaup for band match .... if bands match, perfectMatch_B (classified as perfect if band doesn't match)
# 2. checks species records for each observer for perfect match to a generic for other observer (perfectMatchGen)
# 3. After removing perfect matches, checks remaining species in groups and codes as follows:
# 4. i. noMatchgrp = only one bird for one observer left after perfect matches, this is similar to a "noMatch" record, but in a group
#   ii. countMatchSp = if front and rear share species, check if they match when summed over all species in group
#   iii. countMatchGeneric = if front/rear has species and rear/front has corresponding generic, check if sum totBirds match
# Note: if there are TWO+ species coded + generic, the matching is ambiguous and not considered 
# Matching function is run after ambiguous groups are flagged, ambiguous groups have all birds except perfect matches coded misMatch
# There are 0 ambiguous groups
#   iv. speciesMatch = if species match but not total bird count for species
#    v. genericMatch = if front/rear has species and rear/front has generic, counts do not match
# all remaining records are "misMatch" ... including same generic coded to different species (e.g., f = BLSC and r = SUSC)



Matching.fn <- function(inFrame) {
 
  outFrame <- NULL
  
  front <- inFrame %>% filter(pos == "f") %>% select(species, count, genericSp, index) %>%
    mutate(check1 = paste(species,count), check2 = paste(genericSp,count))
  
  rear <- inFrame %>% filter(pos == "r") %>% select(species, count, genericSp, index) %>%
    mutate(check1 = paste(species,count), check2 = paste(genericSp,count))
  
  # At start, both front and rear have at least one observation in group, or function is not called
  
  # 1. Code any perfect matches (species, count both match) ####
  for (i in 1:nrow(front)) {
    if (nrow(rear) > 0) { # rear is decremented by each match, so need to ensure there are still records to check
      testMatch <- rear$index[front$check1[i] == rear$check1][1] # select first match
      if(!is.na(testMatch)) { # if there is a match, do
        matchType <- "perfectMatch"

        outFrame <- bind_rows(outFrame, 
                              inFrame %>% filter(index %in% c(front$index[i],testMatch)) %>% mutate(reconcile = matchType)
        )
        rear <- rear %>% filter(index != testMatch)    # drop matched record  
      } # end reconcile code update if match exists
    } # end action loop if rear still has records to check
  } # end i loop
  
  # 2. Check for perfect generic match (swap generic in for Front species, swap generic in for Rear species) ####
  # Note: can't do front/rear simultaneously or could match BLSC to SUSC, e.g., only want BLSC to SCOT, etc.
  # This happens after any generics are matched
  
  # 2a. Check front species to rear generic: ####
  front <- front %>% filter(!(index %in% outFrame$index)) # matched records were dropped in rear, need to be dropped from front
  if (nrow(front) > 0) { # first need to check there are still front records to match ....
    # This is the same code as above, using check2 for front, then repeated with check2 for rear:
    for (i in 1:nrow(front)) {
      if (nrow(rear) > 0) { # rear is decremented by each match, so need to ensure there are still records to check
        testMatch <- rear$index[front$check2[i] == rear$check1][1] # select first match
        if(!is.na(testMatch)) { # if there is a match, do
          matchType <- "perfectMatchGen"

          outFrame <- bind_rows(outFrame, 
                                inFrame %>% filter(index %in% c(front$index[i],testMatch)) %>% mutate(reconcile = matchType)
          )
          rear <- rear %>% filter(index != testMatch)    # drop matched record  
        } # end reconcile code update if match exists
      } # end action loop if rear still has records to check
    } # end i loop   
  } # end action loop if front still has records to check
  
  # 2b. Check rear species to front generic: ####
  front <- front %>% filter(!(index %in% outFrame$index)) # matched records were dropped in rear, need to be dropped from front
  if (nrow(rear) > 0) { # first need to check there are still rear records to match ....
    # This is the same code as above, using check2 for rear, then repeated with check2 for front:
    for (i in 1:nrow(rear)) {
      if (nrow(front) > 0) { # front is decremented by each match, so need to ensure there are still records to check
        testMatch <- front$index[rear$check2[i] == front$check1][1] # select first match
        if(!is.na(testMatch)) { # if there is a match, do
          matchType <- "perfectMatchGen"

          outFrame <- bind_rows(outFrame, 
                                inFrame %>% filter(index %in% c(rear$index[i],testMatch)) %>% mutate(reconcile = matchType)
          )
          front <- front %>% filter(index != testMatch)    # drop matched record  
        } # end reconcile code update if match exists
      } # end action loop if front still has records to check
    } # end i loop   
  } # end action loop if rear still has records to check
  rear <- rear %>% filter(!(index %in% outFrame$index)) # matched records were dropped in front, need to be dropped from rear
  
  # 3. consider case of count matches ... ####
  # This occurs for differences in recording "type" or if observers have "broken up" observations into different social pairings
  if (nrow(front) == 0 | nrow(rear) == 0)  { # are there observations left to compare?
    if (nrow(front) > 0) { # one or both could be empty after perfect matches accounted for ....
      testMatch <- front$index
      outFrame <- bind_rows(outFrame, 
                            inFrame %>% filter(index %in% testMatch) %>% mutate(reconcile = "noMatchgrp"))
    }
    if (nrow(rear) > 0) {
      testMatch <- rear$index
      outFrame <- bind_rows(outFrame, 
                            inFrame %>% filter(index %in% testMatch) %>% mutate(reconcile = "noMatchgrp"))
    }
    
  } else { 
    # determine if there are species matches in other observer's records:
    
    sp.match <- intersect(front$species, rear$species)
    if(length(sp.match) > 0) { # check counts for matched species
      compareCounts <- full_join(front %>% filter(species %in% sp.match) %>% 
                                   select(species, tBf = count) %>% group_by(species) %>% 
                                   summarize(tBf = sum(tBf)),
                                 rear %>% filter(species %in% sp.match) %>% 
                                   select(species, tBr = count) %>% group_by(species) %>%
                                   summarize(tBr = sum(tBr))) %>%
        ungroup() %>%
        mutate(countMatch = (tBf == tBr))
      
      sp.matchCount <- compareCounts$species[compareCounts$countMatch] # species with count matches
       
      testMatch <- c(front$index[front$species %in% sp.matchCount],
                     rear$index[rear$species %in% sp.matchCount])
      outFrame <- bind_rows(outFrame, 
                            inFrame %>% filter(index %in% testMatch) %>% mutate(reconcile = "countMatchSp"))
      
      # Remove matched species-count
      front <- front %>% filter(!(species %in% sp.matchCount)) # drop species with count matches
      rear <- rear %>% filter(!(species %in% sp.matchCount))
    }
       
    # check for generic matches ....
    generic.match <- intersect(front$genericSp, rear$genericSp)

    
    if(length(generic.match) > 0 & 
       (unique(inFrame$ambigGrp) == "no")) { # check counts for generics, not including ambiguous groups
        compareCounts <- full_join(front %>% filter(genericSp %in% generic.match) %>% 
                                     select(genericSp, tBf = count) %>% group_by(genericSp) %>% 
                                     summarize(tBf = sum(tBf)),
                                   rear %>% filter(genericSp %in% generic.match) %>% 
                                     select(genericSp, tBr = count) %>% group_by(genericSp) %>%
                                     summarize(tBr = sum(tBr))) %>%
          ungroup() %>%
          mutate(countMatch = (tBf == tBr))
        
        generic.matchCount <- compareCounts$genericSp[compareCounts$countMatch] # generics with count matches
        testMatch <- c(front$index[front$genericSp %in% generic.matchCount],
                       rear$index[rear$genericSp %in% generic.matchCount])
        outFrame <- bind_rows(outFrame, 
                              inFrame %>% filter(index %in% testMatch) %>% mutate(reconcile = "countMatchGeneric"))
        
        front <- front %>% filter(!(genericSp %in% generic.matchCount)) # drop species with generic count matches
        rear <- rear %>% filter(!(genericSp %in% generic.matchCount))
    }   
    
    # Code species matches where counts do not match
    sp.match <- intersect(front$species, rear$species) # species without count matches ....
    testMatch <- c(front$index[front$species %in% sp.match],
                   rear$index[rear$species %in% sp.match])
    outFrame <- bind_rows(outFrame, 
                          inFrame %>% filter(index %in% testMatch) %>% mutate(reconcile = "speciesMatch"))
    
    front <- front %>% filter(!(species %in% sp.match)) # drop species with count matches
    rear <- rear %>% filter(!(species %in% sp.match))
    # Code generic matches where counts do not match
    generic.match <- intersect(front$genericSp, rear$genericSp) # generic without count matches .... include ambiguous groups

    testMatch <- c(front$index[front$genericSp %in% generic.match],
                   rear$index[rear$genericSp %in% generic.match])
    outFrame <- bind_rows(outFrame, 
                          inFrame %>% filter(index %in% testMatch) %>% mutate(reconcile = "genericMatch"))
    
    front <- front %>% filter(!(genericSp %in% generic.match)) # drop species with generic count matches
    rear <- rear %>% filter(!(genericSp %in% generic.match))
      
  
    sp.mismatch <- setdiff(union(front$species,rear$species),intersect(front$species,rear$species))
  
    if(length(sp.mismatch) > 0) { # start code mismatched species
      testMatch <- c(front$index[front$species %in% sp.mismatch],
                     rear$index[rear$species %in% sp.mismatch])
      outFrame <- bind_rows(outFrame, 
                            inFrame %>% filter(index %in% testMatch) %>% mutate(reconcile = "misMatch"))
    } # end coding misMatched species 
    
    
    
  } # end accounting for recording and count differences
  outFrame <- arrange(outFrame, index) 

 # print(outFrame)
  
  outFrame$reconcile # inFrame index should be smallest to largest within grp
} # End of matching function
