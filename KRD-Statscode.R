initialize <- function() {
    library(ggplot2)
    library(dplyr)
    library(gtools)
    library(readr)
    library(magrittr)
}

CleanJammerData <- function(stats) {
    return(stats %>% filter(Lead==TRUE))
}

ShowJammerLeadData <- function(stats) {
    stats %>% group_by(Jammer) %>% summarize(TimeToLead = weighted.mean(TimeToLead, na.rm=TRUE)) %>% arrange(TimeToLead)
}

JammerLeadPerc <- function(jammers){
    #make jammer numbers into strings
    #for each jammer
    #make temp true count
    #make temp false count
    #test for jammer in KRDstats
    #if $Lead == True
    #truecount++
    #elseif #Lead == false
    #falsecount++
    #add jammerdataframe$leadperc truecount/(truecount + falsecount)
}

JammerLeadTime <- function(jammers) {
    #make jammer numbers into strings
    #for each jammer
    #make temp list
    #test for jammer in jammerleaddata
    #append $TimeToLead to temp
    #append to jammerdataframe$leadtime weighted.mean of temp, na.rm=TRUE
}

JammerPointDiff <- function(jammers) {
    #make jammer numbers into strings
    #for each jammer in list
    #make temp pointsfor
    #make temp pointsagainst
    #test for jammer in KRDstats
    #append temp1 with pointsfor
    #append temp2 with pointsagainst
    #temp1 weighted.mean, na.rm=TRUE
    #temp2 weighted.mean, na.rm=TRUE
    #append jammerdataframe$jammer with temp1-temp2
}

PlotJammerLead <- function(number) {
    number <- toString(number)
    temp <- JammerLeadData %>% filter(Jammer == number)
    return(ggplot(temp, aes(y=TimeToLead, x=number)) + geom_boxplot())
}

CompleteJammerData <- function(jammers) {
    JammerDataFrame$Jammers <- jammers
    JammerDataFrame$PointDiff <- JammerPointDiff(jammers)
    JammerDataFrame$LeadPerc <- JammerLeadPerc(jammers)
    JammerDataFrame$LeadTime <- JammerLeadTime(jammers)
    JammerDataFrame <- JammerDataFrame[order(JammerDataFrame$PointDiff), ]
    return(JammerDataFrame)
}

CleanBlockerData <- function(stats) {
    return(stats %>% filter(Lead == FALSE))
}

GetTrios <- function(BlockerList) {
    combs <- combinations(length(BlockerList), 3)
    n <- 1
    while(n <= 3) {
        i <- 1
        while(i <= nrow(combs)) {
            combs[i,n] <- BlockerList[combs[i,n]]
            i <- i+1
        }
        n <- n+1
    }
    return(combs)
}

BlockerPointDiff <- function(trios) {
    #for every row in trios
    #make temp for pointsfor
    #make temp for pointsagainst
    #test for trios in KRDstats
    #append temp1 with pointsfor
    #append temp2 with pointsagainst
    #temp1 weighted.mean, na.rm=TRUE
    #temp2 weighted.mean, na.rm=TRUE
    #add to the trios$pointdiff[row] temp1-temp2
}

BlockerLeadAverage <- function(trios) {
    #for every row in trios
    #make temp list of trios
    #make temp empty list
    #test temp in rows of blockerdata
    #append TimeToLead to empty temp list
    #append weighted.mean of temp in trios$LeadAvg
}

CompleteBlockerData <- function(blockers) {
    Trios <- data.frame(GetTrios(blockers))
    Trios$PointDiff <- BlockerPointDiff(blockers)
    Trios$LeadAvg <- BlockerLeadAverage(blockers)
    Trios <- Trios %>% arrange(desc(PointDiff))
    return(Trios)
}

initialize()

KRDstats <- read_csv("C:/Users/cathe/Documents/GitHub/KRD-Stats/KRDstats.csv", col_types = cols(Blocker1 = col_number(), Blocker2 = col_number(),
                                                      Blocker3 = col_number(), Blocker4 = col_number(), 
                                                      Date = col_date(format = "%m/%d/%Y"), Jammer = col_character(), 
                                                      PointsAgainst = col_number(), PointsFor = col_number(), 
                                                      TimeToLead = col_number()), na = "NA")

JammerLeadData <- CleanJammerData(KRDstats)
ShowJammerLeadData()
