Live Session 6 Assignment Code:

library(dplyr, quietly = TRUE, warn.conflicts =  FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)

1A.
#my code#
setwd("C:\\Users\\Audrene\\Desktop\\SMU_MSDS_PROGRAM\\Term1_spring2019\\MSDS6306\\HW\\live_session_6")
mental.sur<-load(file="N-MHSS-2015-DS0001-data-r.rda")
mental.sur
survey<-mh2015_puf

1B.
states<-levels(survey$LST)
levels(survey$LST) <- gsub(" ","",levels(survey$LST))
print (paste("All State Abbreviations: ", paste(unique(survey$LST), collapse = " ")))


1C.
names(survey)
facility<-levels(survey$FACILITYTYPE)
fac_byState <- survey %>% filter(survey$FACILITYTYPE == "Veterans Administration medical center (VAMC) or other VA health care facility") %>% count(LST)
drops <- c('AK', 'HI', 'PR', 'AS', 'FM', 'GU', 'MH', 'MP', 'PW', 'PR', 'VI')
va_centers <- data.frame(fac_byState[!(fac_byState$LST %in% drops), ])
names(va_centers)<-c("State", "Counts")

1D.

ggplot(va_centers,aes(x=State,y=Counts,fill=State)) + 
geom_bar(aes(reorder(State,Counts)), stat="identity") + 
theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0), legend.position = "right") +
ggtitle("Number of VA Hospitals in the USA by State") + xlab("State") + ylab("Counts of VA Hospitals") + theme(plot.title= element_text(hjust = 0.5))

2A.
state <- read.csv("statesize.csv")
str(statesize)
state.va <- merge(va_centers,state, by.x = 'State', by.y = 'Abbrev', all.x = TRUE)
str(state.va) 
anyNA(state.va) 

state.va[rowSums(is.na(state.va)) > 0,]

*comment: The issue was that the abbreviated State names in the __VA hospital dataset__ had __spaces__ in it, hence it does not match with the abbreviation in __statesize__ dataset. This has already been fixed in #1, hence we did not see this issue. If it was not fixed, we would have seen this issue now.

2B.

Note: This has already been corrected in #1 using the __gsub command__, hence merging in 2A was successful.

2C.

state.va$num.per.1000.sq.miles <- state.va$Count / state.va$SqMiles * 1000
summary(state.va)

2D.

ggplot(na.omit(state.va),aes(x=State,y=num.per.1000.sq.miles,fill=Region)) + 
  geom_bar(aes(reorder(State,-num.per.1000.sq.miles)), stat="identity")  + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) +
  ggtitle("Number of VA Hospitals in the USA by State") + xlab("State") + ylab("Count of VA Hospitals per 1000 sq. miles") +
  theme(plot.title= element_text(hjust = 0.5)) 

2E. 

The __Northeast__ region seems to have the most VA Hospitals per 1000 square miles. 
* The __West__ region seems to have the least VA hospitals per 1000 square miles. 

From the above analysis, it might seem like the states in the West should be targetted for building more VA hospitals since they have the least number of hospitals per 1000 sq. miles. However, this can be a little misleading. States in the Northeast are smaller in size and more densely populated than other states in general. Hence they may need more hospitals per 1000 sq miles to service a potentially larger population of veterans. States in other parts of the country are bigger in size and less densely populated (especially the west). In these states, analysis of VA hospitals needs to take the population density into account. For example, there is no point in building a VA hospital in the middle of a remote location where very few people live in a 1000 mile radius. However, we may need more hospitals in a small region that is more densely populated.

__In conclusion__, more analysis is needed here and needs to take into account not only state populations, but also population densities within each state. Another thing to consider is the number of veretans in each state. Just because a state has a larger size or more population than another state, does not mean that it  will have more veterans than the other state.

