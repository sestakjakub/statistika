statesInfo <- read.csv('stateData.csv')

subset(statesInfo, state.region == 1)

#dataSet[ROWS,COLUMNS]
statesInfo[statesInfo$state.region == 1, ]
