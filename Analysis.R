
#################################################################################################################################################
# Generic Analysis

library(sqldf)

path <- '/Users/blairbilodeau/Documents/Research/NSERC_USRA_2017/Simulation_Model/Model7/'
warmup <- 4368
sim <- 0
version <- 1
ward <- 1
runs <- 1:20
swap <- F
output1.df <- as.data.frame(runs); names(output1.df) <- c('Run')
output3.df <- as.data.frame(runs); names(output3.df) <- c('Run')
net.patient.df <- data.frame(); net.census.df <- data.frame(); net.nurse.df <- data.frame()
num_patients <- c()
for (run in runs){
  
  # read in files
  infileA <- paste0('v', toString(version), 'r', toString(run), '.csv')
  infileB <- paste0('v', toString(version), 'w', toString(ward), 'r', toString(run), '.csv')
  if (sim %in% c(0,6,8)){
    infile <- infileA
  } else {
    infile <- infileB
  }
  
  patient.df <- read.csv(paste0(path, 'Sim', toString(sim), '/patient_df_', infile))
  census.df <- read.csv(paste0(path, 'Sim', toString(sim), '/census_df_', infile))
  nurse.df <- read.csv(paste0(path, 'Sim', toString(sim), '/nurse_df_', infile))
  
  # interarrival times
  interarrival <- c(0)
  prev <- patient.df$X[1]
  for (curr in patient.df$X[2:length(patient.df$X)]){
    val <- patient.df[patient.df$X==curr, 'Admit_Time'] - patient.df[patient.df$X==prev, 'Admit_Time']
    interarrival <- c(interarrival, val)
    prev <- curr
  }
  patient.df$Interarrival <- interarrival
  
  # exclude warmup and patients who didn't finish treatment
  patient.df <- patient.df[patient.df$Admit_Time>warmup & !is.na(patient.df$Discharged),]
  if (swap){
    patient.df <- patient.df[patient.df$Initial_Team>0,]
  } else {
    patient.df <- patient.df[patient.df$Team>0,]
  }
  census.df <- census.df[census.df$X>warmup,]
  nurse.df <- nurse.df[nurse.df$X>warmup,]
  
  # patient metrics
  patient.df$Admit_Week <- patient.df$Admit_Time%/%167
  patient.df$Wait <- pmin(patient.df$Floor_Time-patient.df$Admit_Time, patient.df$Decant_Time-patient.df$Admit_Time, patient.df$Off.Service_Time-patient.df$Admit_Time, na.rm=T)
  patient.df$Total <- patient.df$Discharged - patient.df$Admit_Time
  patient.df$Admit_Hour <- (patient.df$Admit_Time%%24)%/%4
  patient.df$Admit_Day <- (patient.df$Admit_Time %/% 24) %% 7
  patient.df$Order_Hour <- patient.df$Discharge_Order%%24
  patient.df$Order_Day <- (patient.df$Discharge_Order %/% 24) %% 7
  patient.df$Decant_Hour <- patient.df$Decant_Time%%24
  patient.df$Discharged_Hour <- patient.df$Discharged%%24
  patient.df$Discharged_Day <- (patient.df$Discharged %/% 24) %% 7
  patient.df$Decant_Length <- patient.df$Floor_Time-patient.df$Decant_Time
  if (swap)
    patient.df$Swapped <- patient.df$Initial_Team != patient.df$Floor_Team
  
  # census metrics
  census.df$Team1 <- census.df$Team1_Emerg + census.df$Team1_Decant + census.df$Team1_Off.Service + census.df$Team1_Floor
  census.df$Team2 <- census.df$Team2_Emerg + census.df$Team2_Decant + census.df$Team2_Off.Service + census.df$Team2_Floor
  census.df$Team3 <- census.df$Team3_Emerg + census.df$Team3_Decant + census.df$Team3_Off.Service + census.df$Team3_Floor
  census.df$Emerg <- census.df$Team1_Emerg + census.df$Team2_Emerg + census.df$Team3_Emerg
  census.df$Decant <- census.df$Team1_Decant + census.df$Team2_Decant + census.df$Team3_Decant
  census.df$Off.Service <- census.df$Team1_Off.Service + census.df$Team2_Off.Service + census.df$Team3_Off.Service
  census.df$Medicine_Floor <- census.df$Team1_Floor + census.df$Team2_Floor + census.df$Team3_Floor
  census.df$Medicine_Utilization <- census.df$Medicine_Floor / 72
  census.df$Floor_Utilization <- (census.df$Medicine_Floor + census.df$Non.Medicine_Floor) / 72

  # census team variance
  team.avg <- rowMeans(cbind(census.df$Team1, census.df$Team2, census.df$Team3))
  floor.team.avg <- rowMeans(cbind(census.df$Team1_Floor, census.df$Team2_Floor, census.df$Team3_Floor)) 
  census.df$Team_Variance <- ((census.df$Team1 - team.avg)^2 + (census.df$Team2 - team.avg)^2 + (census.df$Team3 - team.avg)^2) / 3
  census.df$Floor_Variance <- ((census.df$Team1_Floor - floor.team.avg)^2 + (census.df$Team2_Floor - floor.team.avg)^2 + (census.df$Team3_Floor - floor.team.avg)^2) / 3
  
  # nurse metrics
  nurse.df$Team1_PPN <- nurse.df$Team1_Patients / nurse.df$Team1_Nurses
  nurse.df$Team2_PPN <- nurse.df$Team2_Patients / nurse.df$Team2_Nurses
  nurse.df$Team3_PPN <- nurse.df$Team3_Patients / nurse.df$Team3_Nurses
  
  # make sure each patient is being matched up 1 to 1 from baseline
  # input.df <- read.csv(paste0(path, 'Sim0/patient_df_', infileA))  
  # error.df <- sqldf("SELECT Start_Time, SUM(Old_Consults) + SUM(New_Consults) AS Num_Visited FROM 'rounds.df' WHERE Type='AM' GROUP BY Start_Time")
  # diff <- numeric(nrow(error.df))
  # for (row in seq(1, nrow(error.df))){
  #   time <- error.df[row, 'Start_Time']
  #   diff[row] <- nrow(input.df[input.df$Admit_Time<time & input.df$Discharge_Order>time+3 & input.df$Team>0,]) - error.df[row, 'Num_Visited']
  # }
  # error.df$Error <- diff
  
  patient.df <- cbind(Run=rep(run, nrow(patient.df)), patient.df)
  census.df <- cbind(Run=rep(run, nrow(census.df)), census.df)
  nurse.df <- cbind(Run=rep(run, nrow(nurse.df)), nurse.df)
  if (run == 1){
    net.patient.df <- patient.df
    net.census.df <- census.df
    net.nurse.df <- nurse.df
  } else {
    net.patient.df <- rbind(net.patient.df, patient.df)
    net.census.df <- rbind(net.census.df, census.df)
    net.nurse.df <- rbind(net.nurse.df, nurse.df)
  }
  
  output1.df[output1.df$Run==run,'W_Tot_Emerg'] <- census.df[1,'Emerg']
  output1.df[output1.df$Run==run,'W_Floor_Util'] <- census.df[1,'Floor_Utilization']
  output1.df[output1.df$Run==run,'W_Medicine_Util'] <- census.df[1,'Medicine_Utilization']
  output1.df[output1.df$Run==run,'S_Avg_LOS'] <- mean(patient.df$Total/24)
  output1.df[output1.df$Run==run,'S_SD_LOS'] <- sd(patient.df$Total/24)
  output1.df[output1.df$Run==run,'S_Avg_Wait'] <- mean(patient.df$Wait)
  output1.df[output1.df$Run==run,'S_Avg_11am_Discharge'] <- sum(patient.df$Discharged_Hour < 11) / nrow(patient.df)
  output1.df[output1.df$Run==run,'S_Avg_2pm_Discharge'] <- sum(patient.df$Discharged_Hour < 14) / nrow(patient.df)
  output1.df[output1.df$Run==run,'S_Avg_Decant_Stay'] <- mean(patient.df$Decant_Length, na.rm=T)
  output1.df[output1.df$Run==run,'S_Pct_Decant'] <- nrow(patient.df[!is.na(patient.df$Decant_Time),]) / nrow(patient.df)
  output1.df[output1.df$Run==run,'S_Pct_Off_Service'] <- nrow(patient.df[!is.na(patient.df$Off.Service_Time) | !(patient.df$Origin %in% c('ED', 'ICU')),]) / nrow(patient.df)
  output1.df[output1.df$Run==run, 'S_Pct_Weekend_Discharge'] <- nrow(patient.df[patient.df$Discharged_Day %in% c(5,6),]) / nrow(patient.df)
  if (swap){
    output1.df[output1.df$Run==run,'S_Pct_Change_Team'] <- nrow(patient.df[patient.df$Initial_Team != patient.df$Floor_Team,]) / nrow(patient.df)
  }
  output1.df[output1.df$Run==run,'S_Pct_No_Ward'] <- sum(patient.df$Floor_Time==patient.df$Discharged) / nrow(patient.df)
  output1.df[output1.df$Run==run,'S_Avg_Tot_Emerg'] <- mean(census.df[census.df$Time=='MIDNIGHT', 'Emerg'])
  output1.df[output1.df$Run==run,'S_Avg_Floor_Util'] <- mean(census.df[census.df$Time=='MIDNIGHT', 'Floor_Utilization'])
  output1.df[output1.df$Run==run,'S_Avg_Medicine_Util'] <- mean(census.df[census.df$Time=='MIDNIGHT', 'Medicine_Utilization'])
  output1.df[output1.df$Run==run,'S_Avg_Census_Var'] <- mean(census.df[census.df$Time=='MORNING', 'Team_Variance'])
  output1.df[output1.df$Run==run,'S_Avg_Floor_Var'] <- mean(census.df[census.df$Time=='MORNING', 'Floor_Variance'])
  
  output3.df[output3.df$Run==run,'Avg_Team1_StartDay_PPN'] <- mean(nurse.df[nurse.df$Time=='START DAY', 'Team1_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team1_EndDay_PPN'] <- mean(nurse.df[nurse.df$Time=='END DAY', 'Team1_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team1_StartNight_PPN'] <- mean(nurse.df[nurse.df$Time=='START NIGHT', 'Team1_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team1_EndNight_PPN'] <- mean(nurse.df[nurse.df$Time=='END NIGHT', 'Team1_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team2_StartDay_PPN'] <- mean(nurse.df[nurse.df$Time=='START DAY', 'Team2_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team2_EndDay_PPN'] <- mean(nurse.df[nurse.df$Time=='END DAY', 'Team2_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team2_StartNight_PPN'] <- mean(nurse.df[nurse.df$Time=='START NIGHT', 'Team2_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team2_EndNight_PPN'] <- mean(nurse.df[nurse.df$Time=='END NIGHT', 'Team2_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team3_StartDay_PPN'] <- mean(nurse.df[nurse.df$Time=='START DAY', 'Team3_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team3_EndDay_PPN'] <- mean(nurse.df[nurse.df$Time=='END DAY', 'Team3_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team3_StartNight_PPN'] <- mean(nurse.df[nurse.df$Time=='START NIGHT', 'Team3_PPN'])
  output3.df[output3.df$Run==run,'Avg_Team3_EndNight_PPN'] <- mean(nurse.df[nurse.df$Time=='END NIGHT', 'Team3_PPN'])
  
  num_patients <- c(num_patients, nrow(patient.df))
}
output1.df$Run <- NULL
output3.df$Run <- NULL
output2.df <- data.frame(sqldf("SELECT Time, AVG(Team1_PPN), AVG(Team2_PPN), AVG(Team3_PPN), VARIANCE(Team1_PPN), VARIANCE(Team2_PPN), VARIANCE(Team3_PPN) FROM 'net.nurse.df' GROUP BY Time"))



floor.plot <- function(run){
  plot(net.census.df[net.census.df$Run==run,'Team1_Floor'], type='l', col='blue')
  lines(net.census.df[net.census.df$Run==run,'Team2_Floor'], col='green')
  lines(net.census.df[net.census.df$Run==run,'Team3_Floor'], col='red')
}
census.plot <- function(run){
  plot(net.census.df[net.census.df$Run==run,'Team1'], type='l', col='blue')
  lines(net.census.df[net.census.df$Run==run,'Team2'], col='green')
  lines(net.census.df[net.census.df$Run==run,'Team3'], col='red') 
}
floor.plot(1)
census.plot(1)

variance.df <- as.data.frame(runs)
for (run in runs){
  variance.df[run, 'Team_Variance'] <- mean(net.census.df[net.census.df$Time=='MORNING' & net.census.df$Run==run, 'Team_Variance'])
}
z <- 1.729 # 95% t-dist
mean(variance.df$Team_Variance) + c(-z, z)*sd(variance.df$Team_Variance)/sqrt(nrow(variance.df))

ppn.df <- as.data.frame(runs)
for (run in runs){
  for (team in 1:3){
    ppn.df[run, paste0('Team', toString(team), '_PPN')] <- mean(net.nurse.df[net.nurse.df$Time=='END NIGHT' & net.nurse.df$Run==run, paste0('Team', toString(team), '_PPN')])
  }
}  
z <- 1.729 # 95% t-dist
for (team in 1:3){
  print(mean(ppn.df[, paste0('Team', toString(team), '_PPN')]) + c(-z, z)*sd(ppn.df[, paste0('Team', toString(team), '_PPN')])/sqrt(nrow(ppn.df))) 
}


hour.df <- as.data.frame(runs)
for (run in runs){
  for (hour in 0:5){
    hour.df[run, paste0('Hour', toString(hour))] <- sum(net.patient.df$Admit_Hour==hour & net.patient.df$Run==run) / sum(net.patient.df$Run==run)
  }
}
CI.df <- as.data.frame(1:2)
z <- 1.729 # 95% t-dist
for (hour in 0:5){
  col <- paste0('Hour', toString(hour))
  CI <- mean(hour.df[,col]) + c(-z, z)*sd(hour.df[,col])/sqrt(nrow(hour.df))
  CI.df[1, col] <- CI[1]
  CI.df[2, col] <- CI[2] 
}
mean(net.patient.df$Discharged - net.patient.df$Discharge_Order)
mean(net.patient.df$Interarrival)
nrow(net.census.df[net.census.df$Time=='MORNING' & net.census.df$Floor_Utilization<0.90,]) / nrow(net.census.df[net.census.df$Time=='MORNING',])
mean(nurse.df[nurse.df$Time=='NIGHT', 'Team3_PPN'])

swap.times <- net.patient.df[net.patient.df$Swapped,'Floor_Time'] - net.patient.df[net.patient.df$Swapped, 'Admit_Time']
hist(swap.times, breaks=30)
hour <- 24
sum(swap.times >= hour) / length(swap.times)
sum(swap.times >= hour) / nrow(net.patient.df)

#hist(error.df$Error)
hist(patient.df$Wait, breaks=100, freq=FALSE, ylim=c(0, 0.01))
barplot(census.df$Total_Utilization)
barplot(net.nurse.df[net.nurse.df$Time=='START DAY', 'Team2_PPN'])
plot(nurse.df[nurse.df$Time=='START NIGHT', 'Team3_PPN']~nurse.df[nurse.df$Time=='START NIGHT', 'X'], type='l')
abline(h=1.0, col='red', lwd=2)
barplot(census.df$Floor_Utilization)
barplot(census.df$Medicine_Utilization)
barplot(census.df$Emerg)
hist(net.patient.df$Interarrival, breaks=50)
plot(net.census.df$Emerg ~ census.df$X, type='l', ylim=c(0,50))
plot(net.census.df$Decant ~ census.df$X)
hist(net.census.df$Decant, breaks=6, freq=F)
hist(net.patient.df$Admit_Hour, breaks=24, freq=F)
hist(net.patient.df$Order_Hour, breaks=24, xlim=c(0,24), freq=F)
hist(net.patient.df$Discharged_Hour, breaks=24, xlim=c(0,24), freq=F)
hist(net.patient.df$Wait / patient.df$Total, breaks=50)
hist(net.patient.df$Decant_Length, breaks=25)
boxplot(net.patient.df$Decant_Length)
hist(net.patient.df$Total/24, breaks=30)
hist(net.census.df$Team1_Floor, breaks = seq(from=8.5, to=31.5, by=1))
hist(net.census.df$Team1, breaks = seq(from=8.5, to=38.5, by=1))
sqldf("SELECT Status, AVG(Wait) AS 'Wait.avg' FROM 'patient.df' GROUP BY Status")
sqldf("SELECT Origin, AVG(Wait) AS 'Wait.avg' FROM 'patient.df' GROUP BY Origin")
sqldf("SELECT Team, AVG(Wait) AS 'Wait.avg' FROM 'patient.df' GROUP BY Team")
sqldf("SELECT Time, AVG(Team_Variance) AS Team_Variance_avg FROM 'net.census.df' GROUP BY Time")

summary(patient.df)
summary(census.df)
summary(nurse.df)

quantile(net.patient.df$Wait, probs=c(0.9, 0.95, 0.99), type=1)

# 95% CI for number of patients
mean(num_patients) + c(-1.96, 1.96)*sd(num_patients)/sqrt(length(num_patients))
                                                      

#################################################################################################################################################


#################################################################################################################################################


histPercent <- function(x, breaks) {
  H <- hist(x, plot = FALSE, breaks=breaks)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density,2), "%", sep="")
  plot(H, freq = FALSE, labels = labs, ylim=c(0, 1.08*max(H$density)))
}

#################################################################################################################################################

boot.mean <- function(data, alpha){
  xbar <- mean(data)
  n <- length(data)
  M <- 10000
  W.boot <- rep(0,M)
  for (i in 1:M){
    data.samp <- sample(data, n, replace=T)
    xbar.samp <- mean(data.samp)
    sd.samp <- sd(data.samp)
    W.boot[i] <- sqrt(n) * (xbar.samp - xbar) / sd.samp
  }
  return(xbar - (quantile(W.boot, c(1-alpha, alpha), names=F) * sd(data) / sqrt(n)))
}

boot.mean(net.census.df$Floor_Utilization, 0.001)
boot.mean(net.census.df$Medicine_Utilization, 0.001)
boot.mean(net.census.df$Enqueued, 0.001)
boot.mean(net.patient.df$Wait, 0.001)
boot.mean(na.omit(net.patient.df$Decant_Length), 0.001)


#################################################################################################################################################
# Comparison of arrival variance


path <- '/Users/blairbilodeau/Documents/Research/NSERC_USRA/Simulation_Model/Model7/'
warmup <- 4368
version <- 1
ward <- 1
swapA <- F
swapB <- F
n <- 1 ## n week intervals
runs <- 1:20
compare.df <- data.frame()
output3.df <- as.data.frame(cbind(simA_Arrival_Var=numeric(length(runs)), simB_Arrival_Var=numeric(length(runs))))
A <- 0; B <- 6
for (run in runs){
  if (A %in% c(0,6,8)){
    infileA <- paste0('v', toString(version), 'r', toString(run), '.csv')
  } else {
    infileA <- paste0('v', toString(version), 'w', toString(ward), 'r', toString(run), '.csv')
  }
  if (B %in% c(0,6,8)){
    infileB <- paste0('v', toString(version), 'r', toString(run), '.csv')
  } else {
    infileB <- paste0('v', toString(version), 'w', toString(ward), 'r', toString(run), '.csv')
  }
  simA.patient.df <- read.csv(paste0(path, 'sim', toString(A), '/patient_df_', infileA))
  simA.census.df <- read.csv(paste0(path, 'sim', toString(A), '/census_df_', infileA))
  simB.patient.df <- read.csv(paste0(path, 'sim', toString(B), '/patient_df_', infileB))
  simB.census.df <- read.csv(paste0(path, 'sim', toString(B), '/census_df_', infileB))
  
  simA.patient.df <- simA.patient.df[simA.patient.df$Admit_Time>warmup & !is.na(simA.patient.df$Discharged),]
  simB.patient.df <- simB.patient.df[simB.patient.df$Admit_Time>warmup & !is.na(simB.patient.df$Discharged),]
  if (swapA){
    simA.patient.df <- simA.patient.df[simA.patient.df$Initial_Team>0,]
  } else {
    simA.patient.df <- simA.patient.df[simA.patient.df$Team>0,]
  }
  
  if (swapB){
    simB.patient.df <- simB.patient.df[simB.patient.df$Initial_Team>0,]
  } else {
    simB.patient.df <- simB.patient.df[simB.patient.df$Team>0,]
  }
  simA.census.df <- simA.census.df[simA.census.df$X>warmup & simA.census.df$Time=='MORNING',]
  simB.census.df <- simB.census.df[simB.census.df$X>warmup & simB.census.df$Time=='MORNING',]
  
  simA.census.df$Team1 <- simA.census.df$Team1_Emerg + simA.census.df$Team1_Decant + simA.census.df$Team1_Off.Service + simA.census.df$Team1_Floor
  simA.census.df$Team2 <- simA.census.df$Team2_Emerg + simA.census.df$Team2_Decant + simA.census.df$Team2_Off.Service + simA.census.df$Team2_Floor
  simA.census.df$Team3 <- simA.census.df$Team3_Emerg + simA.census.df$Team3_Decant + simA.census.df$Team3_Off.Service + simA.census.df$Team3_Floor
  
  simB.census.df$Team1 <- simB.census.df$Team1_Emerg + simB.census.df$Team1_Decant + simB.census.df$Team1_Off.Service + simB.census.df$Team1_Floor
  simB.census.df$Team2 <- simB.census.df$Team2_Emerg + simB.census.df$Team2_Decant + simB.census.df$Team2_Off.Service + simB.census.df$Team2_Floor
  simB.census.df$Team3 <- simB.census.df$Team3_Emerg + simB.census.df$Team3_Decant + simB.census.df$Team3_Off.Service + simB.census.df$Team3_Floor
  
  simA.patient.df <- cbind(simA.patient.df, Admit_nWeek = simA.patient.df$Admit_Time%/%167%/%n, Floor_nWeek = simA.patient.df$Floor_Time%/%167%/%n)
  simB.patient.df <- cbind(simB.patient.df, Admit_nWeek = simB.patient.df$Admit_Time%/%167%/%n, Floor_nWeek = simB.patient.df$Floor_Time%/%167%/%n)
  simA.census.df <- cbind(simA.census.df, Census_nWeek = simA.census.df$X%/%167%/%n)
  simB.census.df <- cbind(simB.census.df, Census_nWeek = simB.census.df$X%/%167%/%n)
                           
  simA.patient.df <- simA.patient.df[simA.patient.df$Admit_nWeek<=76,]
  simB.patient.df <- simB.patient.df[simB.patient.df$Admit_nWeek<=76,]
  simA.census.df <- simA.census.df[simA.census.df$Census_nWeek<=76,]
  simB.census.df <- simB.census.df[simB.census.df$Census_nWeek<=76,]
  
  simA.patient.df$Wait <- pmin(simA.patient.df$Floor_Time-simA.patient.df$Admit_Time, simA.patient.df$Decant_Time-simA.patient.df$Admit_Time, simA.patient.df$Off.Service_Time-simA.patient.df$Admit_Time, na.rm=T)
  simB.patient.df$Wait <- pmin(simB.patient.df$Floor_Time-simB.patient.df$Admit_Time, simB.patient.df$Decant_Time-simB.patient.df$Admit_Time, simB.patient.df$Off.Service_Time-simB.patient.df$Admit_Time, na.rm=T)
  
  nWeeks <- head(unique(simA.patient.df$Admit_nWeek),-1)
  num_weeks = length(nWeeks)
  df <- as.data.frame(cbind(Run=rep(run, num_weeks), nWeek=nWeeks))
  
  for (nWeek in nWeeks){
    if (swapA){
      df[df$nWeek==nWeek, 'simA_Arrival_Team1'] <- nrow(simA.patient.df[simA.patient.df$Admit_nWeek==nWeek & simA.patient.df$Initial_Team==1,])
      df[df$nWeek==nWeek, 'simA_Arrival_Team2'] <- nrow(simA.patient.df[simA.patient.df$Admit_nWeek==nWeek & simA.patient.df$Initial_Team==2,])
      df[df$nWeek==nWeek, 'simA_Arrival_Team3'] <- nrow(simA.patient.df[simA.patient.df$Admit_nWeek==nWeek & simA.patient.df$Initial_Team==3,])
    } else {
      df[df$nWeek==nWeek, 'simA_Arrival_Team1'] <- nrow(simA.patient.df[simA.patient.df$Admit_nWeek==nWeek & simA.patient.df$Team==1,])
      df[df$nWeek==nWeek, 'simA_Arrival_Team2'] <- nrow(simA.patient.df[simA.patient.df$Admit_nWeek==nWeek & simA.patient.df$Team==2,])
      df[df$nWeek==nWeek, 'simA_Arrival_Team3'] <- nrow(simA.patient.df[simA.patient.df$Admit_nWeek==nWeek & simA.patient.df$Team==3,])
    }

    if (swapB){
      df[df$nWeek==nWeek, 'simB_Arrival_Team1'] <- nrow(simB.patient.df[simB.patient.df$Admit_nWeek==nWeek & simB.patient.df$Initial_Team==1,])
      df[df$nWeek==nWeek, 'simB_Arrival_Team2'] <- nrow(simB.patient.df[simB.patient.df$Admit_nWeek==nWeek & simB.patient.df$Initial_Team==2,])
      df[df$nWeek==nWeek, 'simB_Arrival_Team3'] <- nrow(simB.patient.df[simB.patient.df$Admit_nWeek==nWeek & simB.patient.df$Initial_Team==3,])
    } else {
      df[df$nWeek==nWeek, 'simB_Arrival_Team1'] <- nrow(simB.patient.df[simB.patient.df$Admit_nWeek==nWeek & simB.patient.df$Team==1,])
      df[df$nWeek==nWeek, 'simB_Arrival_Team2'] <- nrow(simB.patient.df[simB.patient.df$Admit_nWeek==nWeek & simB.patient.df$Team==2,])
      df[df$nWeek==nWeek, 'simB_Arrival_Team3'] <- nrow(simB.patient.df[simB.patient.df$Admit_nWeek==nWeek & simB.patient.df$Team==3,])
    }
    
    simA_team.avg <- mean(df[df$nWeek==nWeek, 'simA_Arrival_Team1'], df[df$nWeek==nWeek, 'simA_Arrival_Team2'], df[df$nWeek==nWeek, 'simA_Arrival_Team3'])
    df[df$nWeek==nWeek, 'simA_Arrival_Var'] <- ((df[df$nWeek==nWeek, 'simA_Arrival_Team1'] - simA_team.avg)^2 + (df[df$nWeek==nWeek, 'simA_Arrival_Team2'] - simA_team.avg)^2 + (df[df$nWeek==nWeek, 'simA_Arrival_Team3'] - simA_team.avg)^2) / 3
    simB_team.avg <- mean(df[df$nWeek==nWeek, 'simB_Arrival_Team1'], df[df$nWeek==nWeek, 'simB_Arrival_Team2'], df[df$nWeek==nWeek, 'simB_Arrival_Team3'])
    df[df$nWeek==nWeek, 'simB_Arrival_Var'] <- ((df[df$nWeek==nWeek, 'simB_Arrival_Team1'] - simB_team.avg)^2 + (df[df$nWeek==nWeek, 'simB_Arrival_Team2'] - simB_team.avg)^2 + (df[df$nWeek==nWeek, 'simB_Arrival_Team3'] - simB_team.avg)^2) / 3
    
    df[df$nWeek==nWeek, 'simA_Census_Team1'] <- mean(simA.census.df[simA.census.df$Census_nWeek==nWeek, 'Team1'])
    df[df$nWeek==nWeek, 'simA_Census_Team2'] <- mean(simA.census.df[simA.census.df$Census_nWeek==nWeek, 'Team2'])
    df[df$nWeek==nWeek, 'simA_Census_Team3'] <- mean(simA.census.df[simA.census.df$Census_nWeek==nWeek, 'Team3'])
    
    df[df$nWeek==nWeek, 'simB_Census_Team1'] <- mean(simB.census.df[simB.census.df$Census_nWeek==nWeek, 'Team1'])
    df[df$nWeek==nWeek, 'simB_Census_Team2'] <- mean(simB.census.df[simB.census.df$Census_nWeek==nWeek, 'Team2'])
    df[df$nWeek==nWeek, 'simB_Census_Team3'] <- mean(simB.census.df[simB.census.df$Census_nWeek==nWeek, 'Team3'])
  }  
  
  output3.df[run, 'simA_Arrival_Var'] <- mean(df$simA_Arrival_Var)
  output3.df[run, 'simB_Arrival_Var'] <- mean(df$simB_Arrival_Var)
  
  if (run==1){
    compare.df <- df
  } else{
    compare.df <- rbind(compare.df, df)
  }
}

arrival.plot <- function(run){
  plot(compare.df[compare.df$Run==run,'simA_Arrival_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,50), main='Weekly New Admissions by Team \nfor Reference Version', ylab='Number of Patients Admitted', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simA_Arrival_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simA_Arrival_Team3'], col='red')
  legend(35,50,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'))
  
  plot(compare.df[compare.df$Run==run,'simB_Arrival_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,50), main='Weekly New Admissions by Team \nfor Lowest Census Version', ylab='Number of Patients Admitted', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simB_Arrival_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simB_Arrival_Team3'], col='red')
  legend(35,50,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'))
}
arrival.plot(1)

census.plot <- function(run){
  plot(compare.df[compare.df$Run==run,'simA_Census_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,40), main='Weekly Average Census by Team \nfor Reference Version', ylab='Average Morning Census', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simA_Census_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simA_Census_Team3'], col='red')
  legend(35,40,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'))
  
  plot(compare.df[compare.df$Run==run,'simB_Census_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,40), main='Weekly Average Census by Team \nfor Lowest Census Version', ylab='Average Morning Census', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simB_Census_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simB_Census_Team3'], col='red')
  legend(35,40,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'))
}
census.plot(1)

bw.arrival.plot <- function(run){
  plot(compare.df[compare.df$Run==run,'simA_Arrival_Team1'], type='l', lty=1, lwd=1, xlim=c(0,50), ylim=c(10,50), main='Weekly New Admissions by Team \nfor Reference Version', ylab='Number of Patients Admitted', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simA_Arrival_Team2'], lty=2, lwd=1)
  lines(compare.df[compare.df$Run==run,'simA_Arrival_Team3'], lty=3, lwd=2)
  legend(35,50,c('Team A', 'Team B', 'Team C'), lty=c(1,2,3), lwd=c(1,1,2))
  
  plot(compare.df[compare.df$Run==run,'simB_Arrival_Team1'], type='l', lty=1, lwd=1, xlim=c(0,50), ylim=c(10,50), main='Weekly New Admissions by Team \nfor Lowest Census Version', ylab='Number of Patients Admitted', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simB_Arrival_Team2'], lty=2, lwd=1)
  lines(compare.df[compare.df$Run==run,'simB_Arrival_Team3'], lty=3, lwd=2)
  legend(35,50,c('Team A', 'Team B', 'Team C'), lty=c(1,2,3), lwd=c(1,1,2))
}
bw.arrival.plot(1)

bw.census.plot <- function(run){
  plot(compare.df[compare.df$Run==run,'simA_Census_Team1'], type='l', lty=1, lwd=1, xlim=c(0,50), ylim=c(10,40), main='Weekly Average Census by Team \nfor Reference Version', ylab='Average Morning Census', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simA_Census_Team2'], lty=2, lwd=1)
  lines(compare.df[compare.df$Run==run,'simA_Census_Team3'], lty=3, lwd=2)
  legend(35,40,c('Team A', 'Team B', 'Team C'), lty=c(1,2,3), lwd=c(1,1,2))
  
  plot(compare.df[compare.df$Run==run,'simB_Census_Team1'], type='l', lty=1, lwd=1, xlim=c(0,50), ylim=c(10,40), main='Weekly Average Census by Team \nfor Lowest Census Version', ylab='Average Morning Census', xlab='Week')
  lines(compare.df[compare.df$Run==run,'simB_Census_Team2'], lty=2, lwd=1)
  lines(compare.df[compare.df$Run==run,'simB_Census_Team3'], lty=3, lwd=2)
  legend(35,40,c('Team A', 'Team B', 'Team C'), lty=c(1,2,3), lwd=c(1,1,2))
}
bw.census.plot(1)

arrival.pres.plot <- function(run){
  par(cex.main=1.5, cex.lab=1.5)
  
  plot(compare.df[compare.df$Run==run,'simA_Arrival_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,50), main='Weekly New Admissions by Team \nfor Reference Version', ylab='', xlab='')
  lines(compare.df[compare.df$Run==run,'simA_Arrival_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simA_Arrival_Team3'], col='red')
  title(ylab="Number of Patients Admitted", line=2.2)
  title(xlab='Week', line=2.4)
  legend(37,50.5,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'), cex=1.1)
  
  plot(compare.df[compare.df$Run==run,'simB_Arrival_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,50), main='Weekly New Admissions by Team \nfor Lowest Census Version', ylab='', xlab='')
  lines(compare.df[compare.df$Run==run,'simB_Arrival_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simB_Arrival_Team3'], col='red')
  title(ylab="Number of Patients Admitted", line=2.2)
  title(xlab='Week', line=2.4)
  legend(37,50.5,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'), cex=1.1)
}
arrival.pres.plot(1)

census.pres.plot <- function(run){
  par(cex.main=1.5, cex.lab=1.5)
  
  plot(compare.df[compare.df$Run==run,'simA_Census_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,40), main='Weekly Average Census by Team \nfor Reference Version', ylab='', xlab='')
  lines(compare.df[compare.df$Run==run,'simA_Census_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simA_Census_Team3'], col='red')
  title(ylab="Average Morning Census", line=2.2)
  title(xlab='Week', line=2.4)
  legend(37,40.4,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'), cex=1.1)
  
  plot(compare.df[compare.df$Run==run,'simB_Census_Team1'], type='l', col='blue', xlim=c(0,50), ylim=c(10,40), main='Weekly Average Census by Team \nfor Lowest Census Version', ylab='', xlab='')
  lines(compare.df[compare.df$Run==run,'simB_Census_Team2'], col='chartreuse4')
  lines(compare.df[compare.df$Run==run,'simB_Census_Team3'], col='red')
  title(ylab="Average Morning Census", line=2.2)
  title(xlab='Week', line=2.4)
  legend(37,40.4,c('Team A', 'Team B', 'Team C'), lty=c(1,1,1), col=c('blue', 'chartreuse4', 'red'), cex=1.1)
}
census.pres.plot(1)


z = 1.729 # 95% t-dist
for (i in 1:2){
  print(mean(output3.df[,i]))
  print(mean(output3.df[,i]) + c(-z, z)*sd(output3.df[,i])/sqrt(length(output3.df)))
}

one.samp.ttest <- function(xbar, s, n, mu0){
  t <- abs(xbar - mu0) / (s/sqrt(n))
  return(2*pt(t, n-1, lower.tail=F))
}
one.samp.ttest(3.13, 0.41, 20, 94.7)

two.samp.ttest <- function(xbar1, s1, n1, xbar2, s2, n2){
  t <- abs(xbar1 - xbar2) / sqrt((s1^2 / n1) + (s2^2 / n2))
  df <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
  return(2*pt(t, df, lower.tail=F))
}
two.samp.ttest(3.06, 0.14, 20, 1.47, 0.031, 20)

