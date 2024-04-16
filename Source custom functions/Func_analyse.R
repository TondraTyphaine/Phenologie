
####################################################################################################################### 
############################### REGUL TIME SERIE FOR ONE IND ##########################################################
####################################################################################################################### 

# function to regularise time series, 
# for a single individual but several RGB variables 

Regul_timeserie <- function(Data, # a data.table with only one individual
                            Var, # variables that we are interested in
                            TypeFill) # how do we want to fill ?
  # linear : linear interpolation between observation
  # lastobs : fill the na with the last previous observed value
{
  # make a data.frame with all the dates
  DataFilled <- data.table(date =   seq.Date(from=Data[, min(date)],
                                             to=Data[, max(date)],
                                             by="day"),
                           CrownID = unique(Data$CrownID),
                           Genus_Spec = unique(Data$Genus_Spec))
  # fill it in with gapfilled time series for each variable
  for (j in 1:length(Var)) {
    DataVar <- Data
    colnames(DataVar)[which(colnames(DataVar)==Var[j])] <- "Var"
    # run only if the time series for the considered variable is not all na
    if (sum(!is.na(DataVar[, Var])) != 0) {
      # check if there are duplicated dates and deal with it
      if (any(DataVar[, .N, by="date"]$N >1)) {
        warning(paste("Some of the dates for the variable", Var, "were duplicated. Values of for these dates have been averaged.", sep=" ") )
        # get duplicated dates
        count = DataVar[, .N, by="date"] 
        dateDupl <- count[N>1, date]
          # for each duplicated date
          for (dd in dateDupl) {
            # get the mean
            meanValDupl <- mean(DataVar[date==dd, Var])
            # keep only the first line
            Row4repl <- DataVar[date==dd,][1,]
            # replace by mean
            Row4repl$Var <- meanValDupl
            # Remove the line of that date
            DataVar <- DataVar[date!=dd]
            # add the unique row
            DataVar <- rbind(DataVar, Row4repl)
          }
      }
      
      # if the variable is a factor, transform it to a character
      if (is.factor(DataVar$Var)) {
        # transform to character
        DataVar$Var <- as.character(DataVar$Var)
        Char2fact <- TRUE
      } else {
        Char2fact <- FALSE
      }
      
      # transform in zoo object
      TS <- zoo(DataVar[, Var] ,
                DataVar[, date])
      # make it a regular time series
      TSreg <- as.ts(as.zooreg(TS, frequency = 1))
      
      # do gapfilling
      if(TypeFill == "linear") {
        # fill in the NA values by linear interpolation
        TSfull <- na.approx(TSreg)
      }
      
      # or fill all NA with the last observed value
      if(TypeFill == "lastobs") {
        # fill in the NA values by linear interpolation --> na_locf
        TSfull <- na.locf0(TSreg)
      }
      # make it a data.table
      TSfilled <- data.table(date=as.Date(time(TSfull)[1:length(time(TSfull))]),
                             unfill=as.vector(TSreg)[1: length(as.vector(TSfull))], # * see below
                             filled=as.vector(TSfull))
      # * here I do this for cases when the the end of the time serie is missing for one of the variable but not all
      # length(as.Date(time(TSfull)[1:length(time(TSfull))])) # is the length for the considered variable
      # length(as.vector(TSreg)) # => is the length of the dataset
      # length(as.vector(TSfull)) # => is the length for the considered variable as cannot do linear interpolation if no end value
      # => so I need to reduce the length of TSreg to the length of TSfull
      
      # if the variable was a factor, transform it back to factor
      if (Char2fact == TRUE) {
        TSfilled$unfill <- as.factor(TSfilled$unfill)
        TSfilled$filled <- as.factor(TSfilled$filled)
      }
      
      # rename the colum
      colnames(TSfilled) <- c("date",
                              paste(Var[j], "unfill", sep="_"),
                              paste(Var[j], "filled", sep="_"))
      # # add these column to the main data set
      DataFilled <- merge(DataFilled, TSfilled, by="date", all = TRUE)
      # if there is a problem here, see if I just get all.x or all.y instead of all
      
    } else {
      # add two columns with NA to DataFilled
      DataFilled <- cbind(DataFilled, as.numeric(NA), as.numeric(NA))
      # and rename them
      colnames(DataFilled)[c(length(colnames(DataFilled))-1, length(colnames(DataFilled)))] <- 
        c(paste(Var[j], "unfill", sep="_"), paste(Var[j], "filled", sep="_"))
    }
  }
  DataFilled
}


####################################################################################################################### 
############################### REGUL TIME SERIE FOR EVERY IND ########################################################
####################################################################################################################### 

# function to regularise time series, 
# for a several individuals and several RGB variables 
Regul_ts_all <- function(DataAll, # a data.table of observed greeness for several indiv (possibly of different species)
                         Var, # variables that we are interested in,
                         col2keep = NULL, # additional columns to keep
                         TypeFill) # how do we want to fill ? for categorical phases must be lastobs
  # linear : linear interpolation between observation
  # lastobs : fill the na with the last previous observed value
{
  # create a list of as many elements as the number of indiv to store individual DataFilled
  DataFilledL <- vector("list", length(DataAll[, unique(CrownID)]))
  # Apply Regul_timeserie for each individual and store the results (a data.table in the list)
  for (i in 1:length(DataAll[, unique(CrownID)])) {
    DataFilledL[[i]] <- Regul_timeserie(DataAll[CrownID == DataAll[, unique(CrownID)][i]],
                                        Var = Var, TypeFill = TypeFill)
  }
  # transform the list to a data.table
  DataFilledAll <- rbindlist(DataFilledL, use.names = TRUE)
  
  # add the column we want to keep
  if (!(is.null(col2keep))) {
    DataFilledAll <- merge(DataFilledAll, 
                           unique(DataAll[, c("CrownID", col2keep), with=FALSE]),
                           by="CrownID",
                           all.x=TRUE)
  }

  
  return(DataFilledAll)
}


####################################################################################################################### 
############################### AUTOCORELLATION FOR 1 IND  ############################################################ 
####################################################################################################################### 

# function to do autocorrelation on a given indiv

Autocor <- function (Data, # a data.table with only one individual
                     VarRGB, # one RGB variable that we are interested in
                     LagMax, # max time lag on which to do autocorrelation
                     Plot=TRUE) # do we want the plot of result?
  {
  # transform in zoo object to do the checks
  TS <- zoo(Data[, ..VarRGB] , Data[, date])
  # check that the time serie is regular
  if (!(zoo::is.regular(TS, strict = TRUE))) {
    stop("Function Autocor: you must provide a regular time serie")
  }
  # check is there is NA
  # these can happen in case this time series is shorter than the other ones in the data frame
  if (
    sum(is.na(Data[,which(colnames(Data)==VarRGB), with=FALSE])) >0
  ) {
    warning("Function Autocor: you have missing values in the time series, they will be ignored. Check that you are happpy with this.")
  }
  
  # perform autocorrelation
  ResACF <- acf(Data[, ..VarRGB], lag.max = LagMax, plot = FALSE, na.action = na.pass)
  # retreve the result in a data.table
  ResACFdt <- data.table(Lag=as.vector(ResACF$lag),
                         Autocorrel=as.vector(ResACF$acf))
  # get the significance level
  # https://www.squaregoldfish.co.uk/programming/r_acf_significance.md/
  signif <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(Data[, ..VarRGB])))
  # get the maximum significant value past the first group of significant ones
  # add significance to all lag
  ResACFdt$Signif <- as.logical(NA)
  ResACFdt[Autocorrel > signif | Autocorrel < - signif,
           Signif:=TRUE]
  ResACFdt[!(Autocorrel > signif | Autocorrel < - signif),
           Signif:=FALSE]
  # to test but not nice
  # ggplot(data=ResACFdt, aes(x=Lag, y=Autocorrel)) +
  #   geom_line(aes(col=Signif), na.rm = TRUE) +
  #   geom_hline(yintercept = signif, col="blue", linetype="dashed") +
  #   geom_hline(yintercept = - signif, col="blue", linetype="dashed")
   # get the first non significant value
  firstNotSignif <- ResACFdt[Signif==FALSE, min(Lag)]
  
  # get the last significant value after the first peak
  lastSignif <- ResACFdt[Lag > firstNotSignif & Signif==TRUE & Autocorrel > 0,]
  
  # We choose the first value where there is a jump of more then one day of positive value if there is many peaks
  # after the first non singificant value --> means a negative value
  if(any(diff(lastSignif$Lag) >1 )){ # if there is a jump of Lag
  lastSignif_pos <- which(diff(lastSignif$Lag) >1)[1] # We extract the Position of Lag jump
  lastSignif_Lag <- lastSignif$Lag[lastSignif_pos] # We then extract the right Lag -> end of significant 
  } else
  lastSignif_Lag <- ResACFdt[Lag > firstNotSignif & Signif==TRUE & Autocorrel > 0, max(Lag)]
  
  # get the lag at which max of significant after that (every peaks are taken into account)
  MaxAC <- ResACFdt[Autocorrel==ResACFdt[Lag > firstNotSignif & Lag <= lastSignif_Lag &
                                           Signif==TRUE & Autocorrel > 0, max(Autocorrel)], Lag]

  # if length(MaxAC) == 0, it means that there is no autocorrelation past the first group
  if(length(MaxAC) == 0) {
    Regular <- FALSE
    CycleLenght <- NA

  } else { # otherwise, there is a regular pattern
    # if check for case when there could be several values of the max => stop and examine the case
    if (length(MaxAC) >1) {
      stop("function Autocor: there are several lag for which autocorrel is max => check this case")}
    Regular <- TRUE
    CycleLenght <- MaxAC
  }
  
  # make a graph if Plot==TRUE
  if (Plot==TRUE) {
    PlotACF <- ggplot(data=ResACFdt, aes(x=Lag, y=Autocorrel)) + 
      geom_line() + 
      geom_hline(yintercept = signif, col="blue", linetype="dashed") + 
      geom_hline(yintercept = - signif, col="blue", linetype="dashed") + 
      theme_minimal() +
      ggtitle(paste(unique(Data$CrownID), "- ",
                    unique(Data$Genus_Spec), ", ",
                    "Cycle length = ", CycleLenght, sep = "")) + 
      ylab(paste("Autocorrelation for ", VarRGB)) +
      if (!is.na(CycleLenght)){
      # geom_vline(xintercept = firstNotSignif, col="red") +
      geom_vline(xintercept = CycleLenght, col="red", linetype="dashed", linewidth=1)
      } 
  }

  # store results in a list
  if (Plot==TRUE) {
  resultACF <- list(Regular=Regular, 
                    CycleLenght=CycleLenght, 
                    PlotACF=PlotACF)   
  } else {
  resultACF <- list(Regular=Regular, 
                    CycleLenght=CycleLenght)
  }
  return(resultACF)
  
}

####################################################################################################################### 
############################### AUTOCORRELATION FOR EVERY IND ######################################################### 
####################################################################################################################### 

# function to do autocorrelation on sevral indiv 

Autocor_all <- function(DataAll,   # a regular (daily) data.table of observed greeness for several indiv
                        VarRGB, # RGB variables that we are interested in, filled
                        LagMax) # max time lag on which to do autocorrelation
{
  # create a list to store individual results
  ResACFL <- vector("list")
  # For each indiv
  for (i in DataAll[, unique(CrownID)]) {
    # create a list to store individual results
    ResIndiv <- vector("list")
    #  for each VarRGB
    for (v in VarRGB) {
      # if the time series is not all NA
      if(sum(!is.na(DataAll[CrownID == i,which(colnames(DataAll)==v), with=FALSE])) != 0) {
        # perform ACF (without making the plot)
        resACF <- suppressWarnings(Autocor(Data = DataAll[CrownID == i],
                                           VarRGB = v,
                                           LagMax=LagMax, 
                                           Plot=FALSE))
        # store the results
        ResIndiv[[v]] <- data.table(CrownID = i,
                                    Genus_Spec = DataAll[CrownID == i, unique(Genus_Spec)],
                                    varRGB = gsub('_filled','',v),
                                    Regular = as.character(resACF$Regular),
                                    CycleLenght = as.numeric(resACF$CycleLenght))
      } else {
        ResIndiv[[v]] <- data.table(CrownID = i,
                                    Genus_Spec = DataAll[CrownID == i, unique(Genus_Spec)],
                                    varRGB = gsub('_filled','',v),
                                    Regular = as.character("NoData"),
                                    CycleLenght = as.numeric(NA))
      }
    }
    ResACFL[[i]] <- rbindlist(ResIndiv, use.names = TRUE)
  }
  # transform the list to a data.table
  ResACFAll <- rbindlist(ResACFL, use.names = TRUE)
  # # rename the colums
  # colnam <- c("CrownID" , "Genus_Spec")
  # for (v in VarRGB) {
  #   vnam <- gsub('_filled','',v)
  #   colnam <- c(colnam, paste("Reg", vnam, sep="_"), paste("CyclLen", vnam, sep="_"))
  # } 
  # colnames(ResACFAll) <- colnam
  # 
  return(ResACFAll)
}


####################################################################################################################### 
############################### CROSS CORRELATION FOR 2 INDS ########################################################## 
####################################################################################################################### 

# function to do crosscorrelation for two given time series

Crosscor <- function (ts1, # first time serie on the same dates
                      ts2, # second time serie on the same dates
                      Date, # a vector with the dates of both time series
                      LagMax, # max time lag on which to do autocorrelation 
                      titlegraph=NULL, # titre du graph 
                      VarRGB, # name of the considersed variable
                      Plot=TRUE) # do we want the plot of result?

{
  # check that both time serie is regular
  if (any(c(!(zoo::is.regular(zoo(ts1 ,Date))),  !(zoo::is.regular(zoo(ts1 ,Date)))))) {
    stop("Function TimingCC: you must provide a regular time serie")
  }
  # check is there is NA in any of the two time series
  if (any (c(is.na(ts1), is.na(ts1))))  {
    warning("Function Autocor: you have missing values in the time series, they will be ignored. Check that you are happpy with this.")
  } 
    
  # perform cross-correlation
  # see help function: The lag k value returned by ccf(x, y) estimates the correlation between x[t+k] and y[t].
  ResCCF <- ccf(x= ts1, 
                y= ts2, 
                lag.max = LagMax, plot = FALSE, na.action = na.pass)
  # retrieve the result in a data.table
  ResCCFdt <- data.table(Lag=as.vector(ResCCF$lag),
                         Autocorrel=as.vector(ResCCF$acf))
  # get the significance level
  # https://www.squaregoldfish.co.uk/programming/r_acf_significance.md/
  # https://stats.stackexchange.com/questions/3115/cross-correlation-significance-in-r
  signif <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(ts1))) 
  
  # get the lag with maximum positive cross-correlation, past the past the first group of significant ones 
  # attention, it can be a negative one
  # add significance to all lag
  ResCCFdt$Signif <- as.logical(NA)
  ResCCFdt[Autocorrel > signif | Autocorrel < - signif,
           Signif:=TRUE]
  ResCCFdt[!(Autocorrel > signif | Autocorrel < - signif),
           Signif:=FALSE]
  # # to test but not nice
  # Test <- ggplot(data=ResCCFdt, aes(x=Lag, y=Autocorrel)) +
  #   geom_line(aes(col=Signif), na.rm = TRUE) +
  #   geom_hline(yintercept = signif, col="blue", linetype="dashed") +
  #   geom_hline(yintercept = - signif, col="blue", linetype="dashed")
  # get the lag with positive cross-correlation, for positive lag-values
  ResCCFdtPos <- ResCCFdt[Lag>=0]
  # get the lag at which max of significant
  MaxCCPos <- ResCCFdtPos[Autocorrel==ResCCFdtPos[Signif==TRUE, max(Autocorrel)], Lag]
  #### OLD VERSION TO REMOVE WHEN ALL OK
    # #  get the first non significant value
    # firstNotSignif <- ResCCFdtPos[Signif==FALSE, min (Lag)]
    # # get the lag at which max of significant after that after that
    # MaxCCPos <- ResCCFdtPos[Autocorrel==ResCCFdtPos[Lag > firstNotSignif & 
    #                                                   Signif==TRUE, max(Autocorrel)], Lag]
  # do the symetrie for negative values
  ResCCFdtNeg <- ResCCFdt[Lag<0]
  # get the lag at which max of significant
  MaxCCPNeg <- ResCCFdtNeg[Autocorrel==ResCCFdtNeg[Signif==TRUE, max(Autocorrel)], Lag]
  #### OLD VERSION TO REMOVE WHEN ALL OK
    # #  get the last non significant value
    # lastNotSignif <- ResCCFdtNeg[Signif==FALSE, max(Lag)]
    # # get the lag at which max of significant before that
    # MaxCCPNeg <- ResCCFdtNeg[Autocorrel==ResCCFdtNeg[Lag < lastNotSignif & 
    #                                                    Signif==TRUE, max(Autocorrel)], Lag]
  # merge both
  MaxCC <- c(MaxCCPos, MaxCCPNeg)
  
  # if length(MaxCC) == 0, it means that there is no cross-correlation past the first group
  if(length(MaxCC) == 0) {
    CrossCorel <- FALSE
    PhaseDiff <- NA
  } else { # otherwise, there is a cross-correlation
    # check for case when there could be several values of the max => stop and examine the case
    if (length(MaxCC) > 2 ) {
      stop("function TimingCC: there are several lag for which autocorrel is max => check this case")}
    CrossCorel <- TRUE
    # get the one in for positive or negative lag that is higher
    PhaseDiff <- ResCCFdt[Lag %in% MaxCC & Autocorrel == ResCCFdt[Lag %in% MaxCC, max(Autocorrel)] ,Lag]
  }
  
  # make a graph  if Plot==TRUE
  if (Plot==TRUE) {
  PlotCCF <- ggplot(data=ResCCFdt, aes(x=Lag, y=Autocorrel)) + 
    geom_line() + 
    geom_hline(yintercept = signif, col="blue", linetype="dashed") + 
    geom_hline(yintercept = - signif, col="blue", linetype="dashed") + 
    theme_minimal() + 
    # geom_vline(xintercept = firstNotSignif, col="red") +
    geom_vline(xintercept = PhaseDiff, col="red", linetype="dashed", size=1) + 
    ggtitle(paste(titlegraph, ", PhaseDiff = ", PhaseDiff, sep = "")) + 
    ylab(paste("Autocorrelation for ", VarRGB))
  }
  
  # store the results
  if (Plot==TRUE) {
    resultCCF <- list(CrossCorel=CrossCorel, # is there a cross-correlation
                    PhaseDiff=PhaseDiff, # phase difference
                    PlotCCF=PlotCCF)    # graph of cross-correlation
  } else {
    resultCCF <- list(CrossCorel=CrossCorel, # is there a cross-correlation
                      PhaseDiff=PhaseDiff) # phase difference
  }
  return(resultCCF)
}


####################################################################################################################### 
############################### MAKE SINUSOID #########################################################################
####################################################################################################################### 

# Function make sinusoid mimicking an observed regular and daily time series
# make it twice as long as the observed data, centrered on the observed data
Make_sinusoid <- function(CycleLength, # cycle length in days 
                          Data, # DataFilled
                          VarRGB)# one RGB variable that we are interested in
{
  # time serie
    serie <- Data[, which(colnames(Data)==VarRGB), with=FALSE]
  # wavelength in radians
    w <- 2*pi*(1/CycleLength)
  # amplitude of the signal
    Amplitude <- (max(serie, na.rm = TRUE) - min(serie,na.rm = TRUE))/2
  # "centre" of the sinusoid
    Centre <- max(serie, na.rm = TRUE) - Amplitude
  # get a length f the time series twice as long
    newdate <- as.Date((Data$date[1] - round(length(DataFilled$date)/2)) : 
      (Data$date[length(Data$date)] + round(length(DataFilled$date)/2)))
  # make a sinusoid with this wavelength, peaking at 
    sinusoidTS <- zoo(Centre + Amplitude*cos(as.numeric(w)*1:length(newdate)), newdate)
    return(sinusoidTS) 
}


####################################################################################################################### 
############################### TIMING FOR ONE IND ####################################################################
####################################################################################################################### 

# # Function to get timing of peak using cross-correlation between an observed (daily-gapfilled time serie) and simultaed sinusoid
TimingCC <- function (CycleLength, # cycle length in days
                      Data, # a data.table with only one individual
                      VarRGB, # one RGB variable that we are interested in
                      LagMax, # max time lag on which to do autocorrelation
                      Plot=TRUE) # do we want the plots of result?
{
  # transform the observed data in zoo object to do the checks
  TS <- zoo(Data[, ..VarRGB] , Data[, date])
  # check that the time serie is regular
  if (!(zoo::is.regular(TS))) {
    stop("Function TimingCC: you must provide a regular time serie")
  }
  # check is there is NA
  # these can happen in case this time series is shorter than the other ones in the data frame
  if (
    sum(is.na(Data[,which(colnames(Data)==VarRGB), with=FALSE])) >0
  ) {
    warning("Function TimingCC: you have missing values in the time series, they will be ignored. Check that you are happpy with this.")
  }

  # create a sinusoid mimicking the time series of interest
    sinusoidTS <- Make_sinusoid(CycleLength, Data, VarRGB)
    # make it a data frame for later
    Sinusoide_dt <- data.table(date= time(sinusoidTS), simVal = as.vector(sinusoidTS))


  # run function of cross-correlation
    if (Plot==TRUE) {
      resultCCF <- Crosscor(ts1 = as_vector(Data[, ..VarRGB]),
                            ts2=  as_vector(Sinusoide_dt$simVal),
                            Date = as.vector(Data[, date]),
                            LagMax = LagMax,
                            titlegraph = paste(unique(Data$CrownID), "- ", unique(Data$Genus_Spec)),
                            VarRGB = VarRGB)
      
      CrossCorel <- resultCCF$CrossCorel # is there a cross-correlation
      PhaseDiff <- resultCCF$PhaseDiff # phase difference
      PlotCCF <- resultCCF$PlotCCF # graph of cross-correlation
    } else {
      resultCCF <- Crosscor(ts1 = as_vector(Data[, ..VarRGB]),
                            ts2=  as_vector(Sinusoide_dt$simVal),
                            Date = as.vector(Data[, date]),
                            LagMax = LagMax,
                            VarRGB = VarRGB,
                            Plot=FALSE)
      
      CrossCorel <- resultCCF$CrossCorel # is there a cross-correlation
      PhaseDiff <- resultCCF$PhaseDiff # phase difference
    }


  # predict dates of peak
      # get the date of one peak of the sinusoide
      PeakSin <- Sinusoide_dt[simVal==Sinusoide_dt[, max(simVal)], date][1]
      # get a first date of peak
        DatePeakref <- PeakSin + PhaseDiff
        # if the first date of peak calculated is outside of the observation period
        # take the next one until it is not anymore
        x=1
        while(!(DatePeakref %in% Data$date) & 
              ((x+1) <= length(Sinusoide_dt[simVal==Sinusoide_dt[, max(simVal)], date]))) {
                PeakSin <- Sinusoide_dt[simVal==Sinusoide_dt[, max(simVal)], date][x+1]
                # get a first date of peak
                DatePeakref <- PeakSin + PhaseDiff
                # increament x
                x <- x +1
              }
        # if (!(DatePeakref %in% Data$date)) {
        #   # try to get the next peak of the sinusoid 
        #   PeakSin <- Sinusoide_dt[simVal==Sinusoide_dt[, max(simVal)], date][2]
        #   # get a first date of peak
        #   DatePeakref <- PeakSin + PhaseDiff
          # test again of out of observation period
          if (!(DatePeakref %in% Data$date)) {
          stop("function TimingCC: the first date of peak calculated is outside of the observation period")
          }
        #}
        DatePeak <- DatePeakref
        # add the CycleLength for as long as they are in the period of obs
        while (all(DatePeak %in% Data$date)) {
          DatePeak <- c(DatePeak, DatePeak[length(DatePeak)] + as.numeric(CycleLength))
        }
          # this loop puts one more so we remove it
          DatePeak <- DatePeak[which(DatePeak %in% Data$date)]
        # then do the same by substacting
        while (all(DatePeak %in% Data$date)) {
          DatePeak <- c(DatePeak[1] -  as.numeric(CycleLength), DatePeak)
        }
          # this loop puts one more so we remove it
          DatePeak <- DatePeak[which(DatePeak %in% Data$date)]

  # graph of results
    if (Plot==TRUE) {
      # make a dataframe for plotting the obs
      DataObs <- data.table(date=Data$date, VarRGB=Data[,..VarRGB])    
      colnames(DataObs) <- c("date", "VarRGB")
      
      PlotTimingCC <- ggplot(data=Sinusoide_dt, aes(x=date, y=simVal)) + 
        geom_line(linetype="dashed") + theme_minimal() +
        geom_line(data=DataObs, aes(x = date, y=VarRGB), col="red", size=1)  +
        ggtitle(paste(unique(Data$CrownID), "- ", unique(Data$Genus_Spec), ", PhaseDiff = ", PhaseDiff, sep = "")) + 
        ylab(VarRGB)
      for (i in DatePeak) {
        PlotTimingCC <- PlotTimingCC +  geom_vline(xintercept = i, col="blue", linetype="dashed", size=1)  
      }
    }
      
  # store the results
    if (Plot==TRUE) { 
      result <- list(CrossCorel=CrossCorel, # is there a cross-correlation
                     PhaseDiff=PhaseDiff, # phase difference
                     DatePeak= DatePeak, # predicted dates of peak
                     PlotCCF=PlotCCF, # graph of cross-correlation
                     PlotTimingCC = PlotTimingCC) # plot of timing (ref the obs, blue the predicted dates and black the simulated sinusoid)     
    } else {
      result <- list(CrossCorel=CrossCorel, # is there a cross-correlation
                     PhaseDiff=PhaseDiff, # phase difference
                     DatePeak= DatePeak) # predicted dates of peak
    }
    return(result)
}


####################################################################################################################### 
############################### TIMING FOR EVERY IND ##################################################################
####################################################################################################################### 

# Run TimingCC for several individuals in one go
TimingCC_all <- function(ResACFAll, # all the results of the ACF for the considered individuals
                         DataAll,   # a regular (daily) data.table of observed greeness for several indiv
                         VarRGB, # RGB variables that we are interested in, filled
                         LagMax) # max time lag on which to do autocorrelation 
{
  # create a list to store individual results
  ResCCFL <- vector("list")
  # For each indiv
  for (i in DataAll[, unique(CrownID)]) {
    # i = DataAll[, unique(CrownID)][14] # TO DEBUG
    # create a list to store individual results
    ResIndiv <- vector("list")
    
    #  for each VarRGB
    for (v in VarRGB) {
      # v = VarRGB[2] # TO DEBUG
      # if the individual is regular for this variable
      if (ResACFAll[CrownID == i & varRGB==as.factor(v), Regular] == "TRUE") {
        # run the TimingCC function
        resCCF <- suppressWarnings(TimingCC(CycleLength = ResACFAll[CrownID == i & varRGB==as.factor(v), CycleLenght],
                                            Data = DataAll[CrownID == i],
                                            VarRGB = paste(v, "filled", sep="_"),
                                            LagMax = LagMax, 
                                            Plot =FALSE))
        # store the results
        ResIndiv[[v]] <- data.table(CrownID = i, 
                                    Genus_Spec = DataAll[CrownID == i, unique(Genus_Spec)],
                                    varRGB = gsub('_filled','',v),
                                    CrossCorel = as.character(resCCF$CrossCorel),
                                    PhaseDiff = as.numeric(resCCF$PhaseDiff),
                                    DatePeak = as.Date(resCCF$DatePeak))
      }
      # if the individual is not regular 
      if (ResACFAll[CrownID == i & varRGB==as.factor(v), Regular] == "FALSE") {
        ResIndiv[[v]] <- data.table(CrownID = i, 
                                    Genus_Spec = DataAll[CrownID == i, unique(Genus_Spec)],
                                    varRGB = gsub('_filled','',v),
                                    CrossCorel = as.character(NA),
                                    PhaseDiff = as.numeric(NA),
                                    DatePeak = as.Date(NA))
      }
      # if there is no data
      if (ResACFAll[CrownID == i & varRGB==as.factor(v), Regular] == "NoData") {
        ResIndiv[[v]] <- data.table(CrownID = i, 
                                    Genus_Spec = DataAll[CrownID == i, unique(Genus_Spec)],
                                    varRGB = gsub('_filled','',v),
                                    CrossCorel = as.character("NoData"),
                                    PhaseDiff = as.numeric(NA),
                                    DatePeak = as.Date(NA))
      }
    }
    ResCCFL[[i]] <- rbindlist(ResIndiv, use.names = TRUE)
  }
  # transform the list to a data.table
  ResCCF <- rbindlist(ResCCFL, use.names = TRUE)
  
  return(ResCCF)
}

####################################################################################################################### 
############################### CROSS CORRELATION SYNCHRO #############################################################
####################################################################################################################### 

# get cross-correlation between each pairs of indiv, for several VarRGB in one go

Synchro_CC <- function(DataAll,   # a dataframe with regularised (gapfilled) values, for all indiv of the considered species (long table)
                       VarRGB, # RGB variables that we are interested in, filled
                       LagMax) # max time lag on which to do autocorrelation
{
  # check that the length of the date vectors is the same for all indiv
  if (DataAll[, length(unique(date))] !=
      dim(DataAll)[1] / DataAll[, length(unique(CrownID))]) {
    stop("Function Synchro_CC: all individuals don't have the same dates)")}
  
  # make a table with all possible pairs of indic, to store the results
  Allpairs <- data.table(Ind1 = factor(),
                         Ind2 = factor())
  uniqID <- DataAll[, unique(CrownID)]
  for (i in uniqID[1: (length(uniqID)-1)]) {
    temppair <- CJ(i, uniqID[(which(uniqID==i)+1) : length(uniqID)])
    Allpairs <- rbind(Allpairs, temppair, use.names=FALSE)
  }
  # test nb row
  # (length(uniqID)*(length(uniqID)-1)/2) ==dim(res)[1]
  
  # caclculate cross-cor for all pairs and all VarRGB
  # create a list to store res for all pairs 
  ResAllL <- vector("list")
  # for each pairs
  for (i in 1: dim(Allpairs)[1]) {
    # create a list to store result all varRGB or a given pair
    ResPair <- vector("list")
    # for each VarRGB
    for (v in VarRGB) {
      ts1 <- (DataAll[CrownID==Allpairs[i, Ind1], 
                     which(colnames(DataAll)==v), with=FALSE]) # time serie of first indiv of the pair
      ts2 <- (DataAll[CrownID==Allpairs[i, Ind2], 
                     which(colnames(DataAll)==v), with=FALSE]) # time serie of second indiv of the pair
      
      # if both time series are not all NA  
      if (sum(!is.na(ts1))!=0 &
          sum(!is.na(ts2))!=0) {
        # run the CrossCor function between the two times series
        resCC <- suppressWarnings(Crosscor(ts1 = as_vector(ts1), 
                                           ts2 = as_vector(ts2), 
                                           Date = DataAll[, unique(date)], 
                                           LagMax = LagMax, 
                                           VarRGB = v, 
                                           Plot=TRUE)) 
        # store the results
        ResPair[[v]] <- data.table(Allpairs[i],
                                   varRGB = gsub('_filled','',v),
                                   CrossCorel = as.character(resCC$CrossCorel),
                                   PhaseDiff = as.numeric(resCC$PhaseDiff))
      } else { # if one or both have no data
        ResPair[[v]] <- data.table(Allpairs[i],
                                   varRGB = gsub('_filled','',v),
                                   CrossCorel = as.character("NoData"),
                                   PhaseDiff = as.numeric(NA))
      }
    }
    ResAllL[[i]] <- rbindlist(ResPair, use.names = TRUE) 
  } 
  # transform the list to a data.table
  ResAll <- rbindlist(ResAllL, use.names = TRUE)
  
  return(ResAll)
}


####################################################################################################################### 
############################### PROPORTION OF INDIVIDUALS IN A GIVEN PHASES ###########################################
####################################################################################################################### 

  LeafedOTim <- function(Data, Spec, Obs_Veg = "PPVeg", Pattern = "L", breaks = "2 months"){
    
    
    # First filter the wanted species Obs
    Data = Data %>% as_tibble()  # Convert Data to a tibble format
    Data = Data %>% filter(Genus_Spec %in% Spec) %>% dplyr::select(!!sym(Obs_Veg), Genus_Spec, CrownID, date) %>% distinct()  # Filter Data based on the specified species and select certain columns
    
    # Re-factorize the Genus_Spec column to include only the specified levels
    Data$Genus_Spec <- factor(Data$Genus_Spec, levels = Spec)  # Re-factorize Genus_Spec column based on the specified levels
    
    if (Obs_Veg != "PPFlo") {
      ## Check if every individual has an event
      insufind = NULL  # Initialize insufind variable
      for (i in unique(Data$CrownID)) {  # Loop through unique CrownIDs
        ## Check for every individual
        if (Data %>% filter(CrownID == i) %>% dplyr::select(all_of(Obs_Veg)) %>% unique() %>% nrow() <= 2 &  # Check for the number of unique observations
            Data %>% filter(CrownID == i) %>% dplyr::select(all_of(Obs_Veg)) %>% unique() %>% slice_head(n = 1) %>% pull() %in% c("no_obs", NA)) {
          # if there is no observation / or only NA / or only one value
          insufind = c(insufind, i)  # Store the CrownIDs which don't have enough data
        }
      }
      
      Data = Data %>% filter(!CrownID %in% insufind)  # Filter out the CrownID's that don't have enough data
    }
    
    ## Make a data frame for the analysis of proportion
    Prop = Data %>%
      pivot_longer(cols = all_of(Obs_Veg), names_to = "State")  # Reshape the data to long format for analysis 
    
    ## Spotting the date where there is only no_obs
    emptydate = Prop %>% dplyr::select(date, value) %>% group_by(date, value) %>% summarise(tot_value = n()) %>% ungroup()  # Identify dates with only "no_obs"
    emptydate = emptydate %>% filter(tot_value == length(unique(Prop$CrownID)) & value == "no_obs") %>% dplyr::select(date) %>% pull()  # Extract dates with only "no_obs"
    
    if (Obs_Veg != "PPFlo") {
      Prop = Prop %>% filter(value != "no_obs") %>% distinct()  # Filter out "no_obs" values
    }
    
    Prop = Prop %>%
      dplyr:::group_by(Genus_Spec, date, State, value, .drop = FALSE) %>%   # Group by specified columns
      dplyr:::summarise(n = n_distinct(CrownID)) %>%   # Summarize the number of individuals
      dplyr:::mutate(ngr = sum(n)) %>%   # Calculate the total number of individuals
      dplyr:::mutate(prop = n / ngr * 100) %>%   # Calculate the proportion
      ungroup()   # Remove grouping
    
    # We keep only the value in the wanted Pattern
    Prop = Prop %>% filter(value %in% c(Pattern), !(date %in% emptydate))   # Filter based on Pattern and empty dates
    
    # If we work on PPFlo, according to the possible wrong date we exclude them and keep only the date where all the ind are observed
    if (Obs_Veg =="PPFlo"){
      Prop = Prop %>% filter(ngr == Data$CrownID %>% n_distinct())
    }
    
    ## Making the graph into a signal
    Graph = ggplot(Prop, aes(x = date, y = prop)) +   # Create a ggplot object
      geom_line(aes(group = value, col = value), stat = "identity") +   # Add line plot
      theme(axis.text.x = element_text(angle = 90)) +   # Adjust x-axis text angle
      scale_x_date(date_breaks = breaks, date_labels = "%b-%Y") +   # Adjust x-axis date breaks and labels
      scale_colour_manual(values = c(   # Adjust color scale
        "F" = "green4",
        "L" = "cyan4",
        "D" = "tomato4",
        "Fl" = "gold2"
      )) +
      labs(title = paste("Proportion of individuals in Phenophase:",Pattern,"over sampling period" ),
           y = "Proportion (%)" ) + # Add y-axis label
      geom_text(   # Add text to the plot
      size    = 3, ## Enable to print the number of ind for each species selected
      data    = Prop %>% select(Genus_Spec,ngr) %>% unique() %>% group_by(Genus_Spec) %>% slice_max(ngr,n=1) ,
      mapping = aes(
        x = as.Date(Inf),
        y = Inf,
        label = paste("n ind = ", ngr, sep = " ")
      ),
      hjust   = 1.05,
      vjust   = 1.1
    ) + facet_grid(cols = vars(Genus_Spec), rows = vars(value))   # Create facets
  
  Exit = list(Prop, Graph)
return(Exit)
}



####################################################################################################################### 
############################### BREAKPOINTS ANALYSIS STRUCCHANGE PACKAGE############################################### 
####################################################################################################################### 

BpMatch <- function(Data,
                   Obs,
                   Index,
                   ID = NULL,
                   Phase = FALSE,
                   Value = "value",
                   Obs_Veg = "PPVeg") {

  ## Strucchange :: breakpoints --> determination of the optimized breakpoints based on the RSS and BIC ==
  ## Residuals Sum of Square and Bayesian Index Criteria
  
  # Filtering the data with the variable and indew we want to use for One individual
  filtered_data <- Data %>% filter(Type_Obs == Obs,index == Index,CrownID == ID) %>% distinct() %>% 
    dplyr::select(date,Value,!!sym(Obs_Veg),everything())

    # The parameter h is minimal segment size either given as fraction relative to the sample size 
    # or as an integer giving the minimal number of observations in each segment.
    
    # Then analyse the signal across time 
    bp = strucchange::breakpoints(na_interpolation(filtered_data %>% pull(Value),option = "linear") ~ filtered_data$date)
    # We have to fill the data for this type of analysis
    if(sum(is.na(filtered_data %>% pull(Value))) > 1){
      message(paste("There is NA in your data, linear interpolation have been computed for",sum(is.na(filtered_data %>% pull(Value))),"NAs"))
    }
    
    # Then plot the Graph 
    GRAPH = ggplot(filtered_data, aes(x = date, y = na_interpolation(filtered_data %>% pull(Value),option = "linear"))) +
      geom_line(col = "black", linewidth = 0.5) + # the signal
      geom_point(col = "black", size = 1.6) + # the signal
      theme_pubclean() + # classic_theme
      scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") + # better visu of dates 
      theme(axis.text.x = element_text(angle = 90)) +
      geom_text(data= filtered_data %>% filter(!!sym(Obs_Veg) != "no_obs"),aes(x=date, y=min(filtered_data %>% pull(Value),na.rm = TRUE), label := !!sym(Obs_Veg)),
                col="black", 
                na.rm=TRUE)# better visu of dates
      
    ## Some details will depend if we have find breakpoint or not  
  
    # If we find brekapoint
    if (!is.na(sum(breakdates(bp)))) {
      GRAPH = GRAPH + geom_vline(
        xintercept = filtered_data$date[bp$breakpoints],
        col = "red3" ,
        linetype = 'dashed'
      ) +
        labs(title = paste("CrownID",ID), x = "Time", y = "Index Value") +
        geom_point(aes(y = fitted(bp)), col = "grey20",size = 0.2) + # Point the different estimate frm the different cut line
        geom_line(aes(y = fitted(bp)), col = "grey") # line of the different estimate frm the different cut line
      
      
      # We compute a new sequence of binary version for comparison with other method (Moving Average) and see the intercorrelation
      seq = rep(0,length(filtered_data$date))  # We use the original data with same length used before the interpolation
      seq[bp$breakpoints] <- 1 # So the position is the same when we refeer to originial dates and we replace by 1 
      
      # We stock this in a tibble
      Simple <- tibble(CrownID = ID, 
                       date = filtered_data$date,
                       Seq = seq)
      
      # For the Exit I want the date estimated, the graph and the binary version for comparison with other method
      EXIT = list(diff(filtered_data$date[bp$breakpoints]),GRAPH,filtered_data$date[bp$breakpoints],Simple)
      
    # If we don't we make a graph with the signal but another title    
    } else {
      GRAPH = GRAPH +
        labs(
          title = paste(ID, "No breakpoint found", sep = "_"),
          x = "Time",
          y = "Index Value"
        )
      
      # the binary version is a repetition of 0 
      seq = rep(0,length(filtered_data$date))
      
      # We still give the final tibble
      Simple <- tibble(CrownID = ID, 
                       date = filtered_data$date,
                       Seq = seq)
      
      # But the Exit dont have cycle or dates spotted by the breakpoints so only Graph and tibble in binary version 
      EXIT = list(NA,GRAPH,NA,Simple)
      
    }

  
  return(EXIT)
  
}



####################################################################################################################### 
############################### HEATMAP FOR PHENOPHASES DATA ##########################################################
####################################################################################################################### 

Leaf_Pattern <- function(Data,Spec,Obs_Veg ="PPVeg",fertility = TRUE){

  # First filter the wanted species
Data = Data %>% as_tibble()

# FIltering by the wanted species and the NA in the original Obs
Data = Data %>% filter(Genus_Spec %in% Spec) %>% arrange(CrownID)# Filtering by the wanted Species 

## We keep only the need value --> And distinct one to extract other repeated value
Data = Data %>% dplyr::select(!!sym(Obs_Veg),CrownID,Genus_Spec,date,PPFlo) %>% distinct(Genus_Spec,CrownID,date,!!sym(Obs_Veg),.keep_all = TRUE)

## Filter every ind which not have 2 samples of the same pattern
insufind <- Data %>%
  group_by(CrownID) %>%
  summarise(n = n_distinct(!!sym(Obs_Veg))) %>%
  filter(n <= 2) %>%
  pull(CrownID)


## We then fill the different Obs to have a pixel per days 
Datafun = Regul_ts_all(DataAll = Data %>% filter(!(CrownID%in%insufind)) %>% as.data.table(),Var = Obs_Veg, TypeFill = "lastobs") %>%
  as_tibble() %>% 
  rename(!!sym(Obs_Veg) := paste(Obs_Veg,"filled",sep = "_")) # We use the filled data for vegetative phenophase

## Make a data frame for the analysis of proportion
Datafun = Datafun %>%
    pivot_longer(cols=all_of(Obs_Veg),names_to = "State") %>% # Make it on factor to "State" Obs 
  left_join(Data %>% select(PPFlo,date,CrownID)) # Reshape the data and left join with selected columns from Data


## For adding the number of individuals 
add_text = Datafun %>%
  dplyr:::group_by(Genus_Spec) %>%
  dplyr:::summarise(n_ind = length(unique(CrownID)))

  ## Making the graph into a HeatMap
Graph = ggplot(Datafun,aes(x = date,y= CrownID)) + ## For each individuals over the different date
  geom_tile(aes(
    fill = value
  )) + 
  labs(title = paste("Phenophases of",Spec)) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  geom_text(size    = 3,
            data    = add_text,
            mapping = aes(x = as.Date(Inf), y = Inf, label = paste("n ind = ",n_ind,sep = " ")),
            hjust   = 1.2,
            vjust   = 0.8,check_overlap = TRUE) + 
  theme(
  axis.text.y = element_text(
    color = "black",
    size = 8
  )
  )+
  scale_fill_manual(values = c("F" = "green4", "L" = "cyan4","D" = "tomato4", "Fl" = "gold2", "no_obs" = "grey52")) 

# How to add the different shape for the fertility 

if(fertility == TRUE){
  
  add_flo = Datafun %>% # We extract the different value of Flowering column
    filter(Genus_Spec %in% Spec) %>% # for the wanted species 
    dplyr::select(CrownID,PPFlo,date,Genus_Spec) %>% 
    filter(!is.na(PPFlo)) %>% # wihthout NA 
    filter(PPFlo %in% c("Fl","Fl?","Fr")) # Just to be sure there is no other notation (check beforehand)
  
  Graph = Graph + geom_point(data= add_flo, aes(shape = PPFlo,col = PPFlo)) + # add some point 
    scale_color_manual(values = c("gold2","brown4","pink3")) + # with different shape 
    scale_shape_manual(values = c(8,6,21)) # yellow star for flowers / brown triangle for uncertain / pink circle when fruits
} 

# Just make a facet grid if we want to see more then one species 

if (length(Spec) > 1 ){
  Graph = Graph +
  facet_grid(rows = vars(Genus_Spec), scales = "free_y")
  }

Exit=list(Datafun,Graph)
  
  return(Exit)
}


####################################################################################################################### 
############################### TIMING IN CIRCULAR GRAPH ##############################################################
####################################################################################################################### 

Leaf_Circular <- function (Data,Spec,Obs_Veg = "PPVeg", Pattern = "F",perYears = TRUE) {

# Convert to tibble
Data = Data %>%  as_tibble()

if(Obs_Veg == "PPFlo"){
  # First at all if we are regarding to the fertility we have to replace all NA by sterility, which is different from no_obs
  Data = Data %>% mutate(PPFlo = ifelse(is.na(PPFlo),"Str",as.character(PPFlo)),
                         PPFlo = ifelse(PPFlo == " ","Str",as.character(PPFlo)))
}

# Filtering by the Species wanted and Na for having the none gapfilled 
Data = Data %>% filter(Genus_Spec %in% Spec) %>% filter(!is.na(!!sym(Obs_Veg))) %>% distinct(Genus_Spec,CrownID,!!sym(Obs_Veg),date,.keep_all = TRUE) %>% arrange(CrownID) 
Data$Genus_Spec <- factor(Data$Genus_Spec, levels = Spec)

## Check if every individuals has an event
insufind = NULL
for (i in unique(Data$CrownID)){ ## Check for every ind
  if (Data %>% filter(CrownID==i) %>% dplyr::select(!!sym(Obs_Veg)) %>% unique() %>% nrow() <= 2 & 
      Data %>% filter(CrownID==i) %>% dplyr::select(!!sym(Obs_Veg)) %>% unique() %>% slice_head(n=1) %>% dplyr::select(!!sym(Obs_Veg)) == "no_obs"){ # if there is no Obs / or only NA / or only one value 
    insufind= c(insufind,i) # Stock the different Index_ind_SP which doesn't have enough data
  }
}


Data = Data %>% filter(!CrownID%in%insufind) 
n_ind = length(unique(Data$CrownID))
## dplyr::selecting all the wanted !!sym(Obs_Veg) (=variable)
Data = Data %>% mutate(byMon = factor(format(date, format = "%b"),levels=c("janv.","févr.","mars","avr.","mai","juin","juil.","août","sept.","oct.","nov.","déc.")),
                       Years = as.factor(year(date)))


## Making the circular graph 
if (perYears == TRUE) {
  Prop = Data %>% 
    ungroup() %>% 
    mutate(!!sym(Obs_Veg) := as.factor(!!sym(Obs_Veg))) %>% 
    dplyr:::group_by(Genus_Spec,Years,byMon,!!sym(Obs_Veg),.drop = FALSE) %>%  # We group in first by species,years and after month and after we compute the number of phenophases event 
    dplyr:::summarise(n = n()) %>%  # and after we compute the number of phenophases event 
    dplyr:::mutate(ngr = sum(n)) %>%  # Then, the number of phenophases per month
    dplyr:::mutate(prop = n/ngr*100) %>% # Then, the proportion of given phenophases
    ungroup() %>% filter(!!sym(Obs_Veg) %in% Pattern) # But just keep the wanted Obs_Veg 
  
  Graph = ggplot(data = Prop, aes(x = byMon, fill = Years, y = prop))
} else {
  Prop = Data %>% 
    ungroup() %>% 
    mutate(!!sym(Obs_Veg) := as.factor(!!sym(Obs_Veg))) %>% 
    dplyr:::group_by(Genus_Spec,byMon,!!sym(Obs_Veg),.drop = FALSE) %>%  # We group in first by species,years and after month and after we compute the number of phenophases event 
    dplyr:::summarise(n = n()) %>%  # and after we compute the number of phenophases event 
    dplyr:::mutate(ngr = sum(n)) %>%  # Then, the number of phenophases per month
    dplyr:::mutate(prop = n/ngr*100) %>% # Then, the proportion of given phenophases
    ungroup() %>% filter(!!sym(Obs_Veg) %in% Pattern) # But just keep the wanted Obs_Veg 
  
  Graph = ggplot(data = Prop, aes(x = byMon, y = prop))
}




Graph = Graph + 
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
  coord_polar(direction = 1) + 
  labs(x = "Months",
       y = "Proportion (%)",
       title = paste("Proportion of individuals in Phenophase:",Pattern,"at month over sampling period" )) + 
  theme( axis.text.x = element_text(
      color = "grey28",
      size = 8,
      angle = 20
    ),
    axis.text.y = element_text(
      color = "black",
      size = 10,
      lineheight = 2
    ),
    axis.title.x = element_text(
      color = "black",
      size = 10),
    axis.title.y = element_text(
      color = "black",
      size = 10)
  ) + 
  facet_grid(cols = vars(Genus_Spec))  + # Create facets --> we can add in rows the different patterns if we want 
  annotate("text",x = Inf, y = Inf,label = paste("n_ind = ",n_ind),size = 3,hjust = -1.5,vjust=-0.8)

Exit=list(Prop,Graph)

  return(Exit)
}

####################################################################################################################### 
############################### MARKERS OF TIME FOR PHENOPHASE ########################################################
####################################################################################################################### 

PhenoPhase_Time <- function(Data,Pattern ="F",Spec,Obs_Veg = "PPVeg", markers = c("Residence_time","Return_time","Cycle_time")){
  

  #then filter the NA of the Obs (precisely for working with the unfill data and the Species
  Data = Data %>% as_tibble()

  if(Obs_Veg == "PPFlo"){
  # First at all if we are regarding to the fertility we have to replace all NA by sterility, which is different from no_obs
  Data = Data %>% mutate(PPFlo = ifelse(is.na(PPFlo),"Str",as.character(PPFlo)))
  }
  
  Data = Data %>%
    filter(Genus_Spec %in% Spec,!is.na(!!sym(Obs_Veg)),CrownID != "NA",!!sym(Obs_Veg) != "no_obs") %>% 
    dplyr::select(Obs_Veg,CrownID,date,Genus_Spec) %>% 
    distinct() %>% 
    arrange(CrownID) # Filtering by the Species wanted and Na for having the no gapfilled data
  
  ## Filter every ind which not have 2 samples of the same pattern
  if (Obs_Veg != "PPFlo"){
  insufind <- Data %>%
    group_by(CrownID) %>%
    filter(!!sym(Obs_Veg) %in% Pattern) %>%
    summarise(n = n()) %>%
    filter(n < 2) %>%
    pull(CrownID)
  } else{insufind = NULL}
  
  Data = Data %>% filter(!(CrownID %in% insufind)) 
  
  # If after the empty ind there is no data then stop the function 
  if(nrow(Data) == 0 ){
    message("Stop computing. There is not enought data to compute a cycle")
    return(invisible())
  }
  
  ## Creating a process to extract the wanted period
  
  firstdate=(Data %>% slice_head(n=1))$date
  lastdate = (Data %>% slice_tail(n=1))$date
  indend = NULL
  indebegin = NULL
  
  ## Finding the line where the event end
  for (a in unique(Data$CrownID)) {
    # for every crown
    for (i in 1:(nrow(Data %>% filter(CrownID == a))-1)) {
      # for every row
      if (Data[Data$CrownID == a, Obs_Veg][i,] == Pattern &
          ## if the line where the pattern is observed
          Data[Data$CrownID == a, Obs_Veg][i+1,] != Pattern){
        ## And if the next line which is not a "no_obs" is different from the pattern
        ## Then we consider the "no_obs" as not a ending point )
        indend = bind_rows(indend, Data[Data$CrownID == a, ][i, ])
        # If all those conditions are respected, then we consider the line as an ending point
      }
    }
  }
  
  ## Finding the line where the event begin
  for (a in unique(Data$CrownID)) {
    # for every crown
    for (i in 2:(nrow(Data %>% filter(CrownID == a)))) {
      # for every row
      if (Data[Data$CrownID == a, Obs_Veg][i, ] == Pattern &
          ## if the line where the pattern is observed
          Data[Data$CrownID == a, Obs_Veg][i-1, ] != Pattern)
        ## And if the previous line which is not a "no_obs" is different from the pattern
      {
        indebegin= bind_rows(indebegin, Data[Data$CrownID == a, ][i, ])
      }
    }
  }
  
  ## merging and arrange by the date while precising if it's the BEGIN or the END of the event
  if(!is.null(indend)){
  indend = indend %>% mutate(Place = "END")
  }
  if(!is.null(indebegin)){
  indebegin = indebegin %>% mutate(Place = "BEGIN")
  }
  tot = tibble(bind_rows(indebegin,indend)) %>% arrange(CrownID,date) %>% dplyr::select(date,CrownID,all_of(Obs_Veg),Place,Genus_Spec)
  
  ## And now how's working depend on the wanted Phenological traits among: BackTime, Residence_Time, Cycle_Length
  
  
  #---------------------------------------------
  #  RESIDENCE_TIME
  
  
  
  OpenInt = NULL
  ClosedInt = NULL
  ResTime = tibble()
  ind= NULL
  datend = NULL
  
  for (a in tot$CrownID %>% unique()) {
    
    # Case n°1
    ## if the first date is a "end" it means the period before is an Open interval
    if (first(tot[tot$CrownID == a, ])$Place == "END" & last(tot[tot$CrownID == a, ])$Place == "END") {
      OpenInt = c(OpenInt, difftime(first(tot[tot$CrownID == a & tot$Place == "END", ])$date, firstdate))
      ## For the Closed intervals
      for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "BEGIN", ])){ # depend of the number of begin
        # The closed itnerval correspond to every pair of begin and END expect the first end 
        ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i,]$date,
                                         tot[tot$CrownID == a & tot$Place == "END",][i+1,]$date ) # +1 because the first end is an OpenInt
        )
        datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
      }
    }
    
    # Case n°2
    ## if the last date is a "begin" it means the period after is an Open interval
    else if (last(tot[tot$CrownID == a, ])$Place == "BEGIN" & first(tot[tot$CrownID == a, ])$Place == "BEGIN") {
      OpenInt = c(OpenInt, difftime(last(tot[tot$CrownID == a & tot$Place == "BEGIN", ])$date, lastdate))
      ## For the Closed intervals
      for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "END", ])){ # depend of the number of end
        # The closed itnerval correspond to every pair of begin and END expect the first end 
        ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i,]$date,
                                         tot[tot$CrownID == a & tot$Place == "END",][i,]$date ) # The last int won't be computed ? 
        )
        datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
      }
    }
    
    
    # Case n°3
    ## if the last date is a "begin" and the last is a "end" it means bot periods before and after are Open intervals
    else if (last(tot[tot$CrownID == a, ])$Place == "BEGIN" & first(tot[tot$CrownID == a, ])$Place == "END") {
      OpenInt = c(OpenInt, difftime(last(tot[tot$CrownID == a & tot$Place == "BEGIN", ])$date, lastdate))
      OpenInt = c(OpenInt, difftime(first(tot[tot$CrownID == a & tot$Place == "END", ])$date, firstdate))
      ## For the Closed intervals
      for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "BEGIN", ])){ # depend of the number of begin
        # The closed itnerval correspond to every pair of begin and END expect the first end 
        ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i,]$date,
                                         tot[tot$CrownID == a & tot$Place == "END",][i+1,]$date) # +1 because the first end is an OpenInt
        )
        datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
      }
    }
    
    # Case n°4
    ## If there is no OpenInt
    else if (last(tot[tot$CrownID == a, ])$Place == "END" & first(tot[tot$CrownID == a, ])$Place == "BEGIN") {
      OpenInt = c(NA)
      ## For the Closed intervals
      for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "BEGIN", ])){ # depend of the number of begin
        # The closed itnerval correspond to every pair of begin and END expect the first end 
        ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i,]$date,
                                         tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
                      )
        ## We stock the date of the end of the event 
        datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
        
      }
    }
    ## Stocking the 2 type of data in a tibble
    ## Stocking the 2 type of data in a tibble
    ind = bind_rows(tibble(abs(ClosedInt),as.Date(datend)),tibble(abs(OpenInt))) %>% # absolute value 
      rename(ClosedInt='abs(ClosedInt)' ,OpenInt = 'abs(OpenInt)',datend = 'as.Date(datend)') %>% #renaming the columns
      mutate(CrownID = a,Genus_Spec = unique(tot[tot$CrownID ==a,]$Genus_Spec),Marker = "Residence_time") 
    
    ResTime = bind_rows(ResTime,ind)
    
    
    ## Reseting the temporary parameters
    OpenInt = NULL
    ClosedInt = NULL
    ind=NULL
    datend = NULL
  }
  #---------------------------------------------
  
  #  CYCLE_TIME
  
  OpenInt = NULL
  ClosedInt = NULL
  CycleTime = tibble()
  ind= NULL
  datend = NULL
  
  for (a in tot$CrownID %>% unique()) {
    
    # It not depend of the 3 previous case we use only the begin of each event
    # The openintervals are the first begin with the first date and the last begin with the last date
    OpenInt = c(OpenInt, difftime(first(tot[tot$CrownID == a & tot$Place == "BEGIN", ])$date, firstdate))
    OpenInt = c(OpenInt, difftime(last(tot[tot$CrownID == a & tot$Place == "BEGIN", ])$date, lastdate))
    ## For the Closed intervals
    for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "BEGIN", ])){ # depend of the number of begin
      # The closed itnerval correspond to every pair of begin and END exept the first end 
      ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i,]$date,
                                       tot[tot$CrownID == a & tot$Place == "BEGIN",][i+1,]$date ) # +1 because the first end is an OpenInt
      )
      datend = c(datend,tot[tot$CrownID == a & tot$Place == "BEGIN",][i+1,]$date)
    }
    
    ## Stocking the 2 type of data in a tibble
    ind = bind_rows(tibble(abs(ClosedInt),as.Date(datend)),tibble(abs(OpenInt))) %>% # absolute value 
      rename(ClosedInt='abs(ClosedInt)' ,OpenInt = 'abs(OpenInt)',datend = 'as.Date(datend)') %>% #renaming the columns
      mutate(CrownID = a,Genus_Spec = unique(tot[tot$CrownID ==a,]$Genus_Spec),Marker = "Cycle_time") 
    
    CycleTime = bind_rows(CycleTime,ind)
    
    
    ## Reseting the temporary parameters
    OpenInt = NULL
    ClosedInt = NULL
    ind=NULL
    datend = NULL
  }
  
  
  #---------------------------------------------
  
  
  
  
  #  Return_time
  
  OpenInt = NULL
  ClosedInt = NULL
  BackTime = tibble()
  ind= NULL
  datend = NULL
  
  for (a in tot$CrownID %>% unique()) {
    
    # Case n°1
    ## if only the first date is a "BEGIN" it means only the period before is an Open interval --> then the last is event is not a end too
    if (first(tot[tot$CrownID == a, ])$Place == "BEGIN" & last(tot[tot$CrownID == a, ])$Place == "BEGIN") {
      OpenInt = c(OpenInt, difftime(first(tot[tot$CrownID == a & tot$Place == "BEGIN", ])$date, firstdate))
      ## For the Closed intervals
      for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "BEGIN", ])){ # depend of the number of begin
        # The closed itnerval correspond to every pair of begin and END expect the first end 
        ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "END",][i,]$date,
                                         tot[tot$CrownID == a & tot$Place == "BEGIN",][i+1,]$date ) # +1 because the first end is an OpenInt
        )
        datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i+1,]$date)
      }
    }else
      
      # Case n°2
      ## if only the last date is a "END" it means only the period after is an Open interval --> then the first event is not a begin too
      if (last(tot[tot$CrownID == a, ])$Place == "END" & first(tot[tot$CrownID == a, ])$Place == "END") {
        OpenInt = c(OpenInt, difftime(last(tot[tot$CrownID == a & tot$Place == "END", ])$date, lastdate))
        ## For the Closed intervals
        for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "END", ])){ # depend of the number of end
          # The closed interval correspond to every pair of begin and END expect the first end 
          ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i,]$date,
                                           tot[tot$CrownID == a & tot$Place == "END",][i,]$date ) # The last int won't be computed ? 
          )
          datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
        }
      }
    
    
    # Case n°3
    ## if the first date is a "begin" and the last is a "end" it means both periods before and after are Open intervals
    else if (last(tot[tot$CrownID == a, ])$Place == "END" & first(tot[tot$CrownID == a, ])$Place == "BEGIN") {
      OpenInt = c(OpenInt, difftime(last(tot[tot$CrownID == a & tot$Place == "END", ])$date, lastdate))
      OpenInt = c(OpenInt, difftime(first(tot[tot$CrownID == a & tot$Place == "BEGIN", ])$date, firstdate))
      ## For the Closed intervals
      for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "BEGIN", ])){ # depend of the number of begin
        # The closed itnerval correspond to every pair of begin and END expect the first end 
        ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i+1,]$date,
                                         tot[tot$CrownID == a & tot$Place == "END",][i,]$date) # +1 because the first end is an OpenInt
        )
        datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
      }
    }
    
    # Case n°4
    ## If there is no OpenInt
    else if (last(tot[tot$CrownID == a, ])$Place == "BEGIN" & first(tot[tot$CrownID == a, ])$Place == "END") {
      OpenInt = c(NA)
      ## For the Closed intervals
      for (i in 1:nrow(tot[tot$CrownID == a & tot$Place == "BEGIN", ])){ # depend of the number of begin
        # The closed interval correspond to every pair of begin and END expect the first end 
        ClosedInt = c(ClosedInt,difftime(tot[tot$CrownID == a & tot$Place == "BEGIN",][i,]$date,
                                         tot[tot$CrownID == a & tot$Place == "END",][i,]$date) # +1 because the first end is an OpenInt
        )
        datend = c(datend,tot[tot$CrownID == a & tot$Place == "END",][i,]$date)
      }
    }
    
    ## Stocking the 2 type of data in a tibble
    ind = bind_rows(tibble(abs(ClosedInt),as.Date(datend)),tibble(abs(OpenInt))) %>% # absolute value 
      rename(ClosedInt='abs(ClosedInt)' ,OpenInt = 'abs(OpenInt)',datend = 'as.Date(datend)') %>% #renaming the columns
      mutate(CrownID = a,Genus_Spec = unique(tot[tot$CrownID ==a,]$Genus_Spec),Marker = "Return_time") 
    
    BackTime = bind_rows(BackTime,ind)
    
    
    ## Reseting the temporary parameters
    OpenInt = NULL
    ClosedInt = NULL
    ind=NULL
    datend = NULL
    
  }
  
  
  #---------------------------------------------
  
  ## Joining the 3 different markers
  
  final = bind_rows(BackTime,CycleTime,ResTime) %>% 
    pivot_longer(cols = c(OpenInt,ClosedInt),names_to = "TypeInt") %>% 
    filter(!is.na(value)) %>% mutate(Marker = as.factor(Marker),
                                     CrownID = as.factor(CrownID),
                                     TypeInt = as.factor(TypeInt),
                                     Genus_Spec = as.factor(Genus_Spec))
  
  add_text = final %>%
    filter(TypeInt == "ClosedInt") %>% 
    dplyr:::group_by(Genus_Spec,Marker) %>%
    dplyr:::summarise(n_ind = length(unique(CrownID)),n_count = n()) %>% 
    filter(Marker %in% markers)
    
    final = final %>% filter(Marker %in% markers) %>% arrange(Genus_Spec,Marker,TypeInt,value) 
  droplevels.factor(final$Marker) 
  
  # Discar Open Int
  dataplot = final %>% filter(TypeInt == "ClosedInt")
  
  # If after the empty ind there is no data then stop the function 
  if(nrow(dataplot) == 0 ){
    message("Stop computing. There is not enought data for one individual to compute a cycle")
    return(invisible())
  }
  
  ## PLOT 
  ## facet grid enable to dplyr::select different species and plot them in one times 
  Hist_Sp = ggplot(data = dataplot) + 
    geom_histogram(aes(x=value), breaks =seq(min(dataplot$value)-50,max(dataplot$value)+50,5)) + 
    geom_text(size    = 2,
              data    = add_text,
              mapping = aes(x = Inf, y = Inf, label = paste("n ind = ",n_ind,sep = " ")),
              hjust   = 1.05,
              vjust   = 1.5) +
    geom_text(size    = 2,
              data    = add_text,
              mapping = aes(x = Inf, y = Inf, label = paste("n count = ",n_count,sep = " ")),
              hjust   = 1.05,
              vjust   = 2.5) + 
        theme_pubclean() + 
    labs(title = paste("Time distribution of",c(markers),"of Pattern",Pattern, sep = " "), x = "Time of event", y = "Count")
    # geom_vline(xintercept = median(dataplot$value, na.rm=TRUE), col="red") + 
    # # 95% of the values 
    # geom_vline(xintercept = quantile(dataplot$value, probs=0.025, na.rm=TRUE), linetype="dotted") +
    # geom_vline(xintercept = quantile(dataplot$value, probs=0.975, na.rm=TRUE), linetype="dotted") +
    # # half of the values
    # geom_vline(xintercept = quantile(dataplot$value, probs=0.25, na.rm=TRUE), linetype="dashed") +
    # geom_vline(xintercept = quantile(dataplot$value, probs=0.75, na.rm=TRUE), linetype="dashed") +

  
# If more then one markers
if ( length(markers) > 1 ){
    Hist_Sp = Hist_Sp + 
      facet_grid(cols = vars(Genus_Spec),rows = vars(Marker), scales = "fixed") + # then make a facet
      labs(title = paste("PhenoPhase's Time of Pattern",Pattern, sep = " "), x = "Durée d'évènement (jours)", y = "Effectif")
}
  
if ( length(markers) == 1 ){
  Hist_Sp = Hist_Sp + facet_grid(cols = vars(Genus_Spec), scales = "fixed", space = "free") + # facet with "free" space
    labs(title = paste("Time distribution of",c(markers),"of Pattern",Pattern, sep = " "), x = "Durée d'évènement (jours)", y = "Effectif")
  }
  
  
  # The function will return the different number of cycle per individuals and per species + the plot
  
  Exit = list(final,Hist_Sp)
  
  return(Exit)
  
  
}


####################################################################################################################### 
############################### BREAKPOINT SEGMENTED PACKAGE ##########################################################
####################################################################################################################### 

# function to do Breakpoint analysis using the package segmented 
# ! this function is not great because
# need to prepare my.seg outside the function
# as it seems that selgmented doesn't work when it's inside a function
# See in Pradosio_cohlearia_V3 how to prepare it
# See if I can change that...
# Try to replcae the columns name "date" by "MyDate"

Breapoint1Ind <- function(Data, # a data.table with only one individual
                          Var, # name of one continuous variable that we are interested in
                          Subset = NULL, # do we want to subset the time for Drop or Peak? ("Drop" or "Peak)
                          Percentile =NA, # if we want to subset, what percentile is considered as a threshold?
                          MakePlot=TRUE, # do we want the plot of result?
                          my.seg,
                          Data4Phase = NULL, # data to use if we want to add the phenophase
                          LabelHeight=0) # height at which we want to put the label
{
  if(!(is.null(Subset)) & is.na(Percentile)) {  
    stop("Function Breapoint1Ind: if you want to subset Drops or Peaks,
         you need to provide percentile as a threhold")
  }
  
  # prepare the Dataset
  colnames(Data)[which(colnames(Data)==Var)] <- "MyVar"

  # # do the segmented regression
  # # ggplot(Data, aes(x=date, y=MyVar)) + geom_point()
  # out.lm <- lm(MyVar ~ date, data=Data)
  # my.seg <- selgmented(out.lm, type="bic", Kmax=10,  refit = TRUE, msg = FALSE)
  ## 9 breakpoints dplyr::selected

  # get the dplyr::selected breakpoints
  psi <- as.data.table(my.seg$psi)
  #Breaks <- psi$Initial
  Breaks <- as.Date(psi$Initial)
  
  # add the fitted Data to the data set
  Data$Fitted <- fitted(my.seg)
  # get the data at the breakpoints
  Databreak <- Data[date %in% round(Breaks),]

  # subset the drop or peak
  if(!(is.null(Subset))) {
  # get the median of all the time serie
    Threshold <- quantile(Data$MyVar, probs=Percentile)
    # dplyr::select the drop
    if(Subset=="Drop") {
      DataDrop <- Databreak[MyVar<Threshold, ]
    }
    # or the peak
    if(Subset=="Peak") {
      DataPeak <- Databreak[MyVar>Threshold, ]
    }
  }

  # make a graph
  if(MakePlot==TRUE) {
     PlotRes <- ggplot(data=Data, aes(x=date)) +
       geom_line(aes(y=MyVar)) + # observed
       geom_line(aes(y=Fitted), col="red", linewidth=1, linetype="dashed") + # fitted
       geom_vline(xintercept = Breaks, linetype="dashed", col="grey") + # all breakpoint
       ylab(Var) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 90)) +
       scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
       ggtitle(paste(unique(Data$CrownID), " - ",
                     unique(Data$Genus_Spec), " (", Var, ")",
               sep=""))
  
  
     # add the timing subseted
     if(!(is.null(Subset))) {
       if(Subset=="Drop") {
         PlotRes <- PlotRes +
           geom_point(data=DataDrop, aes(x=date, y=MyVar), col="blue", size=3)
       }
       if(Subset=="Peak") {
         PlotRes <- PlotRes +
           geom_point(data=DataPeak, aes(x=date, y=MyVar), col="green", size=3)
       }
     }
  
    #add the phenophase
     if(!(is.null(Data4Phase))) {
       if(dim(Data4Phase)[1]>0) {
         PlotRes <- PlotRes +
           geom_text(data = Data4Phase,
                     aes(x=date, y=LabelHeight, label=PPVeg),
                     col="darkgrey",
                     na.rm=TRUE)
       }
     }
   }

  # Prepare things to return
  colnames(Databreak)[which(colnames(Databreak)=="MyVar")] <- Var
  Output <- list(Breakpoint = Databreak)
  if(!(is.null(Subset))) {
    if(Subset=="Drop") {
      Output <- c(Output, list(TimeDrop = DataDrop$date))
    }
    if(Subset=="Peak") {
      Output <- c(Output, list(TimePeak = DataPeak$date))
    }
  }
  if(MakePlot==TRUE) {
    Output <- c(Output, list(PlotRes=PlotRes))
  }

  return(Output)

}


####################################################################################################################### 
############################### CLUSTERING ON QUALITATIVE DATA ########################################################
####################################################################################################################### 

Pheno_Clust <- function(Data,Spec,K = NULL,Obs_Veg ="PPVeg"){

## First we create the data with the sequence in row in column, the date with the name of ind and Genus_Spec
Data = Data %>%
  filter(!is.na(!!sym(Obs_Veg)), Genus_Spec %in% c(WantedSpec)) %>% # extract the NA and wantedSpec
  dplyr::select(!!sym(Obs_Veg), CrownID, date,Genus_Spec) %>% 
  distinct(!!sym(Obs_Veg), CrownID, date, Genus_Spec) %>% # distinct because we have only Phenophases then we pull out others
  group_by(CrownID) %>% # For each ind
  mutate(!!sym(Obs_Veg) := str_replace(!!sym(Obs_Veg),"no_obs",NA_character_)) %>% # replace no_obs by NA for next step 
  arrange(CrownID, date) %>% pivot_wider(names_from = date, values_from = !!sym(Obs_Veg))  

# Convert sequences into a state sequence object
full_seq <- seqdef(
  data = Data,
  3:50,
  alphabet = c("F","D","L"),
  with.missing = TRUE
)

# Define the substitution cost matrix based on the specifications
# costmat <- matrix(c(0,3,3,3,
#                     3,0,1,3,
#                     3,1,0,3,
#                     3,3,3,0), 
#                   byrow = TRUE, nrow = 4,
#                   dimnames = list(c("F", "D", "L","*"), 
#                                   c("F", "D", "L","*")))

costmat = seqcost(full_seq, method = "INDELSLOG", with.missing = TRUE)

# Create a distance matrix between the sequences using TraMineR
dist_matrix <- seqdist(full_seq, method = "OM", sm = costmat$sm, indel = costmat$indel,
                       full.matrix = TRUE, norm = "auto", with.missing = TRUE)  # OM = Optimal Matching

# Perform agglomerative clustering analysis
clusterward <- agnes(dist_matrix, diss = TRUE, method = "ward", stand = TRUE)  # The "ward" method is used for clustering linkage

# Display only the dendrogram
plot_dendro = plot(clusterward, which.plot = 2)

## Agglomerative coefficient
ac = clusterward$ac

## Mean of entropy 
se = seqstatd(full_seq)$Entropy

## Other index which already need clusters

if (!is.null(K)) {
  
  # Cut the clustering tree into K clusters
  mvad.cl2 <- cutree(clusterward, k = K)  # Cut the tree according to the desired number of clusters
  # Convert the clustering results into factors with labels "Cluster1", "Cluster2", ...
  cl2.lab <- factor(mvad.cl2, labels = paste("Cluster", 1:K))  # Labels are based on the number of specified clusters
  # Create a sequence plot to visualize the clustering results
  
  silhouette_coef = silhouette(mvad.cl2, dist_matrix) %>% as_tibble()
  # Compute the average silhouette coefficient for each cluster
  index_silh = silhouette_coef %>% group_by(cluster) %>% summarise(mean_silh = mean(sil_width)) %>% dplyr::select(mean_silh) %>% pull()
  
  # Name of clusters
  clusters = silhouette_coef %>% group_by(cluster) %>% summarise(mean_silh = sd(sil_width)) %>% dplyr::select(cluster) %>% pull()

  
  Summar_clust = tibble(Cluster=clusters,
                        mean_silh = index_silh,
                        entropy = mean(se))
  
  ## Creating the plot of the different cluster
  seqdplot(full_seq, group = cl2.lab,border=NA) # Sequences are colored according to the clusters and borders are removed
  ## We now compute the differents cluster and the associated species
  Output_cluster = tibble(Data,cl2.lab) %>% rename(Clust = cl2.lab ) %>% dplyr::select(Genus_Spec,Clust,CrownID) %>% 
    arrange(Clust,Genus_Spec,CrownID)
  
  # Here it's the general attribution of species
  plot_Attribute = ggplot(data = Output_cluster) + geom_bar(aes(x = Clust,fill=Genus_Spec), stat='count')
  
  ## Now we want to enable the different outliers
  Outliers = tibble()
  
  for (i in Output_cluster %>% dplyr::select(Clust) %>% pull() %>% unique()){
    
    out_clust = Output_cluster %>% filter(Clust == i)
    Outliers = bind_rows(Outliers,out_clust)
    
  }
  
  
}

Exit = list("plot_dendro" = plot_dendro,"Summar" = Summar_clust,"plot_Attribute" = plot_Attribute,Outliers)

return(Exit)  

}


####################################################################################################################### 
############################### PARAMETERS FOR MOVING AVERAGE ######################################################### 
####################################################################################################################### 

# General function for generating different forms of weighting (from Gilles le Moguedec)
fpoids<-function(n=1,p=1,q=1){
  Poids<-pbeta(q=(0:n)/(n+1),shape1 = p, shape2 = q,lower.tail = FALSE)
  Poids<-c(rev(Poids),Poids[-1])
  Poids<-Poids/sum(Poids)
  Sortie<-list(x=(-n):n,y=Poids)
  return(Sortie)
}

####################################################################################################################### 
############################### COMPUTE MOVING AVERAGE DEPENDING ON PARAMETERS ########################################
####################################################################################################################### 

moving_average<-function(seq, filter){
  elem_op<-function(k,seq,filter){
    n<-(length(filter)-1)/2
    if (k<n+1){begin<-rep(seq[1],n+1-k)}
    else {begin<-c()}
    if (k>length(seq)-n){end<-rep(seq[length(seq)],n+k-length(seq))}
    else {end<-c()}
    Indices<-(k-n):(k+n)
    Indices<-Indices[Indices>0 & Indices<=length(seq)]
    serie<-c(begin,seq[Indices],end)
    sortie<-sum(serie*filter)
    return(sortie)
  }
  Sortie<-sapply(X=1:length((seq)),FUN=elem_op,seq=seq,filter=filter)
  return(Sortie)
}

# How to get residuals ? 
res_ma<-function(seq, filter){
  ma<-moving_average(seq, filter)
  res<-seq/ma
  return (res)}

####################################################################################################################### 
############################### DETECTION MIN - MAX ###################################################################
####################################################################################################################### 

Rang_Min_max<-function(tab, sensibility) {
  Ecart_min = sensibility * (max(tab) - min(tab))
  max <- c()
  min <- c()
  # sens: equals 1 if we go up to a maximum, equals -1 if we go down to a minimum
  # init: initialization rank of an upward or downward phase
  # Initialization: we consider the first point as a minimum if it is closer to the global minimum than the global maximum, otherwise as a maximum
  if (abs(tab[1] - max(tab)) > abs(tab[1] - min(tab))) {
    sens <- 1
    min <- c(min, 1)
  } else {
    sens <- -1
    max <- c(max, 1)
  }
  init = 1
  last_extrema = tab[1]
  # We change from upward to downward phase if three conditions are met:
  # 1) Change of variation direction
  # 2) The difference between the current point and the last extremum is sufficiently high
  # 3) The current point must be higher than the last extremum if it was a minimum, lower if it was a maximum
  for (i in 1:(length(tab) - 1)) {
    if ((tab[i + 1] * sens < tab[i] * sens) & (abs(tab[i] - last_extrema) > Ecart_min) & (sens * tab[i] > last_extrema * sens)) {
      if (sens == 1) {
        max <- c(max, i)
        init = i
        sens = -1
        last_extrema = tab[i]
      } else {
        min <- c(min, i)
        init = i
        sens = 1
        last_extrema = tab[i]
      }
    }
  }
  rbind(min, max)
}


####################################################################################################################### 
############################### PLOT SIGNAL, MOVING AVERAGE, PEAKS DETECTION, CYCLE LENGTH ############################ 
####################################################################################################################### 

# Function definition
Plot_Peaks <- function(Data, ID, Obs,Index, Filter = fpoids(n = 3, p = 2, q = 2)$y,Nd, MinPH = -Inf,Value = "value",Tshld = 0,Obs_Veg = "PPVeg") {
  
  # Filtering the data based on CrownID
  filtered_data <- Data %>% filter(CrownID %in% ID, Type_Obs == Obs, index == Index, !is.na(vars(Value))) %>% dplyr::select(date, Value, !!sym(Obs_Veg), everything())
  # Calculating the moving average
  moyenne_mobile <- moving_average(filtered_data %>% dplyr::select(Value) %>% pull(), filter = Filter)
  
  # Creating a data frame with the original data and the moving average
  plot_data <- tibble(
    date = filtered_data$date,
    signal = filtered_data[[2]],
    moyenne_mobile = moyenne_mobile,
    Phase = filtered_data %>% select(!!sym(Obs_Veg)) %>% pull()
  )
  
  # Filtering out the NA values from the signal
  d_signal <- plot_data %>% filter(!is.na(moyenne_mobile)) %>% dplyr::select(moyenne_mobile) %>% pull()
  
  # Identifying the different positive and negative peaks
  dat_pos <- sort(findpeaks(d_signal, minpeakdistance = Nd, nups =1, minpeakheight = MinPH,threshold = Tshld)[, 2])
  
  # Retrieving the dates without NA points
  dates_WhtNA <- plot_data %>% filter(!is.na(moyenne_mobile)) %>% dplyr::select(date) %>% pull()
  
  # if (plot_data %>% filter(is.na(moyenne_mobile)) %>% length() > 1) {
  #   message(paste(plot_data %>% filter(is.na(moyenne_mobile)) %>% length(), "NA's have been removed for the analysis"))
  # }
  
  # Plotting the original signal, the moving average, and the identified breakpoint points
  Plot <- ggplot(plot_data, aes(x = date)) +
    geom_line(aes(y = signal,linetype ="Main Signal"), col = "grey40") +
    geom_line(aes(y = moyenne_mobile,linetype ="Moving Average"), col = "black") +
    labs(title = paste("Initial signal with moving average of Crown ID",ID,"(",Index,")"), x = "Time", y = "Value") +
    theme_minimal() +
    geom_vline(xintercept = dates_WhtNA[dat_pos], col = "red3", linetype = "dashed") + 
    geom_text(data = plot_data %>% filter(Phase != "no_obs"), aes(x = date, y = -1, label = Phase), col = "black", na.rm = TRUE) +
    scale_linetype_manual(values = c("Main Signal" = "dashed","Moving Average"="solid"))
  
  # Using the original dates to ensure consistent length for every individual without depending on missing values
  original_dates <- Data %>% filter(CrownID %in% ID, Type_Obs == Obs, index == Index) %>% dplyr::select(date) %>% pull()
  
  # Computing a new binary sequence for comparison with other methods
  seq <- rep(0, length(original_dates))
  
  # Creating a tibble with the ID, the dates and the binary seq 
  Simple <- tibble(CrownID = ID, date = original_dates, Seq = seq) %>%
    mutate(Seq = if_else(date %in% dates_WhtNA[dat_pos], 1, Seq)) # we replace by 1 where the maximum has been computed

  # We then give a list with Cycle Length / Plot / the dates (timing) / and the binary seq 
  Exit <- list(diff(dates_WhtNA[dat_pos]), Plot, dates_WhtNA[dat_pos], Simple)

  return(Exit)
}


####################################################################################################################### 
############################### PLOT INDEXS ###########################################################################
####################################################################################################################### 

Plot_Indexs <- function(Data, Obs, Index, ID, Value = "value",Obs_Veg = "PPVeg") {

if(isempty(Obs)|isempty(ID)|isempty(Index)){
    print("no data for this combinaison")
  return(invisible())}
  
  Plot_data = Data %>%
    filter(
      Type_Obs == Obs,
      CrownID == ID,
      grepl(Index, index, ignore.case = TRUE)
    )
  
  if(nrow(Plot_data) == 0){
    message("No data for this Index / Obs ")
    return(invisible())
  }
  
  if (!(Obs %in% c("dense","large"))){ # if we don't are in lidar data
  # Supposons que Plot_data_mean contient les moyennes et Plot_data_sd contient les écarts-types
  Plot_data_mean <- Plot_data %>% filter(grepl(paste(Index, ".mean", sep = ""), index, ignore.case = TRUE))
  Plot_data_sd <- Plot_data %>% filter(grepl(paste(Index, ".sd", sep = ""), index, ignore.case = TRUE))
  } else { # Don't need if we are in lidar data
  Plot_data_mean <- Plot_data 
  }

  plot = ggplot() +
    theme_pubclean() +  
    geom_line(data = Plot_data_mean,
              aes(
                x = date,
                y = !!sym(Value),
                col = index,
                linetype =index),na.rm = TRUE
              ) +
    labs(title = paste(Obs,Index,"CrownID",ID)) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y") +
    geom_text(
      data = Data %>% filter(CrownID == ID, !!sym(Obs_Veg) != "no_obs") %>% select(!!sym(Obs_Veg), CrownID, date) %>% distinct(),
      aes(x = date,
          y = 0,
          label = !!sym(Obs_Veg)),
      col = "black",
      na.rm = TRUE
    ) + 
    scale_colour_manual(values = MyCol) +
    # scale_fill_manual(values = MyCol) + # Ajout pour la couleur de remplissage
    scale_linetype_manual(values = MylineType)
# If it's not lidar data we add the sd as ribbbon
    # if(!(Obs %in% c("dense","large"))){
    #   plot = plot + geom_ribbon(data = Plot_data_mean, # Then we add the sd as a geom
    #                                      aes(
    #                                        x = date,
    #                                        ymin = !!sym(Value) - Plot_data_sd[[Value]],  # Soustraction de l'écart-type
    #                                        ymax = !!sym(Value) + Plot_data_sd[[Value]],  # Addition de l'écart-type
    #                                        fill = index
    #                                      ),
    #                                      alpha = 0.2)
    # }
    # 
  
  return(plot)
}



####################################################################################################################### 
############################### FOURRIER TRANSFORMATION ###############################################################
####################################################################################################################### 


## Function for furrier analysis 
Pheno_fourrier <- function(ID, Obs, Index,Nmax = 4){
  
  # NA at position 49
  signal_raw = Full %>% filter(CrownID == ID,Type_Obs == Obs,index == Index) %>% pull(value)
  NA_pos =  which(is.na(signal_raw))
  
  if(!isempty(NA_pos)){
    # signal  
    signal = signal_raw[-NA_pos]
    # create vector of date
    temps =  Full %>% filter(CrownID == ID,Type_Obs == Obs,index == Index) %>% pull(date) %>% .[-NA_pos]
  } else{ 
    # signal  
    signal = signal_raw
    # create vector of date
    temps =  Full %>% filter(CrownID == ID,Type_Obs == Obs,index == Index) %>% pull(date)
  }
  
  temps = c(temps, max(temps) + 1)
  temps_num = diff.Date(temps) %>% as.vector() %>% cumsum()
  # Calcul de la transformation de Fourier
  transformee <- fft(signal)
  
  # Obtention des fréquences
  frequences <- seq(0, 1/(2*(temps_num[2]-temps_num[1])), length.out = floor(length(transformee)/2)) # floor permet d'avoir le nombre entier
  
  # Obtention des amplitudes
  amplitudes <- 2/length(signal) * abs(transformee[1:(length(signal)/2)])
  
  # Affichage des résultats
  plot(frequences, amplitudes, type = "l", xlab = "Fréquence (Hz)", ylab = "Amplitude")
  
  # Identification de la fréquence maximum 
  data = tibble(frequences,
                amplitudes,
                CrownID = ID)
  
  # On sélectionne les quatres valeurs maximales
  amp_max = data %>% filter(frequences != 0) %>% slice_max(amplitudes, n= 2) %>% pull(frequences)
  # frequence observed in days
  cycle_fft_days = 1 / amp_max 
  
  return(list(data,cycle_fft_days))
  
}