source("Source custom functions/myTheme.R")


# function to load an prepare as a long table
ReadDatNico <- function(filepath, TableID, date, nameVar) {
  Table <- read.csv(filepath, header=FALSE) # read
  Table <- cbind(TableID$V1, Table)
  colnames(Table) <- c("CrownID", colnames(date))
  Table <- as.data.table(Table)
  TableL <- melt(Table, id.vars = "CrownID", variable.name = "date", value.name = nameVar) # make it a long table
  TableL$CrownID <- as.factor(TableL$CrownID)
  TableL$date <- anytime::anydate(TableL$date)
  return(TableL)
}


# function to make graph 1:1 to compare different data set
GraphCompare <- function(Data, Mapping, title, xmin, xmax, ymin, ymax) {
  ggplot(Data, Mapping) +
    geom_point(alpha=0.1) + 
    geom_abline(slope=1) +
    xlim(xmin,xmax) + ylim(ymin,ymax) + 
    theme(legend.position="none") +
    ggtitle(title)
}


# function to plot 1 indiv (three level of greeness)
PlotGrness1Ind <- function (Data, 
                            ID, # ID of the crown
                            VarRGB, # RGB variables that we want to plot
                            Legend = TRUE, 
                            Phase = FALSE, # do we want to add the phenophase on the graph
                            Overlap = FALSE,
                            ncol = 1,
                            LabelHeight=0, # height at which we want to put the label
                            Facet = NULL) { 
  # Transfor in data table
  Data = as.data.table(Data)
  
  # transfor in long table
  DataLInd <- melt(Data[CrownID==ID], measure.vars = VarRGB,
                                       variable.name = "RGBType", value.name = "Value")

  # if I want to do a facet by another column
  if (!is.null(Facet)) {
    colnames(DataLInd)[which(colnames(DataLInd)==Facet)] <- "Facet" # rename the colum
    # I make a data will the phase data set, repeated so that I have all the phases for all the value of Facet
    # I will use it for the labels
      DataPhaseTemp <- DataLInd[!(is.na(PPVeg)),.(fid_1, date, PPVeg)]
      DataPhaseTemp <- data.table(DataPhaseTemp,
                              Facet = rep(DataLInd[!(is.na(Facet)), unique(Facet)], each=dim(DataPhaseTemp)[1]))
      # add one RGBType (will not be use but cannot do the facter without)
      DataPhaseTemp$RGBType <- VarRGB[1]
    # I then select only the non-NA Facet, to avoid having a facet with NA values
      # !!!! This mean that there must be no NA in the facet pour lesquel VarRGB n'est pas NA
      # so I check that first
      if(dim(DataLInd[!(is.na(Value)) & is.na(Facet)])[1] >0)  {
        warning("You have NA in the facet for non-NA value of the VarRGB. They wont' be shown on the graphs.")
      }
      DataLInd <- DataLInd[!(is.na(Facet))] 
  } else {
    DataPhaseTemp <- DataLInd
  }
  
  # make the graph
  graph <- ggplot(data = DataLInd, aes(x=date, y=Value, 
                                       group=RGBType, col=RGBType, linetype = RGBType)) 
  for (Type in VarRGB) {
    graph <- graph + geom_line(data = DataLInd[RGBType==Type & !(is.na(Value)), ], na.rm=TRUE)
  }
   
  graph <- graph +
    ggtitle(paste(ID, DataLInd[, unique(Genus_Spec)], sep=" - ")) + 
    scale_colour_manual(values = MyCol) + 
    scale_linetype_manual(values = MylineType) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))+ #, vjust = 0.5, hjust=1))
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")
  
  
  # separate in different graph if overlap is false
  if(Overlap == FALSE) {
    graph <- graph + 
      facet_wrap(~RGBType, scale="free_y", ncol = ncol) 
  }
  
  # separate by Facet 
  if (!is.null(Facet)) {
    graph <- graph + facet_wrap(~Facet, ncol= ncol, scale="free_y") 
  }
  
  # add phenophase 
  if(Phase==TRUE) {
    graph <- graph + 
      geom_text(data = DataPhaseTemp,
                aes(x=date, y=LabelHeight, label=PPVeg),
                col="darkgrey", 
                na.rm=TRUE)
  }
  
  return(graph)  
}


########################################
# function PrepPhase to prepare phenophase data
# it gives:
# PPraw is the raw phenophase as coded by visual assessment
# PPVegraw is the vegetative phenophase, with all the info from visual assessment
# PPFlo shows flowering
# desynchr indicates a desyncrhonism within the crown
# uncert indicates if we have doubts about the visual assessment 
# PPVeg give a vegetative phenophase with only one phase (the one given first)
# transition indicates trees that were between phases
PrepPhase <- function(Data) {
  Data <- as.data.table(Data)
  # make it a long table
  Data <- suppressWarnings(melt(Data, 
                                id.vars = c("Plot", "Num_crown", "Code.sp", 
                                            "Family", "Genus", "Species", "Obs", "Comm","Update","Usable"),
                                variable.name = "date", value.name = "PPraw"))
  
  # transforme en date
  Data$date <- as.character(Data$date)
  Data[, date := gsub('X', '', date)] # because when important there are X before the date, can be remove if no longer a problem
  Data[, date := as.Date(date, "%d/%m/%Y")]
  
  # separate colum for flowers (;)
  Data[, PPVegraw := tstrsplit(PPraw,";")[[1]]]
  if (length(tstrsplit(Data$PPraw,";"))>1) { # if there are stuff after the ;
    Data[, PPFlo := tstrsplit(PPraw,";")[[2]]]
  } else {
    Data[, PPFlo := ""]
  }
  
  # fill in the colum for special cases
  Data$desynchr <- as.logical(FALSE)
  Data[grepl("\\*", PPVegraw), # when * in the string
       desynchr := TRUE]
  
  Data$uncert <- as.logical(FALSE)  
  Data[grepl("\\?", PPVegraw), # when ? in the string
       uncert := TRUE]
  
  Data$transition <- as.logical(FALSE)
  Data[grepl("/", PPVegraw), # when ? in the string
       transition := TRUE]
  
  Data$poly <- as.logical(FALSE)
  Data[grepl("P", PPVegraw), 
       poly := TRUE]
  
  
  # indicate desynchro (*)
  # Data$desynchr <- as.logical(FALSE)
  Data$PPVeg1 <- as.character(NA)
  # Data[grepl("\\*", PPVegraw), # when * in the string
  #      desynchr := TRUE] 
  Data[grepl("\\*", PPVegraw), # when * in the string
       PPVeg1 := tstrsplit(PPVegraw, "\\*")[[1]]] # replace by what is before the *
  Data[!(grepl("\\*", PPVegraw)), # when * in the string
       PPVeg1 := PPVegraw] 
  
  # indicate uncertainty (?)
  Data$PPVeg2 <- as.character(NA)
  # Data$uncert <- as.logical(FALSE)
  # Data[grepl("\\?", PPVeg1), # when ? in the string
  #      uncert := TRUE]
  Data[grepl("\\?", PPVeg1), # when ? in the string
       PPVeg2 := tstrsplit(PPVeg1, "\\?")[[1]]] # replace by what is before the *
  Data[!(grepl("\\?", PPVeg1)), # when * in the string
       PPVeg2 := PPVeg1] 
  
  # indicate transition between two phases (/)
  Data$PPVeg3 <- as.character(NA)
  # Data$transition <- as.logical(FALSE)
  # Data[grepl("/", PPVeg2), # when ? in the string
  #      transition := TRUE]
  Data[grepl("/", PPVeg2), # when ? in the string
       PPVeg3 := tstrsplit(PPVeg2, "/")[[1]]] # replace by what is before the *
  Data[!(grepl("/", PPVeg2)), # when * in the string
       PPVeg3 := PPVeg2] 
  
  # note the polyciclism
  Data$PPVeg <- as.character(NA)
  # Data$poly <- as.logical(FALSE)
  # Data[PPVeg3=="P", poly := TRUE]
  Data[, PPVeg:=PPVeg3]
  Data[PPVeg=="P", PPVeg:="F"]
  
  # remove the temporary columns
  Data[,PPVeg1:=NULL]
  Data[,PPVeg2:=NULL]
  Data[,PPVeg3:=NULL]
  
  # fill in the empty cell with no_obs (and not NA as NA would be gapfilled)
  Data[PPVeg=="", PPVeg:= "no_obs"]
  Data[is.na(PPVeg), PPVeg:="no_obs"]
  
  # fill in the empty cell with no_obs (and not NA as NA would be gapfilled)
  Data[PPFlo=="", PPFlo:= "no_obs"]
  Data[PPVeg=="no_obs", PPFlo:="no_obs"] # If PPVeg has "no_obs" then it's the same for Flowers ( != from sterility see gap filling in NA)
  
  # change x in Dead
  Data[PPVeg=="x", PPVeg:= "Dead"]

  # rename the columns
  # colnames(Data)[which(colnames(Data)=="Plot")] <- "PlotNum"
  colnames(Data)[which(colnames(Data)=="Num_crown")] <- "CrownID"
  # add full sp name
  Data$Genus_Spec <- Data[, paste(Genus, Species, sep="_")]
  # make colums factors
  Data$PPraw <- as.factor(Data$PPraw)
  Data$PPVegraw <- as.factor(Data$PPVegraw) 
  Data$PPFlo <- as.factor(Data$PPFlo)
  Data$desynchr <- as.factor(Data$desynchr)
  Data$uncert <- as.factor(Data$uncert)
  Data$transition <- as.factor(Data$transition)
  Data$poly <- as.factor(Data$poly)
  Data$PPVeg <- as.factor(Data$PPVeg)
  
  return(Data)
}

# to test the function
# Data = data.table(Plot= "1", # with flower
#                   Num_crown = "251",
#                   Code.sp = "spa",
#                   Family= "ohoi",
#                   Genus= "ivi",
#                   Species= "kviv",
#                   Obs= "bb", 
#                   Comm= "oogog", 
#                   X23.10.2020 = "D;",
#                   X05.11.2020 = "F;",
#                   X23.11.2020 ="D?",
#                   X14.12.2020 ="F?;",
#                   X05.01.2021 = ";Fl",
#                   X18.01.2021 ="/D;",
#                   X08.02.2021 ="D/F;",
#                   X03.03.2021 ="P;",
#                   X16.03.2021 = "D/F?*F;",
#                   X06.04.2021 = "/D?*F",
#                   X06.04.2023 = "")
# test <- PrepPhase(Data)

# Data = data.table(Plot= "1", # without flower
#                   Num_crown = "251",
#                   Code.sp = "spa",
#                   Family= "ohoi",
#                   Genus= "ivi",
#                   Species= "kviv",
#                   Obs= "bb",
#                   Comm= "oogog",
#                   X23.10.2020 = "D;",
#                   X05.11.2020 = "F;",
#                   X23.11.2020 ="D?",
#                   X14.12.2020 ="F?;",
#                   X18.01.2021 ="/D;",
#                   X08.02.2021 ="D/F;",
#                   X03.03.2021 ="P;",
#                   X16.03.2021 = "D/F?*F;",
#                   X06.04.2021 = "/D?*F",
#                   X06.04.2023 = "")
# test <- PrepPhase(Data)


#############################################################################
# function PlotHeatmap
# makes a heatmap for all individual, for continuous or categorical regular time serie

PlotHeatmap <- function(Data, # gapfill data of regular time series
                        index2plot, # name of the colum that we want to plot
                        typedata, # cont if continuous, cat if categorical
                        Graphtitle, # graph title
                        ColPhas = NULL, # color of the phenophase
                        ExplVar = NULL) # explanatory variable we want to explore 
                                        #(the indiv will be sorted by this variable on the plots)
{
  Data$CrownID <- as.factor(Data$CrownID)
  
  # rename the column to use
  colnames(Data)[which(colnames(Data)==index2plot)] <- "index"
  if(typedata=="cat") {
    Data$index <- as.factor(Data$index)
  }
  
  # re-order the CrownID using ExplVar
  if (!(is.null(ExplVar))) {
    colnames(Data)[which(colnames(Data)==ExplVar)] <- "ExplVar"
    Data <- Data[order(ExplVar)]
    Data$CrownID <- ordered(Data$CrownID, 
                            levels=as.character(unique(Data$CrownID)))
  }
  
  if (is.null(ExplVar)) {
  Mygraph <-ggplot(Data, aes(x = date, y = as.factor(CrownID),
                           fill = index)) +
    geom_tile() +
    labs(title=Graphtitle, x="", y="CrownID")
  }
  
  if (!(is.null(ExplVar))) {
    Mygraph <-ggplot(Data, aes(x = date, y = as.factor(round(ExplVar, 4)),
                               fill = index)) +
      geom_tile() +
      labs(title=Graphtitle, x="", y=ExplVar)
  }
    
  Mygraph <- Mygraph + theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))+ #, vjust = 0.5, hjust=1))
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") 
  

  if(typedata=="cont") {
    Mygraph <- Mygraph + scale_fill_gradient(low = "lightgrey", high = "darkgreen")
  }
  if(typedata=="cat") {
    Mygraph <- Mygraph + scale_fill_manual(values = ColPhas)
  }
  Mygraph
}
