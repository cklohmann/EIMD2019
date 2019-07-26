## script to creat plot for NIX 
## create prev v year and severity v year plot 
## July 16 2019
# read in qPCR data
qPCR <- read.csv("Downloads/NIX qPCR FHL for R.csv")
## read in sample data
samples <- read.csv("Downloads/NIX FHL sample data for R.csv")
## merge two datasets into one using the "ID" column
master <- merge(qPCR,samples,"ID")
## install plyr package for ddply 
### create new column for prevalence 
master$infected <- ifelse(master$AverageSQ_1ul > 0, 1, 0)
master$SamplingMethod <- ifelse(master$SamplingMethod == "stock assessment","stock assessment", 
                                 ifelse(master$SamplingMethod == "moribund", "moribund", "stock assessment")
                                 )
### create new column for DNA/MG
master$DNApMG <- (master$AverageSQ_1ul*200)/master$Gill_Tissue_Weight_mg
# summarize data (prevalnce/year)
master_summary <- ddply(master, .(Year,SamplingMethod),summarise,Prevalence = sum(infected)/length(infected), Intensity = sum(DNApMG)/sum(infected))

## plot data using ggplot
## scatter plot 
ggplot(master_summary, aes(Year,Prevalence)) + geom_point()
ggplot(master_summary, aes(Year,Prevalence)) + geom_line(colour = "orange")													
ggplot(master_summary, aes(Year,Intensity)) + geom_point(shape = 23, colour = "red")
ggplot(master_summary, aes(Prevalence,Intensity)) + geom_point()

#plot 2017 based on simpling method
ggplot(master_summary, aes(Year, Prevalence, colour = SamplingMethod)) + geom_point()


# plotting with two y-axis
#ggplot(mpg, aes(displ, hwy)) + 
#geom_point() + 
# scale_y_continuous(
#    "mpg (US)", 
#    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
 # )
# right now it only plots  prevalence, not intensity, hmm...
ggplot(master_summary, aes(Year, Prevalence, Intensity)) +
  geom_point() +
  scale_y_continuous(
    "Prevalence",
    sec.axis = sec_axis(~. *1600000, name = "Intensity"))



