## different models: ACF, backbone, backbone+
## Change of beliefs agents in 1 model
## Visual presentation

Experiments_LHS <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/Experiments_LHS_1batch_exp0.csv", header=TRUE)
`1_agents_B_0` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_B_0_1batch_exp0.csv")
`1_agents_B+_0` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_B+_0_1batch_exp0.csv")
`1_agents_ACF_0_1batch_exp0_rep0` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_ACF_0_1batch_exp0_rep0.csv")
`1_agents_ACF_0_1batch_exp0_rep1` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_ACF_0_1batch_exp0_rep1.csv")
`1_agents_ACF_0_1batch_exp0_rep2` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_ACF_0_1batch_exp0_rep2.csv")
`1_agents_ACF_0_1batch_exp0_rep3` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_ACF_0_1batch_exp0_rep3.csv")
`1_agents_ACF_0_1batch_exp0_rep4` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_ACF_0_1batch_exp0_rep4.csv")
##`1_agents_3S_0` <- read.csv("~/TU Delft/Honours programme/Project/Data (q2)/1_agents_3S_0.csv")

library(ggplot2)

## Function that builds a matrix of de agents policy aim over time (no maximum amount policy has been put in yet)
matrixbuild <- function(Dataset, Agentnumber, modelname) {j = 1
## Take the right column and make it a string
datalist <- as.character(Dataset$Belieftree)
## Substitute [,], ' ' with nothing
datalist <- gsub("\\[|\\]| ","",datalist)
## Split the string into de numbers
datalist <- unlist(strsplit(datalist,","))
## Make the tree into numbers again
datalist <- as.numeric(datalist)
#agentaim <- list()
#for (i in 1:length(datalist)) {agentaim[[j]] <- datalist[(i-1):(i+28)]; i<-i+1500; j<- j+1}
#print(agentaim)
## Making a list of all the aims
aimlst <- list()
## Making a matrix of all the aims of the agents over time
for (k in 1:10){for (i in 0:499) {aimlst[[j]] <- datalist[(51*30*i)+(51*(Agentnumber-1))+(1+((k-1)*3))]; j <- j + 1}}
Aimmatrix1 <- matrix(unlist(aimlst), nrow = 10,byrow = TRUE)
#Plotting the matrix (Aim over time for 1 specific policy, in this case policy 1)
##for (i in 1:nrow(Dataset)) {if (agentnumber==Dataset$AgentID[[i]]) {aimlst[[j]]<-datalist[(i-1)*51:(i*51-21)]; j <- j + 1}}
##{if (i>1) {aimlst[[j]]<-datalist[(i-1)*30+(((policynumber-1)*3)+1)]} else {aimlst[[j]]<- datalist[(((policynumber-1)*3)+1)]}
## Making the matrix with agents on the rows and time on the columns
Aimmatrix1 <- matrix(unlist(aimlst), nrow = 500)
##Aimmatrix1 <- matrix(unlist(agentaim), nrow = 500)
#return(aimlst)
qplot(c(col(Aimmatrix1)), c(Aimmatrix1), colour = c(row(Aimmatrix1)), group = c(row(Aimmatrix1)), geom = "line",xlab="tick",ylab="Aim",main=paste("Aims of Agent",Agentnumber, "in", modelname))
return(Aimmatrix1)
#matplot(t(Aimmatrix1), type = 'l', main = paste('Aim for policy', policynumber, modelname), xlab = 'time', ylab = 'Aim')
}
test0 = matrixbuild(`1_agents_ACF_0_1batch_exp0_rep0`, 5, 'ACF')
test1 = matrixbuild(`1_agents_ACF_0_1batch_exp0_rep1`, 5, 'ACF')
test2 = matrixbuild(`1_agents_ACF_0_1batch_exp0_rep2`, 5, 'ACF')
test3 = matrixbuild(`1_agents_ACF_0_1batch_exp0_rep3`, 5, 'ACF')
test4 = matrixbuild(`1_agents_ACF_0_1batch_exp0_rep4`, 5, 'ACF')
test = data.frame(test0, test1, test2, test3, test4)
mean1 = data.frame(test0[,1], test1[,1], test2[,1], test3[,1], test4[,1])
mean2 = data.frame(test0[,2], test1[,2], test2[,2], test3[,2], test4[,2])
mean3 = data.frame(test0[,3], test1[,3], test2[,3], test3[,3], test4[,3])
mean4 = data.frame(test0[,4], test1[,4], test2[,4], test3[,4], test4[,4])
mean5 = data.frame(test0[,5], test1[,5], test2[,5], test3[,5], test4[,5])
mean6 = data.frame(test0[,6], test1[,6], test2[,6], test3[,6], test4[,6])
mean7 = data.frame(test0[,7], test1[,7], test2[,7], test3[,7], test4[,7])
mean8 = data.frame(test0[,8], test1[,8], test2[,8], test3[,8], test4[,8])
mean9 = data.frame(test0[,9], test1[,9], test2[,9], test3[,9], test4[,9])
mean10 = data.frame(test0[,10], test1[,10], test2[,10], test3[,10], test4[,10])
test <- transform(test, Min = pmin(test0,test1,test2,test3,test4), Max = pmax(test0,test1,test2,test3,test4), indx = seq_len(dim(test)[1]))
test$mean1 <- rowMeans(mean1)
test$mean2 <- rowMeans(mean2)
test$mean3 <- rowMeans(mean3)
test$mean4 <- rowMeans(mean4)
test$mean5 <- rowMeans(mean5)
test$mean6 <- rowMeans(mean6)
test$mean7 <- rowMeans(mean7)
test$mean8 <- rowMeans(mean8)
test$mean9 <- rowMeans(mean9)
test$mean10 <- rowMeans(mean10)

ggplot(test) + labs(title="Plot of agent 5 in ACF", x ="Time", y = "Aim") + 
  geom_line(aes(indx, mean1), group = 1, colour="blue") +
  geom_ribbon(aes(x = indx, ymax = Max.1, ymin = Min.1), alpha = 0.6, fill = "blue") +
  geom_line(aes(indx, mean2), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.2, ymin = Min.2), alpha = 0.6, fill = "skyblue") +
  geom_line(aes(indx, mean3), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.3, ymin = Min.3), alpha = 0.6, fill = "red") +
  geom_line(aes(indx, mean4), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.4, ymin = Min.4), alpha = 0.6, fill = "green") +
  geom_line(aes(indx, mean5), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.5, ymin = Min.5), alpha = 0.6, fill = "yellow") +
  geom_line(aes(indx, mean6), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.6, ymin = Min.6), alpha = 0.6, fill = "pink") +
  geom_line(aes(indx, mean7), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.7, ymin = Min.7), alpha = 0.6, fill = "orange") +
  geom_line(aes(indx, mean8), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.8, ymin = Min.8), alpha = 0.6, fill = "purple") +
  geom_line(aes(indx, mean9), group = 1, colour="gold") +
  geom_ribbon(aes(x = indx, ymax = Max.9, ymin = Min.9), alpha = 0.6, fill = "gold") +
  geom_line(aes(indx, mean10), group = 1) +
  geom_ribbon(aes(x = indx, ymax = Max.10, ymin = Min.10), alpha = 0.6, fill = "grey") 
  
### For the input file, structuring
input1 <- Experiments_LHS
#Eventually, only the first 16 inputs were run
input1 <- input1[1:16,]
colnames(input1) <- c("belieftreeprofile(0,1,2,3)","Affiliation1 weight","Affiliation 2 weight","Affiliation 3 weight", "PAA1(25-50)","PAA2(25,50)","EIOP(0.01-0.1)","RPP(1-10)","RWA(0.05,20)","TDC(0.01,0.1)","CLC1(0.7-0.8)","CLC2(0.8-0.9)","CLC3(0.9-1.0)","Team Gap Belief(0.6-1.0)","Team Gap CloseProb(0.2-0.7)","Team Gap ClosePol(0.2-0.7)","ACF Thres(0.15-0.55)")

## Using the correct column
belieftree3s <- `1_agents_3S_0`$Belieftree_policy
## Turn into a character string
belieftree3s <- as.character(belieftree3s)
## Substitute [,], ' ' with nothing
belieftree3s <- gsub("\\[|\\]| ","",belieftree3s)
## Split the string into de numbers
belieftree3s <- unlist(strsplit(belieftree3s, ","))
## Make the tree into numbers again
belieftree3s <- as.numeric(belieftree3s)

## Making a list of all the aims (this still works with a specific data-set and numbers have to be changed for different policies)
aimlst <- list()
j = 1
for (i in 1:nrow(`1_agents_3S_0`)) {if (i>1) {aimlst[[j]]<-belieftree3s[(i-1)*30+1]} else {aimlst[[j]]<- belieftree3s[1]}; j <- j + 1}
## Making the matrix with agents on the rows and time on the columns 
Aimmatrix1 <- matrix(unlist(aimlst), nrow = 30)
return(Aimmatrix1)
#return(aimlst)
##matplot(t(Aimmatrix1), type = 'l', xlab = 'time', ylab = 'Aim')
##title('Aim for policy 1 3S over time per agent')

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
pl0 = matrixbuild(`1_agents_B+_0`, 5, 1, 'B+')
pl1 = matrixbuild(`1_agents_3S_0`, 5, 1, '3S')
pl2 = matrixbuild(`1_agents_ACF_0_1batch_exp0_rep0`, 5, 1, 'ACF')
df = data.frame(unlist(pl0), unlist(pl1), unlist(pl2))
df$time = 1:length(df$X1)
pl0 = ggplot(df, aes(x = time, y= unlist.pl0.)) + geom_line() + ylim(-1,1)
pl1 = ggplot(df, aes(x = time, y= unlist.pl1.)) + geom_line() + ylim(-1,1)
pl2 = ggplot(df, aes(x = time, y= unlist.pl2.)) + geom_line() + ylim(-1,1)
plt = multiplot(pl0, pl1, pl2, cols=3)