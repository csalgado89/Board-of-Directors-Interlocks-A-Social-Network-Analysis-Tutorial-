#packages and libraries that have to be installed before running the code
if (!require(GetFREData)) install.packages("GetFREData")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(igraph)) install.packages("igraph")
if (!require(ggpubr)) install.packages("ggpubr")
if (!require(data.table)) install.packages("data.table")
if (!require(dplyr)) install.packages("dplyr")
if (!require(stringr)) install.packages("stringr")
if (!require(sjmisc)) install.packages("sjmisc")
if (!require(stringdist)) install.packages("stringdist")
if (!require(sna)) install.packages("sna")
if (!require(keyplayer)) install.packages("keyplayer")

library(GetFREData)
library(tidyverse)
library(igraph)
library(ggpubr)
library(data.table)
library(dplyr)
library(stringr)
library(sjmisc)
library(stringdist)
library(sna)
library(keyplayer)


#Collecting and treating Brazilian companies' data (#Step1 to #Step5)

#Step 1 - Download data
##Command to input the period of data collection. It can last few days to collect all data, depending on the sample size. 
##As a suggestion, we recommend extract year by year or at the most two years at a time, like our example:
df_info <- get_info_companies()
print(df_info)
df_info <- df_info[df_info$CD_CVM != 13773,]
df_info <- df_info[df_info$CD_CVM != 11762,]
l_fre <- get_fre_data(companies_cvm_codes = df_info$CD_CVM ,
                      fre_to_read = 'last',
                      first_year = 2019,
                      last_year = 2020
                      )

#Step 2 - Shortcut to select data by rds file
##Shortcut to be used in case of the reader want to test the program or does not need the current data. Choose de 'rds' file
##that is available with this code

l_fre <- readRDS(file.choose())

#Step 3 - select info of boards
##Selecting the type.job to get the data from the board of directors. We use the following codes:
##20 - Chairman of the Board of Directors
##21 - Vice President Board of Directors 
##22 - Director (effective)  
##24 - Chairman of the Board of Directors (Independent) 
##25 - Vice President Board of Directors (Independent) 
##27 - Outsider director (effective) 
##29 - Other directors 
##30 - Chairman of the Board of Directors and CEO 
##31 - Vice President Board of Directors and e CEO 
##32 - Vice President Board of Directors and Vice CEO 
##33 - Director (effective) and CEO 
##34 - Director (effective) and Vice President Director 
##35 - Director (effective) and Investor Relations Officer 
##39 - Other directors/executives 

##Creating "Table_Directors" with directors data selected.
board_tmp <- data.table(l_fre$df_board_composition)
Table_Directors <- board_tmp[code.type.job %in% c('20','21', '22', '24','25','27','29', '30','31','32', '33', '34', '35','39'),
                           c('CNPJ_CIA','DENOM_CIA','DT_REFER','CD_CVM','person.name','person.cpf')
                          ]

#Step4 - Handling data id and names
##Handling non-structured data already collected from FRE.
##Correcting the number of digits of CPF:
Table_Directors$person.cpf <- str_pad(Table_Directors$person.cpf, 11, pad = "0")
tmp_data <- Table_Directors
##Correcting and standardizing names and spelling.
tmp_data$name_adjust <- tmp_data$person.name
tmp_data$name_adjust <- iconv(tmp_data$name_adjust,"UTF-8", "ASCII//TRANSLIT")
tmp_data$name_adjust = str_replace_all(tmp_data$name_adjust, "[[:punct:]]", " ")
##changing the names to be in lowercase
tmp_data$name_adjust = tolower(tmp_data$name_adjust)
#Removing leading and/or trailing whitespace from character strings.
tmp_data$name_adjust = trimws(tmp_data$name_adjust, which = c("both"))
#Removing comma, slash,  
tmp_data$name_adjust = gsub(',', '',tmp_data$name_adjust)
tmp_data$name_adjust = gsub('-', '',tmp_data$name_adjust)
tmp_data$name_adjust = gsub('  ', ' ',tmp_data$name_adjust)
tmp_data$name_adjust = gsub('/', ' ',tmp_data$name_adjust)
#Linking the names with underline 
tmp_data$name_adjust = gsub(' ', '_',tmp_data$name_adjust)

#Step5 - Handling data and few adjustments
##Checking if there are directors without CPF code.
##Creating codes for the directors without CPF.
tmp <- data.frame(unique(tmp_data[,c('person.cpf','name_adjust')]))
with_CPF <- tmp %>% filter(person.cpf %in% c('0', '99999999999', NULL) == FALSE & !is.na(person.cpf)) 
without_CPF <- tmp %>% filter(person.cpf %in% c('0', '99999999999', NULL) | is.na(person.cpf)) 
without_CPF$person.cpf <- without_CPF$Match <- without_CPF$person.Match_name <- NA
for (i in 1:length(without_CPF$name_adjust)){
  gl = 0
  if (str_contains(without_CPF$name_adjust[i], '.') || str_contains(without_CPF$name_adjust[i],c('neto', 'junior', 'filho'), logic = 'or')){gl = gl + 3}
  gl = ifelse((nchar(without_CPF$name_adjust[i]) < 10), (gl + 1), (gl + 1 + round((nchar(without_CPF$name_adjust[i])-10)/5)))
  
  n <- amatch(without_CPF$name_adjust[i],with_CPF$name_adjust,maxDist=gl)
  if(is.na(n)){
    without_CPF$person.cpf[i] <- as.character(i)
    while (nchar(without_CPF$person.cpf[i]) < 9) {
      without_CPF$person.cpf[i] <-paste("0",without_CPF$person.cpf[i],sep = '')
    }
    without_CPF$person.cpf[i] <- paste("PF",without_CPF$person.cpf[i],sep='')
    #acrescenta na base atual esse novo nome com código criado
    tmp <- c(without_CPF$person.cpf[i],without_CPF$name_adjust[i])
    with_CPF <- rbind(with_CPF,tmp)
  }else{
    without_CPF$Match[i] <- 1
    without_CPF$person.Match_name[i] <- with_CPF$name_adjust[n]
    without_CPF$person.cpf[i] <- with_CPF$person.cpf[n]
  }
  without_CPF$name_adjust[i]
  with_CPF$name_adjust[n]
}

tmp_CPF <- without_CPF %>% filter(Match == 1) %>% select(name_adjust, person.cpf)
with_CPF <- unique(rbind(with_CPF, tmp_CPF[,1:2]))


###############################

#Creating the adjacency matrix using R Studio (#Step6 and #Step7)

#Step6 - create adjacent matrix and create Network Graphs
##Creating the matrix to identify interlocked companies.
Tab_Companies_Directors <- inner_join(tmp_data,with_CPF, by = 'name_adjust')

##We create a separated matrix for each year. If your sample has more than two years, 
#copy and paste the same code will be necessary, changing the years.

#Creating the adjacency matrix for 2019
t2019 <- Tab_Companies_Directors %>%
  filter(substr(Tab_Companies_Directors$DT_REFER,1,4) == '2019') %>%
  rename(CPF = person.cpf.y) %>%
  select(CD_CVM, CPF)
t2019 <- table(t2019$CPF, t2019$CD_CVM)
t2019[t2019 >1] <- 1
matrix_2019 = t(t2019) %*% t2019
diag(matrix_2019) <- 0

c <- c()
for (z in 1:ncol(matrix_2019)){
  if (sum(matrix_2019[z,])==0){
    c <- c(c,z)
  }
}

matrix_2019_connected <- matrix_2019[-c, -c]
Network_2019 <- graph_from_adjacency_matrix(matrix_2019,mode = 'undirected', diag = FALSE)
Network_2019_connected <- graph_from_adjacency_matrix(matrix_2019_connected,mode = 'undirected', diag = FALSE)

##"Network_2019" comprises all of the Brazilian companies of our sample
##"Network_2019_connected" comprises only the companies that shared at least one director in 2019.
##Depending of the visualization/analysis objective, you can use one or another to create the plots.


#Creating the adjacency matrix for 2020
t2020 <- Tab_Companies_Directors %>%
  filter(substr(Tab_Companies_Directors$DT_REFER,1,4) == '2020') %>%
  rename(CPF = person.cpf.y) %>%
  select(CD_CVM, CPF)
t2020 <- table(t2020$CPF, t2020$CD_CVM)
t2020[t2020 >1] <- 1
matrix_2020 = t(t2020) %*% t2020
diag(matrix_2020) <- 0

c <- c()
for (z in 1:ncol(matrix_2020)){
  if (sum(matrix_2020[z,])==0){
    c <- c(c,z)
  }
}

matrix_2020_connected <- matrix_2020[-c, -c]
Network_2020 <- graph_from_adjacency_matrix(matrix_2020,mode = 'undirected', diag = FALSE)
Network_2020_connected <- graph_from_adjacency_matrix(matrix_2020_connected,mode = 'undirected', diag = FALSE)

##"Network_2020" comprises all of the Brazilian companies of our sample
##"Network_2020_connected" comprises only the companies that shared at least one director in 2019.
##Depending of the visualization/analysis objective, you can use one or another to create the plots.

#plot network graphs
##Plotting the general graphs to observe the relations.
windows()
par(mfrow=c(1,2))
  plot(Network_2019_connected, vertex.label= NA,  main=("2019"), vertex.color="orange",
       vertex.size=5,edge.width = 1, layout=layout_nicely)
  plot(Network_2020_connected, vertex.label= NA,  main=("2020"), vertex.color="green",
       vertex.size=5,edge.width = 1, layout=layout_nicely)

#In this example, we created plots considering only companies that have at least 1 interlock. 
#To reach the network comprising companies with no connections, it is necessary to consider the code below with the matrix that
#comprises all companies. You can also change the size of vertex and edges.
  
#plot(Network_2019, vertex.label= NA,  main=("2019"), vertex.color="orange",
#       vertex.size=5,edge.width = 1, layout=layout_nicely)
#plot(Network_2020, vertex.label= NA,  main=("2020"), vertex.color="green",
#       vertex.size=5,edge.width = 1, layout=layout_nicely)  
  
#Step7 - Exporting the complete matrix to CSV format.
dir <- choose.dir(default = "", caption = "Select folder to save .csv for each matrix")
setwd(dir)
write.csv(matrix_2019, "matrix_2019.csv",col.names = TRUE, row.names = TRUE, sep=";")
write.csv(matrix_2020, "matrix_2020.csv",col.names = TRUE, row.names = TRUE, sep=";")

###############################

#Visualizing and measuring board interlocks using R Studio (#Step8 and #Step9)  

#Step8 - Network Measures tables

##Creating tables with the measures of the networks. 
##This data can be used to analyze the different measures, and as variables in the econometric models.

##Key concepts:
##Density: shows the ratio between the number of connections and all the possible connections in this network.
##Degree centrality is the number of connections. It shows how connected are the actors.
##Betweenness: Shows how the actors connect other actors in the network.
##Closeness: Shows how close the actors are in the network.
##Connectedness: Connectedness is the proportion between pairs of nodes that can reach each other by a certain path, 
##Fragmentation: is precisely the opposite of connectedness, that is the ratio between pairs of unreachable nodes (Borgatti et al., 2013).

Tab_Compare <- data.frame(
  Year = rbind('2019', '2020'),
  Density = rbind(
    round(gden(matrix_2019_connected, diag = FALSE, mode = 'graph',ignore.eval = FALSE),4), 
    round(gden(matrix_2020_connected, diag = FALSE, mode = 'graph',ignore.eval = FALSE),4)
  ),
  Degree = rbind(
    round(centralization(matrix_2019_connected, degree,mode='graph',ignore.eval = FALSE),4),
    round(centralization(matrix_2020_connected, degree,mode='graph',ignore.eval = FALSE),4)
  ),
  Betweenness = rbind(
    round(centralization(matrix_2019_connected, betweenness,ignore.eval = FALSE, cmode = "undirected"),4),
    round(centralization(matrix_2020_connected, betweenness,ignore.eval = FALSE, cmode = "undirected"),4)
  ),
  Closeness = rbind(
    round(centralization(matrix_2019_connected, closeness,mode='graph', cmode = "suminvundir", ignore.eval = FALSE),4), 
    round(centralization(matrix_2020_connected, closeness,mode='graph', cmode = "suminvundir", ignore.eval = FALSE),4)
  ),
  Fragmentation = rbind(
    round(max(fragment(matrix_2019_connected)),4),
    round(max(fragment(matrix_2020_connected)),4)
  ),
  Connectedness = rbind(
    round(connectedness(matrix_2019_connected),4),
    round(connectedness(matrix_2020_connected),4)
  ),
  Components = rbind(
    components(matrix_2019_connected),
    components(matrix_2020_connected)
  ) 
)

#Creating tables for each year with the measures of the networks, for each year of the sample: 
Tab_2019 <- data.frame(
  Firm           = str_pad(as_ids(V(Network_2019_connected)),nchar(tail(as_ids(V(Network_2019)),1)),pad='0'),
  Degree         = round(degree(matrix_2019_connected, ignore.eval = FALSE, gmode='graph'),1),
  Betweenness    = round(betweenness(matrix_2019_connected, ignore.eval = FALSE, cmode = "undirected"),2),
  Closeness      = round(closeness(matrix_2019_connected, ignore.eval = FALSE, cmode = "suminvundir", gmode="graph"),4),
  Fragmentation  = as.vector(round(fragment(matrix_2019_connected),4)),
  Component      = round(component.dist(matrix_2019_connected)$membership,1)
)

Tab_2020 <- data.frame(
  Firm           = str_pad(as_ids(V(Network_2020_connected)),nchar(tail(as_ids(V(Network_2019)),1)),pad='0'),
  Degree         = round(degree(matrix_2020_connected, ignore.eval = FALSE, gmode='graph'),1),
  Betweenness    = round(betweenness(matrix_2020_connected, ignore.eval = FALSE, cmode = "undirected"),2),
  Closeness      = round(closeness(matrix_2020_connected, ignore.eval = FALSE, cmode = "suminvundir", gmode="graph"),4),
  Fragmentation  = as.vector(round(fragment(matrix_2020_connected),4)),
  Component      = round(component.dist(matrix_2020_connected)$membership,1)
)

#Saving the .csv with the tables of all measures created.
dir <- choose.dir(default = "", caption = "Select folder to save .csv for each matrix")
setwd(dir)
write.csv(Tab_2019, "Tab_2019.csv",col.names = TRUE, row.names = TRUE, sep=";")
write.csv(Tab_2020, "Tab_2020.csv",col.names = TRUE, row.names = TRUE, sep=";")
write.csv(Tab_Compare, "Tab_Compare.csv",col.names = TRUE, row.names = TRUE, sep=";")

#Step9 - Descriptive statistics
##descriptive statistics for all sample
##joining the data from each year of our sample
Tab_2019$ref_year <- '2019'
Tab_2020$ref_year <- '2020'
Tab_All_Sample <- rbind(Tab_2019,Tab_2020)
Tab_All_Sample_n <- Filter(is.numeric, Tab_All_Sample)

#creating the descriptive statistics
#creating means, median, min, max, and quartiles and standard deviation.
Means <- (transpose(summarise_all(Tab_All_Sample_n,mean)))
Median <- transpose(summarise_all(Tab_All_Sample_n,median))
Min <- transpose(summarise_all(Tab_All_Sample_n,min))
Max <- transpose(summarise_all(Tab_All_Sample_n,max))
Q1 <- transpose(summarize_all(Tab_All_Sample_n, ~ quantile(.x, probs = 0.25)))
Q2 <- transpose(summarize_all(Tab_All_Sample_n, ~ quantile(.x, probs = 0.5)))
Q3 <- transpose(summarize_all(Tab_All_Sample_n, ~ quantile(.x, probs = 0.75)))
SD <- transpose(summarise_all(Tab_All_Sample_n,sd))

variable_name <- colnames(Tab_All_Sample_n) 
table_descriptive_All <- cbind(variable_name, Means, Median, Min, Max, Q1, Q2, Q3, SD)
colnames(table_descriptive_All) <-  c('Variables', 'Means', 'Median', 'Min', 'Max', 'Q1', 'Q2', 'Q3', 'SD')

##Choosing diretory to save the csv. file with descriptive statistics
setwd(choose.dir())
##Creating .csv file
write.csv(table_descriptive_All,"Descriptive_Statistics.csv", 
          col.names = TRUE, row.names = TRUE, sep=";")

#Step10 - Network measures graphs
##Creating the graphs for each measure created before.

dev.off()
windows()
par(mfrow=c(1,2))

#degree centrality network graph for each year.
#You can change graphs' attributes according your preferences. 
#For more tips, you can check https://cran.r-project.org/web/packages/igraph/index.html

plot(Network_2019,
     vertex.label.cex = .6, 
     vertex.label= NA,
     vertex.size = Tab_2019$Degree,
     vertex.color = 'orange',
     main = '2019')

plot(Network_2020,
     vertex.label.cex = .6, 
     vertex.label= NA,
     vertex.size = Tab_2020$Degree,
     vertex.color = 'green',
     main = '2020')

#degree centrality histogram
dev.off()
windows()
par(mfrow=c(1,2))
hist(Tab_2019$Degree, xlab="Degree", xlim=c(0,35), ylim=c(0,300), main="2019", col = "orange")
hist(Tab_2020$Degree, xlab="Degree", xlim=c(0,35), ylim=c(0,300), main="2020", col = "green")


#betweness centrality network graphs for each year.
dev.off()
windows()
par(mfrow=c(1,2))
plot(Network_2019,
     vertex.label.cex = .6, 
     vertex.label= NA,
     vertex.size = Tab_2019$Betweenness/max(Tab_2019$Betweenness)*30,
     vertex.color = 'orange',
     main = '2019')

plot(Network_2020,
     vertex.label.cex = .6, 
     vertex.label= NA,
     vertex.size = Tab_2020$Betweenness/max(Tab_2020$Betweenness)*30,
     vertex.color = 'green',
     main = '2020')

#closeness centrality network graphs for each year.
dev.off()
windows()
par(mfrow=c(1,2))
plot(Network_2019,
     vertex.label.cex = .6, 
     vertex.label= NA,
     vertex.size = Tab_2019$Closeness/max(Tab_2019$Closeness)*15,
     vertex.color = 'orange',
     main = '2019')

plot(Network_2020,
     vertex.label.cex = .6, 
     vertex.label= NA,
     vertex.size = Tab_2020$Closeness/max(Tab_2020$Closeness)*10,
     vertex.color = 'green',
     main = '2020')

