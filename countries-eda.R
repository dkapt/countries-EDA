#######
####
getwd()
setwd("C:\\Users\\kapet\\Desktop\\ΜΑΘΗΜΑΤΑ\\4ο Έτος\\Η' Εξάμηνο\\Εξόρυξη & Ανάλυση Δεδομένων Μεγάλου Όγκου")

dt_st1 <- read.csv("ergasia\\Country-data.csv")
dt_st2 <- read.csv("ergasia\\DevelopedCountriesList.csv")
dt_merged <- merge(dt_st1, dt_st2, by = "country")
DATA <- dt_merged[,-11]
DATA$state2 <- as.factor(ifelse(DATA$hdi < 0.8, "Developing", "Developed"))

DATA$state3 <- as.factor(ifelse(DATA$hdi > 0.8, "Developed",
                         ifelse(DATA$hdi > 0.5, "Developing", "Under-Developed")))

DATA$state5 <- as.factor(ifelse(DATA$hdi > 0.8, "self-sufficient",
                         ifelse(DATA$hdi > 0.65, "ok",
                         ifelse(DATA$hdi > 0.55, "maybe in need",
                         ifelse(DATA$hdi > 0.45, "probably need aid", "definately need aid")))))
  ## counting ##
#sum(DATA$state5=="self-sufficient")
#sum(DATA$state5=="ok")
#sum(DATA$state5=="maybe in need")
#sum(DATA$state5=="probably need aid")
#sum(DATA$state5=="definately need aid")

data <- DATA[,-(11:12)]
# data <- DATA[,-(11)]      with population.

View(data)
head(data)
tail(data)
str(data)
library("ggplot2")                     # Load ggplot2 package
library("GGally")                    # Load GGally package [extention to ggplot2]
ggcorr(data, palette = "RdBu", label = TRUE, label_alpha = TRUE)
#ggcorr(data, palette = "RdGy", label = TRUE, nbreaks = 4)
#ggcorr(data, palette = "RdGy", nbreaks = 4, geom = "circle")
#ggcorr(data, geom = "blank", label = TRUE, hjust = 0.75) +
  #geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  #scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  #guides(color = FALSE, alpha = FALSE)

library(DataExplorer)  #  EDA - package
# Checking if there is missing values in our data frame
plot_missing(
  data, 
  ggtheme = theme_minimal(), # theme_pubclean()
  title = "Missing Values"
)

library(tidyr)
# Histogram for each Attribute
data %>%
  gather(Attributes, value, 2:10) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Country Data - Histograms") +
  theme_bw()


#                       NORTH AMERICA
data$Continent[   data$country == "Anguilla" |
                  data$country == "Antigua and Barbuda" |
                  data$country == "Aruba" |
                  data$country == "Bahamas" |
                  data$country == "Barbados" |
                  data$country == "Belize" |
                  data$country == "Bermuda" |
                  data$country == "Bonaire" |
                  data$country == "British Virgin Islands" |
                  data$country == "Canada" |
                  data$country == "Cayman Islands" |
                  data$country == "Cliperton Island" |
                  data$country == "Costa Rica" |
                  data$country == "Cuba" |
                  data$country == "Curacao" |
                  data$country == "Dominica" |
                  data$country == "Dominican Republic" |
                  data$country == "El Salvador" | 
                  data$country == "Venezuela" |
                  data$country == "Greenland" |
                  data$country == "Grenada" |
                  data$country == "Guadeloupe" |
                  data$country == "Guatemala" | 
                  data$country == "Haiti" |
                  data$country == "Honduras" |
                  data$country == "Jamaica" |
                  data$country == "Martinique" |
                  data$country == "Mexico" | 
                  data$country == "Montseratt" |
                  data$country == "Nicaragua" |
                  data$country == "Panama" |
                  data$country == "Puerto Rico" |
                  data$country == "Saba" | 
                  data$country == "San Andres and Providencia" |
                  data$country == "Saint Barthelemy" |
                  data$country == "Saint Kitts and Nevis" |
                  data$country == "Saint Lucia" |
                  data$country == "Saint Martin" | 
                  data$country == "Saint Pierre Miquelon" |
                  data$country == "St. Vincent and the Grenadines" |
                  data$country == "Sint Eustatius" |
                  data$country == "Trinidad and Tobago" |
                  data$country == "Turks and Caicos Islands" | 
                  data$country == "United States" |
                  data$country == "US Virgin Islands"
          
                ] <- "North America"


#                        SOUTH AMERICA
data$Continent[     data$country == "Brazil" |
                    data$country == "Uruguay" |
                    data$country == "Paraguay" |
                    data$country == "Argentina" |
                    data$country == "Chile" |
                    data$country == "Bolivia" |
                    data$country == "Peru" |
                    data$country == "Ecuador" |
                    data$country == "Colombia" |
                    data$country == "Venezuela" |
                    data$country == "Guyana" |
                    data$country == "Suriname" |
                    data$country == "French Guiana" |
                    data$country == "Falkland Islands" |
                    data$country == "South Georgia and the South Sandwitch Islands"
                                                      
              ] <- "South America"


#                            EUROPE
data$Continent[     data$country == "Albania" |
                    data$country == "Andorra" |
                    data$country == "Austria" |
                    data$country == "Belarus" |
                    data$country == "Belgium" |
                    data$country == "Bosnia and Herzegovina" |
                    data$country == "Bulgaria" |
                    data$country == "Croatia" |
                    data$country == "Cyprus" |
                    data$country == "Czech Republic" |
                    data$country == "Denmark" |
                    data$country == "Estonia" |
                    data$country == "Finland" |
                    data$country == "France" |
                    data$country == "Germany" |
                    data$country == "Greece" |
                    data$country == "Hungary" |
                    data$country == "Iceland" |
                    data$country == "Ireland" |
                    data$country == "Italy" |
                    data$country == "Latvia" |
                    data$country == "Liechtestein" |
                    data$country == "Lithuania" |
                    data$country == "Luxembourg" |
                    data$country == "Malta" |
                    data$country == "Moldova" |
                    data$country == "Montenegro" |
                    data$country == "Netherlands" |
                    data$country == "Macedonia, FYR" |
                    data$country == "Norway" |
                    data$country == "Poland" |
                    data$country == "Portugal" |
                    data$country == "Romania" |
                    data$country == "San Marino" |
                    data$country == "Serbia" |
                    data$country == "Slovak Republic" |
                    data$country == "Slovenia" |
                    data$country == "Spain" |
                    data$country == "Sweden" |
                    data$country == "Switzerland" |
                    data$country == "Ukraine" |
                    data$country == "United Kingdom" |
                    data$country == "Vatican City"
                    
                ] <- "Europe"


#                            AFRICA
data$Continent[     data$country == "Egypt" |
                    data$country == "Libya" |
                    data$country == "Tunisia" |
                    data$country == "Algeria" |
                    data$country == "Morocco" |
                    data$country == "Western Sahara" |
                    data$country == "Mauritania" |
                    data$country == "Mali" |
                    data$country == "Senegal" |
                    data$country == "Gambia" |
                    data$country == "Cape Verde" |
                    data$country == "Guinea-Bissau" |
                    data$country == "Guinea" |
                    data$country == "Sierra Leone" |
                    data$country == "Liberia" |
                    data$country == "Cote d'Ivoire" |
                    data$country == "Burkina Faso" |
                    data$country == "Ghana" |
                    data$country == "Togo" |
                    data$country == "Benin" |
                    data$country == "Nigeria" |
                    data$country == "Niger" |
                    data$country == "Chad" |
                    data$country == "Sudan" |
                    data$country == "Eritrea" |
                    data$country == "Djibouti" |
                    data$country == "Ethiopia" |
                    data$country == "Somalia" |
                    data$country == "Kenya" |
                    data$country == "Uganda" |
                    data$country == "Rwanda" |
                    data$country == "Burundi" |
                    data$country == "Congo, Dem. Rep." |
                    data$country == "Congo, Rep." |
                    data$country == "Central African Republic" |
                    data$country == "Cameroon" |
                    data$country == "Equatorial Guinea" |
                    data$country == "Gabon" |
                    data$country == "Angola" |
                    data$country == "Zambia" |
                    data$country == "Tanzania" |
                    data$country == "Malawi" |
                    data$country == "Mozambique" |
                    data$country == "Comoros" |
                    data$country == "Madagascar" |
                    data$country == "Zimbabwe" |
                    data$country == "Botswana" |
                    data$country == "Namibia" |
                    data$country == "South Africa"|
                    data$country == "Lesotho" |
                    data$country == "Eswatini" |
                    data$country == "Mauritius" |
                    data$country == "Seychelles"
                    
                ] <- "Africa"


#                         ASIA
data$Continent[     data$country == "Turkey" |
                    data$country == "Georgia" |
                    data$country == "Armenia" |
                    data$country == "Azerbaijan" |
                    data$country == "Syria" |
                    data$country == "Iraq" |
                    data$country == "Lebanon" |
                    data$country == "Israel" |
                    data$country == "Palestine" |
                    data$country == "Jordan" |
                    data$country == "Saudi Arabia" |
                    data$country == "Kuwait" |
                    data$country == "Bahrain" |
                    data$country == "Qatar" |
                    data$country == "United Arab Emirates" |
                    data$country == "Oman" |
                    data$country == "Yemen" |
                    data$country == "Iran" |
                    data$country == "Afghanistan" |
                    data$country == "Pakistan" |
                    data$country == "India" |
                    data$country == "Nepal" |
                    data$country == "Bhutan" |
                    data$country == "Bangladesh" |
                    data$country == "Sri Lanka" |
                    data$country == "Maldives" |
                    data$country == "Myanmar" |
                    data$country == "Lao" |
                    data$country == "Vietnam" |
                    data$country == "Cambodia" |
                    data$country == "Thailand" |
                    data$country == "Malaysia" |
                    data$country == "Singapore" |
                    data$country == "Brunei" |
                    data$country == "Philippines" |
                    data$country == "Indonesia" |
                    data$country == "Timor-Leste" |
                    data$country == "Taiwan" |
                    data$country == "Japan" |
                    data$country == "South Korea" |
                    data$country == "North Korea" |
                    data$country == "China" |
                    data$country == "Mongolia" |
                    data$country == "Russia" |
                    data$country == "Kazakhstan" |
                    data$country == "Kyrgyz Republic" |
                    data$country == "Tajikistan" |
                    data$country == "Uzbekistan" |
                    data$country == "Turkmenistan"
                      
              ] <- "Asia"


#                        OCEANIA
data$Continent[     data$country == "Australia" |
                    data$country == "Papua New Guinea" |
                    data$country == "New Zealand" |
                    data$country == "Fiji" |
                    data$country == "Solomon Islands" |
                    data$country == "Micronesia, Fed. Sts." |
                    data$country == "Vanuatu" |
                    data$country == "Samoa" |
                    data$country == "Kiribati" |
                    data$country == "Tonga" |
                    data$country == "Marshall Islands" |
                    data$country == "Palau" |
                    data$country == "Tuvalu" |
                    data$country == "Nauru"
                  
              ]  <- "Oceania"


#data$Continent$color













#
#data$country == "" |
 # data$country == "" |
  #data$country == "" |
  #data$country == "" |
#  data$country == "" |
 # data$country == "" |
  #data$country == "" |
  #data$country == "" |
  #data$country == "" |
  #data$country == "" |
  #data$country == "" |
  #data$country == "" |
  #data$country == "" |
  #data$country == "" |
  #data$country == "" 

#d <- data[,-11]
d <- data[,-(11:12)]
  ggpairs(d[,-1], aes(colour = Continent, alpha = 0.4))
ggpairs(d[,-1], aes(colour = data$state5, alpha = 0.2), upper = list(continuous = wrap("cor", size = 1.75))) + #1.75 - 2.5
    theme(axis.text = element_text(size = 4.5))
  
d <- data[,-(11:14)]
ggpairs(d[,-1], aes(colour = data$state2, alpha = 0.2), upper = list(continuous = wrap("cor", size = 2.5))) + #1.75 - 2.5
   theme(axis.text = element_text(size = 4.5))
ggpairs(d[,-1], aes(colour = data$state3, alpha = 0.2), upper = list(continuous = wrap("cor", size = 2.5))) + #1.75 - 2.5
  theme(axis.text = element_text(size = 4.5))
ggpairs(d[,-1], aes(colour = data$state5, alpha = 0.2), upper = list(continuous = wrap("cor", size = 2.5))) + #1.75 - 2.5
  theme(axis.text = element_text(size = 4.5))

#ggpairs(data[,-1], upper = "blank")

h <- ggplot(data, aes(Continent)) + geom_bar(alpha=0.65, aes(fill=state5))
print(h)

#density <- ggplot(data, aes(income, fill=Continent)) + geom_density(alpha=0.6, position = "fill")  # "stack"
density_income <- ggplot(data, aes(income, colour=Continent)) + geom_density(alpha=0.25, aes(fill=Continent))
print(density_income)
boxplot_incomeA <- ggplot(data, aes(x = state5, y = income, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
boxplot_incomeB <- ggplot(data, aes(x = Continent, y = income, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
grid.arrange(boxplot_incomeA, boxplot_incomeB, ncol=2)

density_lfexp <- ggplot(data, aes(life_expec, colour=Continent)) + geom_density(alpha=0.25, aes(fill=Continent))
print(density_lfexp)
boxplot_lfexpA <- ggplot(data, aes(x = state5, y = life_expec, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
boxplot_lfexpB <- ggplot(data, aes(x = Continent, y = life_expec, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
grid.arrange(boxplot_lfexpA, boxplot_lfexpB, ncol=2)

density_tfert <- ggplot(data, aes(total_fer, colour=Continent)) + geom_density(alpha=0.25, aes(fill=Continent))
print(density_tfert)
boxplot_tfertA <- ggplot(data, aes(x = state5, y = total_fer, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
boxplot_tfertB <- ggplot(data, aes(x = Continent, y = total_fer, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
grid.arrange(boxplot_tfertA, boxplot_tfertB, ncol=2)

density_chmort <- ggplot(data, aes(child_mort, colour=Continent)) + geom_density(alpha=0.25, aes(fill=Continent))
print(density_chmort)
boxplot_chmortA <- ggplot(data, aes(x = state5, y = child_mort, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
boxplot_chmortB <- ggplot(data, aes(x = Continent, y = child_mort, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
grid.arrange(boxplot_chmortA, boxplot_chmortB, ncol=2)

density_health <- ggplot(data, aes(health, colour=Continent)) + geom_density(alpha=0.25, aes(fill=Continent))
print(density_health)
boxplot_healthA <- ggplot(data, aes(x = state5, y = health, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
boxplot_healthB <- ggplot(data, aes(x = Continent, y = health, fill = state5)) + geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 8,size = 2, color = "darkred") +theme(axis.title.y = element_text(size = 18))
grid.arrange(boxplot_healthA, boxplot_healthB, ncol=2)
#
# geom_point()# Varying alpha is useful for large datasets

# income  ~  gdpp  = [0.9]
c_0.9 <- ggplot(data, aes(income, gdpp, colour=state5)) + 
  geom_point(alpha = 0.2, size=1) +
    geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )
print(c_0.9)

# child mortality  ~  total fer.   =   [0.8]
c_0.8 <- ggplot(data, aes(child_mort, total_fer, colour=state5)) +
  geom_point() +
    geom_text( label=data$country,nudge_x=0.45, nudge_y=0.1, size=1.5, family="serif" )
print(c_0.8)

# exports ~ imports  =  [0.7]
c_0.7 <- ggplot(data, aes(exports, imports, colour=state5)) + 
  geom_point(alpha = 0.2) +
  geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )
print(c_0.7)

# life expectancy ~ income  =  [0.6]
c_0.6a <- ggplot(data, aes(life_expec, income, colour=state5)) + 
  geom_point(alpha = 0.2) +
  geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )
print(c_0.6a)


# life expectancy ~ GDP per capita  =  [0.6]
c_0.6b <- ggplot(data, aes(life_expec, gdpp, colour=state5)) + 
  geom_point(alpha = 0.2) +
  geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )
print(c_0.6b)


#=======================================================================
#         PCA

###       **********************
# WRONG way without standardization!
data.pca <- prcomp(data[,2:10])
print(data.pca)
plot(data.pca, type = "lines")
#better plot method
library(factoextra)
get_eig(data.pca)
get_pca(data.pca)#$cor
fviz_eig(data.pca) # Percentage of variance/inertia.
fviz_screeplot(data.pca, addlabels = TRUE, ylim = c(0, 100))
###


#standardize data
data_scaled <- scale(data[,2:10], center = TRUE, scale = TRUE)
row.names(data_scaled) <- data$country
# by scaling we are removing potential bias that the model can have towards features with higher magnitudes.
#correct way
data.pca <- prcomp(data_scaled)
print(data.pca)
plot(data.pca, type = "lines")
#better plot method
library(factoextra)
get_eig(data.pca)
get_pca(data.pca)#$cor
fviz_eig(data.pca)
fviz_eig(data.pca, addlabels = T, ylim = c(0, 100)) # Percentage of variance/inertia.
fviz_screeplot(data.pca, addlabels = TRUE, ylim = c(0, 100))

#Estimate the number of significant components in Principal Component Analysis
########  needs factominer.
estim_ncp(data_scaled, ncp.min=0, ncp.max=NULL, scale=TRUE, method="GCV") 
dimdesc(data.pca) # ? FactoMineR

fviz_pca_var(data.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE  # Avoid text overlapping
)

# Contributions of variables to PC1
pc1 <- fviz_contrib(data.pca, choice = "var", axes = 1, color = "darkblue", top = 4, ggtheme = theme_minimal())
# Contributions of variables to PC2
pc2 <- fviz_contrib(data.pca, choice = "var", axes = 2, color = "darkblue", top = 4, ggtheme = theme_minimal())
library(gridExtra)
grid.arrange(pc1, pc2, ncol=2)

fvz_biplt1 <- fviz_pca_biplot(data.pca, repel = TRUE,
                col.var = "magenta", # Variables color
                col.ind = "#696969",  # Individuals color
                addEllipses = TRUE
)
fvz_biplt1
# Visualize
# Use habillage to specify groups for coloring
fvz_ind1 <- fviz_pca_ind(data.pca, #iris.pca
             label = "none", # hide individual labels
             habillage = as.factor(data$state5), # color by groups
             palette = "RdBl",
             #palette = c("cyan", "pink", "green"),
             addEllipses = TRUE # Concentration ellipses
)


# Confidence ellipses:
#  if we had some more individuals(samples) their mean would be located, 
#    with 95% confidence, inside the ellipse.
fvz_ellips1 <- fviz_ellipses(data.pca, habillage = as.factor(data$state5), ellipse.type = "confidence", geom = "point", palette = "lancet")

# calculate the percentage of variance retained for 2 PC
#  f(r) : r=2
# διακύμαση = (τυπική απόκλιση)^2
data.pca$variance <- data.pca$sdev ^2
sum(data.pca$variance[1:2])/sum(data.pca$variance)
summary(data.pca)

# plot the 2 dimensional projection 
plot(data.pca$x[,1:2], col=as.factor(data$state2), xlab="1st PC",ylab="2nd PC")
plot(data.pca$x[,1:2], col=as.factor(data$state3), xlab="1st PC",ylab="2nd PC")
plot(data.pca$x[,1:2], col=as.factor(data$state5), xlab="1st PC",ylab="2nd PC")

gg2 <- ggplot(data, aes(data.pca$x[,1], data.pca$x[,2], colour=state2)) + 
  geom_point(alpha = 0.2) +
  geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )
print(gg2)

gg3 <- ggplot(data, aes(data.pca$x[,1], data.pca$x[,2], colour=state3)) + 
  geom_point(alpha = 0.2) +
  geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )
print(gg3)

gg5 <- ggplot(data, aes(data.pca$x[,1], data.pca$x[,2], colour=state5)) + 
  geom_point(alpha = 0.2) +
  geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )
print(gg5)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(FactoMineR)
Data.pca <- PCA(data[,2:10])
row.names(Data.pca$call$X) <- data$country

fviz_screeplot(Data.pca, addlabels = TRUE, ylim = c(0, 100))
#Estimate the number of components in Principal Component Analysis
estim_ncp(data[,2:10], ncp.min=0, ncp.max=NULL, scale=TRUE, method="GCV")
dimdesc(Data.pca)
# Optimal representation of the variables
# Visualization of correlation between pairs of variables
#                    by the cosine of their angle.
# Control variable colors using their contributions
fviz_pca_var(Data.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE  # Avoid text overlapping
)


# Contributions of variables to PC1
PC_1 <- fviz_contrib(Data.pca, choice = "var", axes = 1, color = "darkblue", top = 4, ggtheme = theme_minimal())
# Contributions of variables to PC2
PC_2 <- fviz_contrib(Data.pca, choice = "var", axes = 2, color = "darkblue", top = 4, ggtheme = theme_minimal())
library(gridExtra)
grid.arrange(PC_1, PC_2, ncol=2)

grid.arrange(pc1, pc2, PC_1, PC_2, nrow=2, ncol=2)

fvz_biplt2 <- fviz_pca_biplot(Data.pca, repel = TRUE,
                col.var = "magenta", # Variables color
                col.ind = "#696969",  # Individuals color
                addEllipses = TRUE,
                title = "FactoMineR"
)

grid.arrange(fvz_biplt1, fvz_biplt2, ncol=2)

# Visualize
# Use habillage to specify groups for coloring
fvz_ind2 <- fviz_pca_ind(data.pca, #iris.pca
             label = "none", # hide individual labels
             habillage = as.factor(data$Continent), # color by groups
             palette = "RdBl",
             #palette = c("cyan", "pink", "green"),
             addEllipses = TRUE # Concentration ellipses
)
grid.arrange(fvz_ind1, fvz_ind2, ncol=2)

fvz_ellips2 <- fviz_ellipses(Data.pca, habillage = as.factor(data$Continent), ellipse.type = "confidence", geom = "point", palette = "lancet")
grid.arrange(fvz_ellips1, fvz_ellips2, ncol=2)

Data.pca$eig
# cumulative (variance %) ---> component 2. 
Data.pca$eig[2,3]
    
plot(Data.pca$ind$coord[,1:2], col=as.factor(data$state2), xlab="1st PC",ylab="2nd PC")




# Hopkins statistic: If the value of Hopkins statistic is close to 1 (far above 0.5), then we can conclude that the dataset is significantly clusterable.
# VAT (Visual Assessment of cluster Tendency): The VAT detects the clustering tendency in a visual form by counting the number of square shaped dark (or colored) blocks along the diagonal in a VAT image.
# ordered dissimilarity image (ODI)
get_clust_tendency(data_scaled, n=167-1, graph=TRUE)

# get_dist(): Computes a distance matrix between the rows of a data matrix. 
#  Compared to the standard dist() function, it supports correlation-based distance measures including "pearson", "kendall" and "spearman" methods.
distance <- get_dist(data_scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#f TRUE the ordered dissimilarity image (ODI) is shown.
fviz_dist(distance, show_labels=TRUE, order = FALSE, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


fviz_nbclust(data_scaled, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")
fviz_nbclust(data.pca$x, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")
fviz_nbclust(Data.pca$call$X, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")
fviz_nbclust(data[,2:10], kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")


fviz_nbclust(data_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
fviz_nbclust(data.pca$x, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
fviz_nbclust(Data.pca$call$X, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

    #scree-plot??
# GAP STATISTIC compares the total intra-cluster variation for different values of k with their expected values for a distribution with no clustering.
fviz_nbclust(data_scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 100) +
  labs(subtitle = "Gap statistic method")
fviz_nbclust(data.pca$x, kmeans, nstart = 25,  method = "gap_stat", nboot = 100) +
  labs(subtitle = "Gap statistic method")
fviz_nbclust(Data.pca$call$X, kmeans, nstart = 25,  method = "gap_stat", nboot = 100) +
  labs(subtitle = "Gap statistic method")


#increase print limit to 2000 values
options(max.print=2000)
#
#
library(NbClust)
nbclust <- NbClust(data = data.pca$x[,1:2], distance = "euclidean",
                   min.nc = 2, max.nc = 10, method = "kmeans")
# FactoMiNeR
#NBclust <- NbClust(data = Data.pca$ind$coord[,1:2], distance = "euclidean",
   #                min.nc = 2, max.nc = 10, method = "kmeans")

par(mfrow=c(1,1))
best_n_of_clusters <- nbclust[["Best.nc"]]
best_N_of_clusters <- as.integer(best_n_of_clusters[1,])
hist(best_N_of_clusters,
     main = "Clusters proposed by indexes",
     xlab = "Number of clusters",
     border = "Magenta",
     col = "Blue",
     xlim = c (0, 10),
     ylim = c (0, 20),
     breaks = 20)


# k-means clustering
clusters_2 <- kmeans(x = data.pca$x[,1:2], centers = 2, iter.max = 30, nstart = 50)
clusters_3 <- kmeans(x = data.pca$x[,1:2], centers = 3, iter.max = 30, nstart = 50)
clusters_5 <- kmeans(x = data.pca$x[,1:2], centers = 5, iter.max = 30, nstart = 50)

par(mfrow=c(1,2))

plot(data.pca$x[,1:2], col=as.factor(data$state2), xlab="1st PC",ylab="2nd PC")
plot(data.pca$x[,1:2], col=clusters_2$cluster, xlab="X",ylab="Y")
points(clusters_2$centers[,1], clusters_2$centers[,2], col="darkviolet", pch = 8, lwd=1.5)

plot(data.pca$x[,1:2], col=as.factor(data$state3), xlab="1st PC",ylab="2nd PC")
plot(data.pca$x[,1:2], col=clusters_3$cluster, xlab="X",ylab="Y")
points(clusters_3$centers[,1], clusters_3$centers[,2], col="darkviolet", pch = 8, lwd=1.5)

plot(data.pca$x[,1:2], col=as.factor(data$state5), xlab="1st PC",ylab="2nd PC")
plot(data.pca$x[,1:2], col=clusters_5$cluster, xlab="X",ylab="Y")
points(clusters_5$centers[,1], clusters_5$centers[,2], col="darkviolet", pch = 8, lwd=1.5)

par(mfrow=c(1,1))

ggplot(data, aes(data.pca$x[,1], data.pca$x[,2], colour=clusters_2$cluster)) + 
  geom_point(alpha = 0.2) +
  geom_text( label=data$country, nudge_x=0, nudge_y=0, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE ) +
  scale_colour_gradientn(colours=rainbow(2))
#------   ------   -------      -------      ------------------------

plot(data.pca$x[,1:2], col=clusters_2$cluster, xlab="X",ylab="Y")
points(clusters_2$centers[,1], clusters_2$centers[,2], col="darkviolet", pch = 8, lwd=1.5)
plot(data.pca$x[,1:2], col=Clusters_2$cluster, xlab="X",ylab="Y")
points(Clusters_2$medoids[,1], Clusters_2$medoids[,2], col="darkviolet", pch = 8, lwd=1.5)



#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#                         --kpca--
library(kernlab)
test <- sample(1:167,23)
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=1.2),features=2)
# sigma=0.000002
# sigma=0.0000000000000000000000002
class(kpc)
# kernel function
kernelf(kpc)
#print the principal component vectors
pcv(kpc)
#The corresponding eigenvalues
eig(kpc)
#The original data projected (rotated) on the principal components
rotated(kpc)
#The original data matrix  
# == data_scaled
xmatrix(kpc)

#plot the data projection on the components
plot(rotated(kpc),col=as.factor(data[-test,13]),
     xlab="1st Principal Component",ylab="2nd Principal Component")
#embed remaining points 
emb <- predict(kpc,data_scaled[test,])
points(emb,col=as.factor(data[test,13]))
#-------------------------
# sigma=0.01
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.01),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
ggplot(data, aes(rotated(kpc)[,1], rotated(kpc)[,2], colour=state5)) + 
  geom_point(alpha = 0.2) + 
  geom_text( label=data$country, nudge_x=0.45, nudge_y=0.1, hjust=0.5, vjust=-1, angle=0, size=1.65, check_overlap = TRUE )

# kernel function
kernelf(kpc)
#print the principal component vectors
pcv(kpc)
#The corresponding eigenvalues
eig(kpc)





# sigma=0.00000000000000001
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.00000000000000001),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.0000000000000001
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.0000000000000001),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.0000000000000002
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.0000000000000002),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.000000000000000318
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.000000000000000318),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.00000000000000034
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.00000000000000034),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.00000000000000035
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.00000000000000035),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.00000000000000038
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.00000000000000038),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.00000000000000039
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.00000000000000039),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.0000000000000004
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.0000000000000004),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.05
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.05),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.62
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.62),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=0.63
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=0.63),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=13.9
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=13.9),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=14
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=14),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=229
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=229),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=230
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=230),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=799
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=799),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=800
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=800),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=1019
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=1019),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
# sigma=1020
kpc <- kpca(~.,data=as.data.frame(data_scaled),kernel="anovadot",
            kpar=list(sigma=1020),features=2)
plot(rotated(kpc),col=as.factor(data[,13]), pch=19,
     xlab="1st Principal Component",ylab="2nd Principal Component")
title(main = "sigma = 1020")


#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================


# Hierarchical Clustering
library(dendextend)
######################library(dendroextras)
# hclust requires us to provide the data in the form of a distance matrix.

#dist_matrix1 <- dist(data[,2:10], method = "euclidean")
dist_matrix <- dist(data_scaled, method = "euclidean")  # ==distance
##dist_matrix_pca <- dist(data_scaled, method = "euclidean")


# By default, the COMPLETE LINKAGE method is used.
# Complete linkage method finds similar clusters.
clusters_completeL <- hclust(dist_matrix, method = "complete")
clusters_completeL$labels <- data$country

plot(clusters_completeL, 
     xlab = "Agglomerative clustering with 167 initial clusters/countries", 
     cex = 0.48, 
     label = data$country)


clusterCut_2 <- cutree(clusters_completeL, k = 2)    # calculate final labeling given the number of clusters
table(clusterCut_2)                    # Number of members in each cluster
clusterCut_3 <- cutree(clusters_completeL, k = 3)
table(clusterCut_3)
clusterCut_5 <- cutree(clusters_completeL, k = 5)
table(clusterCut_5)

# plot clustering result using 2d scatterplot
plot(data.pca$x[,1:2], col=clusterCut_2, xlab="X", ylab="Y", main="Complete Linkage [2]")
plot(data.pca$x[,1:2], col=clusterCut_3, xlab="X", ylab="Y", main="Complete Linkage [3]")
plot(data.pca$x[,1:2], col=clusterCut_5, xlab="X", ylab="Y", main="Complete Linkage [5]")
#--------------------
dend_completeL <- as.dendrogram(clusters_completeL)

dend_2 <- color_branches(dend_completeL, k = 2)
plot(dend_2, type = "triangle", center = TRUE, cex = 0.4, main = "Complete Linkage method [2]")

dend_3 <- color_branches(dend_completeL, k = 3)
plot(dend_3, type = "triangle", center = TRUE, cex = 0.3, main = "Complete Linkage method [3]")

dend_5 <- color_branches(dend_completeL, k = 5)
plot(dend_5, type = "triangle", center = TRUE, main = "Complete Linkage method [5]")


fviz_dend(clusters_completeL,
          k = 2,
          k_colors = "nejm",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "nejm",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.auto",
          ggtheme = theme_void()  )

fviz_dend(clusters_completeL,
          k = 3,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_with_lgl",
          ggtheme = theme_void()  )

#mypalette = c("#B24745FF", "#DF8F44FF", "#00A1D5FF", "#374E55FF", "#79AF97FF")
fviz_dend(clusters_completeL,
          k = 5,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem",
          ggtheme = theme_void()  )











# This time, we will use the SINGLE LINKAGE method:
# it adopts a ‘friends of friends’ clustering strategy.
clusters_singleL <- hclust(dist_matrix, method = "single")
clusters_singleL$labels <- data$country
plot(clusters_singleL, xlab = "Agglomerative clustering with 167 initial clusters/countries", cex = 0.4)

clusterCut_2 <- cutree(clusters_completeL, k = 2)    # calculate final labeling given the number of clusters
table(clusterCut_2)                    # Number of members in each cluster
clusterCut_3 <- cutree(clusters_completeL, k = 3)
table(clusterCut_3)
clusterCut_5 <- cutree(clusters_completeL, k = 5)
table(clusterCut_5)

# plot clustering result using 2d scatterplot
plot(data.pca$x[,1:2], col=clusterCut_2, xlab="X", ylab="Y", main="Single Linkage [2]")
plot(data.pca$x[,1:2], col=clusterCut_3, xlab="X", ylab="Y", main="Single Linkage [3]")
plot(data.pca$x[,1:2], col=clusterCut_5, xlab="X", ylab="Y", main="Single Linkage [5]")
#--------------------

dend_singleL <- as.dendrogram(clusters_singleL)

dend_2 <- color_branches(dend_singleL, k = 2)
plot(dend_2, type = "triangle", center = TRUE, main = "Single Linkage method [2]")

dend_3 <- color_branches(dend_singleL, k = 3)
plot(dend_3, type = "triangle", center = TRUE, main = "Single Linkage method [3]")

dend_5 <- color_branches(dend_singleL, k = 5)
plot(dend_5, type = "triangle", center = TRUE, main = "Single Linkage method [5]")


fviz_dend(clusters_singleL,
          k = 2,
          k_colors = "nejm",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "nejm",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.auto",
          ggtheme = theme_void()  )

fviz_dend(clusters_singleL,
          k = 3,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_with_lgl",
          ggtheme = theme_void()  )

fviz_dend(clusters_singleL,
          k = 5,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem",
          ggtheme = theme_void()  )























# This time, we will use the AVERAGE method:
clusters_average <- hclust(dist_matrix, method = "average")
clusters_average$labels <- data$country
plot(clusters_average, xlab = "Agglomerative clustering with 200 initial clusters", cex = 0.4)

clusterCut_2 <- cutree(clusters_average, k = 2)    # calculate final labeling given the number of clusters
table(clusterCut_2)                    # Number of members in each cluster
clusterCut_3 <- cutree(clusters_average, k = 3)
table(clusterCut_3)
clusterCut_5 <- cutree(clusters_average, k = 5)
table(clusterCut_5)

# plot clustering result using 2d scatterplot
plot(data.pca$x[,1:2], col=clusterCut_2, xlab="X", ylab="Y", main="Average Method [2]")
plot(data.pca$x[,1:2], col=clusterCut_3, xlab="X", ylab="Y", main="Average Method [3]")
plot(data.pca$x[,1:2], col=clusterCut_5, xlab="X", ylab="Y", main="Average Method [5]")
#--------------------

dend_average <- as.dendrogram(clusters_average)

dend_2 <- color_branches(dend_average, k = 2)
plot(dend_2, type = "triangle", center = TRUE, main = "Average method [2]")

dend_3 <- color_branches(dend_average, k = 3)
plot(dend_3, type = "triangle", center = TRUE, main = "Average method [3]")

dend_5 <- color_branches(dend_average, k = 5)
plot(dend_5, type = "triangle", center = TRUE, main = "Average method [5]")

fviz_dend(clusters_average,
          k = 2,
          k_colors = "nejm",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "nejm",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.auto",
          ggtheme = theme_void()  )

fviz_dend(clusters_average,
          k = 3,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_with_lgl",
          ggtheme = theme_void()  )

fviz_dend(clusters_average,
          k = 5,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem",
          ggtheme = theme_void()  )



# This time, we will use the MEDIAN distance method:
clusters_median <- hclust(dist_matrix, method = "median")
clusters_median$labels <- data$country
plot(clusters_median, xlab = "Agglomerative clustering with 200 initial clusters", cex = 0.4)

clusterCut_2 <- cutree(clusters_median, k = 2)    # calculate final labeling given the number of clusters
table(clusterCut_2)                    # Number of members in each cluster
clusterCut_3 <- cutree(clusters_median, k = 3)
table(clusterCut_3)
clusterCut_5 <- cutree(clusters_median, k = 5)
table(clusterCut_5)

# plot clustering result using 2d scatterplot
plot(data.pca$x[,1:2], col=clusterCut_2, xlab="X", ylab="Y", main="Median method [2]")
plot(data.pca$x[,1:2], col=clusterCut_3, xlab="X", ylab="Y", main="Median method [3]")
plot(data.pca$x[,1:2], col=clusterCut_5, xlab="X", ylab="Y", main="Median method [5]")
#--------------------

dend_median <- as.dendrogram(clusters_median)

dend_2 <- color_branches(dend_median, k = 2)
plot(dend_2, type = "triangle", center = TRUE, main = "Average method [2]")

dend_3 <- color_branches(dend_median, k = 3)
plot(dend_3, type = "triangle", center = TRUE, main = "Average method [3]")

dend_5 <- color_branches(dend_median, k = 5)
plot(dend_5, type = "triangle", center = TRUE, main = "Average method [5]")

fviz_dend(clusters_median,
          k = 2,
          k_colors = "igv",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "igv",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.auto",
          ggtheme = theme_void()  )

fviz_dend(clusters_median,
          k = 3,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_with_lgl",
          ggtheme = theme_void()  )

fviz_dend(clusters_median,
          k = 5,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem",
          ggtheme = theme_void()  )


# This time, we will use the centroid method:
clusters_centroid <- hclust(dist_matrix, method = "centroid")
clusters_centroid$labels <- data$country
plot(clusters_centroid, xlab = "Agglomerative clustering with 200 initial clusters", cex = 0.4)

clusterCut_2 <- cutree(clusters_centroid, k = 2)    # calculate final labeling given the number of clusters
table(clusterCut_2)                    # Number of members in each cluster
clusterCut_3 <- cutree(clusters_centroid, k = 3)
table(clusterCut_3)
clusterCut_5 <- cutree(clusters_centroid, k = 5)
table(clusterCut_5)

# plot clustering result using 2d scatterplot
plot(data.pca$x[,1:2], col=clusterCut_2, xlab="X", ylab="Y", main="Centroid method [2]")
plot(data.pca$x[,1:2], col=clusterCut_3, xlab="X", ylab="Y", main="Centroid method [3]")
plot(data.pca$x[,1:2], col=clusterCut_5, xlab="X", ylab="Y", main="Centroid method [5]")
#--------------------

dend_centroid <- as.dendrogram(clusters_centroid)

dend_2 <- color_branches(dend_centroid, k = 2)
plot(dend_2, type = "triangle", center = TRUE, main = "Centroid method [2]")

dend_3 <- color_branches(dend_centroid, k = 3)
plot(dend_3, type = "triangle", center = TRUE, main = "Centroid method [3]")

dend_5 <- color_branches(dend_centroid, k = 5)
plot(dend_5, type = "triangle", center = TRUE, main = "Centroid method [5]")

fviz_dend(clusters_centroid,
          k = 2,
          k_colors = "nejm",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "nejm",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.auto",
          ggtheme = theme_void()  )

fviz_dend(clusters_centroid,
          k = 3,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_with_lgl",
          ggtheme = theme_void()  )

fviz_dend(clusters_centroid,
          k = 5,
          k_colors = "uchicago",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "uchicago",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem",
          ggtheme = theme_void()  )

# This time, we will use Ward's method:
# Ward's minimum variance method aims at finding compact, spherical clusters.
# "ward.D" does not implement Ward's (1963) clustering criterion,
# whereas option "ward.D2" implements that criterion:
# with the latter, dissimilarities are squared before cluster updating.
clusters_ward <- hclust(dist_matrix, method = "ward.D2")
clusters_ward$labels <- data$country
plot(clusters_ward, xlab = "Agglomerative clustering with 200 initial clusters", cex = 0.4)

rect.hclust(clusters_ward, k = 5, border = 2:5)

clusterCut_2 <- cutree(clusters_ward, k = 2)    # calculate final labeling given the number of clusters
table(clusterCut_2)                    # Number of members in each cluster
clusterCut_3 <- cutree(clusters_ward, k = 3)
table(clusterCut_3)
clusterCut_5 <- cutree(clusters_ward, k = 5)
table(clusterCut_5)

####clusterCut <- cutree(clusters_ward, k = 4)
# Number of members in each cluster
####table(clusterCut)
# plot clustering result using 2d scatterplot
####plot(data.pca$x[,1:2], col = clusterCut, xlab = "X", ylab = "Y", main = "Clustering Result")

plot(data.pca$x[,1:2], col=clusterCut_2, xlab="X", ylab="Y", main="Ward's method [2]")
plot(data.pca$x[,1:2], col=clusterCut_3, xlab="X", ylab="Y", main="Ward's method [3]")
plot(data.pca$x[,1:2], col=clusterCut_5, xlab="X", ylab="Y", main="Ward's method [5]")
#--------------------

dend_ward <- as.dendrogram(clusters_ward)

dend_2 <- color_branches(dend_ward, k = 2)
plot(dend_2, type = "triangle", center = TRUE, main = "Ward's method [2]")

dend_3 <- color_branches(dend_ward, k = 3)
plot(dend_3, type = "triangle", center = TRUE, main = "Ward's method [3]")

dend_5 <- color_branches(dend_ward, k = 5)
plot(dend_5, type = "triangle", center = TRUE, main = "Ward's method [5]")


fviz_dend(clusters_ward,
          k = 2,
          k_colors = "igv",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "igv",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.auto",
          ggtheme = theme_void()  )

fviz_dend(clusters_ward,
          k = 3,
          k_colors = "jco",
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = "jco",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_with_lgl",
          ggtheme = theme_void()  )

fviz_dend(clusters_ward,
          k = 5,
          k_colors = c("#5050FFFF", "#7AA6DCFF", "#868686FF", "#CD534CFF", "#EFC000FF"),
          color_labels_by_k = TRUE,
          lwd = 0.6,
          cex = 0.7,
          rect = TRUE,
          rect_border = c("#5050FFFF", "#7AA6DCFF", "#868686FF", "#CD534CFF", "#EFC000FF"),
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem",
          ggtheme = theme_void()  )





























##
































