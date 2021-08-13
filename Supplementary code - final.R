

#Conditions for low cost green hydrogen production
#R code produced by Thomas Longden
#This code replicates the regressions and graphics in the submission

myPaths <- .libPaths()
myPaths <- c(myPaths, 'c:/Users/u1085909/Documents/R/win-library/3.6')
.libPaths(myPaths) # add new path

library(plotly)

###### Create Figure 1 | Example estimates of the cost of hydrogen production and capital costs ######

F1a <- read.csv("DataForFig1a.csv")

plot_ly(
  x = F1a$Point_x, 
  y = F1a$Point_y, 
  color = F1a$Source,mode = 'markers', symbol = F1a$Source, symbols = c('circle','x',"diamond",'square'), marker = list(size = 10))%>%
  layout(xaxis = list(title = 'Electricity cost ($/MWh)
                      ', linecolor = toRGB("black"), gridcolor = toRGB("gray50")),
         yaxis = list(title = 'Production cost ($/kg)', linecolor = toRGB("black"), gridcolor = toRGB("gray50")))%>%
  colorbar(title = "Source")

F1b <- read.csv("DataForFig1b.csv")

plot_ly(x = F1b$Estimate, type="candlestick",
        open = F1b$Low, close = F1b$High,
        low = F1b$Low, high = F1b$High) %>% 
  layout(xaxis = list(rangeslider = list(visible = F)))%>%
  layout(xaxis = list(title = 'Source and type of electrolyser', linecolor = toRGB("black"), gridcolor = toRGB("gray50")),
         yaxis = list(title = 'Capital cost ($/kW)', linecolor = toRGB("black"), gridcolor = toRGB("gray50")))



###### Set up figures showing raw data and regression tables  ###### 

# Read in raw data for plots and regressions

RDL <- read.csv("RawDataLinear.csv")
RDNL <- read.csv("RawDataNonLinear.csv")
RDNL_PMWheq70 <- filter(RDNL, PMWh == 70)
RDNL_CapCosteq900 <- filter(RDNL, CapCost == 900)


###### Create Figure 3 | Raw data from the NREL PEM model ###### 

RDL_labels <- read.csv("Fig_3a_labels.csv")

plot_ly(type = 'scatter', mode='markers', x = RDL$CapCost, y = RDL$PMWh,
        marker = list(
          color= RDL$Pkg,
          size= 10,
          colorscale='Portland',
          colorbar=list(
            title='Cost of \n H2 ($/kg)', limits = c(0.5, 14)
          )))%>%
  layout(xaxis = list(title = 'Capital cost ($/kW)
                      ', linecolor = toRGB("black"), gridcolor = toRGB("gray50"), tick0 = 0),
         yaxis = list(title = 'Electricity cost ($/MWh)', linecolor = toRGB("black"), gridcolor = toRGB("gray50"), tick0 = 0))%>%
  add_annotations(x = RDL_labels$CapCost,
                  y = RDL_labels$PMWh,
                  text = RDL_labels$Pkg,
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black")

RDNL_labels <- read.csv("Fig_3b_labels.csv")

plot_ly(type = 'scatter3d', mode='markers', x = RDNL$CapCost, y = RDNL$PMWh, z=RDNL$OCF,
        marker = list(
          color= RDNL$Pkg,
          size= 10,
          colorscale='Reds',
          colorbar=list(
            title='Cost of \n H2 ($/kg)', limits = c(0.5, 14)
          )))%>%layout(scene = list(xaxis = list(title = 'Capital cost ($/kW)', linecolor = toRGB("black"), gridcolor = toRGB("black")),
                                    yaxis = list(title = 'Electricity cost ($/MWh)', linecolor = toRGB("black"), gridcolor = toRGB("black")),
                                    zaxis = list(title = 'Operational Capacity Factor (%)', autorange = "reversed"), linecolor = toRGB("black"), gridcolor = toRGB("black"),
                                    eye = list(x = 1, y = 1, z = 1),
                                    annotations = list(list(
                                      x = 900,
                                      y = 70,
                                      z = 10,
                                      text = "$14.36/kg",
                                      xref = "900",
                                      yref = "70",
                                      zref = "10",
                                    showarrow = TRUE,
                                    arrowsize=.5, arrowhead=4,arrowcolor="black"),
                                    list(
                                      x = 900,
                                      y = 70,
                                      z = 97,
                                      text = "$5.11/kg",
                                      xref = "900",
                                      yref = "70",
                                      zref = "97",
                                      showarrow = TRUE,
                                      arrowsize=.5, arrowhead=4,arrowcolor="black"),
                                    list(
                                      x = 900,
                                      y = 10,
                                      z = 10,
                                      text = "$10.90/kg",
                                      xref = "900",
                                      yref = "10",
                                      zref = "10",
                                      showarrow = TRUE,
                                      arrowsize=.5, arrowhead=4,arrowcolor="black"),
                                    list(
                                      x = 900,
                                      y = 10,
                                      z = 97,
                                      text = "$1.65/kg",
                                      xref = "900",
                                      yref = "10",
                                      zref = "97",
                                      showarrow = TRUE,
                                      arrowsize=.5, arrowhead=4,arrowcolor="black"),
                                    list(
                                      x = 300,
                                      y = 70,
                                      z = 10,
                                      text = "$7.91/kg",
                                      xref = "300",
                                      yref = "70",
                                      zref = "10",
                                      showarrow = TRUE,
                                      arrowsize=.5, arrowhead=4,arrowcolor="black"),
                                    list(
                                      x = 300,
                                      y = 70,
                                      z = 97,
                                      text = "$4.44/kg",
                                      xref = "300",
                                      yref = "70",
                                      zref = "97",
                                      showarrow = TRUE,
                                      arrowsize=.5, arrowhead=4,arrowcolor="black"),
                                    list(
                                      x = 300,
                                      y = 10,
                                      z = 97,
                                      text = "$0.98/kg",
                                      xref = "300",
                                      yref = "10",
                                      zref = "97",
                                      showarrow = TRUE,
                                      arrowsize=.5, arrowhead=4,arrowcolor="black"),
                                    list(
                                      x = 300,
                                      y = 10,
                                      z = 10,
                                      text = "$4.45/kg",
                                      xref = "300",
                                      yref = "10",
                                      zref = "10",
                                      showarrow = TRUE,
                                      arrowsize=.5, arrowhead=4,arrowcolor="black"))))


###### Run regressions ######

linearmodel1 <- lm(Pkg ~ PMWh+CapCost, data = RDL)
lm1<-coef(linearmodel1)
summary(linearmodel1)

nonlmodel1<-nls(Pkg~(PMWh*a)+(b*CapCost/OCF), data=RDNL, start=list(a=0.06, b=0.11))
nlm1<-coef(nonlmodel1)
summary(nonlmodel1)
modelr::rsquare(nonlmodel1, RDNL)

nonlmodel2<-nls(Pkg~(PMWh*c)+(d*CapCost+0.10)*(e*OCF^-f), data=RDNL, start=list(c=0.06, d=0.002, e=64.059, f=1))
nlm2<-coef(nonlmodel2)
summary(nonlmodel2)
modelr::rsquare(nonlmodel2, RDNL)


###### Create Figure 4 | Cost of hydrogen production ###### 

# Data for Fig. 4.
V_F4 <- expand.grid(Energy = c(1, 70, 1), Cap = seq(300, 900, 1), OCF = seq(1, 97, 1))
H_F4a <- (V_F4$Energy*nlm2[1])+((nlm2[2]*V_F4$Cap+0.10)*(nlm2[3]*97^-nlm2[4]))
H_F4c <- (20*nlm1[1])+(nlm1[2]*(V_F4$Cap/V_F4$OCF))
H_F4e <- (V_F4$Energy*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*V_F4$OCF^-nlm2[4]))

# Read labels for Fig. 4a from csv file.
F4a <- read.csv("Fig_4a_labels.csv")

# Figure 4a via Plotly
plot_ly(
  x = V_F4$Cap, 
  y = V_F4$Energy, 
  z = H_F4a, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = T,
  contours = list(
    start = 0.5,
    end = 8,
    size = 0.5,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Capital cost ($/kW)
                      '),
         yaxis = list(title = 'Electricity cost ($/MWh)'))%>%
  colorbar(title = "Cost of \n H2 ($/kg)")%>% 
  add_annotations(x = F4a$Point_x,
                  y = F4a$Point_y,
                  text = F4a$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black"))

# Replicate points in Figure 4a
H_F4a_Pointa <- (50*nlm2[1])+((nlm2[2]*900+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4a_Pointa)
H_F4a_Pointb <- (20*nlm2[1])+((nlm2[2]*900+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4a_Pointb)
H_F4a_Pointc <- (1*nlm2[1])+((nlm2[2]*900+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4a_Pointc)
H_F4a_Pointd <- (50*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4a_Pointd)
H_F4a_Pointe <- (20*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4a_Pointe)
H_F4a_Pointf <- (1*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4a_Pointf)

# Percent calculation for Fig. 4b
P_F4b <- ((V_F4$Energy*nlm2[1])/H_F4a)*100

# Figure 4b via Plotly
plot_ly(
  x = V_F4$Cap, 
  y = V_F4$Energy, 
  z = P_F4b, 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = T,
  contours = list(
    start = 10,
    end = 90,
    size = 10,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Capital cost ($/kW)
                      '),
         yaxis = list(title = 'Electricity cost ($/MWh)'))%>%
  colorbar(title = "Percent (%)")%>% 
  add_annotations(x = F4a$Point_x,
                  y = F4a$Point_y,
                  text = F4a$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 



# Read labels for Fig. 4c from csv file.
F4c <- read.csv("Fig_4c_labels.csv")


# Figure 4c via Plotly
plot_ly(
  x = V_F4$Cap, 
  y = V_F4$OCF, 
  z = H_F4c, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = T,
  contours = list(
    start = 0.5,
    end = 8,
    size = 0.5,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Capital cost ($/kW)
                      '),
         yaxis = list(title = 'Operational Capacity Factor (%)'))%>%
  colorbar(title = "Cost of \n H2 ($/kg)")%>% 
  add_annotations(x = F4c$Point_x,
                  y = F4c$Point_y,
                  text = F4c$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black"))


# Replicate points in Figure 4c
H_F4c_Pointa <- (20*nlm1[1])+(0.123*(900/97))
print(H_F4c_Pointa)
H_F4c_Pointb <- (20*nlm1[1])+(0.123*(450/48.5))
print(H_F4c_Pointb)
H_F4c_Pointc <- (20*nlm1[1])+(0.123*(900/45))
print(H_F4c_Pointc)
H_F4c_Pointd <- (20*nlm1[1])+(0.123*(450/22.5))
print(H_F4c_Pointd)

# Percent calculation for Fig. 4d
P_F4d <- (((20*nlm1[1]))/H_F4c)*100

# Figure 4d via Plotly) 
plot_ly(
  x = V_F4$Cap, 
  y = V_F4$OCF, 
  z = P_F4d, 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = T,
  contours = list(
    start = 10,
    end = 90,
    size = 10,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Capital cost ($/kW)
                      '),
         yaxis = list(title = 'Operational Capacity Factor (%)'))%>%
  colorbar(title = "Percent (%)")%>% 
  add_annotations(x = F4c$Point_x,
                  y = F4c$Point_y,
                  text = F4c$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black"))



# Read labels for Fig. 4e from csv file.
F4e <- read.csv("Fig_4e_labels.csv")

# Figure 4e via Plotly
plot_ly(
  x = V_F4$OCF, 
  y = V_F4$Energy, 
  z = H_F4e, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = T,
  contours = list(
    start = 0.5,
    end = 8,
    size = 0.5,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Operational Capacity Factor (%)
                      '),
         yaxis = list(title = 'Electricity cost ($/MWh)'))%>%
  colorbar(title = "Cost of \n H2 ($/kg)")%>%
  add_annotations(x = F4e$Point_x,
                  y = F4e$Point_y,
                  text = F4e$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 

# Replicate points in Figure 4e
H_F4e_Pointa <- (50*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4e_Pointa)
H_F4e_Pointb <- (50*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F4e_Pointb)
H_F4e_Pointc <- (50*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F4e_Pointc)
H_F4e_Pointd <- (20*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4e_Pointd)
H_F4e_Pointe <- (20*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F4e_Pointe)
H_F4e_Pointf <- (20*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F4e_Pointf)
H_F4e_Pointg <- (1*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*97^-nlm2[4]))
print(H_F4e_Pointg)
H_F4e_Pointh <- (1*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F4e_Pointh)
H_F4e_Pointi <- (1*nlm2[1])+((nlm2[2]*450+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F4e_Pointi)

# Percent calculation for Fig. 4f
P_F4f <- ((V_F4$Energy*nlm2[1])/H_F4e)*100

# Figure 4f via Plotly) 
plot_ly(
  x = V_F4$OCF, 
  y = V_F4$Energy,
  z = P_F4f, 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = T,
  contours = list(
    start = 10,
    end = 90,
    size = 10,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Operational Capacity Factor (%)
                      '),
         yaxis = list(title = 'Electricity cost ($/MWh)'))%>%
  colorbar(title = "Percent (%)")%>% 
  add_annotations(x = F4e$Point_x,
                  y = F4e$Point_y,
                  text = F4e$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black"))




###### Create Figure 5 | Production of hydrogen with low cost electricity during curtailment ###### 

# Data for Fig. 5a.
V_F5a <- expand.grid(Q = seq(0, 0.30, 0.01), OCF = seq(1, 60, 1))
H_F5a <- ((30*nlm2[1])*(1-V_F5a$Q))+((1*nlm2[1])*(V_F5a$Q))+((nlm2[2]*900+0.10)*(nlm2[3]*V_F5a$OCF^-nlm2[4]))

# Read labels for Fig. 5a from csv file.
F5a <- read.csv("Fig_5a_labels.csv")

# Figure 5a via Plotly
plot_ly(
  x = V_F5a$Q, 
  y = V_F5a$OCF, 
  z = H_F5a, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = T,
  contours = list(
    start = 0.5,
    end = 8,
    size = 0.5,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Curtailment ratio (q)
                      '),
         yaxis = list(title = 'Operating capacity factor (%)'))%>%
  colorbar(title = "Cost of \n H2 ($/kg)")%>% 
  add_annotations(x = F5a$Point_x,
                  y = F5a$Point_y,
                  text = F5a$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,xanchor = 'left',arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 

# Replicate points in Figure 5a
H_F5a_Solar_HCurt <- ((30*nlm2[1])*(1-0.24))+((1*nlm2[1])*(0.24))+((nlm2[2]*900+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F5a_Solar_HCurt)
H_F5a_Solar_MCurt <- ((30*nlm2[1])*(1-0.14))+((1*nlm2[1])*(0.14))+((nlm2[2]*900+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F5a_Solar_MCurt)
H_F5a_Wind_HCurt <- ((30*nlm2[1])*(1-0.14))+((1*nlm2[1])*(0.14))+((nlm2[2]*900+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F5a_Wind_HCurt)
H_F5a_Wind_MCurt <- ((30*nlm2[1])*(1-0.02))+((1*nlm2[1])*(0.02))+((nlm2[2]*900+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F5a_Wind_MCurt)
H_F5a_Both_HCurt <- ((30*nlm2[1])*(1-0.16))+((1*nlm2[1])*(0.16))+((nlm2[2]*900+0.10)*(nlm2[3]*55^-nlm2[4]))
print(H_F5a_Both_HCurt)
H_F5a_Both_MCurt <- ((30*nlm2[1])*(1-0.08))+((1*nlm2[1])*(0.08))+((nlm2[2]*900+0.10)*(nlm2[3]*55^-nlm2[4]))
print(H_F5a_Both_MCurt)


# Percent calculation for Fig. 5b
P_F5b <- ((((30*nlm2[1])*(1-V_F5a$Q))+((1*nlm2[1])*(V_F5a$Q)))/H_F5a)*100

# Figure 5b via Plotly
plot_ly(
  x = V_F5a$Q, 
  y = V_F5a$OCF, 
  z = P_F5b, 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = T,
  contours = list(
    start = 10,
    end = 90,
    size = 10,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Curtailment ratio (q)
                      '),
         yaxis = list(title = 'Operating capacity factor (%)'))%>%
  colorbar(title = "Percent (%)")%>% 
  add_annotations(x = F5a$Point_x,
                  y = F5a$Point_y,
                  text = F5a$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,xanchor = 'left',arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 


# Data for Fig. 5c.
V_F5c <- expand.grid(Q = seq(0, 0.30, 0.01), OCF = seq(1, 60, 1))
H_F5c <- ((20*nlm2[1])*(1-V_F5c$Q))+((1*nlm2[1])*(V_F5c$Q))+((nlm2[2]*450+0.10)*(nlm2[3]*V_F5c$OCF^-nlm2[4]))

# Read labels for Fig. 5c from csv file.
F5c <- read.csv("Fig_5c_labels.csv")

# Figure 5c via Plotly
plot_ly(
  x = V_F5c$Q, 
  y = V_F5c$OCF, 
  z = H_F5c, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = T,
  contours = list(
    start = 0.5,
    end = 8,
    size = 0.5,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Curtailment ratio (q)
                      '),
         yaxis = list(title = 'Operating capacity factor (%)'))%>%
  colorbar(title = "Cost of \n H2 ($/kg)")%>% 
  add_annotations(x = F5c$Point_x,
                  y = F5c$Point_y,
                  text = F5c$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,xanchor = 'left',arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 

# Replicate points in Figure 5c
H_F5c_Solar_HCurt <- ((20*nlm2[1])*(1-0.24))+((1*nlm2[1])*(0.24))+((nlm2[2]*450+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F5c_Solar_HCurt)
H_F5c_Solar_MCurt <- ((20*nlm2[1])*(1-0.14))+((1*nlm2[1])*(0.14))+((nlm2[2]*450+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F5c_Solar_MCurt)
H_F5c_Wind_HCurt <- ((20*nlm2[1])*(1-0.14))+((1*nlm2[1])*(0.14))+((nlm2[2]*450+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F5c_Wind_HCurt)
H_F5c_Wind_MCurt <- ((20*nlm2[1])*(1-0.02))+((1*nlm2[1])*(0.02))+((nlm2[2]*450+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F5c_Wind_MCurt)
H_F5c_Both_HCurt <- ((20*nlm2[1])*(1-0.16))+((1*nlm2[1])*(0.16))+((nlm2[2]*450+0.10)*(nlm2[3]*55^-nlm2[4]))
print(H_F5c_Both_HCurt)
H_F5c_Both_MCurt <- ((20*nlm2[1])*(1-0.08))+((1*nlm2[1])*(0.08))+((nlm2[2]*450+0.10)*(nlm2[3]*55^-nlm2[4]))
print(H_F5c_Both_MCurt)

# Percent calculation for Fig. 5d
P_F5d <- ((((20*nlm2[1])*(1-V_F5c$Q))+((1*nlm2[1])*(V_F5c$Q)))/H_F5c)*100

# Figure 5d via Plotly
plot_ly(
  x = V_F5c$Q, 
  y = V_F5c$OCF, 
  z = P_F5d, 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = T,
  contours = list(
    start = 10,
    end = 90,
    size = 10,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Curtailment ratio (q)
                      '),
         yaxis = list(title = 'Operating capacity factor (%)'))%>%
  colorbar(title = "Percent (%)")%>% 
  add_annotations(x = F5c$Point_x,
                  y = F5c$Point_y,
                  text = F5c$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,xanchor = 'left',arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 


# Data for Fig. 5e.
V_F5e <- expand.grid(Q = seq(0, 0.30, 0.01), OCF = seq(1, 60, 1))
H_F5e <- ((20*nlm2[1])*(1-V_F5e$Q))+((1*nlm2[1])*(V_F5e$Q))+((nlm2[2]*250+0.10)*(nlm2[3]*V_F5e$OCF^-nlm2[4]))

# Read labels for Fig. 5e from csv file.
F5e <- read.csv("PointsForFig5e.csv")

# Figure 5e via Plotly
plot_ly(
  x = V_F5e$Q, 
  y = V_F5e$OCF, 
  z = H_F5e, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = T,
  contours = list(
    start = 0.5,
    end = 8,
    size = 0.5,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Curtailment ratio (q)
                      '),
         yaxis = list(title = 'Operating capacity factor (%)'))%>%
  colorbar(title = "Cost of \n H2 ($/kg)")%>% 
  add_annotations(x = F5e$Point_x,
                  y = F5e$Point_y,
                  text = F5e$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,xanchor = 'left',arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 

# Replicate points in Figure 5e
H_F5e_Solar_HCurt <- ((20*nlm2[1])*(1-0.24))+((1*nlm2[1])*(0.24))+((nlm2[2]*250+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F5e_Solar_HCurt)
H_F5e_Solar_MCurt <- ((20*nlm2[1])*(1-0.14))+((1*nlm2[1])*(0.14))+((nlm2[2]*250+0.10)*(nlm2[3]*30^-nlm2[4]))
print(H_F5e_Solar_MCurt)
H_F5e_Wind_HCurt <- ((20*nlm2[1])*(1-0.14))+((1*nlm2[1])*(0.14))+((nlm2[2]*250+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F5e_Wind_HCurt)
H_F5e_Wind_MCurt <- ((20*nlm2[1])*(1-0.02))+((1*nlm2[1])*(0.02))+((nlm2[2]*250+0.10)*(nlm2[3]*45^-nlm2[4]))
print(H_F5e_Wind_MCurt)
H_F5e_Both_HCurt <- ((20*nlm2[1])*(1-0.16))+((1*nlm2[1])*(0.16))+((nlm2[2]*250+0.10)*(nlm2[3]*55^-nlm2[4]))
print(H_F5e_Both_HCurt)
H_F5e_Both_MCurt <- ((20*nlm2[1])*(1-0.08))+((1*nlm2[1])*(0.08))+((nlm2[2]*250+0.10)*(nlm2[3]*55^-nlm2[4]))
print(H_F5e_Both_MCurt)

# Percent calculation for Fig. 5f
P_F5f <- ((((20*nlm2[1])*(1-V_F5e$Q))+((1*nlm2[1])*(V_F5e$Q)))/H_F5e)*100

# Figure 5f via Plotly
plot_ly(
  x = V_F5e$Q, 
  y = V_F5e$OCF, 
  z = P_F5f, 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = T,
  contours = list(
    start = 10,
    end = 90,
    size = 10,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Curtailment ratio (q)
                      '),
         yaxis = list(title = 'Operating capacity factor (%)'))%>%
  colorbar(title = "Percent (%)")%>% 
  add_annotations(x = F5e$Point_x,
                  y = F5e$Point_y,
                  text = F5e$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,xanchor = 'left',arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 


###### Create Figure 6 | Combinations that achieve cost competitiveness using end-use reference prices ###### 

# Data for Fig. 6.
M_F6 <- expand.grid(Energy = c(1, 70, 1), Cap = seq(300, 900, 1))
H_F6 <- ((M_F6$Energy*nlm2[1]))+((nlm2[2]*M_F6$Cap+0.10)*(nlm2[3]*55^-nlm2[4]))

# Read labels for Fig. 6 from csv file.
F6 <- read.csv("Fig_6_labels.csv")

# Figure 6 via Plotly
plot_ly(
  x = M_F6$Cap, 
  y = M_F6$Energy, 
  z = H_F6, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = F,
  contours = list(
    start = 0.5,
    end = 8,
    size = 0.5,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%     
  layout(xaxis = list(title = 'Capital cost ($/kW)
                      '),
           yaxis = list(title = 'Electricity cost ($/MWh)'))%>%
    colorbar(title = "Cost of \n H2 ($/kg)")%>% 
  add_trace(x = c(300,900), y = c(65.451812900982, 45.0380598088111), type = "scatter", mode = "line", line = list(width = 3, color = "darkorchid"),name="$4.48/kg")%>% 
  add_trace(x = c(300,900), y = c(32.004252362824, 11.5904543650107), type = "scatter", mode = "line", line = list(width = 3, color = "darkorchid", dash=3),name="$2.55/kg")%>% 
  add_trace(x = c(300,819.057626486951), y = c(18.6598888320563, 1), type = "scatter", mode = "line", line = list(width = 3, color = "orange"),name="$1.78/kg")%>% 
  add_trace(x = c(300,650.964859770415), y = c(12.9408506655675, 1), type = "scatter", mode = "line", line = list(width = 3, color = "orange", dash=3),name="$1.45/kg")%>% 
  add_annotations(x = F6$Point_x,
                  y = F6$Point_y,
                  text = F6$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,xanchor = 'left',arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 



###### Figure S1 | Difference in hydrogen production costs between a Proton Exchange Membrane (PEM) electrolyser and an Alkaline (AE) electrolyser ######


DiffL <- read.csv("D:/ANU/Economics of H2/Stata/LCOH_Diff_PEMcomparedAE_Yates.csv")

linearmodel2 <- lm(Y ~ X+Z, data = DiffL)
lm2<-coef(linearmodel2)
summary(linearmodel2)

# Data for Fig. S1.
V_FS1 <- expand.grid(EPK = c(-6, 6, 0.5), DC = seq(-600, 600, 10))
H_FS1 <- lm2[1]+(lm2[2]*V_F3$DC)+(lm2[3]*V_F3$EPK)

# Read labels for Fig. S1 from csv file.
FS1 <- read.csv("Fig_S1_labels.csv")

# Figure 3 via Plotly
plot_ly(
  x = V_FS1$DC, 
  y = V_FS1$EPK, 
  z = H_FS1, 
  type = "contour",
  colorscale = 'Picnic',
  autocontour = T,
  contours = list(
    start = -1,
    end = 1,
    size = 0.2,
    showlabels = TRUE
  ),
  line = list(smoothing = 1, width = 1, color = "black"))%>%
  layout(xaxis = list(title = 'Difference in capital cost ($/kW)
                      '),
         yaxis = list(title = 'Difference in electricity per kg H2 (kWh/kg)'))%>%
  colorbar(title = "Difference in \n PEM cost \n compared \n to AE ($/kg)")%>% 
  add_annotations(x = F3$Point_x,
                  y = F3$Point_y,
                  text = F3$Label,
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowsize=.5, arrowhead=4,arrowcolor="black",
                  font=list(color = "black"))%>%
  layout(font = list(color = "black")) 

# Replicate points in Figure S1
H_FS1_a <- lm2[1]+(lm2[2]*110)+(lm2[3]*-4)
print(H_FS1_a)
H_FS1_b <- lm2[1]+(lm2[2]*-110)+(lm2[3]*-4)
print(H_FS1_b)
H_FS1_c <- lm2[1]+(lm2[2]*500)+(lm2[3]*-4)
print(H_FS1_c)
