

library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram ----

server <- function(input, output) {
  
  output$PGI1 <- renderPlot({ 
    input_data <- data.frame(x=c(0, input$GL1, input$GL2, input$GL3, input$GL4, input$GL5),
                             y=c(0, input$iAUC1, input$iAUC2, input$iAUC3, input$iAUC4, input$iAUC5))
    
    # calculate PGI 
    y <- input_data$y
    x <- input_data$x
    fit_mod <- lm(y ~ x-1, data=input_data) # 拟合直线，“-1”表示强制过原点
    fit_mod2 <- summary(fit_mod)
    a = fit_mod2$coefficients[1,"Estimate"] 
    
    # plot: 
    ggplot(input_data, aes(x, y)) +
      geom_smooth(formula=y ~ x-1, method = "lm", se = T, linewidth=0.8, color="grey80", fill="grey90")+ 
      geom_point(aes(fill=x), shape=21, color="white", size=8)+  
      
      scale_x_continuous(expand = c(0,0), limits=c(0, ceiling(max(input_data$x))*1.2), breaks = seq(0, max(input_data$x), round(max(input_data$x)/5,0)))+  
      scale_y_continuous(expand = c(0,0), limits=c(0, ceiling(max(input_data$y))*1.2), breaks = seq(0, max(input_data$y), 2))+
        
      scale_fill_gradient2(low = "#F3EDC8", mid="#EAD196", high = "#7D0A0A", midpoint = mean(input_data$y))+ 
      
      xlab('GLs of meals')+
      ylab(expression("Glucose iAUC"["0-2h"]))+ 
      ggtitle(label = paste0("PGI = ", ifelse(a<0.001, format(a, scientific=T, digits=2), round(a,3))))+
      
      labs(fill="iAUC")+
      
      theme_bw()+
      theme(
        # for facet
        strip.text.x = element_text(size = 25, face = "plain", angle = 0), 
        strip.background = element_rect(color = "black", fill= "transparent"),
        panel.spacing = unit(1,"cm"),
        
        # for ggtitle
        plot.title=element_text(color = 'black', size = 25, face = "plain", hjust = 0.5),
        
        panel.grid.major = element_line(color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        
        # legend
        legend.position = "none",
        legend.title = element_text(face = "plain", size=20, hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
        legend.key.width = unit(3,"cm"),
        legend.key.height = unit(1.5,"cm"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        
        # for axis
        axis.title.x = element_text(face = "plain", color = "black"),
        axis.title.y = element_text(face = "plain", color = "black"),
        axis.title = element_text(size=20),
        
        axis.text.y = element_text(angle=0, face = "plain", hjust=1, vjust=0.5), 
        axis.text.x = element_text(angle=0, face = "plain", hjust=0.5, vjust=0.5),
        axis.text = element_text(size=15, color="black")
      ) 
    })
  
  
  
  
  
  
  
  output$PGI2 <- renderPlot({  
    
    # calculate PGI 
    input_data2 <- data.frame(x=c(0, input$GL1, input$GL2, input$GL3, input$GL4, input$GL5),
                              y=c(0, input$iAUC1, input$iAUC2, input$iAUC3, input$iAUC4, input$iAUC5))
    y <- input_data2$y
    x <- input_data2$x
    fit_mod <- lm(y ~ x-1, data=input_data2) # 拟合直线，“-1”表示强制过原点
    fit_mod2 <- summary(fit_mod)
    PGI_value = fit_mod2$coefficients[1,"Estimate"] 
    
    # calculate ranking   
    input_data <- read.csv(input$database$datapath, header = T) 
    ranking <- data.frame(input_data[order(input_data$PGI, decreasing = F),]); names(ranking) <- "PGI"
    ranking$rank <- c(1:length(ranking$PGI))
    ranking$percent <- round(ranking$rank/length(ranking$PGI)*100,2)
    position <- ranking$percent[which.min(abs(ranking$PGI-PGI_value))]

    # plot: 
    ggplot(data = input_data, aes(x=PGI)) +  
      geom_density(linewidth=1, alpha=0.3, color="#EAD196", fill="#F3EDC8") + 
      geom_vline(xintercept = PGI_value, color="#7D0A0A", linewidth=1) +
      
      annotate(x=PGI_value+0.025, y=15, "text", label="Your position", size = 8, angle=0, fontface="plain")+  
      scale_x_continuous(expand = c(0,0), limits = c(0,max((PGI_value+0.05), max(input_data$PGI))))+ 
      scale_y_continuous(expand = c(0,0))+  
      
      xlab('PGI')+
      ylab("% Percentage")+ 
      ggtitle(label = paste0("Your PGI is measured to ", round(PGI_value,3), " \nwhich is equivalent to ", as.character(position), "%  of population" ))+
      
      
      theme_bw()+
      theme(
        # for facet
        strip.text.x = element_text(size = 25, face = "plain", angle = 0), 
        strip.background = element_rect(color = "black", fill= "transparent"),
        panel.spacing = unit(1,"cm"),
        
        # for ggtitle
        plot.title=element_text(color = 'black', size = 25, face = "plain", hjust = 0.5),
        
        panel.grid.major = element_line(color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        
        # legend
        legend.position = "none",
        legend.title = element_text(face = "plain", size=20, hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
        legend.key.width = unit(3,"cm"),
        legend.key.height = unit(1.5,"cm"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        
        # for axis
        axis.title.x = element_text(face = "plain", color = "black"),
        axis.title.y = element_text(face = "plain", color = "black"),
        axis.title = element_text(size=20),
        
        axis.text.y = element_text(angle=0, face = "plain", hjust=1, vjust=0.5), 
        axis.text.x = element_text(angle=0, face = "plain", hjust=0.5, vjust=0.5),
        axis.text = element_text(size=15, color="black")
      ) 
    
  })
  
  

  
  
  
  
  
  output$iUL_PGI <- renderText({
    
    # calculate PGI 
    data_iUL <- data.frame(x=c(0, input$GL1, input$GL2, input$GL3, input$GL4, input$GL5),
                           y=c(0, input$iAUC1, input$iAUC2, input$iAUC3, input$iAUC4, input$iAUC5)) 
    y <- data_iUL$y
    x <- data_iUL$x
    fit_mod <- lm(y ~ x-1, data=data_iUL) # 拟合直线，“-1”表示强制过原点
    fit_mod2 <- summary(fit_mod)
    a = fit_mod2$coefficients[1,"Estimate"]  
    a <- ifelse(a<0.001, format(a, scientific=T, digits=2), round(a,3))
     
  })
  
  
  
  
  
  
  
  output$mealGI <- renderText({ 
    # expr("The individual upper limits for carbohydrate intake (iUL-carb) is ")
    GI_data <- read.csv(input$GI_database$datapath, header = T)
    GI <- GI_data$mealGI[GI_data$food==input$mealName]
    
  })
  
  
  
  
  
  
  
  
  output$iUL <- renderText({ 
    # threshold 
    # glucose-0h = 6.9, glu-1h=11.0, glu-2h=7.8 
    threshold_iAUC <- 1/2*((6.9+11)*1 + (11+7.8)*1)-6.9*2
    
    GI_data <- read.csv(input$GI_database$datapath, header = T)
    GI <- as.numeric(GI_data$mealGI[GI_data$food==input$mealName])
    
    # calculate PGI 
    data_iUL <- data.frame(x=c(0, input$GL1, input$GL2, input$GL3, input$GL4, input$GL5),
                           y=c(0, input$iAUC1, input$iAUC2, input$iAUC3, input$iAUC4, input$iAUC5)) 
    y <- data_iUL$y
    x <- data_iUL$x
    fit_mod <- lm(y ~ x-1, data=data_iUL) # 拟合直线，“-1”表示强制过原点
    fit_mod2 <- summary(fit_mod)
    PGI_val = fit_mod2$coefficients[1,"Estimate"]   
    
    iUL <- threshold_iAUC*100/(PGI_val*GI)
    iUL <- paste0(input$mealName, ": ", round(iUL,2), " g per meal")
    
  })
  
  }

 






