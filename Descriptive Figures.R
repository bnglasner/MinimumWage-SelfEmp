# Ben Glasner
# Minimum wage and nonemployers analysis
# Industry analysis of nonemployers establishments
# Revise and Resubmit analysis code for https://bglasner.com/wp-content/uploads/2020/12/The-Minimum-Wage-Self-Employment-and-the-Online-Gig-Economy-1.pdf 
# This code represents a combination of many smaller files, but should allow for a general replication
# It is intended as an example of my code for the Cash Transfer Lab

# menu commands and shortcuts for folding the code:
# Collapse All - Alt+O
# Collapse - Alt+L
# Expand All - Shift+Alt+O
# Expand - Shift+Alt+L
# Jump To - Shift+Alt+J
# Insert Section - Ctrl+Shift+R (Cmd+Shift+R on the Mac)

# Library   #######
library(dplyr) 
library(geofacet) # plots
library(gganimate) # plots
library(urbnmapr) # plots
library(transformr) # plots
library(ggplot2) # plots
library(scales) # Edit ggplot labels

# Set paths ####

if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/PhD Requirements"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/PhD Requirements"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"\\Nonemployer data\\Data\\")
# Path where plots should be saved
path_output <- paste0(path_project,"\\Minimum wage and alt\\output\\RR")
# Path where animations should be saved 
path_animate <- paste0(path_project,"\\Minimum wage and alt\\output\\RR\\animation")

options(scipen=10000)
set.seed(42)

# To Do #####
# figures I need to create
# 1) show the change in establishments in total and in transportation annually
# 2) plot the relationship between the nominal minimum wage and establishments, establishments per member of the labor force, as well as average receipts of establishments
# 3) plot the geographic rollout of Uber within the US over time
# 4) plot the HHI quantile and HHI quantile to labor force relationship
# 5) plot the geographic distribution of HHI quantiles (2018) and log(Labor Force) size (2018)

####
run <- 1
cbbPalette <- c("#999999","#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for line and point colors, add
# scale_colour_manual(values=cbbPalette)
####
# Analysis #####

if(run==1){
  # Load the Data ##################################################
  setwd(path_data)
  load("primary_analysis_RR.RData")
  # load("csa_analysis_RR.RData")
  # using <- csa
  # using$years <- as.numeric(as.character(using$YEAR.id))
  using$estab_pop <- as.numeric(using$estab/using$Labor.Force)
  using$receipt_estab <- as.numeric(using$rcptot/using$estab)*1000
  # csa <- read.csv("csa_county_reference17.txt")
  # csa <- csa %>% select("csa","csa_name") %>% distinct()
  # using$csa <- as.numeric(as.character(using$csa))
  # test <- left_join(using,csa)
  # rm(csa)
  data_list <- list()
  data_list[[1]] <- subset(using, naics =="00") # Select all nonemployer establishments
  data_list[[2]] <- subset(using, naics !="00") # Select all nonemployer establishments
  data_list[[3]] <- subset(using, naics =="48-49") # Select transportation and warehousing
  
  # Plots #########################################################

    setwd(path_output)
    
    data_list[[1]] %>% 
      mutate(min_increase_num_roundup = ceiling(min_increase_num)) %>%
      group_by(min_increase_num_roundup, min_increase) %>% 
      summarize(min_change_inf = mean(min_change_inf, na.rm = TRUE),
                n = n()) %>%
      ggplot(aes(x = min_increase_num_roundup, 
                 group = min_increase,
                 y = min_change_inf,
                 size = n,
                 color = as.factor(min_increase))) +
      geom_point() +
      geom_hline(yintercept = 0, color = "black") +
      ylab("Average Change in the Real Minimum Wage ($2016)") +
      xlab("Number of Increases in the Real Minimum Wage ($2016)") +
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 20),
            legend.background = element_rect(fill=NA))+ 
      guides(size = FALSE,
             color = FALSE)
    
    data_list[[1]] %>%
      filter(years ==2018) %>%
      group_by(quant_avg) %>%
      summarize(population = sum(population),
                `Number of Counties` = n(),
                uber_mean = mean(Uber_active),
                `Median Min. Wage` = median(minimum_wage_inf)) %>% 
      ggplot(aes(x = quant_avg, y = population)) + 
      geom_point(aes(color = `Median Min. Wage`)) +
      # geom_smooth(method="loess", span = .2) +
      ylab("Population") +
      xlab("County HHI Quantile Group") +
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "right",
            legend.title = element_text(size = 10),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 10),
            legend.background = element_rect(fill=NA)) + 
      scale_y_continuous(label=comma) # add commas to the axis
      
    data_list[[1]] %>%
      filter(years ==2018) %>%
      group_by(quant_avg) %>%
      summarize(population = median(pop_density),
                `Number of Counties` = n(),
                uber_mean = mean(Uber_active),
                `Median Min. Wage` = median(minimum_wage_inf)) %>% 
      ggplot(aes(x = quant_avg, y = population)) + 
      geom_point(aes(color = `Median Min. Wage`)) +
      # geom_smooth(method="loess", span = .2) +
      ylab("Population Density") +
      xlab("County HHI Quantile Group") +
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "right",
            legend.title = element_text(size = 10),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 10),
            legend.background = element_rect(fill=NA)) + 
      scale_y_continuous(label=comma) 
    # Figure #1 - change in establishments in total and in transportation annually -----------------
    
    annual <- using %>% # Use the large dataset
      filter(naics == "00" |naics == "48-49") %>% # subset to only the count of all nonemployers and transportation and warehousing
      group_by(naics, years) %>% # define the groups to aggregate or summarize by
      summarize(establishments = sum(estab), # count the number of establishments by naics and year
                estab_pop = weighted.mean(estab_pop,population), # create an annual mean weighted by population
                receipt_estab = weighted.mean(receipt_estab,population)) # create an annual mean weighted by population
    annual$industry <- "County Total" # create labels 
    annual$industry[annual$naics=="48-49"] <- "Trans./Warehousing" # create labels 
    
    p <- ggplot(data = annual, # data to plot
           aes(x = years, y = establishments)) + # define x and y axis
      geom_point(size = 5) + # create a scatter plot with points of size 5
      geom_line(size = 1.5) + # create a line linking the points
      xlab("Year") + ylab("Nonemployer Establishments") + # define the axis labels to replace variable names
      scale_y_continuous(label=comma) + # add commas to the axis
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 25),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 30),
            strip.text.y = element_text(size = 30),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 20),
            legend.background = element_rect(fill=NA)) +
      facet_wrap( ~ industry,
                  scales = "free") # use the facet plot to put both industry graphs side-by-side
    pdf(file = "Annual_establishments.pdf", width = 20, height = 8)
    plot(p)
    dev.off()
    
    p <- ggplot(data = annual,
           aes(x = years, y = estab_pop)) +
      geom_point(size = 5) +
      geom_line(size = 1.5) +
      xlab("Year") + ylab("Nonemp Estab./Labor Force") +
      scale_y_continuous(label=comma) +
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 25),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 30),
            strip.text.y = element_text(size = 30),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 20),
            legend.background = element_rect(fill=NA)) +
      facet_wrap( ~ industry,
                  scales = "free")
    pdf(file = "Annual_estabpop.pdf", width = 20, height = 8)
    plot(p)
    dev.off()
    
    p <- ggplot(data = annual,
           aes(x = years, y = receipt_estab)) +
      geom_point(size = 5) +
      geom_line(size = 1.5) +
      xlab("Year") + ylab("Average Receipts") +
      scale_y_continuous(label=comma) +
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 25),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 30),
            strip.text.y = element_text(size = 30),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 20),
            legend.background = element_rect(fill=NA)) +
      facet_wrap( ~ industry)
    pdf(file = "Annual_avgreceipt_estab.pdf", width = 20, height = 8)
    plot(p)
    dev.off()
    
    rm(annual)
    

    #  Figure #2 - plot the relationship between mw and estab, estab_pop, avg_rcp -----------------

    scatter_smooth <- using %>%
      filter(naics == "00" |naics == "48-49") 
    
    scatter_smooth$Uber_active_factor <- "Uber Not Active" 
    scatter_smooth$Uber_active_factor[scatter_smooth$Uber_active==1] <- "Uber Active"
    scatter_smooth$Uber_active_factor <- as.factor(scatter_smooth$Uber_active_factor)
    
    scatter_smooth$industry <- "County Total"
    scatter_smooth$industry[scatter_smooth$naics=="48-49"] <- "Trans./Warehousing"
    
    scatter_estab <- scatter_smooth %>% 
          select("industry","minimum_wage_inf","Uber_active_factor","Uber_active","log_estab")
    scatter_rcp <- scatter_smooth %>% 
      select("industry","minimum_wage_inf","Uber_active_factor","Uber_active","log_avg_rcp")
  
    names(scatter_estab) <- c("industry","minimum_wage_inf","Uber_active_factor","Uber_active","dependent")
    scatter_estab$type <- "Log(Nonemp. Estab)"
    
    names(scatter_rcp) <- c("industry","minimum_wage_inf","Uber_active_factor","Uber_active","dependent")
    scatter_rcp$type <- "Log(Average Receipts)"
    
    scatter_smooth <- rbind(scatter_estab,scatter_rcp)
    scatter_smooth$type <- factor(scatter_smooth$type, levels = c("Log(Nonemp. Estab)","Log(Average Receipts)"))

    p <- ggplot(data =scatter_smooth,
                aes(x = minimum_wage_inf,
                    y = dependent,
                    color=Uber_active_factor)) + 
      geom_point(data = scatter_smooth, alpha =.05, size = 3) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==1), 
                  se = FALSE,
                  method = "lm",
                  color = "white", 
                  size = 3) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==1), 
                  se = FALSE,
                  method = "lm",
                  color = cbbPalette[1], 
                  size = 2.5) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==0), 
                  se = FALSE,
                  method = "lm",
                  color = "white",
                  size = 3) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==0), 
                  se = FALSE,
                  method = "lm",
                  color = cbbPalette[2], 
                  size = 2.5) +
      xlab("Real Minimum Wage ($2016)") + 
      theme(axis.text = element_text(size = 35),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 40),
            axis.text.y.right = element_text(margin = margin(r=15)),
            strip.text.x = element_text(size = 40),
            strip.text.y = element_text(size = 40),
            strip.background = element_rect(fill = "white"),
            strip.placement = "outside",
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.background = element_rect(fill=alpha("white", 0.4)),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 35),
            legend.title = element_blank()) +
      facet_grid(type ~ industry,
                 scales = "free_y",switch = "y" ) +
      scale_y_continuous(label=comma) + 
      scale_colour_manual(values=cbbPalette) +
      guides(colour = guide_legend(override.aes = list(size=5, alpha=1)),
             size = FALSE) 
    
    # pdf(file = "mw_dependent.pdf", width = 20, height = 16)
    pdf(file = "mw_dependent_outlier.pdf", width = 20, height = 16)
    plot(p)
    dev.off()
    
    
    png(file = "mw_dependent_outlier-1.png", width = 2000, height = 1600)
    plot(p)
    dev.off()
    
    
    p <- ggplot(data =subset(scatter_smooth, minimum_wage_inf<12),
                aes(x = minimum_wage_inf,
                    y = dependent,
                    color=Uber_active_factor)) + 
      geom_point(data = subset(scatter_smooth, minimum_wage_inf<12), alpha =.05, size = 3) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==1 & minimum_wage_inf<12), 
                  se = FALSE,
                  method = "lm",
                  color = "white", 
                  size = 3) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==1 & minimum_wage_inf<12), 
                  se = FALSE,
                  method = "lm",
                  color = cbbPalette[1], 
                  size = 2.5) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==0 & minimum_wage_inf<12), 
                  se = FALSE,
                  method = "lm",
                  color = "white",
                  size = 3) +
      geom_smooth(data= subset(scatter_smooth, Uber_active==0 & minimum_wage_inf<12), 
                  se = FALSE,
                  method = "lm",
                  color = cbbPalette[2], 
                  size = 2.5) +
      xlab("Real Minimum Wage ($2016)") + 
      theme(axis.text = element_text(size = 35),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 40),
            axis.text.y.right = element_text(margin = margin(r=15)),
            strip.text.x = element_text(size = 40),
            strip.text.y = element_text(size = 40),
            strip.background = element_rect(fill = "white"),
            strip.placement = "outside",
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.background = element_rect(fill=alpha("white", 0.4)),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 35),
            legend.title = element_blank()) +
      facet_grid(type ~ industry,
                 scales = "free_y",switch = "y" ) +
      scale_y_continuous(label=comma) + 
      scale_colour_manual(values=cbbPalette) +
      guides(colour = guide_legend(override.aes = list(size=5, alpha=1)),
             size = FALSE) 
    
    # pdf(file = "mw_dependent.pdf", width = 20, height = 16)
    pdf(file = "mw_dependent.pdf", width = 20, height = 16)
    plot(p)
    dev.off()
    
    
    png(file = "mw_dependent-1.png", width = 2000, height = 1600)
    plot(p)
    dev.off()
    
    rm(scatter_smooth, scatter_estab, scatter_rcp)
    
    
    #  Figure #3 - plot the geographic rollout of Uber within the US over time  -----------------

    counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
    counties_sf$FIPS.combo <- as.numeric(counties_sf$county_fips)
    
    
    map <- using %>%
      filter(naics == "00" & years > 2012) %>%
      select("naics","years","FIPS.combo","Uber_active")
    
    map <- left_join(map, counties_sf, 
                     by = c("FIPS.combo" = "FIPS.combo"))
    map$Uber_active_factor <- "Uber Active"
    map$Uber_active_factor[map$Uber_active==0] <- "Uber is not Active"
    
    p <- map %>%
      ggplot() +
      geom_sf(mapping = aes(fill = Uber_active_factor,
                            geometry = geometry),
              color = NA,
              size = 0.05) +
      labs(fill = "Categorical variable") + 
      theme(axis.text = element_blank(),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30),
            axis.text.y.right = element_text(margin = margin(r=15)),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            panel.spacing = unit(2, "lines"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.background = element_rect(fill=alpha("white", 0.4)),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 25),
            legend.title = element_blank()) +
      scale_fill_manual(values = cbbPalette) +
      facet_wrap(~years,ncol = 2)
    pdf(file = "uber_map.pdf", width = 10, height = 13)
    plot(p)
    dev.off()
    
    png(file = "uber_map-1.png", width = 1000, height = 1300)
    plot(p)
    dev.off()
    
    rm(map,counties_sf)
    
    #  Figure #4 plot the HHI quantile and HHI quantile to labor force relationship  -----------------

    quantile <- using %>%
      filter(naics == "00" & years == 2018) 
    
    p <- quantile %>%
      ggplot(aes(x = as.numeric(quantile),
                 y = as.numeric(HHI_lower),
                 size = as.numeric(population))) +
      geom_point() +
      xlab("Quantile") + ylab("County HHI") +
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 20),
            legend.background = element_rect(fill=NA)) + 
      guides(size = FALSE)
    pdf(file = "HHI_v_quantile.pdf", width = 10, height = 8)
    plot(p)
    dev.off()
    
    quant_sum <- using %>%
      filter(naics == "00" & years == 2018) %>%
      group_by(quantile) %>%
      summarise(log_lf = log(sum(Labor.Force)))
    
    quant <- using %>%
      filter(naics == "00" & years == 2018) %>%
      mutate(log_lf = log(Labor.Force)) %>%
      select("quantile","log_lf")
    
    quant_sum$type <- "Binned Total"
    quant$type <- "Observed"
    
    quant <- rbind(quant,quant_sum)
    
    p <- quant %>%
      ggplot(aes(x = quantile,
                 y = log_lf)) +
      geom_point(alpha = .3) + 
      xlab("Quantile") + ylab("Log(County Labor Force)") +
      facet_wrap(~type) +
      theme(plot.title = element_text(size=20),
            axis.text = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_text(size = 30),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(colour = "grey"),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 20),
            legend.background = element_rect(fill=NA))
    pdf(file = "laborforce_v_quantile.pdf", width = 10, height = 8)
    plot(p)
    dev.off()
    
    rm(quant,quant_sum,quantile)
    #  Figure #5 plot the geographic distribution of HHI quantiles (2018) and log(Labor Force) size (2018) -----------------

    counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
    counties_sf$FIPS.combo <- as.numeric(counties_sf$county_fips)
    
    
    map_quantile <- using %>%
      filter(naics == "00" & years ==2018) %>%
      select("naics","years","FIPS.combo","quantile")
    
    map_quantile <- left_join(map_quantile, counties_sf, 
                              by = c("FIPS.combo" = "FIPS.combo"))
    
    map_lf <- using %>%
      filter(naics == "00" & years ==2018) %>%
      select("naics","years","FIPS.combo","Labor.Force")
    
    map_lf <- left_join(map_lf, counties_sf, 
                        by = c("FIPS.combo" = "FIPS.combo"))
    map_lf$log_lf <- log(map_lf$Labor.Force)
    
    p <- map_quantile %>%
      ggplot() +
      geom_sf(mapping = aes(fill = quantile,
                            geometry = geometry),
              color = NA,
              size = 0.05) +
      theme(axis.text = element_blank(),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30),
            axis.text.y.right = element_text(margin = margin(r=15)),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            panel.spacing = unit(2, "lines"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "right",
            legend.background = element_rect(fill=alpha("white", 0.4)),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 25),
            legend.title = element_blank()) + 
      scale_fill_gradient(low="blue", high="white")
    pdf(file = "quantile_map.pdf", width = 10, height = 6)
    plot(p)
    dev.off()
    
    p <- map_lf %>%
      ggplot() +
      geom_sf(mapping = aes(fill = log_lf,
                            geometry = geometry),
              color = NA,
              size = 0.05) +
      theme(axis.text = element_blank(),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30),
            axis.text.y.right = element_text(margin = margin(r=15)),
            strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            strip.background = element_rect(fill = "white"),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            panel.spacing = unit(2, "lines"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "right",
            legend.background = element_rect(fill=alpha("white", 0.4)),
            legend.key = element_rect(fill="white"),
            legend.text = element_text(size = 25),
            legend.title = element_blank()) + 
      scale_fill_gradient(high ="blue", low="white")
    pdf(file = "lf_map.pdf", width = 10, height = 6)
    plot(p)
    dev.off()
  
}

