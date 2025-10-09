library(tidyverse)
library(countrycode)
library(migest)
library(tweenr)
library(animation)
library(magick)

s <- read_csv("../migration-data/data-bilateral/stock_undesa_ims2024.csv")

d0 <- s %>%
  filter(year == 2024) %>%
  mutate(orig_area = countrycode(sourcevar = orig, custom_dict = dict_ims,
                                 origin = "iso3c", destination = "region_ac2022"),
         dest_area = countrycode(sourcevar = dest, custom_dict = dict_ims,
                                 origin = "iso3c", destination = "region_ac2022")) %>%
  group_by(orig_area, dest_area) %>%
  summarise(stock = sum(stock/1e6)) %>%
  ungroup() %>%
  rename(orig = orig_area,
         dest = dest_area) 


# regions
r <- d0 %>%
  sum_region(drop_diagonal = FALSE, flow = "stock") %>%
  mutate(order = recode(region,
                        "Sub-Saharan Africa" = 1,
                        "North Africa" = 2,
                        "Europe" = 3,
                        "East Europe & Central Asia" = 4,
                        "West Asia" = 5,
                        "South Asia" = 6,
                        "East Asia" = 7,
                        "South-East Asia" = 8,
                        "Oceania" = 9,
                        "Latin America and the Caribbean" = 10,
                        "Northern America" = 11)) %>%
  arrange(order) %>%
  mutate(lab = str_replace(region, " and ", " & "),
         lab = str_wrap_n(string = lab, n = 2)) %>%
  separate(col = lab, into = c("lab1", "lab2"), sep = "\n", remove = FALSE, fill = "right")

# population data
m <- read_csv("../migration-data/data-unilateral/stock_undesa_ims2024_full.csv")
p <- m %>%
  filter(year == 2024, 
         iso3n < 900) %>%
  mutate(region = countrycode(sourcevar = iso3c, custom_dict = dict_ims, 
                              origin = "iso3c", destination = "region_ac2022",
                              custom_match = list("CHA" = "Europe")),
         region = unlist(region)) %>%
  group_by(region) %>%
  summarise(pop = sum(pop))

d1 <- r %>%
  left_join(p) %>%
  mutate(pop = pop/1e3) %>%
  select(region, turn, pop) %>%
  pivot_longer(cols = -1, names_to = "type", values_to = "m") %>%
  mutate(ease = "cubic-in",
         type = ifelse(type == "pop", 1, 0),
         type = as.numeric(type)) %>%
  tween_elements(time = "type", group = "region", ease = "ease", nframes = 50) %>%
  as_tibble()


##
## animation frames
##
pdf("./plot-ims2024/pop.pdf")
for(i in unique(d1$.frame)){
  r_max <- d1 %>%
    filter(.frame == i) %>%
    select(.group, m) %>%
    deframe()
  
  d0 %>%
    mig_chord(
      order = r$region,
      lab_bend1 = r %>%
        select(region, lab1) %>%
        deframe(),
      lab_bend2 = r %>%
        select(region, lab2) %>%
        deframe(),
      xmax = r_max,
    )
  message(i)
}
dev.off()


##
## build animation
##
ff <- c(rep(1, 5), 2:50, rep(51, 10), 50:2)

a <- image_read_pdf(path = "./plot-ims2024/pop.pdf")
# image_info(a)

saveVideo(expr = {
  for(j in ff){
    par(mar = rep(0,4))
    
    x <- xx <- ""
    if(j < 10){
      x <- "Scale: Migrants"
      xx <- "Sector lengths based on sum of region\nimmigrant and emigrant populations"
    }
    if(j > 41){
      x <- "Scale: Population"
      xx <- "Sector lengths based on total\npopulation of region"
    }
    
    a[j] %>%
      image_annotate(
        text = "Global Migration 2024",
        size = 18,
        gravity = "northwest", location = "+10"
      ) %>%
      image_annotate(
        text = x,
        size = 18,
        gravity = "northeast", location = "+10"
      ) %>%
      image_annotate(
        text = xx,
        size = 8, color = "grey30",
        gravity = "northeast", location = "+10+90"
      ) %>%
      # image_annotate(
      #   text =
      #     "The arrowheads shows the direction
      #   of migration. Numbers on the outer axis 
      #   indicate the migrant population size in millions. 
      #   Axis limits are set by the maximum values across both sexes",
      #   size = 8, color = "grey30",
      #   gravity = "southeast", location = "+10+10"
      # ) %>%
      image_annotate(
        text = "
Data from United Nations Department of Economic and Social Affairs, Population Division (2024). International Migrant Stock 2024.",
        size = 6, color = "grey30", 
        gravity = "southwest",  location = "+10+10"
      ) %>%
      image_annotate(
        text = "Plot by Guy Abel | guyabel.com",
        size = 8, color = "grey30", 
        gravity = "northwest",  location = "+10+90"
      ) %>%
      as.raster() %>%
      plot()
  }},
  ani.width = 2100, ani.height = 2100, n = length(ff),
  loop = TRUE, interval = 1/15,
  ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe",
  video.name = "./plot-ims2024/pop.mp4"
)
file.show("./plot-ims2024/pop.mp4")
