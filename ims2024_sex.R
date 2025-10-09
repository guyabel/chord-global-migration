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
  summarise(stock_male = sum(stock_male/1e6),
            stock_female = sum(stock_female/1e6),) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("stock"),
               names_to = "sex",
               values_to = "stock",
               names_prefix = "stock_") %>%
  rename(orig = orig_area,
         dest = dest_area) 


# regions
r <- d0 %>%
  group_by(sex) %>%
  sum_region(drop_diagonal = FALSE, flow = "stock") %>%
  group_by(region) %>%
  filter(turn == max(turn)) %>%
  ungroup() %>%
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

# tween sex
d1 <- d0 %>%
  mutate(corridor = paste(orig, dest, sep = " -> ")) %>%
  select(corridor, sex, stock) %>%
  mutate(ease = "linear",
         sex_n = factor(sex),
         sex_n = as.numeric(sex_n)) %>%
  tween_elements(time = "sex_n", group = "corridor", ease = "ease", nframes = 50) %>%
  as_tibble() %>%
  separate(col = .group, into = c("orig", "dest"), sep = " -> ") %>%
  relocate(orig, dest, stock)

##
## animation frames
##

# maximums
r_max <- r %>%
  select(region, turn) %>%
  deframe()

pdf("./plot-ims2024/sex.pdf")
for(i in unique(d1$.frame)){
  d1 %>%
    filter(.frame == i) %>%
    mig_chord(
      order = r$region,
      lab_bend1 = r %>%
        select(region, lab1) %>%
        deframe(),
      lab_bend2 = r %>%
        select(region, lab2) %>%
        deframe(),
      xmax = r_max,
      axis_breaks = 10
    )
  message(i)
}
dev.off()


##
## totals for insert
##

m <- read_csv("../migration-data/data-unilateral/stock_undesa_ims2024_full.csv") 

d3 <- m %>%
  filter(name == "World", year == 2024)
  

##
## build animation
##
ff <- c(rep(1, 5), 2:50, rep(51, 10), 50:2)

a <- image_read_pdf(path = "./plot-ims2024/sex.pdf")
# image_info(a)

saveVideo(expr = {
  for(j in ff){
    par(mar = rep(0,4))

    x <- p <- xx <- ""
    if(j < 10){
      p <- "Female Population 2024"
      x <- sprintf("%.1f", d3$stock_female/1e6 ) %>%
        paste("million")
      xx <- "Persons living outside\ntheir country of birth"
    }
    if(j > 41){
      p <- "Male Population 2024"
      x <- sprintf("%.1f", d3$stock_male/1e6 ) %>%
        paste("million")
      xx <- "Persons living outside\ntheir country of birth"
    }

    a[j] %>%
      image_annotate(
        text = p,
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
        text =
          "Plot by Guy Abel | guyabel.com",
        size = 8, color = "grey30", 
        gravity = "northwest",  location = "+10+90"
      ) %>%
      as.raster() %>%
      plot()
  }},
  ani.width = 2100, ani.height = 2100, n = length(ff),
  loop = TRUE, interval = 1/15,
  ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe",
  video.name = "./plot-ims2024/sex.mp4"
)
file.show("./plot-ims2024/sex.mp4")
