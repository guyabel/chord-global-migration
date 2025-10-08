library(tidyverse)
library(countrycode)
library(migest)
library(tweenr)
library(circlize)
library(animation)
library(magick)

s <- read_csv("../migration-data/data-bilateral/stock_undesa_ims2024.csv")

d0 <- s %>%
  mutate(orig_area = countrycode(sourcevar = orig, custom_dict = dict_ims,
                                 origin = "iso3c", destination = "region_ac2022"),
         dest_area = countrycode(sourcevar = dest, custom_dict = dict_ims,
                                 origin = "iso3c", destination = "region_ac2022")) %>%
  group_by(year, orig_area, dest_area) %>%
  summarise(stock = sum(stock/1e6)) %>%
  ungroup() %>%
  rename(orig = orig_area,
         dest = dest_area) 


# regions
r <- d0 %>%
  group_by(year) %>%
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

# tween year
d1 <- d0 %>%
  mutate(corridor = paste(orig, dest, sep = " -> ")) %>%
  select(corridor, year, stock) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year", group = "corridor", ease = "ease", 
                 nframes = diff(range(d0$year)) * 4) %>%
  as_tibble() %>%
  separate(col = .group, into = c("orig", "dest"), sep = " -> ") %>%
  relocate(orig, dest, stock)

# # for demo plots
# d2 <- d2 %>%
#   mutate(show = FALSE,
#          show = ifelse(orig == "Sub-Saharan Africa" & dest == "Sub-Saharan Africa", TRUE, show),
#          show = ifelse(orig == "Latin America & the Caribbean" & dest == "North America", TRUE, show),
#          show = ifelse(orig == "North America" & dest == "Latin America & the Caribbean", TRUE, show))

##
## animation frames
##

# maximums
r_max <- r %>%
  select(region, turn) %>%
  deframe()

pdf("./plot-ims2024/fix_time.pdf")
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
  filter(name == "World") %>%
  select(year, stock) %>%
  mutate(stock = stock/1e6) %>%
  approx(n = n_distinct(d1$.frame)) %>%
  as_tibble() %>%
  rename(year = x, m = y)

pdf(file = "./plot-ims2024/fix_time_totals.pdf", width = 7, height = 3)
for(i in 1:nrow(d3)){
  y0 <- d3 %>%
    slice(i) %>%
    pull(year) %>%
    unique() %>%
    floor()
  
  x0 <- d3 %>%
    filter(year == y0)
  
  par(mar = rep(0,4))
  plot(x = d3$year, y = d3$m,
       type = "n", bty ="n",
       axes=FALSE, xlab = "", ylab = "")
  
  points(x= x0$year, y = x0$m, pch = 19, cex = 3)
  lines(x= d3$year, y = d3$m)
  message(i)
}
dev.off()


##
## build animation
##
a0 <- d1 %>%
  select(.frame, year) %>%
  distinct() %>%
  mutate(page = 1:n(),
         stop = year %% 5 == 0,
         stop_last = year == max(year),
         stop_first = year == min(year)) %>%
  group_by(page) %>%
  mutate(page_rep = paste(rep(page, 10), collapse = ","),
         page_last = paste(rep(page, 20), collapse = ","),
         page_first = paste(rep(page, 20), collapse = ",")) %>%
  ungroup() %>%
  mutate(f = case_when(
    stop_first ~ page_first,
    stop_last ~ page_last,
    stop ~ page_rep,
    TRUE ~ as.character(page)
  ))

ff <- a0 %>%
  pull(f) %>%
  paste0(collapse = ",") %>%
  str_split(pattern = ",") %>%
  .[[1]] %>%
  as.numeric()

a <- image_read_pdf(path = "./plot-ims2024/fix_time.pdf")
s <- image_read_pdf(path = "./plot-ims2024/fix_time_totals.pdf")
# image_info(a)

saveVideo(expr = {
  for(j in ff){
    par(mar = rep(0,4))
    p <- d3 %>%
      slice(j) %>%
      distinct(year) %>%
      pull(year) %>%
      round()
    
    x <- d3 %>%
      slice(j) %>%
      pull(m)
    
    s[j] %>%
      image_scale("380") %>%
      image_border("white", "1x80") %>%
      image_composite(a[j], ., gravity = "northeast") %>%
      image_annotate(
        text = paste0("Migrant Population ", p),
        size = 18,
        gravity = "northwest", location = "+10"
      ) %>%
      image_annotate(
        text = paste(sprintf("%.1f", x), "million"),
        size = 18,
        gravity = "northeast", location = "+10"
      ) %>%
      image_annotate(
        text =
          "Axis labels represent the
        estimated millions of
        indviudals migrants
        residing in and
        originating from
        each region at
        each year",
        size = 8, color = "grey30", 
        gravity = "northeast", location = "+10+250"
      ) %>%
      image_annotate(
        text =
          "Data from United Nations Department 
of Economic and Social Affairs, 
Population Division (2024). 
International Migrant 
Stock 2024.

Plot by Guy Abel
https://guyabel.com/",
        size = 8, color = "grey30", 
        gravity = "northwest",  location = "+10+100"
      ) %>%
#       image_annotate(
#         text = "Plot by Guy Abel
# https://guyabel.com/",
#         size = 8, color = "grey30", 
#         gravity = "southeast", location = "+10+10"
#       ) %>%
      as.raster() %>%
      plot()
  }},
  ani.width = 2100, ani.height = 2100, n = length(ff),
  loop = TRUE, interval = 1/15,
  # interval = 1/10,
  # interval = 1/20,
  ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe",
  video.name = "./plot-ims2024/fix_time.mp4"
)
file.show("./plot-ims2024/fix_time.mp4")

##
## gif for readme
##
library(av)
av_encode_video(
  input = "./plot-ims2024/fix_time.mp4",
  output = "./plot-ims2024/fix_time.gif",
)
file.show("./plot-ims2024/fix_time.gif")

