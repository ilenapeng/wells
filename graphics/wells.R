library(rgdal)
library(tidyverse)
library(extrafont)
library(sf)
library(tigris)
library(ggtext)
library(svglite)

### Loading data
# Orphaned (unplugged)
orphaned <- read_csv('https://raw.githubusercontent.com/ilenapeng/wells/main/data_5_4_22/Oil__Gas____Other_Regulated_Wells__Beginning_1860.csv')
# Orphaned, by county & drop index column
unplug_orphaned <- read_csv('https://raw.githubusercontent.com/ilenapeng/wells/main/data_5_4_22/data_processed_5422/unplug_orphaned_5422.csv')
# All active wells
active <- read_csv('https://raw.githubusercontent.com/ilenapeng/wells/main/data_5_4_22/data_processed_5422/active_all_5422.csv')
# Active well counts, by county & drop index column
active_ct <- read_csv('https://raw.githubusercontent.com/ilenapeng/wells/main/data_5_4_22/data_processed_5422/active_5422.csv')
# Time between completed and plugged
timespan <- read_csv('https://raw.githubusercontent.com/ilenapeng/wells/main/data_5_4_22/data_processed_5422/timespan_5422.csv')

#Chart theme
plot_theme <- theme(
  text=element_text(family="Helvetica", color="black"), 
  plot.title=element_text(face="bold", size=16), 
  plot.subtitle=element_text(size=12), 
  plot.caption=element_text(size=8),
  plot.margin=margin(1,1,1,1, "cm"),
  axis.title.y=element_blank(),
  axis.title.x=element_text(color="black", face="bold", size=12),
  axis.text=element_text(color="black", size=12),
  legend.position="left",
  legend.title=element_blank())

########### Map of abandoned and unplugged wells
#Removing nulls now for plotting purposes - did not remove them in the counts because they were not marked as UM 
orphaned <- orphaned %>% filter(!is.na(`Surface Longitude`) & !is.na(`Surface Latitude`)) 
active <- active %>% filter(!is.na(`surfacelongitude`) & !is.na(`surfacelatitude`))
#Merging wells data and shapefile
ny <- counties("New York", cb = TRUE)
merge <- ny %>% full_join(unplug_orphaned, by = c("NAME" = "county"))

p_abdn <-
  ggplot() +
  geom_sf(data=merge, fill="white", color="#DCDCDC") +
  geom_point(data=active, aes(x=surfacelongitude, y=surfacelatitude), alpha=0.1, size=0.05, color="#999999") +
  geom_point(data=orphaned, aes(x=`Surface Longitude`, y=`Surface Latitude`), alpha=0.2, size=0.1, color="#E3655B") +
  annotate("text", x = -78.5, y = 41.25, hjust=0, label = "Allegany County has the most\norphaned and unplugged\nwells in New York") +
  annotate("text", x = -80.3, y = 43.8, hjust=0, label = "Cattaraugus County has the most\nactive wells in New York") +
  theme_void() +
  labs(
    title="Western New York is the region of interest for both<br><span style='color:#E3655B'>orphaned & unplugged</span> and <span style='color:#999999'>active</span> wells", 
    subtitle="Documented orphaned & unplugged and active wells in New York",
    caption="Data from NY State Department of Environmental Conservation via NY Open Data\nGraphic by Ilena Peng") +
  plot_theme + theme (plot.title=element_markdown(face="bold", size=16), axis.text=element_blank(), axis.title.x=element_blank())

print(p_abdn)
ggsave("map.svg", width=9, height=6, unit="in")

### Bar chart
abdn_active <- full_join(unplug_orphaned, active_ct, by = c("county" = "County"))
abdn_active = rename(abdn_active,c("abandon"="count","active"="Ct"))
#replace NAs with 0
abdn_active[is.na(abdn_active)] <- 0
## Creating "Other counties" designation
#variable for keep or not
abdn_active$keep = ifelse(abdn_active$abandon >= 30, "yes", "no")

### COMMENT OUT - this is for creating 'Other Counties' bar
## calculate means for each group so we know the "no" values
# abdn_active %>% group_by(keep) %>% summarise(abandon_avg = mean(abandon), active_avg = mean(active))
## No: abandon average is 6.93, active average is 50.9
for_plt <- abdn_active %>% filter(keep=="yes")
## Add in our new averaged row
# for_plt <- for_plt %>% add_row(county = "Other counties", abandon = 6.93, active=50.9)

#Gather data for stacked bar & drop keep column
for_plt <- for_plt[ -c(4)]
gather <- gather(for_plt, "type", "count", -county)

gather$type = factor(gather$type, levels = c('abandon', 'active'))
gather$county = factor(gather$county, levels = c('Other counties', 'Oneida', 'Wyoming', 'Tompkins', 'Ontario', 'Oswego', 'Chautauqua', 'Erie', 'Steuben', 'Cattaraugus', 'Allegany'))

p_abdnactive <- gather %>% 
  mutate(type = fct_rev(factor(type, levels=c('abandon','active')))) %>% 
  ggplot(aes(y=county, x=count, fill=type)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("#DCDCDC", "#E3655B"), labels=c("Abandon","Active")) +
  annotate("text", x = 2400, y = 9.75, hjust=0, label = "Allegany County now has more orphaned\nand unplugged wells than active ones") +
  annotate("text", x = 3700, y = 4.75, hjust=1, label = "Meanwhile, oil production has largely shifted\nto Chautauqua County, which has over 4,000\nactive wells and relatively few unplugged wells") +
  labs (
    title="<span style='color:#E3655B'>Unplugged</span> wells in Allegany County, former seat of New York oil<br>industry, now exceed number of <span style='color:#999999'>active</span> wells",
    subtitle="Orphaned & unplugged and active wells in New York counties with 30 or more orphaned wells",
    caption="Data from NY State Department of Environmental Conservation via NY Open Data\nGraphic by Ilena Peng",
    x="Number of wells"
  ) +
  theme_minimal() +
  plot_theme +
  theme(legend.position="none", plot.title=element_markdown(face="bold", size=16, lineheight = 1.2), panel.grid.major.y=element_blank())

print(p_abdnactive)
ggsave("abdn_active.svg", width=10, height=7, unit="in")

### Timespan chart
#Ordered by mean, removed because decided to order by median: counties <- c("Allegany", "Cattaraugus", "Steuben", "Genesee", "Erie", "Chautauqua", "Wyoming")
counties <- c("Cattaraugus", "Allegany",  "Genesee", "Steuben", "Erie", "Chautauqua", "Wyoming")

plt_counties <- timespan %>% filter(county %in% counties)

p_timespan <- plt_counties %>%
  mutate(county = fct_rev(factor(county, levels=counties))) %>% 
  ggplot(aes(x=well_time_yr, y=county)) + 
  geom_point(color="#ED6A5A", alpha=0.2, size=3) +
  annotate(geom = "rect", ymin = Inf , ymax = 0, xmax = 30, xmin = -Inf, alpha = .05) +
  geom_vline(xintercept=30, colour="#DCDCDC", size=1) +
  annotate("text", x = 32, y = 4.5, colour="#757575", hjust=0, label = "The 30-year mark at which a well's production quantity begins to decline") +
  annotate("text", x = 36, y = 6.5, hjust=0, label = "34 years is the median for Cattaraugus County, which means half of the\ncounty's wells were recorded as being plugged 34 years after their completion") +
  stat_summary(fun="median", geom="point", shape=124, size=8, color="#B82714") +
  labs(
    title="Most of NY's plugged wells were plugged 20 to 30 years after their initial completion, in\nline with a well's recommended lifespan, but some have remained unplugged much longer",
    subtitle="Years between well completion and plugging in counties that had over 50 wells with recorded dates",
    caption="Data from NY State Department of Environmental Conservation via NY Open Data\nGraphic by Ilena Peng",
    x="Years"
  ) + theme_minimal() + plot_theme + theme(panel.grid.major.y=element_blank(), panel.grid.minor.x=element_blank())

print(p_timespan)
ggsave("timespan.svg",  width=11, height=9, unit="in")
