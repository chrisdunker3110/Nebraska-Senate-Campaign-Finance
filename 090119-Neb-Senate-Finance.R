### SEN. DEB FISCHER'S CAMPAIGN FINANCE REPORT
### SENS. SASSE AND FISCHER CAMPAIGN DONATIONS
### CREATED BY CHRIS DUNKER AUGUST 29, 2019

### READ IN PACKAGES AND CSV FROM THE FEC WEBSITE
require(tidyverse)
require(ggmap)
require(ggplot2)
sasse_donors = read.csv("sasse082819.csv")
fischer_donors = read.csv("fischer082919.csv")

### FIRST, LET'S GET RID OF THE COLUMNS WE DON'T NEED
sasse_donors = select(sasse_donors, 3,10,14,22:25,27:29,36:38)
fischer_donors = select(fischer_donors, 1,5:9,11:13,15:18)

str(fischer_donors)
### NOW LET'S CONVERT THE DATA INTO SOMETHING THAT'S USABLE
sasse_donors[c(2:7,9:10)] = lapply(sasse_donors[c(2:7,9:10)], as.character)
fischer_donors[c(2:6,8:11)] = lapply(fischer_donors[c(2:6,8:11)], as.character)


### ATTACH LONGITUDE/LATITUDE DATA TO ZIP CODES
zip_codes = read.csv("zipcodes.csv")

zip_codes = zip_codes %>%
  rename("zip" = "Zip")

sasse_donors = left_join(sasse_donors, zip_codes, by = "zip")
fischer_donors = left_join(fischer_donors, zip_codes, by = "zip")

### CREATE NEW DATA TO ORDER BY STATE COUNT AND REMOVE STATES WITH FEWER THAN 10 DONATIONS
sasse_sort = sasse_donors %>% group_by(contributor_state) %>% mutate(count = n())
sasse_sort = sasse_sort %>% group_by(contributor_state) %>% filter(n()>= 10) %>% ungroup()

fischer_sort = fischer_donors %>% group_by(contributor_state) %>% mutate(count = n())
fischer_sort = fischer_sort %>% group_by(contributor_state) %>% filter(n()>= 50) %>% ungroup()

View(fischer_sort)
View(sasse_sort)

### REQUIRE FORCATS TO ORDER BY FREQUENCY
require(forcats)

### BAR GRAPH OF SASSE AND FISCHER'S CONTRIBUTOR'S STATES
sasse_sort %>%
  mutate(nebraska = ifelse(contributor_state == "NE", T,F)) %>%
  ggplot(aes(fct_infreq(contributor_state))) +
  geom_bar(aes(fill = nebraska)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none") +
  labs(
    title = "Where are Sen. Ben Sasse's donors from?",
    subtitle = "The majority of individual itemized donations are not from Nebraska",
    caption = "Data from the Federal Election Commission 2015-2019",
    y = "Number of donations",
    x = "States (more than 10 itemized donors)"
  )

### A SASSE BAR GRAPH WITH NEBRASKA REMOVED
sasse_no_nebraska = sasse_sort %>%
  group_by(contributor_state != "NE") %>%
  mutate(count = n())

sasse_no_nebraska %>%
  ggplot(aes(fct_infreq(contributor_state), fill = contributor_state)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none") +
  labs(
    title = "Non-Nebraska donations to Sen. Ben Sasse",
    subtitle = "Filtered by states with more than 50 itemized individual donors",
    caption = "Data from the Federal Election Commission 2015-2019",
    y = "Number of donations",
    x = "States (more than 50 itemized donors)"
  )

### HISTOGRAM PLOT FOR DONATIONS
ggplot(fischer_donors, aes(x = contribution_receipt_amount)) +
  geom_histogram(color = "black", fill = "tomato", bins = 45) +
  geom_density(alpha = .2, fill = "tomato")


### WHAT ABOUT SASSE'S PAC DONATIONS VS NON-PAC DONATIONS


### NOW FOR FISCHER'S DONATIONS BY STATE
fischer_sort %>%
  mutate(nebraska = ifelse(contributor_state == "NE", T, F)) %>%
  ggplot(aes(fct_infreq(contributor_state))) +
  geom_bar(aes(fill = nebraska)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none") +
  labs(
    title = "Where are Sen. Deb Fischer's donors from?",
    subtitle = "The majority of individual itemized donations are from Nebraska",
    caption = "Data from the Federal Election Commission for 2013-18 election cycle",
    y = "Number of donations",
    x = "States (more than 10 itemized donors)"
  )

### SEPARATE OUT FISCHER'S NEBRASKA DONATIONS
fischer_no_nebraska = fischer_sort %>%
  filter(contributor_state != "NE")

View(fischer_no_nebraska)

fischer_no_nebraska %>%
  ggplot(aes(fct_infreq(contributor_state), fill = contributor_state)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none") +
  labs(
    title = "Non-Nebraska donations to Sen. Deb Fischer",
    subtitle = "Filtered by states with more than 50 itemized individual donors",
    caption = "Data from the Federal Election Commission 2015-2019",
    y = "Number of donations",
    x = "States (more than 50 itemized donors)"
  )

### REGISTER GOOGLE KEY AND GET A MAP OF THE UNITED STATES
### REMOVED FOR SECURITY PURPOSES
register_google(key = "xxx")
usa = get_map(location = "united states", size = c(1280, 1280), zoom = 4, maptype = "roadmap", source = "google", color = "color")

### REMOVE A DONATION FROM HAWAII SO THE MAP DOESN'T HAVE AN OUTLYING PLOT
fischer_donors = filter(fischer_donors, contributor_state != "HI")

### PLOT A MAP OF SASSE'S DONORS FOR 2013-18
ggmap(usa, extent = "normal") +
  geom_point(data = sasse_donors, aes(x = Longitude, y = Latitude,
                                      color = "tomato", size = contribution_receipt_amount), alpha = .4) +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 18, face = 'bold'),
        legend.position = "none") +
  labs(
    title = "Where do Sen. Sasse's donors live?",
    caption = "Data from the Federal Election Commission for 2015-2019"
  )

### AND NOW FOR FISCHER
ggmap(usa, extent = "normal") +
  geom_point(data = fischer_donors, aes(x = Longitude, y = Latitude,
                                        color = "tomato", size = contribution_receipt_amount),
             alpha = .4) +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 18, face = 'bold'),
        legend.position = "none") +
  labs(
    title = "Where do Sen. Fischer's donors live?",
    caption = "Data from the Federal Election Commission for 2013-18 election cycle"
  )