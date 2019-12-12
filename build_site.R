# build_site.R
# create the site

library(magrittr)
library(readr)
library(glue)
library(forcats)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(htmltools)
library(ggplot2)
library(cowplot)
library(showtext)
showtext_auto()

font_add_google("Nunito Sans", "nunitosans")
CHART_FONT <- "nunitosans"

BS_THEME <- "lux"
source("_template.R", encoding = "UTF-8")



# Load Metadata --------------------------------------------------------

challenges <- read_csv("data/challenges.csv")
cartographers <- read_csv("data/cartographers.csv", col_types = "ccccc")
# shuffle to give an interesting start
map_metadata <- read_tsv("data/map_metadata.tsv", col_types = "cccccccccc") %>% sample_frac(1)
map_classification <- read_tsv("data/map_classification.tsv", col_types = "ccccccc")
aspect_cols <-
  read_tsv("data/aspects.tsv") %>% 
  mutate(aspect_class = glue("col-xs-{xs} col-sm-{sm} col-md-{md} col-lg-{lg} col-xl-{xl}"))

areas <- 
  map_classification %>% 
  distinct(area) %>% 
  #mutate(area = if_else(area == "_", "unclassified", area)) %>% 
  separate_rows(area, sep = ",") %>% 
  distinct()
continents <- 
  read_csv("data/continents.csv") %>% 
  mutate(datavalue = area %>% str_to_lower() %>% str_replace_all(" ", ""))
countries <-
  areas %>% 
  anti_join(continents) %>% 
  #filter(area != "_") %>% 
  mutate(datavalue = area %>% str_to_lower() %>% str_replace_all(" ", "")) %>% 
  arrange(area)
cities <- 
  map_classification %>% 
  distinct(city) %>% 
  filter(city != "_") %>% 
  separate_rows(city, sep = ",") %>% 
  distinct() %>% 
  arrange(city) %>% 
  bind_rows(tibble(city = "_"))

topics <- 
  map_classification %>% 
  distinct(topics) %>% 
  filter(topics != "_") %>% 
  separate_rows(topics, sep = ",") %>% 
  distinct() %>% 
  arrange(topics) %>% 
  bind_rows(tibble(topics = "_"))

types_of_maps <- 
  map_classification %>% 
  distinct(types) %>% 
  filter(types != "_") %>% 
  separate_rows(types, sep = ",") %>% 
  distinct() %>% 
  arrange(types) %>% 
  bind_rows(tibble(types = "_"))

tools <- 
  map_classification %>% 
  distinct(tools) %>% 
  filter(tools != "_") %>% 
  separate_rows(tools, sep = ",") %>% 
  distinct() %>% 
  arrange(tools) %>% 
  bind_rows(tibble(tools = "_"))




# Make Index --------------------------------------------------------------

index_page <- 
  paste(
    index_header,
    div(class = "row",
        div(class = "col-12",
            p("There were so many awesome maps shared as part of #30DayMapChallenge in November 2019,",
              "but they were dispersed across Twitter. I thought I'd try to collate them",
              "so they're all in one place and somewhat explorable. I'm looking forward",
              "to catching up on the ones I missed and coming back time and again for",
              "inspiration. Hopefully you enjoy them too. Thanks to everyone who took part!"),
            h3("Where are the maps?"),
            p(span(class = "text-info",
                  "Click through to the", a(href = "maps.html", "map gallery"),
                  "where you can explore all* the maps."),
              "Turning the unstructured data of thousands of tweets (map submissions and random discussion)",
              "into a structured dataset is naturally a long and partly manual process, so the gallery",
              "content continues to grow."
              ),
            p("The interface allows you to filter by challenge days and the",
              "areas being mapped, as well as other metadata - the types of",
              "maps and the tools used (where they've so far been classified).",
              "Click on a map card to see the full image, and to link through",
              "to the original tweet and the creator's webpage."),
            p("As you might expect, loading the full page and looking at every map in detail",
              "will load 10s or 100s of MB of data, but the images are loaded lazily",
              "so you don't have to download it all at once!"),
            p("If you have trouble with the gallery, or don't like the approach,",
              "Aurelien Chaumet took a different (but still really awesome) approach",
              a(href = "https://public.tableau.com/profile/aurelien.chaumet#!/vizhome/30daymapchallenge/30DayMapChallenge-Week1?publish=yes",
                "collating them all in Tableau.")),
            h3("How did the challenge start?"),
            p("This was all started by Topi Tjukanov in Finland:"),
            tags$blockquote(class = "twitter-tweet",
                            `data-lang` = "en",
                            p(lang = "en", dir = "ltr",
                              "Announcing #30DayMapChallenge in November 2019!",
                              "Create a map each day of the month with the following themes",
                              tags$br(), tags$br(),
                              "No restriction on tools. All maps should be created by you.",
                              "Doing less than 30 maps is fine. #gischat #geography #cartography #dataviz",
                              a(href = "https://t.co/6Go4VFWcJB",
                              "pic.twitter.com/6Go4VFWcJB")),
                              "- Topi Tjukanov (@tjukanov)",
                              a(href = "https://twitter.com/tjukanov/status/1187713840550744066",
                                "October 25, 2019")),
            p("People know a good idea when they see one, and hundreds of people",
              "from around the globe took part to have some fun and improve their",
              "map making skills."),
            h3("How many people took part? What were the most popular countries?"),
            p("Take a look at the",
              a(href = "stats.html", "stats page"),
              "(though it needs a bit more data at this stage)."),
            h3("Can I help complete the metadata for the maps?"),
            p("Yes, of course!",
              "I'm happy to accept corrections to mistakes or additional metadata",
              "via email (myname at frigge.nz),",
              a(href = "https://twitter.com/dakvid", "tweet,"), "or",
              a(href = "https://github.com/dakvid/30DayMapChallenge", "Github"),
              "issues or pull requests."),
            p("If a map is missing from the gallery then it should be in my",
              "todo list (actually a todo data frame) and will appear soon.",
              "The most helpful area to crowdsource is the metadata on areas,",
              "topics, types and tools. My source data file",
              a(href = "https://github.com/dakvid/30DayMapChallenge/blob/master/data/map_classification.tsv", "is here"),
              "- please feel welcome to fill in any of the gaps."),
            p("Note that I decided to only allow one map per theme/day per person.",
              "Some people made multiple maps for a theme - generally you can see",
              "the others if you click through to the original tweet."),
            h3("Who are you?"),
            p("I'm", a(href = "https://david.frigge.nz/about", "David Friggens"),
              a(href="https://twitter.com/dakvid", "(@dakvid)"),
              "- just another guy on Twitter with an interest in maps.",
              "I had other commitments in early November so only managed 9 maps",
              "in the second half, but wanted to see more of what everyone else",
              "made as there have been so many amazing maps."),
            h3("How did you make this site?"),
            p("With", a(href = "https://getbootstrap.com/", "Bootstrap 4"),
              "and the", a(href = "https://bootswatch.com/lux/", "Lux"), "theme from Bootswatch.",
              "The gallery was made with", a(href="https://vestride.github.io/Shuffle/", "shuffle.js"),
              "and", a(href="https://github.com/aFarkas/lazysizes", "lazysizes."),
              "The data munging and HTML construction is performed by some rough",
              a(href = "https://github.com/dakvid/30DayMapChallenge", "R code.")),
            h3("Is a FAQ style the best way to structure this page?"),
            p("No, probably not."),
        )
    ),
    index_footer,
    collapse = "\n"
  )

write_file(index_page, "index.html")





# Make Statistics ---------------------------------------------------------

num_potential_cartographers <- nrow(cartographers)
num_indexed_cartographers <- map_classification %>% distinct(handle) %>% nrow()
num_countries <- nrow(countries)
num_cities <- nrow(cities)-1
num_maps <- nrow(map_classification)

num_unc_area <- map_classification %>% filter(area == "_") %>% nrow()
pc_unc_area <- round(num_unc_area / num_maps * 100, 1)
num_unc_city <- map_classification %>% filter(city == "_") %>% nrow()
pc_unc_city <- round(num_unc_city / num_maps * 100, 1)
num_unc_topic <- map_classification %>% filter(topics == "_") %>% nrow()
pc_unc_topic <- round(num_unc_topic / num_maps * 100, 1)
num_unc_type <- map_classification %>% filter(types == "_") %>% nrow()
pc_unc_type <- round(num_unc_type / num_maps * 100, 1)
num_unc_tool <- map_classification %>% filter(tools == "_") %>% nrow()
pc_unc_tool <- round(num_unc_tool / num_maps * 100, 1)


full30 <- 
  map_classification %>% 
  count(handle) %>% 
  filter(n == 30)


# > Graphs ----------------------------------------------------------------

g_challenges_data <- 
  map_classification %>% 
  inner_join(challenges, by = "Day") %>% 
  mutate(challenge = paste(Day, Theme)) %>% 
  count(challenge) %>% 
  mutate(challenge = challenge %>% fct_inorder() %>% fct_rev())
g_challenges <- 
  ggplot(g_challenges_data,
         aes(x = challenge, y = n)) +
  geom_col() + 
  geom_text(data = g_challenges_data,
            aes(x = challenge, y = n, label = n),
            hjust = 1, nudge_y = -3,
            color = "white",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "People who completed each daily map")
ggsave(filename = "challenge_count.png",
       path = "images/",
       plot = g_challenges,
       width = 7, height = 5.5, units = "cm")


g_countries_data <- 
  map_classification %>% 
  select(area, handle) %>% 
  filter(area != "_") %>% 
  separate_rows(area, sep = ",") %>%
  group_by(area) %>% 
  summarise(n = n(),
            people = n_distinct(handle)) %>% 
  ungroup() %>% 
  # count(area) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  mutate(area = area %>% fct_inorder() %>% fct_rev())
g_countries <- 
  ggplot(g_countries_data,
         aes(x = area, y = n)) +
  geom_col() +
  geom_col(data = g_countries_data, aes(x = area, y = people), fill = "orange", width = 0.3) +
  geom_text(data = g_countries_data,
            aes(x = area, y = n, label = n),
            hjust = 1, nudge_y = -1,
            color = "white",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Top 20 Map Areas")
ggsave(filename = "area_count.png",
       path = "images/",
       plot = g_countries,
       width = 7, height = 4, units = "cm")

g_tools <- 
  map_classification %>% 
  select(tools) %>% 
  filter(tools != "_") %>% 
  separate_rows(tools, sep = ",") %>% 
  count(tools) %>% 
  arrange(desc(n)) %>% 
  mutate(tools = tools %>% fct_inorder() %>% fct_rev()) %>% 
  ggplot(aes(x = tools, y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Tools Used to Make Maps")
ggsave(filename = "tool_count.png",
       path = "images/",
       plot = g_tools,
       width = 7, height = 4, units = "cm")


# > Page & Save -----------------------------------------------------------

stats_page <- 
  paste(
    stats_header,
    stats_header_nav,
    div(class = 'container',
        h1("Statistics"),
        div(class = "row",
            div(class = "col-12",
                p(glue("There have been at least {num_potential_cartographers} ",
                       "people tweeting on the hashtag. ",
                       "Currently I've indexed {num_maps} maps ",
                       "by {num_indexed_cartographers} people.")),
                p("To see an overview of all the tweets using the hashtag",
                  "(including retweets, announcements, and discussions) see",
                  a(href = "https://tweepsmap.com/hashtag/205F92C1E10C03", "the report by TweepsMap.")),
                h3("Progress"),
                p("The main reason this sort of thing hasn't been done by dozens of",
                  "other people already is that the tweets are a vast unstructured",
                  "data set. Even the seemingly 'simple' task of assigning each map",
                  "to one of the 30 days is manually time consuming - a lot of people",
                  "didn't explicitly label the theme, or they got the number wrong,",
                  "and even if they did there wasn't a consistent format. For something",
                  "like topics or map types without a clear classification scheme it's",
                  "even more time consuming. So be aware with all these stats that",
                  "they're necessarily incomplete for now."),
                p("I've managed to get through over half the tweets and assign",
                  "them a challenge day. I've focussed on the easily identifiable",
                  "ones, so hopefully (timewise) a good proportion of the remainder are",
                  "just discussion rather than new maps.",
                  "Of those I have indexed, the majority initially only had a day",
                  "assigned (in the interest of time). Currently,",
                  span(class = "text-danger", glue("{pc_unc_area}%")),
                  "don't have an area assigned (ie continent or country) and",
                  span(class = "text-danger", glue("{pc_unc_city}%")),
                  "don't have a city assigned (though many don't need one);",
                  "these are usually pretty easily to determine manually (though it's slow) but",
                  "there's limited scope for automation.",
                  span(class = "text-danger", glue("{pc_unc_tool}%")),
                  "don't have any tools assigned; I've automated pretty much all I can here from",
                  "the tweets so the rest will have to come from the creators.",
                  span(class = "text-danger", glue("{pc_unc_type}%")),
                  "don't have the type of map assigned and",
                  span(class = "text-danger", glue("{pc_unc_topic}%")),
                  "don't have topics assigned; both of these require manual inspection and assessment",
                  "which is quite slow."),
                h3("Daily Themes"),
                img(src = "images/challenge_count.png"),
                h3("People"),
                p(glue("There were {nrow(full30)} people who managed the massive task of creating all 30 maps!",
                       "(If you're not on this list and should be then let me know!)")),
                tags$ul(
                  full30 %>%
                    pull(handle) %>%
                    map(~ tags$li(a(href = glue("https://twitter.com/{.x}/"), glue("@{.x}"))))
                  ),
                p(tags$em("I'll aim to identify the location of the map authors, but haven't done that yet.")),
                h3("Places"),
                p(glue("Bear in mind that only {100 - pc_unc_area}% of the maps have an area assigned,"),
                  "so this might reflect my interests to start with."),
                p("The main bar is the number of maps with that label.",
                  "The small orange bar is the number of cartographers who have produced the maps in that area."),
                img(src = "images/area_count.png"),
                h3("Tools"),
                p("Only approximately one third of the tweets mention the tools used."),
                img(src = "images/tool_count.png"),
                h3("Map Types"),
                p(tags$em("Best to wait until we have more data.")),
                h3("Topics"),
                p(tags$em("Best to wait until we have more data.")),
            )
        ),
    ),
    stats_footer,
    collapse = "\n"
)

write_file(stats_page, "stats.html")
        




# Make Maps ---------------------------------------------------------------


# > Cards -----------------------------------------------------------------

make_a_card <- 
  function(mapid, extension, Day, Theme, handle, date_posted, tweet_id, area, city, topics, types, tools, aspect, aspect_class, ...) {
    div(class = glue("map-card {aspect_class}"),
        `data-challenge` = Day,
        `data-area` = area,
        `data-city` = city,
        `data-topics` = topics,
        `data-types` = types,
        `data-tools` = tools,
        `data-date-posted` = date_posted,
        `data-handle` = handle %>% str_to_lower(),
        a(`data-toggle` = "modal",
          `data-target` = glue("#{mapid}_details"),
          div(class = "card",
              div(class = glue("aspect aspect--{aspect}"),
                  div(class = "aspect__inner",
                      img(class = "card-img lazyload",
                          `data-src` = glue("thumbnails/{mapid}.{extension}"))
                  )
              ),
              div(class = "card-img-overlay",
                  span(class = "badge badge-pill badge-warning",
                       Day),
                  span(class = "badge badge-pill badge-primary",
                       Theme),
                  span(class = "badge badge-pill badge-secondary",
                       handle)
              )
          )
        )
    )
  }

map_cards <- 
  map_metadata %>% 
  inner_join(map_classification, by = c("handle", "Day")) %>% 
  inner_join(challenges, by = "Day") %>% 
  inner_join(aspect_cols, by = "aspect") %>% 
  pmap(make_a_card)




# > Modals ----------------------------------------------------------------

make_a_modal <- 
  function(mapid, extension, Day, Theme, handle, date_posted, tweet_id, website, area, city, topics, types, tools, description, username, realname, location, ...) {
    div(id = glue("{mapid}_details"),
        class = "modal fade",
        tabindex = "-1",
        role = "dialog",
        div(class = "modal-dialog modal-xl",
            role = "document",
            div(class = "modal-content",
                div(class = "modal-header",
                    h5(class = "modal-title",
                       glue("Day {Day} ({Theme}) by {handle}")),
                    tags$button(type = "button", class = "close",
                                `data-dismiss` = "modal",
                                `aria-label` = "Close",
                                span(`aria-hidden` = "true",
                                     "×"))
                    ),
                div(class = "modal-body",
                    img(style = "max-width: 100%;",
                        class = "lazyload",
                        `data-src` = glue("maps/{mapid}.{extension}")),
                    p(description),
                    p("By ",
                      if_else(is.na(realname),
                              glue("{username} (@{handle})"),
                              glue("{realname} (@{handle} / {username})")),
                      if (!is.na(location)) { glue("in {location}") }
                      ),
                    tags$ul(class = "list-group",
                            tags$li(class = "list-group-item",
                                    strong("Areas:"),
                                    if_else(area == "_",
                                            "unclassified",
                                            area %>% str_replace_all(",", "; "))),
                            if (city != "_") {
                              tags$li(class = "list-group-item",
                                      strong("Cities:"),
                                      city %>% str_replace_all(",", "; "))
                            },
                            tags$li(class = "list-group-item",
                                    strong("Topics:"),
                                    if_else(topics == "_",
                                            "unclassified",
                                            topics %>% str_replace_all(",", "; "))),
                            tags$li(class = "list-group-item",
                                    strong("Map Type:"),
                                    if_else(types == "_",
                                            "unclassified",
                                            types %>% str_replace_all(",", "; "))),
                            tags$li(class = "list-group-item",
                                    strong("Tools:"),
                                    if_else(tools == "_",
                                            "unknown",
                                            tools %>% str_replace_all(",", "; "))),
                           )
                    ),
                div(class = "modal-footer",
                    tags$button(type = "button", class = "btn btn-secondary",
                                `data-dismiss` = "modal",
                                "Close"),
                    tags$button(type = "button", class = "btn btn-info",
                                a(target = "_blank",
                                  href = glue("https://twitter.com/{handle}/status/{tweet_id}"),
                                  "See Tweet")),
                                  if (website != "_") {
                                    tags$button(type = "button", class = "btn btn-info",
                                                a(target = "_blank",
                                                  href = website,
                                                  "See webpage"))
                                  }
                    )
                )
            )
        )
  }

maps_modals <- 
  map_metadata %>% 
  inner_join(map_classification, by = c("handle", "Day")) %>% 
  inner_join(challenges, by = "Day") %>% 
  inner_join(cartographers, by = "handle") %>%
  pmap(make_a_modal) %>% 
  map(as.character) %>% 
  paste(collapse = "\n")





# > Filter Challenge ------------------------------------------------------

maps_filter_challenge_0110 <- 
    div(class = "col-12@sm",
        div(class = "dropdown",
            tags$button(class = "btn btn-primary dropdown-toggle",
                        type = "button",
                        id = "FilterDay0110",
                        `data-toggle` = "dropdown",
                        `aria-haspopup` = "true",
                        `aria-expanded` = "false",
                        "01-10"),
            div(class = "dropdown-menu mapfilter-challenge",
                `aria-labelledby` = "FilterDay0110",
                challenges %>% 
                  filter(Day <= "10") %>% 
                  pmap(~ tags$button(class = "dropdown-item",
                                     type = "button",
                                     `data-value` = .x,
                                     glue("{.x} {.y}")))
            )
        )
    )
maps_filter_challenge_1120 <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterDay1120",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "11-21"),
          div(class = "dropdown-menu mapfilter-challenge",
              `aria-labelledby` = "FilterDay1120",
              challenges %>% 
                filter(Day >= "11", Day <= "20") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   glue("{.x} {.y}")))
          )
      )
  )
maps_filter_challenge_2130 <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterDay2130",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "21-30"),
          div(class = "dropdown-menu mapfilter-challenge",
              `aria-labelledby` = "FilterDay2130",
              challenges %>% 
                filter(Day >= "21") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   glue("{.x} {.y}")))
          )
      )
  )




# > Filter Area -----------------------------------------------------------

maps_filter_area_continent <- 
    div(class = "col-12@sm",
        div(class = "dropdown",
            tags$button(class = "btn btn-info dropdown-toggle",
                        type = "button",
                        id = "FilterAreaContinent",
                        `data-toggle` = "dropdown",
                        `aria-haspopup` = "true",
                        `aria-expanded` = "false",
                        `data-toggle-tt` = "tooltip",
                        `data-placement` = "top",
                        title = "Only includes maps at this level, not all the individual countries",
                        "Continent"),
            div(class = "dropdown-menu mapfilter-area",
                `aria-labelledby` = "FilterAreaContinent",
                continents %>% 
                  pmap(~ tags$button(class = "dropdown-item",
                                    type = "button",
                                    `data-value` = .x,
                                    if (.x == "_") { "unclassified" } else { .x }))
            )
        )
    )
maps_filter_area_country_AF <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryAF",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Country A-F"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryAF",
              countries %>% 
                filter(area < "G") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   .x))
          )
      )
  )
maps_filter_area_country_GO <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryGO",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "G-O"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryGO",
              countries %>% 
                filter(area > "G", area < "P") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   .x))
          )
      )
  )
maps_filter_area_country_PZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryPZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "P-Z"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryPZ",
              countries %>% 
                filter(area > "P") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   .x))
          )
      )
  )



# > Filter City -----------------------------------------------------------

maps_filter_cities_AC <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesAC",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "City A-C"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesAC",
              cities %>% 
                filter(city < "D", city != "_") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   if (.x == "_") { "unclassified" } else { .x }))
          )
      )
  )
maps_filter_cities_DL <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesDL",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "D-L"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesDL",
              cities %>% 
                filter(city > "D", city < "M", city != "_") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   if (.x == "_") { "unclassified" } else { .x }))
          )
      )
  )
maps_filter_cities_MR <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesMR",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "M-R"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesMR",
              cities %>% 
                filter(city > "M", city < "S", city != "_") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   if (.x == "_") { "unclassified" } else { .x }))
          )
      )
  )
maps_filter_cities_SZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesSZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "S-Z"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesSZ",
              cities %>% 
                filter(city > "S" | city == "_") %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   if (.x == "_") { "unclassified" } else { .x }))
          )
      )
  )



# > Filter Topic -----------------------------------------------------------

maps_filter_topics <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterTopics",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Topics"),
          div(class = "dropdown-menu mapfilter-topic",
              `aria-labelledby` = "FilterTopics",
              topics %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   if (.x == "_") { "unclassified" } else { .x }))
          )
      )
  )



# > Filter Type -----------------------------------------------------------

maps_filter_types <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterTypes",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Map Types"),
          div(class = "dropdown-menu mapfilter-type",
              `aria-labelledby` = "FilterTypes",
              types_of_maps %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   if (.x == "_") { "unclassified" } else { .x }))
          )
      )
  )




# > Filter Tool -----------------------------------------------------------

maps_filter_tools <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterTools",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Tools"),
          div(class = "dropdown-menu mapfilter-tool",
              `aria-labelledby` = "FilterTools",
              tools %>% 
                pmap(~ tags$button(class = "dropdown-item",
                                   type = "button",
                                   `data-value` = .x,
                                   if (.x == "_") { "unclassified" } else { .x }))
          )
      )
  )


# > Sort ------------------------------------------------------------------

maps_sorting <-
  div(class = "col-12@sm",
      span(class = "button_legend",
           "Sort: "),
      div(class = "btn-group btn-group-toggle sort-options",
          `data-toggle` = "buttons",
          tags$label(class = "btn btn-success active",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "dom",
                                checked = NA),
                     "Default"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "challenge"),
                     "Day #"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "posted-old-new"),
                     "Tweet (old-new)"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "posted-new-old"),
                     "Tweet (new-old)"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "handle"),
                     "Handle")
          )
     )



# > Search Handle ---------------------------------------------------------

maps_search_handle <- 
  div(class = "col-12@sm",
      tags$input(type = "text",
                 class = "form-control mapfilter-handle-search",
                 placeholder = "Search Twitter Handle",
                 id = "searchHandle")
      )


# > Reset Filters ---------------------------------------------------------

maps_reset_filters <- 
  div(class = "col-12@sm",
      tags$button(type = "button",
                  class = "btn btn-danger mapfilter-reset",
                  id = "mapfilter-reset-button",
                  "Reset All Filters"
                  )
      )



# > Page & Save -----------------------------------------------------------
  
maps_page <- 
  paste(
    maps_header,
    maps_header_nav,
    div(class = 'container',
        h1("Map Gallery"),
        # filter/sort
        div(class = "row",
            maps_filter_challenge_0110, maps_filter_challenge_1120, maps_filter_challenge_2130,
            maps_filter_area_continent, maps_filter_area_country_AF, maps_filter_area_country_GO, maps_filter_area_country_PZ,
            maps_filter_cities_AC, maps_filter_cities_DL, maps_filter_cities_MR, maps_filter_cities_SZ,
            ),
        div(class = "row",
            maps_filter_topics,
            maps_filter_types,
            maps_filter_tools,
            maps_search_handle,
            maps_reset_filters),
        div(class = "row",
            maps_sorting
            ),
        ),
    # cards
    div(class = "container-fluid",
        div(id = "grid", class = "row my-shuffle-container",
            map_cards,
            div(class = "col-1 my-sizer-element"))),
    # modals
    maps_modals,
    # JS libraries & code
    maps_scripts,
    maps_footer,
    collapse = "\n"
  )


write_file(maps_page, "maps.html")
