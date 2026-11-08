library(tidyverse)
library(here)
library(sf)
library(jsonlite)

# hints
set.seed(12)

team_members <-
  c(
    "Adrian",
    "Ana",
    "Andrea P.",
    "Andrea Z.",
    "Christoph",
    "David",
    "Delia",
    "Jonas",
    "Laura",
    "Liliana",
    "Norah",
    "Pascal",
    "Philipp",
    "Piera",
    "Raphael",
    "Sara",
    "Stefan",
    "Till",
    # restliche Tage mit "Team" auffüllen
    # ohne 24., dieser ist fix fürs Team
    rep("Team", 5)
  )

hints <-
  read_csv(here("hints", "hints.csv")) |>
  pivot_longer(
    starts_with("hint_nr_"),
    names_prefix = "hint_nr_",
    names_to = "hint_nr",
    values_to = "hint",
    values_drop_na = TRUE
  ) |>
  group_by(gemeinde) |>
  nest("hints" = starts_with("hint")) |>
  ungroup() |>
  mutate(
    svg_gemeinde = paste0("maps/svg/", gemeinde, ".svg"),
    svg_kanton = "maps/svg/kanton.svg",
    svg_gewaesser = "maps/svg/gewaesser.svg"
  ) |>
  bind_cols(
    tag = sample(1:24),
    # 24. ist fix fürs Team
    team_member = c(sample(team_members), "Team")
  )

write_json(
  hints,
  here("hints", "hints.json"),
  pretty = TRUE
)

# maps

# verfügbare Layers abfragen
# st_layers(here("maps", "swisstopo", "swissBOUNDARIES3D_1_5_LV95_LN02.gpkg"))
# st_layers(here("maps", "swisstopo", "swissTLMRegio_Product_LV95.gpkg"))

kanton <-
  st_read(
    here("maps", "swisstopo", "swissBOUNDARIES3D_1_5_LV95_LN02.gpkg"),
    layer = "tlm_kantonsgebiet"
  ) |>
  filter(name == "Aargau")

gemeinden <-
  st_read(
    here("maps", "swisstopo", "swissBOUNDARIES3D_1_5_LV95_LN02.gpkg"),
    layer = "tlm_hoheitsgebiet"
  ) |>
  filter(kantonsnummer == 19)

# json mit allen Gemeinden für Lösungsvorschläge schreiben
write_json(
  sort(gemeinden$name),
  here("hints", "gemeinden.json"),
  pretty = TRUE
)

gewaesser_stehend <-
  st_read(
    here("maps", "swisstopo", "swissTLMRegio_Product_LV95.gpkg"),
    layer = "tlmregio_hydrography_lake"
  ) |>
  st_filter(kanton)

gewaesser_fliessend <-
  st_read(
    here("maps", "swisstopo", "swissTLMRegio_Product_LV95.gpkg"),
    layer = "tlmregio_hydrography_flowingwater"
  ) |>
  # Nur ausgewählte Fliessgewässer anzeigen
  filter(
    namn %in%
      c(
        "Aare",
        "Reuss",
        "Limmat",
        "Rhein",
        "Wigger",
        "Suhre",
        "Aabach",
        "Murg",
        "Möhlinbach",
        "Sissle",
        "Surb",
        "Bünz",
        "Wyna",
        "Uerke",
        "Pfaffnern"
      )
  )

# Begrenzung in x und y Richtung raussuchen, so dass alle maps
# immer denselben Abschnitt zeigen
limits <-
  kanton |>
  st_bbox()

save_maps <- function(gemeinde) {
  map_gemeinde <-
    ggplot() +
    geom_sf(
      data = filter(gemeinden, name == gemeinde),
      fill = "#B4D7A3",
      color = "#86A17A",
      size = 0.5
    ) +
    coord_sf(
      xlim = c(limits$xmin, limits$xmax),
      ylim = c(limits$ymin, limits$ymax)
    ) +
    theme_void()

  ggsave(
    here("maps", "svg", paste0(gemeinde, ".svg")),
    map_gemeinde,
    height = 10,
    width = 10
  )
}

hints |>
  pull(gemeinde) |>
  unique() |>
  walk(save_maps)

map_kanton <-
  ggplot() +
  geom_sf(data = kanton, fill = "#F4F5F4", color = "#8A8A8A", size = 0.5) +
  coord_sf(
    xlim = c(limits$xmin, limits$xmax),
    ylim = c(limits$ymin, limits$ymax)
  ) +
  theme_void()

ggsave(here("maps", "svg", "kanton.svg"), map_kanton, height = 10, width = 10)

map_gewaesser <-
  ggplot() +
  geom_sf(
    data = bind_rows(gewaesser_fliessend, gewaesser_stehend),
    fill = "#D8EFFE",
    color = "#448EC1",
    size = 0.4
  ) +
  coord_sf(
    xlim = c(limits$xmin, limits$xmax),
    ylim = c(limits$ymin, limits$ymax)
  ) +
  theme_void()

ggsave(
  here("maps", "svg", "gewaesser.svg"),
  map_gewaesser,
  height = 10,
  width = 10
)
