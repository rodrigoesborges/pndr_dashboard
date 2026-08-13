# AGENTS.md

Guidance for working in this repository. Read this before making changes.

## What this is

`pndrdashboard`: a [golem](https://thinkr-open.github.io/golem/) R package wrapping a
**Shiny dashboard** for the PNDR (Política Nacional de Desenvolvimento Regional). It is a
component of Brazil's *Sistema Nacional de Informações do Desenvolvimento Regional*: it
tracks Brazilian regional dynamics and monitors/evaluates the PNDR policy.

- UI text is **Portuguese (pt-BR)**; identifiers and code comments are English. Keep new UI labels in pt-BR.
- Everything is a work in progress: several modules are empty stubs, commented-out blocks, or experimental branches (e.g. a new SQLite-based relational data model).

## Repo layout

| Path | Purpose |
|---|---|
| `R/` | Package code. `app_ui.R`, `app_server.R`, `run_app.R`, `app_config.R`, plus one file per Shiny module (`mod_*.R`), plus golem utils. |
| `inst/app/www/` | Static assets (logos, favicon, `mdr_vinheta.mp4` splash video, `circ.gif`). Served via `golem_add_external_resources()` in `R/app_ui.R`. |
| `inst/golem-config.yml` | Golem config (`default`/`production`/`dev` profiles). Read with `get_golem_config()` (`R/app_config.R`). |
| `data/` | Package datasets, notably `basemap.rda` (municipality polygons, `LazyData: true`). |
| `data-raw/` | Scripts that prepare datasets (`basemap.R`, `populate_initialdb.R`). Not part of the built package (see `.Rbuildignore`). |
| `dadostat/` | Data-pipeline outputs: indicator RDS files (`Painel de Indicadores/Cálculo Painel de Indicadores/*.RDS`), bibliometric exports, map layers. Mostly gitignored/untracked working data. |
| `dev/` | One-off workflow scripts and the golem dev scaffold (`01_start.R`, `02_dev.R`, `03_deploy.R`). Excluded from package builds. |
| `dev/2024-09-Produto-1/` | Bookdown "Produto 1" report (bibliometric + PCI analysis) with its own `bib/`. |
| `skeleton/` | Standalone report template (`.Rmd` with fonts/bib), used for producing deliverables. |
| `dashboard_db.sqlite` + `pndr_dashboard.dumpfile.sql` | New relational database (SQLite) for indicator metadata/values. Work in progress (commit `e2528b2`). |
| `tests/` | testthat (edition 3) + spelling tests. |
| `app.R` | rsconnect entry point (do not remove the first comment; it's a deploy marker). |

`1_data` at repo root is a **symlink** to `dadostat/__Painel de Indicadores/1_data`.

## Commands

Run the app:

```r
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
pndrdashboard::run_app()
```

- `app.R` does the above plus `options("golem.app.prod" = TRUE)`.
- `dev/run_dev.R` is the dev-mode launcher: random port, `golem::document_and_reload()`, `golem.app.prod = FALSE`.

Tests / checks / docs:

```r
devtools::test()        # testthat suite (includes a slow "app launches" test via golem::expect_running)
devtools::check()       # R CMD check
devtools::document()    # NAMESPACE is roxygen-generated: "do not edit by hand"
devtools::build_readme()# README.md is generated from README.Rmd; edit the .Rmd only
attachment::att_amend_desc()  # see dev/02_dev.R
```

Deploy (Posit Connect / ShinyApps.io): `dev/03_deploy.R` shows the canonical
`rsconnect::deployApp(...)` call. It passes an explicit `appFiles` list
(`R/`, `inst/`, `data/`, `NAMESPACE`, `DESCRIPTION`, `app.R`) — **if you add top-level
assets the app needs at runtime, add them to that list**. `.rscignore` controls what
rsconnect skips.

## Architecture & control flow

Golem app: `run_app()` → `app_ui()` / `app_server()`.

- `app_ui()` renders three sibling modules: `mod_setup_dashboard_ui`, `mod_LandingPage_ui`, `mod_framegov_ui`.
- `app_server()` instantiates their servers plus `rede_policêntrica`, `competitividade_regional`, `cadeias_sustentaveis` (empty stub).
- `mod_framegov` is the visible shell: gov.br header/footer (`shinyGovBRstyle`) with a `tabsetPanel` of tabs ("Rede Policêntrica", "Convergência").
- `mod_setup_dashboard` is a global overlay: loading spinner, gear button, settings panel (language, dataset "bases" selection), draggable/resizable via `shinyjqui`.
- `mod_LandingPage` plays a splash video (`mdr_vinheta.mp4`) that fades out via jQuery. Note it also defines the **global** `mypallet()` helper used by the map module — the helper lives in this file, not in a utils file.
- `mod_rede_policêntrica` is the main working map: `mapgl::maplibre` (positron style), slider for year, select for indicator, `bindCache()`/`bindEvent()` reactives, `maplibre_proxy()` layer updates, `add_legend()`.

Data flow specifics:

- Module files perform **top-level `readRDS()` with relative paths** at load time
  (`mod_LandingPage.R:11`, `mod_rede_policêntrica.R:10` both read
  `dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/9_ind_objetivo_2.RDS`).
  This runs whenever the package is loaded, so **working directory must be the repo root**
  or these fail. Paths are not resolved via `app_sys()`.
- `basemap` (municipality sf polygons) is package data (`data/basemap.rda`), created by
  `data-raw/basemap.R`: `geobr::read_municipality(2020)` simplified with `ms_simplify`,
  `codmun = as.numeric(trunc(code_muni / 10))`, cast to POLYGON, EPSG 4326.
- The SQLite model (`dashboard_db.sqlite`, built by `data-raw/populate_initialdb.R`) is a
  star schema around `mdata` (indicators), `datagroup` (Objetivos/Eixos/…), `geoloc`/`local`
  (municipalities), `data_values` (mdata × local × refdate). It is experimental — the UI
  does not consume it yet; the app still reads the RDS files directly.

## Conventions

- Module pattern: `mod_<name>_ui(id)` / `mod_<name>_server(id)`, `ns <- NS(id)`, roxygen `@noRd` header, and the boilerplate footer comments `## To be copied in the UI` / `## To be copied in the server`.
- Heavy use of the native pipe `|>`.
- `@importFrom` / `@import` declarations in roxygen; `NAMESPACE` is regenerated — never hand-edit it.
- New modules are scaffolded with `golem::add_module(name = ..., with_test = TRUE)` (see `dev/02_dev.R`).

## Gotchas

- **DESCRIPTION is out of date.** `Imports:` lists only `config, golem, leaflet, pkgload, shiny, shinydashboard, shinyjqui, shinymaterial`, but the code uses many undeclared packages (`mapgl`, `dplyr`, `shinyGovBRstyle`, `shinybusy`, `shinyGovBRstyle`, …). `R CMD check` fails on undeclared imports; the app won't launch until missing packages are installed (`mapgl`, `shinyGovBRstyle`, `shinyjqui`, `shinymaterial` were absent from this machine as of writing). Use `attachment::att_amend_desc()` before relying on check results.
- **Accented characters in filenames**: `R/mod_convergência.R`, `R/mod_rede_policêntrica.R`, `dev/mapa_regiões_pci.R`. Shell quoting, git, and grep tooling can trip on these; quote paths explicitly.
- **Module namespacing bug (pre-existing)**: `mod_convergência.R:21` uses `leafletOutput("mapabase")` without `ns()`, colliding with the id used in `mod_rede_policêntrica`. Also, `app_server.R` has `mod_convergência_server` **commented out** while `mod_framegov_ui` still renders its tab — the "Convergência" tab is currently dead UI. Don't assume it works.
- **Global side-effects on load**: the top-level `readRDS()` calls in module files run at package load; they also make loading slow and fail outside the repo root (see Architecture).
- **Secrets**: `.Renviron` contains API keys (`OPENAI_API_KEY`, `SCOPUS_API`) used by dev scripts (e.g. `dev/bta-prov.R` uses `gptchatteR`). `data-raw/credenciais_bd_teste_local_grupo.txt` holds test DB credentials. Never print, log, or commit these.
- **README.md is generated** from `README.Rmd` — edit the source, not the output.
- The repo mixes an old GitLab identity (`desenvolvimentoregional`) and a GitHub one (`painelpndr`) in URLs/DESCRIPTION; install instructions reference `remotes::install_github("rodrigoesborges/painelpndr")`. Don't "fix" these casually.
- `.Rbuildignore` excludes `dev/`, `data-raw/`, `app.R`, `README.Rmd`, `.here`, `rsconnect`, `.rscignore` — dev tooling is intentionally kept out of built packages.

## Testing

- `tests/testthat/test-golem-recommended.R`: standard golem checks (ui/server formals, config profiles, `testServer(app_server, …)`, and an `app launches` test via `golem::expect_running(sleep = 5)` that actually boots the app — slow).
- `tests/spelling.R` uses `inst/WORDLIST` (pt-BR terms like PNDR, Painel, desenvolvimento). Add new Portuguese domain words there if the spelling test complains.
- No module-level tests exist yet; adding them with `golem::add_module(with_test = TRUE)` is the established pattern.
