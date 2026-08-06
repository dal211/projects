# Planned Changes

## Completed

### Add SAT data to school scoring
Pulled live from the E2C Data Hub (same pattern as MCAS/AP — dynamic `max(sy)` query rather than a static CSV download) in [preprocessing.R](preprocessing.R#L94-L120) as `sat_scores`, joined into `towns_sf` by `DIST_CODE`. `sat_rank <- percent_rank(SAT_TOTAL)` added alongside `mcas_rank`/`ap_rank`; `normalized_school_score` now an equal 1/3 average of the three ranks.

### Add AP/SAT participation counts
`TESTS_TAKEN` (AP) and `TAKEN_CNT` (SAT) now pulled alongside the score fields ([preprocessing.R:78](preprocessing.R#L78), already present for SAT). Not yet used in any score/weighting — available for a future "college prep" participation-adjusted metric.

### Add college enrollment outcomes
`college_outcomes` in [preprocessing.R:122-131](preprocessing.R#L122-L131): reads the local `College_and_Career_Outcomes_of_High_School_Graduates_20250525.csv`, filters to immediate fall enrollment (`OUTCOME_YEAR == HS_GRAD_YEAR`) for the latest grad cohort, computes `college_bound_rate = OUTCOME_CNT / GRAD_CNT`, joined by `DIST_CODE`, surfaced in the popup after "High School Size Est."

### Fix mobile scroll trap
**Problem**: On mobile, `sidebarPanel`/`mainPanel` stack vertically (Bootstrap column collapse), and `maplibreOutput` is set to `height:100vh`. Once a user scrolls down past the header into the map, MapLibre GL captures touch-drag gestures as map pan, not page scroll — trapping the user on the map with no easy way to swipe back up to the search controls above it.

**Fix**: Added a fixed-position "back to search" button (`.mobile-search-btn` in [app.R](app.R), only shown below the 767px breakpoint) in the bottom-right corner, always reachable regardless of scroll position, that smooth-scrolls back to the top of the page on tap (icon: up arrow).

**Still open / not done**: this is the low-effort fix (option 1 of 3 considered). If it's not enough in practice, the fuller options are still on the table:
- Off-canvas drawer/bottom sheet for the sidebar on mobile, overlaying the map instead of stacking above it
- Fixed-height app shell (flex column, `100vh`, `overflow:hidden` on `body`) so there's no page-level scroll at all on mobile
- [ ] Test on an actual mobile viewport/device (not just a narrow desktop window) since touch-drag vs. mouse-drag behave differently on the MapLibre canvas

### Add loading feedback
**Problem**: Address search hit MapTiler over the network with no visual indication while in flight, and the initial map render had no loading indicator either.

**Fix** (in [app.R](app.R)):
- `#map-loading-overlay` covers the map with a spinner + "Loading map…" until the first render flush completes, then a `map-ready` custom message hides it permanently
- "Find address" button flips to "Searching…" with a spinning icon immediately on click; the actual (blocking) MapTiler call is deferred via `session$onFlushed` so the busy state reaches the browser before the slow part runs, then reverts via `tryCatch`/`finally` regardless of success/not-found/error

### Improve initial load time
**Problem**: cold R-process starts on shinyapps.io were paying for package loads and per-session work that weren't needed.

**Fix** (in [app.R](app.R)):
- Removed `library(tidyverse)` (~8 unused sub-packages — only `filter()`/`select()` were used, already covered by `dplyr`), `library(scales)` (formatting only happens at preprocessing time, not runtime), and `library(rsconnect)` (deploy-time only, not a runtime dependency)
- Hoisted the map widget build (`maplibre(...) |> add_navigation_control(...) |> ... |> add_categorical_legend(...)`) out of `server()` into a `base_map` object built once per R process instead of rebuilt/re-serialized on every browser session; `renderMaplibre({...})` now just returns it
- Added `st_simplify(dTolerance = 100)` to `commuter_shapes_sf` (matching the tolerance already used for `towns_sf` in preprocessing) to shrink the transit-line payload sent to the browser
- [ ] Visually verify the commuter rail lines still look correct after simplification (not yet checked in a live app)
