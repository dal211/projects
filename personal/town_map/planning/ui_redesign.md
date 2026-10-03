# UI Redesign: Map-First Layout (Mobile Sheet + Web Sidebar)

**Status:** Implemented in `app.R`, `www/styles.css` and `www/layout.js`. Clickable mockup: `planning/mockup.html`.

**Goal:** Make the app look and feel closer to the MBTA mobile app: a full-screen map with a panel that can be shown or hidden.
- **Mobile:** a bottom sheet you drag up and down.
- **Web:** a left sidebar you can collapse and reopen.
- **Toggle:** a switch to choose between the two layouts.
- **Requirement:** everything that works in the app today keeps working.

## Approach

**One set of controls, two layouts.** The map fills the whole screen in both versions, and every control sits in a single panel that floats over it. The panel's styling depends on the mode:

- **Mobile:** the panel is a bottom sheet with a drag handle.
- **Web:** the panel is a left sidebar with a collapse button.

Switching modes only changes styling. The town picker, address search and other controls are the same Shiny inputs either way, so nothing is built twice or needs syncing.

**The toggle.** The app starts in mobile mode on screens narrower than about 768px and in web mode otherwise. A small 📱/🖥 switch flips between them, and the browser remembers the choice.

The app uses MapLibre with MapTiler tiles, not Mapbox. MapLibre is the open-source version of Mapbox's map library, so the MBTA look is achievable without switching.

## Mockups

**Mobile, sheet collapsed (most of the screen is map):**
```
┌──────────────────────────┐
│ 🔍 Search town or address│  ← floating search bar
│                      [ⓘ] │  ← info / legend
│      MAP                 │
│                      [📏]│  ← ruler
│                      [📱]│  ← mode toggle
│                          │
├──────────────────────────┤
│          ━━━             │  ← drag handle
│ Where Should I Live?     │
└──────────────────────────┘
```

**Mobile, sheet at half height after tapping Newton:**
```
┌──────────────────────────┐
│ 🔍 Search…               │
│      MAP (Newton outlined)│
├──────────────────────────┤
│          ━━━             │
│ ● Newton                 │
│   Newton Public Schools  │
│ Home $1.29M (+1.9%) Tax 0.97%│
│ School score 92nd pct    │
│ [Redfin] [Niche]  [Reset]│
└──────────────────────────┘
```

The sheet has three resting positions:
- **Collapsed:** about 15% of the screen.
- **Half:** about 50%.
- **Nearly full:** about 90%.

Dragging snaps the sheet to the nearest position. Tapping a town opens its details in the sheet instead of a map popup, because popups are cramped and cover the map.

**Web:**
```
┌─────────────┬──────────────────────────────────┐
│ Where Should│                       [ⓘ][📏][🖥]│
│ I Live?     │                                  │
│ Redfin·Niche│                                  │
│ ─────────── │             MAP                  │
│ Explore town│                                  │
│ [▼ select ] │                                  │
│ ─── OR ──── │                                  │
│ Address     │                                  │
│ [_________] │                                  │
│ [Find][Reset]                                  │
│ ─────────── │                                  │
│ Newton card │                                  │
│           «│                                  │
└─────────────┴──────────────────────────────────┘
```

The `«` button collapses the sidebar to a thin tab with `»` to reopen it. The map resizes to fill the freed space.

## Existing features in the new layout

| Feature | Mobile | Web |
|---|---|---|
| Town dropdown, highlight and zoom | Top of the sheet | Sidebar |
| Address search with "Searching…" state | Search bar opens the sheet | Sidebar |
| Reset map | In the sheet | Sidebar |
| Clicking a town | Details card in the sheet | Details card in the sidebar (no map popup) |
| Info / legend button | Top-right of the map | Top-right of the map |
| Ruler with driving distance | Top-right of the map | Top-right of the map |
| Commuter rail lines and stations | Unchanged | Unchanged |
| Zoom / compass buttons | Hidden (pinch to zoom) | Top-left, as now |
| Loading overlay | Unchanged | Unchanged |
| Locate me (new) | Top-right stack | Top-right stack |
| Redfin / Niche links | In the sheet | Sidebar header |

The current mobile "back to search" arrow button is removed, since the sheet replaces it.

To put a tapped town's details in a panel, the app needs to know which town was clicked. The map library reports clicks back to the R server, which renders the town card. At first the card reuses today's popup content; regrouping it into labelled sections (Housing, Schools, Size) is a separate follow-up. Hovering a town still shows its name as a tooltip. Commuter station labels stay as small popups.

## Dark theme and locate-me

**Dark theme (both layouts).**
- Switch the basemap from MapTiler's `streets-v2` to a dark MapTiler style (e.g. `streets-v2-dark`), the closest match to the MBTA screenshots.
- Define the panel, card, input and button colors once as CSS variables, so the whole UI stays consistent.
- Restyle the existing map overlays for a dark background:
  - the info/legend and ruler controls and their popups;
  - the school-quality fills (the green and purple tiers) and the town outlines;
  - the purple commuter rail lines and the white station dots.
- Check readability of the red highlight outline and the blue address pin on the dark map.
- No light/dark switch for now: the app is dark only.

**Locate-me button (new).**
- Uses MapLibre's built-in geolocate control, placed in the top-right stack with the info, ruler and mode-toggle buttons.
- Tapping it asks the browser for location permission, then centers the map on the user and shows a blue dot, like the MBTA app.
- Browsers only allow location on HTTPS or localhost. shinyapps.io is HTTPS, so this works when deployed.
- If permission is denied or location is unavailable, show a short message instead of failing silently.
- The location stays in the browser and isn't sent to the R server.


## Code changes

- **`app.R`:** replace `fluidPage` and `sidebarLayout` with a full-screen map and the floating panel. Server changes are small:
  - A handler for town clicks, to fill the details card.
  - A call to redraw the map after the sidebar opens or closes.
- **`www/styles.css` and `www/layout.js`:** move the roughly 300 lines of inline CSS and JavaScript out of `app.R` into these two files. shinyapps.io deploys `www/` automatically.
- **Bottom-sheet dragging:** about 80 lines of plain JavaScript, with no new R packages.

## Decisions needed

1. ~~Dark theme~~ **Decided:** yes. See "Dark theme and locate-me" above.
2. ~~Town details on web~~ **Decided:** details open inside the panels (bottom sheet on mobile, sidebar card on web). No town popups on the map.
3. ~~Toggle placement~~ **Decided:** a button on the map, in the top-right stack with the info and ruler buttons, so it stays reachable even when the panel is collapsed. If the mobile corner gets too crowded, revisit moving it into the sheet on mobile only.
4. ~~"Locate me" button~~ **Decided:** yes. See "Dark theme and locate-me" above.
5. ~~Split CSS/JS into `www/`~~ **Decided:** yes. `app.R` keeps data, layout and server logic; styling goes in `www/styles.css` and behavior in `www/layout.js`. Hosting and the shinyapps.io URL are unaffected, but check at deploy time that `www/` isn't excluded.

Optional next step: build a clickable mockup with the draggable sheet and mode toggle before changing `app.R`.
