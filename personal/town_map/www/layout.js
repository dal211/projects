// Town map: layout behavior and custom map controls.
// Mobile = draggable bottom sheet, web = collapsible left sidebar. One set of
// Shiny inputs lives in #panel; switching modes only swaps CSS classes.
(function () {
  'use strict';

  var MODE_KEY = 'townmap-mode';
  var state = { mode: 'web', sheet: 'half', panelOpen: true };
  var map = null;

  function $(s) { return document.querySelector(s); }
  function clamp(v, a, b) { return Math.max(a, Math.min(b, v)); }

  var ICONS = {
    phone: '<svg class="ctrl-svg" viewBox="0 0 24 24"><rect x="7" y="2.5" width="10" height="19" rx="2.5"/><path d="M11 18.5h2"/></svg>',
    desktop: '<svg class="ctrl-svg" viewBox="0 0 24 24"><rect x="3" y="4" width="18" height="12" rx="2"/><path d="M8 20h8M12 16v4"/></svg>',
    ruler: '<svg class="ctrl-svg" viewBox="0 0 24 24"><path d="M3 17L17 3l4 4L7 21z"/><path d="M7.5 12.5l2 2M10.5 9.5l2 2M13.5 6.5l2 2"/></svg>',
    info: '<svg class="ctrl-svg" viewBox="0 0 24 24"><circle cx="12" cy="12" r="9"/><path d="M12 11v5"/><circle cx="12" cy="7.8" r=".6"/></svg>'
  };

  // ---------- light / dark theme ----------
  // Set before the page draws so there's no flash of the wrong colors. Light is
  // the default; a saved choice wins. The basemap starts light (server default)
  // and is swapped by the server once it hears input$theme.
  var THEME_KEY = 'townmap-theme';
  state.theme = (function () {
    var saved = null;
    try { saved = localStorage.getItem(THEME_KEY); } catch (e) {}
    return saved === 'dark' ? 'dark' : 'light';
  })();
  state.mapTheme = 'light';
  state.serverReady = false;
  document.documentElement.setAttribute('data-theme', state.theme);

  // Marks the current theme in the settings menu
  function updateThemeUI() {
    document.querySelectorAll('[data-theme-choice]').forEach(function (b) {
      var on = b.getAttribute('data-theme-choice') === state.theme;
      b.classList.toggle('active', on);
      b.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
  }

  // ---------- settings menu ----------
  // Positioned next to the gear (below it, or above when the gear sits low,
  // e.g. on a collapsed mobile sheet)
  function placeSettings() {
    var pop = $('#settings-pop'), btn = $('#settings-btn'), st = $('#stage');
    var b = btn.getBoundingClientRect(), s = st.getBoundingClientRect();
    var top = b.bottom - s.top + 8;
    if (top + pop.offsetHeight > st.clientHeight - 8) top = b.top - s.top - pop.offsetHeight - 8;
    pop.style.top = Math.max(8, top) + 'px';
    pop.style.left = clamp(b.right - s.left - pop.offsetWidth, 8, st.clientWidth - pop.offsetWidth - 8) + 'px';
  }
  function setSettingsOpen(open) {
    var pop = $('#settings-pop');
    if (!pop) return;
    pop.classList.toggle('open', open);
    $('#settings-btn').setAttribute('aria-expanded', open ? 'true' : 'false');
    if (open) placeSettings();
  }

  // Keep the loading overlay up until the basemap matches the theme, so a
  // light-mode visitor doesn't see the dark map flash first
  function maybeHideOverlay() {
    if (!state.serverReady || state.mapTheme !== state.theme) return;
    var overlay = document.getElementById('map-loading-overlay');
    if (overlay) overlay.style.display = 'none';
  }

  // Town fills need less opacity over the light basemap. Done here rather than
  // with mapgl's set_paint_property(), which errors while a new style loads.
  var TOWN_PAINT = {
    dark: { 'fill-opacity': 0.28, 'fill-outline-color': '#56606E' },
    light: { 'fill-opacity': 0.2, 'fill-outline-color': '#8C96A3' }
  };
  function tintTowns(theme) {
    if (!map || !map.getLayer('towns')) return;
    Object.keys(TOWN_PAINT[theme]).forEach(function (k) { map.setPaintProperty('towns', k, TOWN_PAINT[theme][k]); });
    applyTiers();
    applyRail();
  }

  // ---------- school tier fills (settings menu) ----------
  // A hidden tier's towns get a transparent fill: outlines stay and the towns
  // can still be clicked for their details. Choice is remembered per browser.
  var TIER_KEY = 'townmap-tiers';
  var TIER_COLOR = { 1: '#009688', 2: '#AB47BC' }; // the fill_color values set in 02_preprocessing.R
  state.tiers = (function () {
    var t = { 1: true, 2: true };
    try {
      var saved = JSON.parse(localStorage.getItem(TIER_KEY) || 'null');
      if (saved) { t[1] = saved[1] !== false; t[2] = saved[2] !== false; }
    } catch (e) {}
    return t;
  })();

  function applyTiers() {
    if (!map || !map.getLayer('towns')) return;
    map.setPaintProperty('towns', 'fill-color', ['case',
      ['==', ['get', 'fill_color'], TIER_COLOR[1]], state.tiers[1] ? TIER_COLOR[1] : 'rgba(0,0,0,0)',
      ['==', ['get', 'fill_color'], TIER_COLOR[2]], state.tiers[2] ? TIER_COLOR[2] : 'rgba(0,0,0,0)',
      ['get', 'fill_color']
    ]);
  }

  function setTier(tier, on) {
    state.tiers[tier] = on;
    try { localStorage.setItem(TIER_KEY, JSON.stringify(state.tiers)); } catch (e) {}
    applyTiers();
  }

  // ---------- commuter rail layer (settings menu) ----------
  var RAIL_KEY = 'townmap-rail';
  var RAIL_LAYERS = ['commuter', 'commuter_stations'];
  state.rail = (function () {
    try { return localStorage.getItem(RAIL_KEY) !== 'off'; } catch (e) { return true; }
  })();

  function applyRail() {
    if (!map) return;
    RAIL_LAYERS.forEach(function (id) {
      if (map.getLayer(id)) map.setLayoutProperty(id, 'visibility', state.rail ? 'visible' : 'none');
    });
  }

  function setRail(on) {
    state.rail = on;
    try { localStorage.setItem(RAIL_KEY, on ? 'on' : 'off'); } catch (e) {}
    applyRail();
  }

  function sendTheme() {
    if (!map || !window.Shiny || !Shiny.setInputValue) return;
    var want = state.theme;
    if (want !== state.mapTheme) {
      // mapgl re-fits the map to its initial bounds when a new style loads, so
      // put the camera back where the user had it
      var cam = { center: map.getCenter(), zoom: map.getZoom(), bearing: map.getBearing(), pitch: map.getPitch() };
      map.once('style.load', function () {
        map.jumpTo(cam);
        map.once('idle', function () {
          map.jumpTo(cam);
          state.mapTheme = want;
          tintTowns(want);
          maybeHideOverlay();
        });
      });
    }
    Shiny.setInputValue('theme', want);
  }

  function setTheme(theme) {
    state.theme = theme;
    document.documentElement.setAttribute('data-theme', theme);
    try { localStorage.setItem(THEME_KEY, theme); } catch (e) {}
    updateThemeUI();
    sendTheme();
  }

  // ---------- toast ----------
  var toastTimer;
  function toast(msg) {
    var el = $('#toast');
    if (!el) return;
    el.textContent = msg;
    el.classList.add('show');
    clearTimeout(toastTimer);
    toastTimer = setTimeout(function () { el.classList.remove('show'); }, 2800);
  }

  // ---------- layout state ----------
  function sheetHeights() {
    var h = $('#stage').clientHeight;
    // Collapsed = just the handle, title, Explore/About tabs and credits footer
    var peek = ['#grip', '#phead', '#tabbar', '#map-credit'].reduce(function (sum, s) {
      var el = $(s);
      if (!el) return sum;
      var cs = getComputedStyle(el);
      return sum + el.offsetHeight + parseFloat(cs.marginTop) + parseFloat(cs.marginBottom);
    }, 0);
    return { collapsed: Math.max(peek, 72), half: Math.round(h * 0.5), full: Math.round(h * 0.92) };
  }

  function setSheet(snap, instant) {
    state.sheet = snap;
    setSettingsOpen(false); // the menu is pinned to the gear's old position
    if (state.mode !== 'mobile') return;
    var p = $('#panel');
    p.style.transition = instant ? 'none' : 'height .3s cubic-bezier(.2,.8,.2,1)';
    p.style.height = sheetHeights()[snap] + 'px';
    p.classList.toggle('is-collapsed', snap === 'collapsed'); // hides the body so nothing peeks
  }

  // Explore (town picker + details) or About (methodology)
  function setView(view) {
    var p = $('#panel');
    if (!p || p.getAttribute('data-view') === view) return;
    p.setAttribute('data-view', view);
    document.querySelectorAll('#tabbar [data-tab]').forEach(function (b) {
      var on = b.getAttribute('data-tab') === view;
      b.classList.toggle('active', on);
      b.setAttribute('aria-selected', on ? 'true' : 'false');
    });
    var body = view === 'about' ? $('#about-view') : $('#pbody');
    if (body) body.scrollTop = 0;
  }

  function setPanel(open) {
    state.panelOpen = open;
    setSettingsOpen(false);
    $('#app').classList.toggle('panel-closed', !open);
  }

  function resizeMap() {
    if (!map) return;
    map.resize();
    setTimeout(function () { map.resize(); }, 350); // after the frame's CSS settles
  }

  function applyMode(mode) {
    state.mode = mode;
    setSettingsOpen(false);
    var app = $('#app');
    app.classList.remove('mode-web', 'mode-mobile');
    app.classList.add('mode-' + mode);
    var p = $('#panel');
    p.style.transition = 'none';
    p.style.height = '';
    if (mode === 'mobile') setSheet(state.sheet, true);
    var btn = $('#mode-toggle-btn');
    if (btn) {
      btn.innerHTML = mode === 'web' ? ICONS.phone : ICONS.desktop;
      btn.title = mode === 'web' ? 'Switch to mobile layout' : 'Switch to web layout';
    }
    resizeMap();
  }

  // Map padding so fitted features land in the part of the map the panel doesn't cover
  function padding(snap) {
    var st = $('#stage'), w = st.clientWidth, h = st.clientHeight, p;
    if (state.mode === 'web') {
      p = { top: 40, bottom: 40, left: (state.panelOpen ? 356 : 0) + 40, right: 80 };
    } else {
      p = { top: 90, bottom: sheetHeights()[snap || state.sheet] + 28, left: 24, right: 64 };
    }
    var maxV = Math.max(0, h - 80), maxH = Math.max(0, w - 80);
    if (p.top + p.bottom > maxV) { var kv = maxV / (p.top + p.bottom); p.top *= kv; p.bottom *= kv; }
    if (p.left + p.right > maxH) { var kh = maxH / (p.left + p.right); p.left *= kh; p.right *= kh; }
    return p;
  }

  // ---------- bottom-sheet dragging ----------
  var drag = null;
  function sheetDown(e) {
    if (state.mode !== 'mobile') return;
    if (e.target.closest('a, button, input, select')) return;
    drag = { y: e.clientY, h: $('#panel').getBoundingClientRect().height, last: e.clientY,
             lastT: performance.now(), v: 0, moved: false, id: e.pointerId, el: e.currentTarget };
    // Capture now, not on the first move: a quick flick can leave the thin
    // handle before it registers, and the moves would go to the map instead
    try { e.currentTarget.setPointerCapture(e.pointerId); } catch (err) {}
  }
  function sheetMove(e) {
    if (!drag) return;
    var dy = e.clientY - drag.y;
    if (!drag.moved && Math.abs(dy) > 4) {
      drag.moved = true;
      $('#panel').style.transition = 'none';
      $('#panel').classList.remove('is-collapsed'); // show the content while dragging
    }
    if (!drag.moved) return;
    var H = sheetHeights(), now = performance.now();
    drag.v = (e.clientY - drag.last) / Math.max(1, now - drag.lastT);
    drag.last = e.clientY;
    drag.lastT = now;
    $('#panel').style.height = clamp(drag.h - dy, H.collapsed, H.full) + 'px';
  }
  function sheetUp() {
    if (!drag) return;
    var d = drag;
    drag = null;
    if (!d.moved) { // a tap cycles collapsed -> half -> full -> collapsed
      setSheet(state.sheet === 'collapsed' ? 'half' : state.sheet === 'half' ? 'full' : 'collapsed');
      return;
    }
    // Snap to the nearest resting height, biased by flick speed
    var H = sheetHeights(), proj = $('#panel').getBoundingClientRect().height - d.v * 220;
    var best = 'collapsed', bestDist = Infinity;
    ['collapsed', 'half', 'full'].forEach(function (k) {
      var dist = Math.abs(H[k] - proj);
      if (dist < bestDist) { bestDist = dist; best = k; }
    });
    setSheet(best);
  }

  // ---------- page setup ----------
  document.addEventListener('DOMContentLoaded', function () {
    var stored = null;
    try { stored = localStorage.getItem(MODE_KEY); } catch (e) {}
    applyMode(stored === 'mobile' || stored === 'web' ? stored : (window.innerWidth < 768 ? 'mobile' : 'web'));

    ['#grip', '#phead'].forEach(function (sel) {
      var el = $(sel);
      el.addEventListener('pointerdown', sheetDown);
      el.addEventListener('pointermove', sheetMove);
      el.addEventListener('pointerup', sheetUp);
      el.addEventListener('pointercancel', function () { drag = null; });
    });

    $('#collapse-btn').addEventListener('click', function () { setPanel(false); });
    updateThemeUI();
    $('#settings-btn').addEventListener('click', function (e) {
      e.stopPropagation();
      setSettingsOpen(!$('#settings-pop').classList.contains('open'));
    });
    $('#settings-pop').addEventListener('click', function (e) { e.stopPropagation(); });
    document.querySelectorAll('[data-theme-choice]').forEach(function (b) {
      b.addEventListener('click', function () { setTheme(b.getAttribute('data-theme-choice')); });
    });
    document.querySelectorAll('input[data-tier]').forEach(function (cb) {
      var tier = cb.getAttribute('data-tier');
      cb.checked = state.tiers[tier];
      cb.addEventListener('change', function () { setTier(tier, cb.checked); });
    });
    var railCb = $('input[data-layer="rail"]');
    if (railCb) {
      railCb.checked = state.rail;
      railCb.addEventListener('change', function () { setRail(railCb.checked); });
    }
    document.addEventListener('click', function () { setSettingsOpen(false); });
    document.addEventListener('keydown', function (e) { if (e.key === 'Escape') setSettingsOpen(false); });
    $('#expand-tab').addEventListener('click', function () { setPanel(true); });
    document.querySelectorAll('#tabbar [data-tab]').forEach(function (b) {
      b.addEventListener('click', function () {
        setView(b.getAttribute('data-tab'));
        // On a collapsed mobile sheet, open it so the chosen view is visible
        if (state.mode === 'mobile' && state.sheet === 'collapsed') setSheet('half');
      });
    });
    $('#m-search').addEventListener('click', function () {
      setView('explore');
      setSheet('full');
      setTimeout(function () { var a = $('#addr_query'); if (a) a.focus(); }, 320);
    });
    $('#addr_query').addEventListener('keydown', function (e) {
      if (e.key === 'Enter') { var b = $('#addr_go'); if (b) b.click(); }
    });

    window.addEventListener('resize', function () {
      if (state.mode === 'mobile') setSheet(state.sheet, true);
    });
  });

  // ---------- messages from the server ----------
  Shiny.addCustomMessageHandler('toast', toast);

  // Fit the map to a bbox [x0, y0, x1, y1], opening the panel/sheet first if asked
  Shiny.addCustomMessageHandler('focus', function (msg) {
    if (msg.panel) { setPanel(true); setView('explore'); } // a town was picked: show its card
    if (msg.sheet) setSheet(msg.sheet, msg.instant);
    if (!map) return;
    var b = msg.bbox;
    map.fitBounds([[b[0], b[1]], [b[2], b[3]]], {
      padding: padding(msg.sheet),
      maxZoom: msg.maxZoom || 13,
      duration: msg.instant ? 0 : 700
    });
  });

  Shiny.addCustomMessageHandler('map-ready', function (msg) { // Shiny requires exactly one argument
    state.serverReady = true;
    maybeHideOverlay();
    // Never leave the overlay up for good if a style swap stalls
    setTimeout(function () { state.mapTheme = state.theme; maybeHideOverlay(); }, 8000);
  });

  // Mode toggle, locate-me, town clicks. Runs once the map widget exists.
  Shiny.addCustomMessageHandler('attach-layout', function (id) {
    var root = document.getElementById(id);
    var widget = HTMLWidgets.find('#' + id);
    if (!root || !widget || !widget.getMap()) return;
    map = widget.getMap();
    var corner = root.querySelector('.maplibregl-ctrl-top-right');
    if (!corner || corner.querySelector('.mode-toggle-ctrl')) return; // avoid dup on hot-reload

    // Map credits: show the attribution text in the panel footer instead of on
    // the map. The map's own control is hidden by CSS, except as a collapsed
    // (i) button while the web sidebar is closed, so the credit stays visible.
    function syncCredits() {
      var a = root.querySelector('.maplibregl-ctrl-attrib');
      var inner = root.querySelector('.maplibregl-ctrl-attrib-inner');
      var foot = $('#map-credit');
      if (inner && foot && inner.innerHTML && foot.innerHTML !== inner.innerHTML) {
        foot.innerHTML = inner.innerHTML;
        if (state.mode === 'mobile') setSheet(state.sheet, true); // footer height changed
      }
      if (a) { // MapLibre starts it expanded; start collapsed instead
        a.classList.remove('maplibregl-compact-show');
        a.removeAttribute('open');
      }
    }
    syncCredits();
    map.once('load', syncCredits);
    map.once('idle', syncCredits); // attribution text fills in as sources load
    setTimeout(syncCredits, 1500);

    // Mobile / web toggle
    var group = document.createElement('div');
    group.className = 'maplibregl-ctrl maplibregl-ctrl-group mode-toggle-ctrl';
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.id = 'mode-toggle-btn';
    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      var next = state.mode === 'web' ? 'mobile' : 'web';
      try { localStorage.setItem(MODE_KEY, next); } catch (err) {}
      applyMode(next);
    });
    group.appendChild(btn);
    corner.appendChild(group);
    applyMode(state.mode); // sets the button icon

    // Tell the server the theme once the map (and mapgl's layers) have loaded,
    // so set_style() has layers to carry over to the new basemap
    // (also applies any saved tier choices now that the towns layer exists)
    function onMapLoaded() { applyTiers(); applyRail(); sendTheme(); }
    if (map.loaded()) setTimeout(onMapLoaded, 0); else map.once('idle', onMapLoaded);

    // Locate me. The position stays in the browser; nothing is sent to Shiny.
    var geo = new maplibregl.GeolocateControl({
      positionOptions: { enableHighAccuracy: true },
      trackUserLocation: false,
      fitBoundsOptions: { maxZoom: 13 }
    });
    map.addControl(geo, 'top-right');
    geo.on('error', function (e) {
      var code = e && (e.code || (e.error && e.error.code));
      toast(code === 1 ? 'Location permission denied' : 'Your location is unavailable');
    });
    geo.on('outofmaxbounds', function () { toast('Your location is outside the map area'); });

    // Town clicks open the town card in the panel (no map popup). Skipped when
    // the click was a ruler point, or landed on a station / address pin.
    map.on('click', 'towns', function (e) {
      var oe = e.originalEvent, f = e.features && e.features[0];
      setTimeout(function () {
        if (!f || (oe && oe.__ruler)) return;
        var other = ['commuter_stations', 'search_pt'].filter(function (l) { return map.getLayer(l); });
        if (other.length && map.queryRenderedFeatures(e.point, { layers: other }).length) return;
        Shiny.setInputValue('town_click', { name: f.properties.town_name, t: Date.now() }, { priority: 'event' });
      }, 0);
    });
    map.on('mouseenter', 'towns', function () { if (!map.getCanvas().style.cursor) map.getCanvas().style.cursor = 'pointer'; });
    map.on('mouseleave', 'towns', function () { if (map.getCanvas().style.cursor === 'pointer') map.getCanvas().style.cursor = ''; });
  });

  // Info button with the school-quality legend
  Shiny.addCustomMessageHandler('attach-tip', function (id) {
    var root = document.getElementById(id);
    if (!root) return;
    var corner = root.querySelector('.maplibregl-ctrl-top-right');
    if (!corner || corner.querySelector('.custom-tip-ctrl')) return;

    var group = document.createElement('div');
    group.className = 'maplibregl-ctrl maplibregl-ctrl-group custom-tip-ctrl';
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.title = 'Map info & legend';
    btn.innerHTML = ICONS.info;
    var panel = document.createElement('div');
    panel.className = 'info-panel';
    panel.innerHTML =
      '<h5>School Quality</h5>' +
      '<div class="legend-row"><span class="legend-swatch green"></span>&gt;70th percentile (Tier 1)</div>' +
      '<div class="legend-row"><span class="legend-swatch purple"></span>50&ndash;69th percentile (Tier 2)</div>' +
      '<div class="tip-text">Tip: Hold Ctrl + drag to tilt &amp; rotate.</div>';

    btn.addEventListener('click', function (e) { e.stopPropagation(); panel.classList.toggle('open'); });
    document.addEventListener('click', function () { panel.classList.remove('open'); });
    panel.addEventListener('click', function (e) { e.stopPropagation(); });

    group.appendChild(btn);
    group.appendChild(panel);
    corner.appendChild(group);
  });

  // Distance-ruler tool: click the button to arm it, click point A, click
  // point B, done. Any click before arming, or after B is placed, is a
  // normal map click (town cards etc. keep working) since the tool
  // auto-disarms itself the instant the second point lands.
  Shiny.addCustomMessageHandler('attach-distance-tool', function (id) {
    var root = document.getElementById(id);
    if (!root) return;
    var widget = HTMLWidgets.find('#' + id);
    if (!widget) return;
    var rmap = widget.getMap();
    if (!rmap) return;

    var corner = root.querySelector('.maplibregl-ctrl-top-right');
    if (!corner) return;
    if (corner.querySelector('.custom-ruler-ctrl')) return; // avoid dup on hot-reload

    var emptyFC = { type: 'FeatureCollection', features: [] };

    // MapLibre rejects addSource() until the style has loaded. Don't gate on
    // isStyleLoaded() though — it is far stricter than addSource needs, and
    // reports false whenever any basemap tile is still streaming in, which
    // would silently swallow clicks. Instead just attempt the setup, retry
    // until it takes, and remember once it has.
    var layersReady = false;
    function ensureLayers() {
      if (layersReady) return true;
      try {
        if (!rmap.getSource('ruler-line')) {
          // Road route sits underneath the straight-line reference.
          rmap.addSource('ruler-route', { type: 'geojson', data: emptyFC });
          rmap.addLayer({
            id: 'ruler-route-layer', type: 'line', source: 'ruler-route',
            layout: { 'line-cap': 'round', 'line-join': 'round' },
            paint: { 'line-color': '#34C26B', 'line-width': 5, 'line-opacity': 0.85 }
          });
          rmap.addSource('ruler-line', { type: 'geojson', data: emptyFC });
          rmap.addLayer({
            id: 'ruler-line-layer', type: 'line', source: 'ruler-line',
            paint: { 'line-color': '#4C8DFF', 'line-width': 3, 'line-dasharray': [2, 1] }
          });
          rmap.addSource('ruler-points', { type: 'geojson', data: emptyFC });
          rmap.addLayer({
            id: 'ruler-points-layer', type: 'circle', source: 'ruler-points',
            paint: {
              'circle-radius': 5, 'circle-color': '#fff',
              'circle-stroke-color': '#4C8DFF', 'circle-stroke-width': 2
            }
          });
        }
        layersReady = true;
      } catch (e) {
        return false; // style not ready yet; retried on load / next click
      }
      return true;
    }

    ensureLayers();
    rmap.on('load', ensureLayers);

    var btn = document.createElement('button');
    btn.type = 'button';
    btn.title = 'Measure distance';
    btn.innerHTML = ICONS.ruler;

    var measuring = false;
    var pointA = null;
    var distancePopup = null;
    var straightLabel = '';
    var routeToken = 0; // guards against a stale OSRM reply landing on a new measurement

    function clearRuler() {
      if (ensureLayers()) {
        rmap.getSource('ruler-line').setData(emptyFC);
        rmap.getSource('ruler-points').setData(emptyFC);
        rmap.getSource('ruler-route').setData(emptyFC);
      }
      routeToken++; // invalidate any in-flight driving lookup
      if (distancePopup) {
        distancePopup.remove();
        distancePopup = null;
      }
      pointA = null;
    }

    function setActive(active) {
      measuring = active;
      rmap.getCanvas().style.cursor = active ? 'crosshair' : '';
      btn.classList.toggle('active', active);
    }

    function popupHtml(primary, secondary) {
      return '<div class="ruler-primary">' + primary + '</div>' +
             '<div class="ruler-secondary">' + secondary + '</div>';
    }

    function placeA(lngLat) {
      if (!ensureLayers()) return;
      pointA = lngLat;
      rmap.getSource('ruler-points').setData({
        type: 'FeatureCollection',
        features: [{ type: 'Feature', geometry: { type: 'Point', coordinates: pointA } }]
      });
    }

    function finishAt(lngLat) {
      if (!ensureLayers()) return;
      var line = turf.lineString([pointA, lngLat]);
      var miles = turf.length(line, { units: 'miles' });
      var label = miles < 0.1
        ? Math.round(miles * 5280) + ' ft'
        : miles.toFixed(2) + ' mi';
      var mid = turf.along(line, miles / 2, { units: 'miles' });

      rmap.getSource('ruler-line').setData(line);
      rmap.getSource('ruler-points').setData({
        type: 'FeatureCollection',
        features: [
          { type: 'Feature', geometry: { type: 'Point', coordinates: pointA } },
          { type: 'Feature', geometry: { type: 'Point', coordinates: lngLat } }
        ]
      });

      straightLabel = label;
      if (distancePopup) distancePopup.remove();
      distancePopup = new maplibregl.Popup({
        closeButton: false,
        closeOnClick: false,
        anchor: 'top' // label hangs below the point, i.e. underneath the line
      })
        .setLngLat(mid.geometry.coordinates)
        .setHTML(popupHtml(label, 'Driving: calculating…'))
        .addTo(rmap);

      // Ask R for the driving route (OSRM). The straight-line number is
      // already on screen, so this only ever upgrades the label.
      if (window.Shiny && Shiny.setInputValue) {
        routeToken++;
        Shiny.setInputValue('ruler_ab', {
          ax: pointA[0], ay: pointA[1],
          bx: lngLat[0], by: lngLat[1],
          token: routeToken
        }, { priority: 'event' });
      }

      // Done after exactly two points; further clicks are ignored until
      // the tool is armed again via the button.
      setActive(false);
    }

    Shiny.addCustomMessageHandler('ruler-route', function (msg) {
      if (!distancePopup) return;
      if (msg.token !== routeToken) return; // superseded by a newer measurement

      if (!msg.ok) {
        distancePopup.setHTML(popupHtml(straightLabel, 'Driving route unavailable'));
        return;
      }

      var driveLabel = msg.miles < 0.1
        ? Math.round(msg.miles * 5280) + ' ft driving'
        : msg.miles.toFixed(2) + ' mi driving';
      var mins = Math.round(msg.minutes);
      var timeLabel = mins >= 60
        ? Math.floor(mins / 60) + ' hr ' + (mins % 60) + ' min'
        : mins + ' min';

      distancePopup.setHTML(
        popupHtml(driveLabel, timeLabel + ' · ' + straightLabel + ' straight-line')
      );

      if (ensureLayers() && msg.coords && msg.coords.length > 1) {
        rmap.getSource('ruler-route').setData({
          type: 'Feature',
          geometry: { type: 'LineString', coordinates: msg.coords }
        });
      }
    });

    rmap.on('click', function (e) {
      if (!measuring) return;
      if (e.originalEvent) e.originalEvent.__ruler = true; // tells the town-click handler to stand down
      var lngLat = [e.lngLat.lng, e.lngLat.lat];
      if (!pointA) {
        placeA(lngLat);
      } else {
        finishAt(lngLat);
      }
    });

    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      clearRuler();
      setActive(true);
      toast('Click point A, then point B');
    });

    var group = document.createElement('div');
    group.className = 'maplibregl-ctrl maplibregl-ctrl-group custom-ruler-ctrl';
    group.appendChild(btn);
    corner.appendChild(group);
  });
})();
