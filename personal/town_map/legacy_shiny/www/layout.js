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
    ruler: '<svg class="ctrl-svg" viewBox="0 0 24 24"><path d="M3 17L17 3l4 4L7 21z"/><path d="M7.5 12.5l2 2M10.5 9.5l2 2M13.5 6.5l2 2"/></svg>',
    legend: '<svg class="ctrl-svg" viewBox="0 0 24 24"><rect x="3.5" y="4.5" width="4" height="4" rx="1"/><rect x="3.5" y="15.5" width="4" height="4" rx="1"/><path d="M11 6.5h9.5M11 17.5h9.5"/></svg>',
    train:'<svg class="ctrl-svg" viewBox="0 0 24 24"><rect x="5" y="3" width="14" height="14" rx="3"/><path d="M5 11h14M9 21l2-4M15 21l-2-4"/><circle cx="9" cy="14" r=".7"/><circle cx="15" cy="14" r=".7"/></svg>'
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
    if (open) setTransitOpen(false); // only one menu at a time
    pop.classList.toggle('open', open);
    $('#settings-btn').setAttribute('aria-expanded', open ? 'true' : 'false');
    if (open) placeSettings();
  }

  // ---------- transit panel (opened by the map's "Transit" button) ----------
  // Sits to the left of the button, level with it; drops below it if the
  // screen is too narrow for that
  function placeTransit() {
    var pop = $('#transit-pop'), btn = $('#transit-btn'), st = $('#stage');
    if (!pop || !btn) return;
    var b = btn.getBoundingClientRect(), s = st.getBoundingClientRect();
    var left = b.left - s.left - pop.offsetWidth - 8, top = b.top - s.top;
    if (left < 8) { left = clamp(b.right - s.left - pop.offsetWidth, 8, st.clientWidth - pop.offsetWidth - 8); top = b.bottom - s.top + 8; }
    pop.style.left = left + 'px';
    pop.style.top = clamp(top, 8, Math.max(8, st.clientHeight - pop.offsetHeight - 8)) + 'px';
  }
  function setTransitOpen(open) {
    var pop = $('#transit-pop'), btn = $('#transit-btn');
    if (!pop) return;
    if (open) setSettingsOpen(false);
    pop.classList.toggle('open', open);
    if (btn) {
      btn.classList.toggle('active', open);
      btn.setAttribute('aria-expanded', open ? 'true' : 'false');
    }
    if (open) placeTransit();
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
    applyTransit();
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

  // ---------- transit layers (settings menu) ----------
  // Each switch shows/hides a group of map layers; choices are remembered
  var TRANSIT = {
    rail: { key: 'townmap-rail', layers: ['commuter', 'commuter_stations'] }
  };
  state.transit = {};
  Object.keys(TRANSIT).forEach(function (name) {
    try { state.transit[name] = localStorage.getItem(TRANSIT[name].key) !== 'off'; } catch (e) { state.transit[name] = true; }
  });

  // T lines: one chip per line (Green's four branches share a chip). The shapes
  // carry a `line` property ("Red", "Green", ...); a station's `lines` property
  // lists every line serving it, so it shows while any of its lines is on.
  var TLINES_KEY = 'townmap-tlines';
  var TLINE_NAMES = ['Red', 'Orange', 'Blue', 'Green', 'Mattapan'];
  state.tlines = (function () {
    var on = {};
    var saved = null;
    try { saved = JSON.parse(localStorage.getItem(TLINES_KEY) || 'null'); } catch (e) {}
    // Off unless the user turned a line on; the commuter rail is the default transit layer
    TLINE_NAMES.forEach(function (n) { on[n] = !!(saved && saved[n] === true); });
    return on;
  })();
  state.tlinesReady = true;

  function applyTLines() {
    if (!map) return;
    var shown = TLINE_NAMES.filter(function (n) { return state.tlines[n]; });
    var vis = shown.length ? 'visible' : 'none';
    ['subway', 'subway_stations'].forEach(function (id) {
      if (map.getLayer(id)) map.setLayoutProperty(id, 'visibility', vis);
    });
    if (!shown.length) return;
    if (map.getLayer('subway')) {
      map.setFilter('subway', ['in', ['get', 'line'], ['literal', shown]]);
    }
    if (map.getLayer('subway_stations')) {
      map.setFilter('subway_stations', ['any'].concat(shown.map(function (n) { return ['in', n, ['get', 'lines']]; })));
    }
  }

  function setTLine(name, on) {
    state.tlines[name] = on;
    try { localStorage.setItem(TLINES_KEY, JSON.stringify(state.tlines)); } catch (e) {}
    applyTLines();
  }

  function applyTransit() {
    if (!map) return;
    Object.keys(TRANSIT).forEach(function (name) {
      TRANSIT[name].layers.forEach(function (id) {
        if (map.getLayer(id)) map.setLayoutProperty(id, 'visibility', state.transit[name] ? 'visible' : 'none');
      });
    });
    applyTLines();
  }

  function setTransit(name, on) {
    state.transit[name] = on;
    try { localStorage.setItem(TRANSIT[name].key, on ? 'on' : 'off'); } catch (e) {}
    applyTransit();
  }

  // ---------- saved (starred) towns ----------
  // The list lives in this browser. The server gets it as input$saved_towns and
  // builds the Saved tab's table, the CSV and the card's star state from it.
  // A ?saved=Town,Town link (from "Copy share link") adds its towns to the list.
  var SAVED_KEY = 'townmap-saved';
  state.saved = (function () {
    var s = null;
    try { s = JSON.parse(localStorage.getItem(SAVED_KEY) || '[]'); } catch (e) {}
    return Array.isArray(s) ? s.filter(function (x) { return typeof x === 'string'; }) : [];
  })();
  state.sharedAdded = 0;
  (function () {
    var search = location.search;
    try { if (window.top !== window) search = window.top.location.search || search; } catch (e) {} // shinyapps.io wraps the app in an iframe
    var shared = new URLSearchParams(search).get('saved');
    if (!shared) return;
    shared.split(',').forEach(function (t) {
      t = t.trim();
      if (t && state.saved.indexOf(t) < 0) { state.saved.push(t); state.sharedAdded++; }
    });
    try { localStorage.setItem(SAVED_KEY, JSON.stringify(state.saved)); } catch (e) {}
  })();

  function sendSaved() {
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('saved_towns', state.saved);
  }
  function updateSavedUI() {
    var n = state.saved.length, badge = $('#saved-count');
    if (badge) { badge.textContent = n; badge.style.display = n ? '' : 'none'; }
    var view = $('#saved-view');
    if (view) view.classList.toggle('is-empty', !n);
    // Flip stars right away; the server's re-render follows
    document.querySelectorAll('.c-star[data-star]').forEach(function (b) {
      var on = state.saved.indexOf(b.getAttribute('data-star')) >= 0;
      b.classList.toggle('saved', on);
      b.setAttribute('aria-pressed', on ? 'true' : 'false');
      b.title = on ? 'Remove from saved towns' : 'Save this town';
    });
  }
  function toggleSaved(town) {
    var i = state.saved.indexOf(town);
    if (i >= 0) state.saved.splice(i, 1); else state.saved.push(town);
    try { localStorage.setItem(SAVED_KEY, JSON.stringify(state.saved)); } catch (e) {}
    updateSavedUI();
    sendSaved();
    toast(i >= 0 ? town + ' removed from saved towns' : town + ' saved');
  }

  // Link that recreates this list in another browser
  function shareSaved() {
    if (!state.saved.length) return;
    var base = location.href;
    try { if (window.top !== window) base = window.top.location.href; } catch (e) {}
    var url = base.split(/[?#]/)[0] + '?saved=' + encodeURIComponent(state.saved.join(','));
    function fallback() { window.prompt('Copy this link to open your saved towns elsewhere:', url); }
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(url).then(function () { toast('Share link copied'); }, fallback);
    } else {
      fallback();
    }
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
    var collapsed = Math.max(peek, 72);
    // Half / full never open taller than the current view's content needs
    var fit = Math.max(collapsed, peek + bodyHeight());
    return {
      collapsed: collapsed,
      half: Math.min(Math.round(h * 0.5), fit),
      full: Math.min(Math.round(h * 0.92), fit)
    };
  }

  // Natural height of the visible view's content (the body itself stretches to
  // fill the sheet, so measure its children instead). The body is hidden on a
  // collapsed sheet; un-hide it just for the measurement, within one frame.
  function bodyHeight() {
    var p = $('#panel'), body = $('.p-body[data-view="' + (p && p.getAttribute('data-view')) + '"]');
    if (!body) return 0;
    var hidden = p.classList.contains('is-collapsed');
    if (hidden) p.classList.remove('is-collapsed');
    var top = body.getBoundingClientRect().top, bottom = 0;
    Array.prototype.forEach.call(body.children, function (c) {
      var r = c.getBoundingClientRect();
      if (r.height) bottom = Math.max(bottom, r.bottom - top + body.scrollTop);
    });
    var h = bottom + parseFloat(getComputedStyle(body).paddingBottom);
    if (hidden) p.classList.add('is-collapsed');
    return Math.ceil(h);
  }

  // Re-fit the sheet after its content changes (town card, Saved table, tab switch)
  var refitTimer;
  function refitSheet() {
    if (state.mode !== 'mobile' || drag) return;
    clearTimeout(refitTimer);
    refitTimer = setTimeout(function () {
      if (state.mode !== 'mobile' || drag) return;
      var p = $('#panel'), want = sheetHeights()[state.sheet];
      if (Math.abs(p.getBoundingClientRect().height - want) < 2) return;
      p.style.transition = 'height .3s cubic-bezier(.2,.8,.2,1)';
      p.style.height = want + 'px';
    }, 30);
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

  // Explore (town picker + details), Saved (starred towns) or About (methodology)
  function setView(view) {
    var p = $('#panel');
    if (!p || p.getAttribute('data-view') === view) return;
    p.setAttribute('data-view', view);
    document.querySelectorAll('#tabbar [data-tab]').forEach(function (b) {
      var on = b.getAttribute('data-tab') === view;
      b.classList.toggle('active', on);
      b.setAttribute('aria-selected', on ? 'true' : 'false');
    });
    var body = $('.p-body[data-view="' + view + '"]');
    if (body) body.scrollTop = 0;
    refitSheet();
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
    setTransitOpen(false);
    var app = $('#app');
    app.classList.remove('mode-web', 'mode-mobile');
    app.classList.add('mode-' + mode);
    var p = $('#panel');
    p.style.transition = 'none';
    p.style.height = '';
    if (mode === 'mobile') setSheet(state.sheet, true);
    document.querySelectorAll('[data-mode-choice]').forEach(function (b) {
      var on = b.getAttribute('data-mode-choice') === mode;
      b.classList.toggle('active', on);
      b.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
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
      // (skipping full when the content already fits at half)
      var T = sheetHeights();
      setSheet(state.sheet === 'collapsed' ? 'half'
        : state.sheet === 'half' && T.full > T.half ? 'full' : 'collapsed');
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
    document.querySelectorAll('[data-mode-choice]').forEach(function (b) {
      b.addEventListener('click', function () {
        var mode = b.getAttribute('data-mode-choice');
        if (mode === state.mode) return;
        try { localStorage.setItem(MODE_KEY, mode); } catch (err) {}
        applyMode(mode);
      });
    });
    document.querySelectorAll('input[data-tier]').forEach(function (cb) {
      var tier = cb.getAttribute('data-tier');
      cb.checked = state.tiers[tier];
      cb.addEventListener('change', function () { setTier(tier, cb.checked); });
    });
    document.querySelectorAll('.tchip[data-tline]').forEach(function (chip) {
      var name = chip.getAttribute('data-tline');
      chip.setAttribute('aria-pressed', state.tlines[name] ? 'true' : 'false');
      chip.addEventListener('click', function () {
        var on = chip.getAttribute('aria-pressed') !== 'true';
        chip.setAttribute('aria-pressed', on ? 'true' : 'false');
        setTLine(name, on);
      });
    });
    document.querySelectorAll('input[data-layer]').forEach(function (cb) {
      var name = cb.getAttribute('data-layer');
      cb.checked = state.transit[name];
      cb.addEventListener('change', function () { setTransit(name, cb.checked); });
    });
    $('#transit-pop').addEventListener('click', function (e) { e.stopPropagation(); });
    document.addEventListener('click', function () { setSettingsOpen(false); setTransitOpen(false); });
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Escape') { setSettingsOpen(false); setTransitOpen(false); }
    });
    $('#expand-tab').addEventListener('click', function () { setPanel(true); });
    document.querySelectorAll('#tabbar [data-tab]').forEach(function (b) {
      b.addEventListener('click', function () {
        setView(b.getAttribute('data-tab'));
        // On a collapsed mobile sheet, open it so the chosen view is visible
        if (state.mode === 'mobile' && state.sheet === 'collapsed') setSheet('half');
      });
    });
    // Saved towns: stars (town card, Saved table) and table rows, delegated
    // since the server re-renders both
    updateSavedUI();
    document.addEventListener('click', function (e) {
      var star = e.target.closest('[data-star]');
      if (star) { toggleSaved(star.getAttribute('data-star')); return; }
      var row = e.target.closest('[data-saved-town]');
      if (row) Shiny.setInputValue('town_click', { name: row.getAttribute('data-saved-town'), t: Date.now() }, { priority: 'event' });
    });
    $('#saved-share').addEventListener('click', shareSaved);
    window.jQuery(document).on('shiny:connected', function () {
      sendSaved();
      if (state.sharedAdded) {
        toast('Added ' + state.sharedAdded + ' town' + (state.sharedAdded > 1 ? 's' : '') + ' from a shared link');
        setView('saved');
      }
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
    // Shiny re-renders the town card and Saved table in place; re-fit the sheet when they change
    if (window.MutationObserver) {
      new MutationObserver(refitSheet).observe($('#panel'), { childList: true, subtree: true });
    }
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

  // Transit button, locate-me, town clicks. Runs once the map widget exists.
  Shiny.addCustomMessageHandler('attach-layout', function (id) {
    var root = document.getElementById(id);
    var widget = HTMLWidgets.find('#' + id);
    if (!root || !widget || !widget.getMap()) return;
    map = widget.getMap();
    var corner = root.querySelector('.maplibregl-ctrl-top-right');
    if (!corner || corner.querySelector('.transit-ctrl')) return; // avoid dup on hot-reload

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

    // Transit button (icon only): opens the commuter rail / T layers panel.
    // CSS `order` keeps it last in the stack.
    var tgroup = document.createElement('div');
    tgroup.className = 'maplibregl-ctrl maplibregl-ctrl-group transit-ctrl';
    var tbtn = document.createElement('button');
    tbtn.type = 'button';
    tbtn.id = 'transit-btn';
    tbtn.title = 'Show or hide commuter rail and T lines';
    tbtn.setAttribute('aria-haspopup', 'true');
    tbtn.setAttribute('aria-expanded', 'false');
    tbtn.innerHTML = ICONS.train;
    tbtn.addEventListener('click', function (e) {
      e.stopPropagation();
      setTransitOpen(!$('#transit-pop').classList.contains('open'));
    });
    tgroup.appendChild(tbtn);
    corner.appendChild(tgroup);

    // Tell the server the theme once the map (and mapgl's layers) have loaded,
    // so set_style() has layers to carry over to the new basemap
    // (also applies any saved tier choices now that the towns layer exists)
    function onMapLoaded() { applyTiers(); applyTransit(); sendTheme(); }
    if (map.loaded()) setTimeout(onMapLoaded, 0); else map.once('idle', onMapLoaded);
    // mapgl re-adds the layers after a basemap swap but drops their filters, and
    // the timing varies, so re-apply the layer choices whenever the map settles.
    // MapLibre ignores a setting that's already applied, so this is cheap.
    map.on('idle', function () { applyTiers(); applyTransit(); });

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
        var other = ['commuter_stations', 'subway_stations', 'search_pt'].filter(function (l) { return map.getLayer(l); });
        if (other.length && map.queryRenderedFeatures(e.point, { layers: other }).length) return;
        Shiny.setInputValue('town_click', { name: f.properties.town_name, t: Date.now() }, { priority: 'event' });
      }, 0);
    });
    map.on('mouseenter', 'towns', function () { if (!map.getCanvas().style.cursor) map.getCanvas().style.cursor = 'pointer'; });
    map.on('mouseleave', 'towns', function () { if (map.getCanvas().style.cursor === 'pointer') map.getCanvas().style.cursor = ''; });
  });

  // School-quality legend, always shown at the end of the control stack
  Shiny.addCustomMessageHandler('attach-tip', function (id) {
    var root = document.getElementById(id);
    if (!root) return;
    var corner = root.querySelector('.maplibregl-ctrl-top-right');
    if (!corner || corner.querySelector('.custom-tip-ctrl')) return;

    var legend = document.createElement('div');
    legend.className = 'maplibregl-ctrl custom-tip-ctrl';
    legend.innerHTML =
      '<button type="button" class="legend-head" aria-expanded="true" title="School quality legend"><h5>School Quality</h5><span class="legend-chev">&#9662;</span>' + ICONS.legend + '</button>' +
      '<div class="legend-body">' +
      '<div class="legend-row"><span class="legend-swatch green"></span>&gt;70th percentile (Tier 1)</div>' +
      '<div class="legend-row"><span class="legend-swatch purple"></span>50&ndash;69th percentile (Tier 2)</div>' +
      '</div>';
    // Collapsible; the choice is remembered
    var LEGEND_KEY = 'townmap-legend';
    var head = legend.querySelector('.legend-head');
    function setLegend(open) {
      legend.classList.toggle('collapsed', !open);
      head.setAttribute('aria-expanded', open ? 'true' : 'false');
      try { localStorage.setItem(LEGEND_KEY, open ? 'open' : 'closed'); } catch (e) {}
    }
    head.addEventListener('click', function (e) { e.stopPropagation(); setLegend(legend.classList.contains('collapsed')); });
    try { if (localStorage.getItem(LEGEND_KEY) === 'closed') setLegend(false); } catch (e) {}
    corner.appendChild(legend);
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
