// The floating panel: a draggable bottom sheet in mobile mode, a collapsible
// left sidebar in web mode. Also the settings menu, the transit menu's
// placement, tabs and light/dark theme.
import { $, $$, clamp, store } from './util.js';

const MODE_KEY = 'townmap-mode';
const THEME_KEY = 'townmap-theme';

export const ui = {
  mode: 'web', sheet: 'half', panelOpen: true,
  theme: document.documentElement.getAttribute('data-theme') === 'dark' ? 'dark' : 'light' // set in index.html
};
let map = null;
let onThemeChange = () => {};

export function setMap(m) { map = m; }

// ---------- light / dark theme ----------
function updateThemeUI() {
  $$('[data-theme-choice]').forEach((b) => {
    const on = b.getAttribute('data-theme-choice') === ui.theme;
    b.classList.toggle('active', on);
    b.setAttribute('aria-pressed', on ? 'true' : 'false');
  });
}
function setTheme(theme) {
  if (theme === ui.theme) return;
  ui.theme = theme;
  document.documentElement.setAttribute('data-theme', theme);
  store.set(THEME_KEY, theme);
  updateThemeUI();
  onThemeChange(theme);
}

// ---------- settings menu ----------
// Next to the gear: below it, or above when the gear sits low (collapsed mobile sheet)
function placeSettings() {
  const pop = $('#settings-pop'), b = $('#settings-btn').getBoundingClientRect(), st = $('#stage');
  const s = st.getBoundingClientRect();
  let top = b.bottom - s.top + 8;
  if (top + pop.offsetHeight > st.clientHeight - 8) top = b.top - s.top - pop.offsetHeight - 8;
  pop.style.top = Math.max(8, top) + 'px';
  pop.style.left = clamp(b.right - s.left - pop.offsetWidth, 8, st.clientWidth - pop.offsetWidth - 8) + 'px';
}
export function setSettingsOpen(open) {
  const pop = $('#settings-pop');
  if (open) setTransitOpen(false); // one menu at a time
  pop.classList.toggle('open', open);
  $('#settings-btn').setAttribute('aria-expanded', open ? 'true' : 'false');
  if (open) placeSettings();
}

// ---------- transit menu (opened by the map's train button) ----------
// Left of the button and level with it; below it if the screen is too narrow
function placeTransit() {
  const pop = $('#transit-pop'), btn = $('#transit-btn'), st = $('#stage');
  if (!btn) return;
  const b = btn.getBoundingClientRect(), s = st.getBoundingClientRect();
  let left = b.left - s.left - pop.offsetWidth - 8, top = b.top - s.top;
  if (left < 8) {
    left = clamp(b.right - s.left - pop.offsetWidth, 8, st.clientWidth - pop.offsetWidth - 8);
    top = b.bottom - s.top + 8;
  }
  pop.style.left = left + 'px';
  pop.style.top = clamp(top, 8, Math.max(8, st.clientHeight - pop.offsetHeight - 8)) + 'px';
}
export function setTransitOpen(open) {
  const pop = $('#transit-pop'), btn = $('#transit-btn');
  if (open) setSettingsOpen(false);
  pop.classList.toggle('open', open);
  if (btn) {
    btn.classList.toggle('active', open);
    btn.setAttribute('aria-expanded', open ? 'true' : 'false');
  }
  if (open) placeTransit();
}

// ---------- bottom sheet heights ----------
function sheetHeights() {
  const h = $('#stage').clientHeight;
  // Collapsed = just the handle, title, tabs and credits footer
  const peek = ['#grip', '#phead', '#tabbar', '#map-credit'].reduce((sum, s) => {
    const el = $(s);
    if (!el) return sum;
    const cs = getComputedStyle(el);
    return sum + el.offsetHeight + parseFloat(cs.marginTop) + parseFloat(cs.marginBottom);
  }, 0);
  const collapsed = Math.max(peek, 72);
  // Half / full never open taller than the current view's content needs
  const fit = Math.max(collapsed, peek + bodyHeight());
  return {
    collapsed,
    half: Math.min(Math.round(h * 0.5), fit),
    full: Math.min(Math.round(h * 0.92), fit)
  };
}

// Natural height of the visible view's content (the body stretches to fill the
// sheet, so measure its children). The body is hidden on a collapsed sheet;
// un-hide it just for the measurement, within one frame.
function bodyHeight() {
  const p = $('#panel'), body = $(`.p-body[data-view="${p.getAttribute('data-view')}"]`);
  if (!body) return 0;
  const hidden = p.classList.contains('is-collapsed');
  if (hidden) p.classList.remove('is-collapsed');
  const top = body.getBoundingClientRect().top;
  let bottom = 0;
  for (const c of body.children) {
    const r = c.getBoundingClientRect();
    if (r.height) bottom = Math.max(bottom, r.bottom - top + body.scrollTop);
  }
  const h = bottom + parseFloat(getComputedStyle(body).paddingBottom);
  if (hidden) p.classList.add('is-collapsed');
  return Math.ceil(h);
}

// Re-fit the sheet after its content changes (town card, Saved table, tab switch)
let refitTimer;
export function refitSheet() {
  if (ui.mode !== 'mobile' || drag) return;
  clearTimeout(refitTimer);
  refitTimer = setTimeout(() => {
    if (ui.mode !== 'mobile' || drag) return;
    const p = $('#panel'), want = sheetHeights()[ui.sheet];
    if (Math.abs(p.getBoundingClientRect().height - want) < 2) return;
    p.style.transition = 'height .3s cubic-bezier(.2,.8,.2,1)';
    p.style.height = want + 'px';
  }, 30);
}

export function setSheet(snap, instant) {
  ui.sheet = snap;
  setSettingsOpen(false); // the menu is pinned to the gear's old position
  if (ui.mode !== 'mobile') return;
  const p = $('#panel');
  p.style.transition = instant ? 'none' : 'height .3s cubic-bezier(.2,.8,.2,1)';
  p.style.height = sheetHeights()[snap] + 'px';
  p.classList.toggle('is-collapsed', snap === 'collapsed'); // hides the body so nothing peeks
}

// Explore (town picker + details), Saved (starred towns) or About (methodology)
export function setView(view) {
  const p = $('#panel');
  if (p.getAttribute('data-view') === view) return;
  p.setAttribute('data-view', view);
  $$('#tabbar [data-tab]').forEach((b) => {
    const on = b.getAttribute('data-tab') === view;
    b.classList.toggle('active', on);
    b.setAttribute('aria-selected', on ? 'true' : 'false');
  });
  const body = $(`.p-body[data-view="${view}"]`);
  if (body) body.scrollTop = 0;
  refitSheet();
}

export function setPanel(open) {
  ui.panelOpen = open;
  setSettingsOpen(false);
  $('#app').classList.toggle('panel-closed', !open);
}

function resizeMap() {
  if (!map) return;
  map.resize();
  setTimeout(() => map.resize(), 350); // after the frame's CSS settles
}

function applyMode(mode) {
  ui.mode = mode;
  setSettingsOpen(false);
  setTransitOpen(false);
  const app = $('#app');
  app.classList.remove('mode-web', 'mode-mobile');
  app.classList.add('mode-' + mode);
  const p = $('#panel');
  p.style.transition = 'none';
  p.style.height = '';
  if (mode === 'mobile') setSheet(ui.sheet, true);
  $$('[data-mode-choice]').forEach((b) => {
    const on = b.getAttribute('data-mode-choice') === mode;
    b.classList.toggle('active', on);
    b.setAttribute('aria-pressed', on ? 'true' : 'false');
  });
  resizeMap();
}

// Map padding so fitted features land in the part of the map the panel doesn't cover
export function padding(snap) {
  const st = $('#stage'), w = st.clientWidth, h = st.clientHeight;
  const p = ui.mode === 'web'
    ? { top: 40, bottom: 40, left: (ui.panelOpen ? 356 : 0) + 40, right: 80 }
    : { top: 90, bottom: sheetHeights()[snap || ui.sheet] + 28, left: 24, right: 64 };
  const maxV = Math.max(0, h - 80), maxH = Math.max(0, w - 80);
  if (p.top + p.bottom > maxV) { const k = maxV / (p.top + p.bottom); p.top *= k; p.bottom *= k; }
  if (p.left + p.right > maxH) { const k = maxH / (p.left + p.right); p.left *= k; p.right *= k; }
  return p;
}

// Fit the map to a bbox [w, s, e, n], opening the panel / moving the sheet first
export function focusMap(bbox, { sheet, panel = false, maxZoom = 13, instant = false } = {}) {
  if (panel) { setPanel(true); setView('explore'); } // a town was picked: show its card
  if (sheet) setSheet(sheet, instant);
  if (!map) return;
  map.fitBounds([[bbox[0], bbox[1]], [bbox[2], bbox[3]]], {
    padding: padding(sheet),
    maxZoom,
    duration: instant ? 0 : 700
  });
}

// ---------- bottom-sheet dragging ----------
let drag = null;
function sheetDown(e) {
  if (ui.mode !== 'mobile') return;
  if (e.target.closest('a, button, input, select')) return;
  drag = { y: e.clientY, h: $('#panel').getBoundingClientRect().height, last: e.clientY, lastT: performance.now(), v: 0, moved: false };
  // Capture now, not on the first move: a quick flick can leave the thin
  // handle before it registers, and the moves would go to the map instead
  try { e.currentTarget.setPointerCapture(e.pointerId); } catch (err) {}
}
function sheetMove(e) {
  if (!drag) return;
  const dy = e.clientY - drag.y;
  if (!drag.moved && Math.abs(dy) > 4) {
    drag.moved = true;
    $('#panel').style.transition = 'none';
    $('#panel').classList.remove('is-collapsed'); // show the content while dragging
  }
  if (!drag.moved) return;
  const H = sheetHeights(), now = performance.now();
  drag.v = (e.clientY - drag.last) / Math.max(1, now - drag.lastT);
  drag.last = e.clientY;
  drag.lastT = now;
  $('#panel').style.height = clamp(drag.h - dy, H.collapsed, H.full) + 'px';
}
function sheetUp() {
  if (!drag) return;
  const d = drag;
  drag = null;
  const H = sheetHeights();
  if (!d.moved) { // a tap cycles collapsed -> half -> full -> collapsed (skipping full when it fits at half)
    setSheet(ui.sheet === 'collapsed' ? 'half' : ui.sheet === 'half' && H.full > H.half ? 'full' : 'collapsed');
    return;
  }
  // Snap to the nearest resting height, biased by flick speed
  const proj = $('#panel').getBoundingClientRect().height - d.v * 220;
  let best = 'collapsed', bestDist = Infinity;
  for (const k of ['collapsed', 'half', 'full']) {
    const dist = Math.abs(H[k] - proj);
    if (dist < bestDist) { bestDist = dist; best = k; }
  }
  setSheet(best);
}

// ---------- setup ----------
export function initPanel({ onTheme }) {
  onThemeChange = onTheme;
  const stored = store.get(MODE_KEY);
  applyMode(stored === 'mobile' || stored === 'web' ? stored : (window.innerWidth < 768 ? 'mobile' : 'web'));

  for (const sel of ['#grip', '#phead']) {
    const el = $(sel);
    el.addEventListener('pointerdown', sheetDown);
    el.addEventListener('pointermove', sheetMove);
    el.addEventListener('pointerup', sheetUp);
    el.addEventListener('pointercancel', () => { drag = null; });
  }

  $('#collapse-btn').addEventListener('click', () => setPanel(false));
  $('#expand-tab').addEventListener('click', () => setPanel(true));

  updateThemeUI();
  $('#settings-btn').addEventListener('click', (e) => {
    e.stopPropagation();
    setSettingsOpen(!$('#settings-pop').classList.contains('open'));
  });
  $('#settings-pop').addEventListener('click', (e) => e.stopPropagation());
  $('#transit-pop').addEventListener('click', (e) => e.stopPropagation());
  $$('[data-theme-choice]').forEach((b) => b.addEventListener('click', () => setTheme(b.getAttribute('data-theme-choice'))));
  $$('[data-mode-choice]').forEach((b) => b.addEventListener('click', () => {
    const mode = b.getAttribute('data-mode-choice');
    if (mode === ui.mode) return;
    store.set(MODE_KEY, mode);
    applyMode(mode);
  }));
  document.addEventListener('click', () => { setSettingsOpen(false); setTransitOpen(false); });
  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') { setSettingsOpen(false); setTransitOpen(false); }
  });

  $$('#tabbar [data-tab]').forEach((b) => b.addEventListener('click', () => {
    setView(b.getAttribute('data-tab'));
    // On a collapsed mobile sheet, open it so the chosen view is visible
    if (ui.mode === 'mobile' && ui.sheet === 'collapsed') setSheet('half');
  }));

  $('#m-search').addEventListener('click', () => {
    setView('explore');
    setSheet('full');
    setTimeout(() => $('#addr_query').focus(), 320);
  });

  window.addEventListener('resize', () => { if (ui.mode === 'mobile') setSheet(ui.sheet, true); });
  // The town card and Saved table are re-rendered in place; re-fit the sheet when they change
  new MutationObserver(refitSheet).observe($('#panel'), { childList: true, subtree: true });
}
