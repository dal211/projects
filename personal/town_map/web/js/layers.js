// Map layers (towns, commuter rail, T lines, highlight, address pin), their
// settings (school tiers, transit) and hover tooltips. addLayers() runs on
// every style load, since swapping the light/dark basemap drops them.
import { $$, esc, store } from './util.js';

let map = null;
let data = null; // GeoJSON for each source, loaded by main.js
let highlighted = null; // town name outlined in red
let searchPt = null; // address-search pin (GeoJSON Feature)

const EMPTY = { type: 'FeatureCollection', features: [] };
const zoomRadius = (lo, hi) => ['interpolate', ['linear'], ['zoom'], 8, lo, 14, hi];

// Town fills need less opacity over the light basemap
const TOWN_PAINT = {
  dark: { 'fill-opacity': 0.28, 'fill-outline-color': '#56606E' },
  light: { 'fill-opacity': 0.2, 'fill-outline-color': '#8C96A3' }
};

export function addLayers(m, d, theme) {
  map = m;
  data = d;
  if (map.getSource('towns')) return; // already on this style
  map.addSource('towns', { type: 'geojson', data: d.towns });
  map.addSource('commuter', { type: 'geojson', data: d.commuterLines });
  map.addSource('commuter_stations', { type: 'geojson', data: d.commuterStations });
  map.addSource('subway', { type: 'geojson', data: d.subwayLines });
  map.addSource('subway_stations', { type: 'geojson', data: d.subwayStations });
  map.addSource('search_pt', { type: 'geojson', data: searchPt || EMPTY });

  map.addLayer({ id: 'commuter', type: 'line', source: 'commuter', paint: { 'line-color': '#C264D6', 'line-width': 2 } });
  map.addLayer({
    id: 'subway', type: 'line', source: 'subway', layout: { visibility: 'none' }, // T lines start off
    paint: { 'line-color': ['get', 'route_color'], 'line-width': 3 } // official MBTA line colors
  });
  map.addLayer({ id: 'towns', type: 'fill', source: 'towns', paint: { 'fill-color': ['get', 'fill_color'], ...TOWN_PAINT[theme] } });
  map.addLayer({
    id: 'highlight', type: 'line', source: 'towns', filter: ['==', ['get', 'town_name'], highlighted ?? ''],
    paint: { 'line-color': '#FF5A4F', 'line-width': 3 }
  });
  map.addLayer({
    id: 'commuter_stations', type: 'circle', source: 'commuter_stations',
    paint: { 'circle-radius': zoomRadius(3, 8), 'circle-color': '#fff', 'circle-stroke-color': '#C264D6', 'circle-stroke-width': 2 }
  });
  map.addLayer({
    id: 'subway_stations', type: 'circle', source: 'subway_stations', layout: { visibility: 'none' },
    paint: {
      'circle-radius': zoomRadius(2.5, 7), 'circle-color': '#fff',
      'circle-stroke-color': ['get', 'station_color'], 'circle-stroke-width': 2 // gray at transfer stations
    }
  });
  map.addLayer({
    id: 'search_pt', type: 'circle', source: 'search_pt',
    paint: { 'circle-radius': 7, 'circle-color': '#4C8DFF', 'circle-stroke-color': '#fff', 'circle-stroke-width': 2.5 }
  });

  applyTiers();
  applyTransit();
}

// ---------- town highlight + address pin ----------
export function setHighlight(name) {
  highlighted = name || null;
  if (map && map.getLayer('highlight')) map.setFilter('highlight', ['==', ['get', 'town_name'], highlighted ?? '']);
}

export function setSearchPoint(lngLat, label) {
  searchPt = lngLat ? { type: 'Feature', properties: { label }, geometry: { type: 'Point', coordinates: lngLat } } : null;
  const src = map && map.getSource('search_pt');
  if (src) src.setData(searchPt || EMPTY);
}

// ---------- school tier fills (settings menu) ----------
// A hidden tier's towns get a transparent fill: outlines stay and the towns
// can still be clicked for their details. Remembered per browser.
const TIER_KEY = 'townmap-tiers';
const TIER_COLOR = { 1: '#009688', 2: '#AB47BC' }; // the fill_color values set in 02_preprocessing.R
const tiers = (() => {
  const saved = store.getJSON(TIER_KEY, null);
  return { 1: !saved || saved[1] !== false, 2: !saved || saved[2] !== false };
})();

function applyTiers() {
  if (!map || !map.getLayer('towns')) return;
  map.setPaintProperty('towns', 'fill-color', ['case',
    ['==', ['get', 'fill_color'], TIER_COLOR[1]], tiers[1] ? TIER_COLOR[1] : 'rgba(0,0,0,0)',
    ['==', ['get', 'fill_color'], TIER_COLOR[2]], tiers[2] ? TIER_COLOR[2] : 'rgba(0,0,0,0)',
    ['get', 'fill_color']
  ]);
}

// ---------- transit layers (transit menu) ----------
const RAIL_KEY = 'townmap-rail';
let railOn = store.get(RAIL_KEY) !== 'off';

// T lines: one chip per line (Green's four branches share a chip). The shapes
// carry a `line` property ("Red", "Green", ...); a station's `lines` property
// lists every line serving it, so it shows while any of its lines is on.
const TLINES_KEY = 'townmap-tlines';
const TLINE_NAMES = ['Red', 'Orange', 'Blue', 'Green', 'Mattapan'];
const tlines = (() => {
  const saved = store.getJSON(TLINES_KEY, null);
  // Off unless the user turned a line on; commuter rail is the default transit layer
  return Object.fromEntries(TLINE_NAMES.map((n) => [n, !!(saved && saved[n] === true)]));
})();

function applyTransit() {
  if (!map || !map.getLayer('commuter')) return;
  for (const id of ['commuter', 'commuter_stations']) map.setLayoutProperty(id, 'visibility', railOn ? 'visible' : 'none');
  const shown = TLINE_NAMES.filter((n) => tlines[n]);
  for (const id of ['subway', 'subway_stations']) map.setLayoutProperty(id, 'visibility', shown.length ? 'visible' : 'none');
  if (!shown.length) return;
  map.setFilter('subway', ['in', ['get', 'line'], ['literal', shown]]);
  map.setFilter('subway_stations', ['any', ...shown.map((n) => ['in', n, ['get', 'lines']])]);
}

export function tintTowns(theme) {
  if (!map || !map.getLayer('towns')) return;
  for (const [k, v] of Object.entries(TOWN_PAINT[theme])) map.setPaintProperty('towns', k, v);
}

export function initLayerSettings() {
  $$('input[data-tier]').forEach((cb) => {
    const tier = cb.getAttribute('data-tier');
    cb.checked = tiers[tier];
    cb.addEventListener('change', () => {
      tiers[tier] = cb.checked;
      store.setJSON(TIER_KEY, tiers);
      applyTiers();
    });
  });
  $$('input[data-layer="rail"]').forEach((cb) => {
    cb.checked = railOn;
    cb.addEventListener('change', () => {
      railOn = cb.checked;
      store.set(RAIL_KEY, railOn ? 'on' : 'off');
      applyTransit();
    });
  });
  $$('.tchip[data-tline]').forEach((chip) => {
    const name = chip.getAttribute('data-tline');
    chip.setAttribute('aria-pressed', tlines[name] ? 'true' : 'false');
    chip.addEventListener('click', () => {
      tlines[name] = chip.getAttribute('aria-pressed') !== 'true';
      chip.setAttribute('aria-pressed', tlines[name] ? 'true' : 'false');
      store.setJSON(TLINES_KEY, tlines);
      applyTransit();
    });
  });
}

// ---------- tooltips and popups ----------
// Hover shows the topmost feature's name (mouse only; touch has no hover).
// Clicking a station or the address pin pins its label in a popup.
const TIP_LAYERS = ['search_pt', 'subway_stations', 'commuter_stations', 'subway', 'commuter', 'towns'];
export const POPUP_LAYERS = ['search_pt', 'subway_stations', 'commuter_stations'];

function tipText(f) {
  switch (f.layer.id) {
    case 'towns': return f.properties.town_name;
    case 'subway': return f.properties.route_name;
    case 'commuter': return 'Commuter Rail';
    default: return f.properties.label;
  }
}

export function initTooltips(m) {
  if (!window.matchMedia('(hover: hover)').matches) return;
  const tip = new maplibregl.Popup({ closeButton: false, closeOnClick: false, className: 'map-tip', offset: 12, maxWidth: '260px' });
  const canvas = m.getCanvas();
  m.on('mousemove', (e) => {
    const layers = TIP_LAYERS.filter((id) => m.getLayer(id));
    const f = layers.length ? m.queryRenderedFeatures(e.point, { layers })[0] : null;
    const text = f && tipText(f);
    // Pointer over anything clickable, unless the ruler's crosshair is showing
    if (canvas.style.cursor !== 'crosshair') canvas.style.cursor = f && f.layer.id !== 'subway' && f.layer.id !== 'commuter' ? 'pointer' : '';
    if (!text) { tip.remove(); return; }
    tip.setLngLat(e.lngLat).setHTML(esc(text));
    if (!tip.isOpen()) tip.addTo(m);
  });
  canvas.addEventListener('mouseleave', () => tip.remove());
}

// Shows the label of a station / address pin under the click, if any. Returns
// true when it did, so the click isn't also treated as a town click.
let clickPopup = null;
export function popupAt(m, e) {
  const layers = POPUP_LAYERS.filter((id) => m.getLayer(id));
  const f = layers.length ? m.queryRenderedFeatures(e.point, { layers })[0] : null;
  if (!f) return false;
  if (clickPopup) clickPopup.remove();
  clickPopup = new maplibregl.Popup({ offset: 10, maxWidth: '260px' })
    .setLngLat(f.geometry.coordinates)
    .setHTML(esc(f.properties.label))
    .addTo(m);
  return true;
}
