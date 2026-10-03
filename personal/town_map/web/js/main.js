// Entry point: loads the data, builds the map and wires the panel to it.
// Data files in data/ are written by 04_export_web.R.
import { $, esc, geomBbox, toast } from './util.js';
import { MAP_STYLES } from './config.js';
import { ui, initPanel, setMap, focusMap, setView } from './panel.js';
import { addLayers, setHighlight, setSearchPoint, tintTowns, initLayerSettings, initTooltips, popupAt } from './layers.js';
import { townCardHtml } from './card.js';
import { initSaved, isSaved } from './saved.js';
import { geocode, boxAround } from './search.js';
import { RulerControl } from './ruler.js';
import { TransitControl, LegendControl } from './controls.js';

// Massachusetts; replaced by the exported bbox once towns.json arrives
let fullBbox = [-73.508, 41.238, -69.929, 42.887];

// ---------- data (fetched in parallel with the basemap) ----------
const getJSON = (f) => fetch('data/' + f).then((r) => {
  if (!r.ok) throw new Error(`${f}: ${r.status}`);
  return r.json();
});
const dataReady = Promise.all([
  getJSON('towns.json'), getJSON('towns.geojson'),
  getJSON('commuter_lines.geojson'), getJSON('commuter_stations.geojson'),
  getJSON('subway_lines.geojson'), getJSON('subway_stations.geojson')
]).then(([info, towns, commuterLines, commuterStations, subwayLines, subwayStations]) => ({
  info, geo: { towns, commuterLines, commuterStations, subwayLines, subwayStations }
}));

// ---------- panel + map ----------
initPanel({ onTheme: swapBasemap });
initLayerSettings();

const map = new maplibregl.Map({
  container: 'map',
  style: MAP_STYLES[ui.theme],
  bounds: [[fullBbox[0], fullBbox[1]], [fullBbox[2], fullBbox[3]]],
  attributionControl: { compact: true }
});
setMap(map);

const ruler = new RulerControl();
map.addControl(new maplibregl.NavigationControl(), 'top-right'); // zoom + compass (hidden on mobile)
const geo = new maplibregl.GeolocateControl({ // position stays in the browser
  positionOptions: { enableHighAccuracy: true },
  trackUserLocation: false,
  fitBoundsOptions: { maxZoom: 13 }
});
map.addControl(geo, 'top-right');
geo.on('error', (e) => {
  const code = e && (e.code || (e.error && e.error.code));
  toast(code === 1 ? 'Location permission denied' : 'Your location is unavailable');
});
geo.on('outofmaxbounds', () => toast('Your location is outside the map area'));
map.addControl(ruler, 'top-right');
map.addControl(new TransitControl(), 'top-right');
map.addControl(new LegendControl(), 'top-right');
initTooltips(map);

// Layers go on every style load: the first one, and each light/dark swap
let data = null, styleLoaded = false;
function onStyle() {
  if (!data || !styleLoaded) return;
  addLayers(map, data.geo, ui.theme);
  ruler.addLayers();
}
map.on('style.load', () => { styleLoaded = true; onStyle(); });

function swapBasemap(theme) {
  const cam = { center: map.getCenter(), zoom: map.getZoom(), bearing: map.getBearing(), pitch: map.getPitch() };
  styleLoaded = false;
  map.setStyle(MAP_STYLES[theme], { diff: false });
  map.once('style.load', () => { map.jumpTo(cam); tintTowns(theme); });
}

// ---------- map credits ----------
// Shown in the panel footer; the map's own (i) shows only while the web sidebar is closed
function syncCredits() {
  const a = document.querySelector('.maplibregl-ctrl-attrib');
  const inner = document.querySelector('.maplibregl-ctrl-attrib-inner');
  const foot = $('#map-credit');
  if (inner && inner.innerHTML && foot.innerHTML !== inner.innerHTML) foot.innerHTML = inner.innerHTML;
  if (a) { a.classList.remove('maplibregl-compact-show'); a.removeAttribute('open'); }
}
map.on('styledata', syncCredits);
map.once('idle', syncCredits);

// ---------- towns: selection, card ----------
let info = new Map(); // town name -> attributes
const townBbox = new Map(); // town name -> [w, s, e, n]
let current = '';

function renderCard() {
  const t = info.get(current);
  $('#town_card').innerHTML = t ? townCardHtml(t, isSaved(current)) : '';
}

function selectTown(name) {
  if (!info.has(name)) return;
  current = name;
  $('#town_sel').value = name;
  renderCard();
  setHighlight(name);
  // Card opens in the panel: half-height sheet on mobile, sidebar on web
  focusMap(townBbox.get(name) || fullBbox, { sheet: 'half', panel: true });
}

function clearTown() {
  current = '';
  $('#town_sel').value = '';
  renderCard();
  setHighlight(null);
}

$('#town_sel').addEventListener('change', (e) => {
  if (e.target.value) selectTown(e.target.value); else clearTown();
});
document.addEventListener('click', (e) => {
  if (!e.target.closest('[data-card-close]')) return;
  clearTown();
  focusMap(fullBbox, { sheet: 'collapsed' });
});

// Map clicks: the ruler first, then station / address-pin labels, then towns
map.on('click', (e) => {
  if (ruler.handleClick(e)) return;
  if (popupAt(map, e)) return;
  const f = map.getLayer('towns') && map.queryRenderedFeatures(e.point, { layers: ['towns'] })[0];
  if (f) selectTown(f.properties.town_name); // re-centres even if it's already open
});

// ---------- address search ----------
let searching = false;
async function findAddress() {
  if (searching) return;
  const query = $('#addr_query').value.trim();
  if (!query) { toast('Please enter an address.'); return; }
  const btn = $('#addr_go');
  searching = true;
  btn.innerHTML = '<span class="spin"></span>Searching…';
  try {
    const hit = await geocode(query);
    if (!hit) { toast('Address not found.'); return; }
    setSearchPoint(hit.lngLat, hit.place);
    focusMap(boxAround(hit.lngLat, 2), { sheet: 'collapsed', maxZoom: 15 }); // drop the sheet so the pin shows
  } catch (err) {
    toast('Address search is unavailable right now.');
  } finally {
    searching = false;
    btn.textContent = 'Find address';
  }
}
$('#addr_go').addEventListener('click', findAddress);
$('#addr_query').addEventListener('keydown', (e) => { if (e.key === 'Enter') findAddress(); });

$('#reset_view').addEventListener('click', () => {
  // Clear highlight, card & address pin, then zoom back out
  clearTown();
  setSearchPoint(null);
  $('#addr_query').value = '';
  focusMap(fullBbox, { sheet: 'collapsed' });
});

// ---------- once the data is in ----------
// The overlay stays up until both the basemap and the town data are ready
let mapLoaded = false;
map.once('load', () => { mapLoaded = true; maybeHideOverlay(); });
function maybeHideOverlay() { if (data && mapLoaded) hideOverlay(); }
function hideOverlay() { $('#map-loading-overlay').style.display = 'none'; }

dataReady.then((d) => {
  data = d;
  fullBbox = d.info.bbox;
  info = new Map(d.info.towns.map((t) => [t.town_name, t]));
  for (const f of d.geo.towns.features) townBbox.set(f.properties.town_name, geomBbox(f.geometry));

  $('#town_sel').insertAdjacentHTML('beforeend',
    [...info.keys()].sort().map((n) => `<option value="${esc(n)}">${esc(n)}</option>`).join(''));

  const sharedAdded = initSaved(info, { onOpen: selectTown });
  if (sharedAdded) {
    toast(`Added ${sharedAdded} town${sharedAdded > 1 ? 's' : ''} from a shared link`);
    setView('saved');
  }

  onStyle();
  focusMap(fullBbox, { instant: true }); // re-fit with the panel accounted for
  maybeHideOverlay();
}).catch((err) => {
  console.error(err);
  $('#map-loading-overlay').innerHTML = '<span>Couldn\'t load the town data. Please refresh to try again.</span>';
});

// Never leave the overlay up for good if the basemap stalls
setTimeout(() => { if (data) hideOverlay(); }, 8000);
