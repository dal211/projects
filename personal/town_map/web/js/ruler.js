// Distance ruler: click the button to arm it, click point A, click point B,
// done. Shows the straight-line distance at once, then upgrades the label with
// the driving distance and time from the public OSRM demo server.
import { ICONS, milesBetween, toast } from './util.js';

const EMPTY = { type: 'FeatureCollection', features: [] };
const OSRM_URL = 'https://router.project-osrm.org/route/v1/driving/';

const fmtMiles = (mi, suffix = '') => (mi < 0.1 ? Math.round(mi * 5280) + ' ft' : mi.toFixed(2) + ' mi') + suffix;
const fmtMinutes = (mins) => {
  mins = Math.round(mins);
  return mins >= 60 ? `${Math.floor(mins / 60)} hr ${mins % 60} min` : `${mins} min`;
};
const popupHtml = (primary, secondary) =>
  `<div class="ruler-primary">${primary}</div><div class="ruler-secondary">${secondary}</div>`;

export class RulerControl {
  constructor() {
    this.measuring = false;
    this.pointA = null;
    this.popup = null;
    this.token = 0; // guards against a stale OSRM reply landing on a newer measurement
    this.shapes = { route: EMPTY, line: EMPTY, points: EMPTY }; // kept to redraw after a basemap swap
  }

  onAdd(map) {
    this.map = map;
    this.el = document.createElement('div');
    this.el.className = 'maplibregl-ctrl maplibregl-ctrl-group custom-ruler-ctrl';
    this.btn = document.createElement('button');
    this.btn.type = 'button';
    this.btn.title = 'Measure distance';
    this.btn.innerHTML = ICONS.ruler;
    this.btn.addEventListener('click', (e) => {
      e.stopPropagation();
      this.clear();
      this.setActive(true);
      toast('Click point A, then point B');
    });
    this.el.appendChild(this.btn);
    return this.el;
  }

  onRemove() { this.el.remove(); }

  // Called on every style load (after the town layers, so the ruler draws on top)
  addLayers() {
    const map = this.map;
    if (map.getSource('ruler-line')) return;
    map.addSource('ruler-route', { type: 'geojson', data: this.shapes.route }); // road route under the straight line
    map.addLayer({
      id: 'ruler-route-layer', type: 'line', source: 'ruler-route',
      layout: { 'line-cap': 'round', 'line-join': 'round' },
      paint: { 'line-color': '#34C26B', 'line-width': 5, 'line-opacity': 0.85 }
    });
    map.addSource('ruler-line', { type: 'geojson', data: this.shapes.line });
    map.addLayer({
      id: 'ruler-line-layer', type: 'line', source: 'ruler-line',
      paint: { 'line-color': '#4C8DFF', 'line-width': 3, 'line-dasharray': [2, 1] }
    });
    map.addSource('ruler-points', { type: 'geojson', data: this.shapes.points });
    map.addLayer({
      id: 'ruler-points-layer', type: 'circle', source: 'ruler-points',
      paint: { 'circle-radius': 5, 'circle-color': '#fff', 'circle-stroke-color': '#4C8DFF', 'circle-stroke-width': 2 }
    });
  }

  draw(key, geojson) {
    this.shapes[key] = geojson;
    const src = this.map.getSource('ruler-' + key);
    if (src) src.setData(geojson);
  }

  setActive(active) {
    this.measuring = active;
    this.map.getCanvas().style.cursor = active ? 'crosshair' : '';
    this.btn.classList.toggle('active', active);
  }

  clear() {
    this.draw('route', EMPTY);
    this.draw('line', EMPTY);
    this.draw('points', EMPTY);
    this.token++; // invalidate any in-flight driving lookup
    if (this.popup) { this.popup.remove(); this.popup = null; }
    this.pointA = null;
  }

  // Map click handler; returns true when the ruler used the click
  handleClick(e) {
    if (!this.measuring) return false;
    const p = [e.lngLat.lng, e.lngLat.lat];
    if (!this.pointA) {
      this.pointA = p;
      this.draw('points', { type: 'FeatureCollection', features: [pointFeature(p)] });
    } else {
      this.finishAt(p);
    }
    return true;
  }

  finishAt(b) {
    const a = this.pointA;
    const straight = fmtMiles(milesBetween(a, b));
    this.draw('line', { type: 'Feature', properties: {}, geometry: { type: 'LineString', coordinates: [a, b] } });
    this.draw('points', { type: 'FeatureCollection', features: [pointFeature(a), pointFeature(b)] });

    if (this.popup) this.popup.remove();
    this.popup = new maplibregl.Popup({ closeButton: false, closeOnClick: false, anchor: 'top' }) // hangs below the line
      .setLngLat([(a[0] + b[0]) / 2, (a[1] + b[1]) / 2])
      .setHTML(popupHtml(straight, 'Driving: calculating…'))
      .addTo(this.map);

    // Done after two points; further clicks are normal map clicks until re-armed
    this.setActive(false);
    this.route(a, b, straight, ++this.token);
  }

  // The straight-line figure is already on screen; this only upgrades the label
  async route(a, b, straight, token) {
    let r = null;
    try {
      const resp = await fetch(`${OSRM_URL}${a[0]},${a[1]};${b[0]},${b[1]}?overview=full&geometries=geojson`);
      const js = resp.ok ? await resp.json() : null;
      r = js && js.code === 'Ok' && js.routes && js.routes[0];
    } catch (e) {
      r = null;
    }
    if (token !== this.token || !this.popup) return; // superseded by a newer measurement
    if (!r) {
      this.popup.setHTML(popupHtml(straight, 'Driving route unavailable'));
      return;
    }
    this.popup.setHTML(popupHtml(
      fmtMiles(r.distance / 1609.344, ' driving'),
      `${fmtMinutes(r.duration / 60)} · ${straight} straight-line`
    ));
    this.draw('route', { type: 'Feature', properties: {}, geometry: r.geometry });
  }
}

function pointFeature(c) {
  return { type: 'Feature', properties: {}, geometry: { type: 'Point', coordinates: c } };
}
