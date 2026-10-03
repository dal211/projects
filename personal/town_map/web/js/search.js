// Address lookup via MapTiler's geocoding API, limited to Massachusetts.
import { MAPTILER_KEY } from './config.js';

const MA_BBOX = '-73.508,41.237,-69.927,42.886';

// Returns { lngLat: [lng, lat], place } or null when nothing is found.
// Throws on a network or API error.
export async function geocode(query) {
  const q = encodeURIComponent(`${query}, MA, USA`);
  const url = `https://api.maptiler.com/geocoding/${q}.json?key=${MAPTILER_KEY}&country=us&bbox=${MA_BBOX}&limit=1`;
  const resp = await fetch(url);
  if (!resp.ok) throw new Error(`Geocoding failed (${resp.status})`);
  const js = await resp.json();
  const f = js.features && js.features[0];
  if (!f) return null;
  return { lngLat: f.geometry.coordinates.slice(0, 2), place: f.place_name || query };
}

// A box of about `km` around a point, for framing an address on the map
export function boxAround([lng, lat], km = 2) {
  const dLat = km / 111.32, dLng = km / (111.32 * Math.cos(lat * Math.PI / 180));
  return [lng - dLng, lat - dLat, lng + dLng, lat + dLat];
}
