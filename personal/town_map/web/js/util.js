// Small helpers shared by the other modules.

export const $ = (s) => document.querySelector(s);
export const $$ = (s) => document.querySelectorAll(s);
export const clamp = (v, a, b) => Math.max(a, Math.min(b, v));

// Escape text for innerHTML
export function esc(s) {
  return String(s ?? '').replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
}

// localStorage that never throws (private windows, blocked site data)
export const store = {
  get(key) { try { return localStorage.getItem(key); } catch (e) { return null; } },
  set(key, val) { try { localStorage.setItem(key, val); } catch (e) {} },
  getJSON(key, fallback) {
    try { const v = JSON.parse(localStorage.getItem(key) || 'null'); return v ?? fallback; } catch (e) { return fallback; }
  },
  setJSON(key, val) { try { localStorage.setItem(key, JSON.stringify(val)); } catch (e) {} }
};

let toastTimer;
export function toast(msg) {
  const el = $('#toast');
  if (!el) return;
  el.textContent = msg;
  el.classList.add('show');
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => el.classList.remove('show'), 2800);
}

export const ICONS = {
  ruler: '<svg class="ctrl-svg" viewBox="0 0 24 24"><path d="M3 17L17 3l4 4L7 21z"/><path d="M7.5 12.5l2 2M10.5 9.5l2 2M13.5 6.5l2 2"/></svg>',
  legend: '<svg class="ctrl-svg" viewBox="0 0 24 24"><rect x="3.5" y="4.5" width="4" height="4" rx="1"/><rect x="3.5" y="15.5" width="4" height="4" rx="1"/><path d="M11 6.5h9.5M11 17.5h9.5"/></svg>',
  train: '<svg class="ctrl-svg" viewBox="0 0 24 24"><rect x="5" y="3" width="14" height="14" rx="3"/><path d="M5 11h14M9 21l2-4M15 21l-2-4"/><circle cx="9" cy="14" r=".7"/><circle cx="15" cy="14" r=".7"/></svg>',
  star: '<svg viewBox="0 0 24 24"><path d="M12 3.5l2.6 5.3 5.9.9-4.3 4.1 1 5.8-5.2-2.7-5.2 2.7 1-5.8-4.3-4.1 5.9-.9z"/></svg>'
};

// Bounding box [w, s, e, n] of any GeoJSON geometry
export function geomBbox(geom) {
  const b = [Infinity, Infinity, -Infinity, -Infinity];
  (function walk(c) {
    if (typeof c[0] === 'number') {
      b[0] = Math.min(b[0], c[0]); b[1] = Math.min(b[1], c[1]);
      b[2] = Math.max(b[2], c[0]); b[3] = Math.max(b[3], c[1]);
    } else c.forEach(walk);
  })(geom.coordinates);
  return b;
}

// Great-circle distance in miles between two [lng, lat] points
export function milesBetween(a, b) {
  const R = 3958.8, rad = Math.PI / 180;
  const dLat = (b[1] - a[1]) * rad, dLng = (b[0] - a[0]) * rad;
  const h = Math.sin(dLat / 2) ** 2 + Math.cos(a[1] * rad) * Math.cos(b[1] * rad) * Math.sin(dLng / 2) ** 2;
  return 2 * R * Math.asin(Math.sqrt(h));
}
