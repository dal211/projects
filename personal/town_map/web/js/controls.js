// Custom map controls: the transit (train) button and the school-quality legend.
// CSS `order` keeps both at the bottom of the top-right stack.
import { $, ICONS, store } from './util.js';
import { setTransitOpen } from './panel.js';

// Opens the commuter rail / T lines menu (#transit-pop)
export class TransitControl {
  onAdd() {
    this.el = document.createElement('div');
    this.el.className = 'maplibregl-ctrl maplibregl-ctrl-group transit-ctrl';
    const btn = document.createElement('button');
    btn.type = 'button';
    btn.id = 'transit-btn';
    btn.title = 'Show or hide commuter rail and T lines';
    btn.setAttribute('aria-haspopup', 'true');
    btn.setAttribute('aria-expanded', 'false');
    btn.innerHTML = ICONS.train;
    btn.addEventListener('click', (e) => {
      e.stopPropagation();
      setTransitOpen(!$('#transit-pop').classList.contains('open'));
    });
    this.el.appendChild(btn);
    return this.el;
  }
  onRemove() { this.el.remove(); }
}

// Collapses to an icon button like the other controls; the choice is remembered.
// Starts collapsed in the mobile layout, where it would cover much of the map.
const LEGEND_KEY = 'townmap-legend';
export class LegendControl {
  onAdd() {
    const el = this.el = document.createElement('div');
    el.className = 'maplibregl-ctrl custom-tip-ctrl';
    el.innerHTML =
      '<button type="button" class="legend-head" aria-expanded="true" title="School quality legend">' +
      '<h5>School Quality</h5><span class="legend-chev">&#9662;</span>' + ICONS.legend + '</button>' +
      '<div class="legend-body">' +
      '<div class="legend-row"><span class="legend-swatch green"></span>&gt;70th percentile (Tier 1)</div>' +
      '<div class="legend-row"><span class="legend-swatch purple"></span>50&ndash;69th percentile (Tier 2)</div>' +
      '</div>';
    const head = el.querySelector('.legend-head');
    const setOpen = (open) => {
      el.classList.toggle('collapsed', !open);
      head.setAttribute('aria-expanded', open ? 'true' : 'false');
      store.set(LEGEND_KEY, open ? 'open' : 'closed');
    };
    head.addEventListener('click', (e) => { e.stopPropagation(); setOpen(el.classList.contains('collapsed')); });
    const saved = store.get(LEGEND_KEY);
    if (saved === 'closed' || (!saved && $('#app').classList.contains('mode-mobile'))) {
      el.classList.add('collapsed');
      head.setAttribute('aria-expanded', 'false');
    }
    return el;
  }
  onRemove() { this.el.remove(); }
}
