// Saved (starred) towns: kept in this browser's localStorage. A
// ?saved=Town,Town link (from "Copy share link") adds its towns to the list.
import { $, $$, esc, store, toast } from './util.js';
import { tierOf, TIER_COLOR, fmt } from './card.js';

const SAVED_KEY = 'townmap-saved';
let saved = [];
let towns = null; // Map of town name -> attributes
let sharedAdded = 0;

export const isSaved = (name) => saved.includes(name);

// Reads the stored list and any shared link. Names that aren't towns are dropped.
export function initSaved(townMap, { onOpen }) {
  towns = townMap;
  const stored = store.getJSON(SAVED_KEY, []);
  saved = (Array.isArray(stored) ? stored : []).filter((t) => towns.has(t));

  const shared = new URLSearchParams(location.search).get('saved');
  if (shared) {
    for (let t of shared.split(',')) {
      t = t.trim();
      if (towns.has(t) && !saved.includes(t)) { saved.push(t); sharedAdded++; }
    }
    // Drop the parameter so a reload doesn't re-add towns the user since removed
    const url = new URL(location.href);
    url.searchParams.delete('saved');
    history.replaceState(null, '', url);
  }
  store.setJSON(SAVED_KEY, saved);

  // Stars (town card, table) and table rows; delegated since both re-render
  document.addEventListener('click', (e) => {
    const star = e.target.closest('[data-star]');
    if (star) { toggleSaved(star.getAttribute('data-star')); return; }
    const row = e.target.closest('[data-saved-town]');
    if (row) onOpen(row.getAttribute('data-saved-town'));
  });
  $('#saved-csv').addEventListener('click', downloadCsv);
  $('#saved-share').addEventListener('click', shareLink);
  render();
  return sharedAdded;
}

function toggleSaved(name) {
  const was = isSaved(name);
  saved = was ? saved.filter((t) => t !== name) : [...saved, name];
  store.setJSON(SAVED_KEY, saved);
  render();
  toast(was ? `${name} removed from saved towns` : `${name} saved`);
}

// Saved towns' attributes, best schools first
function rows() {
  return saved.map((t) => towns.get(t)).sort((a, b) =>
    (b.normalized_school_score ?? -1) - (a.normalized_school_score ?? -1) || a.town_name.localeCompare(b.town_name));
}

function render() {
  const n = saved.length;
  const badge = $('#saved-count');
  badge.textContent = n;
  badge.style.display = n ? '' : 'none';
  $('#saved-view').classList.toggle('is-empty', !n);

  // Star on the open town card
  $$('.c-star[data-star]').forEach((b) => {
    const on = isSaved(b.getAttribute('data-star'));
    b.classList.toggle('saved', on);
    b.setAttribute('aria-pressed', on ? 'true' : 'false');
    b.title = on ? 'Remove from saved towns' : 'Save this town';
  });

  // Rows open the town; the × un-stars it
  $('#saved_table').innerHTML = !n
    ? '<p class="saved-empty">No saved towns yet. Tap the star on a town\'s details to add it here.</p>'
    : `<table class="saved-table">
        <thead><tr><th>Town</th><th>Score</th><th>Home</th><th>Tax</th><th></th></tr></thead>
        <tbody>${rows().map((t) => {
          const name = esc(t.town_name);
          return `<tr data-saved-town="${name}" title="Show ${name}">
            <td class="s-town"><span class="c-dot" style="background:${TIER_COLOR[tierOf(t.fill_color)]}"></span>${name}</td>
            <td>${fmt.orNA(t.normalized_school_score, fmt.int)}</td>
            <td>${fmt.orNA(t.current_typ_home_value, fmt.home)}</td>
            <td>${fmt.orNA(t.prop_rate, (v) => fmt.pct(v, 2))}</td>
            <td><button class="s-remove" type="button" data-star="${name}" title="Remove ${name}">&times;</button></td>
          </tr>`;
        }).join('')}</tbody>
      </table>`;
}

// ---------- CSV: every attribute, with readable column names ----------
const round = (x, d = 0) => (typeof x === 'number' && isFinite(x) ? +x.toFixed(d) : null);
const CSV_COLUMNS = [
  ['Town', (t) => t.town_name],
  ['School district', (t) => t.DIST_NAME],
  ['School tier', (t) => ['Below Tier 2', 'Tier 1', 'Tier 2'][tierOf(t.fill_color)]],
  ['School score (0-100)', (t) => round(t.normalized_school_score, 1)],
  ['MCAS percentile', (t) => round(t.mcas_rank * 100)],
  ['AP percentile', (t) => round(t.ap_rank * 100)],
  ['SAT percentile', (t) => round(t.sat_rank * 100)],
  ['College-bound (%)', (t) => round(t.college_bound_rate * 100, 1)],
  ['High school size (est.)', (t) => t.school_size_est],
  ['Typical 3-bed home value ($)', (t) => round(t.current_typ_home_value)],
  ['1-year price change (%)', (t) => round(t.one_year_price_change, 1)],
  ['Property tax rate (%)', (t) => round(t.prop_rate * 100, 2)],
  ['Density (people/sq mi)', (t) => round(t.density)],
  ['Density category', (t) => t.dens_cat]
];

function csvCell(v) {
  if (v === null || v === undefined) return '';
  return typeof v === 'number' ? String(v) : '"' + String(v).replace(/"/g, '""') + '"';
}

function downloadCsv() {
  const lines = [CSV_COLUMNS.map(([h]) => csvCell(h)).join(',')]
    .concat(rows().map((t) => CSV_COLUMNS.map(([, f]) => csvCell(f(t))).join(',')));
  // BOM so Excel reads it as UTF-8
  const blob = new Blob(['﻿' + lines.join('\r\n') + '\r\n'], { type: 'text/csv;charset=utf-8' });
  const a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = `saved_towns_${new Date().toISOString().slice(0, 10)}.csv`;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(a.href), 1000);
}

// ---------- share link: recreates this list in another browser ----------
function shareLink() {
  if (!saved.length) return;
  const url = new URL(location.href);
  url.search = '';
  url.hash = '';
  url.searchParams.set('saved', saved.join(','));
  const link = url.toString();
  const fallback = () => window.prompt('Copy this link to open your saved towns elsewhere:', link);
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(link).then(() => toast('Share link copied'), fallback);
  } else {
    fallback();
  }
}
