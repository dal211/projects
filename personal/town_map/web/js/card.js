// Town details card and the number formatting it shares with the Saved table.
import { esc, ICONS } from './util.js';

export const REDFIN_URL = 'https://www.redfin.com/';
export const NICHE_URL = 'https://www.niche.com/places-to-live/search/best-places-to-live/';

// School tier from the town's fill color (set in 02_preprocessing.R): 1, 2 or 0
export function tierOf(fillColor) {
  return fillColor === '#009688' ? 1 : fillColor === '#AB47BC' ? 2 : 0;
}
export const TIER_COLOR = ['#6B7585', '#009688', '#AB47BC'];

const isNum = (x) => typeof x === 'number' && isFinite(x);
export const fmt = {
  orNA: (x, f) => (x === null || x === undefined || x === '' || (typeof x === 'number' && !isFinite(x)) ? 'N/A' : f(x)),
  home: (v) => (v >= 1e6 ? '$' + (v / 1e6).toFixed(2) + 'M' : '$' + Math.round(v / 1000) + 'K'),
  pct: (v, digits = 0) => (v * 100).toFixed(digits) + '%',
  comma: (v) => Math.round(v).toLocaleString('en-US'),
  int: (v) => String(Math.round(v))
};

export function townCardHtml(t, saved) {
  const tier = tierOf(t.fill_color);
  const tierTxt = ['Below Tier 2', 'Tier 1 · above 70th percentile', 'Tier 2 · 50–69th percentile'][tier];
  const tierCol = TIER_COLOR[tier];
  const chg = t.one_year_price_change;
  const chgTag = isNum(chg) ? `<em class="${chg >= 0 ? 'up' : 'down'}">${chg >= 0 ? '+' : ''}${chg.toFixed(1)}%</em>` : '';
  const score = t.normalized_school_score;
  const pct = (x) => fmt.orNA(x, (v) => fmt.pct(v));
  const row = (label, value) => `<div class="c-row"><span>${label}</span><b>${value}</b></div>`;
  const density = fmt.orNA(t.dens_cat, esc) + (isNum(t.density) ? ' · ' + fmt.comma(t.density) + '/sq mi' : '');
  const name = esc(t.town_name);

  return `
    <div class="c-head">
      <span class="c-dot" style="background:${tierCol}"></span>
      <div>
        <h2>${name}</h2>
        <p>${fmt.orNA(t.DIST_NAME, (d) => esc(d) + ' schools')}</p>
        <p>${density}</p>
        <span class="chip t${tier}">${tierTxt}</span>
      </div>
      <div class="c-actions">
        <button class="c-star${saved ? ' saved' : ''}" type="button" data-star="${name}"
          title="${saved ? 'Remove from saved towns' : 'Save this town'}" aria-pressed="${saved}">${ICONS.star}</button>
        <button class="c-close" type="button" title="Close" data-card-close>&times;</button>
      </div>
    </div>
    <div class="c-sec">
      <h3>Housing</h3>
      ${row('Typical 3-bed home', fmt.orNA(t.current_typ_home_value, fmt.home) + chgTag)}
      ${row('Property tax rate', fmt.orNA(t.prop_rate, (v) => fmt.pct(v, 2)))}
    </div>
    <div class="c-sec">
      <h3>Schools</h3>
      <div class="c-score"><b>${fmt.orNA(score, fmt.int)}</b><span>school score out of 100</span></div>
      <div class="c-bar"><i style="width:${isNum(score) ? Math.round(score) : 0}%;background:${tierCol}"></i></div>
      <div class="c-note">Average of the district's MCAS, AP and SAT percentile ranks</div>
      ${row('MCAS · AP · SAT rank', [pct(t.mcas_rank), pct(t.ap_rank), pct(t.sat_rank)].join(' · '))}
      ${row('College-bound', pct(t.college_bound_rate))}
      ${row('High school size', fmt.orNA(t.school_size_est, fmt.comma))}
    </div>
    <div class="c-links">
      <a href="${REDFIN_URL}" target="_blank" rel="noopener">Redfin</a>
      <a href="${NICHE_URL}" target="_blank" rel="noopener">Niche</a>
    </div>`;
}
