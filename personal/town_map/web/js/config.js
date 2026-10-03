// MapTiler key for the basemap and address search. It is visible to anyone
// using the site (tile and style requests include it); the protection is the
// "Allowed HTTP Origins" list on the key in the MapTiler dashboard, which must
// include this site's domain (and localhost for local testing).
export const MAPTILER_KEY = 'XukbtwhZN33k7aCdvTkA';

export const MAP_STYLES = {
  light: `https://api.maptiler.com/maps/streets-v2/style.json?key=${MAPTILER_KEY}`,
  dark: `https://api.maptiler.com/maps/streets-v2-dark/style.json?key=${MAPTILER_KEY}`
};
