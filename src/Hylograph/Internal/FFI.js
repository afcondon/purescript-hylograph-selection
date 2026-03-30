// Hylograph.Internal.FFI — minimal key/index utilities
// No D3 dependencies.

export function getIndexFromDatum_(datum) {
  return (typeof datum.index == `undefined`) ? "?" : datum.index;
}

export function keyIsID_(d) {
  return d.id;
}

export function keyIsSourceTarget_(d) {
  return [d.source, d.target];
}

export function swizzledLinkKey_(d) {
  const sourceId = typeof d.source === 'object' ? d.source.id : d.source;
  const targetId = typeof d.target === 'object' ? d.target.id : d.target;
  return `${sourceId}->${targetId}`;
}
