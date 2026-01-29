export const unsafeHead = (arr) => arr[0];
export const unsafeTail = (arr) => arr.slice(1);
export const unsafeIndex = (arr) => (i) => arr[i];
export const stringLength = (s) => s.length;
export const take = (n) => (s) => s.substring(0, n);
export const charAt = (i) => (s) => s.charAt(i);
export const trimStart = (s) => s.trimStart();
export const toUpper = (s) => s.toUpperCase();
export const drop = (n) => (s) => s.substring(n);
