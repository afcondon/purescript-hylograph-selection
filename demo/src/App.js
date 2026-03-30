export const clearElement = (selector) => () => {
  const el = document.querySelector(selector);
  if (el) {
    el.innerHTML = '';
  }
};
