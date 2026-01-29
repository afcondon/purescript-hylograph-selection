export const clearElement = (selector) => () => {
  const el = document.querySelector(selector);
  if (el) {
    el.innerHTML = '';
  }
};

export const getElementWidth = (element) => () => {
  return element.getBoundingClientRect().width;
};
