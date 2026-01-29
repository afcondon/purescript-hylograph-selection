export function highlightAll() {
  if (typeof Prism !== 'undefined' && Prism.highlightAll) {
    // Short delay to ensure DOM is updated by Halogen
    setTimeout(() => {
      Prism.highlightAll();
    }, 10);
  }
}
