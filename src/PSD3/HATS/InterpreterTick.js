// HATS Tick-Driven Interpreter FFI
// Re-exports from Behavior FFI and native Pointer/Zoom modules

import {
  attachSimpleDrag_ as nativeSimpleDrag,
  attachSimulationDragById_ as nativeSimulationDragById,
  attachSimulationDragNestedById_ as nativeSimulationDragNestedById
} from "../PSD3.Interaction.Pointer/foreign.js";

import {
  attachZoomNative_
} from "../PSD3.Interaction.Zoom/foreign.js";

// DOM helpers
export const selectElement = selector => doc => () => {
  return doc.querySelector(selector);
};

// Set attribute on element (handles SVG namespaced attrs and textContent)
export const setAttribute = el => name => value => () => {
  if (name === 'textContent') {
    el.textContent = value;
  } else if (name.startsWith('xlink:')) {
    el.setAttributeNS('http://www.w3.org/1999/xlink', name, value);
  } else if (name.startsWith('xml:')) {
    el.setAttributeNS('http://www.w3.org/XML/1998/namespace', name, value);
  } else {
    el.setAttribute(name, value);
  }
};

// Get attribute from element
export const getAttribute = el => name => () => {
  if (name === 'textContent') {
    return el.textContent || '';
  }
  return el.getAttribute(name) || '';
};

// Bind datum to element (D3-style __data__ property)
export const bindDatum = el => datum => () => {
  el.__data__ = datum;
};

// Set key on element for GUP diffing
export const setKey = el => key => () => {
  el.setAttribute('data-hats-key', key);
};

// Set fold name on element for scoping sibling Folds
export const setFoldName = el => foldName => () => {
  el.setAttribute('data-hats-fold', foldName);
};

// Get key from element
export const getKey = el => () => {
  return el.getAttribute('data-hats-key') || '';
};

// Get direct child elements (for GUP diffing)
export const getChildElements = parent => () => {
  return Array.from(parent.children);
};

// Get direct child elements filtered by tag name (for scoped GUP)
// tagName should be lowercase (e.g., "g", "circle", "path")
export const getChildElementsByTagName = parent => tagName => () => {
  const children = Array.from(parent.children);
  // SVG elements have tagName in lowercase
  return children.filter(el => el.tagName.toLowerCase() === tagName);
};

// Get direct child elements filtered by tag name AND fold name
// This ensures sibling Folds with the same element type don't interfere
export const getChildElementsForFold = parent => tagName => foldName => () => {
  const children = Array.from(parent.children);
  return children.filter(el =>
    el.tagName.toLowerCase() === tagName &&
    el.getAttribute('data-hats-fold') === foldName
  );
};

// Remove element from DOM
export const removeElement = el => () => {
  if (el.parentNode) {
    el.parentNode.removeChild(el);
  }
};

// =============================================================================
// Click handlers
// =============================================================================

export const attachClick = el => handler => () => {
  el.addEventListener('click', function(event) {
    handler();
  });
  el.style.cursor = 'pointer';
};

export const attachClickWithDatum = el => handler => () => {
  el.addEventListener('click', function(event) {
    const d = this.__data__;
    handler(d)();
  });
  el.style.cursor = 'pointer';
};

// =============================================================================
// Mouse event handlers (datum-based - legacy)
// =============================================================================

export const attachMouseEnter = el => handler => () => {
  el.addEventListener('mouseenter', function(event) {
    const d = this.__data__;
    handler(d)();
  });
};

export const attachMouseLeave = el => handler => () => {
  el.addEventListener('mouseleave', function(event) {
    const d = this.__data__;
    handler(d)();
  });
};

export const attachMouseDown = el => handler => () => {
  el.addEventListener('mousedown', function(event) {
    handler();
  });
};

// =============================================================================
// Thunked event handlers (closure-based - new)
// =============================================================================

// Thunked handlers capture their data in closures, so we just invoke the thunk
export const attachMouseEnterThunked = el => handler => () => {
  el.addEventListener('mouseenter', function(event) {
    handler()();  // handler is (Unit -> Effect Unit), invoke with unit then run effect
  });
};

export const attachMouseLeaveThunked = el => handler => () => {
  el.addEventListener('mouseleave', function(event) {
    handler()();
  });
};

export const attachClickThunked = el => handler => () => {
  el.addEventListener('click', function(event) {
    handler()();
  });
  el.style.cursor = 'pointer';
};

// =============================================================================
// Highlight behavior
// =============================================================================

export const attachHighlight = el => enterStyles => leaveStyles => () => {
  el.addEventListener('mouseenter', function(event) {
    enterStyles.forEach(style => {
      this.setAttribute(style.attr, style.value);
    });
  });

  el.addEventListener('mouseleave', function(event) {
    leaveStyles.forEach(style => {
      this.setAttribute(style.attr, style.value);
    });
  });
};

// =============================================================================
// Zoom behavior
// =============================================================================

export const attachZoom = el => scaleMin => scaleMax => targetSelector => () => {
  attachZoomNative_(el)(scaleMin)(scaleMax)(targetSelector)();
};

// =============================================================================
// Drag behaviors
// =============================================================================

export const attachSimpleDrag = el => () => {
  nativeSimpleDrag(el)()();
};

export const attachSimulationDragById = el => simId => () => {
  nativeSimulationDragById(el)(simId)();
};

export const attachSimulationDragNestedById = el => simId => () => {
  nativeSimulationDragNestedById(el)(simId)();
};

// =============================================================================
// Coordinated Highlighting (HATS-specific - thunked)
// =============================================================================

// Global registry for HATS coordinated highlighting
// Key: group name (or "_global" for ungrouped)
// Value: Array of { element, identifyThunk, classifyFn }
const hatsHighlightRegistry = new Map();

// CSS class names (same as BehaviorFFI)
const HIGHLIGHT_PRIMARY = 'highlight-primary';
const HIGHLIGHT_RELATED = 'highlight-related';
const HIGHLIGHT_DIMMED = 'highlight-dimmed';
const ALL_HIGHLIGHT_CLASSES = [HIGHLIGHT_PRIMARY, HIGHLIGHT_RELATED, HIGHLIGHT_DIMMED];

// HighlightClass enum values (must match PureScript)
const HC_PRIMARY = 0;
const HC_RELATED = 1;
const HC_DIMMED = 2;
const HC_NEUTRAL = 3;

/**
 * Get or create a highlight group
 */
function getHatsHighlightGroup(groupName) {
  const key = groupName || '_global';
  if (!hatsHighlightRegistry.has(key)) {
    hatsHighlightRegistry.set(key, []);
  }
  return hatsHighlightRegistry.get(key);
}

/**
 * Apply highlight classes to all elements in a group based on hovered id
 */
function applyHatsHighlights(groupName, hoveredId) {
  const group = getHatsHighlightGroup(groupName);

  group.forEach(entry => {
    const { element, classifyFn } = entry;

    // Remove all highlight classes first
    ALL_HIGHLIGHT_CLASSES.forEach(cls => element.classList.remove(cls));

    // Get classification from PureScript thunked function
    // classifyFn is: String -> HighlightClass (Int)
    // It already has datum captured in closure
    const classification = classifyFn(hoveredId);

    // Apply appropriate class
    switch (classification) {
      case HC_PRIMARY:
        element.classList.add(HIGHLIGHT_PRIMARY);
        break;
      case HC_RELATED:
        element.classList.add(HIGHLIGHT_RELATED);
        break;
      case HC_DIMMED:
        element.classList.add(HIGHLIGHT_DIMMED);
        break;
      case HC_NEUTRAL:
      default:
        // No class applied
        break;
    }
  });
}

/**
 * Clear all highlight classes from a group
 */
function clearHatsHighlightsInGroup(groupName) {
  const group = getHatsHighlightGroup(groupName);
  group.forEach(entry => {
    ALL_HIGHLIGHT_CLASSES.forEach(cls => entry.element.classList.remove(cls));
  });
}

/**
 * Attach HATS coordinated highlight behavior to an element
 *
 * Uses thunked functions that have datum captured in closures.
 *
 * @param {Element} element - The DOM element to attach to
 * @param {Function} identifyThunk - PureScript (Unit -> String), returns this element's identity
 * @param {Function} classifyFn - PureScript (String -> Int), takes hoveredId, returns HighlightClass
 * @param {string|null} groupName - Optional group name (null for global)
 * @returns {Effect Unit}
 */
export const attachCoordinatedHighlightThunked = element => identifyThunk => classifyFn => groupName => () => {
  const group = groupName; // null means global

  // Register this element
  const entry = { element, identifyThunk, classifyFn };
  getHatsHighlightGroup(group).push(entry);

  // Attach mouseenter handler
  element.addEventListener('mouseenter', function(event) {
    // Get identity by invoking the thunk (datum is captured in closure)
    const hoveredId = identifyThunk();
    applyHatsHighlights(group, hoveredId);
  });

  // Attach mouseleave handler
  element.addEventListener('mouseleave', function(event) {
    clearHatsHighlightsInGroup(group);
  });
};

/**
 * Clear all HATS coordinated highlights (useful before re-rendering)
 */
export const clearAllHatsHighlights = () => {
  hatsHighlightRegistry.forEach((group, key) => {
    group.forEach(entry => {
      ALL_HIGHLIGHT_CLASSES.forEach(cls => entry.element.classList.remove(cls));
    });
  });
  hatsHighlightRegistry.clear();
};
