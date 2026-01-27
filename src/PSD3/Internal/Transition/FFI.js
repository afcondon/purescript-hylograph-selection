// Native Web Animations API implementation - no D3 dependencies
//
// This module provides D3-transition-compatible API using native browser features.
// The transition object collects attributes and commits them via Web Animations API.

// =============================================================================
// Transition Handle (replaces D3 transition object)
// =============================================================================

// Create a transition handle that collects attributes and commits via Web Animations
// Mimics D3 transition mechanics:
// - Animate FROM element's current attribute values TO target values
// - Multiple attr() calls animate together as one transition
export function createTransition_(duration) {
  return function(delay) {
    return function(easingName) {
      return function(element) {
        return function() {
          // Create a transition handle that collects attrs and commits on microtask
          const handle = {
            element,
            duration,
            delay: delay ?? 0,
            easing: mapEasingToCss(easingName),
            attrs: [],           // [{name, value}] collected via transitionSetAttribute_
            removeOnEnd: false,  // set by transitionRemove_
            committed: false,
            animation: null
          };

          // Schedule commit on next microtask (after all attrs are set)
          queueMicrotask(() => commitTransition(handle));

          return handle;
        };
      };
    };
  };
}

// Set an attribute on a transition handle
// This collects the attribute for animation (committed on microtask)
export function transitionSetAttribute_(name) {
  return function(value) {
    return function(handle) {
      return function() {
        if (handle.committed) {
          // Already committed - apply directly (shouldn't happen in normal usage)
          handle.element.setAttribute(name, value);
          return;
        }
        handle.attrs.push({ name, value });
      };
    };
  };
}

// Mark transition to remove element after completion
export function transitionRemove_(handle) {
  return function() {
    handle.removeOnEnd = true;
  };
}

// =============================================================================
// Internal: Commit the transition using Web Animations API
// =============================================================================

function commitTransition(handle) {
  if (handle.committed) return;
  handle.committed = true;

  const { element, duration, delay, easing, attrs, removeOnEnd } = handle;

  if (attrs.length === 0) {
    // No attributes to animate
    if (removeOnEnd) {
      // Just remove after delay
      setTimeout(() => element.remove(), delay);
    }
    return;
  }

  // Build keyframes: from current values to target values
  const fromFrame = {};
  const toFrame = {};

  for (const { name, value } of attrs) {
    // Get current value (what we animate FROM)
    const currentValue = element.getAttribute(name) ?? getDefaultAttrValue(name);

    // Map attribute names to CSS property names for Web Animations
    const cssName = attrToCssProp(name);
    fromFrame[cssName] = currentValue;
    toFrame[cssName] = value;
  }

  // Create the animation
  try {
    const animation = element.animate(
      [fromFrame, toFrame],
      {
        duration,
        delay,
        easing,
        fill: 'forwards' // Keep final values
      }
    );

    handle.animation = animation;

    // On finish: commit final attribute values and optionally remove
    animation.onfinish = () => {
      // Commit final values as actual attributes (Web Animations fill:'forwards'
      // only affects CSS, not SVG attributes)
      for (const { name, value } of attrs) {
        element.setAttribute(name, value);
      }

      if (removeOnEnd) {
        element.remove();
      }
    };

  } catch (e) {
    // Fallback: instant attribute set (some attrs can't be animated)
    for (const { name, value } of attrs) {
      element.setAttribute(name, value);
    }
    if (removeOnEnd) {
      setTimeout(() => element.remove(), delay + duration);
    }
  }
}

// =============================================================================
// Easing mapping: D3 easing names to CSS easing functions
// =============================================================================

function mapEasingToCss(name) {
  if (!name) return 'ease-out';

  // Map D3 easing names to CSS cubic-bezier approximations
  // Note: Some D3 easings (elastic, bounce) don't have CSS equivalents
  // and fall back to reasonable approximations
  const easingMap = {
    // Linear
    'linear': 'linear',

    // Polynomial (Quad) - CSS ease equivalents
    'quad': 'ease-out',
    'quadIn': 'ease-in',
    'quadOut': 'ease-out',
    'quadInOut': 'ease-in-out',

    // Cubic - CSS ease equivalents (very close)
    'cubic': 'cubic-bezier(0.215, 0.61, 0.355, 1)',
    'cubicIn': 'cubic-bezier(0.55, 0.055, 0.675, 0.19)',
    'cubicOut': 'cubic-bezier(0.215, 0.61, 0.355, 1)',
    'cubicInOut': 'cubic-bezier(0.645, 0.045, 0.355, 1)',

    // Sinusoidal
    'sin': 'cubic-bezier(0.39, 0.575, 0.565, 1)',
    'sinIn': 'cubic-bezier(0.47, 0, 0.745, 0.715)',
    'sinOut': 'cubic-bezier(0.39, 0.575, 0.565, 1)',
    'sinInOut': 'cubic-bezier(0.445, 0.05, 0.55, 0.95)',

    // Exponential
    'exp': 'cubic-bezier(0.19, 1, 0.22, 1)',
    'expIn': 'cubic-bezier(0.95, 0.05, 0.795, 0.035)',
    'expOut': 'cubic-bezier(0.19, 1, 0.22, 1)',
    'expInOut': 'cubic-bezier(1, 0, 0, 1)',

    // Circular
    'circle': 'cubic-bezier(0.075, 0.82, 0.165, 1)',
    'circleIn': 'cubic-bezier(0.6, 0.04, 0.98, 0.335)',
    'circleOut': 'cubic-bezier(0.075, 0.82, 0.165, 1)',
    'circleInOut': 'cubic-bezier(0.785, 0.135, 0.15, 0.86)',

    // These don't have CSS equivalents - use approximations
    'elastic': 'cubic-bezier(0.68, -0.55, 0.265, 1.55)',
    'elasticIn': 'cubic-bezier(0.6, -0.28, 0.735, 0.045)',
    'elasticOut': 'cubic-bezier(0.175, 0.885, 0.32, 1.275)',
    'elasticInOut': 'cubic-bezier(0.68, -0.55, 0.265, 1.55)',

    'back': 'cubic-bezier(0.175, 0.885, 0.32, 1.275)',
    'backIn': 'cubic-bezier(0.6, -0.28, 0.735, 0.045)',
    'backOut': 'cubic-bezier(0.175, 0.885, 0.32, 1.275)',
    'backInOut': 'cubic-bezier(0.68, -0.55, 0.265, 1.55)',

    'bounce': 'ease-out',
    'bounceIn': 'ease-in',
    'bounceOut': 'ease-out',
    'bounceInOut': 'ease-in-out',
  };

  return easingMap[name] || 'ease-out';
}

// =============================================================================
// SVG Attribute helpers
// =============================================================================

// Map SVG attribute names to CSS property names for Web Animations
function attrToCssProp(name) {
  // Most SVG attributes can be animated directly
  // Some need CSS property name mapping
  const mapping = {
    'fill': 'fill',
    'stroke': 'stroke',
    'opacity': 'opacity',
    'fill-opacity': 'fillOpacity',
    'stroke-opacity': 'strokeOpacity',
    'stroke-width': 'strokeWidth',
    'transform': 'transform',
    // Positional attributes are camelCased in Web Animations
    'cx': 'cx',
    'cy': 'cy',
    'r': 'r',
    'x': 'x',
    'y': 'y',
    'x1': 'x1',
    'y1': 'y1',
    'x2': 'x2',
    'y2': 'y2',
    'width': 'width',
    'height': 'height',
    'rx': 'rx',
    'ry': 'ry',
    'd': 'd',
  };
  return mapping[name] || name;
}

// Get sensible default values for common SVG attributes
function getDefaultAttrValue(name) {
  const defaults = {
    'opacity': '1',
    'fill-opacity': '1',
    'stroke-opacity': '1',
    'stroke-width': '1',
    'r': '0',
    'cx': '0',
    'cy': '0',
    'x': '0',
    'y': '0',
    'width': '0',
    'height': '0',
  };
  return defaults[name] || '0';
}
