// FFI for D3 zoom and drag behaviors
// D3 dependencies: d3-selection, d3-zoom, d3-drag
import { select, selectAll, pointer } from "d3-selection";

/**
 * Update an element's attribute by selector
 * @param {string} selector - CSS selector
 * @param {string} attr - Attribute name
 * @param {string} value - Attribute value
 */
export function updateAttr_(selector) {
  return attr => value => () => {
    select(selector).attr(attr, value);
  };
}
import { zoom, zoomIdentity, zoomTransform } from "d3-zoom";
import { drag } from "d3-drag";

// =============================================================================
// Simulation Registry
// =============================================================================
// Global registry for named simulations, enabling declarative SimulationDrag
// Each entry maps simulationId -> { reheat: Effect Unit }

const simulationRegistry = new Map();

/**
 * Register a simulation by ID
 * @param {string} simId - Unique identifier for the simulation
 * @param {Function} reheatFn - PureScript Effect Unit function to reheat the simulation
 */
export function registerSimulation_(simId) {
  return reheatFn => () => {
    simulationRegistry.set(simId, { reheat: reheatFn });
    console.log(`[SimRegistry] Registered simulation: ${simId}`);
  };
}

/**
 * Unregister a simulation by ID
 * @param {string} simId - The simulation ID to remove
 */
export function unregisterSimulation_(simId) {
  return () => {
    simulationRegistry.delete(simId);
    console.log(`[SimRegistry] Unregistered simulation: ${simId}`);
  };
}

/**
 * Check if a simulation is registered
 * @param {string} simId - The simulation ID to check
 * @returns {boolean}
 */
export function isSimulationRegistered_(simId) {
  return () => simulationRegistry.has(simId);
}

// Internal helper to get simulation's reheat function
function getSimulationReheat(simId) {
  const sim = simulationRegistry.get(simId);
  return sim ? sim.reheat : null;
}

/**
 * Attach simulation-aware drag behavior using the registry
 * Looks up the simulation by ID and calls its reheat function on drag start
 * @param {Element} element - The DOM element to attach drag to
 * @param {string} simId - The registered simulation ID
 * @returns {Element} The element (for chaining)
 */
export function attachSimulationDragById_(element) {
  return simId => () => {
    const selection = select(element);

    function dragstarted(event) {
      // Look up and call the registered reheat function
      const reheat = getSimulationReheat(simId);
      if (reheat) {
        reheat();  // Call PureScript Effect Unit
      } else {
        console.warn(`[SimulationDrag] No simulation registered with ID: ${simId}`);
      }
      // Set fixed position
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }

    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }

    function dragended(event) {
      // Release fixed position
      event.subject.fx = null;
      event.subject.fy = null;
    }

    const dragBehavior = drag()
      .on('start', dragstarted)
      .on('drag', dragged)
      .on('end', dragended);

    selection
      .call(dragBehavior)
      .style('cursor', 'grab');

    return element;
  };
}

/**
 * Attach simulation-aware drag for nested datum structure
 * Like attachSimulationDragById_ but accesses .node field for fx/fy
 * Used when datum is a wrapper containing the actual simulation node
 * @param {Element} element - The DOM element to attach drag to
 * @param {string} simId - The registered simulation ID
 * @returns {Element} The element (for chaining)
 */
export function attachSimulationDragNestedById_(element) {
  return simId => () => {
    const selection = select(element);

    function dragstarted(event) {
      // Look up and call the registered reheat function
      const reheat = getSimulationReheat(simId);
      if (reheat) {
        reheat();  // Call PureScript Effect Unit
      } else {
        console.warn(`[SimulationDragNested] No simulation registered with ID: ${simId}`);
      }
      // Set fixed position on the nested node
      const node = event.subject.node;
      node.fx = node.x;
      node.fy = node.y;
    }

    function dragged(event) {
      const node = event.subject.node;
      node.fx = event.x;
      node.fy = event.y;
    }

    function dragended(event) {
      // Release fixed position
      const node = event.subject.node;
      node.fx = null;
      node.fy = null;
    }

    const dragBehavior = drag()
      .on('start', dragstarted)
      .on('drag', dragged)
      .on('end', dragended);

    selection
      .call(dragBehavior)
      .style('cursor', 'grab');

    return element;
  };
}

/**
 * Attach zoom behavior to an element
 * @param {Element} element - The DOM element to attach zoom to (typically SVG)
 * @param {number} scaleMin - Minimum zoom scale
 * @param {number} scaleMax - Maximum zoom scale
 * @param {string} targetSelector - CSS selector for the element to transform
 * @returns {Element} The element (for chaining)
 */
export function attachZoom_(element) {
  return scaleMin => scaleMax => targetSelector => () => {
    // Create D3 selection from element
    const selection = select(element);

    // Query target lazily on each zoom event (not eagerly at setup time)
    // This allows behaviors to be attached before children are rendered
    function zoomed(event) {
      const target = selection.select(targetSelector);
      target.attr('transform', event.transform);
    }

    const zoomBehavior = zoom()
      .scaleExtent([scaleMin, scaleMax])
      .on('zoom', zoomed);

    selection.call(zoomBehavior);

    return element;
  };
}

/**
 * Get the current zoom transform from an element
 * Returns {k, x, y} or identity transform if none set
 * @param {Element} element - The DOM element with zoom behavior
 * @returns {{k: number, x: number, y: number}} The zoom transform
 */
export function getZoomTransform_(element) {
  return () => {
    try {
      const t = zoomTransform(element);
      return { k: t.k, x: t.x, y: t.y };
    } catch (e) {
      // Return identity if no transform exists
      return { k: 1, x: 0, y: 0 };
    }
  };
}

/**
 * Attach zoom behavior and restore a previous transform
 * @param {Element} element - The DOM element to attach zoom to
 * @param {number} scaleMin - Minimum zoom scale
 * @param {number} scaleMax - Maximum zoom scale
 * @param {string} targetSelector - CSS selector for the element to transform
 * @param {{k: number, x: number, y: number}} transform - Transform to restore
 * @returns {Element} The element (for chaining)
 */
export function attachZoomWithTransform_(element) {
  return scaleMin => scaleMax => targetSelector => transform => () => {
    const selection = select(element);

    function zoomed(event) {
      const target = selection.select(targetSelector);
      target.attr('transform', event.transform);
    }

    const zoomBehavior = zoom()
      .scaleExtent([scaleMin, scaleMax])
      .on('zoom', zoomed);

    selection.call(zoomBehavior);

    // Restore the previous transform
    const t = zoomIdentity.translate(transform.x, transform.y).scale(transform.k);
    selection.call(zoomBehavior.transform, t);

    return element;
  };
}

/**
 * Attach zoom behavior with callback on zoom events
 * Like attachZoomWithTransform_ but also calls a callback with the current transform
 * @param {Element} element - The DOM element to attach zoom to
 * @param {number} scaleMin - Minimum zoom scale
 * @param {number} scaleMax - Maximum zoom scale
 * @param {string} targetSelector - CSS selector for the element to transform
 * @param {{k: number, x: number, y: number}} initialTransform - Transform to restore
 * @param {Function} onZoom - PureScript (ZoomTransform -> Effect Unit) callback
 * @returns {Element} The element (for chaining)
 */
export function attachZoomWithCallback_(element) {
  return scaleMin => scaleMax => targetSelector => initialTransform => onZoom => () => {
    const selection = select(element);

    function zoomed(event) {
      const target = selection.select(targetSelector);
      target.attr('transform', event.transform);
      // Call PureScript callback with transform
      const t = { k: event.transform.k, x: event.transform.x, y: event.transform.y };
      onZoom(t)();
    }

    const zoomBehavior = zoom()
      .scaleExtent([scaleMin, scaleMax])
      .on('zoom', zoomed);

    selection.call(zoomBehavior);

    // Restore the previous transform
    const t = zoomIdentity.translate(initialTransform.x, initialTransform.y).scale(initialTransform.k);
    selection.call(zoomBehavior.transform, t);

    return element;
  };
}

/**
 * Attach simple drag behavior to an element
 * @param {Element} element - The DOM element to attach drag to
 * @returns {Element} The element (for chaining)
 */
export function attachSimpleDrag_(element) {
  return () => () => {
    // Create D3 selection from element
    const selection = select(element);

    let transform = { x: 0, y: 0 };

    function dragstarted(event) {
      select(this).raise();
    }

    function dragged(event) {
      transform.x += event.dx;
      transform.y += event.dy;
      select(this).attr('transform', `translate(${transform.x},${transform.y})`);
    }

    const dragBehavior = drag()
      .on('start', dragstarted)
      .on('drag', dragged);

    selection.call(dragBehavior);

    return element;
  };
}

/**
 * Attach simulation-aware drag behavior to an element
 * @param {Element} element - The DOM element to attach drag to
 * @param {d3.Simulation|null} simulation - The D3 force simulation
 * @param {string} label - Event namespace label
 * @returns {Element} The element (for chaining)
 */
export function attachSimulationDrag_(element) {
  return simulation => label => () => {
    // Create D3 selection from element
    const selection = select(element);

    function dragstarted(event) {
      if (simulation) {
        // Always restart if simulation has stopped (alpha = 0)
        // Only reheat if this is the first concurrent drag (!event.active)
        if (!event.active || simulation.alpha() < 0.001) {
          simulation.alphaTarget(0.3).restart();
        }
      }
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }

    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }

    function dragended(event) {
      if (simulation && !event.active) {
        simulation.alphaTarget(0);
      }
      event.subject.fx = null;
      event.subject.fy = null;
    }

    const dragBehavior = drag()
      .on('start.' + label, dragstarted)
      .on('drag.' + label, dragged)
      .on('end.' + label, dragended);

    selection
      .call(dragBehavior)
      .style('cursor', 'pointer');  // Set cursor to pointer for draggable elements

    return element;
  };
}

/**
 * Attach click handler without datum access
 * @param {Element} element - The DOM element to attach click handler to
 * @param {Function} handler - The PureScript Effect Unit handler
 * @returns {Element} The element (for chaining)
 */
export function attachClick_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = select(element);

    // Attach click event listener
    selection.on('click', function(event) {
      // Call PureScript handler (it's already an Effect, so invoke it)
      handler();
    });

    // Set cursor to pointer for clickable elements
    selection.style('cursor', 'pointer');

    return element;
  };
}

/**
 * Attach click handler with datum access
 * @param {Element} element - The DOM element to attach click handler to
 * @param {Function} handler - The PureScript (datum -> Effect Unit) handler
 * @returns {Element} The element (for chaining)
 */
export function attachClickWithDatum_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = select(element);

    // Attach click event listener
    selection.on('click', function(event, d) {
      // D3 v6+ passes datum as second argument
      // Call PureScript handler with datum (it returns an Effect, so invoke it)
      handler(d)();
    });

    // Set cursor to pointer for clickable elements
    selection.style('cursor', 'pointer');

    return element;
  };
}

/**
 * Attach mouseenter handler with datum access
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript (datum -> Effect Unit) handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseEnter_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = select(element);

    // Attach mouseenter event listener
    selection.on('mouseenter', function(event, d) {
      // D3 v6+ passes datum as second argument
      // Call PureScript handler with datum (it returns an Effect, so invoke it)
      handler(d)();
    });

    return element;
  };
}

/**
 * Attach mouseleave handler with datum access
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript (datum -> Effect Unit) handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseLeave_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = select(element);

    // Attach mouseleave event listener
    selection.on('mouseleave', function(event, d) {
      // D3 v6+ passes datum as second argument
      // Call PureScript handler with datum (it returns an Effect, so invoke it)
      handler(d)();
    });

    return element;
  };
}

/**
 * Attach hover highlight behavior
 * @param {Element} element - The DOM element to attach to
 * @param {Array} enterStyles - Array of {attr, value} to apply on enter
 * @param {Array} leaveStyles - Array of {attr, value} to apply on leave
 * @returns {Element} The element (for chaining)
 */
export function attachHighlight_(element) {
  return enterStyles => leaveStyles => () => {
    // Create D3 selection from element
    const selection = select(element);

    // Attach mouseenter handler
    selection.on('mouseenter', function(event) {
      const sel = select(this);
      // Raise element to front
      sel.raise();
      // Apply enter styles
      enterStyles.forEach(style => {
        sel.attr(style.attr, style.value);
      });
    });

    // Attach mouseleave handler
    selection.on('mouseleave', function(event) {
      const sel = select(this);
      // Apply leave styles
      leaveStyles.forEach(style => {
        sel.attr(style.attr, style.value);
      });
    });

    return element;
  };
}

/**
 * Attach mousemove handler using pure web-events (no D3)
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript EffectFn2 datum MouseEvent Unit handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseMoveWithEvent_(element) {
  return handler => () => {
    element.addEventListener('mousemove', function(event) {
      // Get datum from element's __data__ property (D3 convention)
      const datum = this.__data__;
      handler(datum, event);
    });
    return element;
  };
}

/**
 * DEPRECATED: D3-based version - use attachMouseMoveWithEvent_ instead
 */
export function attachMouseMoveWithInfo_(element) {
  return handler => () => {
    const selection = select(element);

    selection.on('mousemove', function(event, d) {
      const info = {
        clientX: event.clientX,
        clientY: event.clientY,
        pageX: event.pageX,
        pageY: event.pageY,
        offsetX: event.offsetX,
        offsetY: event.offsetY
      };
      handler(d)(info)();
    });

    return element;
  };
}

/**
 * Attach mouseenter handler using pure web-events (no D3)
 */
export function attachMouseEnterWithEvent_(element) {
  return handler => () => {
    element.addEventListener('mouseenter', function(event) {
      const datum = this.__data__;
      handler(datum, event);
    });
    return element;
  };
}

/**
 * DEPRECATED: D3-based version
 */
export function attachMouseEnterWithInfo_(element) {
  return handler => () => {
    const selection = select(element);

    selection.on('mouseenter', function(event, d) {
      const info = {
        clientX: event.clientX,
        clientY: event.clientY,
        pageX: event.pageX,
        pageY: event.pageY,
        offsetX: event.offsetX,
        offsetY: event.offsetY
      };
      handler(d)(info)();
    });

    return element;
  };
}

/**
 * Attach mouseleave handler using pure web-events (no D3)
 */
export function attachMouseLeaveWithEvent_(element) {
  return handler => () => {
    element.addEventListener('mouseleave', function(event) {
      const datum = this.__data__;
      handler(datum, event);
    });
    return element;
  };
}

/**
 * DEPRECATED: D3-based version
 */
export function attachMouseLeaveWithInfo_(element) {
  return handler => () => {
    const selection = select(element);

    selection.on('mouseleave', function(event, d) {
      const info = {
        clientX: event.clientX,
        clientY: event.clientY,
        pageX: event.pageX,
        pageY: event.pageY,
        offsetX: event.offsetX,
        offsetY: event.offsetY
      };
      handler(d)(info)();
    });

    return element;
  };
}

/**
 * Attach mousedown handler without datum access
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript Effect Unit handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseDown_(element) {
  return handler => () => {
    const selection = select(element);

    selection.on('mousedown', function(event) {
      // Call PureScript handler
      handler();
    });

    return element;
  };
}

/**
 * Attach mousedown handler with event info (using web-events)
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript EffectFn2 datum MouseEvent Unit handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseDownWithEvent_(element) {
  return handler => () => {
    element.addEventListener('mousedown', function(event) {
      const datum = this.__data__;
      handler(datum, event);
    });
    return element;
  };
}

/**
 * Attach line chart tooltip behavior
 * Shows series name and interpolated value at mouse position
 */
export function attachLineTooltip_(svgElement) {
  return pathElement => seriesName => points => margin => () => {
    const svg = select(svgElement);
    const path = select(pathElement);

    // Get or create tooltip div
    let tooltip = select('body').select('.line-tooltip');
    if (tooltip.empty()) {
      tooltip = select('body')
        .append('div')
        .attr('class', 'line-tooltip')
        .style('position', 'absolute')
        .style('background', 'rgba(0, 0, 0, 0.8)')
        .style('color', 'white')
        .style('padding', '8px 12px')
        .style('border-radius', '4px')
        .style('font-size', '12px')
        .style('pointer-events', 'none')
        .style('opacity', 0)
        .style('z-index', 1000);
    }

    // Sort points by x for binary search
    const sortedPoints = [...points].sort((a, b) => a.x - b.x);

    // Find nearest point by x coordinate
    function findNearestPoint(mouseX) {
      // Adjust for margin
      const x = mouseX - margin.left;

      // Binary search for nearest point
      let left = 0;
      let right = sortedPoints.length - 1;

      while (right - left > 1) {
        const mid = Math.floor((left + right) / 2);
        if (sortedPoints[mid].x < x) {
          left = mid;
        } else {
          right = mid;
        }
      }

      // Return closer of the two
      if (right >= sortedPoints.length) return sortedPoints[left];
      if (left < 0) return sortedPoints[right];

      const dLeft = Math.abs(sortedPoints[left].x - x);
      const dRight = Math.abs(sortedPoints[right].x - x);
      return dLeft < dRight ? sortedPoints[left] : sortedPoints[right];
    }

    // Attach events
    path
      .on('mouseenter.tooltip', function(event) {
        // Highlight the line
        select(this)
          .raise()
          .attr('stroke', '#333')
          .attr('stroke-width', 2.5);

        tooltip
          .style('opacity', 1);
      })
      .on('mousemove.tooltip', function(event) {
        // Get mouse position relative to SVG
        const [mouseX, mouseY] = pointer(event, svgElement);

        // Find nearest data point
        const point = findNearestPoint(mouseX);

        // Update tooltip content
        tooltip
          .html(`<strong>${seriesName}</strong><br/>${point.label}: ${point.y.toFixed(1)}%`)
          .style('left', (event.pageX + 15) + 'px')
          .style('top', (event.pageY - 10) + 'px');
      })
      .on('mouseleave.tooltip', function(event) {
        // Reset line style
        select(this)
          .attr('stroke', '#ddd')
          .attr('stroke-width', 1.5);

        tooltip
          .style('opacity', 0);
      });

    return pathElement;
  };
}

// =============================================================================
// Tier 2: Coordinated Highlighting
// =============================================================================

// Global registry of elements participating in coordinated highlighting
// Key: group name (or "_global" for ungrouped)
// Value: Array of { element, datum, identifyFn, classifyFn }
const highlightRegistry = new Map();

// CSS class names
const HIGHLIGHT_PRIMARY = 'highlight-primary';
const HIGHLIGHT_RELATED = 'highlight-related';
const HIGHLIGHT_DIMMED = 'highlight-dimmed';
const ALL_HIGHLIGHT_CLASSES = [HIGHLIGHT_PRIMARY, HIGHLIGHT_RELATED, HIGHLIGHT_DIMMED];

// TODO: TECH DEBT - These enum mappings assume specific Int representation from PureScript
// highlightClassToInt and tooltipTriggerToInt. This is fragile. A safer approach would be
// to pass the class name as a string or use a tagged union that can be introspected.
// For now, the PureScript side explicitly converts via highlightClassToInt/tooltipTriggerToInt
// in Operations.purs to ensure alignment.
const HC_PRIMARY = 0;
const HC_RELATED = 1;
const HC_DIMMED = 2;
const HC_NEUTRAL = 3;

const TT_ON_HOVER = 0;
const TT_WHEN_PRIMARY = 1;
const TT_WHEN_RELATED = 2;

// =============================================================================
// Tooltip Management
// =============================================================================

// Container for all coordinated tooltips
let tooltipContainer = null;
// Map of element -> tooltip div (for WhenPrimary/WhenRelated tooltips)
const elementTooltips = new Map();
// The currently active hover tooltip (for OnHover)
let hoverTooltip = null;

/**
 * Get or create the tooltip container
 */
function getTooltipContainer() {
  if (!tooltipContainer) {
    tooltipContainer = document.createElement('div');
    tooltipContainer.className = 'coordinated-tooltip-container';
    tooltipContainer.style.cssText = 'position: fixed; top: 0; left: 0; pointer-events: none; z-index: 10000;';
    document.body.appendChild(tooltipContainer);
  }
  return tooltipContainer;
}

/**
 * Create a tooltip element
 * @returns {HTMLDivElement}
 */
function createTooltipElement() {
  const tooltip = document.createElement('div');
  tooltip.className = 'coordinated-tooltip';
  tooltip.style.cssText = `
    position: absolute;
    background: rgba(15, 23, 42, 0.95);
    color: #e2e8f0;
    padding: 6px 10px;
    border-radius: 4px;
    font-size: 12px;
    font-family: system-ui, -apple-system, sans-serif;
    white-space: nowrap;
    pointer-events: none;
    opacity: 0;
    transition: opacity 0.15s ease-in-out;
    box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
    border: 1px solid rgba(148, 163, 184, 0.2);
  `;
  getTooltipContainer().appendChild(tooltip);
  return tooltip;
}

/**
 * Position a tooltip near an element
 * @param {HTMLElement} tooltip - The tooltip element
 * @param {Element} targetElement - The element to position near
 */
function positionTooltipNearElement(tooltip, targetElement) {
  const rect = targetElement.getBoundingClientRect();
  const tooltipRect = tooltip.getBoundingClientRect();

  // Position above the element, centered
  let x = rect.left + rect.width / 2 - tooltipRect.width / 2;
  let y = rect.top - tooltipRect.height - 8;

  // Keep within viewport
  x = Math.max(8, Math.min(x, window.innerWidth - tooltipRect.width - 8));
  if (y < 8) {
    // Position below if not enough space above
    y = rect.bottom + 8;
  }

  tooltip.style.left = `${x}px`;
  tooltip.style.top = `${y}px`;
}

/**
 * Position a tooltip at mouse position
 * @param {HTMLElement} tooltip - The tooltip element
 * @param {MouseEvent} event - The mouse event
 */
function positionTooltipAtMouse(tooltip, event) {
  const x = event.clientX + 12;
  const y = event.clientY - 8;
  tooltip.style.left = `${x}px`;
  tooltip.style.top = `${y}px`;
}

/**
 * Show a tooltip with content
 * @param {HTMLElement} tooltip - The tooltip element
 * @param {string} content - The text content
 */
function showTooltip(tooltip, content) {
  tooltip.textContent = content;
  tooltip.style.opacity = '1';
}

/**
 * Hide a tooltip
 * @param {HTMLElement} tooltip - The tooltip element
 */
function hideTooltip(tooltip) {
  if (tooltip) {
    tooltip.style.opacity = '0';
  }
}

/**
 * Hide all tooltips
 */
function hideAllTooltips() {
  if (hoverTooltip) {
    hideTooltip(hoverTooltip);
  }
  elementTooltips.forEach(tooltip => hideTooltip(tooltip));
}

/**
 * Get or create a highlight group
 * @param {string|null} groupName - Group name or null for global
 * @returns {Array} The group's element array
 */
function getHighlightGroup(groupName) {
  const key = groupName || '_global';
  if (!highlightRegistry.has(key)) {
    highlightRegistry.set(key, []);
  }
  return highlightRegistry.get(key);
}

/**
 * Apply highlight classes and tooltips to all elements in a group based on hovered id
 * @param {string|null} groupName - Group name
 * @param {string} hoveredId - The identity of the hovered element
 * @param {Element} triggerElement - The element that triggered the highlight (for OnHover tooltips)
 * @param {MouseEvent|null} event - The mouse event (for positioning OnHover tooltips)
 */
function applyHighlights(groupName, hoveredId, triggerElement, event) {
  const group = getHighlightGroup(groupName);

  group.forEach(entry => {
    const { element, classifyFn, tooltipContentFn, tooltipTrigger } = entry;
    // IMPORTANT: Read datum fresh from element, not from cached entry
    // because __data__ may not have been set when behavior was attached
    const datum = element.__data__;
    const sel = select(element);

    // Remove all highlight classes first
    ALL_HIGHLIGHT_CLASSES.forEach(cls => sel.classed(cls, false));

    // Skip if no datum bound to element
    if (!datum) {
      console.warn('[CoordHighlight] applyHighlights: No datum on element', element);
      return;
    }

    // Get classification from PureScript function
    // classifyFn is curried: hoveredId -> datum -> Int
    const classification = classifyFn(hoveredId)(datum);

    // Apply appropriate class
    switch (classification) {
      case HC_PRIMARY:
        sel.classed(HIGHLIGHT_PRIMARY, true);
        break;
      case HC_RELATED:
        sel.classed(HIGHLIGHT_RELATED, true);
        break;
      case HC_DIMMED:
        sel.classed(HIGHLIGHT_DIMMED, true);
        break;
      case HC_NEUTRAL:
      default:
        // No class applied
        break;
    }

    // Handle tooltips if configured
    if (tooltipContentFn) {
      const content = tooltipContentFn(datum);
      const shouldShow =
        (tooltipTrigger === TT_ON_HOVER && element === triggerElement) ||
        (tooltipTrigger === TT_WHEN_PRIMARY && classification === HC_PRIMARY) ||
        (tooltipTrigger === TT_WHEN_RELATED && (classification === HC_PRIMARY || classification === HC_RELATED));

      if (shouldShow) {
        // Get or create tooltip for this element
        let tooltip = elementTooltips.get(element);
        if (!tooltip) {
          tooltip = createTooltipElement();
          elementTooltips.set(element, tooltip);
        }

        showTooltip(tooltip, content);

        // Position based on trigger type
        if (tooltipTrigger === TT_ON_HOVER && event) {
          positionTooltipAtMouse(tooltip, event);
        } else {
          positionTooltipNearElement(tooltip, element);
        }
      } else {
        // Hide tooltip if it exists
        const tooltip = elementTooltips.get(element);
        if (tooltip) {
          hideTooltip(tooltip);
        }
      }
    }
  });
}

/**
 * Clear all highlight classes and tooltips from a group
 * @param {string|null} groupName - Group name
 */
function clearHighlightsInGroup(groupName) {
  const group = getHighlightGroup(groupName);

  group.forEach(entry => {
    const sel = select(entry.element);
    ALL_HIGHLIGHT_CLASSES.forEach(cls => sel.classed(cls, false));

    // Hide tooltip if it exists
    const tooltip = elementTooltips.get(entry.element);
    if (tooltip) {
      hideTooltip(tooltip);
    }
  });
}

/**
 * Attach coordinated highlight behavior to an element
 *
 * When this element is hovered, ALL elements in the same group receive
 * highlight classes based on their classifyFn.
 *
 * @param {Element} element - The DOM element to attach to
 * @param {Function} identifyFn - PureScript (datum -> String)
 * @param {Function} classifyFn - PureScript (String -> datum -> Int)
 * @param {string|null} groupName - Optional group name (null for global)
 * @param {Function|null} tooltipContentFn - Optional PureScript (datum -> String) for tooltip content
 * @param {number} tooltipTrigger - When to show tooltip (0=OnHover, 1=WhenPrimary, 2=WhenRelated)
 * @returns {Element} The element (for chaining)
 */
export function attachCoordinatedHighlight_(element) {
  return identifyFn => classifyFn => groupName => tooltipContentFn => tooltipTrigger => () => {
    const sel = select(element);
    const group = groupName; // null means global
    const groupKey = group || '_global';

    // Register this element with tooltip config
    // Note: datum may not be set yet, will be read fresh in applyHighlights
    const entry = { element, identifyFn, classifyFn, tooltipContentFn, tooltipTrigger };
    getHighlightGroup(group).push(entry);

    // Attach mouseenter handler
    sel.on('mouseenter.coordinated', function(event) {
      const d = this.__data__;
      if (!d) {
        console.warn('[CoordHighlight] mouseenter: No datum on element');
        return;
      }
      const id = identifyFn(d);
      applyHighlights(group, id, element, event);
    });

    // Attach mouseleave handler
    sel.on('mouseleave.coordinated', function(event) {
      clearHighlightsInGroup(group);
    });

    return element;
  };
}

/**
 * Clear all highlight classes and tooltips from all groups
 * Also clears the registry (useful for cleanup before re-rendering)
 */
export function clearAllHighlights_() {
  highlightRegistry.forEach((group, key) => {
    group.forEach(entry => {
      const sel = select(entry.element);
      ALL_HIGHLIGHT_CLASSES.forEach(cls => sel.classed(cls, false));
    });
  });

  // Hide and remove all tooltips
  elementTooltips.forEach(tooltip => {
    if (tooltip.parentNode) {
      tooltip.parentNode.removeChild(tooltip);
    }
  });
  elementTooltips.clear();

  // Clear registry - elements will re-register on next render
  highlightRegistry.clear();
}
