// What the behaviours check runs in the page: HATS trees built from the
// compiled PureScript, rendered more than once into the same container.
// Each scenario returns plain data for check.mjs to judge.
import * as HATS from '../../output/Hylograph.HATS/index.js';
import * as Tick from '../../output/Hylograph.HATS.InterpreterTick/index.js';
import * as Element from '../../output/Hylograph.Element.Types/index.js';
import * as Behavior from '../../output/Hylograph.Behavior.Types/index.js';
import * as Maybe from '../../output/Data.Maybe/index.js';

const Div = Element.Div.value;
const div = (id, children = []) => HATS.elem(Div)([HATS.staticStr('id')(id)])(children);
const render = (tree) => Tick.rerender('#root')(tree)();
const fresh = () => { document.body.innerHTML = '<div id="root"></div>'; };
const click = (id) => document.getElementById(id).dispatchEvent(new MouseEvent('click', { bubbles: true }));
const enter = (id) => document.getElementById(id).dispatchEvent(new MouseEvent('mouseenter'));

window.scenarios = {
  // A static element with a click handler, rendered `renders` times, clicked once.
  staticClicksPerClick(renders) {
    fresh();
    let count = 0;
    const tree = () => div('outer', [HATS.withBehaviors([HATS.onClick(() => { count++; })])(div('button'))]);
    for (let i = 0; i < renders; i++) render(tree());
    click('button');
    return count;
  },

  // The same for mouseenter and pointerdown.
  staticOtherEventsPerEvent(renders) {
    fresh();
    const count = { enter: 0, down: 0 };
    const tree = () => div('outer', [HATS.withBehaviors([HATS.onMouseEnter(() => { count.enter++; }), HATS.onPointerDown(() => { count.down++; })])(div('target'))]);
    for (let i = 0; i < renders; i++) render(tree());
    enter('target');
    document.getElementById('target').dispatchEvent(new MouseEvent('pointerdown', { bubbles: true }));
    return count;
  },

  // A static element whose handler closes over a value that changes from one
  // render to the next: which value does a click see?
  staticHandlerSees() {
    fresh();
    const seen = [];
    const tree = (value) => div('outer', [HATS.withBehaviors([HATS.onClick(() => { seen.push(value); })])(div('button'))]);
    render(tree('first'));
    render(tree('second'));
    click('button');
    return seen;
  },

  // A handler that a later render no longer asks for.
  staticHandlerRemoved() {
    fresh();
    let count = 0;
    render(div('outer', [HATS.withBehaviors([HATS.onClick(() => { count++; })])(div('button'))]));
    render(div('outer', [div('button')]));
    click('button');
    return count;
  },

  // A keyed item, kept from one render to the next while its datum changes.
  keyedHandlerSees(renders) {
    fresh();
    const seen = [];
    const tree = (label) => div('outer', [
      HATS.forEach('items')(Div)([{ key: 'a', label }])((d) => d.key)((d) =>
        HATS.withBehaviors([HATS.onClick(() => { seen.push(d.label); })])(div('item'))),
    ]);
    render(tree('first'));
    for (let i = 1; i < renders; i++) render(tree('second'));
    click('item');
    return seen;
  },

  // Coordinated highlight: how many times is one element asked to classify a
  // hover, and with whose identity?
  highlightAfterRerenders(renders) {
    fresh();
    const asked = [];
    const tree = (name) => div('outer', [
      HATS.withBehaviors([HATS.onCoordinatedHighlight({
        identify: name,
        classify: (hovered) => { asked.push(`${name} asked about ${hovered}`); return Behavior.Primary.value; },
        group: new Maybe.Just('check'),
      })])(div('static')),
      HATS.forEach('items')(Div)([{ key: 'a', name }])((d) => d.key)((d) =>
        HATS.withBehaviors([HATS.onCoordinatedHighlight({
          identify: d.name + '-item',
          classify: (hovered) => { asked.push(`${d.name}-item asked about ${hovered}`); return Behavior.Neutral.value; },
          group: new Maybe.Just('check'),
        })])(div('item'))),
    ]);
    render(tree('first'));
    for (let i = 1; i < renders; i++) render(tree('second'));
    enter('item');
    return asked.sort();
  },

  // A slider: the handler is given the value the user chose, the tree's
  // `value` moves the control even after the user has moved it, and
  // redrawing does not stack handlers.
  slider(renders) {
    fresh();
    const Input = Element.Input.value;
    const heard = [];
    const tree = (value) => div('outer', [
      HATS.withBehaviors([HATS.onInput((chosen) => () => { heard.push(chosen); })])(
        HATS.elem(Input)([HATS.staticStr('id')('slider'), HATS.staticStr('type')('range'), HATS.staticStr('min')('0'), HATS.staticStr('max')('100'), HATS.staticStr('value')(String(value))])([])),
    ]);
    for (let i = 0; i < renders; i++) render(tree(10));
    const slider = document.getElementById('slider');
    const drag = (to) => { slider.value = String(to); slider.dispatchEvent(new Event('input', { bubbles: true })); };
    drag(70);
    const afterDrag = slider.value;
    render(tree(25)); // the program moves it
    const afterRender = slider.value;
    drag(40);
    return { heard, afterDrag, afterRender };
  },

  // The element is the same element throughout (a behaviour fix must not be
  // "throw the element away and make a new one").
  elementsAreKept() {
    fresh();
    const tree = (label) => div('outer', [
      HATS.withBehaviors([HATS.onClick(() => {})])(div('button')),
      HATS.forEach('items')(Div)([{ key: 'a', label }])((d) => d.key)((d) =>
        HATS.withBehaviors([HATS.onClick(() => {})])(div('item'))),
    ]);
    render(tree('first'));
    const before = [document.getElementById('button'), document.getElementById('item')];
    render(tree('second'));
    render(tree('third'));
    return before[0] === document.getElementById('button') && before[1] === document.getElementById('item');
  },
};
