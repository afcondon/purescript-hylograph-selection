// Behaviours across rerenders, in a real browser.
//
//   spago build && node test/behaviours/check.mjs
//
// HATS redraws by handing the interpreter a new tree for the same container.
// An element that is kept must end up with exactly the behaviours the new
// tree gives it: one listener however many times it has been drawn, calling
// the handler from the latest tree. This renders trees repeatedly and counts.
import { spawnSync } from 'node:child_process';
import { readFileSync, mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const films = join(here, '../../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const chrome = process.env.CHROME || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';

const out = join(mkdtempSync(join(tmpdir(), 'hats-behaviours-')), 'bundle.js');
const built = spawnSync('esbuild', [join(here, 'entry.js'), '--bundle', '--format=iife', `--outfile=${out}`, '--log-level=warning'], { encoding: 'utf8' });
if (built.status !== 0) { console.error(built.stderr); process.exit(1); }

const browser = await puppeteer.launch({ executablePath: chrome, headless: true });
let failed = 0;
try {
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', (error) => errors.push(String(error)));
  await page.setContent('<!doctype html><html><body><div id="root"></div></body></html>');
  await page.addScriptTag({ content: readFileSync(out, 'utf8') });
  const run = (name, ...args) => page.evaluate((scenario, parameters) => window.scenarios[scenario](...parameters), name, args);
  const expect = (what, actual, expected) => {
    const ok = JSON.stringify(actual) === JSON.stringify(expected);
    console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}: ${JSON.stringify(actual)}${ok ? '' : ` (expected ${JSON.stringify(expected)})`}`);
    if (!ok) failed++;
  };

  for (const renders of [1, 2, 10]) {
    expect(`static element drawn ${renders}x, clicked once: handler calls`, await run('staticClicksPerClick', renders), 1);
  }
  expect('static element drawn 10x: mouseenter and pointerdown calls per event', await run('staticOtherEventsPerEvent', 10), { enter: 1, down: 1 });
  expect('static element redrawn with a new handler: the click sees', await run('staticHandlerSees'), ['second']);
  expect('static element redrawn without its handler: calls', await run('staticHandlerRemoved'), 0);
  expect('keyed item kept while its datum changes: the click sees', await run('keyedHandlerSees', 2), ['second']);
  expect('keyed item drawn 10x: the click sees', await run('keyedHandlerSees', 10), ['second']);
  expect('coordinated highlight drawn once: who is asked', await run('highlightAfterRerenders', 1), ['first asked about first-item', 'first-item asked about first-item']);
  expect('coordinated highlight drawn 5x: who is asked', await run('highlightAfterRerenders', 5), ['second asked about second-item', 'second-item asked about second-item']);
  expect('a slider drawn 5x, dragged to 70, set to 25 by a redraw, dragged to 40', await run('slider', 5), { heard: ['70', '40'], afterDrag: '70', afterRender: '25' });
  expect('kept elements are the same DOM elements', await run('elementsAreKept'), true);
  if (errors.length) { console.log(`FAIL the page threw: ${errors.join(' | ')}`); failed++; }
} finally {
  await browser.close();
}
console.log(failed ? `${failed} failed` : 'all passed');
process.exit(failed ? 1 : 0);
