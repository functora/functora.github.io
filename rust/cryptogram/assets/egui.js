const OrigRO = window.ResizeObserver;
if (OrigRO) {
  const first = (v) => (Array.isArray(v) ? v[0] : v);
  window.ResizeObserver = class extends OrigRO {
    constructor(callback) {
      super((entries, observer) => {
        for (const entry of entries) {
          if (entry.devicePixelContentBoxSize && entry.contentBoxSize) {
            const dpr = window.devicePixelRatio || 1;
            const content = first(entry.contentBoxSize);
            const actual = first(entry.devicePixelContentBoxSize);
            if (content && actual && 'inlineSize' in content) {
              const expectedW = content.inlineSize * dpr;
              const expectedH = content.blockSize * dpr;
              if (
                Math.abs(actual.inlineSize - expectedW) > 1 ||
                Math.abs(actual.blockSize - expectedH) > 1
              ) {
                try {
                  Object.defineProperty(actual, 'inlineSize', {
                    value: expectedW,
                    configurable: true,
                  });
                  Object.defineProperty(actual, 'blockSize', {
                    value: expectedH,
                    configurable: true,
                  });
                } catch {}
              }
            }
          }
        }
        callback(entries, observer);
      });
    }
  };
}

export function focusCanvas(id = 'the_canvas_id') {
  document.getElementById(id)?.focus();
}

export function registerServiceWorker(vsn, url = './sw.js') {
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register(`${url}?cache=${vsn}`);
  }
}
