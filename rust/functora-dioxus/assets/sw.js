const CACHE = new URL(self.location.href).searchParams.get('cache');
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', () => self.clients.claim());
self.addEventListener('fetch', (e) => {
    const url = new URL(e.request.url);
    if (url.origin !== self.location.origin || e.request.method !== 'GET') return;
    const isNav = e.request.mode === 'navigate';
    e.respondWith(
        caches.open(CACHE).then((cache) =>
            cache.match(e.request).then((cached) => {
                if (cached) return cached;
                return fetch(e.request).then((resp) => {
                    if (resp.ok) cache.put(e.request, resp.clone());
                    return resp;
                }).catch(() => isNav ? cache.match('/') : undefined);
            })
        )
    );
});
