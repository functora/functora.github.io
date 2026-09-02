const CACHE = new URL(self.location.href).searchParams.get('cache');
self.addEventListener('install', (e) => {
    self.skipWaiting();
    e.waitUntil(caches.open(CACHE).then((cache) => cache.add(self.registration.scope)));
});
self.addEventListener('activate', () => self.clients.claim());
self.addEventListener('fetch', (e) => {
    const url = new URL(e.request.url);
    if (url.origin !== self.location.origin || e.request.method !== 'GET') return;
    const isNav = e.request.mode === 'navigate';
    e.respondWith(
        caches.open(CACHE).then((cache) =>
            cache.match(e.request).then((cached) => {
                const revalidate = () =>
                    fetch(e.request).then((resp) => {
                        if (resp.ok) cache.put(e.request, resp.clone());
                        return resp;
                    });
                if (cached) {
                    revalidate().catch(() => {});
                    return cached;
                }
                return revalidate().catch(() =>
                    isNav ? cache.match(self.registration.scope) : undefined
                );
            })
        )
    );
});