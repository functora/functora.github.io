import * as H from 'hash.js';

// Uint8Array -> Uint8Array
function ripemd160(d) {
    return new Uint8Array(H.ripemd160().update(d).digest());
};

// Uint8Array -> Uint8Array
function sha256(d) {
    return new Uint8Array(H.sha256().update(d).digest());
};

// Uint8Array -> Uint8Array -> Uint8Array
function hmacSHA512(k, d) {
    return new Uint8Array(H.hmac(H.sha512, k).update(d).digest());
};

export {
    ripemd160,
    sha256,
    hmacSHA512
};
