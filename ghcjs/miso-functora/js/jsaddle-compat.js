export function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if (!unalignedOk && offset && offset % 8 !== 0) {
    throw "h$wrapBuffer: offset not aligned:" + offset;
  }
  if (!buf || !(buf instanceof ArrayBuffer))
    throw "h$wrapBuffer: not an ArrayBuffer";
  if (!offset) {
    offset = 0;
  }
  if (!length || length < 0) {
    length = buf.byteLength - offset;
  }
  return {
    buf: buf,
    len: length,
    i3: offset % 4 ? null : new Int32Array(buf, offset, length >> 2),
    u8: new Uint8Array(buf, offset, length),
    u1: offset % 2 ? null : new Uint16Array(buf, offset, length >> 1),
    f3: offset % 4 ? null : new Float32Array(buf, offset, length >> 2),
    f6: offset % 8 ? null : new Float64Array(buf, offset, length >> 3),
    dv: new DataView(buf, offset, length),
  };
}

export function h$byteArrayToBase64String(off, len, ba) {
  var bin = "";
  var u8 = ba.u8;
  var end = off + len;
  for (var i = off; i < end; i++) {
    bin += String.fromCharCode(u8[i]);
  }
  return window.btoa(bin);
}

export function h$newByteArrayFromBase64String(base64) {
  var bin = window.atob(base64);
  var ba = h$newByteArray(bin.length);
  var u8 = ba.u8;
  for (var i = 0; i < bin.length; i++) {
    u8[i] = bin.charCodeAt(i);
  }
  return ba;
}

export function h$newByteArray(len) {
  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);
  var buf = new ArrayBuffer(len0);
  return {
    buf: buf,
    len: len,
    i3: new Int32Array(buf),
    u8: new Uint8Array(buf),
    u1: new Uint16Array(buf),
    f3: new Float32Array(buf),
    f6: new Float64Array(buf),
    dv: new DataView(buf),
  };
}

export function h$roundUpToMultipleOf(n, m) {
  var rem = n % m;
  return rem === 0 ? n : n - rem + m;
}

[
  h$wrapBuffer,
  h$byteArrayToBase64String,
  h$newByteArrayFromBase64String,
  h$newByteArray,
  h$roundUpToMultipleOf,
].forEach(function (f) {
  if (!globalThis[f.name]) {
    globalThis[f.name] = f;
  }
});
