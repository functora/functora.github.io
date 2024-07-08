// fixme this function appears to deoptimize a lot due to smallint overflows
function h$imul_shim(a, b) {
  var ah = (a >>> 16) & 0xffff;
  var al = a & 0xffff;
  var bh = (b >>> 16) & 0xffff;
  var bl = b & 0xffff;
  // the shift by 0 fixes the sign on the high part
  // the final |0 converts the unsigned value into a signed value
  return (((al * bl) | 0) + (((ah * bl + al * bh) << 16) >>> 0)) | 0;
}

var h$mulInt32 = Math.imul ? Math.imul : h$imul_shim;

/* FNV-1 hash
 *
 * The FNV-1 hash description: http://isthe.com/chongo/tech/comp/fnv/
 * The FNV-1 hash is public domain: http://isthe.com/chongo/tech/comp/fnv/#public_domain
 */
function h$hashable_fnv_hash_offset(str_a, str_o_zero, o, len, hash) {
  return h$hashable_fnv_hash(str_a, o, len, hash);
}

function h$hashable_fnv_hash(str_d, str_o, len, hash) {
  if (len > 0) {
    var d = str_d.u8;
    for (var i = 0; i < len; i++) {
      hash = h$mulInt32(hash, 16777619) ^ d[str_o + i];
    }
  }
  return hash;
}

// int hashable_getRandomBytes(unsigned char *dest, int nbytes)
function h$hashable_getRandomBytes(dest_d, dest_o, len) {
  if (len > 0) {
    var d = dest_d.u8;
    for (var i = 0; i < len; i++) {
      d[dest_o + i] = Math.floor(Math.random() * 256);
    }
  }
  return len;
}
