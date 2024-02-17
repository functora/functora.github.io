var h$ghcjsbn_zero_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (0)));;
var h$ghcjsbn_one_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (1)));;
var h$ghcjsbn_negOne_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-1)));;
var h$ghcjsbn_null_b = [-1];
var h$ghcjsbn_zero_b = [0];
var h$ghcjsbn_one_b = [1, 1];
var h$ghcjsbn_two31_b = [2, 0, 8];
var h$ghcjsbn_czero_b = [2, 268435455, 15];
var h$ghcjsbn_two31_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (h$ghcjsbn_two31_b)));;
var h$ghcjsbn_negTwo31_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-2147483648)));;
var h$ghcjsbn_smallPrimes =
 [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47
 , 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113
 , 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197
 , 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281
 , 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379
 , 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463
 , 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571
 , 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659
 , 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761
 , 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863
 , 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977
 , 983, 991, 997
 ];
var h$ghcjsbn_smallPrimesM = null;
function h$ghcjsbn_getSmallPrimesM() {
  var a, i;
  if(h$ghcjsbn_smallPrimesM === null) {
    a = [];
    for(i = 0; i < 1008; i++) {
      a[i] = false;
    }
    for(i = h$ghcjsbn_smallPrimes.length - 1; i >= 0; i--) {
      a[h$ghcjsbn_smallPrimes[i]] = true;
    }
    h$ghcjsbn_smallPrimesM = a;
  }
  return h$ghcjsbn_smallPrimesM;
}
function h$ghcjsbn_isPrime_s(s, rounds) {
  if(s < 2 || (s > 2 && ((s&1) === 1))) return false;
  if(s <= 1008) {
    return h$ghcjsbn_getSmallPrimesM()[s];
  }
  throw new Error("isPrime_s");
}
function h$ghcjsbn_random_b(min, max) {
  if(h$ghcjsbn_cmp_bb(min, max) >= 0) {
    return min;
  }
  var range = h$ghcjsbn_sub_bb(max, min);
  var size = 4 + Math.ceil(h$ghcjsbn_nbits_b(range) / 16);
  var r = h$ghcjsbn_zero_b;
  for(var i=0;i<size;i++) {
    var rnd = Math.floor(Math.random()*65536);
    r = h$ghcjsbn_or_bb(h$ghcsbn_shl_b(r, 16), h$ghcjsbn_mkBigNat_w(rnd));
  }
  return h$ghcjsbn_add_bb(h$ghcjsbn_rem_bb(r, range), min);
}
function h$ghcjsbn_testPrime_b(n, rounds) {
  var r = 0, d = n;
  var nm1 = h$ghcjsbn_sub_bw(n,1);
  var two = h$ghcjsbn_mkBigNat_w(2);
  while(h$ghcjsbn_testBit_b(d, 0)) {
    d = h$ghcjsbn_shr_b(d, 1);
    r++;
  }
  for(var round = 0; round < rounds; round++) {
    var a = h$ghcjsbn_random_b(two, nm1);
    var x = h$ghcjsbn_modPow_bbb(a, d, n);
    if(h$ghcjsbn_eq_bw(x,1) || h$ghcjsbn_eq_bb(x,nm1)) continue;
    var found = false;
    for(var rr=0;rr<r;rr++) {
      x = h$ghcjsbn_modPow_bbb(x, two, n);
      if(h$ghcjsbn_eq_bb(x, nm1)) {
        found = true;
        break;
      }
    }
    if(!found) return 0;
  }
  return 1;
}
function h$ghcjsbn_testPrime_w(w, rounds) {
  return h$ghcjsbn_testPrime_b(h$ghcjsbn_mkBigNat_w(w), rounds);
}
function h$integer_gmp_next_prime1(w) {
  var r = h$ghcjsbn_nextPrime_b(h$ghcjsbn_mkBigNat_w(w));
  return h$ghcjsbn_toWord_b(r);
}
function h$ghcjsbn_nextPrime_b(bn) {
  var rounds = 64;
  if(h$ghcjsbn_eq_bw(bn, 2)) {
    return h$ghcjsbn_mkBigNat_w(3);
  }
  var i = bn;
  do {
    i = h$ghcjsbn_add_bw(i, 2);
  } while(!h$ghcjsbn_testPrime_b(i, rounds));
  return i;
}
function h$ghcjsbn_cmp_bb(b1, b2) {
  ;
  ;
  var l1 = b1[0], l2 = b2[0], d1, d2;
  if(l1 === l2) {
    while(--l1 >= 0) {
      d1 = b1[l1+1];
      d2 = b2[l1+1];
      if(d1 !== d2) return d1 < d2 ? 0 : 2;
    }
    return 1;
  } else {
    return l1 > l2 ? 2 : 0;
  }
}
var h$ghcjsbn_tmp_2a = [0, 0, 0];
var h$ghcjsbn_tmp_2b = [0, 0, 0];
var h$ghcjsbn_tmp_a = [0, 0, 0, 0, 0, 0, 0, 0];
var h$ghcjsbn_tmp_b = [0, 0, 0, 0, 0, 0, 0, 0];
function h$ghcjsbn_sub_bw(b, w) {
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  return h$ghcjsbn_sub_bb(b, a);
}
function h$ghcjsbn_sub_bs(b, s) {
  ;
  ;
  var a, ms, r;
  if(s < 0) {
    if(s === -2147483648) {
      r = h$ghcjsbn_add_bb(b, h$ghcjsbn_two31_b);
    } else {
      a = h$ghcjsn_tmp_2a;
      h$ghcjsbn_toBigNat_s(a, -s);
      r = h$ghcjsbn_add_bb(b, a);
    }
  } else {
    a = h$ghcjsn_tmp_2a;
    h$ghcjsbn_toBigNat_s(a, s);
    r = h$ghcjsbn_sub_bb(b, a);
  }
  ;
  return r;
}
function h$ghcjsbn_add_bw(b, w) {
  ;
  ;
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  return h$ghcjsbn_add_bb(b, a);
}
function h$ghcjsbn_add_bs(b, s) {
  ;
  ;
  var a, ms, r;
  if(s < 0) {
    if(s === -2147483648) {
      r = h$ghcjsbn_sub_bb(b, h$ghcjsbn_two31_r);
    } else {
      ms = -s;
      a = h$ghcjsbn_tmp_2a;
      h$ghcjsbn_toBigNat_s(a, ms);
      r = h$ghcjsbn_sub(b, a);
    }
  } else {
    a = h$ghcjsbn_tmp_2a;
    h$ghcjsbn_toBigNat_s(a, s);
    r = h$ghcjsbn_add_bb(b, a);
  }
  ;
  return r;
}
function h$ghcjsbn_add_bb(b1, b2) {
  ;
  ;
  var i, c = 0, l1 = b1[0], l2 = b2[0], t = [0];
  var bl, lmin, lmax;
  if(l1 <= l2) {
    lmin = l1;
    lmax = l2;
    bl = b2;
  } else {
    lmin = l2;
    lmax = l1;
    bl = b1;
  }
  for(i=1;i<=lmin;i++) {
    c += b1[i] + b2[i];
    t[i] = c & 0xfffffff;
    c >>= 28;
  }
  for(i=lmin+1;i<=lmax;i++) {
    c += bl[i];
    t[i] = c & 0xfffffff;
    c >>= 28;
  }
  if(c !== 0) t[++lmax] = c;
  t[0] = lmax;
  ;
  return t;
}
function h$ghcjsbn_addTo_bb(b1, b2) {
  ;
  ;
  var i, c = 0, l1 = b1[0], l2 = b2[0];
  if(l2 > l1) {
    for(i = l1 + 1; i <= l2; i++) {
      b1[i] = 0;
    }
    l1 = l2;
  }
  for(i = 1; i <= l2; i++) {
    c += b1[i] + b2[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  for(i = l2 + 1; c !== 0 && i <= l1; i++) {
    c += b1[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  if(c !== 0) {
    b1[l1] = c;
    b1[0] = l1+1;
  } else {
    b1[0] = l1;
  }
  ;
}
function h$ghcjsbn_sub_bb(b1, b2) {
  ;
  ;
  if(h$ghcjsbn_cmp_bb(b1,b2) === 0) {
    return [];
  } else {
    var i, c = 0, l1 = b1[0], l2 = b2[0], t = [0];
    for(i = 1; i <= l2; i++) {
      c += b1[i] - b2[i];
      t[i] = c & 0xfffffff;
      c >>= 28;
    }
    for(i = l2 + 1; i <= l1; i++) {
      c += b1[i];
      t[i] = c & 0xfffffff;
      c >>= 28;
    }
    while(l1 > 0 && t[l1] === 0) l1--;
    t[0] = l1;
    ;
    return t;
  }
}
function h$ghcjsbn_subTo_bb(b1, b2) {
  ;
  ;
  var i, c = 0, l1 = b1[0], l2 = b2[0];
  for(i = 1; i <= l2; i++) {
    c += b1[i] - b2[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  for(i = l2 + 1; c !== 0 && i <= l1; i++) {
    c += b1[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  while(l1 > 0 && b1[l1] === 0) l1--;
  b1[0] = l1;
  ;
}
function h$ghcjsbn_wrap_p(b) {
  var l = b[0];
  if(l === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (0)));;
  } else if(l === 1) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (b[1])));;
  } else if(l === 2 && (b[2] >> (31 - 28)) === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, ((b[2] << 28)|b[1])));;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (b)));;
  }
}
function h$ghcjsbn_wrap_n(b) {
  var l = b[0];
  if(l === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (0)));;
  } else if(l === 1) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-b[1])));;
  } else if(l === 2 &&
            ((b[2] >> (31 - GHCJSN_BITS)) === 0 ||
             (b[2] === (1 << (31 - 28)) && b[1] === 0))) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, ((-b[2]-b[1])|0)));;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (b)));;
  }
}
function h$ghcjsbn_mulTo_bb(b1, b2) {
  ;
  ;
  var t = h$ghcjsbn_mul_bb(b1, b2);
  h$ghcjsbn_copy(b1, t);
  ;
}
function h$ghcjsbn_mul_bb(b1, b2) {
  ;
  ;
  var l1 = b1[0], l2 = b2[0];
  var n = l1 + l2, i, t = [0];
  for(i = 1; i <= n; i++) t[i] = 0;
  if(l1 > l2) {
    for(i = 0; i < l2; i++) {
      t[i + l1 + 1] = h$ghcjsbn_mul_limb(0, b1, b2[i+1], t, i, 0, l1);
    }
  } else {
    for(i = 0; i < l1; i++) {
      t[i + l2 + 1] = h$ghcjsbn_mul_limb(0, b2, b1[i+1], t, i, 0, l2);
    }
  }
  for(i = l1 + l2; i > 0 && t[i] === 0; i--);
  t[0] = i;
  ;
  return t;
}
function h$ghcjsbn_mul_bw(b, w) {
  ;
  ;
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  var t = h$ghcjsbn_mul_bb(b, a);
  ;
  return t;
}
function h$ghcjsbn_mul_karatsuba_bb(t, b1, b2) {
  throw new Error("not yet updated");
  var l1 = b1.length, l2 = b2.length;
  var i, b = (l1 < l2 ? l1 : l2) >> 1;
  var x0 = [b], x1 = [l1-b], y0 = [b], y1 = [l2-b];
  for(i = 1; i <= b; i++) {
    x0[i] = b1[i];
    y0[i] = b2[i];
  }
  for(i = b + 1; i <= l1; i++) x1[i - b] = b1[i];
  for(i = b + 1; i <= l2; i++) y1[i - b] = b2[i];
  var z0 = h$ghcjsbn_mul_bb(x0, y0), z1, z2 = h$ghcjsbn_mul_bb(x1, y1);
  h$ghcjsbn_addTo_bb(x0, x1);
  h$ghcjsbn_addTo_bb(y0, x1);
  z1 = h$ghcjsbn_mul_bb(x0, y0);
  h$ghcjsbn_subTo_bb(z1, z2);
  h$ghcjsbn_subTo_bb(z1, z0);
  for(i = 0; i < 2*b; i++) t[i] = 0;
  l2 = z2.length;
  for(i = 0; i < l2; i++) t[i+2*b] = z2[i];
  var z1s = [];
  l1 = z1.length;
  for(i = 0; i < b; i++) z1s[i] = 0;
  for(i = 0; i < l1; i++) z1s[i+b] = z1[i];
  h$ghcjsbn_addTo_bb(t, z1s);
  h$ghcjsbn_addTo_bb(t, z0);
  return t;
}
function h$ghcjsbn_mul_limb(i,b,x,w,j,c,n) {
  var xl = x & 0x3fff, xh = x >> 14;
  while(--n >= 0) {
    var l = b[++i] & 0x3fff;
    var h = b[i] >> 14;
    var m = xh * l + h * xl;
    l = xl *l + ((m & 0x3fff) << 14) + w[++j] + c;
    c = (l >> 28) + (m >> 14) + xh * h;
    w[j] = l & 0xfffffff;
  }
  return c;
}
function h$ghcjsbn_quotRem_bb(q, r, b1, b2) {
  ;
  ;
  if(q === null) q = h$ghcjsbn_tmp_a;
  if(r === null) r = h$ghcjsbn_tmp_b;
  var l1 = b1[0], l2 = b2[0], nsh, y = [];
  if(l1 === 0) {
    q[0] = 0;
    r[0] = 0;
    return;
  }
  if(h$ghcjsbn_cmp_bb(b1,b2) === 0) {
    q[0] = 0;
    h$ghcjsbn_copy(r, b1);
    return;
  }
  nsh = 28 -h$ghcjsbn_nbits_s(b2[l2]);
  ;
  if(nsh !== 0) {
    h$ghcjsbn_shlTo_b(y, b2, nsh);
    h$ghcjsbn_shlTo_b(r, b1, nsh);
  } else {
    h$ghcjsbn_copy(y, b2);
    h$ghcjsbn_copy(r, b1);
  }
  ;
  ;
  var ys = y[0], y0 = y[ys];
  var yt = y0*(1<<24)+((ys>1)?y[ys-1]>>4:0);
  var d1 = 4503599627370496/yt, d2 = (1<<24)/yt, e = 1 << 4;
  var i = r[0], j = i-ys, t = q;
  h$ghcjsbn_shlTo_limbs_b(t,y,j);
  if(h$ghcjsbn_cmp_bb(r, t) !== 0) {
    r[r[0]+1] = 1;
    r[0] += 1;
    h$ghcjsbn_subTo_bb(r, t);
  }
  h$ghcjsbn_shlTo_limbs_b(t, h$ghcjsbn_one_b, ys);
  y = h$ghcjsbn_sub_bb(t, y);
  while(y.length <= ys) y[y.length] = 0;
  while(--j >= 0) {
    var qd = (r[(--i)+1]===y0)?0xfffffff:Math.floor(r[i+1]*d1+(r[i]+e)*d2);
    var am = h$ghcjsbn_mul_limb(0, y, qd, r, j, 0, ys);
    if((r[i+1] += am) < qd) {
      h$ghcjsbn_shlTo_limbs_b(t, y, j);
      h$ghcjsbn_subTo_bb(r, t);
      while(r[i+1] < --qd) {
        h$ghcjsbn_subTo_bb(r, t);
      }
    }
  }
  ;
  h$ghcjsbn_shrTo_limbs_b(q, r, ys);
  r[0] = ys;
  while(r[r[0]] === 0 && r[0] > 0 && r[0]--);
  if(nsh !== 0) {
    var r0 = [];
    h$ghcjsbn_copy(r0, r);
    h$ghcjsbn_shrTo_b(r, r0, nsh);
  }
  ;
  ;
}
function h$ghcjsbn_quotRem_bw(q, b, w) {
  ;
  ;
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  var r = [];
  h$ghcjsbn_quotRem_bb(q, r, b, a);
  return h$ghcjsbn_toWord_b(r);
}
function h$ghcjsbn_tmp_toJSBN(b) {
  var j = new BigInteger(), bl = b[0], i;
  for(i = 0; i < bl; i++) j.data[i] = b[i+1];
  j.s = 0;
  j.t = bl;
  return j;
}
function h$ghcjsbn_tmp_fromJSBN(b, j) {
  var bl = j.t, i;
  for(i = 0; i < bl; i++) {
    b[i] = j.data[i];
  }
  return bl;
}
function h$ghcjsbn_rem_bb(b1, b2) {
  ;
  ;
  var t1 = [], t2 = [];
  h$ghcjsbn_quotRem_bb(t1, t2, b1, b2);
  ;
  return t2;
}
function h$ghcjsbn_rem_bw(b, w) {
  ;
  ;
  var r = h$ghcjsbn_quotRem_bw([] , b, w);
  ;
  return r;
}
function h$ghcjsbn_quot_bb(b1, b2) {
  ;
  ;
  var t1 = [], t2 = [];
  h$ghcjsbn_quotRem_bb(t1, t2, b1, b2);
  ;
  return t1;
}
function h$ghcjsbn_sqr_b(b) {
  ;
  var l = b[0], n = 2 * l, i, c, t = [0];
  for(i = 1; i <= n; i++) t[i] = 0;
  for(i = 0; i < l - 1; i++) {
    c = h$ghcjsbn_mul_limb(i, b, b[i+1],t,2*i,0,1);
    if((t[i + l + 1] += h$ghcjsbn_mul_limb(i+1, b, 2*b[i+1], t, 2*i+1, c, l - i - 1)) >= 0x10000000) {
      t[i + l + 1] -= 0x10000000;
      t[i + l + 2] = 1;
    }
  }
  if(n > 0) t[n] += h$ghcjsbn_mul_limb(i, b, b[i+1], t, 2*i, 0, 1);
  if(t[n] === 0) n--;
  t[0] = n;
  ;
  return t;
}
function h$ghcjsbn_pow_bb(b1, b2) {
  ;
  ;
  var i, sq = b1, t = [1,1];
  var bits = h$ghcjsbn_nbits_b(b2);
  for(i = 0; i < bits; i++) {
    if(h$ghcjsbn_testBit_b(b2, i)) {
      h$ghcjsbn_mulTo_bb(t, sq);
    }
    sq = h$ghcjsbn_sqr_b(sq);
  }
  return t;
}
function h$ghcjsbn_pow_bw(b, w) {
  ;
  ;
  var i, sq = b, t = [1,1];
  while(w) {
    if(w&1) h$ghcjsbn_mulTo_bb(t, sq);
    w >>>= 1;
    if(w) {
      sq = h$ghcjsbn_sqr_b(sq);
    }
  }
  ;
  return t;
}
function h$ghcjsbn_pow_ww(w1, w2) {
  ;
  ;
  var b = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(b, w1);
  var t = h$ghcjsbn_pow_bw(b, w2);
  ;
  return t;
}
function h$ghcjsbn_modPow_bbb(b, e, m) {
  var r = h$ghcjsbn_one_b, b = h$ghcjsbn_rem_bb(b, m);
  while(!h$ghcjsbn_eq_bw(e, 0)) {
    if(h$ghcjsbn_testBit_b(e, 0)) {
      r = h$ghcjsbn_rem_bb(h$ghcjsbn_mul_bb(r, b), m);
    }
    e = h$ghcjsbn_shr_b(e, 1);
    b = h$ghcjsbn_rem_bb(h$ghcjsbn_mul_bb(b, b), m);
  }
  return r;
}
function h$ghcjsbn_modPow_bss(b, e, m) {
  return h$ghcjsbn_modPow_sss(h$ghcjsbn_rem_bw(b, m), e, m);
}
function h$ghcjsbn_modPow_sss(b, e, m) {
  return h$integer_gmp_powm_word(b, e, m);
}
function h$ghcjsbn_modular_inverse(a, n) {
    var t = h$ghcjsbn_zero_b, newt = h$ghcjsbn_one_b;
    var r = n, newr = a;
    while(!h$ghcjsbn_cmp_bw(newr, 0)) {
      var quotient = h$ghcjsbn_div_bb(r, newr);
      var tmp = newt;
      newt = h$ghcjsbn_sub_bb(t, h$ghcjsbn_mul_bb(quotient, newt));
      t = tmp;
      tmp = newr;
      newr = h$ghcjsbn_sub_bb(r, h$ghcjsbn_mul_bb(quotient, newr));
      r = tmp;
    }
    if(h$ghcjsbn_cmp_bw(r, 1) > 0) return a;
    return t;
}
function h$ghcjsbn_powModSBigNat(bpos, base, epos, exp, m) {
  var newBase = bpos ? base : h$ghcjsbn_sub_bb(m, base);
  var newExp = epos ? exp : h$ghcjsbn_modular_inverse(exp, m);
  return h$ghcjsbn_modPow_bbb(newBase, newExp, m);
}
function h$integer_gmp_powm_word(b, e, m) {
  var r = 1, b = b % m;
  while(e !== 0) {
    if(e % 2 === 1) r = (r * b) % m;
    e = e >>> 1;
    b = (b * b) % m;
  }
  return r;
}
function h$ghcjsbn_gcd_bb(b1, b2) {
  ;
  ;
  var r;
  if(h$ghcjsbn_cmp_bb(b1, b2) === 2) {
    r = b1;
    b1 = b2;
    b2 = r;
  }
  while(b1[0] > 0) {
    r = h$ghcjsbn_rem_bb(b2, b1);
    b2 = b1;
    b1 = r;
  }
  ;
  return b2;
}
function h$ghcjsbn_gcd_bs(b, s) {
  throw new Error("h$ghcjsbn_gcd_bs not implemented");
}
function h$ghcjsbn_gcd_ss(s1, s2) {
  ;
  ;
  var a, b, r;
  a = s1 < 0 ? -s1 : s1;
  b = s2 < 0 ? -s2 : s2;
  if(b < a) {
    r = a;
    a = b;
    b = r;
  }
  while(a !== 0) {
    r = b % a;
    b = a;
    a = r;
  }
  ;
  return b;
}
function h$ghcjsbn_gcd_ww(w1, w2) {
  ;
  ;
  var a, b, r;
  a = w1 < 0 ? (w1 + 4294967296) : w1;
  b = w2 < 0 ? (w2 + 4294967296) : w2;
  if(b < a) {
    r = a;
    a = b;
    b = r;
  }
  while(a !== 0) {
    r = b % a;
    b = a;
    a = r;
  }
  b = b|0;
  ;
  return b;
}
function h$ghcjsbn_gcd_bw(b, w) {
  ;
  ;
  var q = [], r = h$ghcjsbn_quotRem_bw(q, b, w);
  ;
  if(r === 0) {
    return b[0] === 0 ? 0 : w;
  } else {
    return h$ghcjsbn_gcd_ww(r, w);
  }
}
function h$ghcjsbn_shr_b(b, s) {
  ;
  ;
  var i, v1, v2, l = b[0], sl = (s / 28)|0, t = [0];
  l -= sl;
  if(l <= 0) {
    t[0] = 0;
  } else {
    var sb1 = s % 28, sb2 = 28 - sb1, m = (1<<sb1)-1;
    var c = b[sl + 1] >> sb1, v;
    for(i = 1; i < l; i++) {
      v = b[i + sl + 1];
      t[i] = ((v&m) << sb2)|c;
      c = v >> sb1;
    }
    if(c !== 0) {
      t[l] = c;
      t[0] = l;
    } else {
      t[0] = l - 1;
    }
  }
  ;
  return t;
}
function h$ghcjsbn_shrTo_b(t, b, s) {
  ;
  ;
  var i, v1, v2, l = b[0], sl = (s / 28)|0;
  t[0] = 0;
  l -= sl;
  if(l <= 0) {
    t[0] = 0;
  } else {
    var sb1 = s % 28, sb2 = 28 - sb1, m = (1<<sb1)-1;
    var c = b[sl + 1] >> sb1, v;
    for(i = 1; i < l; i++) {
      v = b[i + sl + 1];
      t[i] = ((v&m) << sb2)|c;
      c = v >> sb1;
    }
    if(c !== 0) {
      t[l] = c;
      t[0] = l;
    } else {
      t[0] = l - 1;
    }
  }
  ;
}
function h$ghcjsbn_shr_neg_b(b, s) {
  if(s === 0) return b;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_zero_b) === 1) return b;
  var t = h$ghcjsbn_sub_bw(b, 1);
  t = h$ghcjsbn_shr_b(t, s);
  return h$ghcjsbn_add_bw(t, 1);
}
function h$ghcjsbn_shl_b(b, s) {
  ;
  ;
  var sl = (s / 28)|0;
  var sb1 = s % 28, sb2 = 28 - sb1;
  var l = b[0];
  if(l === 0) return h$ghcjsbn_zero_b;
  var c = 0, i, v, m = (1 <<sb1) - 1, t = [0];
  for(i = 1; i <= sl; i++) {
    t[i] = 0;
  }
  for(i = 1; i <= l; i++) {
    v = b[i];
    t[i + sl] = ((v << sb1) & 0xfffffff) | c;
    c = v >> sb2;
  }
  if(c !== 0) {
    t[l+sl+1] = c;
    t[0] = l + sl + 1;
  } else {
    t[0] = l + sl;
  }
  ;
  return t;
}
function h$ghcjsbn_shlTo_b(t, b, s) {
  ;
  ;
  var sl = (s / 28)|0;
  var sb1 = s % 28, sb2 = 28 - sb1;
  var l = b[0], c = 0, i, v, m = (1 <<sb1) - 1;
  t[0] = 0;
  for(i = 1; i <= sl; i++) {
    t[i] = 0;
  }
  for(i = 1; i <= l; i++) {
    v = b[i];
    t[i + sl] = ((v << sb1) & 0xfffffff) | c;
    c = v >> sb2;
  }
  if(c !== 0) {
    t[l+sl+1] = c;
    t[0] = l + sl + 1;
  } else {
    t[0] = l + sl;
  }
  ;
}
function h$ghcjsbn_shrTo_limbs_b(t, b, s) {
  ;
  ;
  var l = b[0], l1 = l - s, i;
  if(l1 < 1) {
    t[0] = 0;
  } else {
    t[0] = l1;
    for(i = 1; i <= l1; i++) t[i] = b[i+s];
  }
  ;
}
function h$ghcjsbn_shlTo_limbs_b(t, b, s) {
  ;
  ;
  var l = b[0], l1 = l + s, i;
  if(l === 0) {
    t[0] = 0;
  } else {
    t[0] = l1;
    for(i = 1; i <= s; i++) t[i] = 0;
    for(i = s+1; i <= l1; i++) t[i] = b[i-s];
  }
  ;
}
function h$ghcjsbn_nbits_b(b) {
  ;
  var l = b[0], c = 0, s, t;
  if(l === 0) {
    return 0;
  } else {
    var r = ((l-1)*28) + h$ghcjsbn_nbits_s(b[l]);
    ;
    return r;
  }
}
function h$ghcjsbn_nbits_s(s) {
  ;
  var c = 1, t;
  if((t = s >>> 16) != 0) { s = t; c += 16; }
  if((t = s >> 8) != 0) { s = t; c += 8; }
  if((t = s >> 4) != 0) { s = t; c += 4; }
  if((t = s >> 2) != 0) { s = t; c += 2; }
  if((t = s >> 1) != 0) { s = t; c += 1; }
  ;
  return c;
}
function h$ghcjsbn_showBase(b, base) {
  ;
  ;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_zero_b) === 1) {
    return "0";
  } else {
    return h$ghcjsbn_showBase_rec(b, base, Math.log(base), 0);
  }
}
function h$ghcjsbn_showBase_rec(b, base, logBase, pad) {
  var bits = h$ghcjsbn_nbits_b(b), r;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b) === 0) {
    var ti = h$ghcjsbn_toInt_b(b);
    r = ti === 0 ? "" : ti.toString(base);
  } else {
    var digits = Math.floor(bits * 0.6931471805599453 / logBase);
    var d2 = Math.round(digits/2), p, q = [], r = [];
    p = h$ghcjsbn_pow_ww(base, d2);
    h$ghcjsbn_quotRem_bb(q, r, b, p);
    r = h$ghcjsbn_showBase_rec(q, base, logBase, 0) +
        h$ghcjsbn_showBase_rec(r, base, logBase, d2);
  }
  var rl = r.length;
  if(rl < pad) {
    while(rl <= pad-8) { r = "00000000" + r; rl += 8; }
    switch(pad-rl) {
    case 1: r = "0" + r; break;
    case 2: r = "00" + r; break;
    case 3: r = "000" + r; break;
    case 4: r = "0000" + r; break;
    case 5: r = "00000" + r; break;
    case 6: r = "000000" + r; break;
    case 7: r = "0000000" + r; break;
    }
  }
  return r;
}
function h$ghcjsbn_show(b) {
  throw new Error("show not implemented");
}
function h$ghcjsbn_showHex(b) {
  throw new Error("showHex not implemented");
}
function h$ghcjsbn_copy(t, b) {
  ;
  var l = b[0];
  for(var i = 0; i <= l; i++) {
    t[i] = b[i];
  }
  return l;
}
function h$ghcjsbn_testBit_b(b, n) {
  ;
  ;
  var limb = (n / 28)|0;
  if(limb >= b[0]) {
    return false;
  } else {
    var bit = n - (28 * limb);
    return (b[limb+1] & (1 << bit)) !== 0;
  }
}
function h$ghcjsbn_popCount_b(b) {
  ;
  var c = 0, l = b[0];
  while(l > 0) {
    c += h$popCnt32(b[l--]);
  }
  return c;
}
function h$ghcjsbn_xor_bb(b1, b2) {
  ;
  ;
  var i, lmin, lmax, blmax, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    lmin = l1;
    lmax = l2;
    blmax = b2;
  } else {
    lmin = l2;
    lmax = l1;
    blmax = b1;
  }
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] ^ b2[i];
  }
  for(i = lmin + 1; i <= lmax; i++) {
    t[i] = blmax[i];
  }
  while(lmax > 0 && t[lmax] === 0) lmax--;
  t[0] = lmax;
  ;
  return t;
}
function h$ghcjsbn_or_bb(b1, b2) {
  ;
  ;
  var i, lmin, lmax, blmax, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    lmin = l1;
    lmax = l2;
    blmax = b2;
  } else {
    lmin = l2;
    lmax = l1;
    blmax = b1;
  }
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] | b2[i];
  }
  for(i = lmin + 1; i <= lmax; i++) {
    t[i] = blmax[i];
  }
  t[0] = lmax;
  ;
  return t;
}
function h$ghcjsbn_and_bb(b1, b2) {
  ;
  ;
  var i, lmin, l1 = b1[0], l2 = b2[0], t = [0];
  lmin = l1 <= l2 ? l1 : l2;
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] & b2[i];
  }
  while(lmin > 0 && t[lmin] === 0) lmin--;
  t[0] = lmin;
  ;
  return t;
}
function h$ghcjsbn_andn_bb(b1, b2) {
  ;
  ;
  var i, lmin, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    for(i = 0; i <= l1; i++) t[i] = b1[i] & (~b2[i]);
  } else {
    for(i = 0; i <= l2; i++) t[i] = b1[i] & (~b2[i]);
    for(i = l2+1; i <= l1; i++) t[i] = b1[i];
  }
  while(l1 > 0 && t[l1] === 0) l1--;
  t[0] = l1;
  ;
  return t;
}
function h$ghcjsbn_toInt_b(b) {
  ;
  var bl = b[0], r;
  if(bl >= 2) {
    r = (b[2] << 28) | b[1];
  } else if(bl === 1) {
    r = b[1];
  } else {
    r = 0;
  }
  ;
  return r;
}
function h$ghcjsbn_toWord_b(b) {
  ;
  var bl = b[0], w;
  if(bl >= 2) {
    w = (b[2] << 28) | b[1];
  } else if(bl === 1) {
    w = b[1];
  } else {
    w = 0;
  }
  ;
  return w;
}
var h$integer_bigNatToWord64 = h$ghcjsbn_toWord64_b;
var h$integer_word64ToBigNat = h$ghcjsbn_mkBigNat_ww;
function h$ghcjsbn_toWord64_b(b) {
  ;
  var len = b[0], w1, w2;
  if(len < 2) {
    w2 = 0;
    w1 = (len === 1) ? b[1] : 0;
  } else {
    w1 = b[1] | (b[2] << 28);
    if(len === 2) {
      w2 = b[2] >>> 4;
    } else {
      w2 = (b[2] >>> 4) | (b[3] << 24);
    }
  }
  ;
  ;
  { h$ret1 = (w1); return (w2); };
}
function h$ghcjsbn_toBigNat_s(b, s) {
  ;
  if(s === 0) {
    b[0] = 0;
  } else if(s <= 0xfffffff) {
    b[0] = 1;
    b[1] = s;
  } else {
    b[0] = 2;
    b[1] = s & 0xfffffff;
    b[2] = s >> 0xfffffff;
  }
  ;
}
function h$ghcjsbn_toBigNat_w(b, w) {
  ;
  if(w === 0) {
    b[0] = 0;
  } else if(w > 0 && w <= 0xfffffff) {
    b[0] = 1;
    b[1] = w;
  } else {
    b[0] = 2;
    b[1] = w & 0xfffffff;
    b[2] = w >>> 28;
  }
  ;
}
function h$ghcjsbn_mkBigNat_w(w) {
  ;
  var r;
  if(w === 0) r = h$ghcjsbn_zero_b;
  else if(w === 1) r = h$ghcjsbn_one_b;
  else if(w > 0 && w <= 0xfffffff) r = [1,w];
  else r = [2, w & 0xfffffff, w >>> 28];
  ;
  return r;
}
function h$ghcjsbn_mkBigNat_ww(hw, lw) {
  ;
  ;
  var r;
  if(hw === 0) r = h$ghcjsbn_mkBigNat_w(lw);
  else {
    var w1 = lw & 0xfffffff;
    var w2 = (lw >>> 28) | ((hw << 4) & 0xfffffff);
    var w3 = hw >>> 24;
    if(w3 === 0) {
      r = [2, w1, w2];
    } else {
      r = [3, w1, w2, w3];
    }
  }
  ;
  return r;
}
var h$ghcjsbn_toBigNat_ww = h$ghcjsbn_mkBigNat_ww;
var h$integer_mkInteger = h$ghcjsbn_mkInteger;
function h$ghcjsbn_mkInteger(nonNeg, xs) {
  var r = [0], s = 0, t;
  while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
    t = h$ghcjsbn_shl_b(h$ghcjsbn_mkBigNat_w(((typeof(((xs).d1)) === 'number')?(((xs).d1)):(((xs).d1)).d1)), s);
    h$ghcjsbn_addTo_bb(r, t);
    s += 31;
    xs = ((xs).d2);
  }
  if(nonNeg) {
    if(h$ghcjsbn_cmp_bb(r, h$ghcjsbn_two31_b) === 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (h$ghcjsbn_toInt_b(r))));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (r)));;
    }
  } else {
    var c = h$ghcjsbn_cmp_bb(r, h$ghcjsbn_two31_b);
    if(c === 2) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (r)));;
    } else if(c === 1) {
      return h$ghcjsbn_negTwo31_i;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-h$ghcjsbn_toInt_b(r))));;
    }
  }
}
function h$ghcjsbn_cmp_bw(b, w) {
  ;
  ;
  var w1 = w & 0xfffffff, w2 = w >>> 28, bl = b[0];
  if(w2 === 0) {
    if(bl === 0) {
      return w1 > 0 ? 0 : 1;
    } else if(bl === 1) {
      var bw = b[1];
      return bw > w1 ? 2 : (bw === w1 ? 1 : 0);
    } else {
      return 2;
    }
  } else {
    if(bl < 2) {
      return 0;
    } else if(bl > 2) {
      return 2;
    } else {
      var bw1 = b[1], bw2 = b[2];
      return (bw2 > w2) ? 2
                        : (bw2 < w2 ? 0
                                    : (bw1 > w1 ? 2
                                                : (bw1 < w1 ? 0
                                                            : 1)));
    }
  }
}
function h$ghcjsbn_gt_bw(b, w) {
  ;
  ;
  var bl = b[0];
  if(bl > 2) return true;
  else if(bl === 0) return false;
  else if(bl === 1) return w >= 0 && b[1] > w;
  else {
    var wh = w >>> 28, wl = w & 0xfffffff, b2 = b[2];
    return (b2 > wh || ((wh === b2) && b[1] > wl));
  }
}
function h$ghcjsbn_eq_bb(b1, b2) {
  ;
  ;
  var bl1 = b1[0], bl2 = b2[0];
  if(bl1 !== bl2) {
    return false;
  } else {
    for(var i = bl1; i >= 1; i--) {
      var bw1 = b1[i], bw2 = b2[i];
      if(bw1 !== bw2) return false;
    }
  }
  return true;
}
function h$ghcjsbn_neq_bb(b1, b2) {
  ;
  ;
  var bl1 = b1[0], bl2 = b2[0];
  if(bl1 !== bl2) {
    return true;
  } else {
    for(var i = bl1; i >= 1; i--) {
      var bw1 = b1[i], bw2 = b2[i];
      if(bw1 !== bw2) return true;
    }
  }
  return false;
}
function h$ghcjsbn_eq_bw(b, w) {
  ;
  ;
  var w1 = w & 0xfffffff, w2 = w >>> 28, bl = b[0];
  if(w2 === 0) {
    if(w1 === 0) {
      return bl === 0;
    } else {
      return bl === 1 && b[1] === w;
    }
  } else {
    return bl === 2 && b[1] === w1 && b[2] === w2;
  }
}
function h$ghcjsbn_isZero_b(b) {
  ;
  return b[0] === 0;
}
function h$ghcjsbn_isNull_b(b) {
  return b[0] === -1;
}
function h$ghcjsbn_bitBigNat(n) {
  if(n === 0) {
    r = h$ghcjsbn_one_b;
  } else if(n < 28) {
    r = [1, 1 << n];
  } else {
    var l = (n / 28)|0;
    var r = [l+1];
    for(var i = 1; i<= l; i++) r[i] = 0;
    r[l+1] = 1 << (n - (28 * l));
  }
  ;
  return r;
}
function h$ghcjsbn_integerLog2(i) {
  ;
  if(((i).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    return h$ghcjsbn_nbits_s(((i).d1))-1;
  } else {
    return h$ghcjsbn_nbits_b(((i).d1))-1;
  }
}
function h$ghcjsbn_integerLog2IsPowerOf2(i) {
  ;
  var nb;
  if(((i).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    var sd = ((i).d1);
    ;
    nb = h$ghcjsbn_nbits_s(sd);
    return ((sd === 1 << nb) ? -nb : nb);
  } else {
    var bd = ((i).d1);
    ;
    nb = h$ghcjsbn_nbits_b(bd);
    var i, bl = (nb / 28) | 0, lb = nb - 28 * bl, l = bd[bl+1];
    if(l !== (1 << lb)) return nb;
    for(i = bl; i >= 1; i--) {
      if(bd[i] !== 0) return nb;
    }
    return -nb;
  }
}
function h$ghcjsbn_isValid_b(b) {
  if(!Array.isArray(b)) return 0;
  if(b.length < 1) return 0;
  var bl = b[0], w;
  if(b.length < (bl+1)) return 0;
  for(var i = 0; i <= bl; i++) {
    w = b[i];
    if(typeof w !== 'number' || (w & 0xfffffff) !== w) return 0;
  }
  return 1;
}
function h$ghcjsbn_toInteger_b(b) {
  ;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b) === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (h$ghcjsbn_toInt_b(b))));;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (b)));;
  }
}
function h$ghcjsbn_toNegInteger_b(b) {
  ;
  var c = h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b);
  if(c === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-h$ghcjsbn_toInt_b(b))));;
  } else if(c === 1) {
    return h$ghcjsbn_negTwo31_i;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (b)));;
  }
}
function h$ghcjsbn_sizeof_b(b) {
  if(b.length < 1) return 0;
  var bl = b[0];
  return Math.ceil((bl * 28) / 32);
}
function h$ghcjsbn_index_b(b, w) {
  var limb = (32 * w / 28)|0;
  var l = b[0];
  var l1 = limb < l ? b[limb+1] : 0;
  var l2 = (limb+1) < l ? b[limb+2] : 0;
  var ws = 4 * (w % 7);
  return (l1 >>> ws) | (l2 << 28 - ws);
}
function h$ghcjsbn_toDouble_b(nonNeg, b) {
  throw new Error("toDouble_b");
}
function h$ghcjsbn_byteArrayToBigNat(ba, len) {
  return h$ghcjsbn_importBigNatFromByteArray(ba, 0, len, 0);
}
function h$ghcjsbn_importBigNatFromAddr(a_d, a_o, len, msbf) {
  var r = h$ghcjsbn_zero_b;
  for(var i=0;i<len;i++) {
    var off = msbf ? i : (len-i-1);
    var val = a_d.u8[off];
    var r = h$ghcjsbn_or_bb(h$ghcjsbn_shl_b(r, 8), h$ghcjsbn_mkBigNat_w(val));
  }
  return r;
}
function h$ghcjsbn_importBigNatFromByteArray(ba, ofs, len, msbf) {
  return h$ghcjsbn_importBigNatFromAddr(ba, ofs, len, msbf);
}
function h$ghcjsbn_exportToAddr_b(bn, a_d, a_o, msbf) {
  var bytes = h$ghcjsbn_sizeInBase_b(bn, 256);
  for(var i=0;i<bytes;i++) {
    var b = h$ghcjsbn_toWord_b(bn) & 255;
    var off = msbf ? bytes-i-1 : i;
    bn = h$ghcjsbn_shr_b(bn, 8);
    a_d.u8[a_o+off] = b;
  }
  return bytes;
}
function h$ghcjsbn_exportToAddr_w(w, a_d, a_o, msbf) {
  return h$ghcjsbn_exportToAddr_b(h$ghcjsbn_mkBigNat_w(w), a_d, a_o, msbf);
}
var h$integer_int64ToInteger = h$ghcjsbn_toInteger_s64;
function h$ghcjsbn_toInteger_s64(s_a, s_b) {
  ;
  ;
  if(s_a === 0) {
    if(s_b >= 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (s_b)));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (h$ghcjsbn_mkBigNat_w(s_b))));;
    }
  } else if(s_a === -1) {
    if(s_b < 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (s_b)));;
    } else if(s_b === 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_ww(1,0))));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_w(((~s_b)+1)|0))));;
    }
  } else if(s_a > 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (h$ghcjsbn_mkBigNat_ww(s_a, s_b))));;
  } else {
    if(s_b === 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_ww(((~s_a)+1)|0, 0))));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_ww((~s_a)|0, ((~s_b)+1)|0))));;
    }
  }
}
function h$decodeDoubleInt64(d) {
  ;
  if(isNaN(d)) {
    { h$ret1 = (-1572864); h$ret2 = (0); return (972); };
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) {
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  { h$ret1 = (ret1); h$ret2 = (ret2); return (ret3); };
}
function h$primop_DoubleDecode_Int64Op(d) {
  ;
  if(isNaN(d)) {
    { h$ret1 = (-1572864); h$ret2 = (0); h$ret3 = (972); return (null); };
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) {
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  { h$ret1 = (ret1); h$ret2 = (ret2); h$ret3 = (ret3); return (null); };
}
function h$ghcjsbn_encodeDouble_b(pos, b, e) {
  ;
  ;
  if(e >= 972) {
    return pos ? Infinity : -Infinity;
  }
  var ls = 1, bl = b[0], i, r = b[bl], mul = 1 << 28, rmul = 1/mul, s = 1;
  for(i = bl-1; i >= 1; i--) {
      r = r * mul + s * b[i];
  }
  if(e > 600) {
    r = r * Math.pow(2, e-600) * Math.pow(2,600);
  } else if(e < -600) {
    r = r * Math.pow(2, e+600) * Math.pow(2,-600);
  } else {
    r = r * Math.pow(2, e);
  }
  ;
  return pos ? r : -r;
}
function h$ghcjsbn_toDouble_b(nonNeg, b) {
  return h$ghcjsbn_encodeDouble_b(nonNeg, b, 0);
}
var h$ghcjsbn_encodeDouble_i = h$ghcjsbn_encodeDouble_s;
function h$ghcjsbn_encodeDouble_s(m, e) {
  ;
  ;
  var r = m * Math.pow(2, e);
  ;
  return r;
}
function h$ghcjsbn_sizeInBase_b(bn, base) {
  if(h$ghcjsbn_eq_bb(bn, h$ghcjsbn_zero_b)) return 1;
  var bits = h$ghcjsbn_nbits_b(bn);
  var r;
  if(h$popCnt32(base) === 1) {
    var factor = Math.round(Math.log(base)/Math.log(2));
    r = Math.ceil(bits/factor);
  } else {
    r = Math.ceil(bits*Math.log(2)/Math.log(base));
  }
  return r;
}
function h$integer_gmp_mpn_sizeinbase1(w, base) {
  return h$ghcjsbn_sizeInBase_b(h$ghcjsbn_mkBigNat_w(w), base);
}
!function(n){var i={};function r(t){if(i[t])return i[t].exports;var e=i[t]={i:t,l:!1,exports:{}};return n[t].call(e.exports,e,e.exports,r),e.l=!0,e.exports}r.m=n,r.c=i,r.d=function(t,e,n){r.o(t,e)||Object.defineProperty(t,e,{enumerable:!0,get:n})},r.r=function(t){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},r.t=function(e,t){if(1&t&&(e=r(e)),8&t)return e;if(4&t&&"object"==typeof e&&e&&e.__esModule)return e;var n=Object.create(null);if(r.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var i in e)r.d(n,i,function(t){return e[t]}.bind(null,i));return n},r.n=function(t){var e=t&&t.__esModule?function(){return t.default}:function(){return t};return r.d(e,"a",e),e},r.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},r.p="",r(r.s=109)}([function(t,e,n){"use strict";var i=n(47),r={};r[n(10)("toStringTag")]="z",r+""!="[object z]"&&n(18)(Object.prototype,"toString",function(){return"[object "+i(this)+"]"},!0)},function(t,e,n){for(var i=n(2),r=n(28),o=n(18),a=n(11),s=n(20),c=n(31),u=n(10),l=u("iterator"),f=u("toStringTag"),d=c.Array,p={CSSRuleList:!0,CSSStyleDeclaration:!1,CSSValueList:!1,ClientRectList:!1,DOMRectList:!1,DOMStringList:!1,DOMTokenList:!0,DataTransferItemList:!1,FileList:!1,HTMLAllCollection:!1,HTMLCollection:!1,HTMLFormElement:!1,HTMLSelectElement:!1,MediaList:!0,MimeTypeArray:!1,NamedNodeMap:!1,NodeList:!0,PaintRequestList:!1,Plugin:!1,PluginArray:!1,SVGLengthList:!1,SVGNumberList:!1,SVGPathSegList:!1,SVGPointList:!1,SVGStringList:!1,SVGTransformList:!1,SourceBufferList:!1,StyleSheetList:!0,TextTrackCueList:!1,TextTrackList:!1,TouchList:!1},h=r(p),_=0;_<h.length;_++){var y,m=h[_],v=p[m],b=a[m],E=b&&b.prototype;if(E&&(E[l]||s(E,l,d),E[f]||s(E,f,m),c[m]=d,v))for(y in i)E[y]||o(E,y,i[y],!0)}},function(t,e,n){"use strict";var i=n(58),r=n(59),o=n(31),a=n(22);t.exports=n(46)(Array,"Array",function(t,e){this._t=a(t),this._i=0,this._k=e},function(){var t=this._t,e=this._k,n=this._i++;return!t||n>=t.length?(this._t=void 0,r(1)):r(0,"keys"==e?n:"values"==e?t[n]:[n,t[n]])},"values"),o.Arguments=o.Array,i("keys"),i("values"),i("entries")},function(t,e,n){"use strict";n(91);function i(t){n(18)(RegExp.prototype,s,t,!0)}var r=n(13),o=n(49),a=n(15),s="toString",c=/./[s];n(16)(function(){return"/a/b"!=c.call({source:"a",flags:"b"})})?i(function(){var t=r(this);return"/".concat(t.source,"/","flags"in t?t.flags:!a&&t instanceof RegExp?o.call(t):void 0)}):c.name!=s&&i(function(){return c.call(this)})},function(t,e,n){"use strict";var i=n(60)(!0);n(46)(String,"String",function(t){this._t=String(t),this._i=0},function(){var t,e=this._t,n=this._i;return n>=e.length?{value:void 0,done:!0}:(t=i(e,n),this._i+=t.length,{value:t,done:!1})})},function(t,e,n){n(51)("asyncIterator")},function(t,e,n){"use strict";function i(t){var e=z[t]=w(j[U]);return e._k=t,e}function r(t,e){T(t);for(var n,i=C(e=O(e)),r=0,o=i.length;r<o;)et(t,n=i[r++],e[n]);return t}function o(t){var e=W.call(this,t=R(t,!0));return!(this===Y&&l(z,t)&&!l(X,t))&&(!(e||!l(this,t)||!l(z,t)||l(this,K)&&this[K][t])||e)}function a(t,e){if(t=O(t),e=R(e,!0),t!==Y||!l(z,e)||l(X,e)){var n=F(t,e);return!n||!l(z,e)||l(t,K)&&t[K][e]||(n.enumerable=!0),n}}function s(t){for(var e,n=H(O(t)),i=[],r=0;n.length>r;)l(z,e=n[r++])||e==K||e==h||i.push(e);return i}function c(t){for(var e,n=t===Y,i=H(n?X:O(t)),r=[],o=0;i.length>o;)!l(z,e=i[o++])||n&&!l(Y,e)||r.push(z[e]);return r}var u=n(11),l=n(19),f=n(15),d=n(17),p=n(18),h=n(39).KEY,_=n(16),y=n(33),m=n(36),v=n(25),b=n(10),E=n(52),g=n(51),C=n(79),A=n(82),T=n(13),I=n(14),S=n(38),O=n(22),R=n(34),L=n(26),w=n(30),x=n(85),N=n(45),D=n(57),P=n(12),k=n(28),F=N.f,M=P.f,H=x.f,j=u.Symbol,B=u.JSON,V=B&&B.stringify,U="prototype",K=b("_hidden"),G=b("toPrimitive"),W={}.propertyIsEnumerable,q=y("symbol-registry"),z=y("symbols"),X=y("op-symbols"),Y=Object[U],Q="function"==typeof j&&!!D.f,Z=u.QObject,$=!Z||!Z[U]||!Z[U].findChild,J=f&&_(function(){return 7!=w(M({},"a",{get:function(){return M(this,"a",{value:7}).a}})).a})?function(t,e,n){var i=F(Y,e);i&&delete Y[e],M(t,e,n),i&&t!==Y&&M(Y,e,i)}:M,tt=Q&&"symbol"==typeof j.iterator?function(t){return"symbol"==typeof t}:function(t){return t instanceof j},et=function(t,e,n){return t===Y&&et(X,e,n),T(t),e=R(e,!0),T(n),l(z,e)?(n.enumerable?(l(t,K)&&t[K][e]&&(t[K][e]=!1),n=w(n,{enumerable:L(0,!1)})):(l(t,K)||M(t,K,L(1,{})),t[K][e]=!0),J(t,e,n)):M(t,e,n)};Q||(p((j=function(t){if(this instanceof j)throw TypeError("Symbol is not a constructor!");var e=v(0<arguments.length?t:void 0),n=function(t){this===Y&&n.call(X,t),l(this,K)&&l(this[K],e)&&(this[K][e]=!1),J(this,e,L(1,t))};return f&&$&&J(Y,e,{configurable:!0,set:n}),i(e)})[U],"toString",function(){return this._k}),N.f=a,P.f=et,n(44).f=x.f=s,n(43).f=o,D.f=c,f&&!n(32)&&p(Y,"propertyIsEnumerable",o,!0),E.f=function(t){return i(b(t))}),d(d.G+d.W+d.F*!Q,{Symbol:j});for(var nt="hasInstance,isConcatSpreadable,iterator,match,replace,search,species,split,toPrimitive,toStringTag,unscopables".split(","),it=0;nt.length>it;)b(nt[it++]);for(var rt=k(b.store),ot=0;rt.length>ot;)g(rt[ot++]);d(d.S+d.F*!Q,"Symbol",{for:function(t){return l(q,t+="")?q[t]:q[t]=j(t)},keyFor:function(t){if(!tt(t))throw TypeError(t+" is not a symbol!");for(var e in q)if(q[e]===t)return e},useSetter:function(){$=!0},useSimple:function(){$=!1}}),d(d.S+d.F*!Q,"Object",{create:function(t,e){return void 0===e?w(t):r(w(t),e)},defineProperty:et,defineProperties:r,getOwnPropertyDescriptor:a,getOwnPropertyNames:s,getOwnPropertySymbols:c});var at=_(function(){D.f(1)});d(d.S+d.F*at,"Object",{getOwnPropertySymbols:function(t){return D.f(S(t))}}),B&&d(d.S+d.F*(!Q||_(function(){var t=j();return"[null]"!=V([t])||"{}"!=V({a:t})||"{}"!=V(Object(t))})),"JSON",{stringify:function(t){for(var e,n,i=[t],r=1;r<arguments.length;)i.push(arguments[r++]);if(n=e=i[1],(I(e)||void 0!==t)&&!tt(t))return A(e)||(e=function(t,e){if("function"==typeof n&&(e=n.call(this,t,e)),!tt(e))return e}),i[1]=e,V.apply(B,i)}}),j[U][G]||n(20)(j[U],G,j[U].valueOf),m(j,"Symbol"),m(Math,"Math",!0),m(u.JSON,"JSON",!0)},function(t,e,n){"use strict";var i=n(61),r=n(48);t.exports=n(68)("Map",function(e){return function(t){return e(this,0<arguments.length?t:void 0)}},{get:function(t){var e=i.getEntry(r(this,"Map"),t);return e&&e.v},set:function(t,e){return i.def(r(this,"Map"),0===t?0:t,e)}},i,!0)},function(t,e,n){var i=n(17),c=n(30),u=n(35),l=n(13),f=n(14),r=n(16),d=n(89),p=(n(11).Reflect||{}).construct,h=r(function(){function t(){}return!(p(function(){},[],t)instanceof t)}),_=!r(function(){p(function(){})});i(i.S+i.F*(h||_),"Reflect",{construct:function(t,e,n){u(t),l(e);var i=arguments.length<3?t:u(n);if(_&&!h)return p(t,e,i);if(t==i){switch(e.length){case 0:return new t;case 1:return new t(e[0]);case 2:return new t(e[0],e[1]);case 3:return new t(e[0],e[1],e[2]);case 4:return new t(e[0],e[1],e[2],e[3])}var r=[null];return r.push.apply(r,e),new(d.apply(t,r))}var o=i.prototype,a=c(f(o)?o:Object.prototype),s=Function.apply.call(t,a,e);return f(s)?s:a}})},function(t,e,n){var i=n(17);i(i.S,"Object",{setPrototypeOf:n(71).set})},function(t,e,n){var i=n(33)("wks"),r=n(25),o=n(11).Symbol,a="function"==typeof o;(t.exports=function(t){return i[t]||(i[t]=a&&o[t]||(a?o:r)("Symbol."+t))}).store=i},function(t,e){var n=t.exports="undefined"!=typeof window&&window.Math==Math?window:"undefined"!=typeof self&&self.Math==Math?self:Function("return this")();"number"==typeof __g&&(__g=n)},function(t,e,n){var i=n(13),r=n(53),o=n(34),a=Object.defineProperty;e.f=n(15)?Object.defineProperty:function(t,e,n){if(i(t),e=o(e,!0),i(n),r)try{return a(t,e,n)}catch(t){}if("get"in n||"set"in n)throw TypeError("Accessors not supported!");return"value"in n&&(t[e]=n.value),t}},function(t,e,n){var i=n(14);t.exports=function(t){if(!i(t))throw TypeError(t+" is not an object!");return t}},function(t,e){t.exports=function(t){return"object"==typeof t?null!==t:"function"==typeof t}},function(t,e,n){t.exports=!n(16)(function(){return 7!=Object.defineProperty({},"a",{get:function(){return 7}}).a})},function(t,e){t.exports=function(t){try{return!!t()}catch(t){return!0}}},function(t,e,n){var _=n(11),y=n(21),m=n(20),v=n(18),b=n(27),E="prototype",g=function(t,e,n){var i,r,o,a,s=t&g.F,c=t&g.G,u=t&g.S,l=t&g.P,f=t&g.B,d=c?_:u?_[e]||(_[e]={}):(_[e]||{})[E],p=c?y:y[e]||(y[e]={}),h=p[E]||(p[E]={});for(i in c&&(n=e),n)o=((r=!s&&d&&void 0!==d[i])?d:n)[i],a=f&&r?b(o,_):l&&"function"==typeof o?b(Function.call,o):o,d&&v(d,i,o,t&g.U),p[i]!=o&&m(p,i,a),l&&h[i]!=o&&(h[i]=o)};_.core=y,g.F=1,g.G=2,g.S=4,g.P=8,g.B=16,g.W=32,g.U=64,g.R=128,t.exports=g},function(t,e,n){var o=n(11),a=n(20),s=n(19),c=n(25)("src"),i=n(78),r="toString",u=(""+i).split(r);n(21).inspectSource=function(t){return i.call(t)},(t.exports=function(t,e,n,i){var r="function"==typeof n;r&&(s(n,"name")||a(n,"name",e)),t[e]!==n&&(r&&(s(n,c)||a(n,c,t[e]?""+t[e]:u.join(String(e)))),t===o?t[e]=n:i?t[e]?t[e]=n:a(t,e,n):(delete t[e],a(t,e,n)))})(Function.prototype,r,function(){return"function"==typeof this&&this[c]||i.call(this)})},function(t,e){var n={}.hasOwnProperty;t.exports=function(t,e){return n.call(t,e)}},function(t,e,n){var i=n(12),r=n(26);t.exports=n(15)?function(t,e,n){return i.f(t,e,r(1,n))}:function(t,e,n){return t[e]=n,t}},function(t,e){var n=t.exports={version:"2.6.11"};"number"==typeof __e&&(__e=n)},function(t,e,n){var i=n(80),r=n(23);t.exports=function(t){return i(r(t))}},function(t,e){t.exports=function(t){if(null==t)throw TypeError("Can't call method on  "+t);return t}},function(t,e,n){var v,i,b=n(107),E=n(108),g=(i=[],{activateTrap:function(t){if(0<i.length){var e=i[i.length-1];e!==t&&e.pause()}var n=i.indexOf(t);-1===n||i.splice(n,1),i.push(t)},deactivateTrap:function(t){var e=i.indexOf(t);-1!==e&&i.splice(e,1),0<i.length&&i[i.length-1].unpause()}});function C(t){return setTimeout(t,0)}t.exports=function(t,e){var i=document,n="string"==typeof t?i.querySelector(t):t,r=E({returnFocusOnDeactivate:!0,escapeDeactivates:!0},e),o={firstTabbableNode:null,lastTabbableNode:null,nodeFocusedBeforeActivation:null,mostRecentlyFocusedNode:null,active:!1,paused:!1},a={activate:function(t){if(o.active)return;y(),o.active=!0,o.paused=!1,o.nodeFocusedBeforeActivation=i.activeElement;var e=t&&t.onActivate?t.onActivate:r.onActivate;e&&e();return c(),a},deactivate:s,pause:function(){if(o.paused||!o.active)return;o.paused=!0,u()},unpause:function(){if(!o.paused||!o.active)return;o.paused=!1,y(),c()}};return a;function s(t){if(o.active){clearTimeout(v),u(),o.active=!1,o.paused=!1,g.deactivateTrap(a);var e=t&&void 0!==t.onDeactivate?t.onDeactivate:r.onDeactivate;return e&&e(),(t&&void 0!==t.returnFocus?t.returnFocus:r.returnFocusOnDeactivate)&&C(function(){var t;m((t=o.nodeFocusedBeforeActivation,l("setReturnFocus")||t))}),a}}function c(){if(o.active)return g.activateTrap(a),v=C(function(){m(f())}),i.addEventListener("focusin",p,!0),i.addEventListener("mousedown",d,{capture:!0,passive:!1}),i.addEventListener("touchstart",d,{capture:!0,passive:!1}),i.addEventListener("click",_,{capture:!0,passive:!1}),i.addEventListener("keydown",h,{capture:!0,passive:!1}),a}function u(){if(o.active)return i.removeEventListener("focusin",p,!0),i.removeEventListener("mousedown",d,!0),i.removeEventListener("touchstart",d,!0),i.removeEventListener("click",_,!0),i.removeEventListener("keydown",h,!0),a}function l(t){var e=r[t],n=e;if(!e)return null;if("string"==typeof e&&!(n=i.querySelector(e)))throw new Error("`"+t+"` refers to no known node");if("function"==typeof e&&!(n=e()))throw new Error("`"+t+"` did not return a node");return n}function f(){var t;if(!(t=null!==l("initialFocus")?l("initialFocus"):n.contains(i.activeElement)?i.activeElement:o.firstTabbableNode||l("fallbackFocus")))throw new Error("Your focus-trap needs to have at least one focusable element");return t}function d(t){n.contains(t.target)||(r.clickOutsideDeactivates?s({returnFocus:!b.isFocusable(t.target)}):r.allowOutsideClick&&r.allowOutsideClick(t)||t.preventDefault())}function p(t){n.contains(t.target)||t.target instanceof Document||(t.stopImmediatePropagation(),m(o.mostRecentlyFocusedNode||f()))}function h(t){if(!1!==r.escapeDeactivates&&("Escape"===(e=t).key||"Esc"===e.key||27===e.keyCode))return t.preventDefault(),void s();var e;if("Tab"!==(n=t).key&&9!==n.keyCode)var n;else!function(t){if(y(),t.shiftKey&&t.target===o.firstTabbableNode)return t.preventDefault(),m(o.lastTabbableNode);if(!t.shiftKey&&t.target===o.lastTabbableNode)t.preventDefault(),m(o.firstTabbableNode)}(t)}function _(t){r.clickOutsideDeactivates||n.contains(t.target)||r.allowOutsideClick&&r.allowOutsideClick(t)||(t.preventDefault(),t.stopImmediatePropagation())}function y(){var t=b(n);o.firstTabbableNode=t[0]||f(),o.lastTabbableNode=t[t.length-1]||f()}function m(t){var e;t!==i.activeElement&&(t&&t.focus?(t.focus(),o.mostRecentlyFocusedNode=t,(e=t).tagName&&"input"===e.tagName.toLowerCase()&&"function"==typeof e.select&&t.select()):m(f()))}}},function(t,e){var n=0,i=Math.random();t.exports=function(t){return"Symbol(".concat(void 0===t?"":t,")_",(++n+i).toString(36))}},function(t,e){t.exports=function(t,e){return{enumerable:!(1&t),configurable:!(2&t),writable:!(4&t),value:e}}},function(t,e,n){var o=n(35);t.exports=function(i,r,t){if(o(i),void 0===r)return i;switch(t){case 1:return function(t){return i.call(r,t)};case 2:return function(t,e){return i.call(r,t,e)};case 3:return function(t,e,n){return i.call(r,t,e,n)}}return function(){return i.apply(r,arguments)}}},function(t,e,n){var i=n(55),r=n(42);t.exports=Object.keys||function(t){return i(t,r)}},function(t,e){var n={}.toString;t.exports=function(t){return n.call(t).slice(8,-1)}},function(t,e,i){function r(){}var o=i(13),a=i(83),s=i(42),c=i(41)("IE_PROTO"),u="prototype",l=function(){var t,e=i(54)("iframe"),n=s.length;for(e.style.display="none",i(84).appendChild(e),e.src="javascript:",(t=e.contentWindow.document).open(),t.write("<script>document.F=Object<\/script>"),t.close(),l=t.F;n--;)delete l[u][s[n]];return l()};t.exports=Object.create||function(t,e){var n;return null!==t?(r[u]=o(t),n=new r,r[u]=null,n[c]=t):n=l(),void 0===e?n:a(n,e)}},function(t,e){t.exports={}},function(t,e){t.exports=!1},function(t,e,n){var i=n(21),r=n(11),o="__core-js_shared__",a=r[o]||(r[o]={});(t.exports=function(t,e){return a[t]||(a[t]=void 0!==e?e:{})})("versions",[]).push({version:i.version,mode:n(32)?"pure":"global",copyright:"© 2019 Denis Pushkarev (zloirock.ru)"})},function(t,e,n){var r=n(14);t.exports=function(t,e){if(!r(t))return t;var n,i;if(e&&"function"==typeof(n=t.toString)&&!r(i=n.call(t)))return i;if("function"==typeof(n=t.valueOf)&&!r(i=n.call(t)))return i;if(!e&&"function"==typeof(n=t.toString)&&!r(i=n.call(t)))return i;throw TypeError("Can't convert object to primitive value")}},function(t,e){t.exports=function(t){if("function"!=typeof t)throw TypeError(t+" is not a function!");return t}},function(t,e,n){var i=n(12).f,r=n(19),o=n(10)("toStringTag");t.exports=function(t,e,n){t&&!r(t=n?t:t.prototype,o)&&i(t,o,{configurable:!0,value:e})}},function(t,e,n){var i=n(40),r=Math.min;t.exports=function(t){return 0<t?r(i(t),9007199254740991):0}},function(t,e,n){var i=n(23);t.exports=function(t){return Object(i(t))}},function(t,e,n){function i(t){s(t,r,{value:{i:"O"+ ++c,w:{}}})}var r=n(25)("meta"),o=n(14),a=n(19),s=n(12).f,c=0,u=Object.isExtensible||function(){return!0},l=!n(16)(function(){return u(Object.preventExtensions({}))}),f=t.exports={KEY:r,NEED:!1,fastKey:function(t,e){if(!o(t))return"symbol"==typeof t?t:("string"==typeof t?"S":"P")+t;if(!a(t,r)){if(!u(t))return"F";if(!e)return"E";i(t)}return t[r].i},getWeak:function(t,e){if(!a(t,r)){if(!u(t))return!0;if(!e)return!1;i(t)}return t[r].w},onFreeze:function(t){return l&&f.NEED&&u(t)&&!a(t,r)&&i(t),t}}},function(t,e){var n=Math.ceil,i=Math.floor;t.exports=function(t){return isNaN(t=+t)?0:(0<t?i:n)(t)}},function(t,e,n){var i=n(33)("keys"),r=n(25);t.exports=function(t){return i[t]||(i[t]=r(t))}},function(t,e){t.exports="constructor,hasOwnProperty,isPrototypeOf,propertyIsEnumerable,toLocaleString,toString,valueOf".split(",")},function(t,e){e.f={}.propertyIsEnumerable},function(t,e,n){var i=n(55),r=n(42).concat("length","prototype");e.f=Object.getOwnPropertyNames||function(t){return i(t,r)}},function(t,e,n){var i=n(43),r=n(26),o=n(22),a=n(34),s=n(19),c=n(53),u=Object.getOwnPropertyDescriptor;e.f=n(15)?u:function(t,e){if(t=o(t),e=a(e,!0),c)try{return u(t,e)}catch(t){}if(s(t,e))return r(!i.f.call(t,e),t[e])}},function(t,e,n){"use strict";function b(){return this}var E=n(32),g=n(17),C=n(18),A=n(20),T=n(31),I=n(86),S=n(36),O=n(87),R=n(10)("iterator"),L=!([].keys&&"next"in[].keys()),w="values";t.exports=function(t,e,n,i,r,o,a){I(n,e,i);function s(t){if(!L&&t in h)return h[t];switch(t){case"keys":case w:return function(){return new n(this,t)}}return function(){return new n(this,t)}}var c,u,l,f=e+" Iterator",d=r==w,p=!1,h=t.prototype,_=h[R]||h["@@iterator"]||r&&h[r],y=_||s(r),m=r?d?s("entries"):y:void 0,v="Array"==e&&h.entries||_;if(v&&(l=O(v.call(new t)))!==Object.prototype&&l.next&&(S(l,f,!0),E||"function"==typeof l[R]||A(l,R,b)),d&&_&&_.name!==w&&(p=!0,y=function(){return _.call(this)}),E&&!a||!L&&!p&&h[R]||A(h,R,y),T[e]=y,T[f]=b,r)if(c={values:d?y:s(w),keys:o?y:s("keys"),entries:m},a)for(u in c)u in h||C(h,u,c[u]);else g(g.P+g.F*(L||p),e,c);return c}},function(t,e,n){var r=n(29),o=n(10)("toStringTag"),a="Arguments"==r(function(){return arguments}());t.exports=function(t){var e,n,i;return void 0===t?"Undefined":null===t?"Null":"string"==typeof(n=function(t,e){try{return t[e]}catch(t){}}(e=Object(t),o))?n:a?r(e):"Object"==(i=r(e))&&"function"==typeof e.callee?"Arguments":i}},function(t,e,n){var i=n(14);t.exports=function(t,e){if(!i(t)||t._t!==e)throw TypeError("Incompatible receiver, "+e+" required!");return t}},function(t,e,n){"use strict";var i=n(13);t.exports=function(){var t=i(this),e="";return t.global&&(e+="g"),t.ignoreCase&&(e+="i"),t.multiline&&(e+="m"),t.unicode&&(e+="u"),t.sticky&&(e+="y"),e}},function(t,e,n){"use strict";var i,r,a=n(49),s=RegExp.prototype.exec,c=String.prototype.replace,o=s,u="lastIndex",l=(i=/a/,r=/b*/g,s.call(i,"a"),s.call(r,"a"),0!==i[u]||0!==r[u]),f=void 0!==/()??/.exec("")[1];(l||f)&&(o=function(t){var e,n,i,r,o=this;return f&&(n=new RegExp("^"+o.source+"$(?!\\s)",a.call(o))),l&&(e=o[u]),i=s.call(o,t),l&&i&&(o[u]=o.global?i.index+i[0].length:e),f&&i&&1<i.length&&c.call(i[0],n,function(){for(r=1;r<arguments.length-2;r++)void 0===arguments[r]&&(i[r]=void 0)}),i}),t.exports=o},function(t,e,n){var i=n(11),r=n(21),o=n(32),a=n(52),s=n(12).f;t.exports=function(t){var e=r.Symbol||(r.Symbol=o?{}:i.Symbol||{});"_"==t.charAt(0)||t in e||s(e,t,{value:a.f(t)})}},function(t,e,n){e.f=n(10)},function(t,e,n){t.exports=!n(15)&&!n(16)(function(){return 7!=Object.defineProperty(n(54)("div"),"a",{get:function(){return 7}}).a})},function(t,e,n){var i=n(14),r=n(11).document,o=i(r)&&i(r.createElement);t.exports=function(t){return o?r.createElement(t):{}}},function(t,e,n){var a=n(19),s=n(22),c=n(56)(!1),u=n(41)("IE_PROTO");t.exports=function(t,e){var n,i=s(t),r=0,o=[];for(n in i)n!=u&&a(i,n)&&o.push(n);for(;e.length>r;)a(i,n=e[r++])&&(~c(o,n)||o.push(n));return o}},function(t,e,n){var c=n(22),u=n(37),l=n(81);t.exports=function(s){return function(t,e,n){var i,r=c(t),o=u(r.length),a=l(n,o);if(s&&e!=e){for(;a<o;)if((i=r[a++])!=i)return!0}else for(;a<o;a++)if((s||a in r)&&r[a]===e)return s||a||0;return!s&&-1}}},function(t,e){e.f=Object.getOwnPropertySymbols},function(t,e,n){var i=n(10)("unscopables"),r=Array.prototype;null==r[i]&&n(20)(r,i,{}),t.exports=function(t){r[i][t]=!0}},function(t,e){t.exports=function(t,e){return{value:e,done:!!t}}},function(t,e,n){var c=n(40),u=n(23);t.exports=function(s){return function(t,e){var n,i,r=String(u(t)),o=c(e),a=r.length;return o<0||a<=o?s?"":void 0:(n=r.charCodeAt(o))<55296||56319<n||o+1===a||(i=r.charCodeAt(o+1))<56320||57343<i?s?r.charAt(o):n:s?r.slice(o,o+2):i-56320+(n-55296<<10)+65536}}},function(t,e,n){"use strict";function a(t,e){var n,i=h(e);if("F"!==i)return t._i[i];for(n=t._f;n;n=n.n)if(n.k==e)return n}var s=n(12).f,c=n(30),u=n(62),l=n(27),f=n(63),d=n(64),i=n(46),r=n(59),o=n(88),p=n(15),h=n(39).fastKey,_=n(48),y=p?"_s":"size";t.exports={getConstructor:function(t,o,n,i){var r=t(function(t,e){f(t,r,o,"_i"),t._t=o,t._i=c(null),t._f=void 0,t._l=void 0,t[y]=0,null!=e&&d(e,n,t[i],t)});return u(r.prototype,{clear:function(){for(var t=_(this,o),e=t._i,n=t._f;n;n=n.n)n.r=!0,n.p&&(n.p=n.p.n=void 0),delete e[n.i];t._f=t._l=void 0,t[y]=0},delete:function(t){var e=_(this,o),n=a(e,t);if(n){var i=n.n,r=n.p;delete e._i[n.i],n.r=!0,r&&(r.n=i),i&&(i.p=r),e._f==n&&(e._f=i),e._l==n&&(e._l=r),e[y]--}return!!n},forEach:function(t,e){_(this,o);for(var n,i=l(t,1<arguments.length?e:void 0,3);n=n?n.n:this._f;)for(i(n.v,n.k,this);n&&n.r;)n=n.p},has:function(t){return!!a(_(this,o),t)}}),p&&s(r.prototype,"size",{get:function(){return _(this,o)[y]}}),r},def:function(t,e,n){var i,r,o=a(t,e);return o?o.v=n:(t._l=o={i:r=h(e,!0),k:e,v:n,p:i=t._l,n:void 0,r:!1},t._f||(t._f=o),i&&(i.n=o),t[y]++,"F"!==r&&(t._i[r]=o)),t},getEntry:a,setStrong:function(t,n,e){i(t,n,function(t,e){this._t=_(t,n),this._k=e,this._l=void 0},function(){for(var t=this,e=t._k,n=t._l;n&&n.r;)n=n.p;return t._t&&(t._l=n=n?n.n:t._t._f)?r(0,"keys"==e?n.k:"values"==e?n.v:[n.k,n.v]):(t._t=void 0,r(1))},e?"entries":"values",!e,!0),o(n)}}},function(t,e,n){var r=n(18);t.exports=function(t,e,n){for(var i in e)r(t,i,e[i],n);return t}},function(t,e){t.exports=function(t,e,n,i){if(!(t instanceof e)||void 0!==i&&i in t)throw TypeError(n+": incorrect invocation!");return t}},function(t,e,n){var d=n(27),p=n(65),h=n(66),_=n(13),y=n(37),m=n(67),v={},b={};(e=t.exports=function(t,e,n,i,r){var o,a,s,c,u=r?function(){return t}:m(t),l=d(n,i,e?2:1),f=0;if("function"!=typeof u)throw TypeError(t+" is not iterable!");if(h(u)){for(o=y(t.length);f<o;f++)if((c=e?l(_(a=t[f])[0],a[1]):l(t[f]))===v||c===b)return c}else for(s=u.call(t);!(a=s.next()).done;)if((c=p(s,l,a.value,e))===v||c===b)return c}).BREAK=v,e.RETURN=b},function(t,e,n){var o=n(13);t.exports=function(e,t,n,i){try{return i?t(o(n)[0],n[1]):t(n)}catch(t){var r=e.return;throw void 0!==r&&o(r.call(e)),t}}},function(t,e,n){var i=n(31),r=n(10)("iterator"),o=Array.prototype;t.exports=function(t){return void 0!==t&&(i.Array===t||o[r]===t)}},function(t,e,n){var i=n(47),r=n(10)("iterator"),o=n(31);t.exports=n(21).getIteratorMethod=function(t){if(null!=t)return t[r]||t["@@iterator"]||o[i(t)]}},function(t,e,n){"use strict";var m=n(11),v=n(17),b=n(18),E=n(62),g=n(39),C=n(64),A=n(63),T=n(14),I=n(16),S=n(69),O=n(36),R=n(70);t.exports=function(i,t,e,n,r,o){function a(t){var n=l[t];b(l,t,"delete"==t?function(t){return!(o&&!T(t))&&n.call(this,0===t?0:t)}:"has"==t?function(t){return!(o&&!T(t))&&n.call(this,0===t?0:t)}:"get"==t?function(t){return o&&!T(t)?void 0:n.call(this,0===t?0:t)}:"add"==t?function(t){return n.call(this,0===t?0:t),this}:function(t,e){return n.call(this,0===t?0:t,e),this})}var s=m[i],c=s,u=r?"set":"add",l=c&&c.prototype,f={};if("function"==typeof c&&(o||l.forEach&&!I(function(){(new c).entries().next()}))){var d=new c,p=d[u](o?{}:-0,1)!=d,h=I(function(){d.has(1)}),_=S(function(t){new c(t)}),y=!o&&I(function(){for(var t=new c,e=5;e--;)t[u](e,e);return!t.has(-0)});_||(((c=t(function(t,e){A(t,c,i);var n=R(new s,t,c);return null!=e&&C(e,r,n[u],n),n})).prototype=l).constructor=c),(h||y)&&(a("delete"),a("has"),r&&a("get")),(y||p)&&a(u),o&&l.clear&&delete l.clear}else c=n.getConstructor(t,i,r,u),E(c.prototype,e),g.NEED=!0;return O(c,i),f[i]=c,v(v.G+v.W+v.F*(c!=s),f),o||n.setStrong(c,i,r),c}},function(t,e,n){var o=n(10)("iterator"),a=!1;try{var i=[7][o]();i.return=function(){a=!0},Array.from(i,function(){throw 2})}catch(t){}t.exports=function(t,e){if(!e&&!a)return!1;var n=!1;try{var i=[7],r=i[o]();r.next=function(){return{done:n=!0}},i[o]=function(){return r},t(i)}catch(t){}return n}},function(t,e,n){var o=n(14),a=n(71).set;t.exports=function(t,e,n){var i,r=e.constructor;return r!==n&&"function"==typeof r&&(i=r.prototype)!==n.prototype&&o(i)&&a&&a(t,i),t}},function(t,e,r){function o(t,e){if(i(t),!n(e)&&null!==e)throw TypeError(e+": can't set as prototype!")}var n=r(14),i=r(13);t.exports={set:Object.setPrototypeOf||("__proto__"in{}?function(t,n,i){try{(i=r(27)(Function.call,r(45).f(Object.prototype,"__proto__").set,2))(t,[]),n=!(t instanceof Array)}catch(t){n=!0}return function(t,e){return o(t,e),n?t.__proto__=e:i(t,e),t}}({},!1):void 0),check:o}},function(t,e,n){var i=n(38),r=n(28);n(92)("keys",function(){return function(t){return r(i(t))}})},function(t,e,n){"use strict";var h=n(27),i=n(17),_=n(38),y=n(65),m=n(66),v=n(37),b=n(93),E=n(67);i(i.S+i.F*!n(69)(function(t){Array.from(t)}),"Array",{from:function(t,e,n){var i,r,o,a,s=_(t),c="function"==typeof this?this:Array,u=arguments.length,l=1<u?e:void 0,f=void 0!==l,d=0,p=E(s);if(f&&(l=h(l,2<u?n:void 0,2)),null==p||c==Array&&m(p))for(r=new c(i=v(s.length));d<i;d++)b(r,d,f?l(s[d],d):s[d]);else for(a=p.call(s),r=new c;!(o=a.next()).done;d++)b(r,d,f?y(a,l,[o.value,d],!0):o.value);return r.length=d,r}})},function(t,e,n){var i=n(12).f,r=Function.prototype,o=/^\s*function ([^ (]*)/;"name"in r||n(15)&&i(r,"name",{configurable:!0,get:function(){try{return(""+this).match(o)[1]}catch(t){return""}}})},function(t,e,n){var i=n(14),r=n(29),o=n(10)("match");t.exports=function(t){var e;return i(t)&&(void 0!==(e=t[o])?!!e:"RegExp"==r(t))}},function(t,e,n){"use strict";var i=n(61),r=n(48);t.exports=n(68)("Set",function(e){return function(t){return e(this,0<arguments.length?t:void 0)}},{add:function(t){return i.def(r(this,"Set"),t=0===t?0:t,t)}},i)},function(t,e){(function(){"use strict";var c=window.Document.prototype.createElement,u=window.Document.prototype.createElementNS,l=window.Document.prototype.importNode,f=window.Document.prototype.prepend,d=window.Document.prototype.append,p=window.DocumentFragment.prototype.prepend,h=window.DocumentFragment.prototype.append,_=window.Node.prototype.cloneNode,y=window.Node.prototype.appendChild,m=window.Node.prototype.insertBefore,v=window.Node.prototype.removeChild,b=window.Node.prototype.replaceChild,E=Object.getOwnPropertyDescriptor(window.Node.prototype,"textContent"),g=window.Element.prototype.attachShadow,C=Object.getOwnPropertyDescriptor(window.Element.prototype,"innerHTML"),A=window.Element.prototype.getAttribute,T=window.Element.prototype.setAttribute,I=window.Element.prototype.removeAttribute,S=window.Element.prototype.getAttributeNS,O=window.Element.prototype.setAttributeNS,R=window.Element.prototype.removeAttributeNS,L=window.Element.prototype.insertAdjacentElement,w=window.Element.prototype.insertAdjacentHTML,x=window.Element.prototype.prepend,N=window.Element.prototype.append,D=window.Element.prototype.before,P=window.Element.prototype.after,k=window.Element.prototype.replaceWith,F=window.Element.prototype.remove,M=window.HTMLElement,H=Object.getOwnPropertyDescriptor(window.HTMLElement.prototype,"innerHTML"),j=window.HTMLElement.prototype.insertAdjacentElement,B=window.HTMLElement.prototype.insertAdjacentHTML,n=new Set;function i(t){var e=n.has(t);return t=/^[a-z][.0-9_a-z]*-[-.0-9_a-z]*$/.test(t),!e&&t}"annotation-xml color-profile font-face font-face-src font-face-uri font-face-format font-face-name missing-glyph".split(" ").forEach(function(t){return n.add(t)});var r=document.contains?document.contains.bind(document):document.documentElement.contains.bind(document.documentElement);function V(t){var e=t.isConnected;if(void 0!==e)return e;if(r(t))return!0;for(;t&&!(t.__CE_isImportDocument||t instanceof Document);)t=t.parentNode||(window.ShadowRoot&&t instanceof ShadowRoot?t.host:void 0);return!(!t||!(t.__CE_isImportDocument||t instanceof Document))}function U(t){var e=t.children;if(e)return Array.prototype.slice.call(e);for(e=[],t=t.firstChild;t;t=t.nextSibling)t.nodeType===Node.ELEMENT_NODE&&e.push(t);return e}function s(t,e){for(;e&&e!==t&&!e.nextSibling;)e=e.parentNode;return e&&e!==t?e.nextSibling:null}function K(){var t=!(null==st||!st.noDocumentConstructionObserver),e=!(null==st||!st.shadyDomFastWalk);this.h=[],this.a=[],this.f=!1,this.shadyDomFastWalk=e,this.C=!t}function G(t,e,n,i){var r=window.ShadyDom;if(t.shadyDomFastWalk&&r&&r.inUse){if(e.nodeType===Node.ELEMENT_NODE&&n(e),e.querySelectorAll)for(t=r.nativeMethods.querySelectorAll.call(e,"*"),e=0;e<t.length;e++)n(t[e])}else!function t(e,n,i){for(var r=e;r;){if(r.nodeType===Node.ELEMENT_NODE){var o=r;n(o);var a=o.localName;if("link"===a&&"import"===o.getAttribute("rel")){if(r=o.import,void 0===i&&(i=new Set),r instanceof Node&&!i.has(r))for(i.add(r),r=r.firstChild;r;r=r.nextSibling)t(r,n,i);r=s(e,o);continue}if("template"===a){r=s(e,o);continue}if(o=o.__CE_shadowRoot)for(o=o.firstChild;o;o=o.nextSibling)t(o,n,i)}r=r.firstChild?r.firstChild:s(e,r)}}(e,n,i)}function W(e,t){e.f&&G(e,t,function(t){return q(e,t)})}function q(t,e){if(t.f&&!e.__CE_patched){e.__CE_patched=!0;for(var n=0;n<t.h.length;n++)t.h[n](e);for(n=0;n<t.a.length;n++)t.a[n](e)}}function z(t,e){var n=[];for(G(t,e,function(t){return n.push(t)}),e=0;e<n.length;e++){var i=n[e];1===i.__CE_state?t.connectedCallback(i):Q(t,i)}}function X(t,e){var n=[];for(G(t,e,function(t){return n.push(t)}),e=0;e<n.length;e++){var i=n[e];1===i.__CE_state&&t.disconnectedCallback(i)}}function Y(i,t,e){var r=(e=void 0===e?{}:e).D,o=e.upgrade||function(t){return Q(i,t)},a=[];for(G(i,t,function(n){if(i.f&&q(i,n),"link"===n.localName&&"import"===n.getAttribute("rel")){var t=n.import;t instanceof Node&&(t.__CE_isImportDocument=!0,t.__CE_registry=document.__CE_registry),t&&"complete"===t.readyState?t.__CE_documentLoadHandled=!0:n.addEventListener("load",function(){var t=n.import;if(!t.__CE_documentLoadHandled){t.__CE_documentLoadHandled=!0;var e=new Set;r&&(r.forEach(function(t){return e.add(t)}),e.delete(t)),Y(i,t,{D:e,upgrade:o})}})}else a.push(n)},r),t=0;t<a.length;t++)o(a[t])}function Q(t,e){try{var n=e.ownerDocument,i=n.__CE_registry,r=i&&(n.defaultView||n.__CE_isImportDocument)?it(i,e.localName):void 0;if(r&&void 0===e.__CE_state){r.constructionStack.push(e);try{try{if(new r.constructorFunction!==e)throw Error("The custom element constructor did not produce the element being upgraded.")}finally{r.constructionStack.pop()}}catch(t){throw e.__CE_state=2,t}if(e.__CE_state=1,(e.__CE_definition=r).attributeChangedCallback&&e.hasAttributes()){var o=r.observedAttributes;for(r=0;r<o.length;r++){var a=o[r],s=e.getAttribute(a);null!==s&&t.attributeChangedCallback(e,a,null,s,null)}}V(e)&&t.connectedCallback(e)}}catch(t){$(t)}}function Z(e,n,i,r){var t=n.__CE_registry;if(t&&(null===r||"http://www.w3.org/1999/xhtml"===r)&&(t=it(t,i)))try{var o=new t.constructorFunction;if(void 0===o.__CE_state||void 0===o.__CE_definition)throw Error("Failed to construct '"+i+"': The returned value was not constructed with the HTMLElement constructor.");if("http://www.w3.org/1999/xhtml"!==o.namespaceURI)throw Error("Failed to construct '"+i+"': The constructed element's namespace must be the HTML namespace.");if(o.hasAttributes())throw Error("Failed to construct '"+i+"': The constructed element must not have any attributes.");if(null!==o.firstChild)throw Error("Failed to construct '"+i+"': The constructed element must not have any children.");if(null!==o.parentNode)throw Error("Failed to construct '"+i+"': The constructed element must not have a parent node.");if(o.ownerDocument!==n)throw Error("Failed to construct '"+i+"': The constructed element's owner document is incorrect.");if(o.localName!==i)throw Error("Failed to construct '"+i+"': The constructed element's local name is incorrect.");return o}catch(t){return $(t),n=null===r?c.call(n,i):u.call(n,r,i),Object.setPrototypeOf(n,HTMLUnknownElement.prototype),n.__CE_state=2,n.__CE_definition=void 0,q(e,n),n}return q(e,n=null===r?c.call(n,i):u.call(n,r,i)),n}function $(t){var e=t.message,n=t.sourceURL||t.fileName||"",i=t.line||t.lineNumber||0,r=t.column||t.columnNumber||0,o=void 0;void 0===ErrorEvent.prototype.initErrorEvent?o=new ErrorEvent("error",{cancelable:!0,message:e,filename:n,lineno:i,colno:r,error:t}):((o=document.createEvent("ErrorEvent")).initErrorEvent("error",!1,!0,e,n,i),o.preventDefault=function(){Object.defineProperty(this,"defaultPrevented",{configurable:!0,get:function(){return!0}})}),void 0===o.error&&Object.defineProperty(o,"error",{configurable:!0,enumerable:!0,get:function(){return t}}),window.dispatchEvent(o),o.defaultPrevented||console.error(t)}function o(){var e=this;this.a=void 0,this.w=new Promise(function(t){e.g=t})}function e(t){var e=document;this.g=void 0,this.b=t,this.a=e,Y(this.b,this.a),"loading"===this.a.readyState&&(this.g=new MutationObserver(this.A.bind(this)),this.g.observe(this.a,{childList:!0,subtree:!0}))}function a(t){t.g&&t.g.disconnect()}function J(t){this.j=new Map,this.l=new Map,this.u=new Map,this.o=!1,this.s=new Map,this.i=function(t){return t()},this.c=!1,this.m=[],this.b=t,this.v=t.C?new e(t):void 0}function tt(t,e){if(!i(e))throw new SyntaxError("The element name '"+e+"' is not valid.");if(it(t,e))throw Error("A custom element with name '"+e+"' has already been defined.");if(t.o)throw Error("A custom element is already being defined.")}function et(t,e,n){var i;t.o=!0;try{var r=n.prototype;if(!(r instanceof Object))throw new TypeError("The custom element constructor's prototype is not an object.");var o=function(t){var e=r[t];if(void 0!==e&&!(e instanceof Function))throw Error("The '"+t+"' callback must be a function.");return e},a=o("connectedCallback"),s=o("disconnectedCallback"),c=o("adoptedCallback"),u=(i=o("attributeChangedCallback"))&&n.observedAttributes||[]}catch(t){throw t}finally{t.o=!1}return n={localName:e,constructorFunction:n,connectedCallback:a,disconnectedCallback:s,adoptedCallback:c,attributeChangedCallback:i,observedAttributes:u,constructionStack:[]},t.l.set(e,n),t.u.set(n.constructorFunction,n),n}function nt(i){if(!1!==i.c){i.c=!1;for(var r=[],t=i.m,o=new Map,e=0;e<t.length;e++)o.set(t[e],[]);for(Y(i.b,document,{upgrade:function(t){if(void 0===t.__CE_state){var e=t.localName,n=o.get(e);n?n.push(t):i.l.has(e)&&r.push(t)}}}),e=0;e<r.length;e++)Q(i.b,r[e]);for(e=0;e<t.length;e++){for(var n=t[e],a=o.get(n),s=0;s<a.length;s++)Q(i.b,a[s]);(n=i.s.get(n))&&n.resolve(void 0)}t.length=0}}function it(t,e){var n=t.l.get(e);if(n)return n;if(n=t.j.get(e)){t.j.delete(e);try{return et(t,e,n())}catch(t){$(t)}}}function rt(s,t,e){function n(a){return function(t){for(var e=[],n=0;n<arguments.length;++n)e[n]=arguments[n];n=[];for(var i=[],r=0;r<e.length;r++){var o=e[r];if(o instanceof Element&&V(o)&&i.push(o),o instanceof DocumentFragment)for(o=o.firstChild;o;o=o.nextSibling)n.push(o);else n.push(o)}for(a.apply(this,e),e=0;e<i.length;e++)X(s,i[e]);if(V(this))for(e=0;e<n.length;e++)(i=n[e])instanceof Element&&z(s,i)}}void 0!==e.prepend&&(t.prepend=n(e.prepend)),void 0!==e.append&&(t.append=n(e.append))}function ot(a){function e(t,o){Object.defineProperty(t,"innerHTML",{enumerable:o.enumerable,configurable:!0,get:o.get,set:function(t){var e=this,n=void 0;if(V(this)&&(n=[],G(a,this,function(t){t!==e&&n.push(t)})),o.set.call(this,t),n)for(var i=0;i<n.length;i++){var r=n[i];1===r.__CE_state&&a.disconnectedCallback(r)}return this.ownerDocument.__CE_registry?Y(a,this):W(a,this),t}})}function t(t,i){t.insertAdjacentElement=function(t,e){var n=V(e);return t=i.call(this,t,e),n&&X(a,e),V(t)&&z(a,e),t}}function n(t,i){function r(t,e){for(var n=[];t!==e;t=t.nextSibling)n.push(t);for(e=0;e<n.length;e++)Y(a,n[e])}t.insertAdjacentHTML=function(t,e){if("beforebegin"===(t=t.toLowerCase())){var n=this.previousSibling;i.call(this,t,e),r(n||this.parentNode.firstChild,this)}else if("afterbegin"===t)n=this.firstChild,i.call(this,t,e),r(this.firstChild,n);else if("beforeend"===t)n=this.lastChild,i.call(this,t,e),r(n||this.firstChild,null);else{if("afterend"!==t)throw new SyntaxError("The value provided ("+String(t)+") is not one of 'beforebegin', 'afterbegin', 'beforeend', or 'afterend'.");n=this.nextSibling,i.call(this,t,e),r(this.nextSibling,n)}}}var i,r,s,o;function c(a){return function(t){for(var e=[],n=0;n<arguments.length;++n)e[n]=arguments[n];n=[];for(var i=[],r=0;r<e.length;r++){var o=e[r];if(o instanceof Element&&V(o)&&i.push(o),o instanceof DocumentFragment)for(o=o.firstChild;o;o=o.nextSibling)n.push(o);else n.push(o)}for(a.apply(this,e),e=0;e<i.length;e++)X(s,i[e]);if(V(this))for(e=0;e<n.length;e++)(i=n[e])instanceof Element&&z(s,i)}}g&&(Element.prototype.attachShadow=function(t){if(t=g.call(this,t),a.f&&!t.__CE_patched){t.__CE_patched=!0;for(var e=0;e<a.h.length;e++)a.h[e](t)}return this.__CE_shadowRoot=t}),C&&C.get?e(Element.prototype,C):H&&H.get?e(HTMLElement.prototype,H):(r=function(t){e(t,{enumerable:!0,configurable:!0,get:function(){return _.call(this,!0).innerHTML},set:function(t){var e="template"===this.localName,n=e?this.content:this,i=u.call(document,this.namespaceURI,this.localName);for(i.innerHTML=t;0<n.childNodes.length;)v.call(n,n.childNodes[0]);for(t=e?i.content:i;0<t.childNodes.length;)y.call(n,t.childNodes[0])}})},(i=a).f=!0,i.a.push(r)),Element.prototype.setAttribute=function(t,e){if(1!==this.__CE_state)return T.call(this,t,e);var n=A.call(this,t);T.call(this,t,e),e=A.call(this,t),a.attributeChangedCallback(this,t,n,e,null)},Element.prototype.setAttributeNS=function(t,e,n){if(1!==this.__CE_state)return O.call(this,t,e,n);var i=S.call(this,t,e);O.call(this,t,e,n),n=S.call(this,t,e),a.attributeChangedCallback(this,e,i,n,t)},Element.prototype.removeAttribute=function(t){if(1!==this.__CE_state)return I.call(this,t);var e=A.call(this,t);I.call(this,t),null!==e&&a.attributeChangedCallback(this,t,e,null,null)},Element.prototype.removeAttributeNS=function(t,e){if(1!==this.__CE_state)return R.call(this,t,e);var n=S.call(this,t,e);R.call(this,t,e);var i=S.call(this,t,e);n!==i&&a.attributeChangedCallback(this,e,n,i,t)},j?t(HTMLElement.prototype,j):L&&t(Element.prototype,L),B?n(HTMLElement.prototype,B):w&&n(Element.prototype,w),rt(a,Element.prototype,{prepend:x,append:N}),s=a,o=Element.prototype,void 0!==D&&(o.before=c(D)),void 0!==P&&(o.after=c(P)),void 0!==k&&(o.replaceWith=function(t){for(var e=[],n=0;n<arguments.length;++n)e[n]=arguments[n];n=[];for(var i=[],r=0;r<e.length;r++){var o=e[r];if(o instanceof Element&&V(o)&&i.push(o),o instanceof DocumentFragment)for(o=o.firstChild;o;o=o.nextSibling)n.push(o);else n.push(o)}for(r=V(this),k.apply(this,e),e=0;e<i.length;e++)X(s,i[e]);if(r)for(X(s,this),e=0;e<n.length;e++)(i=n[e])instanceof Element&&z(s,i)}),void 0!==F&&(o.remove=function(){var t=V(this);F.call(this),t&&X(s,this)})}K.prototype.connectedCallback=function(t){var e=t.__CE_definition;if(e.connectedCallback)try{e.connectedCallback.call(t)}catch(t){$(t)}},K.prototype.disconnectedCallback=function(t){var e=t.__CE_definition;if(e.disconnectedCallback)try{e.disconnectedCallback.call(t)}catch(t){$(t)}},K.prototype.attributeChangedCallback=function(t,e,n,i,r){var o=t.__CE_definition;if(o.attributeChangedCallback&&-1<o.observedAttributes.indexOf(e))try{o.attributeChangedCallback.call(t,e,n,i,r)}catch(t){$(t)}},o.prototype.resolve=function(t){if(this.a)throw Error("Already resolved.");this.a=t,this.g(t)},e.prototype.A=function(t){var e=this.a.readyState;for("interactive"!==e&&"complete"!==e||a(this),e=0;e<t.length;e++)for(var n=t[e].addedNodes,i=0;i<n.length;i++)Y(this.b,n[i])},J.prototype.B=function(t,e){var n=this;if(!(e instanceof Function))throw new TypeError("Custom element constructor getters must be functions.");tt(this,t),this.j.set(t,e),this.m.push(t),this.c||(this.c=!0,this.i(function(){return nt(n)}))},J.prototype.define=function(t,e){var n=this;if(!(e instanceof Function))throw new TypeError("Custom element constructors must be functions.");tt(this,t),et(this,t,e),this.m.push(t),this.c||(this.c=!0,this.i(function(){return nt(n)}))},J.prototype.upgrade=function(t){Y(this.b,t)},J.prototype.get=function(t){if(t=it(this,t))return t.constructorFunction},J.prototype.whenDefined=function(t){if(!i(t))return Promise.reject(new SyntaxError("'"+t+"' is not a valid custom element name."));var e=this.s.get(t);if(e)return e.w;e=new o,this.s.set(t,e);var n=this.l.has(t)||this.j.has(t);return t=-1===this.m.indexOf(t),n&&t&&e.resolve(void 0),e.w},J.prototype.polyfillWrapFlushCallback=function(e){this.v&&a(this.v);var n=this.i;this.i=function(t){return e(function(){return n(t)})}},(window.CustomElementRegistry=J).prototype.define=J.prototype.define,J.prototype.upgrade=J.prototype.upgrade,J.prototype.get=J.prototype.get,J.prototype.whenDefined=J.prototype.whenDefined,J.prototype.polyfillDefineLazy=J.prototype.B,J.prototype.polyfillWrapFlushCallback=J.prototype.polyfillWrapFlushCallback;var at={};var st=window.customElements;function t(){var o,n,a,t,e,i=new K;function r(){var t=this.constructor,e=document.__CE_registry.u.get(t);if(!e)throw Error("Failed to construct a custom element: The constructor was not registered with `customElements`.");var n=e.constructionStack;if(0===n.length)return n=c.call(document,e.localName),Object.setPrototypeOf(n,t.prototype),n.__CE_state=1,n.__CE_definition=e,q(o,n),n;var i=n.length-1,r=n[i];if(r===at)throw Error("Failed to construct '"+e.localName+"': This element was already constructed.");return n[i]=at,Object.setPrototypeOf(r,t.prototype),q(o,r),r}function s(t,o){Object.defineProperty(t,"textContent",{enumerable:o.enumerable,configurable:!0,get:o.get,set:function(t){if(this.nodeType===Node.TEXT_NODE)o.set.call(this,t);else{var e=void 0;if(this.firstChild){var n=this.childNodes,i=n.length;if(0<i&&V(this)){e=Array(i);for(var r=0;r<i;r++)e[r]=n[r]}}if(o.set.call(this,t),e)for(t=0;t<e.length;t++)X(a,e[t])}}})}o=i,r.prototype=M.prototype,Object.defineProperty(HTMLElement.prototype,"constructor",{writable:!0,configurable:!0,enumerable:!1,value:r}),window.HTMLElement=r,n=i,Document.prototype.createElement=function(t){return Z(n,this,t,null)},Document.prototype.importNode=function(t,e){return t=l.call(this,t,!!e),this.__CE_registry?Y(n,t):W(n,t),t},Document.prototype.createElementNS=function(t,e){return Z(n,this,e,t)},rt(n,Document.prototype,{prepend:f,append:d}),rt(i,DocumentFragment.prototype,{prepend:p,append:h}),a=i,Node.prototype.insertBefore=function(t,e){if(t instanceof DocumentFragment){var n=U(t);if(t=m.call(this,t,e),V(this))for(e=0;e<n.length;e++)z(a,n[e]);return t}return n=t instanceof Element&&V(t),e=m.call(this,t,e),n&&X(a,t),V(this)&&z(a,t),e},Node.prototype.appendChild=function(t){if(t instanceof DocumentFragment){var e=U(t);if(t=y.call(this,t),V(this))for(var n=0;n<e.length;n++)z(a,e[n]);return t}return e=t instanceof Element&&V(t),n=y.call(this,t),e&&X(a,t),V(this)&&z(a,t),n},Node.prototype.cloneNode=function(t){return t=_.call(this,!!t),this.ownerDocument.__CE_registry?Y(a,t):W(a,t),t},Node.prototype.removeChild=function(t){var e=t instanceof Element&&V(t),n=v.call(this,t);return e&&X(a,t),n},Node.prototype.replaceChild=function(t,e){if(t instanceof DocumentFragment){var n=U(t);if(t=b.call(this,t,e),V(this))for(X(a,e),e=0;e<n.length;e++)z(a,n[e]);return t}n=t instanceof Element&&V(t);var i=b.call(this,t,e),r=V(this);return r&&X(a,e),n&&X(a,t),r&&z(a,t),i},E&&E.get?s(Node.prototype,E):(e=function(t){s(t,{enumerable:!0,configurable:!0,get:function(){for(var t=[],e=this.firstChild;e;e=e.nextSibling)e.nodeType!==Node.COMMENT_NODE&&t.push(e.textContent);return t.join("")},set:function(t){for(;this.firstChild;)v.call(this,this.firstChild);null!=t&&""!==t&&y.call(this,document.createTextNode(t))}})},(t=a).f=!0,t.h.push(e)),ot(i),i=new J(i),document.__CE_registry=i,Object.defineProperty(window,"customElements",{configurable:!0,enumerable:!0,value:i})}st&&!st.forcePolyfill&&"function"==typeof st.define&&"function"==typeof st.get||t(),window.__CE_installPolyfill=t}).call(self)},function(t,e,n){t.exports=n(33)("native-function-to-string",Function.toString)},function(t,e,n){var s=n(28),c=n(57),u=n(43);t.exports=function(t){var e=s(t),n=c.f;if(n)for(var i,r=n(t),o=u.f,a=0;r.length>a;)o.call(t,i=r[a++])&&e.push(i);return e}},function(t,e,n){var i=n(29);t.exports=Object("z").propertyIsEnumerable(0)?Object:function(t){return"String"==i(t)?t.split(""):Object(t)}},function(t,e,n){var i=n(40),r=Math.max,o=Math.min;t.exports=function(t,e){return(t=i(t))<0?r(t+e,0):o(t,e)}},function(t,e,n){var i=n(29);t.exports=Array.isArray||function(t){return"Array"==i(t)}},function(t,e,n){var a=n(12),s=n(13),c=n(28);t.exports=n(15)?Object.defineProperties:function(t,e){s(t);for(var n,i=c(e),r=i.length,o=0;o<r;)a.f(t,n=i[o++],e[n]);return t}},function(t,e,n){var i=n(11).document;t.exports=i&&i.documentElement},function(t,e,n){var i=n(22),r=n(44).f,o={}.toString,a="object"==typeof window&&window&&Object.getOwnPropertyNames?Object.getOwnPropertyNames(window):[];t.exports.f=function(t){return a&&"[object Window]"==o.call(t)?function(t){try{return r(t)}catch(t){return a.slice()}}(t):r(i(t))}},function(t,e,n){"use strict";var i=n(30),r=n(26),o=n(36),a={};n(20)(a,n(10)("iterator"),function(){return this}),t.exports=function(t,e,n){t.prototype=i(a,{next:r(1,n)}),o(t,e+" Iterator")}},function(t,e,n){var i=n(19),r=n(38),o=n(41)("IE_PROTO"),a=Object.prototype;t.exports=Object.getPrototypeOf||function(t){return t=r(t),i(t,o)?t[o]:"function"==typeof t.constructor&&t instanceof t.constructor?t.constructor.prototype:t instanceof Object?a:null}},function(t,e,n){"use strict";var i=n(11),r=n(12),o=n(15),a=n(10)("species");t.exports=function(t){var e=i[t];o&&e&&!e[a]&&r.f(e,a,{configurable:!0,get:function(){return this}})}},function(t,e,n){"use strict";var o=n(35),a=n(14),s=n(90),c=[].slice,u={};t.exports=Function.bind||function(e){var n=o(this),i=c.call(arguments,1),r=function(){var t=i.concat(c.call(arguments));return this instanceof r?function(t,e,n){if(!(e in u)){for(var i=[],r=0;r<e;r++)i[r]="a["+r+"]";u[e]=Function("F,a","return new F("+i.join(",")+")")}return u[e](t,n)}(n,t.length,t):s(n,t,e)};return a(n.prototype)&&(r.prototype=n.prototype),r}},function(t,e){t.exports=function(t,e,n){var i=void 0===n;switch(e.length){case 0:return i?t():t.call(n);case 1:return i?t(e[0]):t.call(n,e[0]);case 2:return i?t(e[0],e[1]):t.call(n,e[0],e[1]);case 3:return i?t(e[0],e[1],e[2]):t.call(n,e[0],e[1],e[2]);case 4:return i?t(e[0],e[1],e[2],e[3]):t.call(n,e[0],e[1],e[2],e[3])}return t.apply(n,e)}},function(t,e,n){n(15)&&"g"!=/./g.flags&&n(12).f(RegExp.prototype,"flags",{configurable:!0,get:n(49)})},function(t,e,n){var r=n(17),o=n(21),a=n(16);t.exports=function(t,e){var n=(o.Object||{})[t]||Object[t],i={};i[t]=e(n),r(r.S+r.F*a(function(){n(1)}),"Object",i)}},function(t,e,n){"use strict";var i=n(12),r=n(26);t.exports=function(t,e,n){e in t?i.f(t,e,r(0,n)):t[e]=n}},function(t,e,n){"use strict";var i=n(17),r=n(56)(!0);i(i.P,"Array",{includes:function(t,e){return r(this,t,1<arguments.length?e:void 0)}}),n(58)("includes")},function(t,e,n){"use strict";var i=n(17),r=n(96),o="includes";i(i.P+i.F*n(97)(o),"String",{includes:function(t,e){return!!~r(this,t,o).indexOf(t,1<arguments.length?e:void 0)}})},function(t,e,n){var i=n(75),r=n(23);t.exports=function(t,e,n){if(i(e))throw TypeError("String#"+n+" doesn't accept regex!");return String(r(t))}},function(t,e,n){var i=n(10)("match");t.exports=function(e){var n=/./;try{"/./"[e](n)}catch(t){try{return n[i]=!1,!"/./"[e](n)}catch(t){}}return!0}},function(t,e,n){"use strict";var f=n(75),b=n(13),E=n(99),g=n(100),C=n(37),A=n(101),d=n(50),i=n(16),T=Math.min,p=[].push,a="split",h="length",_="lastIndex",I=4294967295,S=!i(function(){RegExp(I,"y")});n(102)("split",2,function(r,o,y,m){var v;return v="c"=="abbc"[a](/(b)*/)[1]||4!="test"[a](/(?:)/,-1)[h]||2!="ab"[a](/(?:ab)*/)[h]||4!="."[a](/(.?)(.?)/)[h]||1<"."[a](/()()/)[h]||""[a](/.?/)[h]?function(t,e){var n=String(this);if(void 0===t&&0===e)return[];if(!f(t))return y.call(n,t,e);for(var i,r,o,a=[],s=(t.ignoreCase?"i":"")+(t.multiline?"m":"")+(t.unicode?"u":"")+(t.sticky?"y":""),c=0,u=void 0===e?I:e>>>0,l=new RegExp(t.source,s+"g");(i=d.call(l,n))&&!(c<(r=l[_])&&(a.push(n.slice(c,i.index)),1<i[h]&&i.index<n[h]&&p.apply(a,i.slice(1)),o=i[0][h],c=r,a[h]>=u));)l[_]===i.index&&l[_]++;return c===n[h]?!o&&l.test("")||a.push(""):a.push(n.slice(c)),a[h]>u?a.slice(0,u):a}:"0"[a](void 0,0)[h]?function(t,e){return void 0===t&&0===e?[]:y.call(this,t,e)}:y,[function(t,e){var n=r(this),i=null==t?void 0:t[o];return void 0!==i?i.call(t,n,e):v.call(String(n),t,e)},function(t,e){var n=m(v,t,this,e,v!==y);if(n.done)return n.value;var i=b(t),r=String(this),o=E(i,RegExp),a=i.unicode,s=(i.ignoreCase?"i":"")+(i.multiline?"m":"")+(i.unicode?"u":"")+(S?"y":"g"),c=new o(S?i:"^(?:"+i.source+")",s),u=void 0===e?I:e>>>0;if(0==u)return[];if(0===r.length)return null===A(c,r)?[r]:[];for(var l=0,f=0,d=[];f<r.length;){c.lastIndex=S?f:0;var p,h=A(c,S?r:r.slice(f));if(null===h||(p=T(C(c.lastIndex+(S?0:f)),r.length))===l)f=g(r,f,a);else{if(d.push(r.slice(l,f)),d.length===u)return d;for(var _=1;_<=h.length-1;_++)if(d.push(h[_]),d.length===u)return d;f=l=p}}return d.push(r.slice(l)),d}]})},function(t,e,n){var r=n(13),o=n(35),a=n(10)("species");t.exports=function(t,e){var n,i=r(t).constructor;return void 0===i||null==(n=r(i)[a])?e:o(n)}},function(t,e,n){"use strict";var i=n(60)(!0);t.exports=function(t,e,n){return e+(n?i(t,e).length:1)}},function(t,e,n){"use strict";var r=n(47),o=RegExp.prototype.exec;t.exports=function(t,e){var n=t.exec;if("function"==typeof n){var i=n.call(t,e);if("object"!=typeof i)throw new TypeError("RegExp exec method returned something other than an Object or null");return i}if("RegExp"!==r(t))throw new TypeError("RegExp#exec called on incompatible receiver");return o.call(t,e)}},function(t,e,n){"use strict";n(103);var l=n(18),f=n(20),d=n(16),p=n(23),h=n(10),_=n(50),y=h("species"),m=!d(function(){var t=/./;return t.exec=function(){var t=[];return t.groups={a:"7"},t},"7"!=="".replace(t,"$<a>")}),v=function(){var t=/(?:)/,e=t.exec;t.exec=function(){return e.apply(this,arguments)};var n="ab".split(t);return 2===n.length&&"a"===n[0]&&"b"===n[1]}();t.exports=function(n,t,e){var i=h(n),o=!d(function(){var t={};return t[i]=function(){return 7},7!=""[n](t)}),r=o?!d(function(){var t=!1,e=/a/;return e.exec=function(){return t=!0,null},"split"===n&&(e.constructor={},e.constructor[y]=function(){return e}),e[i](""),!t}):void 0;if(!o||!r||"replace"===n&&!m||"split"===n&&!v){var a=/./[i],s=e(p,i,""[n],function(t,e,n,i,r){return e.exec===_?o&&!r?{done:!0,value:a.call(e,n,i)}:{done:!0,value:t.call(n,e,i)}:{done:!1}}),c=s[0],u=s[1];l(String.prototype,n,c),f(RegExp.prototype,i,2==t?function(t,e){return u.call(t,this,e)}:function(t){return u.call(t,this)})}}},function(t,e,n){"use strict";var i=n(50);n(17)({target:"RegExp",proto:!0,forced:i!==/./.exec},{exec:i})},function(t,e,n){"use strict";function i(t){var e=l(t,!1);if("string"==typeof e&&2<e.length){var n,i,r,o=(e=b?e.trim():p(e,3)).charCodeAt(0);if(43===o||45===o){if(88===(n=e.charCodeAt(2))||120===n)return NaN}else if(48===o){switch(e.charCodeAt(1)){case 66:case 98:i=2,r=49;break;case 79:case 111:i=8,r=55;break;default:return+e}for(var a,s=e.slice(2),c=0,u=s.length;c<u;c++)if((a=s.charCodeAt(c))<48||r<a)return NaN;return parseInt(s,i)}}return+e}var r=n(11),o=n(19),a=n(29),s=n(70),l=n(34),c=n(16),u=n(44).f,f=n(45).f,d=n(12).f,p=n(105).trim,h="Number",_=r[h],y=_,m=_.prototype,v=a(n(30)(m))==h,b="trim"in String.prototype;if(!_(" 0o1")||!_("0b1")||_("+0x1")){_=function(t){var e=arguments.length<1?0:t,n=this;return n instanceof _&&(v?c(function(){m.valueOf.call(n)}):a(n)!=h)?s(new y(i(e)),n,_):i(e)};for(var E,g=n(15)?u(y):"MAX_VALUE,MIN_VALUE,NaN,NEGATIVE_INFINITY,POSITIVE_INFINITY,EPSILON,isFinite,isInteger,isNaN,isSafeInteger,MAX_SAFE_INTEGER,MIN_SAFE_INTEGER,parseFloat,parseInt,isInteger".split(","),C=0;g.length>C;C++)o(y,E=g[C])&&!o(_,E)&&d(_,E,f(y,E));(_.prototype=m).constructor=_,n(18)(r,h,_)}},function(t,e,n){function i(t,e,n){var i={},r=s(function(){return!!c[t]()||"​"!="​"[t]()}),o=i[t]=r?e(f):c[t];n&&(i[n]=o),a(a.P+a.F*r,"String",i)}var a=n(17),r=n(23),s=n(16),c=n(106),o="["+c+"]",u=RegExp("^"+o+o+"*"),l=RegExp(o+o+"*$"),f=i.trim=function(t,e){return t=String(r(t)),1&e&&(t=t.replace(u,"")),2&e&&(t=t.replace(l,"")),t};t.exports=i},function(t,e){t.exports="\t\n\v\f\r   ᠎             　\u2028\u2029\ufeff"},function(t,e){var n=["input","select","textarea","a[href]","button","[tabindex]","audio[controls]","video[controls]",'[contenteditable]:not([contenteditable="false"])'],c=n.join(","),u="undefined"==typeof Element?function(){}:Element.prototype.matches||Element.prototype.msMatchesSelector||Element.prototype.webkitMatchesSelector;function i(t,e){e=e||{};var n,i,r,o=[],a=[],s=t.querySelectorAll(c);for(e.includeContainer&&u.call(t,c)&&(s=Array.prototype.slice.apply(s)).unshift(t),n=0;n<s.length;n++)l(i=s[n])&&(0===(r=f(i))?o.push(i):a.push({documentOrder:n,tabIndex:r,node:i}));return a.sort(d).map(function(t){return t.node}).concat(o)}function l(t){return!(!r(t)||function(t){return a(t)&&"radio"===t.type}(e=t)&&!function(t){if(!t.name)return 1;var e=function(t){for(var e=0;e<t.length;e++)if(t[e].checked)return t[e]}(t.ownerDocument.querySelectorAll('input[type="radio"][name="'+t.name+'"]'));return!e||e===t}(e)||f(t)<0);var e}function r(t){return!t.disabled&&(!a(n=t)||"hidden"!==n.type)&&(null!==(e=t).offsetParent&&"hidden"!==getComputedStyle(e).visibility);var e,n}i.isTabbable=function(t){if(!t)throw new Error("No node provided");return!1!==u.call(t,c)&&l(t)},i.isFocusable=function(t){if(!t)throw new Error("No node provided");return!1!==u.call(t,o)&&r(t)};var o=n.concat("iframe").join(",");function f(t){var e=parseInt(t.getAttribute("tabindex"),10);return isNaN(e)?"true"===t.contentEditable?0:t.tabIndex:e}function d(t,e){return t.tabIndex===e.tabIndex?t.documentOrder-e.documentOrder:t.tabIndex-e.tabIndex}function a(t){return"INPUT"===t.tagName}t.exports=i},function(t,e){t.exports=function(){for(var t={},e=0;e<arguments.length;e++){var n=arguments[e];for(var i in n)r.call(n,i)&&(t[i]=n[i])}return t};var r=Object.prototype.hasOwnProperty},function(t,e,n){"use strict";n.r(e);n(77),n(5),n(6),n(1),n(2),n(4),n(7),n(8),n(3),n(0),n(9);var i=function(t,e){return(i=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(t,e){t.__proto__=e}||function(t,e){for(var n in e)e.hasOwnProperty(n)&&(t[n]=e[n])})(t,e)};function r(t,e){function n(){this.constructor=t}i(t,e),t.prototype=null===e?Object.create(e):(n.prototype=e.prototype,new n)}var a=function(){return(a=Object.assign||function(t){for(var e,n=1,i=arguments.length;n<i;n++)for(var r in e=arguments[n])Object.prototype.hasOwnProperty.call(e,r)&&(t[r]=e[r]);return t}).apply(this,arguments)};function f(t){var e="function"==typeof Symbol&&t[Symbol.iterator],n=0;return e?e.call(t):{next:function(){return t&&n>=t.length&&(t=void 0),{value:t&&t[n++],done:!t}}}}function o(t,e){var n="function"==typeof Symbol&&t[Symbol.iterator];if(!n)return t;var i,r,o=n.call(t),a=[];try{for(;(void 0===e||0<e--)&&!(i=o.next()).done;)a.push(i.value)}catch(t){r={error:t}}finally{try{i&&!i.done&&(n=o.return)&&n.call(o)}finally{if(r)throw r.error}}return a}
var s=(Object.defineProperty(c,"cssClasses",{get:function(){return{}},enumerable:!0,configurable:!0}),Object.defineProperty(c,"strings",{get:function(){return{}},enumerable:!0,configurable:!0}),Object.defineProperty(c,"numbers",{get:function(){return{}},enumerable:!0,configurable:!0}),Object.defineProperty(c,"defaultAdapter",{get:function(){return{}},enumerable:!0,configurable:!0}),c.prototype.init=function(){},c.prototype.destroy=function(){},c);function c(t){void 0===t&&(t={}),this.adapter_=t}var u=(l.attachTo=function(t){return new l(t,new s({}))},l.prototype.initialize=function(){for(var t=[],e=0;e<arguments.length;e++)t[e]=arguments[e]},l.prototype.getDefaultFoundation=function(){throw new Error("Subclasses must override getDefaultFoundation to return a properly configured foundation class")},l.prototype.initialSyncWithDOM=function(){},l.prototype.destroy=function(){this.foundation_.destroy()},l.prototype.listen=function(t,e,n){this.root_.addEventListener(t,e,n)},l.prototype.unlisten=function(t,e,n){this.root_.removeEventListener(t,e,n)},l.prototype.emit=function(t,e,n){var i;void 0===n&&(n=!1),"function"==typeof CustomEvent?i=new CustomEvent(t,{bubbles:n,detail:e}):(i=document.createEvent("CustomEvent")).initCustomEvent(t,n,!1,e),this.root_.dispatchEvent(i)},l);
   function l(t,e){for(var n=[],i=2;i<arguments.length;i++)n[i-2]=arguments[i];this.root_=t,this.initialize.apply(this,function(){for(var t=[],e=0;e<arguments.length;e++)t=t.concat(o(arguments[e]));return t}(n)),this.foundation_=void 0===e?this.getDefaultFoundation():e,this.foundation_.init(),this.initialSyncWithDOM()}
function d(t){return void 0===t&&(t=window),!!function(t){void 0===t&&(t=window);var e=!1;try{var n={get passive(){return!(e=!0)}},i=function(){};t.document.addEventListener("test",i,n),t.document.removeEventListener("test",i,n)}catch(t){e=!1}return e}
   (t)&&{passive:!0}}function p(t,e){if(t.closest)return t.closest(e);for(var n=t;n;){if(h(n,e))return n;n=n.parentElement}return null}function h(t,e){return(t.matches||t.webkitMatchesSelector||t.msMatchesSelector).call(t,e)}n(72);
   var _,y={BG_FOCUSED:"mdc-ripple-upgraded--background-focused",FG_ACTIVATION:"mdc-ripple-upgraded--foreground-activation",FG_DEACTIVATION:"mdc-ripple-upgraded--foreground-deactivation",ROOT:"mdc-ripple-upgraded",UNBOUNDED:"mdc-ripple-upgraded--unbounded"},m={VAR_FG_SCALE:"--mdc-ripple-fg-scale",VAR_FG_SIZE:"--mdc-ripple-fg-size",VAR_FG_TRANSLATE_END:"--mdc-ripple-fg-translate-end",VAR_FG_TRANSLATE_START:"--mdc-ripple-fg-translate-start",VAR_LEFT:"--mdc-ripple-left",VAR_TOP:"--mdc-ripple-top"},v={DEACTIVATION_TIMEOUT_MS:225,FG_DEACTIVATION_MS:150,INITIAL_ORIGIN_SCALE:.6,PADDING:10,TAP_DELAY_MS:300};function b(t,e){void 0===e&&(e=!1);var n,i=t.CSS;if("boolean"==typeof _&&!e)return _;if(!(i&&"function"==typeof i.supports))return!1;var r=i.supports("--css-vars","yes"),o=i.supports("(--css-vars: yes)")&&i.supports("color","#00000000");return n=r||o,e||(_=n),n}function E(t,e,n){if(!t)return{x:0,y:0};var i,r,o=e.x,a=e.y,s=o+n.left,c=a+n.top;if("touchstart"===t.type){var u=t;i=u.changedTouches[0].pageX-s,r=u.changedTouches[0].pageY-c}else{var l=t;i=l.pageX-s,r=l.pageY-c}return{x:i,y:r}}
   var g,C=["touchstart","pointerdown","mousedown","keydown"],A=["touchend","pointerup","mouseup","contextmenu"],T=[],I=(r(S,g=s),Object.defineProperty(S,"cssClasses",{get:function(){return y},enumerable:!1,configurable:!0}),Object.defineProperty(S,"strings",{get:function(){return m},enumerable:!1,configurable:!0}),Object.defineProperty(S,"numbers",{get:function(){return v},enumerable:!1,configurable:!0}),Object.defineProperty(S,"defaultAdapter",{get:function(){return{addClass:function(){},browserSupportsCssVars:function(){return!0},computeBoundingRect:function(){return{top:0,right:0,bottom:0,left:0,width:0,height:0}},containsEventTarget:function(){return!0},deregisterDocumentInteractionHandler:function(){},deregisterInteractionHandler:function(){},deregisterResizeHandler:function(){},getWindowPageOffset:function(){return{x:0,y:0}},isSurfaceActive:function(){return!0},isSurfaceDisabled:function(){return!0},isUnbounded:function(){return!0},registerDocumentInteractionHandler:function(){},registerInteractionHandler:function(){},registerResizeHandler:function(){},removeClass:function(){},updateCssVariable:function(){}}},enumerable:!1,configurable:!0}),S.prototype.init=function(){var t=this,e=this.supportsPressRipple_();if(this.registerRootHandlers_(e),e){var n=S.cssClasses,i=n.ROOT,r=n.UNBOUNDED;requestAnimationFrame(function(){t.adapter_.addClass(i),t.adapter_.isUnbounded()&&(t.adapter_.addClass(r),t.layoutInternal_())})}},S.prototype.destroy=function(){var t=this;if(this.supportsPressRipple_()){this.activationTimer_&&(clearTimeout(this.activationTimer_),this.activationTimer_=0,this.adapter_.removeClass(S.cssClasses.FG_ACTIVATION)),this.fgDeactivationRemovalTimer_&&(clearTimeout(this.fgDeactivationRemovalTimer_),this.fgDeactivationRemovalTimer_=0,this.adapter_.removeClass(S.cssClasses.FG_DEACTIVATION));var e=S.cssClasses,n=e.ROOT,i=e.UNBOUNDED;requestAnimationFrame(function(){t.adapter_.removeClass(n),t.adapter_.removeClass(i),t.removeCssVars_()})}this.deregisterRootHandlers_(),this.deregisterDeactivationHandlers_()},S.prototype.activate=function(t){this.activate_(t)},S.prototype.deactivate=function(){this.deactivate_()},S.prototype.layout=function(){var t=this;this.layoutFrame_&&cancelAnimationFrame(this.layoutFrame_),this.layoutFrame_=requestAnimationFrame(function(){t.layoutInternal_(),t.layoutFrame_=0})},S.prototype.setUnbounded=function(t){var e=S.cssClasses.UNBOUNDED;t?this.adapter_.addClass(e):this.adapter_.removeClass(e)},S.prototype.handleFocus=function(){var t=this;requestAnimationFrame(function(){return t.adapter_.addClass(S.cssClasses.BG_FOCUSED)})},S.prototype.handleBlur=function(){var t=this;requestAnimationFrame(function(){return t.adapter_.removeClass(S.cssClasses.BG_FOCUSED)})},S.prototype.supportsPressRipple_=function(){return this.adapter_.browserSupportsCssVars()},S.prototype.defaultActivationState_=function(){return{activationEvent:void 0,hasDeactivationUXRun:!1,isActivated:!1,isProgrammatic:!1,wasActivatedByPointer:!1,wasElementMadeActive:!1}},S.prototype.registerRootHandlers_=function(t){var e=this;t&&(C.forEach(function(t){e.adapter_.registerInteractionHandler(t,e.activateHandler_)}),this.adapter_.isUnbounded()&&this.adapter_.registerResizeHandler(this.resizeHandler_)),this.adapter_.registerInteractionHandler("focus",this.focusHandler_),this.adapter_.registerInteractionHandler("blur",this.blurHandler_)},S.prototype.registerDeactivationHandlers_=function(t){var e=this;"keydown"===t.type?this.adapter_.registerInteractionHandler("keyup",this.deactivateHandler_):A.forEach(function(t){e.adapter_.registerDocumentInteractionHandler(t,e.deactivateHandler_)})},S.prototype.deregisterRootHandlers_=function(){var e=this;C.forEach(function(t){e.adapter_.deregisterInteractionHandler(t,e.activateHandler_)}),this.adapter_.deregisterInteractionHandler("focus",this.focusHandler_),this.adapter_.deregisterInteractionHandler("blur",this.blurHandler_),this.adapter_.isUnbounded()&&this.adapter_.deregisterResizeHandler(this.resizeHandler_)},S.prototype.deregisterDeactivationHandlers_=function(){var e=this;this.adapter_.deregisterInteractionHandler("keyup",this.deactivateHandler_),A.forEach(function(t){e.adapter_.deregisterDocumentInteractionHandler(t,e.deactivateHandler_)})},S.prototype.removeCssVars_=function(){var e=this,n=S.strings;Object.keys(n).forEach(function(t){0===t.indexOf("VAR_")&&e.adapter_.updateCssVariable(n[t],null)})},S.prototype.activate_=function(t){var e=this;if(!this.adapter_.isSurfaceDisabled()){var n=this.activationState_;if(!n.isActivated){var i=this.previousActivationEvent_;i&&void 0!==t&&i.type!==t.type||(n.isActivated=!0,n.isProgrammatic=void 0===t,n.activationEvent=t,n.wasActivatedByPointer=!n.isProgrammatic&&void 0!==t&&("mousedown"===t.type||"touchstart"===t.type||"pointerdown"===t.type),void 0!==t&&0<T.length&&T.some(function(t){return e.adapter_.containsEventTarget(t)})?this.resetActivationState_():(void 0!==t&&(T.push(t.target),this.registerDeactivationHandlers_(t)),n.wasElementMadeActive=this.checkElementMadeActive_(t),n.wasElementMadeActive&&this.animateActivation_(),requestAnimationFrame(function(){T=[],n.wasElementMadeActive||void 0===t||" "!==t.key&&32!==t.keyCode||(n.wasElementMadeActive=e.checkElementMadeActive_(t),n.wasElementMadeActive&&e.animateActivation_()),n.wasElementMadeActive||(e.activationState_=e.defaultActivationState_())})))}}},S.prototype.checkElementMadeActive_=function(t){return void 0===t||"keydown"!==t.type||this.adapter_.isSurfaceActive()},S.prototype.animateActivation_=function(){var t=this,e=S.strings,n=e.VAR_FG_TRANSLATE_START,i=e.VAR_FG_TRANSLATE_END,r=S.cssClasses,o=r.FG_DEACTIVATION,a=r.FG_ACTIVATION,s=S.numbers.DEACTIVATION_TIMEOUT_MS;this.layoutInternal_();var c="",u="";if(!this.adapter_.isUnbounded()){var l=this.getFgTranslationCoordinates_(),f=l.startPoint,d=l.endPoint;c=f.x+"px, "+f.y+"px",u=d.x+"px, "+d.y+"px"}this.adapter_.updateCssVariable(n,c),this.adapter_.updateCssVariable(i,u),clearTimeout(this.activationTimer_),clearTimeout(this.fgDeactivationRemovalTimer_),this.rmBoundedActivationClasses_(),this.adapter_.removeClass(o),this.adapter_.computeBoundingRect(),this.adapter_.addClass(a),this.activationTimer_=setTimeout(function(){return t.activationTimerCallback_()},s)},S.prototype.getFgTranslationCoordinates_=function(){var t,e=this.activationState_,n=e.activationEvent;return{startPoint:t={x:(t=e.wasActivatedByPointer?E(n,this.adapter_.getWindowPageOffset(),this.adapter_.computeBoundingRect()):{x:this.frame_.width/2,y:this.frame_.height/2}).x-this.initialSize_/2,y:t.y-this.initialSize_/2},endPoint:{x:this.frame_.width/2-this.initialSize_/2,y:this.frame_.height/2-this.initialSize_/2}}},S.prototype.runDeactivationUXLogicIfReady_=function(){var t=this,e=S.cssClasses.FG_DEACTIVATION,n=this.activationState_,i=n.hasDeactivationUXRun,r=n.isActivated;!i&&r||!this.activationAnimationHasEnded_||(this.rmBoundedActivationClasses_(),this.adapter_.addClass(e),this.fgDeactivationRemovalTimer_=setTimeout(function(){t.adapter_.removeClass(e)},v.FG_DEACTIVATION_MS))},S.prototype.rmBoundedActivationClasses_=function(){var t=S.cssClasses.FG_ACTIVATION;this.adapter_.removeClass(t),this.activationAnimationHasEnded_=!1,this.adapter_.computeBoundingRect()},S.prototype.resetActivationState_=function(){var t=this;this.previousActivationEvent_=this.activationState_.activationEvent,this.activationState_=this.defaultActivationState_(),setTimeout(function(){return t.previousActivationEvent_=void 0},S.numbers.TAP_DELAY_MS)},S.prototype.deactivate_=function(){var t=this,e=this.activationState_;if(e.isActivated){var n=a({},e);e.isProgrammatic?(requestAnimationFrame(function(){return t.animateDeactivation_(n)}),this.resetActivationState_()):(this.deregisterDeactivationHandlers_(),requestAnimationFrame(function(){t.activationState_.hasDeactivationUXRun=!0,t.animateDeactivation_(n),t.resetActivationState_()}))}},S.prototype.animateDeactivation_=function(t){var e=t.wasActivatedByPointer,n=t.wasElementMadeActive;(e||n)&&this.runDeactivationUXLogicIfReady_()},S.prototype.layoutInternal_=function(){var t=this;this.frame_=this.adapter_.computeBoundingRect();var e=Math.max(this.frame_.height,this.frame_.width);this.maxRadius_=this.adapter_.isUnbounded()?e:Math.sqrt(Math.pow(t.frame_.width,2)+Math.pow(t.frame_.height,2))+S.numbers.PADDING;var n=Math.floor(e*S.numbers.INITIAL_ORIGIN_SCALE);this.adapter_.isUnbounded()&&n%2!=0?this.initialSize_=n-1:this.initialSize_=n,this.fgScale_=""+this.maxRadius_/this.initialSize_,this.updateLayoutCssVars_()},S.prototype.updateLayoutCssVars_=function(){var t=S.strings,e=t.VAR_FG_SIZE,n=t.VAR_LEFT,i=t.VAR_TOP,r=t.VAR_FG_SCALE;this.adapter_.updateCssVariable(e,this.initialSize_+"px"),this.adapter_.updateCssVariable(r,this.fgScale_),this.adapter_.isUnbounded()&&(this.unboundedCoords_={left:Math.round(this.frame_.width/2-this.initialSize_/2),top:Math.round(this.frame_.height/2-this.initialSize_/2)},this.adapter_.updateCssVariable(n,this.unboundedCoords_.left+"px"),this.adapter_.updateCssVariable(i,this.unboundedCoords_.top+"px"))},S);function S(t){var e=g.call(this,a(a({},S.defaultAdapter),t))||this;return e.activationAnimationHasEnded_=!1,e.activationTimer_=0,e.fgDeactivationRemovalTimer_=0,e.fgScale_="0",e.frame_={width:0,height:0},e.initialSize_=0,e.layoutFrame_=0,e.maxRadius_=0,e.unboundedCoords_={left:0,top:0},e.activationState_=e.defaultActivationState_(),e.activationTimerCallback_=function(){e.activationAnimationHasEnded_=!0,e.runDeactivationUXLogicIfReady_()},e.activateHandler_=function(t){return e.activate_(t)},e.deactivateHandler_=function(){return e.deactivate_()},e.focusHandler_=function(){return e.handleFocus()},e.blurHandler_=function(){return e.handleBlur()},e.resizeHandler_=function(){return e.layout()},e}var O,R=(r(L,O=u),L.attachTo=function(t,e){void 0===e&&(e={isUnbounded:void 0});var n=new L(t);return void 0!==e.isUnbounded&&(n.unbounded=e.isUnbounded),n},L.createAdapter=function(n){return{addClass:function(t){return n.root_.classList.add(t)},browserSupportsCssVars:function(){return b(window)},computeBoundingRect:function(){return n.root_.getBoundingClientRect()},containsEventTarget:function(t){return n.root_.contains(t)},deregisterDocumentInteractionHandler:function(t,e){return document.documentElement.removeEventListener(t,e,d())},deregisterInteractionHandler:function(t,e){return n.root_.removeEventListener(t,e,d())},deregisterResizeHandler:function(t){return window.removeEventListener("resize",t)},getWindowPageOffset:function(){return{x:window.pageXOffset,y:window.pageYOffset}},isSurfaceActive:function(){return h(n.root_,":active")},isSurfaceDisabled:function(){return Boolean(n.disabled)},isUnbounded:function(){return Boolean(n.unbounded)},registerDocumentInteractionHandler:function(t,e){return document.documentElement.addEventListener(t,e,d())},registerInteractionHandler:function(t,e){return n.root_.addEventListener(t,e,d())},registerResizeHandler:function(t){return window.addEventListener("resize",t)},removeClass:function(t){return n.root_.classList.remove(t)},updateCssVariable:function(t,e){return n.root_.style.setProperty(t,e)}}},Object.defineProperty(L.prototype,"unbounded",{get:function(){return Boolean(this.unbounded_)},set:function(t){this.unbounded_=Boolean(t),this.setUnbounded_()},enumerable:!1,configurable:!0}),L.prototype.activate=function(){this.foundation_.activate()},L.prototype.deactivate=function(){this.foundation_.deactivate()},L.prototype.layout=function(){this.foundation_.layout()},L.prototype.getDefaultFoundation=function(){return new I(L.createAdapter(this))},L.prototype.initialSyncWithDOM=function(){var t=this.root_;this.unbounded="mdcRippleIsUnbounded"in t.dataset},L.prototype.setUnbounded_=function(){this.foundation_.setUnbounded(Boolean(this.unbounded_))},L);
   function L(){var t=null!==O&&O.apply(this,arguments)||this;return t.disabled=!1,t}n(73),n(74),n(94),n(95),n(98);function w(t){return function(t){if(Array.isArray(t))return D(t)}(t)||function(t){if("undefined"!=typeof Symbol&&Symbol.iterator in Object(t))return Array.from(t)}(t)||N(t)||function(){throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}()}function x(t,e){var n;if("undefined"==typeof Symbol||null==t[Symbol.iterator]){if(Array.isArray(t)||(n=N(t))||e&&t&&"number"==typeof t.length){n&&(t=n);var i=0,r=function(){};return{s:r,n:function(){return i>=t.length?{done:!0}:{done:!1,value:t[i++]}},e:function(t){throw t},f:r}}throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}var o,a=!0,s=!1;return{s:function(){n=t[Symbol.iterator]()},n:function(){var t=n.next();return a=t.done,t},e:function(t){s=!0,o=t},f:function(){try{a||null==n.return||n.return()}finally{if(s)throw o}}}}function N(t,e){if(t){if("string"==typeof t)return D(t,e);var n=Object.prototype.toString.call(t).slice(8,-1);return"Object"===n&&t.constructor&&(n=t.constructor.name),"Map"===n||"Set"===n?Array.from(t):"Arguments"===n||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)?D(t,e):void 0}}function D(t,e){(null==e||e>t.length)&&(e=t.length);for(var n=0,i=new Array(e);n<e;n++)i[n]=t[n];return i}function P(){return"".concat(this.classList)}function k(){Object.defineProperty(this,"className",{get:P.bind(this),set:function(t){var e,n=t.split(" ").filter(function(t){return""!==t}),i=x(n);try{for(i.s();!(e=i.n()).done;){var r=e.value;this.classList.add(r)}}catch(t){i.e(t)}finally{i.f()}var o,a=x(this.className_);try{for(a.s();!(o=a.n()).done;){var s=o.value;if(this.foucClassNames&&this.foucClassNames.includes(s))return;n.includes(s)||this.classList.remove(s)}}catch(t){a.e(t)}finally{a.f()}this.className_=n}.bind(this),configurable:!0}),this.className_=w(this.classList)}function F(){for(var t,e=Object.getPrototypeOf(this);!(t=Object.getOwnPropertyDescriptor(e,"className"));)e=Object.getPrototypeOf(e);Object.defineProperty(this,"className",t),delete this.className_,this.className=P.call(this)}function M(t){return(M="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function H(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function j(t,e,n){return e&&H(t.prototype,e),n&&H(t,n),t}function B(o){var a=K();return function(){var t,e,n,i=W(o);if(a){var r=W(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==M(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function V(t){var i="function"==typeof Map?new Map:void 0;return(V=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return U(t,arguments,W(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),G(n,t)})(t)}function U(t,e,n){return(U=K()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&G(r,n.prototype),r}).apply(null,arguments)}function K(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function G(t,e){return(G=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function W(t){return(W=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var q=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&G(t,e)}(n,V(HTMLElement));var e=B(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).ripple_,t}return j(n,[{key:"disabled",get:function(){return this.disabled_},set:function(t){this.disabled_=t}}]),j(n,[{key:"connectedCallback",value:function(){k.call(this),this.ripple_=new R(this.querySelector(".mdc-button"))}},{key:"disconnectedCallback",value:function(){this.ripple_.destroy(),F.call(this)}}]),n}();function z(t){return(z="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function X(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Y(t,e,n){return e&&X(t.prototype,e),n&&X(t,n),t}function Q(o){var a=J();return function(){var t,e,n,i=et(o);if(a){var r=et(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==z(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Z(t){var i="function"==typeof Map?new Map:void 0;return(Z=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return $(t,arguments,et(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),tt(n,t)})(t)}function $(t,e,n){return($=J()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&tt(r,n.prototype),r}).apply(null,arguments)}function J(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function tt(t,e){return(tt=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function et(t){return(et=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-button",q);var nt=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&tt(t,e)}(n,Z(HTMLElement));var e=Q(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).primaryAction_,t.ripple_,t}return Y(n,[{key:"focus",value:function(){this.primaryAction_&&this.primaryAction_.focus()}},{key:"blur",value:function(){this.primaryAction_&&this.primaryAction_.blur()}}]),Y(n,[{key:"connectedCallback",value:function(){k.call(this);var t=this.querySelector(".mdc-card__primary-action");t&&(this.primaryAction_=t),this.primaryAction_&&(this.ripple_=new R(this.primaryAction_))}},{key:"disconnectedCallback",value:function(){this.ripple_&&this.ripple_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-card",nt);
var it={animation:{prefixed:"-webkit-animation",standard:"animation"},transform:{prefixed:"-webkit-transform",standard:"transform"},transition:{prefixed:"-webkit-transition",standard:"transition"}},rt={animationend:{cssProperty:"animation",prefixed:"webkitAnimationEnd",standard:"animationend"},animationiteration:{cssProperty:"animation",prefixed:"webkitAnimationIteration",standard:"animationiteration"},animationstart:{cssProperty:"animation",prefixed:"webkitAnimationStart",standard:"animationstart"},transitionend:{cssProperty:"transition",prefixed:"webkitTransitionEnd",standard:"transitionend"}};function ot(t){return Boolean(t.document)&&"function"==typeof t.document.createElement}function at(t,e){if(ot(t)&&e in it){var n=t.document.createElement("div"),i=it[e],r=i.standard,o=i.prefixed;return r in n.style?r:o}return e}function st(t,e){if(ot(t)&&e in rt){var n=t.document.createElement("div"),i=rt[e],r=i.standard,o=i.prefixed;return i.cssProperty in n.style?r:o}return e}
   var ct,ut=["touchstart","pointerdown","mousedown","keydown"],lt=["touchend","pointerup","mouseup","contextmenu"],ft=[],dt=(r(pt,ct=s),Object.defineProperty(pt,"cssClasses",{get:function(){return y},enumerable:!0,configurable:!0}),Object.defineProperty(pt,"strings",{get:function(){return m},enumerable:!0,configurable:!0}),Object.defineProperty(pt,"numbers",{get:function(){return v},enumerable:!0,configurable:!0}),Object.defineProperty(pt,"defaultAdapter",{get:function(){return{addClass:function(){},browserSupportsCssVars:function(){return!0},computeBoundingRect:function(){return{top:0,right:0,bottom:0,left:0,width:0,height:0}},containsEventTarget:function(){return!0},deregisterDocumentInteractionHandler:function(){},deregisterInteractionHandler:function(){},deregisterResizeHandler:function(){},getWindowPageOffset:function(){return{x:0,y:0}},isSurfaceActive:function(){return!0},isSurfaceDisabled:function(){return!0},isUnbounded:function(){return!0},registerDocumentInteractionHandler:function(){},registerInteractionHandler:function(){},registerResizeHandler:function(){},removeClass:function(){},updateCssVariable:function(){}}},enumerable:!0,configurable:!0}),pt.prototype.init=function(){var t=this,e=this.supportsPressRipple_();if(this.registerRootHandlers_(e),e){var n=pt.cssClasses,i=n.ROOT,r=n.UNBOUNDED;requestAnimationFrame(function(){t.adapter_.addClass(i),t.adapter_.isUnbounded()&&(t.adapter_.addClass(r),t.layoutInternal_())})}},pt.prototype.destroy=function(){var t=this;if(this.supportsPressRipple_()){this.activationTimer_&&(clearTimeout(this.activationTimer_),this.activationTimer_=0,this.adapter_.removeClass(pt.cssClasses.FG_ACTIVATION)),this.fgDeactivationRemovalTimer_&&(clearTimeout(this.fgDeactivationRemovalTimer_),this.fgDeactivationRemovalTimer_=0,this.adapter_.removeClass(pt.cssClasses.FG_DEACTIVATION));var e=pt.cssClasses,n=e.ROOT,i=e.UNBOUNDED;requestAnimationFrame(function(){t.adapter_.removeClass(n),t.adapter_.removeClass(i),t.removeCssVars_()})}this.deregisterRootHandlers_(),this.deregisterDeactivationHandlers_()},pt.prototype.activate=function(t){this.activate_(t)},pt.prototype.deactivate=function(){this.deactivate_()},pt.prototype.layout=function(){var t=this;this.layoutFrame_&&cancelAnimationFrame(this.layoutFrame_),this.layoutFrame_=requestAnimationFrame(function(){t.layoutInternal_(),t.layoutFrame_=0})},pt.prototype.setUnbounded=function(t){var e=pt.cssClasses.UNBOUNDED;t?this.adapter_.addClass(e):this.adapter_.removeClass(e)},pt.prototype.handleFocus=function(){var t=this;requestAnimationFrame(function(){return t.adapter_.addClass(pt.cssClasses.BG_FOCUSED)})},pt.prototype.handleBlur=function(){var t=this;requestAnimationFrame(function(){return t.adapter_.removeClass(pt.cssClasses.BG_FOCUSED)})},pt.prototype.supportsPressRipple_=function(){return this.adapter_.browserSupportsCssVars()},pt.prototype.defaultActivationState_=function(){return{activationEvent:void 0,hasDeactivationUXRun:!1,isActivated:!1,isProgrammatic:!1,wasActivatedByPointer:!1,wasElementMadeActive:!1}},pt.prototype.registerRootHandlers_=function(t){var e=this;t&&(ut.forEach(function(t){e.adapter_.registerInteractionHandler(t,e.activateHandler_)}),this.adapter_.isUnbounded()&&this.adapter_.registerResizeHandler(this.resizeHandler_)),this.adapter_.registerInteractionHandler("focus",this.focusHandler_),this.adapter_.registerInteractionHandler("blur",this.blurHandler_)},pt.prototype.registerDeactivationHandlers_=function(t){var e=this;"keydown"===t.type?this.adapter_.registerInteractionHandler("keyup",this.deactivateHandler_):lt.forEach(function(t){e.adapter_.registerDocumentInteractionHandler(t,e.deactivateHandler_)})},pt.prototype.deregisterRootHandlers_=function(){var e=this;ut.forEach(function(t){e.adapter_.deregisterInteractionHandler(t,e.activateHandler_)}),this.adapter_.deregisterInteractionHandler("focus",this.focusHandler_),this.adapter_.deregisterInteractionHandler("blur",this.blurHandler_),this.adapter_.isUnbounded()&&this.adapter_.deregisterResizeHandler(this.resizeHandler_)},pt.prototype.deregisterDeactivationHandlers_=function(){var e=this;this.adapter_.deregisterInteractionHandler("keyup",this.deactivateHandler_),lt.forEach(function(t){e.adapter_.deregisterDocumentInteractionHandler(t,e.deactivateHandler_)})},pt.prototype.removeCssVars_=function(){var e=this,n=pt.strings;Object.keys(n).forEach(function(t){0===t.indexOf("VAR_")&&e.adapter_.updateCssVariable(n[t],null)})},pt.prototype.activate_=function(t){var e=this;if(!this.adapter_.isSurfaceDisabled()){var n=this.activationState_;if(!n.isActivated){var i=this.previousActivationEvent_;i&&void 0!==t&&i.type!==t.type||(n.isActivated=!0,n.isProgrammatic=void 0===t,n.activationEvent=t,n.wasActivatedByPointer=!n.isProgrammatic&&void 0!==t&&("mousedown"===t.type||"touchstart"===t.type||"pointerdown"===t.type),void 0!==t&&0<ft.length&&ft.some(function(t){return e.adapter_.containsEventTarget(t)})?this.resetActivationState_():(void 0!==t&&(ft.push(t.target),this.registerDeactivationHandlers_(t)),n.wasElementMadeActive=this.checkElementMadeActive_(t),n.wasElementMadeActive&&this.animateActivation_(),requestAnimationFrame(function(){ft=[],n.wasElementMadeActive||void 0===t||" "!==t.key&&32!==t.keyCode||(n.wasElementMadeActive=e.checkElementMadeActive_(t),n.wasElementMadeActive&&e.animateActivation_()),n.wasElementMadeActive||(e.activationState_=e.defaultActivationState_())})))}}},pt.prototype.checkElementMadeActive_=function(t){return void 0===t||"keydown"!==t.type||this.adapter_.isSurfaceActive()},pt.prototype.animateActivation_=function(){var t=this,e=pt.strings,n=e.VAR_FG_TRANSLATE_START,i=e.VAR_FG_TRANSLATE_END,r=pt.cssClasses,o=r.FG_DEACTIVATION,a=r.FG_ACTIVATION,s=pt.numbers.DEACTIVATION_TIMEOUT_MS;this.layoutInternal_();var c="",u="";if(!this.adapter_.isUnbounded()){var l=this.getFgTranslationCoordinates_(),f=l.startPoint,d=l.endPoint;c=f.x+"px, "+f.y+"px",u=d.x+"px, "+d.y+"px"}this.adapter_.updateCssVariable(n,c),this.adapter_.updateCssVariable(i,u),clearTimeout(this.activationTimer_),clearTimeout(this.fgDeactivationRemovalTimer_),this.rmBoundedActivationClasses_(),this.adapter_.removeClass(o),this.adapter_.computeBoundingRect(),this.adapter_.addClass(a),this.activationTimer_=setTimeout(function(){return t.activationTimerCallback_()},s)},pt.prototype.getFgTranslationCoordinates_=function(){var t,e=this.activationState_,n=e.activationEvent;return{startPoint:t={x:(t=e.wasActivatedByPointer?E(n,this.adapter_.getWindowPageOffset(),this.adapter_.computeBoundingRect()):{x:this.frame_.width/2,y:this.frame_.height/2}).x-this.initialSize_/2,y:t.y-this.initialSize_/2},endPoint:{x:this.frame_.width/2-this.initialSize_/2,y:this.frame_.height/2-this.initialSize_/2}}},pt.prototype.runDeactivationUXLogicIfReady_=function(){var t=this,e=pt.cssClasses.FG_DEACTIVATION,n=this.activationState_,i=n.hasDeactivationUXRun,r=n.isActivated;!i&&r||!this.activationAnimationHasEnded_||(this.rmBoundedActivationClasses_(),this.adapter_.addClass(e),this.fgDeactivationRemovalTimer_=setTimeout(function(){t.adapter_.removeClass(e)},v.FG_DEACTIVATION_MS))},pt.prototype.rmBoundedActivationClasses_=function(){var t=pt.cssClasses.FG_ACTIVATION;this.adapter_.removeClass(t),this.activationAnimationHasEnded_=!1,this.adapter_.computeBoundingRect()},pt.prototype.resetActivationState_=function(){var t=this;this.previousActivationEvent_=this.activationState_.activationEvent,this.activationState_=this.defaultActivationState_(),setTimeout(function(){return t.previousActivationEvent_=void 0},pt.numbers.TAP_DELAY_MS)},pt.prototype.deactivate_=function(){var t=this,e=this.activationState_;if(e.isActivated){var n=a({},e);e.isProgrammatic?(requestAnimationFrame(function(){return t.animateDeactivation_(n)}),this.resetActivationState_()):(this.deregisterDeactivationHandlers_(),requestAnimationFrame(function(){t.activationState_.hasDeactivationUXRun=!0,t.animateDeactivation_(n),t.resetActivationState_()}))}},pt.prototype.animateDeactivation_=function(t){var e=t.wasActivatedByPointer,n=t.wasElementMadeActive;(e||n)&&this.runDeactivationUXLogicIfReady_()},pt.prototype.layoutInternal_=function(){var t=this;this.frame_=this.adapter_.computeBoundingRect();var e=Math.max(this.frame_.height,this.frame_.width);this.maxRadius_=this.adapter_.isUnbounded()?e:Math.sqrt(Math.pow(t.frame_.width,2)+Math.pow(t.frame_.height,2))+pt.numbers.PADDING;var n=Math.floor(e*pt.numbers.INITIAL_ORIGIN_SCALE);this.adapter_.isUnbounded()&&n%2!=0?this.initialSize_=n-1:this.initialSize_=n,this.fgScale_=""+this.maxRadius_/this.initialSize_,this.updateLayoutCssVars_()},pt.prototype.updateLayoutCssVars_=function(){var t=pt.strings,e=t.VAR_FG_SIZE,n=t.VAR_LEFT,i=t.VAR_TOP,r=t.VAR_FG_SCALE;this.adapter_.updateCssVariable(e,this.initialSize_+"px"),this.adapter_.updateCssVariable(r,this.fgScale_),this.adapter_.isUnbounded()&&(this.unboundedCoords_={left:Math.round(this.frame_.width/2-this.initialSize_/2),top:Math.round(this.frame_.height/2-this.initialSize_/2)},this.adapter_.updateCssVariable(n,this.unboundedCoords_.left+"px"),this.adapter_.updateCssVariable(i,this.unboundedCoords_.top+"px"))},pt);function pt(t){var e=ct.call(this,a(a({},pt.defaultAdapter),t))||this;return e.activationAnimationHasEnded_=!1,e.activationTimer_=0,e.fgDeactivationRemovalTimer_=0,e.fgScale_="0",e.frame_={width:0,height:0},e.initialSize_=0,e.layoutFrame_=0,e.maxRadius_=0,e.unboundedCoords_={left:0,top:0},e.activationState_=e.defaultActivationState_(),e.activationTimerCallback_=function(){e.activationAnimationHasEnded_=!0,e.runDeactivationUXLogicIfReady_()},e.activateHandler_=function(t){return e.activate_(t)},e.deactivateHandler_=function(){return e.deactivate_()},e.focusHandler_=function(){return e.handleFocus()},e.blurHandler_=function(){return e.handleBlur()},e.resizeHandler_=function(){return e.layout()},e}var ht,_t=(r(yt,ht=u),yt.attachTo=function(t,e){void 0===e&&(e={isUnbounded:void 0});var n=new yt(t);return void 0!==e.isUnbounded&&(n.unbounded=e.isUnbounded),n},yt.createAdapter=function(n){return{addClass:function(t){return n.root_.classList.add(t)},browserSupportsCssVars:function(){return b(window)},computeBoundingRect:function(){return n.root_.getBoundingClientRect()},containsEventTarget:function(t){return n.root_.contains(t)},deregisterDocumentInteractionHandler:function(t,e){return document.documentElement.removeEventListener(t,e,d())},deregisterInteractionHandler:function(t,e){return n.root_.removeEventListener(t,e,d())},deregisterResizeHandler:function(t){return window.removeEventListener("resize",t)},getWindowPageOffset:function(){return{x:window.pageXOffset,y:window.pageYOffset}},isSurfaceActive:function(){return h(n.root_,":active")},isSurfaceDisabled:function(){return Boolean(n.disabled)},isUnbounded:function(){return Boolean(n.unbounded)},registerDocumentInteractionHandler:function(t,e){return document.documentElement.addEventListener(t,e,d())},registerInteractionHandler:function(t,e){return n.root_.addEventListener(t,e,d())},registerResizeHandler:function(t){return window.addEventListener("resize",t)},removeClass:function(t){return n.root_.classList.remove(t)},updateCssVariable:function(t,e){return n.root_.style.setProperty(t,e)}}},Object.defineProperty(yt.prototype,"unbounded",{get:function(){return Boolean(this.unbounded_)},set:function(t){this.unbounded_=Boolean(t),this.setUnbounded_()},enumerable:!0,configurable:!0}),yt.prototype.activate=function(){this.foundation_.activate()},yt.prototype.deactivate=function(){this.foundation_.deactivate()},yt.prototype.layout=function(){this.foundation_.layout()},yt.prototype.getDefaultFoundation=function(){return new dt(yt.createAdapter(this))},yt.prototype.initialSyncWithDOM=function(){var t=this.root_;this.unbounded="mdcRippleIsUnbounded"in t.dataset},yt.prototype.setUnbounded_=function(){this.foundation_.setUnbounded(Boolean(this.unbounded_))},yt);
   function yt(){var t=null!==ht&&ht.apply(this,arguments)||this;return t.disabled=!1,t}
var mt,vt={ANIM_CHECKED_INDETERMINATE:"mdc-checkbox--anim-checked-indeterminate",ANIM_CHECKED_UNCHECKED:"mdc-checkbox--anim-checked-unchecked",ANIM_INDETERMINATE_CHECKED:"mdc-checkbox--anim-indeterminate-checked",ANIM_INDETERMINATE_UNCHECKED:"mdc-checkbox--anim-indeterminate-unchecked",ANIM_UNCHECKED_CHECKED:"mdc-checkbox--anim-unchecked-checked",ANIM_UNCHECKED_INDETERMINATE:"mdc-checkbox--anim-unchecked-indeterminate",BACKGROUND:"mdc-checkbox__background",CHECKED:"mdc-checkbox--checked",CHECKMARK:"mdc-checkbox__checkmark",CHECKMARK_PATH:"mdc-checkbox__checkmark-path",DISABLED:"mdc-checkbox--disabled",INDETERMINATE:"mdc-checkbox--indeterminate",MIXEDMARK:"mdc-checkbox__mixedmark",NATIVE_CONTROL:"mdc-checkbox__native-control",ROOT:"mdc-checkbox",SELECTED:"mdc-checkbox--selected",UPGRADED:"mdc-checkbox--upgraded"},bt={ARIA_CHECKED_ATTR:"aria-checked",ARIA_CHECKED_INDETERMINATE_VALUE:"mixed",DATA_INDETERMINATE_ATTR:"data-indeterminate",NATIVE_CONTROL_SELECTOR:".mdc-checkbox__native-control",TRANSITION_STATE_CHECKED:"checked",TRANSITION_STATE_INDETERMINATE:"indeterminate",TRANSITION_STATE_INIT:"init",TRANSITION_STATE_UNCHECKED:"unchecked"},Et={ANIM_END_LATCH_MS:250},gt=(r(Ct,mt=s),Object.defineProperty(Ct,"cssClasses",{get:function(){return vt},enumerable:!1,configurable:!0}),Object.defineProperty(Ct,"strings",{get:function(){return bt},enumerable:!1,configurable:!0}),Object.defineProperty(Ct,"numbers",{get:function(){return Et},enumerable:!1,configurable:!0}),Object.defineProperty(Ct,"defaultAdapter",{get:function(){return{addClass:function(){},forceLayout:function(){},hasNativeControl:function(){return!1},isAttachedToDOM:function(){return!1},isChecked:function(){return!1},isIndeterminate:function(){return!1},removeClass:function(){},removeNativeControlAttr:function(){},setNativeControlAttr:function(){},setNativeControlDisabled:function(){}}},enumerable:!1,configurable:!0}),Ct.prototype.init=function(){this.currentCheckState_=this.determineCheckState_(),this.updateAriaChecked_(),this.adapter_.addClass(vt.UPGRADED)},Ct.prototype.destroy=function(){clearTimeout(this.animEndLatchTimer_)},Ct.prototype.setDisabled=function(t){this.adapter_.setNativeControlDisabled(t),t?this.adapter_.addClass(vt.DISABLED):this.adapter_.removeClass(vt.DISABLED)},Ct.prototype.handleAnimationEnd=function(){var t=this;this.enableAnimationEndHandler_&&(clearTimeout(this.animEndLatchTimer_),this.animEndLatchTimer_=setTimeout(function(){t.adapter_.removeClass(t.currentAnimationClass_),t.enableAnimationEndHandler_=!1},Et.ANIM_END_LATCH_MS))},Ct.prototype.handleChange=function(){this.transitionCheckState_()},Ct.prototype.transitionCheckState_=function(){if(this.adapter_.hasNativeControl()){var t=this.currentCheckState_,e=this.determineCheckState_();if(t!==e){this.updateAriaChecked_();var n=vt.SELECTED;e===bt.TRANSITION_STATE_UNCHECKED?this.adapter_.removeClass(n):this.adapter_.addClass(n),0<this.currentAnimationClass_.length&&(clearTimeout(this.animEndLatchTimer_),this.adapter_.forceLayout(),this.adapter_.removeClass(this.currentAnimationClass_)),this.currentAnimationClass_=this.getTransitionAnimationClass_(t,e),this.currentCheckState_=e,this.adapter_.isAttachedToDOM()&&0<this.currentAnimationClass_.length&&(this.adapter_.addClass(this.currentAnimationClass_),this.enableAnimationEndHandler_=!0)}}},Ct.prototype.determineCheckState_=function(){var t=bt.TRANSITION_STATE_INDETERMINATE,e=bt.TRANSITION_STATE_CHECKED,n=bt.TRANSITION_STATE_UNCHECKED;return this.adapter_.isIndeterminate()?t:this.adapter_.isChecked()?e:n},Ct.prototype.getTransitionAnimationClass_=function(t,e){var n=bt.TRANSITION_STATE_INIT,i=bt.TRANSITION_STATE_CHECKED,r=bt.TRANSITION_STATE_UNCHECKED,o=Ct.cssClasses,a=o.ANIM_UNCHECKED_CHECKED,s=o.ANIM_UNCHECKED_INDETERMINATE,c=o.ANIM_CHECKED_UNCHECKED,u=o.ANIM_CHECKED_INDETERMINATE,l=o.ANIM_INDETERMINATE_CHECKED,f=o.ANIM_INDETERMINATE_UNCHECKED;switch(t){case n:return e===r?"":e===i?l:f;case r:return e===i?a:s;case i:return e===r?c:u;default:return e===i?l:f}},Ct.prototype.updateAriaChecked_=function(){this.adapter_.isIndeterminate()?this.adapter_.setNativeControlAttr(bt.ARIA_CHECKED_ATTR,bt.ARIA_CHECKED_INDETERMINATE_VALUE):this.adapter_.removeNativeControlAttr(bt.ARIA_CHECKED_ATTR)},Ct);function Ct(t){var e=mt.call(this,a(a({},Ct.defaultAdapter),t))||this;return e.currentCheckState_=bt.TRANSITION_STATE_INIT,e.currentAnimationClass_="",e.animEndLatchTimer_=0,e.enableAnimationEndHandler_=!1,e}var At,Tt=["checked","indeterminate"],It=(r(St,At=u),St.attachTo=function(t){return new St(t)},Object.defineProperty(St.prototype,"ripple",{get:function(){return this.ripple_},enumerable:!1,configurable:!0}),Object.defineProperty(St.prototype,"checked",{get:function(){return this.nativeControl_.checked},set:function(t){this.nativeControl_.checked=t},enumerable:!1,configurable:!0}),Object.defineProperty(St.prototype,"indeterminate",{get:function(){return this.nativeControl_.indeterminate},set:function(t){this.nativeControl_.indeterminate=t},enumerable:!1,configurable:!0}),Object.defineProperty(St.prototype,"disabled",{get:function(){return this.nativeControl_.disabled},set:function(t){this.foundation_.setDisabled(t)},enumerable:!1,configurable:!0}),Object.defineProperty(St.prototype,"value",{get:function(){return this.nativeControl_.value},set:function(t){this.nativeControl_.value=t},enumerable:!1,configurable:!0}),St.prototype.initialize=function(){var t=bt.DATA_INDETERMINATE_ATTR;this.nativeControl_.indeterminate="true"===this.nativeControl_.getAttribute(t),this.nativeControl_.removeAttribute(t)},St.prototype.initialSyncWithDOM=function(){var t=this;this.handleAnimationEnd_=function(){return t.foundation_.handleAnimationEnd()},this.listen(st(window,"animationend"),this.handleAnimationEnd_),this.installPropertyChangeHooks_()},St.prototype.destroy=function(){this.ripple_.destroy(),this.unlisten(st(window,"animationend"),this.handleAnimationEnd_),this.uninstallPropertyChangeHooks_(),At.prototype.destroy.call(this)},St.prototype.getDefaultFoundation=function(){var n=this;return new gt({addClass:function(t){return n.root_.classList.add(t)},forceLayout:function(){return n.root_.offsetWidth},hasNativeControl:function(){return!!n.nativeControl_},isAttachedToDOM:function(){return Boolean(n.root_.parentNode)},isChecked:function(){return n.checked},isIndeterminate:function(){return n.indeterminate},removeClass:function(t){n.root_.classList.remove(t)},removeNativeControlAttr:function(t){n.nativeControl_.removeAttribute(t)},setNativeControlAttr:function(t,e){n.nativeControl_.setAttribute(t,e)},setNativeControlDisabled:function(t){n.nativeControl_.disabled=t}})},St.prototype.createRipple_=function(){var n=this,t=a(a({},_t.createAdapter(this)),{deregisterInteractionHandler:function(t,e){return n.nativeControl_.removeEventListener(t,e,d())},isSurfaceActive:function(){return h(n.nativeControl_,":active")},isUnbounded:function(){return!0},registerInteractionHandler:function(t,e){return n.nativeControl_.addEventListener(t,e,d())}});return new _t(this.root_,new dt(t))},St.prototype.installPropertyChangeHooks_=function(){var r=this,o=this.nativeControl_,a=Object.getPrototypeOf(o);Tt.forEach(function(t){var e=Object.getOwnPropertyDescriptor(a,t);if(Ot(e)){var n=e.get,i={configurable:e.configurable,enumerable:e.enumerable,get:n,set:function(t){e.set.call(o,t),r.foundation_.handleChange()}};Object.defineProperty(o,t,i)}})},St.prototype.uninstallPropertyChangeHooks_=function(){var n=this.nativeControl_,i=Object.getPrototypeOf(n);Tt.forEach(function(t){var e=Object.getOwnPropertyDescriptor(i,t);Ot(e)&&Object.defineProperty(n,t,e)})},Object.defineProperty(St.prototype,"nativeControl_",{get:function(){var t=bt.NATIVE_CONTROL_SELECTOR,e=this.root_.querySelector(t);if(!e)throw new Error("Checkbox component requires a "+t+" element");return e},enumerable:!1,configurable:!0}),St);
   function St(){var t=null!==At&&At.apply(this,arguments)||this;return t.ripple_=t.createRipple_(),t}function Ot(t){return!!t&&"function"==typeof t.set}function Rt(t){return(Rt="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Lt(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function wt(t,e,n){return e&&Lt(t.prototype,e),n&&Lt(t,n),t}function xt(o){var a=Pt();return function(){var t,e,n,i=Ft(o);if(a){var r=Ft(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Rt(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Nt(t){var i="function"==typeof Map?new Map:void 0;return(Nt=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Dt(t,arguments,Ft(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),kt(n,t)})(t)}function Dt(t,e,n){return(Dt=Pt()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&kt(r,n.prototype),r}).apply(null,arguments)}function Pt(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function kt(t,e){return(kt=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Ft(t){return(Ft=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var Mt,Ht,jt,Bt,Vt=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&kt(t,e)}(n,Nt(HTMLElement));var e=xt(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).checked_=!1,t.indeterminate_=!1,t.disabled_=!1,t.checkbox_,t}return wt(n,[{key:"focus",value:function(){this.checkbox_.nativeControl_.focus()}},{key:"blur",value:function(){this.checkbox_.nativeControl_.blur()}},{key:"checked",get:function(){return this.checked_},set:function(t){this.checked_=t,this.checkbox_&&(this.checkbox_.checked=t)}},{key:"indeterminate",get:function(){return this.indeterminate_},set:function(t){this.indeterminate_=t,this.checkbox_&&(this.checkbox_.indeterminate=t)}},{key:"disabled",get:function(){return this.disabled_},set:function(t){this.disabled_=t,this.checkbox_&&(this.checkbox_.disabled=t)}}]),wt(n,[{key:"connectedCallback",value:function(){k.call(this),this.checkbox_=new It(this),this.checkbox_.checked=this.checked_,this.checkbox_.indeterminate=this.indeterminate_,this.checkbox_.disabled=this.disabled_}},{key:"destroy",value:function(){this.checkbox_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-checkbox",Vt),(Ht=Mt=Mt||{})[Ht.RIGHT=0]="RIGHT",Ht[Ht.LEFT=1]="LEFT",(Bt=jt=jt||{})[Bt.PRIMARY=0]="PRIMARY",Bt[Bt.TRAILING=1]="TRAILING",Bt[Bt.NONE=2]="NONE";var Ut={ADDED_ANNOUNCEMENT_ATTRIBUTE:"data-mdc-chip-added-announcement",ARIA_CHECKED:"aria-checked",ARROW_DOWN_KEY:"ArrowDown",ARROW_LEFT_KEY:"ArrowLeft",ARROW_RIGHT_KEY:"ArrowRight",ARROW_UP_KEY:"ArrowUp",BACKSPACE_KEY:"Backspace",CHECKMARK_SELECTOR:".mdc-chip__checkmark",DELETE_KEY:"Delete",END_KEY:"End",ENTER_KEY:"Enter",ENTRY_ANIMATION_NAME:"mdc-chip-entry",HOME_KEY:"Home",IE_ARROW_DOWN_KEY:"Down",IE_ARROW_LEFT_KEY:"Left",IE_ARROW_RIGHT_KEY:"Right",IE_ARROW_UP_KEY:"Up",IE_DELETE_KEY:"Del",INTERACTION_EVENT:"MDCChip:interaction",LEADING_ICON_SELECTOR:".mdc-chip__icon--leading",NAVIGATION_EVENT:"MDCChip:navigation",PRIMARY_ACTION_SELECTOR:".mdc-chip__primary-action",REMOVED_ANNOUNCEMENT_ATTRIBUTE:"data-mdc-chip-removed-announcement",REMOVAL_EVENT:"MDCChip:removal",SELECTION_EVENT:"MDCChip:selection",SPACEBAR_KEY:" ",TAB_INDEX:"tabindex",TRAILING_ACTION_SELECTOR:".mdc-chip__trailing-action",TRAILING_ICON_INTERACTION_EVENT:"MDCChip:trailingIconInteraction",TRAILING_ICON_SELECTOR:".mdc-chip__icon--trailing"},Kt={CHECKMARK:"mdc-chip__checkmark",CHIP_EXIT:"mdc-chip--exit",DELETABLE:"mdc-chip--deletable",HIDDEN_LEADING_ICON:"mdc-chip__icon--leading-hidden",LEADING_ICON:"mdc-chip__icon--leading",PRIMARY_ACTION:"mdc-chip__primary-action",PRIMARY_ACTION_FOCUSED:"mdc-chip--primary-action-focused",SELECTED:"mdc-chip--selected",TEXT:"mdc-chip__text",TRAILING_ACTION:"mdc-chip__trailing-action",TRAILING_ICON:"mdc-chip__icon--trailing"},Gt=new Set;Gt.add(Ut.ARROW_LEFT_KEY),Gt.add(Ut.ARROW_RIGHT_KEY),Gt.add(Ut.ARROW_DOWN_KEY),Gt.add(Ut.ARROW_UP_KEY),Gt.add(Ut.END_KEY),Gt.add(Ut.HOME_KEY),Gt.add(Ut.IE_ARROW_LEFT_KEY),Gt.add(Ut.IE_ARROW_RIGHT_KEY),Gt.add(Ut.IE_ARROW_DOWN_KEY),Gt.add(Ut.IE_ARROW_UP_KEY);var Wt=new Set;Wt.add(Ut.ARROW_UP_KEY),Wt.add(Ut.ARROW_DOWN_KEY),Wt.add(Ut.HOME_KEY),Wt.add(Ut.END_KEY),Wt.add(Ut.IE_ARROW_UP_KEY),Wt.add(Ut.IE_ARROW_DOWN_KEY);
var qt,zt={bottom:0,height:0,left:0,right:0,top:0,width:0},Xt=(r(Yt,qt=s),Object.defineProperty(Yt,"strings",{get:function(){return Ut},enumerable:!1,configurable:!0}),Object.defineProperty(Yt,"cssClasses",{get:function(){return Kt},enumerable:!1,configurable:!0}),Object.defineProperty(Yt,"defaultAdapter",{get:function(){return{addClass:function(){},addClassToLeadingIcon:function(){},eventTargetHasClass:function(){return!1},focusPrimaryAction:function(){},focusTrailingAction:function(){},getAttribute:function(){return null},getCheckmarkBoundingClientRect:function(){return zt},getComputedStyleValue:function(){return""},getRootBoundingClientRect:function(){return zt},hasClass:function(){return!1},hasLeadingIcon:function(){return!1},hasTrailingAction:function(){return!1},isRTL:function(){return!1},notifyInteraction:function(){},notifyNavigation:function(){},notifyRemoval:function(){},notifySelection:function(){},notifyTrailingIconInteraction:function(){},removeClass:function(){},removeClassFromLeadingIcon:function(){},setPrimaryActionAttr:function(){},setStyleProperty:function(){},setTrailingActionAttr:function(){}}},enumerable:!1,configurable:!0}),Yt.prototype.isSelected=function(){return this.adapter_.hasClass(Kt.SELECTED)},Yt.prototype.setSelected=function(t){this.setSelected_(t),this.notifySelection_(t)},Yt.prototype.setSelectedFromChipSet=function(t,e){this.setSelected_(t),e&&this.notifyIgnoredSelection_(t)},Yt.prototype.getShouldRemoveOnTrailingIconClick=function(){return this.shouldRemoveOnTrailingIconClick_},Yt.prototype.setShouldRemoveOnTrailingIconClick=function(t){this.shouldRemoveOnTrailingIconClick_=t},Yt.prototype.getDimensions=function(){function t(){return e.adapter_.getRootBoundingClientRect()}var e=this;if(!this.adapter_.hasLeadingIcon()){var n=e.adapter_.getCheckmarkBoundingClientRect();if(n){var i=t();return{bottom:i.bottom,height:i.height,left:i.left,right:i.right,top:i.top,width:i.width+n.height}}}return t()},Yt.prototype.beginExit=function(){this.adapter_.addClass(Kt.CHIP_EXIT)},Yt.prototype.handleInteraction=function(t){this.shouldHandleInteraction_(t)&&(this.adapter_.notifyInteraction(),this.focusPrimaryAction_())},Yt.prototype.handleTransitionEnd=function(t){var e=this,n=this.adapter_.eventTargetHasClass(t.target,Kt.CHIP_EXIT),i="width"===t.propertyName,r="opacity"===t.propertyName;if(n&&r){var o=this.adapter_.getComputedStyleValue("width");requestAnimationFrame(function(){e.adapter_.setStyleProperty("width",o),e.adapter_.setStyleProperty("padding","0"),e.adapter_.setStyleProperty("margin","0"),requestAnimationFrame(function(){e.adapter_.setStyleProperty("width","0")})})}else{if(n&&i){this.removeFocus_();var a=this.adapter_.getAttribute(Ut.REMOVED_ANNOUNCEMENT_ATTRIBUTE);this.adapter_.notifyRemoval(a)}if(r){var s=this.adapter_.eventTargetHasClass(t.target,Kt.LEADING_ICON)&&this.adapter_.hasClass(Kt.SELECTED),c=this.adapter_.eventTargetHasClass(t.target,Kt.CHECKMARK)&&!this.adapter_.hasClass(Kt.SELECTED);return s?this.adapter_.addClassToLeadingIcon(Kt.HIDDEN_LEADING_ICON):c?this.adapter_.removeClassFromLeadingIcon(Kt.HIDDEN_LEADING_ICON):void 0}}},Yt.prototype.handleFocusIn=function(t){this.eventFromPrimaryAction_(t)&&this.adapter_.addClass(Kt.PRIMARY_ACTION_FOCUSED)},Yt.prototype.handleFocusOut=function(t){this.eventFromPrimaryAction_(t)&&this.adapter_.removeClass(Kt.PRIMARY_ACTION_FOCUSED)},Yt.prototype.handleTrailingIconInteraction=function(t){this.shouldHandleInteraction_(t)&&(this.adapter_.notifyTrailingIconInteraction(),this.removeChip_(t))},Yt.prototype.handleKeydown=function(t){if(this.shouldRemoveChip_(t))return this.removeChip_(t);var e=t.key;Gt.has(e)&&(t.preventDefault(),this.focusNextAction_(t))},Yt.prototype.removeFocus=function(){this.adapter_.setPrimaryActionAttr(Ut.TAB_INDEX,"-1"),this.adapter_.setTrailingActionAttr(Ut.TAB_INDEX,"-1")},Yt.prototype.focusPrimaryAction=function(){this.focusPrimaryAction_()},Yt.prototype.focusTrailingAction=function(){if(!this.adapter_.hasTrailingAction())return this.focusPrimaryAction_();this.focusTrailingAction_()},Yt.prototype.focusNextAction_=function(t){var e=t.key,n=this.adapter_.hasTrailingAction(),i=this.getDirection_(e),r=this.getEvtSource_(t);if(!Wt.has(e)&&n)return r===jt.PRIMARY&&i===Mt.RIGHT?this.focusTrailingAction_():r===jt.TRAILING&&i===Mt.LEFT?this.focusPrimaryAction_():void this.adapter_.notifyNavigation(e,jt.NONE);this.adapter_.notifyNavigation(e,r)},Yt.prototype.getEvtSource_=function(t){return this.adapter_.eventTargetHasClass(t.target,Kt.PRIMARY_ACTION)?jt.PRIMARY:this.adapter_.eventTargetHasClass(t.target,Kt.TRAILING_ACTION)?jt.TRAILING:jt.NONE},Yt.prototype.getDirection_=function(t){var e=this.adapter_.isRTL();return!e&&(t===Ut.ARROW_LEFT_KEY||t===Ut.IE_ARROW_LEFT_KEY)||e&&(t===Ut.ARROW_RIGHT_KEY||t===Ut.IE_ARROW_RIGHT_KEY)?Mt.LEFT:Mt.RIGHT},Yt.prototype.focusPrimaryAction_=function(){this.adapter_.setPrimaryActionAttr(Ut.TAB_INDEX,"0"),this.adapter_.focusPrimaryAction(),this.adapter_.setTrailingActionAttr(Ut.TAB_INDEX,"-1")},Yt.prototype.focusTrailingAction_=function(){this.adapter_.setTrailingActionAttr(Ut.TAB_INDEX,"0"),this.adapter_.focusTrailingAction(),this.adapter_.setPrimaryActionAttr(Ut.TAB_INDEX,"-1")},Yt.prototype.removeFocus_=function(){this.adapter_.setTrailingActionAttr(Ut.TAB_INDEX,"-1"),this.adapter_.setPrimaryActionAttr(Ut.TAB_INDEX,"-1")},Yt.prototype.removeChip_=function(t){t.stopPropagation(),t.preventDefault(),this.shouldRemoveOnTrailingIconClick_&&this.beginExit()},Yt.prototype.shouldHandleInteraction_=function(t){if("click"===t.type)return!0;var e=t;return e.key===Ut.ENTER_KEY||e.key===Ut.SPACEBAR_KEY},Yt.prototype.shouldRemoveChip_=function(t){return this.adapter_.hasClass(Kt.DELETABLE)&&(t.key===Ut.BACKSPACE_KEY||t.key===Ut.DELETE_KEY||t.key===Ut.IE_DELETE_KEY)},Yt.prototype.setSelected_=function(t){t?(this.adapter_.addClass(Kt.SELECTED),this.adapter_.setPrimaryActionAttr(Ut.ARIA_CHECKED,"true")):(this.adapter_.removeClass(Kt.SELECTED),this.adapter_.setPrimaryActionAttr(Ut.ARIA_CHECKED,"false"))},Yt.prototype.notifySelection_=function(t){this.adapter_.notifySelection(t,!1)},Yt.prototype.notifyIgnoredSelection_=function(t){this.adapter_.notifySelection(t,!0)},Yt.prototype.eventFromPrimaryAction_=function(t){return this.adapter_.eventTargetHasClass(t.target,Kt.PRIMARY_ACTION)},Yt);function Yt(t){var e=qt.call(this,a(a({},Yt.defaultAdapter),t))||this;return e.shouldRemoveOnTrailingIconClick_=!0,e}var Qt,Zt=["click","keydown"],$t=(r(Jt,Qt=u),Object.defineProperty(Jt.prototype,"selected",{get:function(){return this.foundation_.isSelected()},set:function(t){this.foundation_.setSelected(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Jt.prototype,"shouldRemoveOnTrailingIconClick",{get:function(){return this.foundation_.getShouldRemoveOnTrailingIconClick()},set:function(t){this.foundation_.setShouldRemoveOnTrailingIconClick(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Jt.prototype,"ripple",{get:function(){return this.ripple_},enumerable:!1,configurable:!0}),Object.defineProperty(Jt.prototype,"id",{get:function(){return this.root_.id},enumerable:!1,configurable:!0}),Jt.attachTo=function(t){return new Jt(t)},Jt.prototype.initialize=function(t){var e=this;void 0===t&&(t=function(t,e){return new _t(t,e)}),this.leadingIcon_=this.root_.querySelector(Ut.LEADING_ICON_SELECTOR),this.trailingIcon_=this.root_.querySelector(Ut.TRAILING_ICON_SELECTOR),this.checkmark_=this.root_.querySelector(Ut.CHECKMARK_SELECTOR),this.primaryAction_=this.root_.querySelector(Ut.PRIMARY_ACTION_SELECTOR),this.trailingAction_=this.root_.querySelector(Ut.TRAILING_ACTION_SELECTOR);var n=a(a({},_t.createAdapter(this)),{computeBoundingRect:function(){return e.foundation_.getDimensions()}});this.ripple_=t(this.root_,new dt(n))},Jt.prototype.initialSyncWithDOM=function(){var e=this;this.handleInteraction_=function(t){return e.foundation_.handleInteraction(t)},this.handleTransitionEnd_=function(t){return e.foundation_.handleTransitionEnd(t)},this.handleTrailingIconInteraction_=function(t){return e.foundation_.handleTrailingIconInteraction(t)},this.handleKeydown_=function(t){return e.foundation_.handleKeydown(t)},this.handleFocusIn_=function(t){e.foundation_.handleFocusIn(t)},this.handleFocusOut_=function(t){e.foundation_.handleFocusOut(t)},Zt.forEach(function(t){e.listen(t,e.handleInteraction_)}),this.listen("transitionend",this.handleTransitionEnd_),this.listen("keydown",this.handleKeydown_),this.listen("focusin",this.handleFocusIn_),this.listen("focusout",this.handleFocusOut_),this.trailingIcon_&&Zt.forEach(function(t){e.trailingIcon_.addEventListener(t,e.handleTrailingIconInteraction_)})},Jt.prototype.destroy=function(){var e=this;this.ripple_.destroy(),Zt.forEach(function(t){e.unlisten(t,e.handleInteraction_)}),this.unlisten("transitionend",this.handleTransitionEnd_),this.unlisten("keydown",this.handleKeydown_),this.unlisten("focusin",this.handleFocusIn_),this.unlisten("focusout",this.handleFocusOut_),this.trailingIcon_&&Zt.forEach(function(t){e.trailingIcon_.removeEventListener(t,e.handleTrailingIconInteraction_)}),Qt.prototype.destroy.call(this)},Jt.prototype.beginExit=function(){this.foundation_.beginExit()},Jt.prototype.getDefaultFoundation=function(){var n=this;return new Xt({addClass:function(t){return n.root_.classList.add(t)},addClassToLeadingIcon:function(t){n.leadingIcon_&&n.leadingIcon_.classList.add(t)},eventTargetHasClass:function(t,e){return!!t&&t.classList.contains(e)},focusPrimaryAction:function(){n.primaryAction_&&n.primaryAction_.focus()},focusTrailingAction:function(){n.trailingAction_&&n.trailingAction_.focus()},getAttribute:function(t){return n.root_.getAttribute(t)},getCheckmarkBoundingClientRect:function(){return n.checkmark_?n.checkmark_.getBoundingClientRect():null},getComputedStyleValue:function(t){return window.getComputedStyle(n.root_).getPropertyValue(t)},getRootBoundingClientRect:function(){return n.root_.getBoundingClientRect()},hasClass:function(t){return n.root_.classList.contains(t)},hasLeadingIcon:function(){return!!n.leadingIcon_},hasTrailingAction:function(){return!!n.trailingAction_},isRTL:function(){return"rtl"===window.getComputedStyle(n.root_).getPropertyValue("direction")},notifyInteraction:function(){return n.emit(Ut.INTERACTION_EVENT,{chipId:n.id},!0)},notifyNavigation:function(t,e){return n.emit(Ut.NAVIGATION_EVENT,{chipId:n.id,key:t,source:e},!0)},notifyRemoval:function(t){n.emit(Ut.REMOVAL_EVENT,{chipId:n.id,removedAnnouncement:t},!0)},notifySelection:function(t,e){return n.emit(Ut.SELECTION_EVENT,{chipId:n.id,selected:t,shouldIgnore:e},!0)},notifyTrailingIconInteraction:function(){return n.emit(Ut.TRAILING_ICON_INTERACTION_EVENT,{chipId:n.id},!0)},removeClass:function(t){return n.root_.classList.remove(t)},removeClassFromLeadingIcon:function(t){n.leadingIcon_&&n.leadingIcon_.classList.remove(t)},setPrimaryActionAttr:function(t,e){n.primaryAction_&&n.primaryAction_.setAttribute(t,e)},setStyleProperty:function(t,e){return n.root_.style.setProperty(t,e)},setTrailingActionAttr:function(t,e){n.trailingAction_&&n.trailingAction_.setAttribute(t,e)}})},Jt.prototype.setSelectedFromChipSet=function(t,e){this.foundation_.setSelectedFromChipSet(t,e)},Jt.prototype.focusPrimaryAction=function(){this.foundation_.focusPrimaryAction()},Jt.prototype.focusTrailingAction=function(){this.foundation_.focusTrailingAction()},Jt.prototype.removeFocus=function(){this.foundation_.removeFocus()},Jt.prototype.makePrimaryActionFocusable=function(){this.primaryAction_&&this.primaryAction_.setAttribute("tabindex","0")},Jt.prototype.isPrimaryActionFocusable=function(){return!!this.primaryAction_&&"0"===this.primaryAction_.getAttribute("tabindex")},Jt);
   function Jt(){return null!==Qt&&Qt.apply(this,arguments)||this}function te(t){return(te="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function ee(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function ne(t,e,n){return e&&ee(t.prototype,e),n&&ee(t,n),t}function ie(o){var a=ae();return function(){var t,e,n,i=ce(o);if(a){var r=ce(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==te(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function re(t){var i="function"==typeof Map?new Map:void 0;return(re=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return oe(t,arguments,ce(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),se(n,t)})(t)}function oe(t,e,n){return(oe=ae()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&se(r,n.prototype),r}).apply(null,arguments)}function ae(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function se(t,e){return(se=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function ce(t){return(ce=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var ue,le,fe=0,de=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&se(t,e)}(n,re(HTMLElement));var e=ie(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).selected_=!1,t.id_="mdc-chip-".concat(++fe),t.chipset_=null,t}return ne(n,[{key:"id",get:function(){return this.id_},set:function(t){this.id_=t}},{key:"selected",get:function(){return this.selected_},set:function(t){this.selected_=t,this.chip_&&(this.chip_.selected=t)}}]),ne(n,[{key:"connectedCallback",value:function(){this.chip_=new $t(this),this.chip_.selected=this.selected_,this.chipset_=this.parentElement.parentElement.chipset_,this.chipset_.addChip(this)}},{key:"disconnectedCallback",value:function(){this.chip_.destroy(),this.chipset_.foundation_.adapter_.removeChipAtIndex(this.chipset_.findChipIndex_(this.id)),F.call(this)}}]),n}();customElements.define("mdc-chip",de),(le=ue=ue||{}).POLITE="polite",le.ASSERTIVE="assertive";var pe=(he.getInstance=function(){return he.instance||(he.instance=new he),he.instance},he.prototype.say=function(t,e){void 0===e&&(e=ue.POLITE);var n=this.getLiveRegion(e);n.textContent="",setTimeout(function(){n.textContent=t},1)},he.prototype.getLiveRegion=function(t){var e=this.liveRegions.get(t);if(e&&document.body.contains(e))return e;var n=this.createLiveRegion(t);return this.liveRegions.set(t,n),n},he.prototype.createLiveRegion=function(t){var e=document.createElement("div");return e.style.position="absolute",e.style.top="-9999px",e.style.left="-9999px",e.style.height="1px",e.style.overflow="hidden",e.setAttribute("aria-atomic","true"),e.setAttribute("aria-live",t),document.body.appendChild(e),e},he);
   function he(){this.liveRegions=new Map}var _e,ye={CHIP_SELECTOR:".mdc-chip"},me={CHOICE:"mdc-chip-set--choice",FILTER:"mdc-chip-set--filter"},ve=(r(be,_e=s),Object.defineProperty(be,"strings",{get:function(){return ye},enumerable:!1,configurable:!0}),Object.defineProperty(be,"cssClasses",{get:function(){return me},enumerable:!1,configurable:!0}),Object.defineProperty(be,"defaultAdapter",{get:function(){return{announceMessage:function(){},focusChipPrimaryActionAtIndex:function(){},focusChipTrailingActionAtIndex:function(){},getChipListCount:function(){return-1},getIndexOfChipById:function(){return-1},hasClass:function(){return!1},isRTL:function(){return!1},removeChipAtIndex:function(){},removeFocusFromChipAtIndex:function(){},selectChipAtIndex:function(){}}},enumerable:!1,configurable:!0}),be.prototype.getSelectedChipIds=function(){return this.selectedChipIds_.slice()},be.prototype.select=function(t){this.select_(t,!1)},be.prototype.handleChipInteraction=function(t){var e=t.chipId,n=this.adapter_.getIndexOfChipById(e);this.removeFocusFromChipsExcept_(n)},be.prototype.handleChipSelection=function(t){var e=t.chipId,n=t.selected;if(!t.shouldIgnore){var i=0<=this.selectedChipIds_.indexOf(e);n&&!i?this.select(e):!n&&i&&this.deselect_(e)}},be.prototype.handleChipRemoval=function(t){var e=t.chipId,n=t.removedAnnouncement;n&&this.adapter_.announceMessage(n);var i=this.adapter_.getIndexOfChipById(e);this.deselectAndNotifyClients_(e),this.adapter_.removeChipAtIndex(i);var r=this.adapter_.getChipListCount()-1,o=Math.min(i,r);this.removeFocusFromChipsExcept_(o),this.adapter_.focusChipTrailingActionAtIndex(o)},be.prototype.handleChipNavigation=function(t){var e=t.chipId,n=t.key,i=t.source,r=this.adapter_.getChipListCount()-1,o=this.adapter_.getIndexOfChipById(e);if(-1!==o&&Gt.has(n)){var a=this.adapter_.isRTL();!a&&(n===Ut.ARROW_RIGHT_KEY||n===Ut.IE_ARROW_RIGHT_KEY)||a&&(n===Ut.ARROW_LEFT_KEY||n===Ut.IE_ARROW_LEFT_KEY)||n===Ut.ARROW_DOWN_KEY||n===Ut.IE_ARROW_DOWN_KEY?o++:n===Ut.HOME_KEY?o=0:n===Ut.END_KEY?o=r:o--,o<0||r<o||(this.removeFocusFromChipsExcept_(o),this.focusChipAction_(o,n,i))}},be.prototype.focusChipAction_=function(t,e,n){var i=Wt.has(e);if(i&&n===jt.PRIMARY)return this.adapter_.focusChipPrimaryActionAtIndex(t);if(i&&n===jt.TRAILING)return this.adapter_.focusChipTrailingActionAtIndex(t);var r=this.getDirection_(e);return r===Mt.LEFT?this.adapter_.focusChipTrailingActionAtIndex(t):r===Mt.RIGHT?this.adapter_.focusChipPrimaryActionAtIndex(t):void 0},be.prototype.getDirection_=function(t){var e=this.adapter_.isRTL();return!e&&(t===Ut.ARROW_LEFT_KEY||t===Ut.IE_ARROW_LEFT_KEY)||e&&(t===Ut.ARROW_RIGHT_KEY||t===Ut.IE_ARROW_RIGHT_KEY)?Mt.LEFT:Mt.RIGHT},be.prototype.deselect_=function(t,e){void 0===e&&(e=!1);var n=this.selectedChipIds_.indexOf(t);if(0<=n){this.selectedChipIds_.splice(n,1);var i=this.adapter_.getIndexOfChipById(t);this.adapter_.selectChipAtIndex(i,!1,e)}},be.prototype.deselectAndNotifyClients_=function(t){this.deselect_(t,!0)},be.prototype.removeFocusFromChipsExcept_=function(t){for(var e=this.adapter_.getChipListCount(),n=0;n<e;n++)n!==t&&this.adapter_.removeFocusFromChipAtIndex(n)},be.prototype.select_=function(t,e){if(!(0<=this.selectedChipIds_.indexOf(t))){if(this.adapter_.hasClass(me.CHOICE)&&0<this.selectedChipIds_.length){var n=this.selectedChipIds_[0],i=this.adapter_.getIndexOfChipById(n);this.selectedChipIds_=[],this.adapter_.selectChipAtIndex(i,!1,e)}this.selectedChipIds_.push(t);var r=this.adapter_.getIndexOfChipById(t);this.adapter_.selectChipAtIndex(r,!0,e)}},be);function be(t){var e=_e.call(this,a(a({},be.defaultAdapter),t))||this;return e.selectedChipIds_=[],e}var Ee,ge=Xt.strings,Ce=ge.INTERACTION_EVENT,Ae=ge.SELECTION_EVENT,Te=ge.REMOVAL_EVENT,Ie=ge.NAVIGATION_EVENT,Se=(r(Oe,Ee=u),Oe.attachTo=function(t){return new Oe(t)},Object.defineProperty(Oe.prototype,"chips",{get:function(){return this.chips_.slice()},enumerable:!1,configurable:!0}),Object.defineProperty(Oe.prototype,"selectedChipIds",{get:function(){return this.foundation_.getSelectedChipIds()},enumerable:!1,configurable:!0}),Oe.prototype.initialize=function(){this.chipFactory_=function(t){return t.chip_},this.chips_=[]},Oe.prototype.initialSyncWithDOM=function(){var e=this;this.chips_.forEach(function(t){t.id&&t.selected&&e.foundation_.select(t.id)}),this.handleChipInteraction_=function(t){return e.foundation_.handleChipInteraction(t.detail)},this.handleChipSelection_=function(t){return e.foundation_.handleChipSelection(t.detail)},this.handleChipRemoval_=function(t){return e.foundation_.handleChipRemoval(t.detail)},this.handleChipNavigation_=function(t){return e.foundation_.handleChipNavigation(t.detail)},this.listen(Ce,this.handleChipInteraction_),this.listen(Ae,this.handleChipSelection_),this.listen(Ie,this.handleChipNavigation_)},Oe.prototype.destroy=function(){this.chips_.forEach(function(t){t.destroy()}),this.unlisten(Ce,this.handleChipInteraction_),this.unlisten(Ae,this.handleChipSelection_),this.unlisten(Te,this.handleChipRemoval_),this.unlisten(Ie,this.handleChipNavigation_),Ee.prototype.destroy.call(this)},Oe.prototype.addChip=function(t){this.chips_.push(this.chipFactory_(t)),1===this.chips_.length&&this.chips_[0].makePrimaryActionFocusable()},Oe.prototype.getDefaultFoundation=function(){var i=this;return new ve({announceMessage:function(t){var e,n;e=t,pe.getInstance().say(e,n)},focusChipPrimaryActionAtIndex:function(t){i.chips_[t].focusPrimaryAction()},focusChipTrailingActionAtIndex:function(t){i.chips_[t].focusTrailingAction()},getChipListCount:function(){return i.chips_.length},getIndexOfChipById:function(t){return i.findChipIndex_(t)},hasClass:function(t){return i.root_.classList.contains(t)},isRTL:function(){return"rtl"===window.getComputedStyle(i.root_).getPropertyValue("direction")},removeChipAtIndex:function(t){0<=t&&t<i.chips_.length&&(i.chips_.splice(t,1),0<i.chips_.length&&!i.chips_.some(function(t){return t.isPrimaryActionFocusable()})&&i.chips_[Math.max(0,t-1)].makePrimaryActionFocusable())},removeFocusFromChipAtIndex:function(t){i.chips_[t].removeFocus()},selectChipAtIndex:function(t,e,n){0<=t&&t<i.chips_.length&&i.chips_[t].setSelectedFromChipSet(e,n)}})},Oe.prototype.findChipIndex_=function(t){for(var e=0;e<this.chips_.length;e++)if(this.chips_[e].id===t)return e;return-1},Oe);
   function Oe(){return null!==Ee&&Ee.apply(this,arguments)||this}function Re(t){return(Re="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Le(t,e){var n;if("undefined"==typeof Symbol||null==t[Symbol.iterator]){if(Array.isArray(t)||(n=function(t,e){if(!t)return;if("string"==typeof t)return we(t,e);var n=Object.prototype.toString.call(t).slice(8,-1);"Object"===n&&t.constructor&&(n=t.constructor.name);if("Map"===n||"Set"===n)return Array.from(t);if("Arguments"===n||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))return we(t,e)}(t))||e&&t&&"number"==typeof t.length){n&&(t=n);var i=0,r=function(){};return{s:r,n:function(){return i>=t.length?{done:!0}:{done:!1,value:t[i++]}},e:function(t){throw t},f:r}}throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}var o,a=!0,s=!1;return{s:function(){n=t[Symbol.iterator]()},n:function(){var t=n.next();return a=t.done,t},e:function(t){s=!0,o=t},f:function(){try{a||null==n.return||n.return()}finally{if(s)throw o}}}}function we(t,e){(null==e||e>t.length)&&(e=t.length);for(var n=0,i=new Array(e);n<e;n++)i[n]=t[n];return i}function xe(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Ne(t,e,n){return e&&xe(t.prototype,e),n&&xe(t,n),t}function De(o){var a=Fe();return function(){var t,e,n,i=He(o);if(a){var r=He(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Re(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Pe(t){var i="function"==typeof Map?new Map:void 0;return(Pe=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return ke(t,arguments,He(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),Me(n,t)})(t)}function ke(t,e,n){return(ke=Fe()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&Me(r,n.prototype),r}).apply(null,arguments)}function Fe(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function Me(t,e){return(Me=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function He(t){return(He=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var je=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&Me(t,e)}(e,Pe(HTMLElement));var t=De(e);function e(){return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,e),t.call(this)}return Ne(e,[{key:"focus",value:function(){var t,e=null,n=Le(this.chipset_.chips);try{for(n.s();!(t=n.n()).done;){var i=t.value;if(i.selected){e=i;break}}}catch(t){n.e(t)}finally{n.f()}e?e.focusPrimaryAction():0<this.chipset_.chips.length&&this.chipset_.chips[0].focusPrimaryAction()}},{key:"blur",value:function(){this.contains(document.activeElement)&&document.activeElement.blur()}}]),Ne(e,[{key:"connectedCallback",value:function(){this.chipset_=new Se(this)}},{key:"disconnectedCallback",value:function(){this.chipset_.destroy()}}]),e}();customElements.define("mdc-chip-set",je);n(104);
   var Be,Ve={INDETERMINATE_CLASS:"mdc-circular-progress--indeterminate",CLOSED_CLASS:"mdc-circular-progress--closed"},Ue={DETERMINATE_CIRCLE_SELECTOR:".mdc-circular-progress__determinate-circle",ARIA_VALUENOW:"aria-valuenow",RADIUS:"r",STROKE_DASHOFFSET:"stroke-dashoffset"},Ke=(r(Ge,Be=s),Object.defineProperty(Ge,"cssClasses",{get:function(){return Ve},enumerable:!1,configurable:!0}),Object.defineProperty(Ge,"strings",{get:function(){return Ue},enumerable:!1,configurable:!0}),Object.defineProperty(Ge,"defaultAdapter",{get:function(){return{addClass:function(){},getDeterminateCircleAttribute:function(){return null},hasClass:function(){return!1},removeClass:function(){},removeAttribute:function(){},setAttribute:function(){},setDeterminateCircleAttribute:function(){}}},enumerable:!1,configurable:!0}),Ge.prototype.init=function(){this.isClosed_=this.adapter_.hasClass(Ve.CLOSED_CLASS),this.isDeterminate_=!this.adapter_.hasClass(Ve.INDETERMINATE_CLASS),this.progress_=0,this.isDeterminate_&&this.adapter_.setAttribute(Ue.ARIA_VALUENOW,this.progress_.toString()),this.radius_=Number(this.adapter_.getDeterminateCircleAttribute(Ue.RADIUS))},Ge.prototype.isDeterminate=function(){return this.isDeterminate_},Ge.prototype.getProgress=function(){return this.progress_},Ge.prototype.isClosed=function(){return this.isClosed_},Ge.prototype.setDeterminate=function(t){this.isDeterminate_=t,this.isDeterminate_?(this.adapter_.removeClass(Ve.INDETERMINATE_CLASS),this.setProgress(this.progress_)):(this.adapter_.addClass(Ve.INDETERMINATE_CLASS),this.adapter_.removeAttribute(Ue.ARIA_VALUENOW))},Ge.prototype.setProgress=function(t){if(this.progress_=t,this.isDeterminate_){var e=(1-this.progress_)*(2*Math.PI*this.radius_);this.adapter_.setDeterminateCircleAttribute(Ue.STROKE_DASHOFFSET,""+e),this.adapter_.setAttribute(Ue.ARIA_VALUENOW,this.progress_.toString())}},Ge.prototype.open=function(){this.isClosed_=!1,this.adapter_.removeClass(Ve.CLOSED_CLASS)},Ge.prototype.close=function(){this.isClosed_=!0,this.adapter_.addClass(Ve.CLOSED_CLASS)},Ge);function Ge(t){return Be.call(this,a(a({},Ge.defaultAdapter),t))||this}var We,qe=(r(ze,We=u),ze.prototype.initialize=function(){this.determinateCircle_=this.root_.querySelector(Ke.strings.DETERMINATE_CIRCLE_SELECTOR)},ze.attachTo=function(t){return new ze(t)},Object.defineProperty(ze.prototype,"determinate",{set:function(t){this.foundation_.setDeterminate(t)},enumerable:!1,configurable:!0}),Object.defineProperty(ze.prototype,"progress",{set:function(t){this.foundation_.setProgress(t)},enumerable:!1,configurable:!0}),Object.defineProperty(ze.prototype,"isClosed",{get:function(){return this.foundation_.isClosed()},enumerable:!1,configurable:!0}),ze.prototype.open=function(){this.foundation_.open()},ze.prototype.close=function(){this.foundation_.close()},ze.prototype.getDefaultFoundation=function(){var n=this;return new Ke({addClass:function(t){return n.root_.classList.add(t)},getDeterminateCircleAttribute:function(t){return n.determinateCircle_.getAttribute(t)},hasClass:function(t){return n.root_.classList.contains(t)},removeClass:function(t){return n.root_.classList.remove(t)},removeAttribute:function(t){return n.root_.removeAttribute(t)},setAttribute:function(t,e){return n.root_.setAttribute(t,e)},setDeterminateCircleAttribute:function(t,e){return n.determinateCircle_.setAttribute(t,e)}})},ze);
   function ze(){return null!==We&&We.apply(this,arguments)||this}function Xe(t){return(Xe="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Ye(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Qe(t,e,n){return e&&Ye(t.prototype,e),n&&Ye(t,n),t}function Ze(o){var a=tn();return function(){var t,e,n,i=nn(o);if(a){var r=nn(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Xe(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function $e(t){var i="function"==typeof Map?new Map:void 0;return($e=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Je(t,arguments,nn(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),en(n,t)})(t)}function Je(t,e,n){return(Je=tn()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&en(r,n.prototype),r}).apply(null,arguments)}function tn(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function en(t,e){return(en=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function nn(t){return(nn=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var rn=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&en(t,e)}(n,$e(HTMLElement));var e=Ze(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).determinate_=!1,t.progress_=0,t.closed_=!1,t.circularProgress_,t}return Qe(n,[{key:"determinate",get:function(){return this.determinate_},set:function(t){this.determinate_=t,this.circularProgress_&&(this.circularProgress_.determinate=t)}},{key:"progress",get:function(){return this.progress_},set:function(t){this.progress_=t,this.circularProgress_&&(this.circularProgress_.progress=t)}},{key:"closed",get:function(){return this.closed_},set:function(t){this.closed_=t,this.circularProgress_&&(t?this.circularProgress_.close():this.circularProgress_.open())}}]),Qe(n,[{key:"connectedCallback",value:function(){k.call(this),this.circularProgress_=new qe(this),this.circularProgress_.determinate=this.determinate_,this.circularProgress_.progress=this.progress_,this.closed_?this.circularProgress_.close():this.circularProgress_.open()}},{key:"disconnectedCallback",value:function(){this.circularProgress_.destroy(),F.call(this)}}]),n}();function on(t){return(on="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function an(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function sn(o){var a=ln();return function(){var t,e,n,i=dn(o);if(a){var r=dn(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==on(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function cn(t){var i="function"==typeof Map?new Map:void 0;return(cn=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return un(t,arguments,dn(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),fn(n,t)})(t)}function un(t,e,n){return(un=ln()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&fn(r,n.prototype),r}).apply(null,arguments)}function ln(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function fn(t,e){return(fn=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function dn(t){return(dn=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-circular-progress",rn);var pn=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&fn(t,e)}(r,cn(HTMLElement));var t,e,n,i=sn(r);function r(){return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,r),i.call(this)}return t=r,(e=[{key:"connectedCallback",value:function(){}},{key:"disconnectedCallback",value:function(){}}])&&an(t.prototype,e),n&&an(t,n),r}();customElements.define("mdc-data-table",pn);
var hn,_n={CLOSING:"mdc-dialog--closing",OPEN:"mdc-dialog--open",OPENING:"mdc-dialog--opening",SCROLLABLE:"mdc-dialog--scrollable",SCROLL_LOCK:"mdc-dialog-scroll-lock",STACKED:"mdc-dialog--stacked"},yn={ACTION_ATTRIBUTE:"data-mdc-dialog-action",BUTTON_DEFAULT_ATTRIBUTE:"data-mdc-dialog-button-default",BUTTON_SELECTOR:".mdc-dialog__button",CLOSED_EVENT:"MDCDialog:closed",CLOSE_ACTION:"close",CLOSING_EVENT:"MDCDialog:closing",CONTAINER_SELECTOR:".mdc-dialog__container",CONTENT_SELECTOR:".mdc-dialog__content",DESTROY_ACTION:"destroy",INITIAL_FOCUS_ATTRIBUTE:"data-mdc-dialog-initial-focus",OPENED_EVENT:"MDCDialog:opened",OPENING_EVENT:"MDCDialog:opening",SCRIM_SELECTOR:".mdc-dialog__scrim",SUPPRESS_DEFAULT_PRESS_SELECTOR:["textarea",".mdc-menu .mdc-list-item"].join(", "),SURFACE_SELECTOR:".mdc-dialog__surface"},mn={DIALOG_ANIMATION_CLOSE_TIME_MS:75,DIALOG_ANIMATION_OPEN_TIME_MS:150},vn=(r(bn,hn=s),Object.defineProperty(bn,"cssClasses",{get:function(){return _n},enumerable:!1,configurable:!0}),Object.defineProperty(bn,"strings",{get:function(){return yn},enumerable:!1,configurable:!0}),Object.defineProperty(bn,"numbers",{get:function(){return mn},enumerable:!1,configurable:!0}),Object.defineProperty(bn,"defaultAdapter",{get:function(){return{addBodyClass:function(){},addClass:function(){},areButtonsStacked:function(){return!1},clickDefaultButton:function(){},eventTargetMatches:function(){return!1},getActionFromEvent:function(){return""},getInitialFocusEl:function(){return null},hasClass:function(){return!1},isContentScrollable:function(){return!1},notifyClosed:function(){},notifyClosing:function(){},notifyOpened:function(){},notifyOpening:function(){},releaseFocus:function(){},removeBodyClass:function(){},removeClass:function(){},reverseButtons:function(){},trapFocus:function(){}}},enumerable:!1,configurable:!0}),bn.prototype.init=function(){this.adapter_.hasClass(_n.STACKED)&&this.setAutoStackButtons(!1)},bn.prototype.destroy=function(){this.isOpen_&&this.close(yn.DESTROY_ACTION),this.animationTimer_&&(clearTimeout(this.animationTimer_),this.handleAnimationTimerEnd_()),this.layoutFrame_&&(cancelAnimationFrame(this.layoutFrame_),this.layoutFrame_=0)},bn.prototype.open=function(){var t=this;this.isOpen_=!0,this.adapter_.notifyOpening(),this.adapter_.addClass(_n.OPENING),this.runNextAnimationFrame_(function(){t.adapter_.addClass(_n.OPEN),t.adapter_.addBodyClass(_n.SCROLL_LOCK),t.layout(),t.animationTimer_=setTimeout(function(){t.handleAnimationTimerEnd_(),t.adapter_.trapFocus(t.adapter_.getInitialFocusEl()),t.adapter_.notifyOpened()},mn.DIALOG_ANIMATION_OPEN_TIME_MS)})},bn.prototype.close=function(t){var e=this;void 0===t&&(t=""),this.isOpen_&&(this.isOpen_=!1,this.adapter_.notifyClosing(t),this.adapter_.addClass(_n.CLOSING),this.adapter_.removeClass(_n.OPEN),this.adapter_.removeBodyClass(_n.SCROLL_LOCK),cancelAnimationFrame(this.animationFrame_),this.animationFrame_=0,clearTimeout(this.animationTimer_),this.animationTimer_=setTimeout(function(){e.adapter_.releaseFocus(),e.handleAnimationTimerEnd_(),e.adapter_.notifyClosed(t)},mn.DIALOG_ANIMATION_CLOSE_TIME_MS))},bn.prototype.isOpen=function(){return this.isOpen_},bn.prototype.getEscapeKeyAction=function(){return this.escapeKeyAction_},bn.prototype.setEscapeKeyAction=function(t){this.escapeKeyAction_=t},bn.prototype.getScrimClickAction=function(){return this.scrimClickAction_},bn.prototype.setScrimClickAction=function(t){this.scrimClickAction_=t},bn.prototype.getAutoStackButtons=function(){return this.autoStackButtons_},bn.prototype.setAutoStackButtons=function(t){this.autoStackButtons_=t},bn.prototype.layout=function(){var t=this;this.layoutFrame_&&cancelAnimationFrame(this.layoutFrame_),this.layoutFrame_=requestAnimationFrame(function(){t.layoutInternal_(),t.layoutFrame_=0})},bn.prototype.handleClick=function(t){if(this.adapter_.eventTargetMatches(t.target,yn.SCRIM_SELECTOR)&&""!==this.scrimClickAction_)this.close(this.scrimClickAction_);else{var e=this.adapter_.getActionFromEvent(t);e&&this.close(e)}},bn.prototype.handleKeydown=function(t){var e="Enter"===t.key||13===t.keyCode;if(e&&!this.adapter_.getActionFromEvent(t)){var n=!this.adapter_.eventTargetMatches(t.target,yn.SUPPRESS_DEFAULT_PRESS_SELECTOR);e&&n&&this.adapter_.clickDefaultButton()}},bn.prototype.handleDocumentKeydown=function(t){"Escape"!==t.key&&27!==t.keyCode||""===this.escapeKeyAction_||this.close(this.escapeKeyAction_)},bn.prototype.layoutInternal_=function(){this.autoStackButtons_&&this.detectStackedButtons_(),this.detectScrollableContent_()},bn.prototype.handleAnimationTimerEnd_=function(){this.animationTimer_=0,this.adapter_.removeClass(_n.OPENING),this.adapter_.removeClass(_n.CLOSING)},bn.prototype.runNextAnimationFrame_=function(t){var e=this;cancelAnimationFrame(this.animationFrame_),this.animationFrame_=requestAnimationFrame(function(){e.animationFrame_=0,clearTimeout(e.animationTimer_),e.animationTimer_=setTimeout(t,0)})},bn.prototype.detectStackedButtons_=function(){this.adapter_.removeClass(_n.STACKED);var t=this.adapter_.areButtonsStacked();t&&this.adapter_.addClass(_n.STACKED),t!==this.areButtonsStacked_&&(this.adapter_.reverseButtons(),this.areButtonsStacked_=t)},bn.prototype.detectScrollableContent_=function(){this.adapter_.removeClass(_n.SCROLLABLE),this.adapter_.isContentScrollable()&&this.adapter_.addClass(_n.SCROLLABLE)},bn);function bn(t){var e=hn.call(this,a(a({},bn.defaultAdapter),t))||this;return e.isOpen_=!1,e.animationFrame_=0,e.animationTimer_=0,e.layoutFrame_=0,e.escapeKeyAction_=yn.CLOSE_ACTION,e.scrimClickAction_=yn.CLOSE_ACTION,e.autoStackButtons_=!0,e.areButtonsStacked_=!1,e}n(76);var En=n(24),gn=n.n(En);
var Cn,An=vn.strings,Tn=(r(In,Cn=u),Object.defineProperty(In.prototype,"isOpen",{get:function(){return this.foundation_.isOpen()},enumerable:!1,configurable:!0}),Object.defineProperty(In.prototype,"escapeKeyAction",{get:function(){return this.foundation_.getEscapeKeyAction()},set:function(t){this.foundation_.setEscapeKeyAction(t)},enumerable:!1,configurable:!0}),Object.defineProperty(In.prototype,"scrimClickAction",{get:function(){return this.foundation_.getScrimClickAction()},set:function(t){this.foundation_.setScrimClickAction(t)},enumerable:!1,configurable:!0}),Object.defineProperty(In.prototype,"autoStackButtons",{get:function(){return this.foundation_.getAutoStackButtons()},set:function(t){this.foundation_.setAutoStackButtons(t)},enumerable:!1,configurable:!0}),In.attachTo=function(t){return new In(t)},In.prototype.initialize=function(t){var e=this.root_.querySelector(An.CONTAINER_SELECTOR);if(!e)throw new Error("Dialog component requires a "+An.CONTAINER_SELECTOR+" container element");this.container_=e,this.content_=this.root_.querySelector(An.CONTENT_SELECTOR),this.buttons_=[].slice.call(this.root_.querySelectorAll(An.BUTTON_SELECTOR)),this.focusTrapFactory_=t},In.prototype.initialSyncWithDOM=function(){var t,e,n,i=this;this.focusTrap_=(t=this.container_,e=this.focusTrapFactory_,n=this.getInitialFocusEl_()||void 0,void 0===e&&(e=gn.a),e(t,{clickOutsideDeactivates:!0,escapeDeactivates:!1,initialFocus:n})),this.handleClick_=this.handleClick.bind(this),this.handleDocumentKeydown_=this.handleDocumentKeydown.bind(this),this.handleLayout_=this.layout.bind(this);var r=["resize","orientationchange"];this.handleOpening_=function(){r.forEach(function(t){return window.addEventListener(t,i.handleLayout_)}),document.addEventListener("keydown",i.handleDocumentKeydown_)},this.handleClosing_=function(){r.forEach(function(t){return window.removeEventListener(t,i.handleLayout_)}),document.removeEventListener("keydown",i.handleDocumentKeydown_)},this.listen("click",this.handleClick_),this.listen(An.OPENING_EVENT,this.handleOpening_),this.listen(An.CLOSING_EVENT,this.handleClosing_)},In.prototype.handleClick=function(t){this.eventTargetMatches(t.target,An.SCRIM_SELECTOR)&&this.emit("MDCDialog:close",{})},In.prototype.eventTargetMatches=function(t,e){return!!t&&h(t,e)},In.prototype.handleDocumentKeydown=function(t){"Escape"!==t.key&&27!==t.keyCode||this.emit("MDCDialog:close",{})},In.prototype.destroy=function(){this.unlisten("click",this.handleClick_),this.unlisten(An.OPENING_EVENT,this.handleOpening_),this.unlisten(An.CLOSING_EVENT,this.handleClosing_),this.handleClosing_(),Cn.prototype.destroy.call(this)},In.prototype.layout=function(){this.foundation_.layout()},In.prototype.open=function(){this.foundation_.open()},In.prototype.close=function(t){void 0===t&&(t=""),this.foundation_.close(t)},In.prototype.getDefaultFoundation=function(){var n=this;return new vn({addBodyClass:function(t){return document.body.classList.add(t)},addClass:function(t){return n.root_.classList.add(t)},areButtonsStacked:function(){return t=n.buttons_,e=new Set,[].forEach.call(t,function(t){return e.add(t.offsetTop)}),1<e.size;var t,e},clickDefaultButton:function(){},eventTargetMatches:function(){return!1},getActionFromEvent:function(){return null},getInitialFocusEl:function(){return n.getInitialFocusEl_()},hasClass:function(t){return n.root_.classList.contains(t)},isContentScrollable:function(){return!!(t=n.content_)&&t.scrollHeight>t.offsetHeight;var t},notifyClosed:function(t){return n.emit(An.CLOSED_EVENT,t?{action:t}:{})},notifyClosing:function(t){return n.emit(An.CLOSING_EVENT,t?{action:t}:{})},notifyOpened:function(){return n.emit(An.OPENED_EVENT,{})},notifyOpening:function(){return n.emit(An.OPENING_EVENT,{})},releaseFocus:function(){return n.focusTrap_.deactivate()},removeBodyClass:function(t){return document.body.classList.remove(t)},removeClass:function(t){return n.root_.classList.remove(t)},reverseButtons:function(){n.buttons_.reverse(),n.buttons_.forEach(function(t){t.parentElement.appendChild(t)})},trapFocus:function(){return n.focusTrap_.activate()}})},In.prototype.getInitialFocusEl_=function(){return document.querySelector("["+An.INITIAL_FOCUS_ATTRIBUTE+"]")},In);function In(){return null!==Cn&&Cn.apply(this,arguments)||this}function Sn(t){return(Sn="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function On(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Rn(t,e,n){return e&&On(t.prototype,e),n&&On(t,n),t}function Ln(o){var a=Nn();return function(){var t,e,n,i=Pn(o);if(a){var r=Pn(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Sn(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function wn(t){var i="function"==typeof Map?new Map:void 0;return(wn=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return xn(t,arguments,Pn(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),Dn(n,t)})(t)}function xn(t,e,n){return(xn=Nn()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&Dn(r,n.prototype),r}).apply(null,arguments)}function Nn(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function Dn(t,e){return(Dn=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Pn(t){return(Pn=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var kn=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&Dn(t,e)}(n,wn(HTMLElement));var e=Ln(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).open_=!1,t}return Rn(n,[{key:"open",get:function(){return this.open_},set:function(t){this.open_=t,this.dialog_&&(t?this.dialog_.open():this.dialog_.close())}}]),Rn(n,[{key:"connectedCallback",value:function(){k.call(this),this.dialog_=new Tn(this),this.open_&&this.dialog_.open()}},{key:"disconnectedCallback",value:function(){this.dialog_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-dialog",kn);
var Fn={LIST_ITEM_ACTIVATED_CLASS:"mdc-list-item--activated",LIST_ITEM_CLASS:"mdc-list-item",LIST_ITEM_DISABLED_CLASS:"mdc-list-item--disabled",LIST_ITEM_SELECTED_CLASS:"mdc-list-item--selected",ROOT:"mdc-list"},Mn={ACTION_EVENT:"MDCList:action",ARIA_CHECKED:"aria-checked",ARIA_CHECKED_CHECKBOX_SELECTOR:'[role="checkbox"][aria-checked="true"]',ARIA_CHECKED_RADIO_SELECTOR:'[role="radio"][aria-checked="true"]',ARIA_CURRENT:"aria-current",ARIA_DISABLED:"aria-disabled",ARIA_ORIENTATION:"aria-orientation",ARIA_ORIENTATION_HORIZONTAL:"horizontal",ARIA_ROLE_CHECKBOX_SELECTOR:'[role="checkbox"]',ARIA_SELECTED:"aria-selected",CHECKBOX_RADIO_SELECTOR:'input[type="checkbox"], input[type="radio"]',CHECKBOX_SELECTOR:'input[type="checkbox"]',CHILD_ELEMENTS_TO_TOGGLE_TABINDEX:"\n    ."+Fn.LIST_ITEM_CLASS+" button:not(:disabled),\n    ."+Fn.LIST_ITEM_CLASS+" a\n  ",FOCUSABLE_CHILD_ELEMENTS:"\n    ."+Fn.LIST_ITEM_CLASS+" button:not(:disabled),\n    ."+Fn.LIST_ITEM_CLASS+" a,\n    ."+Fn.LIST_ITEM_CLASS+' input[type="radio"]:not(:disabled),\n    .'+Fn.LIST_ITEM_CLASS+' input[type="checkbox"]:not(:disabled)\n  ',RADIO_SELECTOR:'input[type="radio"]'},Hn={UNSET_INDEX:-1},jn=["input","button","textarea","select"];var Bn,Vn=(r(Un,Bn=s),Object.defineProperty(Un,"strings",{get:function(){return Mn},enumerable:!0,configurable:!0}),Object.defineProperty(Un,"cssClasses",{get:function(){return Fn},enumerable:!0,configurable:!0}),Object.defineProperty(Un,"numbers",{get:function(){return Hn},enumerable:!0,configurable:!0}),Object.defineProperty(Un,"defaultAdapter",{get:function(){return{addClassForElementIndex:function(){},focusItemAtIndex:function(){},getAttributeForElementIndex:function(){return null},getFocusedElementIndex:function(){return 0},getListItemCount:function(){return 0},hasCheckboxAtIndex:function(){return!1},hasRadioAtIndex:function(){return!1},isCheckboxCheckedAtIndex:function(){return!1},isFocusInsideList:function(){return!1},isRootFocused:function(){return!1},listItemAtIndexHasClass:function(){return!1},notifyAction:function(){},removeClassForElementIndex:function(){},setAttributeForElementIndex:function(){},setCheckedCheckboxOrRadioAtIndex:function(){},setTabIndexForListItemChildren:function(){}}},enumerable:!0,configurable:!0}),Un.prototype.layout=function(){0!==this.adapter_.getListItemCount()&&(this.adapter_.hasCheckboxAtIndex(0)?this.isCheckboxList_=!0:this.adapter_.hasRadioAtIndex(0)&&(this.isRadioList_=!0))},Un.prototype.setWrapFocus=function(t){this.wrapFocus_=t},Un.prototype.setVerticalOrientation=function(t){this.isVertical_=t},Un.prototype.setSingleSelection=function(t){this.isSingleSelectionList_=t},Un.prototype.setUseActivatedClass=function(t){this.useActivatedClass_=t},Un.prototype.getSelectedIndex=function(){return this.selectedIndex_},Un.prototype.setSelectedIndex=function(t){this.isIndexValid_(t)&&(this.isCheckboxList_?this.setCheckboxAtIndex_(t):this.isRadioList_?this.setRadioAtIndex_(t):this.setSingleSelectionAtIndex_(t))},Un.prototype.handleFocusIn=function(t,e){0<=e&&this.adapter_.setTabIndexForListItemChildren(e,"0")},Un.prototype.handleFocusOut=function(t,e){var n=this;0<=e&&this.adapter_.setTabIndexForListItemChildren(e,"-1"),setTimeout(function(){n.adapter_.isFocusInsideList()||n.setTabindexToFirstSelectedItem_()},0)},Un.prototype.handleKeydown=function(t,e,n){var i="ArrowLeft"===t.key||37===t.keyCode,r="ArrowUp"===t.key||38===t.keyCode,o="ArrowRight"===t.key||39===t.keyCode,a="ArrowDown"===t.key||40===t.keyCode,s="Home"===t.key||36===t.keyCode,c="End"===t.key||35===t.keyCode,u="Enter"===t.key||13===t.keyCode,l="Space"===t.key||32===t.keyCode;if(this.adapter_.isRootFocused())r||c?(t.preventDefault(),this.focusLastElement()):(a||s)&&(t.preventDefault(),this.focusFirstElement());else{var f=this.adapter_.getFocusedElementIndex();if(!(-1===f&&(f=n)<0)){var d;if(this.isVertical_&&a||!this.isVertical_&&o)this.preventDefaultEvent_(t),d=this.focusNextElement(f);else if(this.isVertical_&&r||!this.isVertical_&&i)this.preventDefaultEvent_(t),d=this.focusPrevElement(f);else if(s)this.preventDefaultEvent_(t),d=this.focusFirstElement();else if(c)this.preventDefaultEvent_(t),d=this.focusLastElement();else if((u||l)&&e){var p=t.target;if(p&&"A"===p.tagName&&u)return;if(this.preventDefaultEvent_(t),this.adapter_.listItemAtIndexHasClass(f,Fn.LIST_ITEM_DISABLED_CLASS))return;this.isSelectableList_()&&this.setSelectedIndexOnAction_(f),this.adapter_.notifyAction(f)}this.focusedItemIndex_=f,void 0!==d&&(this.setTabindexAtIndex_(d),this.focusedItemIndex_=d)}}},Un.prototype.handleClick=function(t,e){t!==Hn.UNSET_INDEX&&(this.setTabindexAtIndex_(t),this.focusedItemIndex_=t,this.adapter_.listItemAtIndexHasClass(t,Fn.LIST_ITEM_DISABLED_CLASS)||(this.isSelectableList_()&&this.setSelectedIndexOnAction_(t,e),this.adapter_.notifyAction(t)))},Un.prototype.focusNextElement=function(t){var e=t+1;if(this.adapter_.getListItemCount()<=e){if(!this.wrapFocus_)return t;e=0}return this.adapter_.focusItemAtIndex(e),e},Un.prototype.focusPrevElement=function(t){var e=t-1;if(e<0){if(!this.wrapFocus_)return t;e=this.adapter_.getListItemCount()-1}return this.adapter_.focusItemAtIndex(e),e},Un.prototype.focusFirstElement=function(){return this.adapter_.focusItemAtIndex(0),0},Un.prototype.focusLastElement=function(){var t=this.adapter_.getListItemCount()-1;return this.adapter_.focusItemAtIndex(t),t},Un.prototype.setEnabled=function(t,e){this.isIndexValid_(t)&&(e?(this.adapter_.removeClassForElementIndex(t,Fn.LIST_ITEM_DISABLED_CLASS),this.adapter_.setAttributeForElementIndex(t,Mn.ARIA_DISABLED,"false")):(this.adapter_.addClassForElementIndex(t,Fn.LIST_ITEM_DISABLED_CLASS),this.adapter_.setAttributeForElementIndex(t,Mn.ARIA_DISABLED,"true")))},Un.prototype.preventDefaultEvent_=function(t){var e=(""+t.target.tagName).toLowerCase();-1===jn.indexOf(e)&&t.preventDefault()},Un.prototype.setSingleSelectionAtIndex_=function(t){if(this.selectedIndex_!==t){var e=Fn.LIST_ITEM_SELECTED_CLASS;this.useActivatedClass_&&(e=Fn.LIST_ITEM_ACTIVATED_CLASS),this.selectedIndex_!==Hn.UNSET_INDEX&&this.adapter_.removeClassForElementIndex(this.selectedIndex_,e),this.adapter_.addClassForElementIndex(t,e),this.setAriaForSingleSelectionAtIndex_(t),this.selectedIndex_=t}},Un.prototype.setAriaForSingleSelectionAtIndex_=function(t){this.selectedIndex_===Hn.UNSET_INDEX&&(this.ariaCurrentAttrValue_=this.adapter_.getAttributeForElementIndex(t,Mn.ARIA_CURRENT));var e=null!==this.ariaCurrentAttrValue_,n=e?Mn.ARIA_CURRENT:Mn.ARIA_SELECTED;this.selectedIndex_!==Hn.UNSET_INDEX&&this.adapter_.setAttributeForElementIndex(this.selectedIndex_,n,"false");var i=e?this.ariaCurrentAttrValue_:"true";this.adapter_.setAttributeForElementIndex(t,n,i)},Un.prototype.setRadioAtIndex_=function(t){this.adapter_.setCheckedCheckboxOrRadioAtIndex(t,!0),this.selectedIndex_!==Hn.UNSET_INDEX&&this.adapter_.setAttributeForElementIndex(this.selectedIndex_,Mn.ARIA_CHECKED,"false"),this.adapter_.setAttributeForElementIndex(t,Mn.ARIA_CHECKED,"true"),this.selectedIndex_=t},Un.prototype.setCheckboxAtIndex_=function(t){for(var e=0;e<this.adapter_.getListItemCount();e++){var n=!1;0<=t.indexOf(e)&&(n=!0),this.adapter_.setCheckedCheckboxOrRadioAtIndex(e,n),this.adapter_.setAttributeForElementIndex(e,Mn.ARIA_CHECKED,n?"true":"false")}this.selectedIndex_=t},Un.prototype.setTabindexAtIndex_=function(t){this.focusedItemIndex_===Hn.UNSET_INDEX&&0!==t?this.adapter_.setAttributeForElementIndex(0,"tabindex","-1"):0<=this.focusedItemIndex_&&this.focusedItemIndex_!==t&&this.adapter_.setAttributeForElementIndex(this.focusedItemIndex_,"tabindex","-1"),this.adapter_.setAttributeForElementIndex(t,"tabindex","0")},Un.prototype.isSelectableList_=function(){return this.isSingleSelectionList_||this.isCheckboxList_||this.isRadioList_},Un.prototype.setTabindexToFirstSelectedItem_=function(){var t=0;this.isSelectableList_()&&("number"==typeof this.selectedIndex_&&this.selectedIndex_!==Hn.UNSET_INDEX?t=this.selectedIndex_:this.selectedIndex_ instanceof Array&&0<this.selectedIndex_.length&&(t=this.selectedIndex_.reduce(function(t,e){return Math.min(t,e)}))),this.setTabindexAtIndex_(t)},Un.prototype.isIndexValid_=function(t){var e=this;if(t instanceof Array){if(!this.isCheckboxList_)throw new Error("MDCListFoundation: Array of index is only supported for checkbox based list");return 0===t.length||t.some(function(t){return e.isIndexInRange_(t)})}if("number"!=typeof t)return!1;if(this.isCheckboxList_)throw new Error("MDCListFoundation: Expected array of index for checkbox based list but got number: "+t);return this.isIndexInRange_(t)},Un.prototype.isIndexInRange_=function(t){var e=this.adapter_.getListItemCount();return 0<=t&&t<e},Un.prototype.setSelectedIndexOnAction_=function(t,e){void 0===e&&(e=!0),this.isCheckboxList_?this.toggleCheckboxAtIndex_(t,e):this.setSelectedIndex(t)},Un.prototype.toggleCheckboxAtIndex_=function(e,t){var n=this.adapter_.isCheckboxCheckedAtIndex(e);t&&(n=!n,this.adapter_.setCheckedCheckboxOrRadioAtIndex(e,n)),this.adapter_.setAttributeForElementIndex(e,Mn.ARIA_CHECKED,n?"true":"false");var i=this.selectedIndex_===Hn.UNSET_INDEX?[]:this.selectedIndex_.slice();n?i.push(e):i=i.filter(function(t){return t!==e}),this.selectedIndex_=i},Un);function Un(t){var e=Bn.call(this,a(a({},Un.defaultAdapter),t))||this;return e.wrapFocus_=!1,e.isVertical_=!0,e.isSingleSelectionList_=!1,e.selectedIndex_=Hn.UNSET_INDEX,e.focusedItemIndex_=Hn.UNSET_INDEX,e.useActivatedClass_=!1,e.ariaCurrentAttrValue_=null,e.isCheckboxList_=!1,e.isRadioList_=!1,e}var Kn,Gn={ANIMATE:"mdc-drawer--animate",CLOSING:"mdc-drawer--closing",DISMISSIBLE:"mdc-drawer--dismissible",MODAL:"mdc-drawer--modal",OPEN:"mdc-drawer--open",OPENING:"mdc-drawer--opening",ROOT:"mdc-drawer"},Wn={APP_CONTENT_SELECTOR:".mdc-drawer-app-content",CLOSE_EVENT:"MDCDrawer:closed",OPEN_EVENT:"MDCDrawer:opened",SCRIM_SELECTOR:".mdc-drawer-scrim"},qn=(r(zn,Kn=s),Object.defineProperty(zn,"strings",{get:function(){return Wn},enumerable:!1,configurable:!0}),Object.defineProperty(zn,"cssClasses",{get:function(){return Gn},enumerable:!1,configurable:!0}),Object.defineProperty(zn,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},elementHasClass:function(){return!1},notifyClose:function(){},notifyOpen:function(){},saveFocus:function(){},restoreFocus:function(){},focusActiveNavigationItem:function(){},trapFocus:function(){},releaseFocus:function(){}}},enumerable:!1,configurable:!0}),zn.prototype.destroy=function(){this.animationFrame_&&cancelAnimationFrame(this.animationFrame_),this.animationTimer_&&clearTimeout(this.animationTimer_)},zn.prototype.open=function(){var t=this;this.isOpen()||this.isOpening()||this.isClosing()||(this.adapter_.addClass(Gn.OPEN),this.adapter_.addClass(Gn.ANIMATE),this.runNextAnimationFrame_(function(){t.adapter_.addClass(Gn.OPENING)}),this.adapter_.saveFocus())},zn.prototype.close=function(){!this.isOpen()||this.isOpening()||this.isClosing()||this.adapter_.addClass(Gn.CLOSING)},zn.prototype.isOpen=function(){return this.adapter_.hasClass(Gn.OPEN)},zn.prototype.isOpening=function(){return this.adapter_.hasClass(Gn.OPENING)||this.adapter_.hasClass(Gn.ANIMATE)},zn.prototype.isClosing=function(){return this.adapter_.hasClass(Gn.CLOSING)},zn.prototype.handleKeydown=function(t){var e=t.keyCode;"Escape"!==t.key&&27!==e||this.close()},zn.prototype.handleTransitionEnd=function(t){var e=Gn.OPENING,n=Gn.CLOSING,i=Gn.OPEN,r=Gn.ANIMATE,o=Gn.ROOT;this.isElement_(t.target)&&this.adapter_.elementHasClass(t.target,o)&&(this.isClosing()?(this.adapter_.removeClass(i),this.closed_(),this.adapter_.restoreFocus(),this.adapter_.notifyClose()):(this.adapter_.focusActiveNavigationItem(),this.opened_(),this.adapter_.notifyOpen()),this.adapter_.removeClass(r),this.adapter_.removeClass(e),this.adapter_.removeClass(n))},zn.prototype.opened_=function(){},zn.prototype.closed_=function(){},zn.prototype.runNextAnimationFrame_=function(t){var e=this;cancelAnimationFrame(this.animationFrame_),this.animationFrame_=requestAnimationFrame(function(){e.animationFrame_=0,clearTimeout(e.animationTimer_),e.animationTimer_=setTimeout(t,0)})},zn.prototype.isElement_=function(t){return Boolean(t.classList)},zn);
   function zn(t){var e=Kn.call(this,a(a({},zn.defaultAdapter),t))||this;return e.animationFrame_=0,e.animationTimer_=0,e}var Xn,Yn=(r(Qn,Xn=qn),Qn.prototype.handleScrimClick=function(){this.close()},Qn.prototype.opened_=function(){this.adapter_.trapFocus()},Qn.prototype.closed_=function(){this.adapter_.releaseFocus()},Qn);
   function Qn(){return null!==Xn&&Xn.apply(this,arguments)||this}
var Zn,$n=qn.cssClasses,Jn=qn.strings,ti=(r(ei,Zn=u),ei.attachTo=function(t){return new ei(t)},Object.defineProperty(ei.prototype,"open",{get:function(){return this.foundation_.isOpen()},set:function(t){t?this.foundation_.open():this.foundation_.close()},enumerable:!1,configurable:!0}),Object.defineProperty(ei.prototype,"list",{get:function(){return this.list_},enumerable:!1,configurable:!0}),ei.prototype.initialize=function(t){void 0===t&&(t=gn.a),this.focusTrapFactory_=t},ei.prototype.initialSyncWithDOM=function(){var t,e,n=this,i=$n.MODAL,r=Jn.SCRIM_SELECTOR;this.scrim_=this.root_.parentNode.querySelector(r),this.scrim_&&this.root_.classList.contains(i)&&(this.handleScrimClick_=function(){return n.handleScrimClick()},this.scrim_.addEventListener("click",this.handleScrimClick_),this.focusTrap_=(t=this.root_,void 0===(e=this.focusTrapFactory_)&&(e=gn.a),e(t,{clickOutsideDeactivates:!0,escapeDeactivates:!1,initialFocus:void 0,returnFocusOnDeactivate:!1}))),this.handleKeydown_=function(t){return n.handleKeydown(t)},this.handleTransitionEnd_=function(t){return n.foundation_.handleTransitionEnd(t)},this.listen("keydown",this.handleKeydown_),this.listen("transitionend",this.handleTransitionEnd_)},ei.prototype.handleScrimClick=function(){this.emit("MDCDrawer:close",{},!0)},ei.prototype.handleKeydown=function(t){var e=t.keyCode;"Escape"!==t.key&&27!==e||this.emit("MDCDrawer:close",{},!0)},ei.prototype.destroy=function(){this.unlisten("keydown",this.handleKeydown_),this.unlisten("transitionend",this.handleTransitionEnd_);var t=$n.MODAL;this.scrim_&&this.handleScrimClick_&&this.root_.classList.contains(t)&&(this.scrim_.removeEventListener("click",this.handleScrimClick_),this.open=!1)},ei.prototype.getDefaultFoundation=function(){var e=this,t={addClass:function(t){return e.root_.classList.add(t)},removeClass:function(t){return e.root_.classList.remove(t)},hasClass:function(t){return e.root_.classList.contains(t)},elementHasClass:function(t,e){return t.classList.contains(e)},saveFocus:function(){return e.previousFocus_=document.activeElement},restoreFocus:function(){var t=e.previousFocus_;t&&t.focus&&e.root_.contains(document.activeElement)&&t.focus()},focusActiveNavigationItem:function(){var t=e.root_.querySelector("."+Vn.cssClasses.LIST_ITEM_ACTIVATED_CLASS);t&&t.focus()},notifyClose:function(){return e.emit(Jn.CLOSE_EVENT,{},!0)},notifyOpen:function(){return e.emit(Jn.OPEN_EVENT,{},!0)},trapFocus:function(){return e.focusTrap_.activate()},releaseFocus:function(){return e.focusTrap_.deactivate()}},n=$n.DISMISSIBLE,i=$n.MODAL;if(this.root_.classList.contains(n))return new qn(t);if(this.root_.classList.contains(i))return new Yn(t);throw new Error("MDCDrawer: Failed to instantiate component. Supported variants are "+n+" and "+i+".")},ei);function ei(){return null!==Zn&&Zn.apply(this,arguments)||this}function ni(t){return(ni="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function ii(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function ri(t,e,n){return e&&ii(t.prototype,e),n&&ii(t,n),t}function oi(o){var a=ci();return function(){var t,e,n,i=li(o);if(a){var r=li(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==ni(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function ai(t){var i="function"==typeof Map?new Map:void 0;return(ai=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return si(t,arguments,li(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),ui(n,t)})(t)}function si(t,e,n){return(si=ci()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&ui(r,n.prototype),r}).apply(null,arguments)}function ci(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function ui(t,e){return(ui=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function li(t){return(li=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var fi=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&ui(t,e)}(n,ai(HTMLElement));var e=oi(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).open_=!1,t.drawer_,t}return ri(n,[{key:"open",get:function(){return this.open_},set:function(t){this.open_=t,this.drawer_&&(this.drawer_.open=t)}}]),ri(n,[{key:"connectedCallback",value:function(){k.call(this),this.drawer_=new ti(this),this.drawer_.open=this.open_}},{key:"disconnectedCallback",value:function(){this.drawer_.destroy(),F.call(this)}}]),n}();function di(t){return(di="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function pi(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function hi(o){var a=mi();return function(){var t,e,n,i=bi(o);if(a){var r=bi(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==di(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function _i(t){var i="function"==typeof Map?new Map:void 0;return(_i=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return yi(t,arguments,bi(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),vi(n,t)})(t)}function yi(t,e,n){return(yi=mi()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&vi(r,n.prototype),r}).apply(null,arguments)}function mi(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function vi(t,e){return(vi=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function bi(t){return(bi=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-drawer",fi);var Ei=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&vi(t,e)}(r,_i(HTMLElement));var t,e,n,i=hi(r);function r(){return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,r),i.call(this)}return t=r,(e=[{key:"connectedCallback",value:function(){k.call(this),this.ripple_=new R(this)}},{key:"disconnectedCallback",value:function(){this.ripple_.destroy(),F.call(this)}}])&&pi(t.prototype,e),n&&pi(t,n),r}();customElements.define("mdc-fab",Ei);
var gi,Ci={ROOT:"mdc-form-field"},Ai={LABEL_SELECTOR:".mdc-form-field > label"},Ti=(r(Ii,gi=s),Object.defineProperty(Ii,"cssClasses",{get:function(){return Ci},enumerable:!1,configurable:!0}),Object.defineProperty(Ii,"strings",{get:function(){return Ai},enumerable:!1,configurable:!0}),Object.defineProperty(Ii,"defaultAdapter",{get:function(){return{activateInputRipple:function(){},deactivateInputRipple:function(){},deregisterInteractionHandler:function(){},registerInteractionHandler:function(){}}},enumerable:!1,configurable:!0}),Ii.prototype.init=function(){this.adapter_.registerInteractionHandler("click",this.clickHandler_)},Ii.prototype.destroy=function(){this.adapter_.deregisterInteractionHandler("click",this.clickHandler_)},Ii.prototype.handleClick_=function(){var t=this;this.adapter_.activateInputRipple(),requestAnimationFrame(function(){return t.adapter_.deactivateInputRipple()})},Ii);function Ii(t){var e=gi.call(this,a(a({},Ii.defaultAdapter),t))||this;return e.clickHandler_=function(){return e.handleClick_()},e}var Si,Oi=(r(Ri,Si=u),Ri.attachTo=function(t){return new Ri(t)},Object.defineProperty(Ri.prototype,"input",{get:function(){return this.input_},set:function(t){this.input_=t},enumerable:!1,configurable:!0}),Object.defineProperty(Ri.prototype,"label_",{get:function(){var t=Ti.strings.LABEL_SELECTOR;return this.root_.querySelector(t)},enumerable:!1,configurable:!0}),Ri.prototype.getDefaultFoundation=function(){var n=this;return new Ti({activateInputRipple:function(){n.input_&&n.input_.ripple&&n.input_.ripple.activate()},deactivateInputRipple:function(){n.input_&&n.input_.ripple&&n.input_.ripple.deactivate()},deregisterInteractionHandler:function(t,e){n.label_&&n.label_.removeEventListener(t,e)},registerInteractionHandler:function(t,e){n.label_&&n.label_.addEventListener(t,e)}})},Ri);
   function Ri(){return null!==Si&&Si.apply(this,arguments)||this}function Li(t){return(Li="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function wi(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function xi(t,e,n){return e&&wi(t.prototype,e),n&&wi(t,n),t}function Ni(o){var a=ki();return function(){var t,e,n,i=Mi(o);if(a){var r=Mi(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Li(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Di(t){var i="function"==typeof Map?new Map:void 0;return(Di=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Pi(t,arguments,Mi(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),Fi(n,t)})(t)}function Pi(t,e,n){return(Pi=ki()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&Fi(r,n.prototype),r}).apply(null,arguments)}function ki(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function Fi(t,e){return(Fi=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Mi(t){return(Mi=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var Hi=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&Fi(t,e)}(n,Di(HTMLElement));var e=Ni(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).formField_,t}return xi(n,[{key:"focus",value:function(){this.formField_.input&&this.formField_.input.focus()}},{key:"blur",value:function(){this.formField_.input&&this.formField_.input.blur()}}]),xi(n,[{key:"connectedCallback",value:function(){this.formField_=new Oi(this);var t=this.querySelector("input");t&&(this.formField_.input=t)}},{key:"disconnectedCallback",value:function(){this.formField_.destroy()}}]),n}();customElements.define("mdc-form-field",Hi);
var ji,Bi={ICON_BUTTON_ON:"mdc-icon-button--on",ROOT:"mdc-icon-button"},Vi={ARIA_LABEL:"aria-label",ARIA_PRESSED:"aria-pressed",DATA_ARIA_LABEL_OFF:"data-aria-label-off",DATA_ARIA_LABEL_ON:"data-aria-label-on",CHANGE_EVENT:"MDCIconButtonToggle:change"},Ui=(r(Ki,ji=s),Object.defineProperty(Ki,"cssClasses",{get:function(){return Bi},enumerable:!1,configurable:!0}),Object.defineProperty(Ki,"strings",{get:function(){return Vi},enumerable:!1,configurable:!0}),Object.defineProperty(Ki,"defaultAdapter",{get:function(){return{addClass:function(){},hasClass:function(){return!1},notifyChange:function(){},removeClass:function(){},getAttr:function(){return null},setAttr:function(){}}},enumerable:!1,configurable:!0}),Ki.prototype.init=function(){var t=this.adapter_.getAttr(Vi.DATA_ARIA_LABEL_ON),e=this.adapter_.getAttr(Vi.DATA_ARIA_LABEL_OFF);if(t&&e){if(null!==this.adapter_.getAttr(Vi.ARIA_PRESSED))throw new Error("MDCIconButtonToggleFoundation: Button should not set `aria-pressed` if it has a toggled aria label.");this.hasToggledAriaLabel=!0}else this.adapter_.setAttr(Vi.ARIA_PRESSED,String(this.isOn()))},Ki.prototype.handleClick=function(){this.toggle(),this.adapter_.notifyChange({isOn:this.isOn()})},Ki.prototype.isOn=function(){return this.adapter_.hasClass(Bi.ICON_BUTTON_ON)},Ki.prototype.toggle=function(t){if(void 0===t&&(t=!this.isOn()),t?this.adapter_.addClass(Bi.ICON_BUTTON_ON):this.adapter_.removeClass(Bi.ICON_BUTTON_ON),this.hasToggledAriaLabel){var e=t?this.adapter_.getAttr(Vi.DATA_ARIA_LABEL_ON):this.adapter_.getAttr(Vi.DATA_ARIA_LABEL_OFF);this.adapter_.setAttr(Vi.ARIA_LABEL,e||"")}else this.adapter_.setAttr(Vi.ARIA_PRESSED,""+t)},Ki);function Ki(t){var e=ji.call(this,a(a({},Ki.defaultAdapter),t))||this;return e.hasToggledAriaLabel=!1,e}var Gi,Wi=Ui.strings,qi=(r(zi,Gi=u),zi.attachTo=function(t){return new zi(t)},zi.prototype.initialSyncWithDOM=function(){this.handleClick_=this.handleClick.bind(this),this.listen("click",this.handleClick_)},zi.prototype.destroy=function(){this.unlisten("click",this.handleClick_),this.ripple_.destroy(),Gi.prototype.destroy.call(this)},zi.prototype.getDefaultFoundation=function(){var n=this;return new Ui({addClass:function(t){return n.root_.classList.add(t)},hasClass:function(t){return n.root_.classList.contains(t)},notifyChange:function(){},removeClass:function(t){return n.root_.classList.remove(t)},getAttr:function(t){return n.root_.getAttribute(t)},setAttr:function(t,e){return n.root_.setAttribute(t,e)}})},zi.prototype.handleClick=function(){this.emit(Wi.CHANGE_EVENT,{isOn:!this.on})},Object.defineProperty(zi.prototype,"ripple",{get:function(){return this.ripple_},enumerable:!1,configurable:!0}),Object.defineProperty(zi.prototype,"on",{get:function(){return this.foundation_.isOn()},set:function(t){this.foundation_.toggle(t)},enumerable:!1,configurable:!0}),zi.prototype.createRipple_=function(){var t=new _t(this.root_);return t.unbounded=!0,t},zi);
   function zi(){var t=null!==Gi&&Gi.apply(this,arguments)||this;return t.ripple_=t.createRipple_(),t}function Xi(t){return(Xi="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Yi(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Qi(t,e,n){return e&&Yi(t.prototype,e),n&&Yi(t,n),t}function Zi(o){var a=tr();return function(){var t,e,n,i=nr(o);if(a){var r=nr(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Xi(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function $i(t){var i="function"==typeof Map?new Map:void 0;return($i=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Ji(t,arguments,nr(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),er(n,t)})(t)}function Ji(t,e,n){return(Ji=tr()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&er(r,n.prototype),r}).apply(null,arguments)}function tr(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function er(t,e){return(er=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function nr(t){return(nr=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var ir=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&er(t,e)}(n,$i(HTMLElement));var e=Zi(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).on_=!1,t.iconButtonToggle_,t}return Qi(n,[{key:"on",get:function(){return this.on_},set:function(t){this.on_=t,this.iconButtonToggle_&&(this.iconButtonToggle_.on=t)}}]),Qi(n,[{key:"connectedCallback",value:function(){k.call(this),this.iconButtonToggle_=new qi(this),this.iconButtonToggle_.on=this.on_}},{key:"disconnectedCallback",value:function(){this.iconButtonToggle_.destroy(),F.call(this)}}]),n}();function rr(t){return(rr="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function or(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function ar(o){var a=ur();return function(){var t,e,n,i=fr(o);if(a){var r=fr(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==rr(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function sr(t){var i="function"==typeof Map?new Map:void 0;return(sr=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return cr(t,arguments,fr(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),lr(n,t)})(t)}function cr(t,e,n){return(cr=ur()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&lr(r,n.prototype),r}).apply(null,arguments)}function ur(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function lr(t,e){return(lr=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function fr(t){return(fr=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-icon-button",ir);var dr=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&lr(t,e)}(r,sr(HTMLElement));var t,e,n,i=ar(r);function r(){return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,r),i.call(this)}return t=r,(e=[{key:"connectedCallback",value:function(){}},{key:"disconnectedCallback",value:function(){}}])&&or(t.prototype,e),n&&or(t,n),r}();function pr(t){return(pr="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function hr(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function _r(o){var a=vr();return function(){var t,e,n,i=Er(o);if(a){var r=Er(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==pr(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function yr(t){var i="function"==typeof Map?new Map:void 0;return(yr=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return mr(t,arguments,Er(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),br(n,t)})(t)}function mr(t,e,n){return(mr=vr()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&br(r,n.prototype),r}).apply(null,arguments)}function vr(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function br(t,e){return(br=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Er(t){return(Er=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-image-list",dr);var gr=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&br(t,e)}(r,yr(HTMLElement));var t,e,n,i=_r(r);function r(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,r),(t=i.call(this)).ripple_,t}return t=r,(e=[{key:"connectedCallback",value:function(){k.call(this);var t=this.querySelector(".mdc-ripple-surface");t&&(this.ripple_=new R(t))}},{key:"disconnectedCallback",value:function(){this.ripple_&&this.ripple_.destroy(),F.call(this)}}])&&hr(t.prototype,e),n&&hr(t,n),r}();function Cr(t){return(Cr="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Ar(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Tr(o){var a=Or();return function(){var t,e,n,i=Lr(o);if(a){var r=Lr(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Cr(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Ir(t){var i="function"==typeof Map?new Map:void 0;return(Ir=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Sr(t,arguments,Lr(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),Rr(n,t)})(t)}function Sr(t,e,n){return(Sr=Or()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&Rr(r,n.prototype),r}).apply(null,arguments)}function Or(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function Rr(t,e){return(Rr=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Lr(t){return(Lr=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-image-list-item",gr);var wr=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&Rr(t,e)}(r,Ir(HTMLElement));var t,e,n,i=Tr(r);function r(){return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,r),i.call(this)}return t=r,(e=[{key:"connectedCallback",value:function(){}},{key:"disconnectedCallback",value:function(){}}])&&Ar(t.prototype,e),n&&Ar(t,n),r}();customElements.define("mdc-layout-grid",wr);
var xr,Nr={CLOSED_CLASS:"mdc-linear-progress--closed",INDETERMINATE_CLASS:"mdc-linear-progress--indeterminate",REVERSED_CLASS:"mdc-linear-progress--reversed"},Dr={ARIA_VALUENOW:"aria-valuenow",BUFFER_BAR_SELECTOR:".mdc-linear-progress__buffer-bar",FLEX_BASIS:"flex-basis",PRIMARY_BAR_SELECTOR:".mdc-linear-progress__primary-bar"},Pr=(r(kr,xr=s),Object.defineProperty(kr,"cssClasses",{get:function(){return Nr},enumerable:!1,configurable:!0}),Object.defineProperty(kr,"strings",{get:function(){return Dr},enumerable:!1,configurable:!0}),Object.defineProperty(kr,"defaultAdapter",{get:function(){return{addClass:function(){},forceLayout:function(){},setBufferBarStyle:function(){return null},setPrimaryBarStyle:function(){return null},hasClass:function(){return!1},removeAttribute:function(){},removeClass:function(){},setAttribute:function(){}}},enumerable:!1,configurable:!0}),kr.prototype.init=function(){this.isDeterminate_=!this.adapter_.hasClass(Nr.INDETERMINATE_CLASS),this.isReversed_=this.adapter_.hasClass(Nr.REVERSED_CLASS),this.progress_=0,this.buffer_=1},kr.prototype.setDeterminate=function(t){if(this.isDeterminate_=t,this.isDeterminate_)return this.adapter_.removeClass(Nr.INDETERMINATE_CLASS),this.adapter_.setAttribute(Dr.ARIA_VALUENOW,this.progress_.toString()),this.setPrimaryBarProgress_(this.progress_),void this.setBufferBarProgress_(this.buffer_);this.isReversed_&&(this.adapter_.removeClass(Nr.REVERSED_CLASS),this.adapter_.forceLayout(),this.adapter_.addClass(Nr.REVERSED_CLASS)),this.adapter_.addClass(Nr.INDETERMINATE_CLASS),this.adapter_.removeAttribute(Dr.ARIA_VALUENOW),this.setPrimaryBarProgress_(1),this.setBufferBarProgress_(1)},kr.prototype.isDeterminate=function(){return this.isDeterminate_},kr.prototype.setProgress=function(t){this.progress_=t,this.isDeterminate_&&(this.setPrimaryBarProgress_(t),this.adapter_.setAttribute(Dr.ARIA_VALUENOW,t.toString()))},kr.prototype.getProgress=function(){return this.progress_},kr.prototype.setBuffer=function(t){this.buffer_=t,this.isDeterminate_&&this.setBufferBarProgress_(t)},kr.prototype.setReverse=function(t){this.isReversed_=t,this.isDeterminate_||(this.adapter_.removeClass(Nr.INDETERMINATE_CLASS),this.adapter_.forceLayout(),this.adapter_.addClass(Nr.INDETERMINATE_CLASS)),this.isReversed_?this.adapter_.addClass(Nr.REVERSED_CLASS):this.adapter_.removeClass(Nr.REVERSED_CLASS)},kr.prototype.open=function(){this.adapter_.removeClass(Nr.CLOSED_CLASS)},kr.prototype.close=function(){this.adapter_.addClass(Nr.CLOSED_CLASS)},kr.prototype.setPrimaryBarProgress_=function(t){var e="scaleX("+t+")",n="undefined"!=typeof window?at(window,"transform"):"transform";this.adapter_.setPrimaryBarStyle(n,e)},kr.prototype.setBufferBarProgress_=function(t){var e=100*t+"%";this.adapter_.setBufferBarStyle(Dr.FLEX_BASIS,e)},kr);function kr(t){return xr.call(this,a(a({},kr.defaultAdapter),t))||this}var Fr,Mr=(r(Hr,Fr=u),Hr.attachTo=function(t){return new Hr(t)},Object.defineProperty(Hr.prototype,"determinate",{set:function(t){this.foundation_.setDeterminate(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Hr.prototype,"progress",{set:function(t){this.foundation_.setProgress(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Hr.prototype,"buffer",{set:function(t){this.foundation_.setBuffer(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Hr.prototype,"reverse",{set:function(t){this.foundation_.setReverse(t)},enumerable:!1,configurable:!0}),Hr.prototype.open=function(){this.foundation_.open()},Hr.prototype.close=function(){this.foundation_.close()},Hr.prototype.getDefaultFoundation=function(){var n=this;return new Pr({addClass:function(t){return n.root_.classList.add(t)},forceLayout:function(){return n.root_.offsetWidth},setBufferBarStyle:function(t,e){n.root_.querySelector(Pr.strings.BUFFER_BAR_SELECTOR).style.setProperty(t,e)},setPrimaryBarStyle:function(t,e){n.root_.querySelector(Pr.strings.PRIMARY_BAR_SELECTOR).style.setProperty(t,e)},hasClass:function(t){return n.root_.classList.contains(t)},removeAttribute:function(t){n.root_.removeAttribute(t)},removeClass:function(t){return n.root_.classList.remove(t)},setAttribute:function(t,e){n.root_.setAttribute(t,e)}})},Hr);
   function Hr(){return null!==Fr&&Fr.apply(this,arguments)||this}function jr(t){return(jr="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Br(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Vr(t,e,n){return e&&Br(t.prototype,e),n&&Br(t,n),t}function Ur(o){var a=Wr();return function(){var t,e,n,i=zr(o);if(a){var r=zr(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==jr(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Kr(t){var i="function"==typeof Map?new Map:void 0;return(Kr=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Gr(t,arguments,zr(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),qr(n,t)})(t)}function Gr(t,e,n){return(Gr=Wr()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&qr(r,n.prototype),r}).apply(null,arguments)}function Wr(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function qr(t,e){return(qr=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function zr(t){return(zr=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var Xr=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&qr(t,e)}(n,Kr(HTMLElement));var e=Ur(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).determinate_=!1,t.progress_=0,t.buffer_=0,t.reverse_=!1,t.closed_=!1,t.linearProgress_,t}return Vr(n,[{key:"determinate",get:function(){return this.determinate_},set:function(t){this.determinate_=t,this.linearProgress_&&(this.linearProgress_.determinate=t)}},{key:"progress",get:function(){return this.progress_},set:function(t){this.progress_=t,this.linearProgress_&&(this.linearProgress_.progress=t)}},{key:"buffer",get:function(){return this.buffer_},set:function(t){this.buffer_=t,this.linearProgress_&&(this.linearProgress_.buffer=t)}},{key:"reverse",get:function(){return this.reverse_},set:function(t){this.reverse_=t,this.linearProgress_&&(this.linearProgress.reverse=reverse_)}},{key:"closed",get:function(){return this.closed_},set:function(t){this.closed_=t,this.linearProgress_&&(t?this.linearProgress_.close():this.linearProgress_.open())}}]),Vr(n,[{key:"connectedCallback",value:function(){k.call(this),this.linearProgress_=new Mr(this),this.linearProgress_.determinate=this.determinate_,this.linearProgress_.progress=this.progress_,this.linearProgress_.buffer=this.buffer_,this.linearProgress_.reverse=this.reverse_,this.closed_?this.linearProgress_.close():this.linearProgress_.open()}},{key:"disconnectedCallback",value:function(){this.linearProgress_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-linear-progress",Xr);
var Yr=["input","button","textarea","select"];var Qr,Zr=(r($r,Qr=s),Object.defineProperty($r,"strings",{get:function(){return Mn},enumerable:!1,configurable:!0}),Object.defineProperty($r,"cssClasses",{get:function(){return Fn},enumerable:!1,configurable:!0}),Object.defineProperty($r,"numbers",{get:function(){return Hn},enumerable:!1,configurable:!0}),Object.defineProperty($r,"defaultAdapter",{get:function(){return{addClassForElementIndex:function(){},focusItemAtIndex:function(){},getAttributeForElementIndex:function(){return null},getFocusedElementIndex:function(){return 0},getListItemCount:function(){return 0},hasCheckboxAtIndex:function(){return!1},hasRadioAtIndex:function(){return!1},isCheckboxCheckedAtIndex:function(){return!1},isFocusInsideList:function(){return!1},isRootFocused:function(){return!1},listItemAtIndexHasClass:function(){return!1},notifyAction:function(){},removeClassForElementIndex:function(){},setAttributeForElementIndex:function(){},setCheckedCheckboxOrRadioAtIndex:function(){},setTabIndexForListItemChildren:function(){}}},enumerable:!1,configurable:!0}),$r.prototype.layout=function(){0!==this.adapter_.getListItemCount()&&(this.adapter_.hasCheckboxAtIndex(0)?this.isCheckboxList_=!0:this.adapter_.hasRadioAtIndex(0)&&(this.isRadioList_=!0))},$r.prototype.setWrapFocus=function(t){this.wrapFocus_=t},$r.prototype.setVerticalOrientation=function(t){this.isVertical_=t},$r.prototype.setSingleSelection=function(t){this.isSingleSelectionList_=t},$r.prototype.getSelectedIndex=function(){return this.selectedIndex_},$r.prototype.setSelectedIndex=function(t){this.isIndexValid_(t)&&(this.isCheckboxList_?this.setCheckboxAtIndex_(t):this.isRadioList_?this.setCheckboxAtIndex_(t):this.setSingleSelectionAtIndex_(t))},$r.prototype.handleFocusIn=function(t,e){0<=e&&this.adapter_.setTabIndexForListItemChildren(e,"0")},$r.prototype.handleFocusOut=function(t,e){var n=this;0<=e&&this.adapter_.setTabIndexForListItemChildren(e,"-1"),setTimeout(function(){n.adapter_.isFocusInsideList()||n.setTabindexToFirstSelectedItem_()},0)},$r.prototype.handleKeydown=function(t,e,n){var i="ArrowLeft"===t.key||37===t.keyCode,r="ArrowUp"===t.key||38===t.keyCode,o="ArrowRight"===t.key||39===t.keyCode,a="ArrowDown"===t.key||40===t.keyCode,s="Home"===t.key||36===t.keyCode,c="End"===t.key||35===t.keyCode,u="Enter"===t.key||13===t.keyCode,l="Space"===t.key||32===t.keyCode;if(this.adapter_.isRootFocused())r||c?(t.preventDefault(),this.focusLastElement()):(a||s)&&(t.preventDefault(),this.focusFirstElement());else{var f=this.adapter_.getFocusedElementIndex();if(!(-1===f&&(f=n)<0)){var d;if(this.isVertical_&&a||!this.isVertical_&&o)this.preventDefaultEvent_(t),d=this.focusNextElement(f);else if(this.isVertical_&&r||!this.isVertical_&&i)this.preventDefaultEvent_(t),d=this.focusPrevElement(f);else if(s)this.preventDefaultEvent_(t),d=this.focusFirstElement();else if(c)this.preventDefaultEvent_(t),d=this.focusLastElement();else if((u||l)&&e){var p=t.target;if(p&&"A"===p.tagName&&u)return;if(this.preventDefaultEvent_(t),this.adapter_.listItemAtIndexHasClass(f,Fn.LIST_ITEM_DISABLED_CLASS))return;this.isSelectableList_()&&this.setSelectedIndexOnAction_(f),this.adapter_.notifyAction(f)}this.focusedItemIndex_=f,void 0!==d&&(this.setTabindexAtIndex_(d),this.focusedItemIndex_=d)}}},$r.prototype.handleClick=function(t,e){t!==Hn.UNSET_INDEX&&(this.setTabindexAtIndex_(t),this.focusedItemIndex_=t,this.adapter_.listItemAtIndexHasClass(t,Fn.LIST_ITEM_DISABLED_CLASS)||(this.isSelectableList_()&&this.setSelectedIndexOnAction_(t,e),this.adapter_.notifyAction(t)))},$r.prototype.focusNextElement=function(t){var e=t+1;if(this.adapter_.getListItemCount()<=e){if(!this.wrapFocus_)return t;e=0}return this.adapter_.focusItemAtIndex(e),e},$r.prototype.focusPrevElement=function(t){var e=t-1;if(e<0){if(!this.wrapFocus_)return t;e=this.adapter_.getListItemCount()-1}return this.adapter_.focusItemAtIndex(e),e},$r.prototype.focusFirstElement=function(){return this.adapter_.focusItemAtIndex(0),0},$r.prototype.focusLastElement=function(){var t=this.adapter_.getListItemCount()-1;return this.adapter_.focusItemAtIndex(t),t},$r.prototype.setEnabled=function(t,e){this.isIndexValid_(t)&&(e?this.adapter_.setAttributeForElementIndex(t,Mn.ARIA_DISABLED,"false"):this.adapter_.setAttributeForElementIndex(t,Mn.ARIA_DISABLED,"true"))},$r.prototype.preventDefaultEvent_=function(t){var e=(""+t.target.tagName).toLowerCase();-1===Yr.indexOf(e)&&t.preventDefault()},$r.prototype.setSingleSelectionAtIndex_=function(t){for(var e=0;e<this.adapter_.getListItemCount();e++){var n=!1;0<=t.indexOf(e)&&(n=!0),this.setAriaForSingleSelectionAtIndex_(e,n)}this.selectedIndex_=t},$r.prototype.setAriaForSingleSelectionAtIndex_=function(t,e){this.selectedIndex_===Hn.UNSET_INDEX&&(this.ariaCurrentAttrValue_=this.adapter_.getAttributeForElementIndex(t,Mn.ARIA_CURRENT));var n=null!==this.ariaCurrentAttrValue_,i=n?Mn.ARIA_CURRENT:Mn.ARIA_SELECTED,r=n?this.ariaCurrentAttrValue_:"true";this.adapter_.setAttributeForElementIndex(t,i,e?r:"false")},$r.prototype.setCheckboxAtIndex_=function(t){for(var e=0;e<this.adapter_.getListItemCount();e++){var n=!1;0<=t.indexOf(e)&&(n=!0),this.adapter_.setAttributeForElementIndex(e,Mn.ARIA_CHECKED,n?"true":"false")}this.selectedIndex_=t},$r.prototype.setTabindexAtIndex_=function(t){this.focusedItemIndex_===Hn.UNSET_INDEX&&0!==t?this.adapter_.setAttributeForElementIndex(0,"tabindex","-1"):0<=this.focusedItemIndex_&&this.focusedItemIndex_!==t&&this.adapter_.setAttributeForElementIndex(this.focusedItemIndex_,"tabindex","-1"),this.adapter_.setAttributeForElementIndex(t,"tabindex","0")},$r.prototype.isSelectableList_=function(){return this.isSingleSelectionList_||this.isCheckboxList_||this.isRadioList_},$r.prototype.setTabindexToFirstSelectedItem_=function(){var t=0;this.isSelectableList_()&&("number"==typeof this.selectedIndex_&&this.selectedIndex_!==Hn.UNSET_INDEX?t=this.selectedIndex_:this.selectedIndex_ instanceof Array&&0<this.selectedIndex_.length&&(t=this.selectedIndex_.reduce(function(t,e){return Math.min(t,e)}))),this.setTabindexAtIndex_(t)},$r.prototype.isIndexValid_=function(t){var e=this;return t instanceof Array&&(0===t.length||t.some(function(t){return e.isIndexInRange_(t)}))},$r.prototype.isIndexInRange_=function(t){var e=this.adapter_.getListItemCount();return 0<=t&&t<e},$r.prototype.setSelectedIndexOnAction_=function(t,e){void 0===e&&(e=!0),this.isCheckboxList_||this.isRadioList_?this.toggleCheckboxAtIndex_(t,e):this.setSelectedIndex([t])},$r.prototype.toggleCheckboxAtIndex_=function(t,e){var n=this.adapter_.isCheckboxCheckedAtIndex(t);e&&(n=!n,this.adapter_.setCheckedCheckboxOrRadioAtIndex(t,n))},$r);function $r(t){var e=Qr.call(this,a(a({},$r.defaultAdapter),t))||this;return e.wrapFocus_=!1,e.isVertical_=!0,e.isSingleSelectionList_=!1,e.selectedIndex_=-1,e.focusedItemIndex_=Hn.UNSET_INDEX,e.ariaCurrentAttrValue_=null,e.isCheckboxList_=!1,e.isRadioList_=!1,e}var Jr,to=(r(eo,Jr=u),Object.defineProperty(eo.prototype,"vertical",{set:function(t){this.foundation_.setVerticalOrientation(t)},enumerable:!1,configurable:!0}),Object.defineProperty(eo.prototype,"listElements",{get:function(){return[].slice.call(this.root_.querySelectorAll("."+Fn.LIST_ITEM_CLASS))},enumerable:!1,configurable:!0}),Object.defineProperty(eo.prototype,"wrapFocus",{set:function(t){this.foundation_.setWrapFocus(t)},enumerable:!1,configurable:!0}),Object.defineProperty(eo.prototype,"singleSelection",{set:function(t){this.foundation_.setSingleSelection(t)},enumerable:!1,configurable:!0}),Object.defineProperty(eo.prototype,"selectedIndex",{get:function(){return this.foundation_.getSelectedIndex()},set:function(t){this.foundation_.setSelectedIndex(t)},enumerable:!1,configurable:!0}),eo.attachTo=function(t){return new eo(t)},eo.prototype.initialSyncWithDOM=function(){this.handleClick_=this.handleClickEvent_.bind(this),this.handleKeydown_=this.handleKeydownEvent_.bind(this),this.focusInEventListener_=this.handleFocusInEvent_.bind(this),this.focusOutEventListener_=this.handleFocusOutEvent_.bind(this),this.listen("keydown",this.handleKeydown_),this.listen("click",this.handleClick_),this.listen("focusin",this.focusInEventListener_),this.listen("focusout",this.focusOutEventListener_),this.layout(),this.initializeListType()},eo.prototype.destroy=function(){this.unlisten("keydown",this.handleKeydown_),this.unlisten("click",this.handleClick_),this.unlisten("focusin",this.focusInEventListener_),this.unlisten("focusout",this.focusOutEventListener_)},eo.prototype.layout=function(){var t=this.root_.getAttribute(Mn.ARIA_ORIENTATION);this.vertical=t!==Mn.ARIA_ORIENTATION_HORIZONTAL,[].slice.call(this.root_.querySelectorAll(".mdc-list-item:not([tabindex])")).forEach(function(t){t.setAttribute("tabindex","-1")}),[].slice.call(this.root_.querySelectorAll(Mn.FOCUSABLE_CHILD_ELEMENTS)).forEach(function(t){return t.setAttribute("tabindex","-1")}),this.foundation_.layout()},eo.prototype.initializeListType=function(){var e=this,t=this.root_.querySelectorAll(Mn.ARIA_ROLE_CHECKBOX_SELECTOR),n=this.root_.querySelector("\n      ."+Fn.LIST_ITEM_ACTIVATED_CLASS+",\n      ."+Fn.LIST_ITEM_SELECTED_CLASS+"\n    "),i=this.root_.querySelector(Mn.ARIA_CHECKED_RADIO_SELECTOR);if(t.length){var r=this.root_.querySelectorAll(Mn.ARIA_CHECKED_CHECKBOX_SELECTOR);this.selectedIndex=[].map.call(r,function(t){return e.listElements.indexOf(t)})}else n?(this.singleSelection=!0,this.selectedIndex=this.listElements.indexOf(n)):i&&(this.selectedIndex=this.listElements.indexOf(i))},eo.prototype.setEnabled=function(t,e){this.foundation_.setEnabled(t,e)},eo.prototype.getDefaultFoundation=function(){var r=this;return new Zr({addClassForElementIndex:function(t,e){var n=r.listElements[t];n&&n.classList.add(e)},focusItemAtIndex:function(t){var e=r.listElements[t];e&&e.focus()},getAttributeForElementIndex:function(t,e){return r.listElements[t].getAttribute(e)},getFocusedElementIndex:function(){return r.listElements.indexOf(document.activeElement)},getListItemCount:function(){return r.listElements.length},hasCheckboxAtIndex:function(t){return!!r.listElements[t].querySelector(Mn.CHECKBOX_SELECTOR)},hasRadioAtIndex:function(t){return!!r.listElements[t].querySelector(Mn.RADIO_SELECTOR)},isCheckboxCheckedAtIndex:function(t){return r.listElements[t].querySelector(Mn.CHECKBOX_RADIO_SELECTOR).checked},isFocusInsideList:function(){return r.root_.contains(document.activeElement)},isRootFocused:function(){return document.activeElement===r.root_},listItemAtIndexHasClass:function(t,e){return r.listElements[t].classList.contains(e)},notifyAction:function(t){r.emit(Mn.ACTION_EVENT,{index:t},!0)},removeClassForElementIndex:function(t,e){var n=r.listElements[t];n&&n.classList.remove(e)},setAttributeForElementIndex:function(t,e,n){var i=r.listElements[t];i&&i.setAttribute(e,n)},setCheckedCheckboxOrRadioAtIndex:function(t){var e=r.listElements[t].querySelector(Mn.CHECKBOX_RADIO_SELECTOR),n=document.createEvent("Event");n.initEvent("change",!0,!0),e.dispatchEvent(n)},setTabIndexForListItemChildren:function(t,e){var n=r.listElements[t];[].slice.call(n.querySelectorAll(Mn.CHILD_ELEMENTS_TO_TOGGLE_TABINDEX)).forEach(function(t){return t.setAttribute("tabindex",e)})}})},eo.prototype.getListItemIndex_=function(t){var e=p(t.target,"."+Fn.LIST_ITEM_CLASS+", ."+Fn.ROOT);return e&&h(e,"."+Fn.LIST_ITEM_CLASS)?this.listElements.indexOf(e):-1},eo.prototype.handleFocusInEvent_=function(t){var e=this.getListItemIndex_(t);this.foundation_.handleFocusIn(t,e)},eo.prototype.handleFocusOutEvent_=function(t){var e=this.getListItemIndex_(t);this.foundation_.handleFocusOut(t,e)},eo.prototype.handleKeydownEvent_=function(t){var e=this.getListItemIndex_(t),n=t.target;this.foundation_.handleKeydown(t,n.classList.contains(Fn.LIST_ITEM_CLASS),e)},eo.prototype.handleClickEvent_=function(t){var e=this.getListItemIndex_(t),n=!h(t.target,Mn.CHECKBOX_RADIO_SELECTOR);this.foundation_.handleClick(e,n)},eo);
   function eo(){return null!==Jr&&Jr.apply(this,arguments)||this}function no(t){return(no="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function io(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function ro(t,e,n){return e&&io(t.prototype,e),n&&io(t,n),t}function oo(o){var a=co();return function(){var t,e,n,i=lo(o);if(a){var r=lo(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==no(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function ao(t){var i="function"==typeof Map?new Map:void 0;return(ao=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return so(t,arguments,lo(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),uo(n,t)})(t)}function so(t,e,n){return(so=co()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&uo(r,n.prototype),r}).apply(null,arguments)}function co(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function uo(t,e){return(uo=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function lo(t){return(lo=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var fo=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&uo(t,e)}(n,ao(HTMLElement));var e=oo(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).selectedIndex_=-1,t.wrapFocus_=!1,t.vertical_=!0,t.list_,t}return ro(n,[{key:"focus",value:function(){var t=this.list_.foundation_.getSelectedIndex();"number"!=typeof t&&(t=0<t.length?t[0]:-1),0<=t&&t<this.list_.listElements.length?this.list_.listElements[t].focus():0<this.list_.listElements.length&&this.list_.listElements[0].focus()}},{key:"blur",value:function(){this.contains(document.activeElement)&&document.activeElement.blur()}},{key:"selectedIndex",get:function(){return this.selectedIndex_},set:function(t){var e=this.selectedIndex_;if(this.selectedIndex_=t,this.list_){this.list_.selectedIndex=t;var n=0<e.length?e[0]:-1;-1!==n&&this.list_.listElements[n]&&this.list_.listElements[n].setAttribute("tabindex","-1"),0<t.length&&this.list_.listElements.length>t[0]?this.list_.listElements[t[0]].setAttribute("tabindex","0"):0<this.list_.listElements.length&&this.list_.listElements[0].setAttribute("tabindex","0")}}},{key:"vertical",get:function(){return this.vertical_},set:function(t){this.vertical_=t,this.list_&&(this.list_.vertical=t)}},{key:"wrapFocus",get:function(){return this.wrapFocus_},set:function(t){this.wrapFocus_=t,this.list_&&(this.list_.wrapFocus=t)}}]),ro(n,[{key:"connectedCallback",value:function(){this.style.display="block",k.call(this),this.list_=new to(this),this.list_.selectedIndex=this.selectedIndex_,this.list_.vertical=this.vertical_,this.list_.wrapFocus=this.wrapFocus_;var t=this.querySelector(".mdc-list-item--selected, .mdc-list-item--activated");if(t)t.setAttribute("tabindex",0);else{var e=this.querySelector(".mdc-list-item");e&&e.setAttribute("tabindex",0)}var n=this.parentElement;n.classList.contains("mdc-menu")&&n.listSetup(this)}},{key:"disconnectedCallback",value:function(){this.list_.destroy(),F.call(this)}}]),n}();function po(t){return(po="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function ho(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function _o(o){var a=vo();return function(){var t,e,n,i=Eo(o);if(a){var r=Eo(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==po(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function yo(t){var i="function"==typeof Map?new Map:void 0;return(yo=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return mo(t,arguments,Eo(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),bo(n,t)})(t)}function mo(t,e,n){return(mo=vo()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&bo(r,n.prototype),r}).apply(null,arguments)}function vo(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function bo(t,e){return(bo=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Eo(t){return(Eo=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-list",fo);var go=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&bo(t,e)}(r,yo(HTMLElement));var t,e,n,i=_o(r);function r(){return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,r),i.call(this)}return t=r,(e=[{key:"connectedCallback",value:function(){k.call(this),this.classList.contains("mdc-list-item")?this.ripple_=new R(this):this.ripple_=new R(this.querySelector(".mdc-list-item"))}},{key:"disconnectedCallback",value:function(){this.ripple_.destroy(),F.call(this)}}])&&ho(t.prototype,e),n&&ho(t,n),r}();customElements.define("mdc-list-item",go);
var Co,Ao,To,Io,So={ANCHOR:"mdc-menu-surface--anchor",ANIMATING_CLOSED:"mdc-menu-surface--animating-closed",ANIMATING_OPEN:"mdc-menu-surface--animating-open",FIXED:"mdc-menu-surface--fixed",IS_OPEN_BELOW:"mdc-menu-surface--is-open-below",OPEN:"mdc-menu-surface--open",ROOT:"mdc-menu-surface"},Oo={CLOSED_EVENT:"MDCMenuSurface:closed",OPENED_EVENT:"MDCMenuSurface:opened",FOCUSABLE_ELEMENTS:["button:not(:disabled)",'[href]:not([aria-disabled="true"])',"input:not(:disabled)","select:not(:disabled)","textarea:not(:disabled)",'[tabindex]:not([tabindex="-1"]):not([aria-disabled="true"])'].join(", ")},Ro={TRANSITION_OPEN_DURATION:120,TRANSITION_CLOSE_DURATION:75,MARGIN_TO_EDGE:32,ANCHOR_TO_MENU_SURFACE_WIDTH_RATIO:.67};(Ao=Co=Co||{})[Ao.BOTTOM=1]="BOTTOM",Ao[Ao.CENTER=2]="CENTER",Ao[Ao.RIGHT=4]="RIGHT",Ao[Ao.FLIP_RTL=8]="FLIP_RTL",(Io=To=To||{})[Io.TOP_LEFT=0]="TOP_LEFT",Io[Io.TOP_RIGHT=4]="TOP_RIGHT",Io[Io.BOTTOM_LEFT=1]="BOTTOM_LEFT",Io[Io.BOTTOM_RIGHT=5]="BOTTOM_RIGHT",Io[Io.TOP_START=8]="TOP_START",Io[Io.TOP_END=12]="TOP_END",Io[Io.BOTTOM_START=9]="BOTTOM_START",Io[Io.BOTTOM_END=13]="BOTTOM_END";
var Lo,wo=(r(xo,Lo=s),Object.defineProperty(xo,"cssClasses",{get:function(){return So},enumerable:!1,configurable:!0}),Object.defineProperty(xo,"strings",{get:function(){return Oo},enumerable:!1,configurable:!0}),Object.defineProperty(xo,"numbers",{get:function(){return Ro},enumerable:!1,configurable:!0}),Object.defineProperty(xo,"Corner",{get:function(){return To},enumerable:!1,configurable:!0}),Object.defineProperty(xo,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},hasAnchor:function(){return!1},isElementInContainer:function(){return!1},isFocused:function(){return!1},isRtl:function(){return!1},getInnerDimensions:function(){return{height:0,width:0}},getAnchorDimensions:function(){return null},getWindowDimensions:function(){return{height:0,width:0}},getBodyDimensions:function(){return{height:0,width:0}},getWindowScroll:function(){return{x:0,y:0}},setPosition:function(){},setMaxHeight:function(){},setTransformOrigin:function(){},saveFocus:function(){},restoreFocus:function(){},notifyClose:function(){},notifyOpen:function(){}}},enumerable:!1,configurable:!0}),xo.prototype.init=function(){var t=xo.cssClasses,e=t.ROOT,n=t.OPEN;if(!this.adapter_.hasClass(e))throw new Error(e+" class required in root element.");this.adapter_.hasClass(n)&&(this.isOpen_=!0)},xo.prototype.destroy=function(){clearTimeout(this.openAnimationEndTimerId_),clearTimeout(this.closeAnimationEndTimerId_),cancelAnimationFrame(this.animationRequestId_)},xo.prototype.setAnchorCorner=function(t){this.anchorCorner_=t},xo.prototype.flipCornerHorizontally=function(){this.originCorner_=this.originCorner_^Co.RIGHT},xo.prototype.setAnchorMargin=function(t){this.anchorMargin_.top=t.top||0,this.anchorMargin_.right=t.right||0,this.anchorMargin_.bottom=t.bottom||0,this.anchorMargin_.left=t.left||0},xo.prototype.setIsHoisted=function(t){this.isHoistedElement_=t},xo.prototype.setFixedPosition=function(t){this.isFixedPosition_=t},xo.prototype.setAbsolutePosition=function(t,e){this.position_.x=this.isFinite_(t)?t:0,this.position_.y=this.isFinite_(e)?e:0},xo.prototype.setQuickOpen=function(t){this.isQuickOpen_=t},xo.prototype.isOpen=function(){return this.isOpen_},xo.prototype.open=function(){var t=this;this.isOpen_||(this.adapter_.saveFocus(),this.isQuickOpen_?(this.isOpen_=!0,this.adapter_.addClass(xo.cssClasses.OPEN),this.dimensions_=this.adapter_.getInnerDimensions(),this.autoPosition_(),this.adapter_.notifyOpen()):(this.adapter_.addClass(xo.cssClasses.ANIMATING_OPEN),this.animationRequestId_=requestAnimationFrame(function(){t.adapter_.addClass(xo.cssClasses.OPEN),t.dimensions_=t.adapter_.getInnerDimensions(),t.autoPosition_(),t.openAnimationEndTimerId_=setTimeout(function(){t.openAnimationEndTimerId_=0,t.adapter_.removeClass(xo.cssClasses.ANIMATING_OPEN),t.adapter_.notifyOpen()},Ro.TRANSITION_OPEN_DURATION)}),this.isOpen_=!0))},xo.prototype.close=function(t){var e=this;void 0===t&&(t=!1),this.isOpen_&&(this.isQuickOpen_?(this.isOpen_=!1,t||this.maybeRestoreFocus_(),this.adapter_.removeClass(xo.cssClasses.OPEN),this.adapter_.removeClass(xo.cssClasses.IS_OPEN_BELOW),this.adapter_.notifyClose()):(this.adapter_.addClass(xo.cssClasses.ANIMATING_CLOSED),requestAnimationFrame(function(){e.adapter_.removeClass(xo.cssClasses.OPEN),e.adapter_.removeClass(xo.cssClasses.IS_OPEN_BELOW),e.closeAnimationEndTimerId_=setTimeout(function(){e.closeAnimationEndTimerId_=0,e.adapter_.removeClass(xo.cssClasses.ANIMATING_CLOSED),e.adapter_.notifyClose()},Ro.TRANSITION_CLOSE_DURATION)}),this.isOpen_=!1,t||this.maybeRestoreFocus_()))},xo.prototype.autoPosition_=function(){var t;this.measurements_=this.getAutoLayoutMeasurements_();var e=this.getOriginCorner_(),n=this.getMenuSurfaceMaxHeight_(e),i=this.hasBit_(e,Co.BOTTOM)?"bottom":"top",r=this.hasBit_(e,Co.RIGHT)?"right":"left",o=this.getHorizontalOriginOffset_(e),a=this.getVerticalOriginOffset_(e),s=this.measurements_,c=s.anchorSize,u=s.surfaceSize,l=((t={})[r]=o,t[i]=a,t);c.width/u.width>Ro.ANCHOR_TO_MENU_SURFACE_WIDTH_RATIO&&(r="center"),(this.isHoistedElement_||this.isFixedPosition_)&&this.adjustPositionForHoistedElement_(l),this.adapter_.setTransformOrigin(r+" "+i),this.adapter_.setPosition(l),this.adapter_.setMaxHeight(n?n+"px":""),this.hasBit_(e,Co.BOTTOM)||this.adapter_.addClass(xo.cssClasses.IS_OPEN_BELOW)},xo.prototype.getAutoLayoutMeasurements_=function(){var t=this.adapter_.getAnchorDimensions(),e=this.adapter_.getBodyDimensions(),n=this.adapter_.getWindowDimensions(),i=this.adapter_.getWindowScroll();return{anchorSize:t=t||{top:this.position_.y,right:this.position_.x,bottom:this.position_.y,left:this.position_.x,width:0,height:0},bodySize:e,surfaceSize:this.dimensions_,viewportDistance:{top:t.top,right:n.width-t.right,bottom:n.height-t.bottom,left:t.left},viewportSize:n,windowScroll:i}},xo.prototype.getOriginCorner_=function(){var t,e,n=this.originCorner_,i=this.measurements_,r=i.viewportDistance,o=i.anchorSize,a=i.surfaceSize,s=xo.numbers.MARGIN_TO_EDGE;!(0<(e=this.hasBit_(this.anchorCorner_,Co.BOTTOM)?(t=r.top-s+o.height+this.anchorMargin_.bottom,r.bottom-s-this.anchorMargin_.bottom):(t=r.top-s+this.anchorMargin_.top,r.bottom-s+o.height-this.anchorMargin_.top))-a.height)&&e<=t&&(n=this.setBit_(n,Co.BOTTOM));var c,u,l=this.adapter_.isRtl(),f=this.hasBit_(this.anchorCorner_,Co.FLIP_RTL),d=this.hasBit_(this.anchorCorner_,Co.RIGHT),p=!1;u=(p=l&&f?!d:d)?(c=r.left+o.width+this.anchorMargin_.right,r.right-this.anchorMargin_.right):(c=r.left+this.anchorMargin_.left,r.right+o.width-this.anchorMargin_.left);var h=0<c-a.width,_=0<u-a.width,y=this.hasBit_(n,Co.FLIP_RTL)&&this.hasBit_(n,Co.RIGHT);return _&&y&&l||!h&&y?n=this.unsetBit_(n,Co.RIGHT):(h&&p&&l||h&&!p&&d||!_&&u<=c)&&(n=this.setBit_(n,Co.RIGHT)),n},xo.prototype.getMenuSurfaceMaxHeight_=function(t){var e=this.measurements_.viewportDistance,n=0,i=this.hasBit_(t,Co.BOTTOM),r=this.hasBit_(this.anchorCorner_,Co.BOTTOM),o=xo.numbers.MARGIN_TO_EDGE;return i?(n=e.top+this.anchorMargin_.top-o,r||(n+=this.measurements_.anchorSize.height)):(n=e.bottom-this.anchorMargin_.bottom+this.measurements_.anchorSize.height-o,r&&(n-=this.measurements_.anchorSize.height)),n},xo.prototype.getHorizontalOriginOffset_=function(t){var e=this.measurements_.anchorSize,n=this.hasBit_(t,Co.RIGHT),i=this.hasBit_(this.anchorCorner_,Co.RIGHT);if(n){var r=i?e.width-this.anchorMargin_.left:this.anchorMargin_.right;return this.isHoistedElement_||this.isFixedPosition_?r-(this.measurements_.viewportSize.width-this.measurements_.bodySize.width):r}return i?e.width-this.anchorMargin_.right:this.anchorMargin_.left},xo.prototype.getVerticalOriginOffset_=function(t){var e=this.measurements_.anchorSize,n=this.hasBit_(t,Co.BOTTOM),i=this.hasBit_(this.anchorCorner_,Co.BOTTOM);return n?i?e.height-this.anchorMargin_.top:-this.anchorMargin_.bottom:i?e.height+this.anchorMargin_.bottom:this.anchorMargin_.top},xo.prototype.adjustPositionForHoistedElement_=function(t){var e,n,i=this.measurements_,r=i.windowScroll,o=i.viewportDistance,a=Object.keys(t);try{for(var s=f(a),c=s.next();!c.done;c=s.next()){var u=c.value,l=t[u]||0;l+=o[u],this.isFixedPosition_||("top"===u?l+=r.y:"bottom"===u?l-=r.y:"left"===u?l+=r.x:l-=r.x),t[u]=l}}catch(t){e={error:t}}finally{try{c&&!c.done&&(n=s.return)&&n.call(s)}finally{if(e)throw e.error}}},xo.prototype.maybeRestoreFocus_=function(){var t=this.adapter_.isFocused(),e=document.activeElement&&this.adapter_.isElementInContainer(document.activeElement);(t||e)&&this.adapter_.restoreFocus()},xo.prototype.hasBit_=function(t,e){return Boolean(t&e)},xo.prototype.setBit_=function(t,e){return t|e},xo.prototype.unsetBit_=function(t,e){return t^e},xo.prototype.isFinite_=function(t){return"number"==typeof t&&isFinite(t)},xo);function xo(t){var e=Lo.call(this,a(a({},xo.defaultAdapter),t))||this;return e.isOpen_=!1,e.isQuickOpen_=!1,e.isHoistedElement_=!1,e.isFixedPosition_=!1,e.openAnimationEndTimerId_=0,e.closeAnimationEndTimerId_=0,e.animationRequestId_=0,e.anchorCorner_=To.TOP_START,e.originCorner_=To.TOP_START,e.anchorMargin_={top:0,right:0,bottom:0,left:0},e.position_={x:0,y:0},e}var No;
var Do,Po=(r(ko,Do=u),ko.attachTo=function(t){return new ko(t)},ko.prototype.initialSyncWithDOM=function(){var e=this,t=this.root_.parentElement;this.anchorElement=t&&t.classList.contains(So.ANCHOR)?t:null,this.root_.classList.contains(So.FIXED)&&this.setFixedPosition(!0),this.handleKeydown_=function(t){return e.handleKeydown(t)},this.handleBodyClick_=function(t){return e.handleBodyClick(t)},this.registerBodyClickListener_=function(){return document.body.addEventListener("click",e.handleBodyClick_,{capture:!0})},this.deregisterBodyClickListener_=function(){return document.body.removeEventListener("click",e.handleBodyClick_)},this.listen("keydown",this.handleKeydown_),this.listen(Oo.OPENED_EVENT,this.registerBodyClickListener_),this.listen(Oo.CLOSED_EVENT,this.deregisterBodyClickListener_)},ko.prototype.handleBodyClick=function(t){var e=t.target;this.isElementInContainer(e)||this.emit("MDCMenuSurface:close",{},!0)},ko.prototype.handleKeydown=function(t){var e=t.keyCode;"Escape"!==t.key&&27!==e||this.emit("MDCMenuSurface:close",{},!0)},ko.prototype.isElementInContainer=function(t){return this.root_.contains(t)},ko.prototype.destroy=function(){this.unlisten("keydown",this.handleKeydown_),this.unlisten(Oo.OPENED_EVENT,this.registerBodyClickListener_),this.deregisterBodyClickListener_(),this.unlisten(Oo.CLOSED_EVENT,this.deregisterBodyClickListener_),Do.prototype.destroy.call(this)},ko.prototype.isOpen=function(){return this.foundation_.isOpen()},ko.prototype.open=function(){this.foundation_.open()},ko.prototype.close=function(t){void 0===t&&(t=!1),this.foundation_.close(t)},Object.defineProperty(ko.prototype,"quickOpen",{set:function(t){this.foundation_.setQuickOpen(t)},enumerable:!1,configurable:!0}),ko.prototype.setIsHoisted=function(t){this.foundation_.setIsHoisted(t)},ko.prototype.setMenuSurfaceAnchorElement=function(t){this.anchorElement=t},ko.prototype.setFixedPosition=function(t){t?this.root_.classList.add(So.FIXED):this.root_.classList.remove(So.FIXED),this.foundation_.setFixedPosition(t)},ko.prototype.setAbsolutePosition=function(t,e){this.foundation_.setAbsolutePosition(t,e),this.setIsHoisted(!0)},ko.prototype.setAnchorCorner=function(t){this.foundation_.setAnchorCorner(t)},ko.prototype.setAnchorMargin=function(t){this.foundation_.setAnchorMargin(t)},ko.prototype.getDefaultFoundation=function(){var n=this;return new wo({addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},hasClass:function(t){return n.root_.classList.contains(t)},hasAnchor:function(){return!!n.anchorElement},notifyClose:function(){return n.emit(wo.strings.CLOSED_EVENT,{})},notifyOpen:function(){return n.emit(wo.strings.OPENED_EVENT,{})},isElementInContainer:function(t){return n.isElementInContainer(t)},isRtl:function(){return"rtl"===getComputedStyle(n.root_).getPropertyValue("direction")},setTransformOrigin:function(t){var e=function(t,e){if(void 0===e&&(e=!1),void 0===No||e){var n=t.document.createElement("div");No="transform"in n.style?"transform":"webkitTransform"}return No}(window)+"-origin";n.root_.style.setProperty(e,t)},isFocused:function(){return document.activeElement===n.root_},saveFocus:function(){n.previousFocus_=document.activeElement},restoreFocus:function(){n.root_.contains(document.activeElement)&&n.previousFocus_&&n.previousFocus_.focus&&n.previousFocus_.focus()},getInnerDimensions:function(){return{width:n.root_.offsetWidth,height:n.root_.offsetHeight}},getAnchorDimensions:function(){return n.anchorElement?n.anchorElement.getBoundingClientRect():null},getWindowDimensions:function(){return{width:window.innerWidth,height:window.innerHeight}},getBodyDimensions:function(){return{width:document.body.clientWidth,height:document.body.clientHeight}},getWindowScroll:function(){return{x:window.pageXOffset,y:window.pageYOffset}},setPosition:function(t){n.root_.style.left="left"in t?t.left+"px":"",n.root_.style.right="right"in t?t.right+"px":"",n.root_.style.top="top"in t?t.top+"px":"",n.root_.style.bottom="bottom"in t?t.bottom+"px":""},setMaxHeight:function(t){n.root_.style.maxHeight=t}})},ko);
   function ko(){return null!==Do&&Do.apply(this,arguments)||this}var Fo,Mo=(r(Ho,Fo=s),Object.defineProperty(Ho,"cssClasses",{get:function(){return So},enumerable:!0,configurable:!0}),Object.defineProperty(Ho,"strings",{get:function(){return Oo},enumerable:!0,configurable:!0}),Object.defineProperty(Ho,"numbers",{get:function(){return Ro},enumerable:!0,configurable:!0}),Object.defineProperty(Ho,"Corner",{get:function(){return To},enumerable:!0,configurable:!0}),Object.defineProperty(Ho,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},hasAnchor:function(){return!1},isElementInContainer:function(){return!1},isFocused:function(){return!1},isRtl:function(){return!1},getInnerDimensions:function(){return{height:0,width:0}},getAnchorDimensions:function(){return null},getWindowDimensions:function(){return{height:0,width:0}},getBodyDimensions:function(){return{height:0,width:0}},getWindowScroll:function(){return{x:0,y:0}},setPosition:function(){},setMaxHeight:function(){},setTransformOrigin:function(){},saveFocus:function(){},restoreFocus:function(){},notifyClose:function(){},notifyOpen:function(){}}},enumerable:!0,configurable:!0}),Ho.prototype.init=function(){var t=Ho.cssClasses,e=t.ROOT,n=t.OPEN;if(!this.adapter_.hasClass(e))throw new Error(e+" class required in root element.");this.adapter_.hasClass(n)&&(this.isOpen_=!0)},Ho.prototype.destroy=function(){clearTimeout(this.openAnimationEndTimerId_),clearTimeout(this.closeAnimationEndTimerId_),cancelAnimationFrame(this.animationRequestId_)},Ho.prototype.setAnchorCorner=function(t){this.anchorCorner_=t},Ho.prototype.flipCornerHorizontally=function(){this.originCorner_=this.originCorner_^Co.RIGHT},Ho.prototype.setAnchorMargin=function(t){this.anchorMargin_.top=t.top||0,this.anchorMargin_.right=t.right||0,this.anchorMargin_.bottom=t.bottom||0,this.anchorMargin_.left=t.left||0},Ho.prototype.setIsHoisted=function(t){this.isHoistedElement_=t},Ho.prototype.setFixedPosition=function(t){this.isFixedPosition_=t},Ho.prototype.setAbsolutePosition=function(t,e){this.position_.x=this.isFinite_(t)?t:0,this.position_.y=this.isFinite_(e)?e:0},Ho.prototype.setQuickOpen=function(t){this.isQuickOpen_=t},Ho.prototype.isOpen=function(){return this.isOpen_},Ho.prototype.open=function(){var t=this;this.isOpen_||(this.adapter_.saveFocus(),this.isQuickOpen_?(this.isOpen_=!0,this.adapter_.addClass(Ho.cssClasses.OPEN),this.dimensions_=this.adapter_.getInnerDimensions(),this.autoPosition_(),this.adapter_.notifyOpen()):(this.adapter_.addClass(Ho.cssClasses.ANIMATING_OPEN),this.animationRequestId_=requestAnimationFrame(function(){t.adapter_.addClass(Ho.cssClasses.OPEN),t.dimensions_=t.adapter_.getInnerDimensions(),t.autoPosition_(),t.openAnimationEndTimerId_=setTimeout(function(){t.openAnimationEndTimerId_=0,t.adapter_.removeClass(Ho.cssClasses.ANIMATING_OPEN),t.adapter_.notifyOpen()},Ro.TRANSITION_OPEN_DURATION)}),this.isOpen_=!0))},Ho.prototype.close=function(t){var e=this;void 0===t&&(t=!1),this.isOpen_&&(this.isQuickOpen_?(this.isOpen_=!1,t||this.maybeRestoreFocus_(),this.adapter_.removeClass(Ho.cssClasses.OPEN),this.adapter_.removeClass(Ho.cssClasses.IS_OPEN_BELOW),this.adapter_.notifyClose()):(this.adapter_.addClass(Ho.cssClasses.ANIMATING_CLOSED),requestAnimationFrame(function(){e.adapter_.removeClass(Ho.cssClasses.OPEN),e.adapter_.removeClass(Ho.cssClasses.IS_OPEN_BELOW),e.closeAnimationEndTimerId_=setTimeout(function(){e.closeAnimationEndTimerId_=0,e.adapter_.removeClass(Ho.cssClasses.ANIMATING_CLOSED),e.adapter_.notifyClose()},Ro.TRANSITION_CLOSE_DURATION)}),this.isOpen_=!1,t||this.maybeRestoreFocus_()))},Ho.prototype.handleBodyClick=function(t){var e=t.target;this.adapter_.isElementInContainer(e)||this.close()},Ho.prototype.handleKeydown=function(t){var e=t.keyCode;"Escape"!==t.key&&27!==e||this.close()},Ho.prototype.autoPosition_=function(){var t;this.measurements_=this.getAutoLayoutMeasurements_();var e=this.getOriginCorner_(),n=this.getMenuSurfaceMaxHeight_(e),i=this.hasBit_(e,Co.BOTTOM)?"bottom":"top",r=this.hasBit_(e,Co.RIGHT)?"right":"left",o=this.getHorizontalOriginOffset_(e),a=this.getVerticalOriginOffset_(e),s=this.measurements_,c=s.anchorSize,u=s.surfaceSize,l=((t={})[r]=o,t[i]=a,t);c.width/u.width>Ro.ANCHOR_TO_MENU_SURFACE_WIDTH_RATIO&&(r="center"),(this.isHoistedElement_||this.isFixedPosition_)&&this.adjustPositionForHoistedElement_(l),this.adapter_.setTransformOrigin(r+" "+i),this.adapter_.setPosition(l),this.adapter_.setMaxHeight(n?n+"px":""),this.hasBit_(e,Co.BOTTOM)||this.adapter_.addClass(Ho.cssClasses.IS_OPEN_BELOW)},Ho.prototype.getAutoLayoutMeasurements_=function(){var t=this.adapter_.getAnchorDimensions(),e=this.adapter_.getBodyDimensions(),n=this.adapter_.getWindowDimensions(),i=this.adapter_.getWindowScroll();return{anchorSize:t=t||{top:this.position_.y,right:this.position_.x,bottom:this.position_.y,left:this.position_.x,width:0,height:0},bodySize:e,surfaceSize:this.dimensions_,viewportDistance:{top:t.top,right:n.width-t.right,bottom:n.height-t.bottom,left:t.left},viewportSize:n,windowScroll:i}},Ho.prototype.getOriginCorner_=function(){var t,e,n=this.originCorner_,i=this.measurements_,r=i.viewportDistance,o=i.anchorSize,a=i.surfaceSize,s=Ho.numbers.MARGIN_TO_EDGE;!(0<(e=this.hasBit_(this.anchorCorner_,Co.BOTTOM)?(t=r.top-s+o.height+this.anchorMargin_.bottom,r.bottom-s-this.anchorMargin_.bottom):(t=r.top-s+this.anchorMargin_.top,r.bottom-s+o.height-this.anchorMargin_.top))-a.height)&&e<=t&&(n=this.setBit_(n,Co.BOTTOM));var c,u,l=this.adapter_.isRtl(),f=this.hasBit_(this.anchorCorner_,Co.FLIP_RTL),d=this.hasBit_(this.anchorCorner_,Co.RIGHT),p=!1;u=(p=l&&f?!d:d)?(c=r.left+o.width+this.anchorMargin_.right,r.right-this.anchorMargin_.right):(c=r.left+this.anchorMargin_.left,r.right+o.width-this.anchorMargin_.left);var h=0<c-a.width,_=0<u-a.width,y=this.hasBit_(n,Co.FLIP_RTL)&&this.hasBit_(n,Co.RIGHT);return _&&y&&l||!h&&y?n=this.unsetBit_(n,Co.RIGHT):(h&&p&&l||h&&!p&&d||!_&&u<=c)&&(n=this.setBit_(n,Co.RIGHT)),n},Ho.prototype.getMenuSurfaceMaxHeight_=function(t){var e=this.measurements_.viewportDistance,n=0,i=this.hasBit_(t,Co.BOTTOM),r=this.hasBit_(this.anchorCorner_,Co.BOTTOM),o=Ho.numbers.MARGIN_TO_EDGE;return i?(n=e.top+this.anchorMargin_.top-o,r||(n+=this.measurements_.anchorSize.height)):(n=e.bottom-this.anchorMargin_.bottom+this.measurements_.anchorSize.height-o,r&&(n-=this.measurements_.anchorSize.height)),n},Ho.prototype.getHorizontalOriginOffset_=function(t){var e=this.measurements_.anchorSize,n=this.hasBit_(t,Co.RIGHT),i=this.hasBit_(this.anchorCorner_,Co.RIGHT);if(n){var r=i?e.width-this.anchorMargin_.left:this.anchorMargin_.right;return this.isHoistedElement_||this.isFixedPosition_?r-(this.measurements_.viewportSize.width-this.measurements_.bodySize.width):r}return i?e.width-this.anchorMargin_.right:this.anchorMargin_.left},Ho.prototype.getVerticalOriginOffset_=function(t){var e=this.measurements_.anchorSize,n=this.hasBit_(t,Co.BOTTOM),i=this.hasBit_(this.anchorCorner_,Co.BOTTOM);return n?i?e.height-this.anchorMargin_.top:-this.anchorMargin_.bottom:i?e.height+this.anchorMargin_.bottom:this.anchorMargin_.top},Ho.prototype.adjustPositionForHoistedElement_=function(t){var e,n,i=this.measurements_,r=i.windowScroll,o=i.viewportDistance,a=Object.keys(t);try{for(var s=f(a),c=s.next();!c.done;c=s.next()){var u=c.value,l=t[u]||0;l+=o[u],this.isFixedPosition_||("top"===u?l+=r.y:"bottom"===u?l-=r.y:"left"===u?l+=r.x:l-=r.x),t[u]=l}}catch(t){e={error:t}}finally{try{c&&!c.done&&(n=s.return)&&n.call(s)}finally{if(e)throw e.error}}},Ho.prototype.maybeRestoreFocus_=function(){var t=this.adapter_.isFocused(),e=document.activeElement&&this.adapter_.isElementInContainer(document.activeElement);(t||e)&&this.adapter_.restoreFocus()},Ho.prototype.hasBit_=function(t,e){return Boolean(t&e)},Ho.prototype.setBit_=function(t,e){return t|e},Ho.prototype.unsetBit_=function(t,e){return t^e},Ho.prototype.isFinite_=function(t){return"number"==typeof t&&isFinite(t)},Ho);function Ho(t){var e=Fo.call(this,a(a({},Ho.defaultAdapter),t))||this;return e.isOpen_=!1,e.isQuickOpen_=!1,e.isHoistedElement_=!1,e.isFixedPosition_=!1,e.openAnimationEndTimerId_=0,e.closeAnimationEndTimerId_=0,e.animationRequestId_=0,e.anchorCorner_=To.TOP_START,e.originCorner_=To.TOP_START,e.anchorMargin_={top:0,right:0,bottom:0,left:0},e.position_={x:0,y:0},e}var jo,Bo,Vo={MENU_SELECTED_LIST_ITEM:"mdc-menu-item--selected",MENU_SELECTION_GROUP:"mdc-menu__selection-group",ROOT:"mdc-menu"},Uo={ARIA_CHECKED_ATTR:"aria-checked",ARIA_DISABLED_ATTR:"aria-disabled",CHECKBOX_SELECTOR:'input[type="checkbox"]',LIST_SELECTOR:".mdc-list",SELECTED_EVENT:"MDCMenu:selected"},Ko={FOCUS_ROOT_INDEX:-1};
   (Bo=jo=jo||{})[Bo.NONE=0]="NONE",Bo[Bo.LIST_ROOT=1]="LIST_ROOT",Bo[Bo.FIRST_ITEM=2]="FIRST_ITEM",Bo[Bo.LAST_ITEM=3]="LAST_ITEM";
var Go,Wo=(r(qo,Go=s),Object.defineProperty(qo,"cssClasses",{get:function(){return Vo},enumerable:!1,configurable:!0}),Object.defineProperty(qo,"strings",{get:function(){return Uo},enumerable:!1,configurable:!0}),Object.defineProperty(qo,"numbers",{get:function(){return Ko},enumerable:!1,configurable:!0}),Object.defineProperty(qo,"defaultAdapter",{get:function(){return{addClassToElementAtIndex:function(){},removeClassFromElementAtIndex:function(){},addAttributeToElementAtIndex:function(){},removeAttributeFromElementAtIndex:function(){},elementContainsClass:function(){return!1},closeSurface:function(){},getElementIndex:function(){return-1},notifySelected:function(){},getMenuItemCount:function(){return 0},focusItemAtIndex:function(){},focusListRoot:function(){},getSelectedSiblingOfItemAtIndex:function(){return-1},isSelectableItemAtIndex:function(){return!1}}},enumerable:!1,configurable:!0}),qo.prototype.destroy=function(){this.closeAnimationEndTimerId_&&clearTimeout(this.closeAnimationEndTimerId_),this.adapter_.closeSurface()},qo.prototype.handleKeydown=function(t){var e=t.key,n=t.keyCode;"Tab"!==e&&9!==n||this.adapter_.closeSurface(!0)},qo.prototype.handleItemAction=function(e){var n=this,t=this.adapter_.getElementIndex(e);t<0||(this.adapter_.notifySelected({index:t}),this.adapter_.closeSurface(),this.closeAnimationEndTimerId_=setTimeout(function(){var t=n.adapter_.getElementIndex(e);n.adapter_.isSelectableItemAtIndex(t)&&n.setSelectedIndex(t)},Mo.numbers.TRANSITION_CLOSE_DURATION))},qo.prototype.handleMenuSurfaceOpened=function(){switch(this.defaultFocusState_){case jo.FIRST_ITEM:this.adapter_.focusItemAtIndex(0);break;case jo.LAST_ITEM:this.adapter_.focusItemAtIndex(this.adapter_.getMenuItemCount()-1);break;case jo.NONE:break;default:this.adapter_.focusListRoot()}},qo.prototype.setDefaultFocusState=function(t){this.defaultFocusState_=t},qo.prototype.setSelectedIndex=function(t){if(this.validatedIndex_(t),!this.adapter_.isSelectableItemAtIndex(t))throw new Error("MDCMenuFoundation: No selection group at specified index.");var e=this.adapter_.getSelectedSiblingOfItemAtIndex(t);0<=e&&(this.adapter_.removeAttributeFromElementAtIndex(e,Uo.ARIA_CHECKED_ATTR),this.adapter_.removeClassFromElementAtIndex(e,Vo.MENU_SELECTED_LIST_ITEM)),this.adapter_.addClassToElementAtIndex(t,Vo.MENU_SELECTED_LIST_ITEM),this.adapter_.addAttributeToElementAtIndex(t,Uo.ARIA_CHECKED_ATTR,"true")},qo.prototype.setEnabled=function(t,e){this.validatedIndex_(t),e?(this.adapter_.removeClassFromElementAtIndex(t,Fn.LIST_ITEM_DISABLED_CLASS),this.adapter_.addAttributeToElementAtIndex(t,Uo.ARIA_DISABLED_ATTR,"false")):(this.adapter_.addClassToElementAtIndex(t,Fn.LIST_ITEM_DISABLED_CLASS),this.adapter_.addAttributeToElementAtIndex(t,Uo.ARIA_DISABLED_ATTR,"true"))},qo.prototype.validatedIndex_=function(t){var e=this.adapter_.getMenuItemCount();if(!(0<=t&&t<e))throw new Error("MDCMenuFoundation: No list item at specified index.")},qo);function qo(t){var e=Go.call(this,a(a({},qo.defaultAdapter),t))||this;return e.closeAnimationEndTimerId_=0,e.defaultFocusState_=jo.LIST_ROOT,e}var zo,Xo=(r(Yo,zo=u),Yo.attachTo=function(t){return new Yo(t)},Yo.prototype.initialize=function(t){void 0===t&&(t=function(t){return new Po(t)}),this.menuSurfaceFactory_=t},Yo.prototype.initialSyncWithDOM=function(){var e=this;this.menuSurface_=this.menuSurfaceFactory_(this.root_),this.handleKeydown_=function(t){return e.foundation_.handleKeydown(t)},this.handleItemAction_=function(t){return e.foundation_.handleItemAction(e.items[t.detail.index])},this.handleMenuSurfaceOpened_=function(){return e.foundation_.handleMenuSurfaceOpened()},this.menuSurface_.listen(Mo.strings.OPENED_EVENT,this.handleMenuSurfaceOpened_),this.listen("keydown",this.handleKeydown_),this.listen(Vn.strings.ACTION_EVENT,this.handleItemAction_)},Yo.prototype.listSetup=function(t){this.list_=t.list_,this.list_.wrapFocus=!0},Yo.prototype.destroy=function(){this.menuSurface_.destroy(),this.menuSurface_.unlisten(Mo.strings.OPENED_EVENT,this.handleMenuSurfaceOpened_),this.unlisten("keydown",this.handleKeydown_),this.unlisten(Vn.strings.ACTION_EVENT,this.handleItemAction_),zo.prototype.destroy.call(this)},Object.defineProperty(Yo.prototype,"open",{get:function(){return this.menuSurface_.isOpen()},set:function(t){t?this.menuSurface_.open():this.menuSurface_.close()},enumerable:!1,configurable:!0}),Object.defineProperty(Yo.prototype,"wrapFocus",{get:function(){return!!this.list_&&this.list_.wrapFocus},set:function(t){this.list_&&(this.list_.wrapFocus=t)},enumerable:!1,configurable:!0}),Object.defineProperty(Yo.prototype,"items",{get:function(){return this.list_?this.list_.listElements:[]},enumerable:!1,configurable:!0}),Object.defineProperty(Yo.prototype,"quickOpen",{set:function(t){this.menuSurface_.quickOpen=t},enumerable:!1,configurable:!0}),Yo.prototype.setDefaultFocusState=function(t){this.foundation_.setDefaultFocusState(t)},Yo.prototype.setAnchorCorner=function(t){this.menuSurface_.setAnchorCorner(t)},Yo.prototype.setAnchorMargin=function(t){this.menuSurface_.setAnchorMargin(t)},Yo.prototype.setSelectedIndex=function(t){this.foundation_.setSelectedIndex(t)},Yo.prototype.setEnabled=function(t,e){this.foundation_.setEnabled(t,e)},Yo.prototype.getOptionByIndex=function(t){return t<this.items.length?this.items[t]:null},Yo.prototype.setFixedPosition=function(t){this.menuSurface_.setFixedPosition(t)},Yo.prototype.setIsHoisted=function(t){this.menuSurface_.setIsHoisted(t)},Yo.prototype.setAbsolutePosition=function(t,e){this.menuSurface_.setAbsolutePosition(t,e)},Yo.prototype.setAnchorElement=function(t){this.menuSurface_.anchorElement=t},Yo.prototype.getDefaultFoundation=function(){var i=this;return new Wo({addClassToElementAtIndex:function(t,e){i.items[t].classList.add(e)},removeClassFromElementAtIndex:function(t,e){i.items[t].classList.remove(e)},addAttributeToElementAtIndex:function(t,e,n){i.items[t].setAttribute(e,n)},removeAttributeFromElementAtIndex:function(t,e){i.items[t].removeAttribute(e)},elementContainsClass:function(t,e){return t.classList.contains(e)},closeSurface:function(){return i.emit("MDCMenu:close",{},!0)},getElementIndex:function(t){return i.items.indexOf(t)},notifySelected:function(t){return i.emit(Uo.SELECTED_EVENT,{index:t.index,item:i.items[t.index]})},getMenuItemCount:function(){return i.items.length},focusItemAtIndex:function(t){return i.items[t].focus()},focusListRoot:function(){return i.root_.querySelector(Uo.LIST_SELECTOR).focus()},isSelectableItemAtIndex:function(t){return!!p(i.items[t],"."+Vo.MENU_SELECTION_GROUP)},getSelectedSiblingOfItemAtIndex:function(t){var e=p(i.items[t],"."+Vo.MENU_SELECTION_GROUP).querySelector("."+Vo.MENU_SELECTED_LIST_ITEM);return e?i.items.indexOf(e):-1}})},Yo);
   function Yo(){return null!==zo&&zo.apply(this,arguments)||this}function Qo(t){return(Qo="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Zo(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function $o(t,e,n){return e&&Zo(t.prototype,e),n&&Zo(t,n),t}function Jo(o){var a=na();return function(){var t,e,n,i=ra(o);if(a){var r=ra(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Qo(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function ta(t){var i="function"==typeof Map?new Map:void 0;return(ta=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return ea(t,arguments,ra(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),ia(n,t)})(t)}function ea(t,e,n){return(ea=na()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&ia(r,n.prototype),r}).apply(null,arguments)}function na(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function ia(t,e){return(ia=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function ra(t){return(ra=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var oa=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&ia(t,e)}(n,ta(HTMLElement));var e=Jo(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).open_=!1,t.quickOpen_=!1,t.menu_,t}return $o(n,[{key:"open",get:function(){return this.open_},set:function(t){this.open_=t,this.menu_&&(this.menu_.open=t)}},{key:"quickOpen",get:function(){return this.quickOpen_},set:function(t){this.quickOpen_=t,this.menu_&&(this.menu_.quickOpen=t)}}]),$o(n,[{key:"connectedCallback",value:function(){k.call(this),this.menu_=new Xo(this),this.menu_.open=this.open_,this.menu_.quickOpen=this.quickOpen_}},{key:"listSetup",value:function(t){if(this.menu_.listSetup(t),this.classList.contains("mdc-select__menu")){var e=this.parentElement;e.classList.contains("mdc-select")&&e.menuSetup(this)}}},{key:"disconnectedCallback",value:function(){this.menu_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-menu",oa);
var aa,sa={NATIVE_CONTROL_SELECTOR:".mdc-radio__native-control"},ca={DISABLED:"mdc-radio--disabled",ROOT:"mdc-radio"},ua=(r(la,aa=s),Object.defineProperty(la,"cssClasses",{get:function(){return ca},enumerable:!1,configurable:!0}),Object.defineProperty(la,"strings",{get:function(){return sa},enumerable:!1,configurable:!0}),Object.defineProperty(la,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},setNativeControlDisabled:function(){}}},enumerable:!1,configurable:!0}),la.prototype.setDisabled=function(t){var e=la.cssClasses.DISABLED;this.adapter_.setNativeControlDisabled(t),t?this.adapter_.addClass(e):this.adapter_.removeClass(e)},la);function la(t){return aa.call(this,a(a({},la.defaultAdapter),t))||this}var fa,da=(r(pa,fa=u),pa.attachTo=function(t){return new pa(t)},Object.defineProperty(pa.prototype,"checked",{get:function(){return this.nativeControl_.checked},set:function(t){this.nativeControl_.checked=t},enumerable:!1,configurable:!0}),Object.defineProperty(pa.prototype,"disabled",{get:function(){return this.nativeControl_.disabled},set:function(t){this.foundation_.setDisabled(t)},enumerable:!1,configurable:!0}),Object.defineProperty(pa.prototype,"value",{get:function(){return this.nativeControl_.value},set:function(t){this.nativeControl_.value=t},enumerable:!1,configurable:!0}),Object.defineProperty(pa.prototype,"ripple",{get:function(){return this.ripple_},enumerable:!1,configurable:!0}),pa.prototype.destroy=function(){this.ripple_.destroy(),fa.prototype.destroy.call(this)},pa.prototype.getDefaultFoundation=function(){var e=this;return new ua({addClass:function(t){return e.root_.classList.add(t)},removeClass:function(t){return e.root_.classList.remove(t)},setNativeControlDisabled:function(t){return e.nativeControl_.disabled=t}})},pa.prototype.createRipple_=function(){var n=this,t=a(a({},_t.createAdapter(this)),{registerInteractionHandler:function(t,e){return n.nativeControl_.addEventListener(t,e,d())},deregisterInteractionHandler:function(t,e){return n.nativeControl_.removeEventListener(t,e,d())},isSurfaceActive:function(){return!1},isUnbounded:function(){return!0}});return new _t(this.root_,new dt(t))},Object.defineProperty(pa.prototype,"nativeControl_",{get:function(){var t=ua.strings.NATIVE_CONTROL_SELECTOR,e=this.root_.querySelector(t);if(!e)throw new Error("Radio component requires a "+t+" element");return e},enumerable:!1,configurable:!0}),pa);
   function pa(){var t=null!==fa&&fa.apply(this,arguments)||this;return t.ripple_=t.createRipple_(),t}function ha(t){return(ha="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function _a(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function ya(t,e,n){return e&&_a(t.prototype,e),n&&_a(t,n),t}function ma(o){var a=Ea();return function(){var t,e,n,i=Ca(o);if(a){var r=Ca(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==ha(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function va(t){var i="function"==typeof Map?new Map:void 0;return(va=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return ba(t,arguments,Ca(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),ga(n,t)})(t)}function ba(t,e,n){return(ba=Ea()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&ga(r,n.prototype),r}).apply(null,arguments)}function Ea(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function ga(t,e){return(ga=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Ca(t){return(Ca=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var Aa=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&ga(t,e)}(n,va(HTMLElement));var e=ma(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).checked_=!1,t.disabled_=!1,t.radio_,t}return ya(n,[{key:"focus",value:function(){this.radio_.nativeControl_.focus()}},{key:"blur",value:function(){this.radio_.nativeControl_.blur()}},{key:"checked",get:function(){return this.checked_},set:function(t){this.checked_=t,this.radio_&&(this.radio_.checked=t)}},{key:"disabled",get:function(){return this.disabled_},set:function(t){this.disabled_=t,this.radio_&&(this.radio_.disabled=t)}}]),ya(n,[{key:"connectedCallback",value:function(){k.call(this),this.radio_=new da(this),this.radio_.checked=this.checked_,this.radio_.disabled=this.disabled_}},{key:"disconnectedCallback",value:function(){this.radio_.destroy(),F.call(this)}}]),n}();function Ta(t){return(Ta="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Ia(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function Sa(t,e,n){return e&&Ia(t.prototype,e),n&&Ia(t,n),t}function Oa(o){var a=wa();return function(){var t,e,n,i=Na(o);if(a){var r=Na(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Ta(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Ra(t){var i="function"==typeof Map?new Map:void 0;return(Ra=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return La(t,arguments,Na(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),xa(n,t)})(t)}function La(t,e,n){return(La=wa()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&xa(r,n.prototype),r}).apply(null,arguments)}function wa(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function xa(t,e){return(xa=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Na(t){return(Na=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}customElements.define("mdc-radio",Aa);var Da=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&xa(t,e)}(n,Ra(HTMLElement));var e=Oa(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).unbounded_=!1,t.ripple_,t}return Sa(n,[{key:"unbounded",get:function(){return this.unbounded_},set:function(t){this.unbounded_=t,this.ripple_&&(this.ripple_.unbounded=t)}}]),Sa(n,[{key:"connectedCallback",value:function(){k.call(this),this.ripple_=new R(this),this.ripple_.unbounded=this.unbounded_}},{key:"disconnectedCallback",value:function(){this.ripple_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-ripple",Da);
var Pa,ka={LABEL_FLOAT_ABOVE:"mdc-floating-label--float-above",LABEL_SHAKE:"mdc-floating-label--shake",ROOT:"mdc-floating-label"},Fa=(r(Ma,Pa=s),Object.defineProperty(Ma,"cssClasses",{get:function(){return ka},enumerable:!0,configurable:!0}),Object.defineProperty(Ma,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},getWidth:function(){return 0},registerInteractionHandler:function(){},deregisterInteractionHandler:function(){}}},enumerable:!0,configurable:!0}),Ma.prototype.init=function(){this.adapter_.registerInteractionHandler("animationend",this.shakeAnimationEndHandler_)},Ma.prototype.destroy=function(){this.adapter_.deregisterInteractionHandler("animationend",this.shakeAnimationEndHandler_)},Ma.prototype.getWidth=function(){return this.adapter_.getWidth()},Ma.prototype.shake=function(t){var e=Ma.cssClasses.LABEL_SHAKE;t?this.adapter_.addClass(e):this.adapter_.removeClass(e)},Ma.prototype.float=function(t){var e=Ma.cssClasses,n=e.LABEL_FLOAT_ABOVE,i=e.LABEL_SHAKE;t?this.adapter_.addClass(n):(this.adapter_.removeClass(n),this.adapter_.removeClass(i))},Ma.prototype.handleShakeAnimationEnd_=function(){var t=Ma.cssClasses.LABEL_SHAKE;this.adapter_.removeClass(t)},Ma);
   function Ma(t){var e=Pa.call(this,a(a({},Ma.defaultAdapter),t))||this;return e.shakeAnimationEndHandler_=function(){return e.handleShakeAnimationEnd_()},e}var Ha,ja=(r(Ba,Ha=u),Ba.attachTo=function(t){return new Ba(t)},Ba.prototype.shake=function(t){this.foundation_.shake(t)},Ba.prototype.float=function(t){this.foundation_.float(t)},Ba.prototype.getWidth=function(){return this.foundation_.getWidth()},Ba.prototype.getDefaultFoundation=function(){var n=this;return new Fa({addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},getWidth:function(){return function(t){var e=t;if(null!==e.offsetParent)return e.scrollWidth;var n=e.cloneNode(!0);n.style.setProperty("position","absolute"),n.style.setProperty("transform","translate(-9999px, -9999px)"),document.documentElement.appendChild(n);var i=n.scrollWidth;return document.documentElement.removeChild(n),i}(n.root_)},registerInteractionHandler:function(t,e){return n.listen(t,e)},deregisterInteractionHandler:function(t,e){return n.unlisten(t,e)}})},Ba);
   function Ba(){return null!==Ha&&Ha.apply(this,arguments)||this}
var Va,Ua={LINE_RIPPLE_ACTIVE:"mdc-line-ripple--active",LINE_RIPPLE_DEACTIVATING:"mdc-line-ripple--deactivating"},Ka=(r(Ga,Va=s),Object.defineProperty(Ga,"cssClasses",{get:function(){return Ua},enumerable:!0,configurable:!0}),Object.defineProperty(Ga,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},setStyle:function(){},registerEventHandler:function(){},deregisterEventHandler:function(){}}},enumerable:!0,configurable:!0}),Ga.prototype.init=function(){this.adapter_.registerEventHandler("transitionend",this.transitionEndHandler_)},Ga.prototype.destroy=function(){this.adapter_.deregisterEventHandler("transitionend",this.transitionEndHandler_)},Ga.prototype.activate=function(){this.adapter_.removeClass(Ua.LINE_RIPPLE_DEACTIVATING),this.adapter_.addClass(Ua.LINE_RIPPLE_ACTIVE)},Ga.prototype.setRippleCenter=function(t){this.adapter_.setStyle("transform-origin",t+"px center")},Ga.prototype.deactivate=function(){this.adapter_.addClass(Ua.LINE_RIPPLE_DEACTIVATING)},Ga.prototype.handleTransitionEnd=function(t){var e=this.adapter_.hasClass(Ua.LINE_RIPPLE_DEACTIVATING);"opacity"===t.propertyName&&e&&(this.adapter_.removeClass(Ua.LINE_RIPPLE_ACTIVE),this.adapter_.removeClass(Ua.LINE_RIPPLE_DEACTIVATING))},Ga);
   function Ga(t){var e=Va.call(this,a(a({},Ga.defaultAdapter),t))||this;return e.transitionEndHandler_=function(t){return e.handleTransitionEnd(t)},e}var Wa,qa=(r(za,Wa=u),za.attachTo=function(t){return new za(t)},za.prototype.activate=function(){this.foundation_.activate()},za.prototype.deactivate=function(){this.foundation_.deactivate()},za.prototype.setRippleCenter=function(t){this.foundation_.setRippleCenter(t)},za.prototype.getDefaultFoundation=function(){var n=this;return new Ka({addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},hasClass:function(t){return n.root_.classList.contains(t)},setStyle:function(t,e){return n.root_.style.setProperty(t,e)},registerEventHandler:function(t,e){return n.listen(t,e)},deregisterEventHandler:function(t,e){return n.unlisten(t,e)}})},za);
   function za(){return null!==Wa&&Wa.apply(this,arguments)||this}
var Xa,Ya={NOTCH_ELEMENT_SELECTOR:".mdc-notched-outline__notch"},Qa={NOTCH_ELEMENT_PADDING:8},Za={NO_LABEL:"mdc-notched-outline--no-label",OUTLINE_NOTCHED:"mdc-notched-outline--notched",OUTLINE_UPGRADED:"mdc-notched-outline--upgraded"},$a=(r(Ja,Xa=s),Object.defineProperty(Ja,"strings",{get:function(){return Ya},enumerable:!0,configurable:!0}),Object.defineProperty(Ja,"cssClasses",{get:function(){return Za},enumerable:!0,configurable:!0}),Object.defineProperty(Ja,"numbers",{get:function(){return Qa},enumerable:!0,configurable:!0}),Object.defineProperty(Ja,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},setNotchWidthProperty:function(){},removeNotchWidthProperty:function(){}}},enumerable:!0,configurable:!0}),Ja.prototype.notch=function(t){var e=Ja.cssClasses.OUTLINE_NOTCHED;0<t&&(t+=Qa.NOTCH_ELEMENT_PADDING),this.adapter_.setNotchWidthProperty(t),this.adapter_.addClass(e)},Ja.prototype.closeNotch=function(){var t=Ja.cssClasses.OUTLINE_NOTCHED;this.adapter_.removeClass(t),this.adapter_.removeNotchWidthProperty()},Ja);function Ja(t){return Xa.call(this,a(a({},Ja.defaultAdapter),t))||this}var ts,es=(r(ns,ts=u),ns.attachTo=function(t){return new ns(t)},ns.prototype.initialSyncWithDOM=function(){this.notchElement_=this.root_.querySelector(Ya.NOTCH_ELEMENT_SELECTOR);var t=this.root_.querySelector("."+Fa.cssClasses.ROOT);t?(t.style.transitionDuration="0s",this.root_.classList.add(Za.OUTLINE_UPGRADED),requestAnimationFrame(function(){t.style.transitionDuration=""})):this.root_.classList.add(Za.NO_LABEL)},ns.prototype.notch=function(t){this.foundation_.notch(t)},ns.prototype.closeNotch=function(){this.foundation_.closeNotch()},ns.prototype.getDefaultFoundation=function(){var e=this;return new $a({addClass:function(t){return e.root_.classList.add(t)},removeClass:function(t){return e.root_.classList.remove(t)},setNotchWidthProperty:function(t){return e.notchElement_.style.setProperty("width",t+"px")},removeNotchWidthProperty:function(){return e.notchElement_.style.removeProperty("width")}})},ns);
   function ns(){return null!==ts&&ts.apply(this,arguments)||this}
var is,rs={ACTIVATED:"mdc-select--activated",DISABLED:"mdc-select--disabled",FOCUSED:"mdc-select--focused",INVALID:"mdc-select--invalid",OUTLINED:"mdc-select--outlined",REQUIRED:"mdc-select--required",ROOT:"mdc-select",SELECTED_ITEM_CLASS:"mdc-list-item--selected",WITH_LEADING_ICON:"mdc-select--with-leading-icon"},os={ARIA_CONTROLS:"aria-controls",ARIA_SELECTED_ATTR:"aria-selected",CHANGE_EVENT:"MDCSelect:change",LABEL_SELECTOR:".mdc-floating-label",LEADING_ICON_SELECTOR:".mdc-select__icon",LINE_RIPPLE_SELECTOR:".mdc-line-ripple",MENU_SELECTOR:".mdc-select__menu",OUTLINE_SELECTOR:".mdc-notched-outline",SELECTED_ITEM_SELECTOR:"."+rs.SELECTED_ITEM_CLASS,SELECTED_TEXT_SELECTOR:".mdc-select__selected-text",SELECT_ANCHOR_SELECTOR:".mdc-select__anchor",VALUE_ATTR:"data-value"},as={LABEL_SCALE:.75,UNSET_INDEX:-1},ss=(r(cs,is=s),Object.defineProperty(cs,"cssClasses",{get:function(){return rs},enumerable:!1,configurable:!0}),Object.defineProperty(cs,"numbers",{get:function(){return as},enumerable:!1,configurable:!0}),Object.defineProperty(cs,"strings",{get:function(){return os},enumerable:!1,configurable:!0}),Object.defineProperty(cs,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},activateBottomLine:function(){},deactivateBottomLine:function(){},getSelectedMenuItem:function(){return null},hasLabel:function(){return!1},floatLabel:function(){},getLabelWidth:function(){return 0},hasOutline:function(){return!1},notchOutline:function(){},closeOutline:function(){},setRippleCenter:function(){},notifyChange:function(){},setSelectedText:function(){},isSelectAnchorFocused:function(){return!1},getSelectAnchorAttr:function(){return""},setSelectAnchorAttr:function(){},openMenu:function(){},closeMenu:function(){},getAnchorElement:function(){return null},setMenuAnchorElement:function(){},setMenuAnchorCorner:function(){},setMenuWrapFocus:function(){},setAttributeAtIndex:function(){},removeAttributeAtIndex:function(){},focusMenuItemAtIndex:function(){},getMenuItemCount:function(){return 0},getMenuItemValues:function(){return[]},getMenuItemTextAtIndex:function(){return""},getMenuItemAttr:function(){return""},addClassAtIndex:function(){},removeClassAtIndex:function(){}}},enumerable:!1,configurable:!0}),cs.prototype.getSelectedIndex=function(){return this.selectedIndex},cs.prototype.setSelectedIndex=function(t,e){if(void 0===e&&(e=!1),!(t>=this.adapter_.getMenuItemCount())){var n=this.selectedIndex;this.selectedIndex=t,this.selectedIndex===as.UNSET_INDEX?this.adapter_.setSelectedText(""):this.adapter_.setSelectedText(this.adapter_.getMenuItemTextAtIndex(this.selectedIndex).trim()),n!==as.UNSET_INDEX&&(this.adapter_.removeClassAtIndex(n,rs.SELECTED_ITEM_CLASS),this.adapter_.removeAttributeAtIndex(n,os.ARIA_SELECTED_ATTR)),this.selectedIndex!==as.UNSET_INDEX&&(this.adapter_.addClassAtIndex(this.selectedIndex,rs.SELECTED_ITEM_CLASS),this.adapter_.setAttributeAtIndex(this.selectedIndex,os.ARIA_SELECTED_ATTR,"true")),this.layout(),e&&this.adapter_.closeMenu(),this.handleChange()}},cs.prototype.getDisabled=function(){return this.disabled},cs.prototype.setDisabled=function(t){this.disabled=t,this.disabled?(this.adapter_.addClass(rs.DISABLED),this.adapter_.closeMenu()):this.adapter_.removeClass(rs.DISABLED),this.leadingIcon&&this.leadingIcon.setDisabled(this.disabled),this.adapter_.setSelectAnchorAttr("tabindex",this.disabled?"-1":"0"),this.adapter_.setSelectAnchorAttr("aria-disabled",this.disabled.toString())},cs.prototype.setHelperTextContent=function(t){this.helperText&&this.helperText.setContent(t)},cs.prototype.layout=function(){if(this.adapter_.hasLabel()){var t=this.selectedIndex!==as.UNSET_INDEX&&0<this.adapter_.getMenuItemTextAtIndex(this.selectedIndex).length;this.notchOutline(t)}},cs.prototype.handleMenuOpened=function(){if(0!==this.adapter_.getMenuItemValues().length){this.adapter_.addClass(rs.ACTIVATED);var t=0<=this.selectedIndex?this.selectedIndex:0;this.adapter_.focusMenuItemAtIndex(t)}},cs.prototype.handleMenuClosed=function(){this.adapter_.removeClass(rs.ACTIVATED),this.isMenuOpen=!1,this.adapter_.setSelectAnchorAttr("aria-expanded","false"),this.adapter_.isSelectAnchorFocused()||this.blur()},cs.prototype.handleChange=function(){this.updateLabel(),this.adapter_.hasClass(rs.REQUIRED)&&(this.setValid(this.isValid()),this.helperText&&this.helperText.setValidity(this.isValid()))},cs.prototype.handleMenuItemAction=function(t){this.setSelectedIndex(t,!0)},cs.prototype.handleFocus=function(){this.adapter_.addClass(rs.FOCUSED),this.adapter_.hasLabel()&&(this.notchOutline(!0),this.adapter_.floatLabel(!0)),this.adapter_.activateBottomLine(),this.helperText&&this.helperText.showToScreenReader()},cs.prototype.handleBlur=function(){this.isMenuOpen||this.blur()},cs.prototype.handleClick=function(t){this.isMenuOpen||(this.adapter_.setRippleCenter(t),this.adapter_.openMenu(),this.isMenuOpen=!0,this.adapter_.setSelectAnchorAttr("aria-expanded","true"))},cs.prototype.handleKeydown=function(t){if(!this.isMenuOpen){var e="Enter"===t.key||13===t.keyCode,n="Space"===t.key||32===t.keyCode,i="ArrowUp"===t.key||38===t.keyCode,r="ArrowDown"===t.key||40===t.keyCode;this.adapter_.hasClass(rs.FOCUSED)&&(e||n||i||r)&&(this.adapter_.openMenu(),this.isMenuOpen=!0,this.adapter_.setSelectAnchorAttr("aria-expanded","true"),t.preventDefault())}},cs.prototype.notchOutline=function(t){if(this.adapter_.hasOutline()){var e=this.adapter_.hasClass(rs.FOCUSED);if(t){var n=as.LABEL_SCALE,i=this.adapter_.getLabelWidth()*n;this.adapter_.notchOutline(i)}else e||this.adapter_.closeOutline()}},cs.prototype.setLeadingIconAriaLabel=function(t){this.leadingIcon&&this.leadingIcon.setAriaLabel(t)},cs.prototype.setLeadingIconContent=function(t){this.leadingIcon&&this.leadingIcon.setContent(t)},cs.prototype.setValid=function(t){this.adapter_.setSelectAnchorAttr("aria-invalid",(!t).toString()),t?this.adapter_.removeClass(rs.INVALID):this.adapter_.addClass(rs.INVALID)},cs.prototype.isValid=function(){return!(this.adapter_.hasClass(rs.REQUIRED)&&!this.adapter_.hasClass(rs.DISABLED))||this.selectedIndex!==as.UNSET_INDEX&&(0!==this.selectedIndex||0<this.adapter_.getMenuItemTextAtIndex(this.selectedIndex).length)},cs.prototype.setRequired=function(t){t?this.adapter_.addClass(rs.REQUIRED):this.adapter_.removeClass(rs.REQUIRED),this.adapter_.setSelectAnchorAttr("aria-required",t.toString())},cs.prototype.getRequired=function(){return"true"===this.adapter_.getSelectAnchorAttr("aria-required")},cs.prototype.initMenu=function(){var t=this.adapter_.getAnchorElement();t&&(this.adapter_.setMenuAnchorElement(t),this.adapter_.setMenuAnchorCorner(To.BOTTOM_START)),this.adapter_.setMenuWrapFocus(!1),this.updateLabel()},cs.prototype.updateLabel=function(){var t=this.selectedIndex!==as.UNSET_INDEX&&0<this.adapter_.getMenuItemTextAtIndex(this.selectedIndex).length;this.adapter_.hasLabel()&&(this.notchOutline(t),this.adapter_.hasClass(rs.FOCUSED)||this.adapter_.floatLabel(t))},cs.prototype.blur=function(){this.adapter_.removeClass(rs.FOCUSED),this.updateLabel(),this.adapter_.deactivateBottomLine(),this.adapter_.hasClass(rs.REQUIRED)&&(this.setValid(this.isValid()),this.helperText&&this.helperText.setValidity(this.isValid()))},cs);function cs(t,e){void 0===e&&(e={});var n=is.call(this,a(a({},cs.defaultAdapter),t))||this;return n.selectedIndex=as.UNSET_INDEX,n.disabled=!1,n.isMenuOpen=!1,n.leadingIcon=e.leadingIcon,n.helperText=e.helperText,n.setDisabled(n.adapter_.hasClass(rs.DISABLED)),n}var us,ls={ARIA_HIDDEN:"aria-hidden",ROLE:"role"},fs={HELPER_TEXT_PERSISTENT:"mdc-select-helper-text--persistent",HELPER_TEXT_VALIDATION_MSG:"mdc-select-helper-text--validation-msg"},ds=(r(ps,us=s),Object.defineProperty(ps,"cssClasses",{get:function(){return fs},enumerable:!0,configurable:!0}),Object.defineProperty(ps,"strings",{get:function(){return ls},enumerable:!0,configurable:!0}),Object.defineProperty(ps,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},setAttr:function(){},removeAttr:function(){},setContent:function(){}}},enumerable:!0,configurable:!0}),ps.prototype.setContent=function(t){this.adapter_.setContent(t)},ps.prototype.setPersistent=function(t){t?this.adapter_.addClass(fs.HELPER_TEXT_PERSISTENT):this.adapter_.removeClass(fs.HELPER_TEXT_PERSISTENT)},ps.prototype.setValidation=function(t){t?this.adapter_.addClass(fs.HELPER_TEXT_VALIDATION_MSG):this.adapter_.removeClass(fs.HELPER_TEXT_VALIDATION_MSG)},ps.prototype.showToScreenReader=function(){this.adapter_.removeAttr(ls.ARIA_HIDDEN)},ps.prototype.setValidity=function(t){var e=this.adapter_.hasClass(fs.HELPER_TEXT_PERSISTENT),n=this.adapter_.hasClass(fs.HELPER_TEXT_VALIDATION_MSG)&&!t;n?this.adapter_.setAttr(ls.ROLE,"alert"):this.adapter_.removeAttr(ls.ROLE),e||n||this.hide_()},ps.prototype.hide_=function(){this.adapter_.setAttr(ls.ARIA_HIDDEN,"true")},ps);
   function ps(t){return us.call(this,a(a({},ps.defaultAdapter),t))||this}var hs,_s=(r(ys,hs=u),ys.attachTo=function(t){return new ys(t)},Object.defineProperty(ys.prototype,"foundation",{get:function(){return this.foundation_},enumerable:!0,configurable:!0}),ys.prototype.getDefaultFoundation=function(){var n=this;return new ds({addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},hasClass:function(t){return n.root_.classList.contains(t)},setAttr:function(t,e){return n.root_.setAttribute(t,e)},removeAttr:function(t){return n.root_.removeAttribute(t)},setContent:function(t){n.root_.textContent=t}})},ys);
   function ys(){return null!==hs&&hs.apply(this,arguments)||this}
var ms,vs={ICON_EVENT:"MDCSelect:icon",ICON_ROLE:"button"},bs=["click","keydown"],Es=(r(gs,ms=s),Object.defineProperty(gs,"strings",{get:function(){return vs},enumerable:!0,configurable:!0}),Object.defineProperty(gs,"defaultAdapter",{get:function(){return{getAttr:function(){return null},setAttr:function(){},removeAttr:function(){},setContent:function(){},registerInteractionHandler:function(){},deregisterInteractionHandler:function(){},notifyIconAction:function(){}}},enumerable:!0,configurable:!0}),gs.prototype.init=function(){var e=this;this.savedTabIndex_=this.adapter_.getAttr("tabindex"),bs.forEach(function(t){e.adapter_.registerInteractionHandler(t,e.interactionHandler_)})},gs.prototype.destroy=function(){var e=this;bs.forEach(function(t){e.adapter_.deregisterInteractionHandler(t,e.interactionHandler_)})},gs.prototype.setDisabled=function(t){this.savedTabIndex_&&(t?(this.adapter_.setAttr("tabindex","-1"),this.adapter_.removeAttr("role")):(this.adapter_.setAttr("tabindex",this.savedTabIndex_),this.adapter_.setAttr("role",vs.ICON_ROLE)))},gs.prototype.setAriaLabel=function(t){this.adapter_.setAttr("aria-label",t)},gs.prototype.setContent=function(t){this.adapter_.setContent(t)},gs.prototype.handleInteraction=function(t){var e="Enter"===t.key||13===t.keyCode;"click"!==t.type&&!e||this.adapter_.notifyIconAction()},gs);
   function gs(t){var e=ms.call(this,a(a({},gs.defaultAdapter),t))||this;return e.savedTabIndex_=null,e.interactionHandler_=function(t){return e.handleInteraction(t)},e}var Cs,As=(r(Ts,Cs=u),Ts.attachTo=function(t){return new Ts(t)},Object.defineProperty(Ts.prototype,"foundation",{get:function(){return this.foundation_},enumerable:!0,configurable:!0}),Ts.prototype.getDefaultFoundation=function(){var n=this;return new Es({getAttr:function(t){return n.root_.getAttribute(t)},setAttr:function(t,e){return n.root_.setAttribute(t,e)},removeAttr:function(t){return n.root_.removeAttribute(t)},setContent:function(t){n.root_.textContent=t},registerInteractionHandler:function(t,e){return n.listen(t,e)},deregisterInteractionHandler:function(t,e){return n.unlisten(t,e)},notifyIconAction:function(){return n.emit(Es.strings.ICON_EVENT,{},!0)}})},Ts);
   function Ts(){return null!==Cs&&Cs.apply(this,arguments)||this}
var Is,Ss=(r(Os,Is=u),Os.attachTo=function(t){return new Os(t)},Os.prototype.initialize=function(t,e,n,i,r){if(void 0===t&&(t=function(t){return new ja(t)}),void 0===e&&(e=function(t){return new qa(t)}),void 0===n&&(n=function(t){return new es(t)}),void 0===i&&(i=function(t){return new As(t)}),void 0===r&&(r=function(t){return new _s(t)}),this.selectAnchor=this.root_.querySelector(os.SELECT_ANCHOR_SELECTOR),this.selectedText=this.root_.querySelector(os.SELECTED_TEXT_SELECTOR),!this.selectedText)throw new Error("MDCSelect: Missing required element: The following selector must be present: '"+os.SELECTED_TEXT_SELECTOR+"'");if(this.selectAnchor.hasAttribute(os.ARIA_CONTROLS)){var o=document.getElementById(this.selectAnchor.getAttribute(os.ARIA_CONTROLS));o&&(this.helperText=r(o))}var a=this.root_.querySelector(os.LABEL_SELECTOR);this.label=a?t(a):null;var s=this.root_.querySelector(os.LINE_RIPPLE_SELECTOR);this.lineRipple=s?e(s):null;var c=this.root_.querySelector(os.OUTLINE_SELECTOR);this.outline=c?n(c):null;var u=this.root_.querySelector(os.LEADING_ICON_SELECTOR);u&&(this.leadingIcon=i(u)),this.root_.classList.contains(rs.OUTLINED)||(this.ripple=this.createRipple())},Os.prototype.initialSyncWithDOM=function(){var e=this;this.handleChange=function(){e.foundation_.handleChange()},this.handleFocus=function(){e.foundation_.handleFocus()},this.handleBlur=function(){e.foundation_.handleBlur()},this.handleClick=function(t){e.selectAnchor.focus(),e.foundation_.handleClick(e.getNormalizedXCoordinate(t))},this.handleKeydown=function(t){e.foundation_.handleKeydown(t)},this.handleMenuItemAction=function(t){e.foundation_.handleMenuItemAction(t.detail.index)},this.handleMenuOpened=function(){e.foundation_.handleMenuOpened()},this.handleMenuClose=function(){e.menu.open=!1},this.handleMenuClosed=function(){e.foundation_.handleMenuClosed()},this.selectAnchor.addEventListener("focus",this.handleFocus),this.selectAnchor.addEventListener("blur",this.handleBlur),this.selectAnchor.addEventListener("click",this.handleClick),this.selectAnchor.addEventListener("keydown",this.handleKeydown)},Os.prototype.destroy=function(){this.selectAnchor.removeEventListener("change",this.handleChange),this.selectAnchor.removeEventListener("focus",this.handleFocus),this.selectAnchor.removeEventListener("blur",this.handleBlur),this.selectAnchor.removeEventListener("keydown",this.handleKeydown),this.selectAnchor.removeEventListener("click",this.handleClick),this.menu.unlisten("MDCMenuSurface:close",this.handleMenuClose),this.menu.unlisten(Oo.CLOSED_EVENT,this.handleMenuClosed),this.menu.unlisten(Oo.OPENED_EVENT,this.handleMenuOpened),this.menu.unlisten(Uo.SELECTED_EVENT,this.handleMenuItemAction),this.menu.destroy(),this.ripple&&this.ripple.destroy(),this.outline&&this.outline.destroy(),this.leadingIcon&&this.leadingIcon.destroy(),this.helperText&&this.helperText.destroy(),Is.prototype.destroy.call(this)},Object.defineProperty(Os.prototype,"selectedIndex",{get:function(){return this.foundation_.getSelectedIndex()},set:function(t){this.foundation_.setSelectedIndex(t,!0)},enumerable:!1,configurable:!0}),Object.defineProperty(Os.prototype,"disabled",{get:function(){return this.foundation_.getDisabled()},set:function(t){this.foundation_.setDisabled(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Os.prototype,"leadingIconAriaLabel",{set:function(t){this.foundation_.setLeadingIconAriaLabel(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Os.prototype,"leadingIconContent",{set:function(t){this.foundation_.setLeadingIconContent(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Os.prototype,"helperTextContent",{set:function(t){this.foundation_.setHelperTextContent(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Os.prototype,"valid",{get:function(){return this.foundation_.isValid()},set:function(t){this.foundation_.setValid(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Os.prototype,"required",{get:function(){return this.foundation_.getRequired()},set:function(t){this.foundation_.setRequired(t)},enumerable:!1,configurable:!0}),Os.prototype.layout=function(){this.foundation_.layout()},Os.prototype.getDefaultFoundation=function(){var t=a(a(a(a({},this.getSelectAdapterMethods()),this.getCommonAdapterMethods()),this.getOutlineAdapterMethods()),this.getLabelAdapterMethods());return new ss(t,this.getFoundationMap())},Os.prototype.menuSetup=function(t){this.menuElement=t,this.menu=this.menuElement.menu_,this.menu.listen("MDCMenuSurface:close",this.handleMenuClose),this.menu.listen(Oo.CLOSED_EVENT,this.handleMenuClosed),this.menu.listen(Oo.OPENED_EVENT,this.handleMenuOpened),this.menu.listen(Uo.SELECTED_EVENT,this.handleMenuItemAction),this.foundation_.initMenu()},Os.prototype.createRipple=function(){var n=this,t=a(a({},_t.createAdapter({root_:this.selectAnchor})),{registerInteractionHandler:function(t,e){n.selectAnchor.addEventListener(t,e)},deregisterInteractionHandler:function(t,e){n.selectAnchor.removeEventListener(t,e)}});return new _t(this.selectAnchor,new dt(t))},Os.prototype.getSelectAdapterMethods=function(){var i=this;return{getSelectedMenuItem:function(){return i.menuElement.querySelector(os.SELECTED_ITEM_SELECTOR)},getMenuItemAttr:function(t,e){return t.getAttribute(e)},setSelectedText:function(t){i.selectedText.value=t},isSelectAnchorFocused:function(){return document.activeElement===i.selectAnchor},getSelectAnchorAttr:function(t){return i.selectAnchor.getAttribute(t)},setSelectAnchorAttr:function(t,e){i.selectAnchor.setAttribute(t,e)},openMenu:function(){i.menu.open=!0},closeMenu:function(){i.menu.open=!1},getAnchorElement:function(){return i.root_.querySelector(os.SELECT_ANCHOR_SELECTOR)},setMenuAnchorElement:function(t){i.menu.setAnchorElement(t)},setMenuAnchorCorner:function(t){i.menu.setAnchorCorner(t)},setMenuWrapFocus:function(t){i.menu.wrapFocus=t},setAttributeAtIndex:function(t,e,n){i.menu.items[t].setAttribute(e,n)},removeAttributeAtIndex:function(t,e){i.menu.items[t].removeAttribute(e)},focusMenuItemAtIndex:function(t){i.menu.items[t].focus()},getMenuItemCount:function(){return i.menu.items.length},getMenuItemValues:function(){return i.menu.items.map(function(t){return t.getAttribute(os.VALUE_ATTR)||""})},getMenuItemTextAtIndex:function(t){return i.menu.items[t].textContent},addClassAtIndex:function(t,e){i.menu.items[t].classList.add(e)},removeClassAtIndex:function(t,e){i.menu.items[t].classList.remove(e)}}},Os.prototype.getCommonAdapterMethods=function(){var n=this;return{addClass:function(t){n.root_.classList.add(t)},removeClass:function(t){n.root_.classList.remove(t)},hasClass:function(t){return n.root_.classList.contains(t)},setRippleCenter:function(t){n.lineRipple&&n.lineRipple.setRippleCenter(t)},activateBottomLine:function(){n.lineRipple&&n.lineRipple.activate()},deactivateBottomLine:function(){n.lineRipple&&n.lineRipple.deactivate()},notifyChange:function(t){var e=n.selectedIndex;n.emit(os.CHANGE_EVENT,{value:t,index:e},!0)}}},Os.prototype.getOutlineAdapterMethods=function(){var e=this;return{hasOutline:function(){return Boolean(e.outline)},notchOutline:function(t){e.outline&&e.outline.notch(t)},closeOutline:function(){e.outline&&e.outline.closeNotch()}}},Os.prototype.getLabelAdapterMethods=function(){var e=this;return{hasLabel:function(){return!!e.label},floatLabel:function(t){e.label&&e.label.float(t)},getLabelWidth:function(){return e.label?e.label.getWidth():0}}},Os.prototype.getNormalizedXCoordinate=function(t){var e=t.target.getBoundingClientRect();return(this.isTouchEvent(t)?t.touches[0].clientX:t.clientX)-e.left},Os.prototype.isTouchEvent=function(t){return Boolean(t.touches)},Os.prototype.getFoundationMap=function(){return{helperText:this.helperText?this.helperText.foundation:void 0,leadingIcon:this.leadingIcon?this.leadingIcon.foundation:void 0}},Os);function Os(){return null!==Is&&Is.apply(this,arguments)||this}function Rs(t){return(Rs="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Ls(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function ws(t,e,n){return e&&Ls(t.prototype,e),n&&Ls(t,n),t}function xs(o){var a=Ps();return function(){var t,e,n,i=Fs(o);if(a){var r=Fs(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Rs(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Ns(t){var i="function"==typeof Map?new Map:void 0;return(Ns=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Ds(t,arguments,Fs(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),ks(n,t)})(t)}function Ds(t,e,n){return(Ds=Ps()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&ks(r,n.prototype),r}).apply(null,arguments)}function Ps(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function ks(t,e){return(ks=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Fs(t){return(Fs=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var Ms=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&ks(t,e)}(n,Ns(HTMLElement));var e=xs(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).selectedIndex_=-1,t.disabled_=!1,t.valid_=!0,t.required_=!1,t.select_,t}return ws(n,[{key:"focus",value:function(){this.select_.selectAnchor.focus()}},{key:"blur",value:function(){this.select_.selectAnchor.blur()}},{key:"selectedIndex",get:function(){return this.select_?this.select_.selectedIndex:this.selectedIndex_},set:function(t){this.selectedIndex_=t,this.select_&&(this.select_.selectedIndex=t)}},{key:"disabled",get:function(){return this.select_?this.select_.disabled:this.disabled_},set:function(t){this.disabled_=t,this.select_&&(this.select_.disabled=t)}},{key:"valid",get:function(){return this.select_?this.select_.valid:this.valid_},set:function(t){this.valid_=t,this.select_&&(this.select_.valid=t)}},{key:"required",get:function(){return this.select_?this.select_.required:this.required_},set:function(t){this.required_=t,this.select_&&(this.select_.required=t)}}]),ws(n,[{key:"connectedCallback",value:function(){k.call(this),this.select_=new Ss(this),this.select_.disabled=this.disabled_,this.select_.valid=this.valid_,this.select_.required=this.required_}},{key:"menuSetup",value:function(t){this.select_.menuSetup(t),this.select_.selectedIndex=this.selectedIndex_}},{key:"disconnectedCallback",value:function(){this.select_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-select",Ms);
var Hs,js={ACTIVE:"mdc-slider--active",DISABLED:"mdc-slider--disabled",DISCRETE:"mdc-slider--discrete",FOCUS:"mdc-slider--focus",HAS_TRACK_MARKER:"mdc-slider--display-markers",IN_TRANSIT:"mdc-slider--in-transit",IS_DISCRETE:"mdc-slider--discrete"},Bs={ARIA_DISABLED:"aria-disabled",ARIA_VALUEMAX:"aria-valuemax",ARIA_VALUEMIN:"aria-valuemin",ARIA_VALUENOW:"aria-valuenow",CHANGE_EVENT:"MDCSlider:change",INPUT_EVENT:"MDCSlider:input",PIN_VALUE_MARKER_SELECTOR:".mdc-slider__pin-value-marker",STEP_DATA_ATTR:"data-step",THUMB_CONTAINER_SELECTOR:".mdc-slider__thumb-container",TRACK_MARKER_CONTAINER_SELECTOR:".mdc-slider__track-marker-container",TRACK_SELECTOR:".mdc-slider__track"},Vs={PAGE_FACTOR:4},Us=!!window.PointerEvent,Ks=Us?["pointerdown"]:["mousedown","touchstart"],Gs=Us?["pointerup"]:["mouseup","touchend"],Ws={mousedown:"mousemove",pointerdown:"pointermove",touchstart:"touchmove"},qs="ArrowDown",zs="ArrowLeft",Xs="ArrowRight",Ys="ArrowUp",Qs="End",Zs="Home",$s="PageDown",Js="PageUp",tc=(r(ec,Hs=s),Object.defineProperty(ec,"cssClasses",{get:function(){return js},enumerable:!1,configurable:!0}),Object.defineProperty(ec,"strings",{get:function(){return Bs},enumerable:!1,configurable:!0}),Object.defineProperty(ec,"numbers",{get:function(){return Vs},enumerable:!1,configurable:!0}),Object.defineProperty(ec,"defaultAdapter",{get:function(){return{hasClass:function(){return!1},addClass:function(){},removeClass:function(){},getAttribute:function(){return null},setAttribute:function(){},removeAttribute:function(){},computeBoundingRect:function(){return{top:0,right:0,bottom:0,left:0,width:0,height:0}},getTabIndex:function(){return 0},registerInteractionHandler:function(){},deregisterInteractionHandler:function(){},registerThumbContainerInteractionHandler:function(){},deregisterThumbContainerInteractionHandler:function(){},registerBodyInteractionHandler:function(){},deregisterBodyInteractionHandler:function(){},registerResizeHandler:function(){},deregisterResizeHandler:function(){},notifyInput:function(){},notifyChange:function(){},setThumbContainerStyleProperty:function(){},setTrackStyleProperty:function(){},setMarkerValue:function(){},setTrackMarkers:function(){},isRTL:function(){return!1}}},enumerable:!1,configurable:!0}),ec.prototype.init=function(){var e=this;this.isDiscrete_=this.adapter_.hasClass(js.IS_DISCRETE),this.hasTrackMarker_=this.adapter_.hasClass(js.HAS_TRACK_MARKER),Ks.forEach(function(t){e.adapter_.registerInteractionHandler(t,e.interactionStartHandler_),e.adapter_.registerThumbContainerInteractionHandler(t,e.thumbContainerPointerHandler_)}),this.adapter_.registerInteractionHandler("keydown",this.keydownHandler_),this.adapter_.registerInteractionHandler("focus",this.focusHandler_),this.adapter_.registerInteractionHandler("blur",this.blurHandler_),this.adapter_.registerResizeHandler(this.resizeHandler_),this.layout(),this.isDiscrete_&&0===this.getStep()&&(this.step_=1)},ec.prototype.destroy=function(){var e=this;Ks.forEach(function(t){e.adapter_.deregisterInteractionHandler(t,e.interactionStartHandler_),e.adapter_.deregisterThumbContainerInteractionHandler(t,e.thumbContainerPointerHandler_)}),this.adapter_.deregisterInteractionHandler("keydown",this.keydownHandler_),this.adapter_.deregisterInteractionHandler("focus",this.focusHandler_),this.adapter_.deregisterInteractionHandler("blur",this.blurHandler_),this.adapter_.deregisterResizeHandler(this.resizeHandler_)},ec.prototype.setupTrackMarker=function(){this.isDiscrete_&&this.hasTrackMarker_&&0!==this.getStep()&&this.adapter_.setTrackMarkers(this.getStep(),this.getMax(),this.getMin())},ec.prototype.layout=function(){this.rect_=this.adapter_.computeBoundingRect(),this.updateUIForCurrentValue_()},ec.prototype.getValue=function(){return this.value_},ec.prototype.setValue=function(t){this.setValue_(t,!1)},ec.prototype.getMax=function(){return this.max_},ec.prototype.setMax=function(t){if(t<this.min_)throw new Error("Cannot set max to be less than the slider's minimum value");this.max_=t,this.setValue_(this.value_,!1,!0),this.adapter_.setAttribute(Bs.ARIA_VALUEMAX,String(this.max_)),this.setupTrackMarker()},ec.prototype.getMin=function(){return this.min_},ec.prototype.setMin=function(t){if(t>this.max_)throw new Error("Cannot set min to be greater than the slider's maximum value");this.min_=t,this.setValue_(this.value_,!1,!0),this.adapter_.setAttribute(Bs.ARIA_VALUEMIN,String(this.min_)),this.setupTrackMarker()},ec.prototype.getStep=function(){return this.step_},ec.prototype.setStep=function(t){if(t<0)throw new Error("Step cannot be set to a negative number");this.isDiscrete_&&("number"!=typeof t||t<1)&&(t=1),this.step_=t,this.setValue_(this.value_,!1,!0),this.setupTrackMarker()},ec.prototype.isDisabled=function(){return this.disabled_},ec.prototype.setDisabled=function(t){this.disabled_=t,this.toggleClass_(js.DISABLED,this.disabled_),this.disabled_?(this.savedTabIndex_=this.adapter_.getTabIndex(),this.adapter_.setAttribute(Bs.ARIA_DISABLED,"true"),this.adapter_.removeAttribute("tabindex")):(this.adapter_.removeAttribute(Bs.ARIA_DISABLED),isNaN(this.savedTabIndex_)||this.adapter_.setAttribute("tabindex",String(this.savedTabIndex_)))},ec.prototype.handleDown_=function(t){var n=this;if(!this.disabled_){this.preventFocusState_=!0,this.setInTransit_(!this.handlingThumbTargetEvt_),this.handlingThumbTargetEvt_=!1,this.setActive_(!0);var i=function(t){n.handleMove_(t)},r=Ws[t.type],e=function e(){n.handleUp_(),n.adapter_.deregisterBodyInteractionHandler(r,i),Gs.forEach(function(t){return n.adapter_.deregisterBodyInteractionHandler(t,e)})};this.adapter_.registerBodyInteractionHandler(r,i),Gs.forEach(function(t){return n.adapter_.registerBodyInteractionHandler(t,e)}),this.setValueFromEvt_(t)}},ec.prototype.handleMove_=function(t){t.preventDefault(),this.setValueFromEvt_(t)},ec.prototype.handleUp_=function(){this.setActive_(!1),this.adapter_.notifyChange()},ec.prototype.getClientX_=function(t){return t.targetTouches&&0<t.targetTouches.length?t.targetTouches[0].clientX:t.clientX},ec.prototype.setValueFromEvt_=function(t){var e=this.getClientX_(t),n=this.computeValueFromClientX_(e);this.setValue_(n,!0)},ec.prototype.computeValueFromClientX_=function(t){var e=this.max_,n=this.min_,i=(t-this.rect_.left)/this.rect_.width;return this.adapter_.isRTL()&&(i=1-i),n+i*(e-n)},ec.prototype.handleKeydown_=function(t){var e=this.getKeyId_(t),n=this.getValueForKeyId_(e);isNaN(n)||(t.preventDefault(),this.adapter_.addClass(js.FOCUS),this.setValue_(n,!0),this.adapter_.notifyChange())},ec.prototype.getKeyId_=function(t){return t.key===zs||37===t.keyCode?zs:t.key===Xs||39===t.keyCode?Xs:t.key===Ys||38===t.keyCode?Ys:t.key===qs||40===t.keyCode?qs:t.key===Zs||36===t.keyCode?Zs:t.key===Qs||35===t.keyCode?Qs:t.key===Js||33===t.keyCode?Js:t.key===$s||34===t.keyCode?$s:""},ec.prototype.getValueForKeyId_=function(t){var e=this.max_,n=this.min_,i=this.step_||(e-n)/100;switch(!this.adapter_.isRTL()||t!==zs&&t!==Xs||(i=-i),t){case zs:case qs:return this.value_-i;case Xs:case Ys:return this.value_+i;case Zs:return this.min_;case Qs:return this.max_;case Js:return this.value_+i*Vs.PAGE_FACTOR;case $s:return this.value_-i*Vs.PAGE_FACTOR;default:return NaN}},ec.prototype.handleFocus_=function(){this.preventFocusState_||this.adapter_.addClass(js.FOCUS)},ec.prototype.handleBlur_=function(){this.preventFocusState_=!1,this.adapter_.removeClass(js.FOCUS)},ec.prototype.setValue_=function(t,e,n){if(void 0===n&&(n=!1),t!==this.value_||n){var i=this.min_,r=this.max_,o=t===i||t===r;this.step_&&!o&&(t=this.quantize_(t)),t<i?t=i:r<t&&(t=r),t=t||0,this.value_=t,this.adapter_.setAttribute(Bs.ARIA_VALUENOW,String(this.value_)),this.updateUIForCurrentValue_(),e&&(this.adapter_.notifyInput(),this.isDiscrete_&&this.adapter_.setMarkerValue(t))}},ec.prototype.quantize_=function(t){return Math.round(t/this.step_)*this.step_},ec.prototype.updateUIForCurrentValue_=function(){var e=this,t=this.max_,n=this.min_,i=(this.value_-n)/(t-n),r=i*this.rect_.width;this.adapter_.isRTL()&&(r=this.rect_.width-r);var o="undefined"!=typeof window,a=o?at(window,"transform"):"transform",s=o?st(window,"transitionend"):"transitionend";this.inTransit_&&this.adapter_.registerThumbContainerInteractionHandler(s,function t(){e.setInTransit_(!1),e.adapter_.deregisterThumbContainerInteractionHandler(s,t)}),requestAnimationFrame(function(){e.adapter_.setThumbContainerStyleProperty(a,"translateX("+r+"px) translateX(-50%)"),e.adapter_.setTrackStyleProperty(a,"scaleX("+i+")")})},ec.prototype.setActive_=function(t){this.active_=t,this.toggleClass_(js.ACTIVE,this.active_)},ec.prototype.setInTransit_=function(t){this.inTransit_=t,this.toggleClass_(js.IN_TRANSIT,this.inTransit_)},ec.prototype.toggleClass_=function(t,e){e?this.adapter_.addClass(t):this.adapter_.removeClass(t)},ec);function ec(t){var e=Hs.call(this,a(a({},ec.defaultAdapter),t))||this;return e.savedTabIndex_=NaN,e.active_=!1,e.inTransit_=!1,e.isDiscrete_=!1,e.hasTrackMarker_=!1,e.handlingThumbTargetEvt_=!1,e.min_=0,e.max_=100,e.step_=0,e.value_=0,e.disabled_=!1,e.preventFocusState_=!1,e.thumbContainerPointerHandler_=function(){return e.handlingThumbTargetEvt_=!0},e.interactionStartHandler_=function(t){return e.handleDown_(t)},e.keydownHandler_=function(t){return e.handleKeydown_(t)},e.focusHandler_=function(){return e.handleFocus_()},e.blurHandler_=function(){return e.handleBlur_()},e.resizeHandler_=function(){return e.layout()},e}var nc,ic=(r(rc,nc=u),rc.attachTo=function(t){return new rc(t)},Object.defineProperty(rc.prototype,"value",{get:function(){return this.foundation_.getValue()},set:function(t){this.foundation_.setValue(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rc.prototype,"min",{get:function(){return this.foundation_.getMin()},set:function(t){this.foundation_.setMin(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rc.prototype,"max",{get:function(){return this.foundation_.getMax()},set:function(t){this.foundation_.setMax(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rc.prototype,"step",{get:function(){return this.foundation_.getStep()},set:function(t){this.foundation_.setStep(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rc.prototype,"disabled",{get:function(){return this.foundation_.isDisabled()},set:function(t){this.foundation_.setDisabled(t)},enumerable:!1,configurable:!0}),rc.prototype.initialize=function(){this.thumbContainer_=this.root_.querySelector(Bs.THUMB_CONTAINER_SELECTOR),this.track_=this.root_.querySelector(Bs.TRACK_SELECTOR),this.pinValueMarker_=this.root_.querySelector(Bs.PIN_VALUE_MARKER_SELECTOR),this.trackMarkerContainer_=this.root_.querySelector(Bs.TRACK_MARKER_CONTAINER_SELECTOR)},rc.prototype.getDefaultFoundation=function(){var o=this;return new tc({hasClass:function(t){return o.root_.classList.contains(t)},addClass:function(t){return o.root_.classList.add(t)},removeClass:function(t){return o.root_.classList.remove(t)},getAttribute:function(t){return o.root_.getAttribute(t)},setAttribute:function(t,e){return o.root_.setAttribute(t,e)},removeAttribute:function(t){return o.root_.removeAttribute(t)},computeBoundingRect:function(){return o.root_.getBoundingClientRect()},getTabIndex:function(){return o.root_.tabIndex},registerInteractionHandler:function(t,e){return o.listen(t,e)},deregisterInteractionHandler:function(t,e){return o.unlisten(t,e)},registerThumbContainerInteractionHandler:function(t,e){o.thumbContainer_.addEventListener(t,e)},deregisterThumbContainerInteractionHandler:function(t,e){o.thumbContainer_.removeEventListener(t,e)},registerBodyInteractionHandler:function(t,e){return document.body.addEventListener(t,e)},deregisterBodyInteractionHandler:function(t,e){return document.body.removeEventListener(t,e)},registerResizeHandler:function(t){return window.addEventListener("resize",t)},deregisterResizeHandler:function(t){return window.removeEventListener("resize",t)},notifyInput:function(){return o.emit(Bs.INPUT_EVENT,o)},notifyChange:function(){return o.emit(Bs.CHANGE_EVENT,o)},setThumbContainerStyleProperty:function(t,e){o.thumbContainer_.style.setProperty(t,e)},setTrackStyleProperty:function(t,e){return o.track_.style.setProperty(t,e)},setMarkerValue:function(t){return o.pinValueMarker_.innerText=t.toLocaleString()},setTrackMarkers:function(t,e,n){var i=t.toLocaleString(),r="linear-gradient(to right, currentColor 2px, transparent 0) 0 center / calc((100% - 2px) / (("+e.toLocaleString()+" - "+n.toLocaleString()+") / "+i+")) 100% repeat-x";o.trackMarkerContainer_.style.setProperty("background",r)},isRTL:function(){return"rtl"===getComputedStyle(o.root_).direction}})},rc.prototype.initialSyncWithDOM=function(){var t=this.parseFloat_(this.root_.getAttribute(Bs.ARIA_VALUENOW),this.value),e=this.parseFloat_(this.root_.getAttribute(Bs.ARIA_VALUEMIN),this.min),n=this.parseFloat_(this.root_.getAttribute(Bs.ARIA_VALUEMAX),this.max);e>=this.max?(this.max=n,this.min=e):(this.min=e,this.max=n),this.step=this.parseFloat_(this.root_.getAttribute(Bs.STEP_DATA_ATTR),this.step),this.value=t,this.disabled=this.root_.hasAttribute(Bs.ARIA_DISABLED)&&"false"!==this.root_.getAttribute(Bs.ARIA_DISABLED),this.foundation_.setupTrackMarker()},rc.prototype.layout=function(){this.foundation_.layout()},rc.prototype.stepUp=function(t){void 0===t&&(t=this.step||1),this.value+=t},rc.prototype.stepDown=function(t){void 0===t&&(t=this.step||1),this.value-=t},rc.prototype.parseFloat_=function(t,e){var n=parseFloat(t);return"number"==typeof n&&isFinite(n)?n:e},rc);
   function rc(){return null!==nc&&nc.apply(this,arguments)||this}function oc(t){return(oc="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function ac(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function sc(t,e,n){return e&&ac(t.prototype,e),n&&ac(t,n),t}function cc(o){var a=fc();return function(){var t,e,n,i=pc(o);if(a){var r=pc(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==oc(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function uc(t){var i="function"==typeof Map?new Map:void 0;return(uc=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return lc(t,arguments,pc(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),dc(n,t)})(t)}function lc(t,e,n){return(lc=fc()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&dc(r,n.prototype),r}).apply(null,arguments)}function fc(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function dc(t,e){return(dc=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function pc(t){return(pc=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var hc=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&dc(t,e)}(n,uc(HTMLElement));var e=cc(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).value_=0,t.min_=0,t.max_=100,t.step_=0,t.disabled_=!1,t.slider_,t}return sc(n,[{key:"value",get:function(){return this.slider_?this.slider_.value:this.value_},set:function(t){this.value_=t,this.slider_&&(this.slider_.value=t)}},{key:"min",get:function(){return this.min_},set:function(t){this.min_=t,this.slider_&&(this.slider_.min=t)}},{key:"max",get:function(){return this.max_},set:function(t){this.max_=t,this.slider_&&(this.slider_.max=t)}},{key:"step",get:function(){return this.step_},set:function(t){this.step_=t,this.slider_&&(this.slider_.step=t)}},{key:"disabled",get:function(){return this.disabled_},set:function(t){this.disabled_=t,this.slider_&&(this.slider_.disabled=t)}}]),sc(n,[{key:"connectedCallback",value:function(){k.call(this),this.slider_=new ic(this),this.slider_.value=this.value_,this.slider_.min=this.min_,this.slider_.max=this.max_,this.slider_.step=this.step_,this.slider_.disabled=this.disabled_}},{key:"disconnectedCallback",value:function(){this.slider_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-slider",hc);
var _c,yc={CLOSING:"mdc-snackbar--closing",OPEN:"mdc-snackbar--open",OPENING:"mdc-snackbar--opening"},mc={ACTION_SELECTOR:".mdc-snackbar__action",ARIA_LIVE_LABEL_TEXT_ATTR:"data-mdc-snackbar-label-text",CLOSED_EVENT:"MDCSnackbar:closed",CLOSING_EVENT:"MDCSnackbar:closing",DISMISS_SELECTOR:".mdc-snackbar__dismiss",LABEL_SELECTOR:".mdc-snackbar__label",OPENED_EVENT:"MDCSnackbar:opened",OPENING_EVENT:"MDCSnackbar:opening",REASON_ACTION:"action",REASON_DISMISS:"dismiss",SURFACE_SELECTOR:".mdc-snackbar__surface"},vc={DEFAULT_AUTO_DISMISS_TIMEOUT_MS:5e3,INDETERMINATE:-1,MAX_AUTO_DISMISS_TIMEOUT_MS:1e4,MIN_AUTO_DISMISS_TIMEOUT_MS:4e3,SNACKBAR_ANIMATION_CLOSE_TIME_MS:75,SNACKBAR_ANIMATION_OPEN_TIME_MS:150,ARIA_LIVE_DELAY_MS:1e3},bc=yc.OPENING,Ec=yc.OPEN,gc=yc.CLOSING,Cc=mc.REASON_ACTION,Ac=mc.REASON_DISMISS,Tc=(r(Ic,_c=s),Object.defineProperty(Ic,"cssClasses",{get:function(){return yc},enumerable:!1,configurable:!0}),Object.defineProperty(Ic,"strings",{get:function(){return mc},enumerable:!1,configurable:!0}),Object.defineProperty(Ic,"numbers",{get:function(){return vc},enumerable:!1,configurable:!0}),Object.defineProperty(Ic,"defaultAdapter",{get:function(){return{addClass:function(){},announce:function(){},notifyClosed:function(){},notifyClosing:function(){},notifyOpened:function(){},notifyOpening:function(){},removeClass:function(){}}},enumerable:!1,configurable:!0}),Ic.prototype.destroy=function(){this.clearAutoDismissTimer_(),cancelAnimationFrame(this.animationFrame_),this.animationFrame_=0,clearTimeout(this.animationTimer_),this.animationTimer_=0,this.adapter_.removeClass(bc),this.adapter_.removeClass(Ec),this.adapter_.removeClass(gc)},Ic.prototype.open=function(){var e=this;this.clearAutoDismissTimer_(),this.isOpen_=!0,this.adapter_.notifyOpening(),this.adapter_.removeClass(gc),this.adapter_.addClass(bc),this.adapter_.announce(),this.runNextAnimationFrame_(function(){e.adapter_.addClass(Ec),e.animationTimer_=setTimeout(function(){var t=e.getTimeoutMs();e.handleAnimationTimerEnd_(),e.adapter_.notifyOpened(),t!==vc.INDETERMINATE&&(e.autoDismissTimer_=setTimeout(function(){e.close(Ac)},t))},vc.SNACKBAR_ANIMATION_OPEN_TIME_MS)})},Ic.prototype.close=function(t){var e=this;void 0===t&&(t=""),this.isOpen_&&(cancelAnimationFrame(this.animationFrame_),this.animationFrame_=0,this.clearAutoDismissTimer_(),this.isOpen_=!1,this.adapter_.notifyClosing(t),this.adapter_.addClass(yc.CLOSING),this.adapter_.removeClass(yc.OPEN),this.adapter_.removeClass(yc.OPENING),clearTimeout(this.animationTimer_),this.animationTimer_=setTimeout(function(){e.handleAnimationTimerEnd_(),e.adapter_.notifyClosed(t)},vc.SNACKBAR_ANIMATION_CLOSE_TIME_MS))},Ic.prototype.isOpen=function(){return this.isOpen_},Ic.prototype.getTimeoutMs=function(){return this.autoDismissTimeoutMs_},Ic.prototype.setTimeoutMs=function(t){var e=vc.MIN_AUTO_DISMISS_TIMEOUT_MS,n=vc.MAX_AUTO_DISMISS_TIMEOUT_MS;if(!(t===vc.INDETERMINATE||t<=n&&e<=t))throw new Error("\n        timeoutMs must be an integer in the range "+e+"–"+n+"\n        (or "+vc.INDETERMINATE+" to disable), but got '"+t+"'");this.autoDismissTimeoutMs_=t},Ic.prototype.getCloseOnEscape=function(){return this.closeOnEscape_},Ic.prototype.setCloseOnEscape=function(t){this.closeOnEscape_=t},Ic.prototype.handleKeyDown=function(t){"Escape"!==t.key&&27!==t.keyCode||!this.getCloseOnEscape()||this.close(Ac)},Ic.prototype.handleActionButtonClick=function(t){this.close(Cc)},Ic.prototype.handleActionIconClick=function(t){this.close(Ac)},Ic.prototype.clearAutoDismissTimer_=function(){clearTimeout(this.autoDismissTimer_),this.autoDismissTimer_=0},Ic.prototype.handleAnimationTimerEnd_=function(){this.animationTimer_=0,this.adapter_.removeClass(yc.OPENING),this.adapter_.removeClass(yc.CLOSING)},Ic.prototype.runNextAnimationFrame_=function(t){var e=this;cancelAnimationFrame(this.animationFrame_),this.animationFrame_=requestAnimationFrame(function(){e.animationFrame_=0,clearTimeout(e.animationTimer_),e.animationTimer_=setTimeout(t,0)})},Ic);function Ic(t){var e=_c.call(this,a(a({},Ic.defaultAdapter),t))||this;return e.isOpen_=!1,e.animationFrame_=0,e.animationTimer_=0,e.autoDismissTimer_=0,e.autoDismissTimeoutMs_=vc.DEFAULT_AUTO_DISMISS_TIMEOUT_MS,e.closeOnEscape_=!0,e}var Sc,Oc=mc.SURFACE_SELECTOR,Rc=mc.LABEL_SELECTOR,Lc=mc.ACTION_SELECTOR,wc=mc.DISMISS_SELECTOR,xc=mc.OPENING_EVENT,Nc=mc.OPENED_EVENT,Dc=mc.CLOSING_EVENT,Pc=mc.CLOSED_EVENT,kc=(r(Fc,Sc=u),Fc.attachTo=function(t){return new Fc(t)},Fc.prototype.initialize=function(){},Fc.prototype.initialSyncWithDOM=function(){var n=this;this.surfaceEl_=this.root_.querySelector(Oc),this.labelEl_=this.root_.querySelector(Rc),this.actionEl_=this.root_.querySelector(Lc),this.handleKeyDown_=function(t){return n.foundation_.handleKeyDown(t)},this.handleSurfaceClick_=function(t){var e=t.target;n.isActionButton_(e)?n.foundation_.handleActionButtonClick(t):n.isActionIcon_(e)&&n.foundation_.handleActionIconClick(t)},this.registerKeyDownHandler_(this.handleKeyDown_),this.registerSurfaceClickHandler_(this.handleSurfaceClick_)},Fc.prototype.destroy=function(){Sc.prototype.destroy.call(this),this.deregisterKeyDownHandler_(this.handleKeyDown_),this.deregisterSurfaceClickHandler_(this.handleSurfaceClick_)},Fc.prototype.open=function(){this.foundation_.open()},Fc.prototype.close=function(t){void 0===t&&(t=""),this.foundation_.close(t)},Fc.prototype.getDefaultFoundation=function(){var e=this;return new Tc({addClass:function(t){return e.root_.classList.add(t)},announce:function(){},notifyClosed:function(t){return e.emit(Pc,t?{reason:t}:{})},notifyClosing:function(t){return e.emit(Dc,t?{reason:t}:{})},notifyOpened:function(){return e.emit(Nc,{})},notifyOpening:function(){return e.emit(xc,{})},removeClass:function(t){return e.root_.classList.remove(t)}})},Object.defineProperty(Fc.prototype,"timeoutMs",{get:function(){return this.foundation_.getTimeoutMs()},set:function(t){this.foundation_.setTimeoutMs(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Fc.prototype,"closeOnEscape",{get:function(){return this.foundation_.getCloseOnEscape()},set:function(t){this.foundation_.setCloseOnEscape(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Fc.prototype,"isOpen",{get:function(){return this.foundation_.isOpen()},enumerable:!1,configurable:!0}),Object.defineProperty(Fc.prototype,"labelText",{get:function(){return this.labelEl_.textContent},set:function(t){this.labelEl_.textContent=t},enumerable:!1,configurable:!0}),Object.defineProperty(Fc.prototype,"actionButtonText",{get:function(){return this.actionEl_.textContent},set:function(t){this.actionEl_.textContent=t},enumerable:!1,configurable:!0}),Fc.prototype.registerKeyDownHandler_=function(t){this.listen("keydown",t)},Fc.prototype.deregisterKeyDownHandler_=function(t){this.unlisten("keydown",t)},Fc.prototype.registerSurfaceClickHandler_=function(t){this.surfaceEl_.addEventListener("click",t)},Fc.prototype.deregisterSurfaceClickHandler_=function(t){this.surfaceEl_.removeEventListener("click",t)},Fc.prototype.isActionButton_=function(t){return Boolean(p(t,Lc))},Fc.prototype.isActionIcon_=function(t){return Boolean(p(t,wc))},Fc);
   function Fc(){return null!==Sc&&Sc.apply(this,arguments)||this}function Mc(t){return(Mc="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Hc(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function jc(t,e,n){return e&&Hc(t.prototype,e),n&&Hc(t,n),t}function Bc(o){var a=Kc();return function(){var t,e,n,i=Wc(o);if(a){var r=Wc(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Mc(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Vc(t){var i="function"==typeof Map?new Map:void 0;return(Vc=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Uc(t,arguments,Wc(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),Gc(n,t)})(t)}function Uc(t,e,n){return(Uc=Kc()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&Gc(r,n.prototype),r}).apply(null,arguments)}function Kc(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function Gc(t,e){return(Gc=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Wc(t){return(Wc=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var qc=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&Gc(t,e)}(n,Vc(HTMLElement));var e=Bc(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).closeOnEscape_=!1,t.timeoutMs_=5e3,t.messageId_=-1,t.snackbar_,t}return jc(n,[{key:"closeOnEscape",get:function(){return this.closeOnEscape_},set:function(t){this.closeOnEscape_=t,this.snackbar_&&(this.snackbar_.closeOnEscape=t)}},{key:"timeoutMs",get:function(){return this.timeoutMs_},set:function(t){this.timeoutMs_=t,this.snackbar_&&(this.snackbar_.timeoutMs=t)}},{key:"messageId",get:function(){return this.messageId_},set:function(t){this.messageId_=t,this.snackbar_&&-1!==t&&this.snackbar_.open()}}]),jc(n,[{key:"connectedCallback",value:function(){k.call(this),this.snackbar_=new kc(this),this.snackbar_.closeOnEscape=this.closeOnEscape_,this.snackbar_.timeoutMs=this.timeoutMs_,-1!==this.messageId_&&this.snackbar_.open()}},{key:"disconnectedCallback",value:function(){this.snackbar_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-snackbar",qc);
var zc,Xc={CHECKED:"mdc-switch--checked",DISABLED:"mdc-switch--disabled"},Yc={ARIA_CHECKED_ATTR:"aria-checked",NATIVE_CONTROL_SELECTOR:".mdc-switch__native-control",RIPPLE_SURFACE_SELECTOR:".mdc-switch__thumb-underlay"},Qc=(r(Zc,zc=s),Object.defineProperty(Zc,"strings",{get:function(){return Yc},enumerable:!1,configurable:!0}),Object.defineProperty(Zc,"cssClasses",{get:function(){return Xc},enumerable:!1,configurable:!0}),Object.defineProperty(Zc,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},setNativeControlChecked:function(){},setNativeControlDisabled:function(){},setNativeControlAttr:function(){}}},enumerable:!1,configurable:!0}),Zc.prototype.setChecked=function(t){this.adapter_.setNativeControlChecked(t),this.updateAriaChecked_(t),this.updateCheckedStyling_(t)},Zc.prototype.setDisabled=function(t){this.adapter_.setNativeControlDisabled(t),t?this.adapter_.addClass(Xc.DISABLED):this.adapter_.removeClass(Xc.DISABLED)},Zc.prototype.handleChange=function(t){var e=t.target;this.updateAriaChecked_(e.checked),this.updateCheckedStyling_(e.checked)},Zc.prototype.updateCheckedStyling_=function(t){t?this.adapter_.addClass(Xc.CHECKED):this.adapter_.removeClass(Xc.CHECKED)},Zc.prototype.updateAriaChecked_=function(t){this.adapter_.setNativeControlAttr(Yc.ARIA_CHECKED_ATTR,""+!!t)},Zc);function Zc(t){return zc.call(this,a(a({},Zc.defaultAdapter),t))||this}var $c,Jc=(r(tu,$c=u),tu.attachTo=function(t){return new tu(t)},tu.prototype.destroy=function(){$c.prototype.destroy.call(this),this.ripple_.destroy()},tu.prototype.getDefaultFoundation=function(){var n=this;return new Qc({addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},setNativeControlChecked:function(t){return n.nativeControl_.checked=t},setNativeControlDisabled:function(t){return n.nativeControl_.disabled=t},setNativeControlAttr:function(t,e){return n.nativeControl_.setAttribute(t,e)}})},Object.defineProperty(tu.prototype,"ripple",{get:function(){return this.ripple_},enumerable:!1,configurable:!0}),Object.defineProperty(tu.prototype,"checked",{get:function(){return this.nativeControl_.checked},set:function(t){this.foundation_.setChecked(t),this.foundation_.handleChange({target:this.nativeControl_})},enumerable:!1,configurable:!0}),Object.defineProperty(tu.prototype,"disabled",{get:function(){return this.nativeControl_.disabled},set:function(t){this.foundation_.setDisabled(t)},enumerable:!1,configurable:!0}),tu.prototype.createRipple_=function(){var n=this,t=Qc.strings.RIPPLE_SURFACE_SELECTOR,i=this.root_.querySelector(t),e=a(a({},_t.createAdapter(this)),{addClass:function(t){return i.classList.add(t)},computeBoundingRect:function(){return i.getBoundingClientRect()},deregisterInteractionHandler:function(t,e){n.nativeControl_.removeEventListener(t,e,d())},isSurfaceActive:function(){return h(n.nativeControl_,":active")},isUnbounded:function(){return!0},registerInteractionHandler:function(t,e){n.nativeControl_.addEventListener(t,e,d())},removeClass:function(t){i.classList.remove(t)},updateCssVariable:function(t,e){i.style.setProperty(t,e)}});return new _t(this.root_,new dt(e))},Object.defineProperty(tu.prototype,"nativeControl_",{get:function(){var t=Qc.strings.NATIVE_CONTROL_SELECTOR;return this.root_.querySelector(t)},enumerable:!1,configurable:!0}),tu);
   function tu(){var t=null!==$c&&$c.apply(this,arguments)||this;return t.ripple_=t.createRipple_(),t}function eu(t){return(eu="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function nu(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function iu(t,e,n){return e&&nu(t.prototype,e),n&&nu(t,n),t}function ru(o){var a=su();return function(){var t,e,n,i=uu(o);if(a){var r=uu(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==eu(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function ou(t){var i="function"==typeof Map?new Map:void 0;return(ou=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return au(t,arguments,uu(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),cu(n,t)})(t)}function au(t,e,n){return(au=su()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&cu(r,n.prototype),r}).apply(null,arguments)}function su(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function cu(t,e){return(cu=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function uu(t){return(uu=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var lu=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&cu(t,e)}(n,ou(HTMLElement));var e=ru(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).checked_=!1,t.disabled_=!1,t.switch_,t}return iu(n,[{key:"focus",value:function(){this.switch_.nativeControl_.focus()}},{key:"blur",value:function(){this.switch_.nativeControl_.blur()}},{key:"checked",get:function(){return this.checked_},set:function(t){this.checked_=t,this.switch_&&(this.switch_.checked=t)}},{key:"disabled",get:function(){return this.disabled_},set:function(t){this.disabled_=t,this.switch_&&(this.switch_.disabled=t)}}]),iu(n,[{key:"connectedCallback",value:function(){k.call(this),this.switch_=new Jc(this),this.switch_.checked=this.checked_,this.switch_.disabled=this.disabled_}},{key:"disconnectedCallback",value:function(){this.switch_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-switch",lu);function fu(t){this.adapter_=t}
var du,pu={ANIMATING:"mdc-tab-scroller--animating",SCROLL_AREA_SCROLL:"mdc-tab-scroller__scroll-area--scroll",SCROLL_TEST:"mdc-tab-scroller__test"},hu={AREA_SELECTOR:".mdc-tab-scroller__scroll-area",CONTENT_SELECTOR:".mdc-tab-scroller__scroll-content"},_u=(r(yu,du=fu),yu.prototype.getScrollPositionRTL=function(){var t=this.adapter_.getScrollAreaScrollLeft(),e=this.calculateScrollEdges_().right;return Math.round(e-t)},yu.prototype.scrollToRTL=function(t){var e=this.calculateScrollEdges_(),n=this.adapter_.getScrollAreaScrollLeft(),i=this.clampScrollValue_(e.right-t);return{finalScrollPosition:i,scrollDelta:i-n}},yu.prototype.incrementScrollRTL=function(t){var e=this.adapter_.getScrollAreaScrollLeft(),n=this.clampScrollValue_(e-t);return{finalScrollPosition:n,scrollDelta:n-e}},yu.prototype.getAnimatingScrollPosition=function(t){return t},yu.prototype.calculateScrollEdges_=function(){return{left:0,right:this.adapter_.getScrollContentOffsetWidth()-this.adapter_.getScrollAreaOffsetWidth()}},yu.prototype.clampScrollValue_=function(t){var e=this.calculateScrollEdges_();return Math.min(Math.max(e.left,t),e.right)},yu);function yu(){return null!==du&&du.apply(this,arguments)||this}var mu,vu=(r(bu,mu=fu),bu.prototype.getScrollPositionRTL=function(t){var e=this.adapter_.getScrollAreaScrollLeft();return Math.round(t-e)},bu.prototype.scrollToRTL=function(t){var e=this.adapter_.getScrollAreaScrollLeft(),n=this.clampScrollValue_(-t);return{finalScrollPosition:n,scrollDelta:n-e}},bu.prototype.incrementScrollRTL=function(t){var e=this.adapter_.getScrollAreaScrollLeft(),n=this.clampScrollValue_(e-t);return{finalScrollPosition:n,scrollDelta:n-e}},bu.prototype.getAnimatingScrollPosition=function(t,e){return t-e},bu.prototype.calculateScrollEdges_=function(){var t=this.adapter_.getScrollContentOffsetWidth();return{left:this.adapter_.getScrollAreaOffsetWidth()-t,right:0}},bu.prototype.clampScrollValue_=function(t){var e=this.calculateScrollEdges_();return Math.max(Math.min(e.right,t),e.left)},bu);
   function bu(){return null!==mu&&mu.apply(this,arguments)||this}var Eu,gu=(r(Cu,Eu=fu),Cu.prototype.getScrollPositionRTL=function(t){var e=this.adapter_.getScrollAreaScrollLeft();return Math.round(e-t)},Cu.prototype.scrollToRTL=function(t){var e=this.adapter_.getScrollAreaScrollLeft(),n=this.clampScrollValue_(t);return{finalScrollPosition:n,scrollDelta:e-n}},Cu.prototype.incrementScrollRTL=function(t){var e=this.adapter_.getScrollAreaScrollLeft(),n=this.clampScrollValue_(e+t);return{finalScrollPosition:n,scrollDelta:e-n}},Cu.prototype.getAnimatingScrollPosition=function(t,e){return t+e},Cu.prototype.calculateScrollEdges_=function(){return{left:this.adapter_.getScrollContentOffsetWidth()-this.adapter_.getScrollAreaOffsetWidth(),right:0}},Cu.prototype.clampScrollValue_=function(t){var e=this.calculateScrollEdges_();return Math.min(Math.max(e.right,t),e.left)},Cu);
   function Cu(){return null!==Eu&&Eu.apply(this,arguments)||this}var Au,Tu=(r(Iu,Au=s),Object.defineProperty(Iu,"cssClasses",{get:function(){return pu},enumerable:!0,configurable:!0}),Object.defineProperty(Iu,"strings",{get:function(){return hu},enumerable:!0,configurable:!0}),Object.defineProperty(Iu,"defaultAdapter",{get:function(){return{eventTargetMatchesSelector:function(){return!1},addClass:function(){},removeClass:function(){},addScrollAreaClass:function(){},setScrollAreaStyleProperty:function(){},setScrollContentStyleProperty:function(){},getScrollContentStyleValue:function(){return""},setScrollAreaScrollLeft:function(){},getScrollAreaScrollLeft:function(){return 0},getScrollContentOffsetWidth:function(){return 0},getScrollAreaOffsetWidth:function(){return 0},computeScrollAreaClientRect:function(){return{top:0,right:0,bottom:0,left:0,width:0,height:0}},computeScrollContentClientRect:function(){return{top:0,right:0,bottom:0,left:0,width:0,height:0}},computeHorizontalScrollbarHeight:function(){return 0}}},enumerable:!0,configurable:!0}),Iu.prototype.init=function(){var t=this.adapter_.computeHorizontalScrollbarHeight();this.adapter_.setScrollAreaStyleProperty("margin-bottom",-t+"px"),this.adapter_.addScrollAreaClass(Iu.cssClasses.SCROLL_AREA_SCROLL)},Iu.prototype.getScrollPosition=function(){if(this.isRTL_())return this.computeCurrentScrollPositionRTL_();var t=this.calculateCurrentTranslateX_();return this.adapter_.getScrollAreaScrollLeft()-t},Iu.prototype.handleInteraction=function(){this.isAnimating_&&this.stopScrollAnimation_()},Iu.prototype.handleTransitionEnd=function(t){var e=t.target;this.isAnimating_&&this.adapter_.eventTargetMatchesSelector(e,Iu.strings.CONTENT_SELECTOR)&&(this.isAnimating_=!1,this.adapter_.removeClass(Iu.cssClasses.ANIMATING))},Iu.prototype.incrementScroll=function(t){0!==t&&this.animate_(this.getIncrementScrollOperation_(t))},Iu.prototype.incrementScrollImmediate=function(t){if(0!==t){var e=this.getIncrementScrollOperation_(t);0!==e.scrollDelta&&(this.stopScrollAnimation_(),this.adapter_.setScrollAreaScrollLeft(e.finalScrollPosition))}},Iu.prototype.scrollTo=function(t){if(this.isRTL_())return this.scrollToRTL_(t);this.scrollTo_(t)},Iu.prototype.getRTLScroller=function(){return this.rtlScrollerInstance_||(this.rtlScrollerInstance_=this.rtlScrollerFactory_()),this.rtlScrollerInstance_},Iu.prototype.calculateCurrentTranslateX_=function(){var t=this.adapter_.getScrollContentStyleValue("transform");if("none"===t)return 0;var e=/\((.+?)\)/.exec(t);if(!e)return 0;var n=o(e[1].split(","),6),i=(n[0],n[1],n[2],n[3],n[4]);return n[5],parseFloat(i)},Iu.prototype.clampScrollValue_=function(t){var e=this.calculateScrollEdges_();return Math.min(Math.max(e.left,t),e.right)},Iu.prototype.computeCurrentScrollPositionRTL_=function(){var t=this.calculateCurrentTranslateX_();return this.getRTLScroller().getScrollPositionRTL(t)},Iu.prototype.calculateScrollEdges_=function(){return{left:0,right:this.adapter_.getScrollContentOffsetWidth()-this.adapter_.getScrollAreaOffsetWidth()}},Iu.prototype.scrollTo_=function(t){var e=this.getScrollPosition(),n=this.clampScrollValue_(t),i=n-e;this.animate_({finalScrollPosition:n,scrollDelta:i})},Iu.prototype.scrollToRTL_=function(t){var e=this.getRTLScroller().scrollToRTL(t);this.animate_(e)},Iu.prototype.getIncrementScrollOperation_=function(t){if(this.isRTL_())return this.getRTLScroller().incrementScrollRTL(t);var e=this.getScrollPosition(),n=t+e,i=this.clampScrollValue_(n);return{finalScrollPosition:i,scrollDelta:i-e}},Iu.prototype.animate_=function(t){var e=this;0!==t.scrollDelta&&(this.stopScrollAnimation_(),this.adapter_.setScrollAreaScrollLeft(t.finalScrollPosition),this.adapter_.setScrollContentStyleProperty("transform","translateX("+t.scrollDelta+"px)"),this.adapter_.computeScrollAreaClientRect(),requestAnimationFrame(function(){e.adapter_.addClass(Iu.cssClasses.ANIMATING),e.adapter_.setScrollContentStyleProperty("transform","none")}),this.isAnimating_=!0)},Iu.prototype.stopScrollAnimation_=function(){this.isAnimating_=!1;var t=this.getAnimatingScrollPosition_();this.adapter_.removeClass(Iu.cssClasses.ANIMATING),this.adapter_.setScrollContentStyleProperty("transform","translateX(0px)"),this.adapter_.setScrollAreaScrollLeft(t)},Iu.prototype.getAnimatingScrollPosition_=function(){var t=this.calculateCurrentTranslateX_(),e=this.adapter_.getScrollAreaScrollLeft();return this.isRTL_()?this.getRTLScroller().getAnimatingScrollPosition(e,t):e-t},Iu.prototype.rtlScrollerFactory_=function(){var t=this.adapter_.getScrollAreaScrollLeft();this.adapter_.setScrollAreaScrollLeft(t-1);var e=this.adapter_.getScrollAreaScrollLeft();if(e<0)return this.adapter_.setScrollAreaScrollLeft(t),new vu(this.adapter_);var n=this.adapter_.computeScrollAreaClientRect(),i=this.adapter_.computeScrollContentClientRect(),r=Math.round(i.right-n.right);return this.adapter_.setScrollAreaScrollLeft(t),r===e?new gu(this.adapter_):new _u(this.adapter_)},Iu.prototype.isRTL_=function(){return"rtl"===this.adapter_.getScrollContentStyleValue("direction")},Iu);
   function Iu(t){var e=Au.call(this,a(a({},Iu.defaultAdapter),t))||this;return e.isAnimating_=!1,e}var Su;
var Ou,Ru=(r(Lu,Ou=u),Lu.attachTo=function(t){return new Lu(t)},Lu.prototype.initialize=function(){this.area_=this.root_.querySelector(Tu.strings.AREA_SELECTOR),this.content_=this.root_.querySelector(Tu.strings.CONTENT_SELECTOR)},Lu.prototype.initialSyncWithDOM=function(){var e=this;this.handleInteraction_=function(){return e.foundation_.handleInteraction()},this.handleTransitionEnd_=function(t){return e.foundation_.handleTransitionEnd(t)},this.area_.addEventListener("wheel",this.handleInteraction_,d()),this.area_.addEventListener("touchstart",this.handleInteraction_,d()),this.area_.addEventListener("pointerdown",this.handleInteraction_,d()),this.area_.addEventListener("mousedown",this.handleInteraction_,d()),this.area_.addEventListener("keydown",this.handleInteraction_,d()),this.content_.addEventListener("transitionend",this.handleTransitionEnd_)},Lu.prototype.destroy=function(){Ou.prototype.destroy.call(this),this.area_.removeEventListener("wheel",this.handleInteraction_,d()),this.area_.removeEventListener("touchstart",this.handleInteraction_,d()),this.area_.removeEventListener("pointerdown",this.handleInteraction_,d()),this.area_.removeEventListener("mousedown",this.handleInteraction_,d()),this.area_.removeEventListener("keydown",this.handleInteraction_,d()),this.content_.removeEventListener("transitionend",this.handleTransitionEnd_)},Lu.prototype.getDefaultFoundation=function(){var n=this;return new Tu({eventTargetMatchesSelector:function(t,e){return h(t,e)},addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},addScrollAreaClass:function(t){return n.area_.classList.add(t)},setScrollAreaStyleProperty:function(t,e){return n.area_.style.setProperty(t,e)},setScrollContentStyleProperty:function(t,e){return n.content_.style.setProperty(t,e)},getScrollContentStyleValue:function(t){return window.getComputedStyle(n.content_).getPropertyValue(t)},setScrollAreaScrollLeft:function(t){return n.area_.scrollLeft=t},getScrollAreaScrollLeft:function(){return n.area_.scrollLeft},getScrollContentOffsetWidth:function(){return n.content_.offsetWidth},getScrollAreaOffsetWidth:function(){return n.area_.offsetWidth},computeScrollAreaClientRect:function(){return n.area_.getBoundingClientRect()},computeScrollContentClientRect:function(){return n.content_.getBoundingClientRect()},computeHorizontalScrollbarHeight:function(){return function(t,e){if(void 0===e&&(e=!0),e&&void 0!==Su)return Su;var n=t.createElement("div");n.classList.add(pu.SCROLL_TEST),t.body.appendChild(n);var i=n.offsetHeight-n.clientHeight;return t.body.removeChild(n),e&&(Su=i),i}(document)}})},Lu.prototype.getScrollPosition=function(){return this.foundation_.getScrollPosition()},Lu.prototype.getScrollContentWidth=function(){return this.content_.offsetWidth},Lu.prototype.incrementScroll=function(t){this.foundation_.incrementScroll(t)},Lu.prototype.scrollTo=function(t){this.foundation_.scrollTo(t)},Lu);
   function Lu(){return null!==Ou&&Ou.apply(this,arguments)||this}var wu,xu={ACTIVE:"mdc-tab-indicator--active",FADE:"mdc-tab-indicator--fade",NO_TRANSITION:"mdc-tab-indicator--no-transition"},Nu={CONTENT_SELECTOR:".mdc-tab-indicator__content"},Du=(r(Pu,wu=s),Object.defineProperty(Pu,"cssClasses",{get:function(){return xu},enumerable:!0,configurable:!0}),Object.defineProperty(Pu,"strings",{get:function(){return Nu},enumerable:!0,configurable:!0}),Object.defineProperty(Pu,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},computeContentClientRect:function(){return{top:0,right:0,bottom:0,left:0,width:0,height:0}},setContentStyleProperty:function(){}}},enumerable:!0,configurable:!0}),Pu.prototype.computeContentClientRect=function(){return this.adapter_.computeContentClientRect()},Pu);function Pu(t){return wu.call(this,a(a({},Pu.defaultAdapter),t))||this}var ku,Fu=(r(Mu,ku=Du),Mu.prototype.activate=function(){this.adapter_.addClass(Du.cssClasses.ACTIVE)},Mu.prototype.deactivate=function(){this.adapter_.removeClass(Du.cssClasses.ACTIVE)},Mu);
   function Mu(){return null!==ku&&ku.apply(this,arguments)||this}var Hu,ju=(r(Bu,Hu=Du),Bu.prototype.activate=function(t){if(t){var e=this.computeContentClientRect(),n=t.width/e.width,i=t.left-e.left;this.adapter_.addClass(Du.cssClasses.NO_TRANSITION),this.adapter_.setContentStyleProperty("transform","translateX("+i+"px) scaleX("+n+")"),this.computeContentClientRect(),this.adapter_.removeClass(Du.cssClasses.NO_TRANSITION),this.adapter_.addClass(Du.cssClasses.ACTIVE),this.adapter_.setContentStyleProperty("transform","")}else this.adapter_.addClass(Du.cssClasses.ACTIVE)},Bu.prototype.deactivate=function(){this.adapter_.removeClass(Du.cssClasses.ACTIVE)},Bu);
   function Bu(){return null!==Hu&&Hu.apply(this,arguments)||this}var Vu,Uu=(r(Ku,Vu=u),Ku.attachTo=function(t){return new Ku(t)},Ku.prototype.initialize=function(){this.content_=this.root_.querySelector(Du.strings.CONTENT_SELECTOR)},Ku.prototype.computeContentClientRect=function(){return this.foundation_.computeContentClientRect()},Ku.prototype.getDefaultFoundation=function(){var n=this,t={addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},computeContentClientRect:function(){return n.content_.getBoundingClientRect()},setContentStyleProperty:function(t,e){return n.content_.style.setProperty(t,e)}};return this.root_.classList.contains(Du.cssClasses.FADE)?new Fu(t):new ju(t)},Ku.prototype.activate=function(t){this.foundation_.activate(t)},Ku.prototype.deactivate=function(){this.foundation_.deactivate()},Ku);
   function Ku(){return null!==Vu&&Vu.apply(this,arguments)||this}
var Gu,Wu={ACTIVE:"mdc-tab--active"},qu={ARIA_SELECTED:"aria-selected",CONTENT_SELECTOR:".mdc-tab__content",INTERACTED_EVENT:"MDCTab:interacted",RIPPLE_SELECTOR:".mdc-tab__ripple",TABINDEX:"tabIndex",TAB_INDICATOR_SELECTOR:".mdc-tab-indicator"},zu=(r(Xu,Gu=s),Object.defineProperty(Xu,"cssClasses",{get:function(){return Wu},enumerable:!0,configurable:!0}),Object.defineProperty(Xu,"strings",{get:function(){return qu},enumerable:!0,configurable:!0}),Object.defineProperty(Xu,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},setAttr:function(){},activateIndicator:function(){},deactivateIndicator:function(){},notifyInteracted:function(){},getOffsetLeft:function(){return 0},getOffsetWidth:function(){return 0},getContentOffsetLeft:function(){return 0},getContentOffsetWidth:function(){return 0},focus:function(){}}},enumerable:!0,configurable:!0}),Xu.prototype.handleClick=function(){this.adapter_.notifyInteracted()},Xu.prototype.isActive=function(){return this.adapter_.hasClass(Wu.ACTIVE)},Xu.prototype.setFocusOnActivate=function(t){this.focusOnActivate_=t},Xu.prototype.activate=function(t){this.adapter_.addClass(Wu.ACTIVE),this.adapter_.setAttr(qu.ARIA_SELECTED,"true"),this.adapter_.setAttr(qu.TABINDEX,"0"),this.adapter_.activateIndicator(t),this.focusOnActivate_&&this.adapter_.focus()},Xu.prototype.deactivate=function(){this.isActive()&&(this.adapter_.removeClass(Wu.ACTIVE),this.adapter_.setAttr(qu.ARIA_SELECTED,"false"),this.adapter_.setAttr(qu.TABINDEX,"-1"),this.adapter_.deactivateIndicator())},Xu.prototype.computeDimensions=function(){var t=this.adapter_.getOffsetWidth(),e=this.adapter_.getOffsetLeft(),n=this.adapter_.getContentOffsetWidth(),i=this.adapter_.getContentOffsetLeft();return{contentLeft:e+i,contentRight:e+i+n,rootLeft:e,rootRight:e+t}},Xu);function Xu(t){var e=Gu.call(this,a(a({},Xu.defaultAdapter),t))||this;return e.focusOnActivate_=!0,e}var Yu,Qu=(r(Zu,Yu=u),Zu.attachTo=function(t){return new Zu(t)},Zu.prototype.initialize=function(t,e){void 0===t&&(t=function(t,e){return new _t(t,e)}),void 0===e&&(e=function(t){return new Uu(t)}),this.id=this.root_.id;var n=this.root_.querySelector(zu.strings.RIPPLE_SELECTOR),i=a(a({},_t.createAdapter(this)),{addClass:function(t){return n.classList.add(t)},removeClass:function(t){return n.classList.remove(t)},updateCssVariable:function(t,e){return n.style.setProperty(t,e)}}),r=new dt(i);this.ripple_=t(this.root_,r);var o=this.root_.querySelector(zu.strings.TAB_INDICATOR_SELECTOR);this.tabIndicator_=e(o),this.content_=this.root_.querySelector(zu.strings.CONTENT_SELECTOR)},Zu.prototype.initialSyncWithDOM=function(){var t=this;this.handleClick_=function(){return t.foundation_.handleClick()},this.listen("click",this.handleClick_)},Zu.prototype.destroy=function(){this.unlisten("click",this.handleClick_),this.ripple_.destroy(),Yu.prototype.destroy.call(this)},Zu.prototype.getDefaultFoundation=function(){var n=this;return new zu({setAttr:function(t,e){return n.root_.setAttribute(t,e)},addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},hasClass:function(t){return n.root_.classList.contains(t)},activateIndicator:function(t){return n.tabIndicator_.activate(t)},deactivateIndicator:function(){return n.tabIndicator_.deactivate()},notifyInteracted:function(){return n.emit(zu.strings.INTERACTED_EVENT,{tabId:n.id},!0)},getOffsetLeft:function(){return n.root_.offsetLeft},getOffsetWidth:function(){return n.root_.offsetWidth},getContentOffsetLeft:function(){return n.content_.offsetLeft},getContentOffsetWidth:function(){return n.content_.offsetWidth},focus:function(){return n.root_.focus()}})},Object.defineProperty(Zu.prototype,"active",{get:function(){return this.foundation_.isActive()},enumerable:!0,configurable:!0}),Object.defineProperty(Zu.prototype,"focusOnActivate",{set:function(t){this.foundation_.setFocusOnActivate(t)},enumerable:!0,configurable:!0}),Zu.prototype.activate=function(t){this.foundation_.activate(t)},Zu.prototype.deactivate=function(){this.foundation_.deactivate()},Zu.prototype.computeIndicatorClientRect=function(){return this.tabIndicator_.computeContentClientRect()},Zu.prototype.computeDimensions=function(){return this.foundation_.computeDimensions()},Zu.prototype.focus=function(){this.root_.focus()},Zu);
   function Zu(){return null!==Yu&&Yu.apply(this,arguments)||this}
var $u={ARROW_LEFT_KEY:"ArrowLeft",ARROW_RIGHT_KEY:"ArrowRight",END_KEY:"End",ENTER_KEY:"Enter",HOME_KEY:"Home",SPACE_KEY:"Space",TAB_ACTIVATED_EVENT:"MDCTabBar:activated",TAB_SCROLLER_SELECTOR:".mdc-tab-scroller",TAB_SELECTOR:".mdc-tab"},Ju={ARROW_LEFT_KEYCODE:37,ARROW_RIGHT_KEYCODE:39,END_KEYCODE:35,ENTER_KEYCODE:13,EXTRA_SCROLL_AMOUNT:20,HOME_KEYCODE:36,SPACE_KEYCODE:32},tl=new Set;tl.add($u.ARROW_LEFT_KEY),tl.add($u.ARROW_RIGHT_KEY),tl.add($u.END_KEY),tl.add($u.HOME_KEY),tl.add($u.ENTER_KEY),tl.add($u.SPACE_KEY);var el=new Map;el.set(Ju.ARROW_LEFT_KEYCODE,$u.ARROW_LEFT_KEY),el.set(Ju.ARROW_RIGHT_KEYCODE,$u.ARROW_RIGHT_KEY),el.set(Ju.END_KEYCODE,$u.END_KEY),el.set(Ju.HOME_KEYCODE,$u.HOME_KEY),el.set(Ju.ENTER_KEYCODE,$u.ENTER_KEY),el.set(Ju.SPACE_KEYCODE,$u.SPACE_KEY);var nl,il=(r(rl,nl=s),Object.defineProperty(rl,"strings",{get:function(){return $u},enumerable:!1,configurable:!0}),Object.defineProperty(rl,"numbers",{get:function(){return Ju},enumerable:!1,configurable:!0}),Object.defineProperty(rl,"defaultAdapter",{get:function(){return{scrollTo:function(){},incrementScroll:function(){},getScrollPosition:function(){return 0},getScrollContentWidth:function(){return 0},getOffsetWidth:function(){return 0},isRTL:function(){return!1},setActiveTab:function(){},activateTabAtIndex:function(){},deactivateTabAtIndex:function(){},focusTabAtIndex:function(){},getTabIndicatorClientRectAtIndex:function(){return{top:0,right:0,bottom:0,left:0,width:0,height:0}},getTabDimensionsAtIndex:function(){return{rootLeft:0,rootRight:0,contentLeft:0,contentRight:0}},getPreviousActiveTabIndex:function(){return-1},getFocusedTabIndex:function(){return-1},getIndexOfTabById:function(){return-1},getTabListLength:function(){return 0},notifyTabActivated:function(){}}},enumerable:!1,configurable:!0}),rl.prototype.setUseAutomaticActivation=function(t){this.useAutomaticActivation_=t},rl.prototype.activateTab=function(t){var e,n=this.adapter_.getPreviousActiveTabIndex();this.indexIsInRange_(t)&&t!==n&&(-1!==n&&(this.adapter_.deactivateTabAtIndex(n),e=this.adapter_.getTabIndicatorClientRectAtIndex(n)),this.adapter_.activateTabAtIndex(t,e),this.scrollIntoView(t),this.adapter_.notifyTabActivated(t))},rl.prototype.handleKeyDown=function(t){var e=this.getKeyFromEvent_(t);if(void 0!==e)if(this.isActivationKey_(e)||t.preventDefault(),this.useAutomaticActivation_){if(this.isActivationKey_(e))return;var n=this.determineTargetFromKey_(this.adapter_.getPreviousActiveTabIndex(),e);this.adapter_.setActiveTab(n),this.scrollIntoView(n)}else{var i=this.adapter_.getFocusedTabIndex();this.isActivationKey_(e)?this.adapter_.setActiveTab(i):(n=this.determineTargetFromKey_(i,e),this.adapter_.focusTabAtIndex(n),this.scrollIntoView(n))}},rl.prototype.handleTabInteraction=function(t){this.adapter_.setActiveTab(this.adapter_.getIndexOfTabById(t.detail.tabId))},rl.prototype.scrollIntoView=function(t){if(this.indexIsInRange_(t))return 0===t?this.adapter_.scrollTo(0):t===this.adapter_.getTabListLength()-1?this.adapter_.scrollTo(this.adapter_.getScrollContentWidth()):this.isRTL_()?this.scrollIntoViewRTL_(t):void this.scrollIntoView_(t)},rl.prototype.determineTargetFromKey_=function(t,e){var n=this.isRTL_(),i=this.adapter_.getTabListLength()-1,r=t;return e===$u.END_KEY?r=i:e===$u.ARROW_LEFT_KEY&&!n||e===$u.ARROW_RIGHT_KEY&&n?r-=1:e===$u.ARROW_RIGHT_KEY&&!n||e===$u.ARROW_LEFT_KEY&&n?r+=1:r=0,r<0?r=i:i<r&&(r=0),r},rl.prototype.calculateScrollIncrement_=function(t,e,n,i){var r=this.adapter_.getTabDimensionsAtIndex(e),o=r.contentLeft-n-i,a=r.contentRight-n-Ju.EXTRA_SCROLL_AMOUNT,s=o+Ju.EXTRA_SCROLL_AMOUNT;return e<t?Math.min(a,0):Math.max(s,0)},rl.prototype.calculateScrollIncrementRTL_=function(t,e,n,i,r){var o=this.adapter_.getTabDimensionsAtIndex(e),a=r-o.contentLeft-n,s=r-o.contentRight-n-i+Ju.EXTRA_SCROLL_AMOUNT,c=a-Ju.EXTRA_SCROLL_AMOUNT;return t<e?Math.max(s,0):Math.min(c,0)},rl.prototype.findAdjacentTabIndexClosestToEdge_=function(t,e,n,i){var r=e.rootLeft-n,o=e.rootRight-n-i,a=r+o;return r<0||a<0?t-1:0<o||0<a?t+1:-1},rl.prototype.findAdjacentTabIndexClosestToEdgeRTL_=function(t,e,n,i,r){var o=r-e.rootLeft-i-n,a=r-e.rootRight-n,s=o+a;return 0<o||0<s?t+1:a<0||s<0?t-1:-1},rl.prototype.getKeyFromEvent_=function(t){return tl.has(t.key)?t.key:el.get(t.keyCode)},rl.prototype.isActivationKey_=function(t){return t===$u.SPACE_KEY||t===$u.ENTER_KEY},rl.prototype.indexIsInRange_=function(t){return 0<=t&&t<this.adapter_.getTabListLength()},rl.prototype.isRTL_=function(){return this.adapter_.isRTL()},rl.prototype.scrollIntoView_=function(t){var e=this.adapter_.getScrollPosition(),n=this.adapter_.getOffsetWidth(),i=this.adapter_.getTabDimensionsAtIndex(t),r=this.findAdjacentTabIndexClosestToEdge_(t,i,e,n);if(this.indexIsInRange_(r)){var o=this.calculateScrollIncrement_(t,r,e,n);this.adapter_.incrementScroll(o)}},rl.prototype.scrollIntoViewRTL_=function(t){var e=this.adapter_.getScrollPosition(),n=this.adapter_.getOffsetWidth(),i=this.adapter_.getTabDimensionsAtIndex(t),r=this.adapter_.getScrollContentWidth(),o=this.findAdjacentTabIndexClosestToEdgeRTL_(t,i,e,n,r);if(this.indexIsInRange_(o)){var a=this.calculateScrollIncrementRTL_(t,o,e,n,r);this.adapter_.incrementScroll(a)}},rl);function rl(t){var e=nl.call(this,a(a({},rl.defaultAdapter),t))||this;return e.useAutomaticActivation_=!1,e}var ol,al=il.strings,sl=0,cl=(r(ul,ol=u),ul.attachTo=function(t){return new ul(t)},Object.defineProperty(ul.prototype,"focusOnActivate",{set:function(e){this.tabList_.forEach(function(t){return t.focusOnActivate=e})},enumerable:!1,configurable:!0}),Object.defineProperty(ul.prototype,"useAutomaticActivation",{set:function(t){this.foundation_.setUseAutomaticActivation(t)},enumerable:!1,configurable:!0}),ul.prototype.initialize=function(t,e){void 0===t&&(t=function(t){return new Qu(t)}),void 0===e&&(e=function(t){return new Ru(t)}),this.tabList_=this.instantiateTabs_(t),this.tabScroller_=this.instantiateTabScroller_(e)},ul.prototype.initialSyncWithDOM=function(){var e=this;this.handleTabInteraction_=function(t){return e.foundation_.handleTabInteraction(t)},this.handleKeyDown_=function(t){return e.foundation_.handleKeyDown(t)},this.listen(zu.strings.INTERACTED_EVENT,this.handleTabInteraction_),this.listen("keydown",this.handleKeyDown_);for(var t=0;t<this.tabList_.length;t++)if(this.tabList_[t].active){this.scrollIntoView(t);break}},ul.prototype.destroy=function(){ol.prototype.destroy.call(this),this.unlisten(zu.strings.INTERACTED_EVENT,this.handleTabInteraction_),this.unlisten("keydown",this.handleKeyDown_),this.tabList_.forEach(function(t){return t.destroy()}),this.tabScroller_&&this.tabScroller_.destroy()},ul.prototype.getDefaultFoundation=function(){var n=this;return new il({scrollTo:function(t){return n.tabScroller_.scrollTo(t)},incrementScroll:function(t){return n.tabScroller_.incrementScroll(t)},getScrollPosition:function(){return n.tabScroller_.getScrollPosition()},getScrollContentWidth:function(){return n.tabScroller_.getScrollContentWidth()},getOffsetWidth:function(){return n.root_.offsetWidth},isRTL:function(){return"rtl"===window.getComputedStyle(n.root_).getPropertyValue("direction")},setActiveTab:function(){},activateTabAtIndex:function(t,e){return n.tabList_[t].activate(e)},deactivateTabAtIndex:function(t){return n.tabList_[t].deactivate()},focusTabAtIndex:function(t){return n.tabList_[t].focus()},getTabIndicatorClientRectAtIndex:function(t){return n.tabList_[t].computeIndicatorClientRect()},getTabDimensionsAtIndex:function(t){return n.tabList_[t].computeDimensions()},getPreviousActiveTabIndex:function(){for(var t=0;t<n.tabList_.length;t++)if(n.tabList_[t].active)return t;return-1},getFocusedTabIndex:function(){var t=n.getTabElements_(),e=document.activeElement;return t.indexOf(e)},getIndexOfTabById:function(t){for(var e=0;e<n.tabList_.length;e++)if(n.tabList_[e].id===t)return e;return-1},getTabListLength:function(){return n.tabList_.length},notifyTabActivated:function(t){return n.emit(al.TAB_ACTIVATED_EVENT,{index:t},!0)}})},ul.prototype.activateTab=function(t){this.foundation_.activateTab(t)},ul.prototype.scrollIntoView=function(t){this.foundation_.scrollIntoView(t)},ul.prototype.getTabElements_=function(){return[].slice.call(this.root_.querySelectorAll(al.TAB_SELECTOR))},ul.prototype.instantiateTabs_=function(e){return this.getTabElements_().map(function(t){return t.id=t.id||"mdc-tab-"+ ++sl,e(t)})},ul.prototype.instantiateTabScroller_=function(t){var e=this.root_.querySelector(al.TAB_SCROLLER_SELECTOR);return e?t(e):null},ul);
   function ul(){return null!==ol&&ol.apply(this,arguments)||this}function ll(t){return(ll="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function fl(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function dl(t,e,n){return e&&fl(t.prototype,e),n&&fl(t,n),t}function pl(o){var a=yl();return function(){var t,e,n,i=vl(o);if(a){var r=vl(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==ll(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function hl(t){var i="function"==typeof Map?new Map:void 0;return(hl=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return _l(t,arguments,vl(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),ml(n,t)})(t)}function _l(t,e,n){return(_l=yl()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&ml(r,n.prototype),r}).apply(null,arguments)}function yl(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function ml(t,e){return(ml=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function vl(t){return(vl=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var bl=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&ml(t,e)}(n,hl(HTMLElement));var e=pl(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).activeTabIndex_=-1,t.focusOnActivate_=!1,t.useAutomaticActivation_=!1,t.tabBar_,t}return dl(n,[{key:"focus",value:function(){for(var t=0;t<this.tabBar_.tabList_.length;t++)if(this.tabBar_.tabList_[t].active)return void this.tabBar_.tabList_[t].focus();0<this.tabBar_.tabList[0].length&&this.tabBar_.tabList_[0].focus()}},{key:"blur",value:function(){this.contains(document.activeElement)&&document.activeElement.blur()}},{key:"focusOnActivate",get:function(){return this.focusOnActivate_},set:function(t){this.focusOnActivate_=t,this.tabBar_&&(this.tabBar_.focusOnActivate=t)}},{key:"useAutomaticActivation",get:function(){return this.useAutomaticActivation_},set:function(t){this.useAutomaticActivation_=t,this.tabBar_&&(this.tabBar_.useAutomaticActivation=t)}},{key:"activeTabIndex",get:function(){return this.activeTabIndex_},set:function(t){var e=this.activeTabIndex_;this.activeTabIndex_=t,this.tabBar_&&(-1!==e?this.tabBar_.foundation_.activateTab(t):this.tabBar_.tabList_[t].activate())}}]),dl(n,[{key:"connectedCallback",value:function(){this.tabBar_=new cl(this),this.tabBar_.focusOnActivate=this.focusOnActivate_,this.tabBar_.useAutomaticActivation=this.useAutomaticActivation_,-1!==this.activeTabIndex&&this.tabBar_.tabList_[this.activeTabIndex_].activate()}},{key:"disconnectedCallback",value:function(){this.tabBar_.destroy()}}]),n}();customElements.define("mdc-tab-bar",bl);
var El,gl={ROOT:"mdc-text-field-character-counter"},Cl={ROOT_SELECTOR:"."+gl.ROOT},Al=(r(Tl,El=s),Object.defineProperty(Tl,"cssClasses",{get:function(){return gl},enumerable:!0,configurable:!0}),Object.defineProperty(Tl,"strings",{get:function(){return Cl},enumerable:!0,configurable:!0}),Object.defineProperty(Tl,"defaultAdapter",{get:function(){return{setContent:function(){}}},enumerable:!0,configurable:!0}),Tl.prototype.setCounterValue=function(t,e){t=Math.min(t,e),this.adapter_.setContent(t+" / "+e)},Tl);function Tl(t){return El.call(this,a(a({},Tl.defaultAdapter),t))||this}var Il,Sl=(r(Ol,Il=u),Ol.attachTo=function(t){return new Ol(t)},Object.defineProperty(Ol.prototype,"foundation",{get:function(){return this.foundation_},enumerable:!0,configurable:!0}),Ol.prototype.getDefaultFoundation=function(){var e=this;return new Al({setContent:function(t){e.root_.textContent=t}})},Ol);
   function Ol(){return null!==Il&&Il.apply(this,arguments)||this}
var Rl,Ll={ARIA_CONTROLS:"aria-controls",INPUT_SELECTOR:".mdc-text-field__input",LABEL_SELECTOR:".mdc-floating-label",LEADING_ICON_SELECTOR:".mdc-text-field__icon--leading",LINE_RIPPLE_SELECTOR:".mdc-line-ripple",OUTLINE_SELECTOR:".mdc-notched-outline",PREFIX_SELECTOR:".mdc-text-field__affix--prefix",SUFFIX_SELECTOR:".mdc-text-field__affix--suffix",TRAILING_ICON_SELECTOR:".mdc-text-field__icon--trailing"},wl={DISABLED:"mdc-text-field--disabled",FOCUSED:"mdc-text-field--focused",FULLWIDTH:"mdc-text-field--fullwidth",HELPER_LINE:"mdc-text-field-helper-line",INVALID:"mdc-text-field--invalid",LABEL_FLOATING:"mdc-text-field--label-floating",NO_LABEL:"mdc-text-field--no-label",OUTLINED:"mdc-text-field--outlined",ROOT:"mdc-text-field",TEXTAREA:"mdc-text-field--textarea",WITH_LEADING_ICON:"mdc-text-field--with-leading-icon",WITH_TRAILING_ICON:"mdc-text-field--with-trailing-icon"},xl={LABEL_SCALE:.75},Nl=["pattern","min","max","required","step","minlength","maxlength"],Dl=["color","date","datetime-local","month","range","time","week"],Pl=["mousedown","touchstart"],kl=["click","keydown"],Fl=(r(Ml,Rl=s),Object.defineProperty(Ml,"cssClasses",{get:function(){return wl},enumerable:!1,configurable:!0}),Object.defineProperty(Ml,"strings",{get:function(){return Ll},enumerable:!1,configurable:!0}),Object.defineProperty(Ml,"numbers",{get:function(){return xl},enumerable:!1,configurable:!0}),Object.defineProperty(Ml.prototype,"shouldAlwaysFloat_",{get:function(){var t=this.getNativeInput_().type;return 0<=Dl.indexOf(t)},enumerable:!1,configurable:!0}),Object.defineProperty(Ml.prototype,"shouldFloat",{get:function(){return this.shouldAlwaysFloat_||this.isFocused_||!!this.getValue()||this.isBadInput_()},enumerable:!1,configurable:!0}),Object.defineProperty(Ml.prototype,"shouldShake",{get:function(){return!this.isFocused_&&!this.isValid()&&!!this.getValue()},enumerable:!1,configurable:!0}),Object.defineProperty(Ml,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!0},registerTextFieldInteractionHandler:function(){},deregisterTextFieldInteractionHandler:function(){},registerInputInteractionHandler:function(){},deregisterInputInteractionHandler:function(){},registerValidationAttributeChangeHandler:function(){return new MutationObserver(function(){})},deregisterValidationAttributeChangeHandler:function(){},getNativeInput:function(){return null},isFocused:function(){return!1},activateLineRipple:function(){},deactivateLineRipple:function(){},setLineRippleTransformOrigin:function(){},shakeLabel:function(){},floatLabel:function(){},hasLabel:function(){return!1},getLabelWidth:function(){return 0},hasOutline:function(){return!1},notchOutline:function(){},closeOutline:function(){}}},enumerable:!1,configurable:!0}),Ml.prototype.init=function(){var e=this;this.adapter_.isFocused()?this.inputFocusHandler_():this.adapter_.hasLabel()&&this.shouldFloat&&(this.notchOutline(!0),this.adapter_.floatLabel(!0),this.styleFloating_(!0)),this.adapter_.registerInputInteractionHandler("focus",this.inputFocusHandler_),this.adapter_.registerInputInteractionHandler("blur",this.inputBlurHandler_),this.adapter_.registerInputInteractionHandler("input",this.inputInputHandler_),Pl.forEach(function(t){e.adapter_.registerInputInteractionHandler(t,e.setPointerXOffset_)}),kl.forEach(function(t){e.adapter_.registerTextFieldInteractionHandler(t,e.textFieldInteractionHandler_)}),this.validationObserver_=this.adapter_.registerValidationAttributeChangeHandler(this.validationAttributeChangeHandler_),this.setCharacterCounter_(this.getValue().length)},Ml.prototype.destroy=function(){var e=this;this.adapter_.deregisterInputInteractionHandler("focus",this.inputFocusHandler_),this.adapter_.deregisterInputInteractionHandler("blur",this.inputBlurHandler_),this.adapter_.deregisterInputInteractionHandler("input",this.inputInputHandler_),Pl.forEach(function(t){e.adapter_.deregisterInputInteractionHandler(t,e.setPointerXOffset_)}),kl.forEach(function(t){e.adapter_.deregisterTextFieldInteractionHandler(t,e.textFieldInteractionHandler_)}),this.adapter_.deregisterValidationAttributeChangeHandler(this.validationObserver_)},Ml.prototype.handleTextFieldInteraction=function(){var t=this.adapter_.getNativeInput();t&&t.disabled||(this.receivedUserInput_=!0)},Ml.prototype.handleValidationAttributeChange=function(t){var e=this;t.some(function(t){return-1<Nl.indexOf(t)&&(e.styleValidity_(!0),!0)}),-1<t.indexOf("maxlength")&&this.setCharacterCounter_(this.getValue().length)},Ml.prototype.notchOutline=function(t){if(this.adapter_.hasOutline())if(t){var e=this.adapter_.getLabelWidth()*xl.LABEL_SCALE;this.adapter_.notchOutline(e)}else this.adapter_.closeOutline()},Ml.prototype.activateFocus=function(){this.isFocused_=!0,this.styleFocused_(this.isFocused_),this.adapter_.activateLineRipple(),this.adapter_.hasLabel()&&(this.notchOutline(this.shouldFloat),this.adapter_.floatLabel(this.shouldFloat),this.styleFloating_(this.shouldFloat),this.adapter_.shakeLabel(this.shouldShake)),this.helperText_&&this.helperText_.showToScreenReader()},Ml.prototype.setTransformOrigin=function(t){var e=t.touches,n=e?e[0]:t,i=n.target.getBoundingClientRect(),r=n.clientX-i.left;this.adapter_.setLineRippleTransformOrigin(r)},Ml.prototype.handleInput=function(){this.autoCompleteFocus(),this.setCharacterCounter_(this.getValue().length)},Ml.prototype.autoCompleteFocus=function(){this.receivedUserInput_||this.activateFocus()},Ml.prototype.deactivateFocus=function(){this.isFocused_=!1,this.adapter_.deactivateLineRipple();var t=this.isValid();this.styleValidity_(t),this.styleFocused_(this.isFocused_),this.adapter_.hasLabel()&&(this.notchOutline(this.shouldFloat),this.adapter_.floatLabel(this.shouldFloat),this.styleFloating_(this.shouldFloat),this.adapter_.shakeLabel(this.shouldShake)),this.shouldFloat||(this.receivedUserInput_=!1)},Ml.prototype.getValue=function(){return this.getNativeInput_().value},Ml.prototype.setValue=function(t){this.getValue()!==t&&(this.getNativeInput_().value=t),this.setCharacterCounter_(t.length);var e=this.isValid();this.styleValidity_(e),this.adapter_.hasLabel()&&(this.notchOutline(this.shouldFloat),this.adapter_.floatLabel(this.shouldFloat),this.styleFloating_(this.shouldFloat),this.adapter_.shakeLabel(this.shouldShake))},Ml.prototype.isValid=function(){return this.useNativeValidation_?this.isNativeInputValid_():this.isValid_},Ml.prototype.setValid=function(t){this.isValid_=t,this.styleValidity_(t);var e=!t&&!this.isFocused_&&!!this.getValue();this.adapter_.hasLabel()&&this.adapter_.shakeLabel(e)},Ml.prototype.setUseNativeValidation=function(t){this.useNativeValidation_=t},Ml.prototype.isDisabled=function(){return this.getNativeInput_().disabled},Ml.prototype.setDisabled=function(t){this.getNativeInput_().disabled=t,this.styleDisabled_(t)},Ml.prototype.setHelperTextContent=function(t){this.helperText_&&this.helperText_.setContent(t)},Ml.prototype.setLeadingIconAriaLabel=function(t){this.leadingIcon_&&this.leadingIcon_.setAriaLabel(t)},Ml.prototype.setLeadingIconContent=function(t){this.leadingIcon_&&this.leadingIcon_.setContent(t)},Ml.prototype.setTrailingIconAriaLabel=function(t){this.trailingIcon_&&this.trailingIcon_.setAriaLabel(t)},Ml.prototype.setTrailingIconContent=function(t){this.trailingIcon_&&this.trailingIcon_.setContent(t)},Ml.prototype.setCharacterCounter_=function(t){if(this.characterCounter_){var e=this.getNativeInput_().maxLength;if(-1===e)throw new Error("MDCTextFieldFoundation: Expected maxlength html property on text input or textarea.");this.characterCounter_.setCounterValue(t,e)}},Ml.prototype.isBadInput_=function(){return this.getNativeInput_().validity.badInput||!1},Ml.prototype.isNativeInputValid_=function(){return this.getNativeInput_().validity.valid},Ml.prototype.styleValidity_=function(t){var e=Ml.cssClasses.INVALID;t?this.adapter_.removeClass(e):this.adapter_.addClass(e),this.helperText_&&this.helperText_.setValidity(t)},Ml.prototype.styleFocused_=function(t){var e=Ml.cssClasses.FOCUSED;t?this.adapter_.addClass(e):this.adapter_.removeClass(e)},Ml.prototype.styleDisabled_=function(t){var e=Ml.cssClasses,n=e.DISABLED,i=e.INVALID;t?(this.adapter_.addClass(n),this.adapter_.removeClass(i)):this.adapter_.removeClass(n),this.leadingIcon_&&this.leadingIcon_.setDisabled(t),this.trailingIcon_&&this.trailingIcon_.setDisabled(t)},Ml.prototype.styleFloating_=function(t){var e=Ml.cssClasses.LABEL_FLOATING;t?this.adapter_.addClass(e):this.adapter_.removeClass(e)},Ml.prototype.getNativeInput_=function(){return(this.adapter_?this.adapter_.getNativeInput():null)||{disabled:!1,maxLength:-1,type:"input",validity:{badInput:!1,valid:!0},value:""}},Ml);function Ml(t,e){void 0===e&&(e={});var n=Rl.call(this,a(a({},Ml.defaultAdapter),t))||this;return n.isFocused_=!1,n.receivedUserInput_=!1,n.isValid_=!0,n.useNativeValidation_=!0,n.helperText_=e.helperText,n.characterCounter_=e.characterCounter,n.leadingIcon_=e.leadingIcon,n.trailingIcon_=e.trailingIcon,n.inputFocusHandler_=function(){return n.activateFocus()},n.inputBlurHandler_=function(){return n.deactivateFocus()},n.inputInputHandler_=function(){return n.handleInput()},n.setPointerXOffset_=function(t){return n.setTransformOrigin(t)},n.textFieldInteractionHandler_=function(){return n.handleTextFieldInteraction()},n.validationAttributeChangeHandler_=function(t){return n.handleValidationAttributeChange(t)},n}var Hl,jl={HELPER_TEXT_PERSISTENT:"mdc-text-field-helper-text--persistent",HELPER_TEXT_VALIDATION_MSG:"mdc-text-field-helper-text--validation-msg",ROOT:"mdc-text-field-helper-text"},Bl={ARIA_HIDDEN:"aria-hidden",ROLE:"role",ROOT_SELECTOR:"."+jl.ROOT},Vl=(r(Ul,Hl=s),Object.defineProperty(Ul,"cssClasses",{get:function(){return jl},enumerable:!0,configurable:!0}),Object.defineProperty(Ul,"strings",{get:function(){return Bl},enumerable:!0,configurable:!0}),Object.defineProperty(Ul,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},setAttr:function(){},removeAttr:function(){},setContent:function(){}}},enumerable:!0,configurable:!0}),Ul.prototype.setContent=function(t){this.adapter_.setContent(t)},Ul.prototype.setPersistent=function(t){t?this.adapter_.addClass(jl.HELPER_TEXT_PERSISTENT):this.adapter_.removeClass(jl.HELPER_TEXT_PERSISTENT)},Ul.prototype.setValidation=function(t){t?this.adapter_.addClass(jl.HELPER_TEXT_VALIDATION_MSG):this.adapter_.removeClass(jl.HELPER_TEXT_VALIDATION_MSG)},Ul.prototype.showToScreenReader=function(){this.adapter_.removeAttr(Bl.ARIA_HIDDEN)},Ul.prototype.setValidity=function(t){var e=this.adapter_.hasClass(jl.HELPER_TEXT_PERSISTENT),n=this.adapter_.hasClass(jl.HELPER_TEXT_VALIDATION_MSG)&&!t;n?this.adapter_.setAttr(Bl.ROLE,"alert"):this.adapter_.removeAttr(Bl.ROLE),e||n||this.hide_()},Ul.prototype.hide_=function(){this.adapter_.setAttr(Bl.ARIA_HIDDEN,"true")},Ul);
   function Ul(t){return Hl.call(this,a(a({},Ul.defaultAdapter),t))||this}var Kl,Gl=(r(Wl,Kl=u),Wl.attachTo=function(t){return new Wl(t)},Object.defineProperty(Wl.prototype,"foundation",{get:function(){return this.foundation_},enumerable:!0,configurable:!0}),Wl.prototype.getDefaultFoundation=function(){var n=this;return new Vl({addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},hasClass:function(t){return n.root_.classList.contains(t)},setAttr:function(t,e){return n.root_.setAttribute(t,e)},removeAttr:function(t){return n.root_.removeAttribute(t)},setContent:function(t){n.root_.textContent=t}})},Wl);
   function Wl(){return null!==Kl&&Kl.apply(this,arguments)||this}
var ql,zl={ICON_EVENT:"MDCTextField:icon",ICON_ROLE:"button"},Xl={ROOT:"mdc-text-field__icon"},Yl=["click","keydown"],Ql=(r(Zl,ql=s),Object.defineProperty(Zl,"strings",{get:function(){return zl},enumerable:!0,configurable:!0}),Object.defineProperty(Zl,"cssClasses",{get:function(){return Xl},enumerable:!0,configurable:!0}),Object.defineProperty(Zl,"defaultAdapter",{get:function(){return{getAttr:function(){return null},setAttr:function(){},removeAttr:function(){},setContent:function(){},registerInteractionHandler:function(){},deregisterInteractionHandler:function(){},notifyIconAction:function(){}}},enumerable:!0,configurable:!0}),Zl.prototype.init=function(){var e=this;this.savedTabIndex_=this.adapter_.getAttr("tabindex"),Yl.forEach(function(t){e.adapter_.registerInteractionHandler(t,e.interactionHandler_)})},Zl.prototype.destroy=function(){var e=this;Yl.forEach(function(t){e.adapter_.deregisterInteractionHandler(t,e.interactionHandler_)})},Zl.prototype.setDisabled=function(t){this.savedTabIndex_&&(t?(this.adapter_.setAttr("tabindex","-1"),this.adapter_.removeAttr("role")):(this.adapter_.setAttr("tabindex",this.savedTabIndex_),this.adapter_.setAttr("role",zl.ICON_ROLE)))},Zl.prototype.setAriaLabel=function(t){this.adapter_.setAttr("aria-label",t)},Zl.prototype.setContent=function(t){this.adapter_.setContent(t)},Zl.prototype.handleInteraction=function(t){var e="Enter"===t.key||13===t.keyCode;"click"!==t.type&&!e||(t.preventDefault(),this.adapter_.notifyIconAction())},Zl);function Zl(t){var e=ql.call(this,a(a({},Zl.defaultAdapter),t))||this;return e.savedTabIndex_=null,e.interactionHandler_=function(t){return e.handleInteraction(t)},e}var $l,Jl=(r(tf,$l=u),tf.attachTo=function(t){return new tf(t)},Object.defineProperty(tf.prototype,"foundation",{get:function(){return this.foundation_},enumerable:!0,configurable:!0}),tf.prototype.getDefaultFoundation=function(){var n=this;return new Ql({getAttr:function(t){return n.root_.getAttribute(t)},setAttr:function(t,e){return n.root_.setAttribute(t,e)},removeAttr:function(t){return n.root_.removeAttribute(t)},setContent:function(t){n.root_.textContent=t},registerInteractionHandler:function(t,e){return n.listen(t,e)},deregisterInteractionHandler:function(t,e){return n.unlisten(t,e)},notifyIconAction:function(){return n.emit(Ql.strings.ICON_EVENT,{},!0)}})},tf);
   function tf(){return null!==$l&&$l.apply(this,arguments)||this}
var ef,nf=(r(rf,ef=u),rf.attachTo=function(t){return new rf(t)},rf.prototype.initialize=function(t,e,n,i,r,o,a){void 0===t&&(t=function(t,e){return new _t(t,e)}),void 0===e&&(e=function(t){return new qa(t)}),void 0===n&&(n=function(t){return new Gl(t)}),void 0===i&&(i=function(t){return new Sl(t)}),void 0===r&&(r=function(t){return new Jl(t)}),void 0===o&&(o=function(t){return new ja(t)}),void 0===a&&(a=function(t){return new es(t)}),this.input_=this.root_.querySelector(Ll.INPUT_SELECTOR);var s=this.root_.querySelector(Ll.LABEL_SELECTOR);this.label_=s?o(s):null;var c=this.root_.querySelector(Ll.LINE_RIPPLE_SELECTOR);this.lineRipple_=c?e(c):null;var u=this.root_.querySelector(Ll.OUTLINE_SELECTOR);this.outline_=u?a(u):null;var l=Vl.strings,f=this.root_.nextElementSibling,d=f&&f.classList.contains(wl.HELPER_LINE),p=d&&f&&f.querySelector(l.ROOT_SELECTOR);this.helperText_=p?n(p):null;var h=Al.strings,_=this.root_.querySelector(h.ROOT_SELECTOR);!_&&d&&f&&(_=f.querySelector(h.ROOT_SELECTOR)),this.characterCounter_=_?i(_):null;var y=this.root_.querySelector(Ll.LEADING_ICON_SELECTOR);this.leadingIcon_=y?r(y):null;var m=this.root_.querySelector(Ll.TRAILING_ICON_SELECTOR);this.trailingIcon_=m?r(m):null,this.prefix_=this.root_.querySelector(Ll.PREFIX_SELECTOR),this.suffix_=this.root_.querySelector(Ll.SUFFIX_SELECTOR),this.ripple=this.createRipple_(t)},rf.prototype.destroy=function(){this.ripple&&this.ripple.destroy(),this.lineRipple_&&this.lineRipple_.destroy(),this.helperText_&&this.helperText_.destroy(),this.characterCounter_&&this.characterCounter_.destroy(),this.leadingIcon_&&this.leadingIcon_.destroy(),this.trailingIcon_&&this.trailingIcon_.destroy(),this.label_&&this.label_.destroy(),this.outline_&&this.outline_.destroy(),ef.prototype.destroy.call(this)},rf.prototype.initialSyncWithDOM=function(){this.disabled=this.input_.disabled},Object.defineProperty(rf.prototype,"value",{get:function(){return this.foundation_.getValue()},set:function(t){this.foundation_.setValue(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"disabled",{get:function(){return this.foundation_.isDisabled()},set:function(t){this.foundation_.setDisabled(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"valid",{get:function(){return this.foundation_.isValid()},set:function(t){this.foundation_.setValid(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"required",{get:function(){return this.input_.required},set:function(t){this.input_.required=t},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"pattern",{get:function(){return this.input_.pattern},set:function(t){null===t?delete this.input_.pattern:this.input_.pattern=t},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"minLength",{get:function(){return this.input_.minLength},set:function(t){t<0?this.input_.removeAttribute("minLength"):this.input_.minLength=t},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"maxLength",{get:function(){return this.input_.maxLength},set:function(t){t<0?this.input_.removeAttribute("maxLength"):this.input_.maxLength=t},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"min",{get:function(){return this.input_.min},set:function(t){this.input_.min=t},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"max",{get:function(){return this.input_.max},set:function(t){this.input_.max=t},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"step",{get:function(){return this.input_.step},set:function(t){this.input_.step=t},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"helperTextContent",{set:function(t){this.foundation_.setHelperTextContent(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"leadingIconAriaLabel",{set:function(t){this.foundation_.setLeadingIconAriaLabel(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"leadingIconContent",{set:function(t){this.foundation_.setLeadingIconContent(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"trailingIconAriaLabel",{set:function(t){this.foundation_.setTrailingIconAriaLabel(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"trailingIconContent",{set:function(t){this.foundation_.setTrailingIconContent(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"useNativeValidation",{set:function(t){this.foundation_.setUseNativeValidation(t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"prefixText",{get:function(){return this.prefix_?this.prefix_.textContent:null},set:function(t){this.prefix_&&(this.prefix_.textContent=t)},enumerable:!1,configurable:!0}),Object.defineProperty(rf.prototype,"suffixText",{get:function(){return this.suffix_?this.suffix_.textContent:null},set:function(t){this.suffix_&&(this.suffix_.textContent=t)},enumerable:!1,configurable:!0}),rf.prototype.focus=function(){this.input_.focus()},rf.prototype.layout=function(){var t=this.foundation_.shouldFloat;this.foundation_.notchOutline(t)},rf.prototype.getDefaultFoundation=function(){var t=a(a(a(a(a({},this.getRootAdapterMethods_()),this.getInputAdapterMethods_()),this.getLabelAdapterMethods_()),this.getLineRippleAdapterMethods_()),this.getOutlineAdapterMethods_());return new Fl(t,this.getFoundationMap_())},rf.prototype.getRootAdapterMethods_=function(){var n=this;return{addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},hasClass:function(t){return n.root_.classList.contains(t)},registerTextFieldInteractionHandler:function(t,e){return n.listen(t,e)},deregisterTextFieldInteractionHandler:function(t,e){return n.unlisten(t,e)},registerValidationAttributeChangeHandler:function(e){var t=new MutationObserver(function(t){return e(t.map(function(t){return t.attributeName}).filter(function(t){return t}))});return t.observe(n.input_,{attributes:!0}),t},deregisterValidationAttributeChangeHandler:function(t){return t.disconnect()}}},rf.prototype.getInputAdapterMethods_=function(){var n=this;return{getNativeInput:function(){return n.input_},isFocused:function(){return document.activeElement===n.input_},registerInputInteractionHandler:function(t,e){return n.input_.addEventListener(t,e,d())},deregisterInputInteractionHandler:function(t,e){return n.input_.removeEventListener(t,e,d())}}},rf.prototype.getLabelAdapterMethods_=function(){var e=this;return{floatLabel:function(t){return e.label_&&e.label_.float(t)},getLabelWidth:function(){return e.label_?e.label_.getWidth():0},hasLabel:function(){return Boolean(e.label_)},shakeLabel:function(t){return e.label_&&e.label_.shake(t)}}},rf.prototype.getLineRippleAdapterMethods_=function(){var e=this;return{activateLineRipple:function(){e.lineRipple_&&e.lineRipple_.activate()},deactivateLineRipple:function(){e.lineRipple_&&e.lineRipple_.deactivate()},setLineRippleTransformOrigin:function(t){e.lineRipple_&&e.lineRipple_.setRippleCenter(t)}}},rf.prototype.getOutlineAdapterMethods_=function(){var e=this;return{closeOutline:function(){return e.outline_&&e.outline_.closeNotch()},hasOutline:function(){return Boolean(e.outline_)},notchOutline:function(t){return e.outline_&&e.outline_.notch(t)}}},rf.prototype.getFoundationMap_=function(){return{characterCounter:this.characterCounter_?this.characterCounter_.foundation:void 0,helperText:this.helperText_?this.helperText_.foundation:void 0,leadingIcon:this.leadingIcon_?this.leadingIcon_.foundation:void 0,trailingIcon:this.trailingIcon_?this.trailingIcon_.foundation:void 0}},rf.prototype.createRipple_=function(t){var n=this,e=this.root_.classList.contains(wl.TEXTAREA),i=this.root_.classList.contains(wl.OUTLINED);if(e||i)return null;var r=a(a({},_t.createAdapter(this)),{isSurfaceActive:function(){return h(n.input_,":active")},registerInteractionHandler:function(t,e){return n.input_.addEventListener(t,e,d())},deregisterInteractionHandler:function(t,e){return n.input_.removeEventListener(t,e,d())}});return t(this.root_,new dt(r))},rf);function rf(){return null!==ef&&ef.apply(this,arguments)||this}function of(t){return(of="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function af(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function sf(t,e,n){return e&&af(t.prototype,e),n&&af(t,n),t}function cf(o){var a=ff();return function(){var t,e,n,i=pf(o);if(a){var r=pf(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==of(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function uf(t){var i="function"==typeof Map?new Map:void 0;return(uf=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return lf(t,arguments,pf(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),df(n,t)})(t)}function lf(t,e,n){return(lf=ff()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&df(r,n.prototype),r}).apply(null,arguments)}function ff(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function df(t,e){return(df=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function pf(t){return(pf=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var hf=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&df(t,e)}(n,uf(HTMLElement));var e=cf(n);function n(){var t;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,n),(t=e.call(this)).value_="",t.disabled_=!1,t.valid_=!0,t.required_=!1,t.pattern_=null,t.minLength_=-1,t.maxLength_=-1,t.min_="",t.max_="",t.step_="",t.textField_,t}return sf(n,[{key:"focus",value:function(){this.textField_&&this.textField_.input_&&this.textField_.input_.focus()}},{key:"value",get:function(){return this.textField_?this.textField_.value:this.value_},set:function(t){this.value_=t,this.textField_&&(this.textField_.value=t)}},{key:"disabled",get:function(){return this.disabled_},set:function(t){this.disabled_=t,this.textField_&&(this.textField_.disabled=t)}},{key:"valid",get:function(){return this.valid_},set:function(t){this.valid_=t,this.textField_&&(this.textField_.valid=t)}},{key:"required",get:function(){return this.required_},set:function(t){this.required_=t,this.textField_&&(this.required_=t)}},{key:"pattern",get:function(){return this.pattern_},set:function(t){this.pattern_=t,this.textField_&&(this.textField_.pattern=t)}},{key:"minLength",get:function(){return this.minLength_},set:function(t){this.minLength_=t,this.textField&&(this.textField_.minLength=t)}},{key:"maxLength",get:function(){return this.maxLength_},set:function(t){this.maxLength_=t,this.textField_&&(this.textField_.maxLength=t)}},{key:"min",get:function(){return this.min_},set:function(t){this.min_=t,this.textField_&&(this.textField_.min=t)}},{key:"max",get:function(){return this.max_},set:function(t){this.max_=t,this.textField_&&(this.textField_.max=t)}},{key:"step",get:function(){return this.step_},set:function(t){this.step_=t,this.textField_&&(this.textField_.step=t)}}]),sf(n,[{key:"connectedCallback",value:function(){k.call(this),this.textField_=new nf(this),this.textField_.useNativeValidation=!1,this.textField_.value=this.value_,this.textField_.disabled=this.disabled_,this.textField_.valid=this.valid_,this.textField_.required=this.required_,this.textField_.pattern=this.pattern_,this.textField_.minLength=this.minLength_,this.textField_.maxLength=this.maxLength_,this.textField_.min=this.min_,this.textField_.max=this.max_,this.textField_.step=this.step_;var t=this.querySelector(".mdc-floating-label");t&&k.call(t)}},{key:"disconnectedCallback",value:function(){var t=this.querySelector(".mdc-floating-label");t&&F.call(t),this.textField_.destroy(),F.call(this)}}]),n}();customElements.define("mdc-text-field",hf);
var _f,yf={FIXED_CLASS:"mdc-top-app-bar--fixed",FIXED_SCROLLED_CLASS:"mdc-top-app-bar--fixed-scrolled",SHORT_CLASS:"mdc-top-app-bar--short",SHORT_COLLAPSED_CLASS:"mdc-top-app-bar--short-collapsed",SHORT_HAS_ACTION_ITEM_CLASS:"mdc-top-app-bar--short-has-action-item"},mf={DEBOUNCE_THROTTLE_RESIZE_TIME_MS:100,MAX_TOP_APP_BAR_HEIGHT:128},vf={ACTION_ITEM_SELECTOR:".mdc-top-app-bar__action-item",NAVIGATION_EVENT:"MDCTopAppBar:nav",NAVIGATION_ICON_SELECTOR:".mdc-top-app-bar__navigation-icon",ROOT_SELECTOR:".mdc-top-app-bar",TITLE_SELECTOR:".mdc-top-app-bar__title"},bf=(r(Ef,_f=s),Object.defineProperty(Ef,"strings",{get:function(){return vf},enumerable:!0,configurable:!0}),Object.defineProperty(Ef,"cssClasses",{get:function(){return yf},enumerable:!0,configurable:!0}),Object.defineProperty(Ef,"numbers",{get:function(){return mf},enumerable:!0,configurable:!0}),Object.defineProperty(Ef,"defaultAdapter",{get:function(){return{addClass:function(){},removeClass:function(){},hasClass:function(){return!1},setStyle:function(){},getTopAppBarHeight:function(){return 0},notifyNavigationIconClicked:function(){},getViewportScrollY:function(){return 0},getTotalActionItems:function(){return 0}}},enumerable:!0,configurable:!0}),Ef.prototype.handleTargetScroll=function(){},Ef.prototype.handleWindowResize=function(){},Ef.prototype.handleNavigationClick=function(){this.adapter_.notifyNavigationIconClicked()},Ef);function Ef(t){return _f.call(this,a(a({},Ef.defaultAdapter),t))||this}var gf,Cf=(r(Af,gf=bf),Af.prototype.destroy=function(){gf.prototype.destroy.call(this),this.adapter_.setStyle("top","")},Af.prototype.handleTargetScroll=function(){var t=Math.max(this.adapter_.getViewportScrollY(),0),e=t-this.lastScrollPosition_;this.lastScrollPosition_=t,this.isCurrentlyBeingResized_||(this.currentAppBarOffsetTop_-=e,0<this.currentAppBarOffsetTop_?this.currentAppBarOffsetTop_=0:Math.abs(this.currentAppBarOffsetTop_)>this.topAppBarHeight_&&(this.currentAppBarOffsetTop_=-this.topAppBarHeight_),this.moveTopAppBar_())},Af.prototype.handleWindowResize=function(){var t=this;this.resizeThrottleId_||(this.resizeThrottleId_=setTimeout(function(){t.resizeThrottleId_=0,t.throttledResizeHandler_()},mf.DEBOUNCE_THROTTLE_RESIZE_TIME_MS)),this.isCurrentlyBeingResized_=!0,this.resizeDebounceId_&&clearTimeout(this.resizeDebounceId_),this.resizeDebounceId_=setTimeout(function(){t.handleTargetScroll(),t.isCurrentlyBeingResized_=!1,t.resizeDebounceId_=0},mf.DEBOUNCE_THROTTLE_RESIZE_TIME_MS)},Af.prototype.checkForUpdate_=function(){var t=-this.topAppBarHeight_,e=this.currentAppBarOffsetTop_<0,n=this.currentAppBarOffsetTop_>t,i=e&&n;if(i)this.wasDocked_=!1;else{if(!this.wasDocked_)return this.wasDocked_=!0;if(this.isDockedShowing_!==n)return this.isDockedShowing_=n,!0}return i},Af.prototype.moveTopAppBar_=function(){if(this.checkForUpdate_()){var t=this.currentAppBarOffsetTop_;Math.abs(t)>=this.topAppBarHeight_&&(t=-mf.MAX_TOP_APP_BAR_HEIGHT),this.adapter_.setStyle("top",t+"px")}},Af.prototype.throttledResizeHandler_=function(){var t=this.adapter_.getTopAppBarHeight();this.topAppBarHeight_!==t&&(this.wasDocked_=!1,this.currentAppBarOffsetTop_-=this.topAppBarHeight_-t,this.topAppBarHeight_=t),this.handleTargetScroll()},Af);
   function Af(t){var e=gf.call(this,t)||this;return e.wasDocked_=!0,e.isDockedShowing_=!0,e.currentAppBarOffsetTop_=0,e.isCurrentlyBeingResized_=!1,e.resizeThrottleId_=0,e.resizeDebounceId_=0,e.lastScrollPosition_=e.adapter_.getViewportScrollY(),e.topAppBarHeight_=e.adapter_.getTopAppBarHeight(),e}var Tf,If=(r(Sf,Tf=Cf),Sf.prototype.handleTargetScroll=function(){this.adapter_.getViewportScrollY()<=0?this.wasScrolled_&&(this.adapter_.removeClass(yf.FIXED_SCROLLED_CLASS),this.wasScrolled_=!1):this.wasScrolled_||(this.adapter_.addClass(yf.FIXED_SCROLLED_CLASS),this.wasScrolled_=!0)},Sf);
   function Sf(){var t=null!==Tf&&Tf.apply(this,arguments)||this;return t.wasScrolled_=!1,t}var Of,Rf=(r(Lf,Of=bf),Object.defineProperty(Lf.prototype,"isCollapsed",{get:function(){return this.isCollapsed_},enumerable:!0,configurable:!0}),Lf.prototype.init=function(){Of.prototype.init.call(this),0<this.adapter_.getTotalActionItems()&&this.adapter_.addClass(yf.SHORT_HAS_ACTION_ITEM_CLASS),this.setAlwaysCollapsed(this.adapter_.hasClass(yf.SHORT_COLLAPSED_CLASS))},Lf.prototype.setAlwaysCollapsed=function(t){this.isAlwaysCollapsed_=!!t,this.isAlwaysCollapsed_?this.collapse_():this.maybeCollapseBar_()},Lf.prototype.getAlwaysCollapsed=function(){return this.isAlwaysCollapsed_},Lf.prototype.handleTargetScroll=function(){this.maybeCollapseBar_()},Lf.prototype.maybeCollapseBar_=function(){this.isAlwaysCollapsed_||(this.adapter_.getViewportScrollY()<=0?this.isCollapsed_&&this.uncollapse_():this.isCollapsed_||this.collapse_())},Lf.prototype.uncollapse_=function(){this.adapter_.removeClass(yf.SHORT_COLLAPSED_CLASS),this.isCollapsed_=!1},Lf.prototype.collapse_=function(){this.adapter_.addClass(yf.SHORT_COLLAPSED_CLASS),this.isCollapsed_=!0},Lf);
   function Lf(t){var e=Of.call(this,t)||this;return e.isCollapsed_=!1,e.isAlwaysCollapsed_=!1,e}var wf,xf=(r(Nf,wf=u),Nf.attachTo=function(t){return new Nf(t)},Nf.prototype.initialize=function(){this.navIcon_=this.root_.querySelector(vf.NAVIGATION_ICON_SELECTOR),this.scrollTarget_=window},Nf.prototype.initialSyncWithDOM=function(){this.handleNavigationClick_=this.foundation_.handleNavigationClick.bind(this.foundation_),this.handleWindowResize_=this.foundation_.handleWindowResize.bind(this.foundation_),this.handleTargetScroll_=this.foundation_.handleTargetScroll.bind(this.foundation_),this.scrollTarget_.addEventListener("scroll",this.handleTargetScroll_),this.navIcon_&&this.navIcon_.addEventListener("click",this.handleNavigationClick_);var t=this.root_.classList.contains(yf.FIXED_CLASS);this.root_.classList.contains(yf.SHORT_CLASS)||t||window.addEventListener("resize",this.handleWindowResize_)},Nf.prototype.destroy=function(){this.scrollTarget_.removeEventListener("scroll",this.handleTargetScroll_),this.navIcon_&&this.navIcon_.removeEventListener("click",this.handleNavigationClick_);var t=this.root_.classList.contains(yf.FIXED_CLASS);this.root_.classList.contains(yf.SHORT_CLASS)||t||window.removeEventListener("resize",this.handleWindowResize_),wf.prototype.destroy.call(this)},Nf.prototype.setScrollTarget=function(t){this.scrollTarget_.removeEventListener("scroll",this.handleTargetScroll_),this.scrollTarget_=t,this.handleTargetScroll_=this.foundation_.handleTargetScroll.bind(this.foundation_),this.scrollTarget_.addEventListener("scroll",this.handleTargetScroll_)},Nf.prototype.getDefaultFoundation=function(){var n=this,t={hasClass:function(t){return n.root_.classList.contains(t)},addClass:function(t){return n.root_.classList.add(t)},removeClass:function(t){return n.root_.classList.remove(t)},setStyle:function(t,e){return n.root_.style.setProperty(t,e)},getTopAppBarHeight:function(){return n.root_.clientHeight},notifyNavigationIconClicked:function(){return n.emit(vf.NAVIGATION_EVENT,{})},getViewportScrollY:function(){var t=n.scrollTarget_,e=n.scrollTarget_;return void 0!==t.pageYOffset?t.pageYOffset:e.scrollTop},getTotalActionItems:function(){return n.root_.querySelectorAll(vf.ACTION_ITEM_SELECTOR).length}};return this.root_.classList.contains(yf.SHORT_CLASS)?new Rf(t):this.root_.classList.contains(yf.FIXED_CLASS)?new If(t):new Cf(t)},Nf);
   function Nf(){return null!==wf&&wf.apply(this,arguments)||this}function Df(t){return(Df="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function Pf(t,e){for(var n=0;n<e.length;n++){var i=e[n];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(t,i.key,i)}}function kf(o){var a=Hf();return function(){var t,e,n,i=Bf(o);if(a){var r=Bf(this).constructor;t=Reflect.construct(i,arguments,r)}else t=i.apply(this,arguments);return e=this,!(n=t)||"object"!==Df(n)&&"function"!=typeof n?function(t){if(void 0!==t)return t;throw new ReferenceError("this hasn't been initialised - super() hasn't been called")}(e):n}}function Ff(t){var i="function"==typeof Map?new Map:void 0;return(Ff=function(t){if(null===t||(e=t,-1===Function.toString.call(e).indexOf("[native code]")))return t;var e;if("function"!=typeof t)throw new TypeError("Super expression must either be null or a function");if(void 0!==i){if(i.has(t))return i.get(t);i.set(t,n)}function n(){return Mf(t,arguments,Bf(this).constructor)}return n.prototype=Object.create(t.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),jf(n,t)})(t)}function Mf(t,e,n){return(Mf=Hf()?Reflect.construct:function(t,e,n){var i=[null];i.push.apply(i,e);var r=new(Function.bind.apply(t,i));return n&&jf(r,n.prototype),r}).apply(null,arguments)}function Hf(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],function(){})),!0}catch(t){return!1}}function jf(t,e){return(jf=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function Bf(t){return(Bf=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var Vf=function(){!function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),e&&jf(t,e)}(r,Ff(HTMLElement));var t,e,n,i=kf(r);function r(){return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,r),i.call(this)}return t=r,(e=[{key:"connectedCallback",value:function(){k.call(this),this.topAppBar_=new xf(this)}},{key:"disconnectedCallback",value:function(){this.topAppBar_.destroy(),F.call(this)}}])&&Pf(t.prototype,e),n&&Pf(t,n),r}();customElements.define("mdc-top-app-bar",Vf)}]);
window = typeof window === 'undefined' ? {} : window;
window['diff'] = function diff(currentObj, newObj, parent, doc) {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) window['createNode'](newObj, parent, doc);
  else if (currentObj && !newObj) window['destroyNode'](currentObj, parent);
  else {
    if (currentObj.type === 'vtext') {
      if (newObj.type === 'vnode') window['replaceTextWithElement'](currentObj, newObj, parent, doc);
      else window['diffTextNodes'](currentObj, newObj);
    } else {
      if (newObj.type === 'vnode') window['diffVNodes'](currentObj, newObj, parent, doc);
      else window['replaceElementWithText'](currentObj, newObj, parent, doc);
    }
  }
};
window['destroyNode'] = function destroyNode(obj, parent) {
  window['callBeforeDestroyedRecursive'](obj);
  parent.removeChild(obj['domRef']);
  window['callDestroyedRecursive'](obj);
};
window['callDestroyedRecursive'] = function callDestroyedRecursive(obj) {
  window['callDestroyed'](obj);
  for (var i in obj.children)
    window['callDestroyedRecursive'](obj.children[i]);
};
window['callDestroyed'] = function callDestroyed(obj) {
  if (obj['onDestroyed']) obj['onDestroyed']();
};
window['callBeforeDestroyed'] = function callBeforeDestroyed(obj) {
  if (obj['onBeforeDestroyed']) obj['onBeforeDestroyed']();
};
window['callBeforeDestroyedRecursive'] = function callBeforeDestroyedRecursive(obj) {
  window['callBeforeDestroyed'](obj);
  for (var i in obj.children)
    window['callBeforeDestroyedRecursive'](obj.children[i]);
};
window['diffTextNodes'] = function diffTextNodes(c, n) {
  if (c['text'] !== n['text']) c['domRef'].textContent = n['text'];
  n['domRef'] = c['domRef'];
};
window['replaceElementWithText'] = function replaceElementWithText(c, n, parent, doc) {
  n['domRef'] = doc.createTextNode(n['text']);
  window['callBeforeDestroyedRecursive'](c);
  parent.replaceChild(n['domRef'], c['domRef']);
  window['callDestroyedRecursive'](c);
};
window['replaceTextWithElement'] = function replaceTextWithElement(c, n, parent, doc) {
  window['createElement'](n, doc);
  parent.replaceChild(n['domRef'], c['domRef']);
  window['callCreated'](n);
};
window['callCreated'] = function callCreated(obj) {
  if (obj['onCreated']) obj['onCreated']();
};
window['populate'] = function populate(c, n, doc) {
  if (!c) c = {
    props: null,
    css: null,
    children: []
  }
  window['diffProps'](c['props'], n['props'], n['domRef'], n['ns'] === 'svg');
  window['diffCss'](c['css'], n['css'], n['domRef']);
  window['diffChildren'](c['children'], n['children'], n['domRef'], doc);
};
window['diffVNodes'] = function diffVNodes(c, n, parent, doc) {
  if (c['tag'] === n['tag'] && n['key'] === c['key']) {
    n['domRef'] = c['domRef'];
    window['populate'](c, n, doc);
  } else {
    window['createElement'](n, doc);
    window['callBeforeDestroyedRecursive'](c);
    parent.replaceChild(n['domRef'], c['domRef']);
    window['callDestroyedRecursive'](c);
    window['callCreated'](n);
  }
};
window['diffProps'] = function diffProps(cProps, nProps, node, isSvg) {
  var newProp;
  for (var c in cProps) {
    newProp = nProps[c];
    if (newProp === undefined) {
      if (isSvg || !(c in node))
        node.removeAttribute(c, cProps[c]);
      else
        node[c] = '';
    } else {
      if (newProp === cProps[c] && c !== 'checked' && c !== 'value') continue;
      if (isSvg) {
        if (c === 'href')
          node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);
        else
          node.setAttribute(c, newProp);
      } else if (c in node && !(c === 'list' || c === 'form')) {
        node[c] = newProp;
      } else {
        node.setAttribute(c, newProp);
      }
    }
  }
  for (var n in nProps) {
    if (cProps && cProps[n]) continue;
    newProp = nProps[n];
    if (isSvg) {
      if (n === 'href')
        node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);
      else
        node.setAttribute(n, newProp);
    } else if (n in node && !(n === 'list' || n === 'form')) {
      node[n] = nProps[n];
    } else {
      node.setAttribute(n, newProp);
    }
  }
};
window['diffCss'] = function diffCss(cCss, nCss, node) {
  var result;
  for (var c in cCss) {
    result = nCss[c];
    if (!result) {
      node.style[c] = null;
    } else if (result !== cCss[c]) {
      node.style[c] = result;
    }
  }
  for (var n in nCss) {
    if (cCss && cCss[n]) continue;
    node.style[n] = nCss[n];
  }
};
window['hasKeys'] = function hasKeys(ns, cs) {
  return ns.length > 0 && cs.length > 0 && ns[0]['key'] != null && cs[0]['key'] != null;
};
window['diffChildren'] = function diffChildren(cs, ns, parent, doc) {
  var longest = ns.length > cs.length ? ns.length : cs.length;
  if (window['hasKeys'](ns, cs)) {
    window['syncChildren'](cs, ns, parent, doc);
  } else {
    for (var i = 0; i < longest; i++)
      window['diff'](cs[i], ns[i], parent, doc);
  }
};
window['createElement'] = function createElement(obj, doc) {
  if (obj['ns'] === 'svg') {
    obj['domRef'] = doc.createElementNS('http://www.w3.org/2000/svg', obj['tag']);
  } else if (obj['ns'] === 'mathml') {
    obj['domRef'] = doc.createElementNS('http://www.w3.org/1998/Math/MathML', obj['tag']);
  } else {
    obj['domRef'] = doc.createElement(obj['tag']);
  }
  window['populate'](null, obj, doc);
};
window['createNode'] = function createNode(obj, parent, doc) {
  if (obj.type === 'vnode') window['createElement'](obj, doc);
  else obj['domRef'] = doc.createTextNode(obj['text']);
  parent.appendChild(obj['domRef']);
  window['callCreated'](obj);
};
window['syncChildren'] = function syncChildren(os, ns, parent, doc) {
  var oldFirstIndex = 0,
    newFirstIndex = 0,
    oldLastIndex = os.length - 1,
    newLastIndex = ns.length - 1,
    nFirst, nLast, oLast, oFirst, tmp, found, node;
  for (;;) {
    if (newFirstIndex > newLastIndex && oldFirstIndex > oldLastIndex) {
      break;
    }
    nFirst = ns[newFirstIndex];
    nLast = ns[newLastIndex];
    oFirst = os[oldFirstIndex];
    oLast = os[oldLastIndex];
    if (oldFirstIndex > oldLastIndex) {
      window['diff'](null, nFirst, parent, doc);
      parent.insertBefore(nFirst['domRef'], oFirst ? oFirst['domRef'] : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    }
    else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        parent.removeChild(os[oldLastIndex--]['domRef']);
      }
      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);
      break;
    }
    else if (oFirst['key'] === nFirst['key']) {
      window['diff'](os[oldFirstIndex++], ns[newFirstIndex++], parent, doc);
    } else if (oLast['key'] === nLast['key']) {
      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);
    }
    else if (oFirst['key'] === nLast['key'] && nFirst['key'] === oLast['key']) {
      window['swapDomRefs'](node, oLast['domRef'], oFirst['domRef'], parent);
      window['swap'](os, oldFirstIndex, oldLastIndex);
      window['diff'](os[oldFirstIndex++], ns[newFirstIndex++], parent, doc);
      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);
    }
    else if (oFirst['key'] === nLast['key']) {
      parent.insertBefore(oFirst['domRef'], oLast['domRef'].nextSibling);
      os.splice(oldLastIndex,0,os.splice(oldFirstIndex,1)[0]);
      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);
    }
    else if (oLast['key'] === nFirst['key']) {
      parent.insertBefore(oLast['domRef'], oFirst['domRef']);
      os.splice(oldFirstIndex,0, os.splice(oldLastIndex,1)[0]);
      window['diff'](os[oldFirstIndex++], nFirst, parent, doc);
      newFirstIndex++;
    }
    else {
      found = false;
      tmp = oldFirstIndex;
      while (tmp <= oldLastIndex) {
        if (os[tmp]['key'] === nFirst['key']) {
          found = true;
          node = os[tmp];
          break;
        }
        tmp++;
      }
      if (found) {
        os.splice(oldFirstIndex,0, os.splice(tmp,1)[0]);
        window['diff'](os[oldFirstIndex++], nFirst, parent, doc);
        parent.insertBefore(node['domRef'], os[oldFirstIndex]['domRef']);
        newFirstIndex++;
      }
      else {
        window['createElement'](nFirst, doc);
        parent.insertBefore(nFirst['domRef'], oFirst['domRef']);
        os.splice(oldFirstIndex++, 0, nFirst);
        newFirstIndex++;
        oldLastIndex++;
      }
    }
  }
};
window['swapDomRefs'] = function swapDomRefs(tmp,a,b,p) {
  tmp = a.nextSibling;
  p.insertBefore(a,b);
  p.insertBefore(b,tmp);
};
window['swap']= function swap(os,l,r) {
  var k = os[l];
  os[l] = os[r];
  os[r] = k;
};
window = typeof window === 'undefined' ? {} : window;
window['oldCallbacks'] = [];
window['currentCallbacks'] = [];
window['registerCallback'] = function registerCallback(cb) {
  window['currentCallbacks'].push(cb);
};
window['swapCallbacks'] = function swapCallbacks() {
  window['oldCallbacks'] = window['currentCallbacks'];
  window['currentCallbacks'] = [];
};
window['releaseCallbacks'] = function releaseCallbacks() {
  for (var i in window['oldCallbacks'])
    h$release(window['oldCallbacks'][i]);
  window['oldCallbacks'] = [];
};
window['delegate'] = function delegate(mountPointElement, events, getVTree) {
  for (var event in events) {
    mountPointElement.addEventListener(events[event][0], function(e) {
      getVTree(function (obj) {
        window['delegateEvent'](e, obj, window['buildTargetToElement'](mountPointElement, e.target), []);
      });
    }, events[event][1]);
  }
};
window['delegateEvent'] = function delegateEvent (event, obj, stack, parentStack) {
  if (!stack.length) return;
  else if (stack.length > 1) {
    parentStack.unshift(obj);
    for (var o = 0; o < obj.children.length; o++) {
      if (obj.children[o]['domRef'] === stack[1]) {
        delegateEvent( event, obj.children[o], stack.slice(1), parentStack );
        break;
      }
    }
  }
  else {
    var eventObj = obj['events'][event.type];
    if (eventObj) {
      var options = eventObj.options;
      if (options['preventDefault'])
        event.preventDefault();
      eventObj['runEvent'](event);
      if (!options['stopPropagation'])
        window['propogateWhileAble'] (parentStack, event);
    } else {
      window['propogateWhileAble'] (parentStack, event);
    }
  }
};
window['buildTargetToElement'] = function buildTargetToElement (element, target) {
  var stack = [];
  while (element !== target) {
    stack.unshift (target);
    target = target.parentNode;
  }
  return stack;
};
window['propogateWhileAble'] = function propogateWhileAble (parentStack, event) {
  for (var i = 0; i < parentStack.length; i++) {
    if (parentStack[i]['events'][event.type]) {
      var eventObj = parentStack[i]['events'][event.type],
        options = eventObj['options'];
      if (options['preventDefault']) event.preventDefault();
      eventObj['runEvent'](event);
      if (options['stopPropagation']) break;
    }
  }
};
window['objectToJSON'] = function objectToJSON (at, obj) {
  if (typeof at[0] == 'object') {
    var ret = [];
    for (var i = 0; i < at.length; i++)
      ret.push(window['objectToJSON'](at[i], obj));
    return ret;
  }
  for (var i in at) obj = obj[at[i]];
  var newObj;
  if (obj instanceof Array || ('length' in obj && obj['localName'] !== 'select')) {
    newObj = [];
    for (var i = 0; i < obj.length; i++)
      newObj.push(window['objectToJSON']([], obj[i]));
    return newObj;
  }
  newObj = {};
  for (var i in getAllPropertyNames(obj)){
    if ((obj['localName'] === 'input') && (i === 'selectionDirection' || i === 'selectionStart' || i === 'selectionEnd'))
      continue;
    if (typeof obj[i] == 'string' || typeof obj[i] == 'number' || typeof obj[i] == 'boolean')
      newObj[i] = obj[i];
  }
  return newObj;
};
function getAllPropertyNames(obj) {
  var props = {}, i = 0;
  do {
    var names = Object.getOwnPropertyNames(obj);
    for (i = 0; i < names.length; i++) {
      props [names[i]] = null;
    }
  } while (obj = Object.getPrototypeOf(obj));
  return props;
};
window = typeof window === 'undefined' ? {} : window;
window['collapseSiblingTextNodes'] = function collapseSiblingTextNodes(vs) {
  if (!vs) { return []; }
  var ax = 0, adjusted = vs.length > 0 ? [vs[0]] : [];
  for (var ix = 1; ix < vs.length; ix++) {
    if (adjusted[ax]['type'] === 'vtext' && vs[ix]['type'] === 'vtext') {
      adjusted[ax]['text'] += vs[ix]['text'];
      continue;
    }
    adjusted[++ax] = vs[ix];
  }
  return adjusted;
}
window['copyDOMIntoVTree'] = function copyDOMIntoVTree(logLevel,mountPoint, vtree, doc) {
  if (!doc) { doc = window.document; }
  var mountChildIdx = 0, node;
  if (!mountPoint) {
    if (doc.body.childNodes.length > 0) {
      node = doc.body.firstChild;
    } else {
      node = doc.body.appendChild (doc.createElement('div'));
    }
  } else if (mountPoint.childNodes.length === 0) {
    node = mountPoint.appendChild (doc.createElement('div'));
  } else {
    while (mountPoint.childNodes[mountChildIdx] && (mountPoint.childNodes[mountChildIdx].nodeType === Node.TEXT_NODE || mountPoint.childNodes[mountChildIdx].localName === 'script')){
      mountChildIdx++;
    }
    if (!mountPoint.childNodes[mountChildIdx]) {
      node = doc.body.appendChild (doc.createElement('div'));
    } else {
      node = mountPoint.childNodes[mountChildIdx];
    }
  }
  if (!window['walk'](logLevel,vtree, node, doc)) {
    if (logLevel) {
      console.warn('Could not copy DOM into virtual DOM, falling back to diff');
    }
    while (node.firstChild) node.removeChild(node.lastChild);
    vtree['domRef'] = node;
    window['populate'](null, vtree, doc);
    return false;
  }
  if (logLevel) {
    console.info ('Successfully prendered page');
  }
  return true;
}
window['diagnoseError'] = function diagnoseError(logLevel, vtree, node) {
  if (logLevel) console.warn('VTree differed from node', vtree, node);
}
window['walk'] = function walk(logLevel, vtree, node, doc) {
  var vdomChild,
    domChild;
  vtree['domRef'] = node;
  window['callCreated'](vtree);
  vtree.children = window['collapseSiblingTextNodes'](vtree.children);
  for (var i = 0; i < vtree.children.length; i++) {
    vdomChild = vtree['children'][i];
    domChild = node.childNodes[i];
    if (!domChild) {
      window['diagnoseError'](logLevel,vdomChild, domChild);
      return false;
    }
    if (vdomChild.type === 'vtext') {
      if (domChild.nodeType !== Node.TEXT_NODE) {
        window['diagnoseError'](logLevel, vdomChild, domChild);
        return false;
      }
      if (vdomChild['text'] === domChild.textContent) {
        vdomChild['domRef'] = domChild;
      } else {
        window['diagnoseError'](logLevel, vdomChild, domChild);
        return false;
      }
    } else {
      if (domChild.nodeType !== Node.ELEMENT_NODE) return false;
      vdomChild['domRef'] = domChild;
      if(!window['walk'](logLevel, vdomChild, domChild, doc)) return false;
    }
  }
  return true;
}
window = typeof window === 'undefined' ? {} : window;
window['callFocus'] = function callFocus(id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.focus) ele.focus()
  }, 50);
}
window['callBlur'] = function callBlur(id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.blur) ele.blur()
  }, 50);
}
function h$fromArray(a) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var i=a.length-1;i>=0;i--) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return a;
}
function h$fromArrayNoWrap(a) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var i=a.length-1;i>=0;i--) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return a;
}
function h$listToArray(xs) {
    var a = [], i = 0;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 a[i++] = ((((xs).d1)).d1);
 xs = ((xs).d2);
    }
    return a;
}
function h$listToArrayWrap(xs) {
    return (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (h$listToArray(xs))));
}
function h$animationFrameCancel(h) {
    if(h.handle) window.cancelAnimationFrame(h.handle);
    if(h.callback) {
        h$release(h.callback)
        h.callback = null;
    }
}
function h$animationFrameRequest(h) {
    h.handle = window.requestAnimationFrame(function(ts) {
        var cb = h.callback;
        if(cb) {
         h$release(cb);
         h.callback = null;
         cb(ts);
        }
    });
}
function h$exportValue(fp1a,fp1b,fp2a,fp2b,o) {
  var e = { fp1a: fp1a
          , fp1b: fp1b
          , fp2a: fp2a
          , fp2b: fp2b
          , released: false
          , root: o
          , _key: -1
          };
  h$retain(e);
  return e;
}
function h$derefExport(fp1a,fp1b,fp2a,fp2b,e) {
  if(!e || typeof e !== 'object') return null;
  if(e.released) return null;
  if(fp1a !== e.fp1a || fp1b !== e.fp1b ||
     fp2a !== e.fp2a || fp2b !== e.fp2b) return null;
  return e.root;
}
function h$releaseExport(e) {
  h$release(e);
  e.released = true;
  e.root = null;
}
var h$jsstringEmpty = (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, ('')));
var h$jsstringHead, h$jsstringTail, h$jsstringCons,
    h$jsstringSingleton, h$jsstringSnoc, h$jsstringUncons,
    h$jsstringIndex, h$jsstringUncheckedIndex;
var h$fromCodePoint;
if(String.prototype.fromCodePoint) {
    h$fromCodePoint = String.fromCodePoint;
} else {
    h$fromCodePoint =
      (function() {
          var stringFromCharCode = String.fromCharCode;
          var floor = Math.floor;
          return function(_) {
              var MAX_SIZE = 0x4000;
              var codeUnits = [];
              var highSurrogate;
              var lowSurrogate;
              var index = -1;
              var length = arguments.length;
              if (!length) {
                  return '';
              }
              var result = '';
              while (++index < length) {
                  var codePoint = Number(arguments[index]);
                  if (
                      !isFinite(codePoint) ||
                      codePoint < 0 ||
                      codePoint > 0x10FFFF ||
                      floor(codePoint) != codePoint
                  ) {
                      throw RangeError('Invalid code point: ' + codePoint);
                  }
                  if (codePoint <= 0xFFFF) {
                      codeUnits.push(codePoint);
                  } else {
                      codePoint -= 0x10000;
                      highSurrogate = (codePoint >> 10) + 0xD800;
                      lowSurrogate = (codePoint % 0x400) + 0xDC00;
                      codeUnits.push(highSurrogate, lowSurrogate);
                  }
                  if (index + 1 == length || codeUnits.length > MAX_SIZE) {
                      result += stringFromCharCode.apply(null, codeUnits);
                      codeUnits.length = 0;
                  }
              }
              return result;
          }
      })();
}
if(String.prototype.codePointAt) {
    h$jsstringSingleton = function(ch) {
        ;
 return String.fromCodePoint(ch);
    }
    h$jsstringHead = function(str) {
        ;
 var cp = str.codePointAt(0);
 return (cp === undefined) ? -1 : (cp|0);
    }
    h$jsstringTail = function(str) {
        ;
 var l = str.length;
 if(l===0) return null;
 var ch = str.codePointAt(0);
 if(ch === undefined) return null;
 return str.substr(((ch)>=0x10000)?2:1);
    }
    h$jsstringCons = function(ch, str) {
        ;
 return String.fromCodePoint(ch)+str;
    }
    h$jsstringSnoc = function(str, ch) {
        ;
 return str+String.fromCodePoint(ch);
    }
    h$jsstringUncons = function(str) {
        ;
 var l = str.length;
 if(l===0) {
          { h$ret1 = (null); return (-1); };
        }
 var ch = str.codePointAt(0);
        if(ch === undefined) {
     { h$ret1 = (null); return (-1); };
        }
        { h$ret1 = (str.substr(((ch)>=0x10000)?2:1)); return (ch); };
    }
    h$jsstringIndex = function(i, str) {
        ;
 var ch = str.codePointAt(i);
 if(ch === undefined) return -1;
 return ch;
    }
    h$jsstringUncheckedIndex = function(i, str) {
        ;
 return str.codePointAt(i);
    }
} else {
    h$jsstringSingleton = function(ch) {
        ;
 return (((ch)>=0x10000)) ? String.fromCharCode(((((ch)-0x10000)>>>10)+0xDC00), (((ch)&0x3FF)+0xD800))
                               : String.fromCharCode(ch);
    }
    h$jsstringHead = function(str) {
        ;
 var l = str.length;
 if(l===0) return -1;
 var ch = str.charCodeAt(0);
 if(((ch|1023)===0xDBFF)) {
     return (l>1) ? ((((ch)-0xD800)<<10)+(str.charCodeAt(1))-9216) : -1;
 } else {
     return ch;
 }
    }
    h$jsstringTail = function(str) {
        ;
 var l = str.length;
 if(l===0) return null;
 var ch = str.charCodeAt(0);
 if(((ch|1023)===0xDBFF)) {
     return (l>1)?str.substr(2):null;
 } else return str.substr(1);
    }
    h$jsstringCons = function(ch, str) {
        ;
 return ((((ch)>=0x10000)) ? String.fromCharCode(((((ch)-0x10000)>>>10)+0xDC00), (((ch)&0x3FF)+0xD800))
                                : String.fromCharCode(ch))
                                + str;
    }
    h$jsstringSnoc = function(str, ch) {
        ;
 return str + ((((ch)>=0x10000)) ? String.fromCharCode(((((ch)-0x10000)>>>10)+0xDC00), (((ch)&0x3FF)+0xD800))
                                      : String.fromCharCode(ch));
    }
    h$jsstringUncons = function(str) {
        ;
 var l = str.length;
 if(l===0) {
          { h$ret1 = (null); return (-1); };
        }
 var ch = str.charCodeAt(0);
 if(((ch|1023)===0xDBFF)) {
   if(l > 1) {
        { h$ret1 = (str.substr(2)); return (((((ch)-0xD800)<<10)+(str.charCodeAt(1))-9216)); };
   } else {
       { h$ret1 = (null); return (-1); };
   }
 } else {
      { h$ret1 = (str.substr(1)); return (ch); };
 }
    }
    h$jsstringIndex = function(i, str) {
 var ch = str.charCodeAt(i);
 if(ch != ch) return -1;
 return (((ch|1023)===0xDBFF)) ? ((((ch)-0xD800)<<10)+(str.charCodeAt(i+1))-9216) : ch;
    }
    h$jsstringUncheckedIndex = function(i, str) {
        ;
 var ch = str.charCodeAt(i);
 return (((ch|1023)===0xDBFF)) ? ((((ch)-0xD800)<<10)+(str.charCodeAt(i+1))-9216) : ch;
    }
}
function h$jsstringUnsnoc(str) {
  ;
  var l = str.length;
  if(l===0) {
    { h$ret1 = (null); return (-1); };
  }
  var ch = str.charCodeAt(l-1);
  if(((ch|1023)===0xDFFF)) {
    if(l !== 1) {
      { h$ret1 = (str.substr(0,l-2)); return (((((str.charCodeAt(l-2))-0xD800)<<10)+(ch)-9216)); };
    } else {
      { h$ret1 = (null); return (-1); };
    }
  } else {
    { h$ret1 = (str.substr(0,l-1)); return (ch); };
  }
}
function h$jsstringPack(xs) {
    var r = '', i = 0, a = [], c;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 c = ((xs).d1);
 a[i++] = ((typeof(c) === 'number')?(c):(c).d1);
 if(i >= 60000) {
     r += h$fromCodePoint.apply(null, a);
     a = [];
     i = 0;
 }
 xs = ((xs).d2);
    }
    if(i > 0) r += h$fromCodePoint.apply(null, a);
    ;
    return r;
}
function h$jsstringPackReverse(xs) {
    var a = [], i = 0, c;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 c = ((xs).d1);
 a[i++] = ((typeof(c) === 'number')?(c):(c).d1);
 xs = ((xs).d2);
    }
    if(i===0) return '';
    var r = h$jsstringConvertArray(a.reverse());
    ;
    return r;
}
function h$jsstringPackArray(arr) {
    ;
    return h$jsstringConvertArray(arr);
}
function h$jsstringPackArrayReverse(arr) {
    ;
    return h$jsstringConvertArray(arr.reverse());
}
function h$jsstringConvertArray(arr) {
    if(arr.length < 60000) {
 return h$fromCodePoint.apply(null, arr);
    } else {
 var r = '';
 for(var i=0; i<arr.length; i+=60000) {
     r += h$fromCodePoint.apply(null, arr.slice(i, i+60000));
 }
 return r;
    }
}
function h$jsstringInit(str) {
    ;
    var l = str.length;
    if(l===0) return null;
    var ch = str.charCodeAt(l-1);
    var o = ((ch|1023)===0xDFFF)?2:1;
    var r = str.substr(0, l-o);
    return r;
}
function h$jsstringLast(str) {
    ;
    var l = str.length;
    if(l===0) return -1;
    var ch = str.charCodeAt(l-1);
    if(((ch|1023)===0xDFFF)) {
 return (l>1) ? ((((str.charCodeAt(l-2))-0xD800)<<10)+(ch)-9216) : -1;
    } else return ch;
}
function h$jsstringIndexR(i, str) {
    ;
    if(i < 0 || i > str.length) return -1;
    var ch = str.charCodeAt(i);
    return (((ch|1023)===0xDFFF)) ? ((((str.charCodeAt(i-1))-0xD800)<<10)+(ch)-9216) : ch;
}
function h$jsstringNextIndex(i, str) {
    ;
    return i + (((str.charCodeAt(i)|1023)===0xDBFF)?2:1);
}
function h$jsstringTake(n, str) {
    ;
    if(n <= 0) return '';
    var i = 0, l = str.length, ch;
    if(n >= l) return str;
    while(n--) {
 ch = str.charCodeAt(i++);
 if(((ch|1023)===0xDBFF)) i++;
 if(i >= l) return str;
    }
    return str.substr(0,i);
}
function h$jsstringDrop(n, str) {
    ;
    if(n <= 0) return str;
    var i = 0, l = str.length, ch;
    if(n >= l) return '';
    while(n--) {
 ch = str.charCodeAt(i++);
 if(((ch|1023)===0xDBFF)) i++;
 if(i >= l) return str;
    }
    return str.substr(i);
}
function h$jsstringSplitAt(n, str) {
  ;
  if(n <= 0) {
    { h$ret1 = (str); return (""); };
  } else if(n >= str.length) {
    { h$ret1 = (""); return (str); };
  }
  var i = 0, l = str.length, ch;
  while(n--) {
    ch = str.charCodeAt(i++);
    if(((ch|1023)===0xDBFF)) i++;
    if(i >= l) {
      { h$ret1 = (""); return (str); };
    }
  }
  { h$ret1 = (str.substr(i)); return (str.substr(0,i)); };
}
function h$jsstringTakeEnd(n, str) {
    ;
    if(n <= 0) return '';
    var l = str.length, i = l-1, ch;
    if(n >= l) return str;
    while(n-- && i > 0) {
 ch = str.charCodeAt(i--);
 if(((ch|1023)===0xDFFF)) i--;
    }
    return (i<0) ? str : str.substr(i+1);
}
function h$jsstringDropEnd(n, str) {
    ;
    if(n <= 0) return str;
    var l = str.length, i = l-1, ch;
    if(n >= l) return '';
    while(n-- && i > 0) {
 ch = str.charCodeAt(i--);
 if(((ch|1023)===0xDFFF)) i--;
    }
    return (i<0) ? '' : str.substr(0,i+1);
}
function h$jsstringIntercalate(x, ys) {
    ;
    var a = [], i = 0;
    while(((ys).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 if(i) a[i++] = x;
 a[i++] = ((((ys).d1)).d1);
 ys = ((ys).d2);
    }
    return a.join('');
}
function h$jsstringIntersperse(ch, ys) {
    ;
    var i = 0, l = ys.length, j = 0, a = [], ych;
    if(((ch)>=0x10000)) {
 var ch1 = ((((ch)-0x10000)>>>10)+0xDC00), ch2 = (((ch)&0x3FF)+0xD800);
 while(j < l) {
     if(i) {
  a[i++] = ch1;
  a[i++] = ch2;
     }
     ych = ys.charCodeAt(j++);
     a[i++] = ych;
     if(((ych|1023)===0xDBFF)) a[i++] = ys.charCodeAt(j++);
 }
    } else {
 while(j < l) {
     if(i) a[i++] = ch;
     ych = ys.charCodeAt(j++);
     a[i++] = ych;
     if(((ych|1023)===0xDBFF)) a[i++] = ys.charCodeAt(j++);
 }
    }
    return h$jsstringConvertArray(a);
}
function h$jsstringConcat(xs) {
    ;
    var a = [], i = 0;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 a[i++] = ((((xs).d1)).d1);
 xs = ((xs).d2);
    }
    return a.join('');
}
var h$jsstringStripPrefix, h$jsstringStripSuffix,
    h$jsstringIsPrefixOf, h$jsstringIsSuffixOf,
    h$jsstringIsInfixOf;
if(String.prototype.startsWith) {
    h$jsstringStripPrefix = function(p, x) {
 ;
 if(x.startsWith(p)) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(p.length)))))));
 } else {
     return h$baseZCGHCziMaybeziNothing;
 }
    }
    h$jsstringIsPrefixOf = function(p, x) {
 ;
 return x.startsWith(p);
    }
} else {
    h$jsstringStripPrefix = function(p, x) {
 ;
 if(x.indexOf(p) === 0) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(p.length)))))));
 } else {
   return h$baseZCGHCziMaybeziNothing;
 }
    }
    h$jsstringIsPrefixOf = function(p, x) {
 ;
 return x.indexOf(p) === 0;
    }
}
if(String.prototype.endsWith) {
    h$jsstringStripSuffix = function(s, x) {
 ;
 if(x.endsWith(s)) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,x.length-s.length)))))));
 } else {
   return h$baseZCGHCziMaybeziNothing;
 }
    }
    h$jsstringIsSuffixOf = function(s, x) {
 ;
 return x.endsWith(s);
    }
} else {
    h$jsstringStripSuffix = function(s, x) {
 ;
 var i = x.lastIndexOf(s);
 var l = x.length - s.length;
 if(i !== -1 && i === l) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,l)))))));
 } else {
   return h$baseZCGHCziMaybeziNothing;
 }
    }
      h$jsstringIsSuffixOf = function(s, x) {
 ;
        var i = x.lastIndexOf(s);
 return i !== -1 && i === x.length - s.length;
    }
}
if(String.prototype.includes) {
    h$jsstringIsInfixOf = function(i, x) {
        ;
 return x.includes(i);
    }
} else {
    h$jsstringIsInfixOf = function(i, x) {
        ;
 return x.indexOf(i) !== -1;
    }
}
function h$jsstringCommonPrefixes(x, y) {
    ;
    var lx = x.length, ly = y.length, i = 0, cx;
    var l = lx <= ly ? lx : ly;
    if(lx === 0 || ly === 0 || x.charCodeAt(0) !== y.charCodeAt(0)) {
      return h$baseZCGHCziMaybeziNothing;
    }
    while(++i<l) {
 cx = x.charCodeAt(i);
 if(cx !== y.charCodeAt(i)) {
     if(((cx|1023)===0xDFFF)) i--;
     break;
 }
    }
  if(i===0) return h$baseZCGHCziMaybeziNothing;
    return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c3(h$ghczmprimZCGHCziTupleziZLz2cUz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, ((i===lx)?x:((i===ly)?y:x.substr(0,i)))))),((i===lx) ? h$jsstringEmpty : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(i))))),((i===ly) ? h$jsstringEmpty : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (y.substr(i))))))))));
}
function h$jsstringBreakOn(b, x) {
    ;
    var i = x.indexOf(b);
    if(i===-1) {
        { h$ret1 = (""); return (x); };
    }
    if(i===0) {
        { h$ret1 = (x); return (""); };
    }
    { h$ret1 = (x.substr(i)); return (x.substr(0,i)); };
}
function h$jsstringBreakOnEnd(b, x) {
    ;
    var i = x.lastIndexOf(b);
  if(i===-1) {
    { h$ret1 = (x); return (""); };
    }
  i += b.length;
    { h$ret1 = (x.substr(i)); return (x.substr(0,i)); };
}
function h$jsstringBreakOnAll1(n, b, x) {
    ;
    var i = x.indexOf(b, n);
    if(i===0) {
       { h$ret1 = (""); h$ret2 = (x); return (b.length); };
    }
    if(i===-1) {
       { h$ret1 = (null); h$ret2 = (null); return (-1); };
    }
    { h$ret1 = (x.substr(0,i)); h$ret2 = (x.substr(i)); return (i+b.length); };
}
function h$jsstringBreakOnAll(pat, src) {
    ;
    var a = [], i = 0, n = 0, r = h$ghczmprimZCGHCziTypesziZMZN, pl = pat.length;
    while(true) {
 var x = src.indexOf(pat, n);
 if(x === -1) break;
 a[i++] = (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (src.substr(0,x))))),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (src.substr(x)))))));
 n = x + pl;
    }
    while(--i >= 0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return r;
}
function h$jsstringSplitOn1(n, p, x) {
    ;
    var i = x.indexOf(p, n);
    if(i === -1) {
        { h$ret1 = (null); return (-1); };
    }
    var r1 = (i==n) ? "" : x.substr(n, i-n);
    { h$ret1 = (r1); return (i + p.length); };
}
function h$jsstringSplitOn(p, x) {
    ;
    var a = x.split(p);
    var r = h$ghczmprimZCGHCziTypesziZMZN, i = a.length;
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return r;
}
function h$jsstringWords1(n, x) {
    ;
    var m = n, s = n, l = x.length;
    if(m >= l) return -1;
    do {
 if(m >= l) return -1;
    } while(h$isSpace(x.charCodeAt(m++)));
    s = m - 1;
    while(m < l) {
 if(h$isSpace(x.charCodeAt(m++))) {
            var r1 = (m-s<=1) ? "" : x.substr(s,m-s-1);
            { h$ret1 = (r1); return (m); };
 }
    }
    if(s < l) {
        var r1 = s === 0 ? x : x.substr(s);
        { h$ret1 = (r1); return (m); };
    }
    { h$ret1 = (null); return (-1); };
}
function h$jsstringWords(x) {
    ;
    var a = null, i = 0, n, s = -1, m = 0, w, l = x.length, r = h$ghczmprimZCGHCziTypesziZMZN;
    outer:
    while(m < l) {
 do {
     if(m >= l) { s = m; break outer; }
 } while(h$isSpace(x.charCodeAt(m++)));
 s = m - 1;
 while(m < l) {
     if(h$isSpace(x.charCodeAt(m++))) {
  w = (m-s<=1) ? h$jsstringEmpty
                             : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(s,m-s-1))));
  if(i) a[i++] = w; else { a = [w]; i = 1; }
  s = m;
  break;
     }
 }
    }
    if(s !== -1 && s < l) {
 w = (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (s === 0 ? x : x.substr(s))));
 if(i) a[i++] = w; else { a = [w]; i = 1; }
    }
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return r;
}
function h$jsstringLines1(n, x) {
    ;
    var m = n, l = x.length;
    if(n >= l) return -1;
    while(m < l) {
 if(x.charCodeAt(m++) === 10) {
     if(n > 0 && n === l-1) return -1;
            var r1 = (m-n<=1) ? "" : x.substr(n,m-n-1);
            { h$ret1 = (r1); return (m); };
 }
    }
    { h$ret1 = (x.substr(n)); return (m); };
}
function h$jsstringLines(x) {
    ;
    var a = null, m = 0, i = 0, l = x.length, s = 0, r = h$ghczmprimZCGHCziTypesziZMZN, w;
    if(l === 0) return h$ghczmprimZCGHCziTypesziZMZN;
    outer:
    while(true) {
 s = m;
 do {
     if(m >= l) break outer;
 } while(x.charCodeAt(m++) !== 10);
 w = (m-s<=1) ? h$jsstringEmpty : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(s,m-s-1))));
 if(i) a[i++] = w; else { a = [w]; i = 1; }
    }
    if(s < l) {
 w = (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(s))));
 if(i) a[i++] = w; else { a = [w]; i = 1; }
    }
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return r;
}
function h$jsstringGroup(x) {
    ;
    var xl = x.length;
    if(xl === 0) return h$ghczmprimZCGHCziTypesziZMZN;
    var i = xl-1, si, ch, s=xl, r=h$ghczmprimZCGHCziTypesziZMZN;
    var tch = x.charCodeAt(i--);
    if(((tch|1023)===0xDFFF)) tch = ((((x.charCodeAt(i--))-0xD800)<<10)+(tch)-9216);
    while(i >= 0) {
 si = i;
 ch = x.charCodeAt(i--);
 if(((ch|1023)===0xDFFF)) {
     ch = ((((x.charCodeAt(i--))-0xD800)<<10)+(ch)-9216);
 }
 if(ch != tch) {
     tch = ch;
     r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(si+1,s-si))))), (r)));
     s = si;
 }
    }
    return (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,s+1))))), (r)));
}
function h$jsstringChunksOf1(n, s, x) {
    ;
    var m = s, c = 0, l = x.length, ch;
    if(n <= 0 || l === 0 || s >= l) return -1
    while(++m < l && ++c < n) {
 ch = x.charCodeAt(m);
 if(((ch|1023)===0xDBFF)) ++m;
    }
    var r1 = (m >= l && s === c) ? x : x.substr(s,m-s);
    { h$ret1 = (r1); return (m); };
}
function h$jsstringChunksOf(n, x) {
    ;
    var l = x.length;
    if(l===0 || n <= 0) return h$ghczmprimZCGHCziTypesziZMZN;
    if(l <= n) return (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x)))), (h$ghczmprimZCGHCziTypesziZMZN)));
    var a = [], i = 0, s = 0, ch, m = 0, c, r = h$ghczmprimZCGHCziTypesziZMZN;
    while(m < l) {
 s = m;
 c = 0;
 while(m < l && ++c <= n) {
     ch = x.charCodeAt(m++);
     if(((ch|1023)===0xDBFF)) ++m;
 }
 if(c) a[i++] = x.substr(s, m-s);
    }
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return r;
}
function h$jsstringCount(pat, src) {
    ;
    var i = 0, n = 0, pl = pat.length, sl = src.length;
    while(i<sl) {
 i = src.indexOf(pat, i);
 if(i===-1) break;
 n++;
 i += pl;
    }
    return n;
}
function h$jsstringReplicate(n, str) {
    ;
    if(n === 0 || str == '') return '';
    if(n === 1) return str;
    var r = '';
    do {
 if(n&1) r+=str;
        str+=str;
        n >>= 1;
    } while(n > 1);
    return r+str;
}
var h$jsstringReverse;
if(Array.from) {
    h$jsstringReverse = function(str) {
 ;
 return Array.from(str).reverse().join('');
    }
} else {
    h$jsstringReverse = function(str) {
 ;
 var l = str.length, a = [], o = 0, i = 0, c, c1, s = '';
 while(i < l) {
     c = str.charCodeAt(i);
     if(((c|1023)===0xDBFF)) {
  a[i] = str.charCodeAt(i+1);
  a[i+1] = c;
  i += 2;
     } else a[i++] = c;
     if(i-o > 60000) {
  s = String.fromCharCode.apply(null, a.reverse()) + s;
  o = -i;
  a = [];
     }
 }
 return (i===0) ? s : String.fromCharCode.apply(null,a.reverse()) + s;
    }
}
function h$jsstringUnpack(str) {
    ;
    var r = h$ghczmprimZCGHCziTypesziZMZN, i = str.length-1, c;
    while(i >= 0) {
 c = str.charCodeAt(i--);
 if(((c|1023)===0xDFFF)) c = ((((str.charCodeAt(i--))-0xD800)<<10)+(c)-9216)
 r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (c), (r)));
    }
    return r;
}
function h$jsstringDecInteger(val) {
  ;
  if(((val).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    return '' + ((val).d1);
  } else if(((val).f === h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e)) {
    return h$ghcjsbn_showBase(((val).d1), 10);
  } else {
    return '-' + h$ghcjsbn_showBase(((val).d1), 10);
  }
}
function h$jsstringDecI64(hi,lo) {
    ;
    var lo0 = (lo < 0) ? lo+4294967296:lo;
    if(hi < 0) {
 if(hi === -1) return ''+(lo0-4294967296);
 lo0 = 4294967296 - lo0;
 var hi0 = -1 - hi;
 var x0 = hi0 * 967296;
 var x1 = (lo0 + x0) % 1000000;
 var x2 = hi0*4294+Math.floor((x0+lo0-x1)/1000000);
 return '-' + x2 + h$jsstringDecIPadded6(x1);
    } else {
 if(hi === 0) return ''+lo0;
 var x0 = hi * 967296;
 var x1 = (lo0 + x0) % 1000000;
 var x2 = hi*4294+Math.floor((x0+lo0-x1)/1000000);
 return '' + x2 + h$jsstringDecIPadded6(x1);
    }
}
function h$jsstringDecW64(hi,lo) {
    ;
    var lo0 = (lo < 0) ? lo+4294967296 : lo;
    if(hi === 0) return ''+lo0;
    var hi0 = (hi < 0) ? hi+4294967296 : hi;
    var x0 = hi0 * 967296;
    var x1 = (lo0 + x0) % 1000000;
    var x2 = hi0*4294+Math.floor((x0+lo0-x1)/1000000);
    return '' + x2 + h$jsstringDecIPadded6(x1);
}
function h$jsstringHexInteger(val) {
  ;
  if(((val).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    return '' + ((val).d1);
  } else {
    return h$ghcjsbn_showBase(((val).d1), 16);
  }
}
function h$jsstringHexI64(hi,lo) {
    var lo0 = lo<0 ? lo+4294967296 : lo;
    if(hi === 0) return lo0.toString(16);
    return ((hi<0)?hi+4294967296:hi).toString(16) + h$jsstringHexIPadded8(lo0);
}
function h$jsstringHexW64(hi,lo) {
    var lo0 = lo<0 ? lo+4294967296 : lo;
    if(hi === 0) return lo0.toString(16);
    return ((hi<0)?hi+4294967296:hi).toString(16) + h$jsstringHexIPadded8(lo0);
}
function h$jsstringDecIPadded9(n) {
    ;
    if(n === 0) return '000000000';
    var pad = (n>=100000000)?'':
              (n>=10000000)?'0':
              (n>=1000000)?'00':
              (n>=100000)?'000':
              (n>=10000)?'0000':
              (n>=1000)?'00000':
              (n>=100)?'000000':
              (n>=10)?'0000000':
                     '00000000';
    return pad+n;
}
function h$jsstringDecIPadded6(n) {
    ;
    if(n === 0) return '000000';
    var pad = (n>=100000)?'':
              (n>=10000)?'0':
              (n>=1000)?'00':
              (n>=100)?'000':
              (n>=10)?'0000':
                     '00000';
    return pad+n;
}
function h$jsstringHexIPadded8(n) {
    ;
   if(n === 0) return '00000000';
   var pad = (n>=0x10000000)?'':
             (n>=0x1000000)?'0':
             (n>=0x100000)?'00':
             (n>=0x10000)?'000':
             (n>=0x1000)?'0000':
             (n>=0x100)?'00000':
             (n>=0x10)?'000000':
                      '0000000';
    return pad+n.toString(16);
}
function h$jsstringZeroes(n) {
    var r;
    switch(n&7) {
 case 0: r = ''; break;
 case 1: r = '0'; break;
 case 2: r = '00'; break;
 case 3: r = '000'; break;
 case 4: r = '0000'; break;
 case 5: r = '00000'; break;
 case 6: r = '000000'; break;
 case 7: r = '0000000';
    }
    for(var i=n>>3;i>0;i--) r = r + '00000000';
    return r;
}
function h$jsstringDoubleToFixed(decs, d) {
    if(decs >= 0) {
 if(Math.abs(d) < 1e21) {
     var r = d.toFixed(Math.min(20,decs));
     if(decs > 20) r = r + h$jsstringZeroes(decs-20);
     return r;
 } else {
     var r = d.toExponential();
     var ei = r.indexOf('e');
     var di = r.indexOf('.');
     var e = parseInt(r.substr(ei+1));
     return r.substring(0,di) + r.substring(di,ei) + h$jsstringZeroes(di-ei+e) +
                   ((decs > 0) ? ('.' + h$jsstringZeroes(decs)) : '');
 }
    }
    var r = Math.abs(d).toExponential();
    var ei = r.indexOf('e');
    var e = parseInt(r.substr(ei+1));
    var m = d < 0 ? '-' : '';
    r = r.substr(0,1) + r.substring(2,ei);
    if(e >= 0) {
 return (e > r.length) ? m + r + h$jsstringZeroes(r.length-e-1) + '.0'
                       : m + r.substr(0,e+1) + '.' + r.substr(e+1);
    } else {
 return m + '0.' + h$jsstringZeroes(-e-1) + r;
    }
}
function h$jsstringDoubleToExponent(decs, d) {
    var r;
    if(decs ===-1) {
 r = d.toExponential().replace('+','');
    } else {
 r = d.toExponential(Math.max(1, Math.min(20,decs))).replace('+','');
    }
    if(r.indexOf('.') === -1) {
 r = r.replace('e', '.0e');
    }
    if(decs > 20) r = r.replace('e', h$jsstringZeroes(decs-20)+'e');
    return r;
}
function h$jsstringDoubleGeneric(decs, d) {
    var r;
    if(decs === -1) {
 r = d.toString(10).replace('+','');
    } else {
 r = d.toPrecision(Math.max(decs+1,1)).replace('+','');
    }
    if(decs !== 0 && r.indexOf('.') === -1) {
 if(r.indexOf('e') !== -1) {
     r = r.replace('e', '.0e');
 } else {
     r = r + '.0';
 }
    }
    return r;
}
function h$jsstringAppend(x, y) {
    ;
    return x+y;
}
function h$jsstringCompare(x, y) {
    ;
    return (x<y)?-1:((x>y)?1:0);
}
function h$jsstringUnlines(xs) {
    var r = '';
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 r = r + ((((xs).d1)).d1) + '\n';
 xs = ((xs).d2);
    }
    return r;
}
function h$jsstringUnwords(xs) {
    if(((xs).f === h$ghczmprimZCGHCziTypesziZMZN_con_e)) return '';
    var r = ((((xs).d1)).d1);
    xs = ((xs).d2);
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 r = r + ' ' + ((((xs).d1)).d1);
 xs = ((xs).d2);
    }
    return r;
}
function h$jsstringReplace(pat, rep, src) {
    ;
    var r = src.replace(pat, rep, 'g');
    if(r.indexOf(pat) !== -1) {
 r = src.split(pat).join(rep);
    }
    return r;
}
function h$jsstringReplicateChar(n, ch) {
    ;
    return h$jsstringReplicate(n, h$jsstringSingleton(ch));
}
function h$jsstringIsInteger(str) {
    return /^-?\d+$/.test(str);
}
function h$jsstringIsNatural(str) {
    return /^\d+$/.test(str);
}
function h$jsstringReadInt(str) {
    if(!/^-?\d+/.test(str)) return null;
    var x = parseInt(str, 10);
    var x0 = x|0;
    return (x===x0) ? x0 : null;
}
function h$jsstringLenientReadInt(str) {
    var x = parseInt(str, 10);
    var x0 = x|0;
    return (x===x0) ? x0 : null;
}
function h$jsstringReadWord(str) {
  if(!/^\d+/.test(str)) return null;
  var x = parseInt(str, 10);
  var x0 = x|0;
  if(x0<0) return (x===x0+2147483648) ? x0 : null;
  else return (x===x0) ? x0 : null;
}
function h$jsstringReadDouble(str) {
    return parseFloat(str, 10);
}
function h$jsstringLenientReadDouble(str) {
    return parseFloat(str, 10);
}
function h$jsstringReadInteger(str) {
  ;
  if(!/^(-)?\d+$/.test(str)) {
    return null;
  } else if(str.length <= 9) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (parseInt(str, 10))));;
  } else {
    return h$ghcjsbn_readInteger(str);
  }
}
function h$jsstringReadInt64(str) {
  if(!/^(-)?\d+$/.test(str)) {
      { h$ret1 = (0); h$ret2 = (0); return (0); };
  }
  if(str.charCodeAt(0) === 45) {
    return h$jsstringReadValue64(str, 1, true);
  } else {
    return h$jsstringReadValue64(str, 0, false);
  }
}
function h$jsstringReadWord64(str) {
  if(!/^\d+$/.test(str)) {
    { h$ret1 = (0); h$ret2 = (0); return (0); };
  }
  return h$jsstringReadValue64(str, 0, false);
}
var h$jsstringLongs = null;
function h$jsstringReadValue64(str, start, negate) {
  var l = str.length, i = start;
  while(i < l) {
    if(str.charCodeAt(i) !== 48) break;
    i++;
  }
  if(i >= l) { h$ret1 = (0); h$ret2 = (0); return (1); };
  if(h$jsstringLongs === null) {
    h$jsstringLongs = [];
    for(var t=10; t<=1000000000; t*=10) {
      h$jsstringLongs.push(goog.math.Long.fromInt(t));
    }
  }
  var li = l-i;
  if(li < 10 && !negate) {
    { h$ret1 = (0); h$ret2 = (parseInt(str.substr(i), 10)); return (1); };
  }
  var r = goog.math.Long.fromInt(parseInt(str.substr(li,9),10));
  li += 9;
  while(li < l) {
    r = r.multiply(h$jsstringLongs[Math.min(l-li-1,8)])
         .add(goog.math.Long.fromInt(parseInt(str.substr(li,9), 10)));
    li += 9;
  }
  if(negate) {
    r = r.negate();
  }
  { h$ret1 = (r.getHighBits()); h$ret2 = (r.getLowBits()); return (1); };
}
function h$jsstringExecRE(i, str, re) {
    re.lastIndex = i;
    var m = re.exec(str);
    if(m === null) return -1;
    var a = [], x, j = 1, r = h$ghczmprimZCGHCziTypesziZMZN;
    while(true) {
 x = m[j];
 if(typeof x === 'undefined') break;
 a[j-1] = x;
 j++;
    }
    j-=1;
    while(--j>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[j])))), (r)));
    { h$ret1 = (m[0]); h$ret2 = (r); return (m.index); };
}
function h$jsstringReplaceRE(pat, replacement, str) {
    return str.replace(pat, replacement);
}
function h$jsstringSplitRE(limit, re, str) {
    re.lastIndex = i;
    var s = (limit < 0) ? str.split(re) : str.split(re, limit);
    var i = s.length, r = h$ghczmprimZCGHCziTypesziZMZN;
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return r;
}
function h$jsstringRawChunksOf(k, x) {
    var l = x.length;
    if(l === 0) return h$ghczmprimZCGHCziTypesziZMZN;
    if(l <= k) return (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x)))), (h$ghczmprimZCGHCziTypesziZMZN)));
    var r=h$ghczmprimZCGHCziTypesziZMZN;
    for(var i=ls-k;i>=0;i-=k) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(i,i+k))))), (r)));
    return r;
}
function h$jsstringRawSplitAt(k, x) {
    if(k === 0) return (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,(h$jsstringEmpty),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x))))));
    if(k >= x.length) return (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x)))),(h$jsstringEmpty)));
    return (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,k))))),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(k)))))));
}
function h$foreignListProps(o) {
    var r = HS_NIL;
    if(typeof o === 'undefined' || o === null) return null;
    throw "h$foreignListProps";
}
function h$textToString(arr, off, len) {
    var a = [];
    var end = off+len;
    var k = 0;
    var u1 = arr.u1;
    var s = '';
    for(var i=off;i<end;i++) {
 var cc = u1[i];
 a[k++] = cc;
 if(k === 60000) {
     s += String.fromCharCode.apply(this, a);
     k = 0;
     a = [];
 }
    }
    return s + String.fromCharCode.apply(this, a);
}
function h$textFromString(s) {
    var l = s.length;
    var b = h$newByteArray(l * 2);
    var u1 = b.u1;
    for(var i=l-1;i>=0;i--) u1[i] = s.charCodeAt(i);
    { h$ret1 = (l); return (b); };
}
function h$lazyTextToString(txt) {
    var s = '';
    while(((txt).f.a === 2)) {
        var head = ((txt));
        s += h$textToString(((head).d1), ((head).d2.d1), ((head).d2.d2));
        txt = ((txt).d2.d3);
    }
    return s;
}
function h$safeTextFromString(x) {
    if(typeof x !== 'string') {
 { h$ret1 = (0); return (null); };
    }
    return h$textFromString(x);
}
function h$allProps(o) {
    var a = [], i = 0;
    for(var p in o) a[i++] = p;
    return a;
}
function h$listProps(o) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var p in o) { r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (p)))), (r))); }
    return r;
}
function h$listAssocs(o) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var p in o) { r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (p)))),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (o[p]))))))), (r))); }
    return r;
}
function h$isNumber(o) {
    return typeof(o) === 'number';
}
function h$isObject(o) {
    return typeof(o) === 'object';
}
function h$isString(o) {
    return typeof(o) === 'string';
}
function h$isSymbol(o) {
    return typeof(o) === 'symbol';
}
function h$isBoolean(o) {
    return typeof(o) === 'boolean';
}
function h$isFunction(o) {
    return typeof(o) === 'function';
}
function h$jsTypeOf(o) {
    var t = typeof(o);
    if(t === 'undefined') return 0;
    if(t === 'object') return 1;
    if(t === 'boolean') return 2;
    if(t === 'number') return 3;
    if(t === 'string') return 4;
    if(t === 'symbol') return 5;
    if(t === 'function') return 6;
    return 7;
}
function h$jsonTypeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            return 5;
        } else if (!o) {
            return 0;
        } else {
            return 6;
        }
    }
}
function h$sendXHR(xhr, d, cont) {
    xhr.addEventListener('error', function () {
 cont(2);
    });
    xhr.addEventListener('abort', function() {
 cont(1);
    });
    xhr.addEventListener('load', function() {
 cont(0);
    });
    if(d) {
 xhr.send(d);
    } else {
 xhr.send();
    }
}
function h$createWebSocket(url, protocols) {
  return new WebSocket(url, protocols);
}
function h$openWebSocket(ws, mcb, ccb, c) {
  if(ws.readyState !== 0) {
    throw new Error("h$openWebSocket: unexpected readyState, socket must be CONNECTING");
  }
  ws.lastError = null;
  ws.onopen = function() {
    if(mcb) {
      ws.onmessage = mcb;
    }
    if(ccb || mcb) {
      ws.onclose = function(ce) {
        if(ws.onmessage) {
          h$release(ws.onmessage);
          ws.onmessage = null;
        }
        if(ccb) {
          h$release(ccb);
          ccb(ce);
        }
      };
    };
    ws.onerror = function(err) {
      ws.lastError = err;
      if(ws.onmessage) {
        h$release(ws.onmessage);
        ws.onmessage = null;
      }
      ws.close();
    };
    c(null);
  };
  ws.onerror = function(err) {
    if(ccb) h$release(ccb);
    if(mcb) h$release(mcb);
    ws.onmessage = null;
    ws.close();
    c(err);
  };
}
function h$closeWebSocket(status, reason, ws) {
  ws.onerror = null;
  if(ws.onmessage) {
    h$release(ws.onmessage);
    ws.onmessage = null;
  }
  ws.close(status, reason);
}
goog.provide('goog.crypt.Hash');
goog.crypt.Hash = function() {
  this.blockSize = -1;
};
goog.crypt.Hash.prototype.reset = goog.abstractMethod;
goog.crypt.Hash.prototype.update = goog.abstractMethod;
goog.crypt.Hash.prototype.digest = goog.abstractMethod;
goog.provide('goog.crypt.Md5');
goog.require('goog.crypt.Hash');
goog.crypt.Md5 = function() {
  goog.crypt.Md5.base(this, 'constructor');
  this.blockSize = 512 / 8;
  this.chain_ = new Array(4);
  this.block_ = new Array(this.blockSize);
  this.blockLength_ = 0;
  this.totalLength_ = 0;
  this.reset();
};
goog.inherits(goog.crypt.Md5, goog.crypt.Hash);
goog.crypt.Md5.prototype.reset = function() {
  this.chain_[0] = 0x67452301;
  this.chain_[1] = 0xefcdab89;
  this.chain_[2] = 0x98badcfe;
  this.chain_[3] = 0x10325476;
  this.blockLength_ = 0;
  this.totalLength_ = 0;
};
goog.crypt.Md5.prototype.compress_ = function(buf, opt_offset) {
  if (!opt_offset) {
    opt_offset = 0;
  }
  var X = new Array(16);
  if (goog.isString(buf)) {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf.charCodeAt(opt_offset++)) |
             (buf.charCodeAt(opt_offset++) << 8) |
             (buf.charCodeAt(opt_offset++) << 16) |
             (buf.charCodeAt(opt_offset++) << 24);
    }
  } else {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf[opt_offset++]) |
             (buf[opt_offset++] << 8) |
             (buf[opt_offset++] << 16) |
             (buf[opt_offset++] << 24);
    }
  }
  var A = this.chain_[0];
  var B = this.chain_[1];
  var C = this.chain_[2];
  var D = this.chain_[3];
  var sum = 0;
  sum = (A + (D ^ (B & (C ^ D))) + X[0] + 0xd76aa478) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[1] + 0xe8c7b756) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[2] + 0x242070db) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[3] + 0xc1bdceee) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[4] + 0xf57c0faf) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[5] + 0x4787c62a) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[6] + 0xa8304613) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[7] + 0xfd469501) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[8] + 0x698098d8) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[9] + 0x8b44f7af) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[10] + 0xffff5bb1) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[11] + 0x895cd7be) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[12] + 0x6b901122) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[13] + 0xfd987193) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[14] + 0xa679438e) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[15] + 0x49b40821) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (C ^ (D & (B ^ C))) + X[1] + 0xf61e2562) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[6] + 0xc040b340) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[11] + 0x265e5a51) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[0] + 0xe9b6c7aa) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[5] + 0xd62f105d) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[10] + 0x02441453) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[15] + 0xd8a1e681) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[4] + 0xe7d3fbc8) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[9] + 0x21e1cde6) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[14] + 0xc33707d6) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[3] + 0xf4d50d87) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[8] + 0x455a14ed) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[13] + 0xa9e3e905) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[2] + 0xfcefa3f8) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[7] + 0x676f02d9) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[12] + 0x8d2a4c8a) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (B ^ C ^ D) + X[5] + 0xfffa3942) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[8] + 0x8771f681) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[11] + 0x6d9d6122) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[14] + 0xfde5380c) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[1] + 0xa4beea44) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[4] + 0x4bdecfa9) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[7] + 0xf6bb4b60) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[10] + 0xbebfbc70) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[13] + 0x289b7ec6) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[0] + 0xeaa127fa) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[3] + 0xd4ef3085) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[6] + 0x04881d05) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[9] + 0xd9d4d039) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[12] + 0xe6db99e5) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[15] + 0x1fa27cf8) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[2] + 0xc4ac5665) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (C ^ (B | (~D))) + X[0] + 0xf4292244) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[7] + 0x432aff97) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[14] + 0xab9423a7) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[5] + 0xfc93a039) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[12] + 0x655b59c3) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[3] + 0x8f0ccc92) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[10] + 0xffeff47d) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[1] + 0x85845dd1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[8] + 0x6fa87e4f) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[15] + 0xfe2ce6e0) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[6] + 0xa3014314) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[13] + 0x4e0811a1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[4] + 0xf7537e82) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[11] + 0xbd3af235) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[2] + 0x2ad7d2bb) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[9] + 0xeb86d391) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  this.chain_[0] = (this.chain_[0] + A) & 0xffffffff;
  this.chain_[1] = (this.chain_[1] + B) & 0xffffffff;
  this.chain_[2] = (this.chain_[2] + C) & 0xffffffff;
  this.chain_[3] = (this.chain_[3] + D) & 0xffffffff;
};
goog.crypt.Md5.prototype.update = function(bytes, opt_length) {
  if (!goog.isDef(opt_length)) {
    opt_length = bytes.length;
  }
  var lengthMinusBlock = opt_length - this.blockSize;
  var block = this.block_;
  var blockLength = this.blockLength_;
  var i = 0;
  while (i < opt_length) {
    if (blockLength == 0) {
      while (i <= lengthMinusBlock) {
        this.compress_(bytes, i);
        i += this.blockSize;
      }
    }
    if (goog.isString(bytes)) {
      while (i < opt_length) {
        block[blockLength++] = bytes.charCodeAt(i++);
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          break;
        }
      }
    } else {
      while (i < opt_length) {
        block[blockLength++] = bytes[i++];
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          break;
        }
      }
    }
  }
  this.blockLength_ = blockLength;
  this.totalLength_ += opt_length;
};
goog.crypt.Md5.prototype.digest = function() {
  var pad = new Array((this.blockLength_ < 56 ?
                       this.blockSize :
                       this.blockSize * 2) - this.blockLength_);
  pad[0] = 0x80;
  for (var i = 1; i < pad.length - 8; ++i) {
    pad[i] = 0;
  }
  var totalBits = this.totalLength_ * 8;
  for (var i = pad.length - 8; i < pad.length; ++i) {
    pad[i] = totalBits & 0xff;
    totalBits /= 0x100;
  }
  this.update(pad);
  var digest = new Array(16);
  var n = 0;
  for (var i = 0; i < 4; ++i) {
    for (var j = 0; j < 32; j += 8) {
      digest[n++] = (this.chain_[i] >>> j) & 0xff;
    }
  }
  return digest;
};
function h$base_access(file, file_off, mode, c) {
    ;
    if(h$isNode) {
        h$fs.stat(fd, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                c(mode & fs.mode);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_chmod(file, file_off, mode, c) {
    ;
    if(h$isNode) {
        h$fs.chmod(h$decodeUtf8z(file, file_off), mode, function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_close(fd, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo) {
        delete h$base_fds[fd];
        if(--fdo.refs < 1) {
          ;
          if(fdo.close) {
            fdo.close(fd, fdo, c);
          } else {
            c(0);
          }
        } else {
          ;
          c(0);
        }
    } else {
        ;
        h$errno = 22;
        c(-1);
    }
}
function h$base_dup(fd, c) {
    h$base_dup2(fd, h$base_fdN--, c);
}
function h$base_dup2(fd, new_fd, c) {
   ;
    var fdo = h$base_fds[fd];
    if(!fdo) {
      ;
      h$errno = 22;
      c(-1);
    } else {
      var new_fdo = h$base_fds[new_fd];
      function f() {
        h$base_fds[new_fd] = fdo;
        fdo.refs++;
        c(new_fd);
      }
      if(new_fdo) {
        ;
        h$base_close(new_fd, f);
      } else {
        f();
      }
    }
}
function h$base_fstat(fd, stat, stat_off, c) {
    ;
    if(h$isNode) {
        h$fs.fstat(fd, function(err, fs) {
            if(err) {
                h$handlErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_isatty(fd) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && typeof fdo.isatty !== 'undefined') {
      if(typeof fdo.isatty === 'function') return fdo.isatty() ? 1 : 0;
      return fdo.isatty ? 1 : 0;
    }
    return 0;
}
function h$base_lseek(fd, pos_1, pos_2, whence, c) {
    ;
    if(h$isNode) {
        var p = goog.math.Long.fromBits(pos_2, pos_1), p1;
        var o = h$base_fds[fd];
        if(!o) {
            h$errno = CONST_BADF;
            c(-1,-1);
        } else {
            switch(whence) {
            case 0:
                o.pos = p.toNumber();
                c(p.getHighBits(), p.getLowBits());
                break;
            case 1:
                o.pos += p.toNumber();
                p1 = goog.math.Long.fromNumber(o.pos);
                c(p1.getHighBits(), p1.getLowBits());
                break;
            case 2:
                h$fs.fstat(fd, function(err, fs) {
                    if(err) {
                        h$setErrno(err);
                        c(-1,-1);
                    } else {
                        o.pos = fs.size + p.toNumber();
                        p1 = goog.math.Long.fromNumber(o.pos);
                        c(p1.getHighBits(), p1.getLowBits());
                    }
                });
                break;
            default:
                h$errno = 22;
                c(-1,-1);
            }
        }
    } else {
        h$unsupported();
        c(-1, -1);
    }
}
function h$base_lstat(file, file_off, stat, stat_off, c) {
    ;
    if(h$isNode) {
        h$fs.lstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_open(file, file_off, how, mode, c) {
    if(h$isNode) {
        var flags, off;
        var fp = h$decodeUtf8z(file, file_off);
        ;
        var acc = how & h$base_o_accmode;
        if(acc === h$base_o_rdonly) {
            flags = h$processConstants['fs']['O_RDONLY'];
        } else if(acc === h$base_o_wronly) {
            flags = h$processConstants['fs']['O_WRONLY'];
        } else {
            flags = h$processConstants['fs']['O_RDWR'];
        }
        off = (how & h$base_o_append) ? -1 : 0;
        flags = flags | ((how & h$base_o_trunc) ? h$processConstants['fs']['O_TRUNC'] : 0)
                      | ((how & h$base_o_creat) ? h$processConstants['fs']['O_CREAT'] : 0)
                      | ((how & h$base_o_excl) ? h$processConstants['fs']['O_EXCL'] : 0)
                      | ((how & h$base_o_append) ? h$processConstants['fs']['O_APPEND'] : 0);
        h$fs.open(fp, flags, mode, function(err, fd) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var f = function(p) {
                    h$base_fds[fd] = { read: h$base_readFile
                                     , write: h$base_writeFile
                                     , close: h$base_closeFile
                                     , fd: fd
                                     , pos: p
                                     , refs: 1
                                     };
                    ;
                    c(fd);
                }
                if(off === -1) {
                    h$fs.stat(fp, function(err, fs) {
                        if(err) h$handleErrnoC(err, -1, 0, c); else f(fs.size);
                    });
                } else {
                    f(0);
                }
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_read(fd, buf, buf_off, n, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && fdo.read) {
        fdo.read(fd, fdo, buf, buf_off, n, c);
    } else {
        h$fs.read(fd, buf.u8, buf_off, n, null, function(err, bytesRead, buf0) {
            h$handleErrnoC(err, -1, bytesRead, c);
        });
    }
}
function h$base_stat(file, file_off, stat, stat_off, c) {
    ;
    if(h$isNode) {
        h$fs.stat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handlErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_umask(mode) {
    ;
    if(h$isNode) return process.umask(mode);
    return 0;
}
function h$base_write(fd, buf, buf_off, n, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && fdo.write) {
        fdo.write(fd, fdo, buf, buf_off, n, c);
    } else {
        h$fs.write(fd, buf.u8, buf_off, n, function(err, bytesWritten, buf0) {
            h$handleErrnoC(err, -1, bytesWritten, c);
        });
    }
}
function h$base_ftruncate(fd, pos_1, pos_2, c) {
    ;
    if(h$isNode) {
        h$fs.ftruncate(fd, goog.math.Long.fromBits(pos_2, pos_1).toNumber(), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_unlink(file, file_off, c) {
    ;
    if(h$isNode) {
        h$fs.unlink(h$decodeUtf8z(file, file_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_getpid() {
    ;
    if(h$isNode) return process.pid;
    return 0;
}
function h$base_link(file1, file1_off, file2, file2_off, c) {
    ;
    if(h$isNode) {
        h$fs.link(h$decodeUtf8z(file1, file1_off), h$decodeUtf8z(file2, file2_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_mkfifo(file, file_off, mode, c) {
    throw "h$base_mkfifo";
}
function h$base_sigemptyset(sigset, sigset_off) {
    return 0;
}
function h$base_sigaddset(sigset, sigset_off, sig) {
    return 0;
}
function h$base_sigprocmask(sig, sigset1, sigset1_off, sigset2, sigset2_off) {
    return 0;
}
function h$base_tcgetattr(attr, termios, termios_off) {
    return 0;
}
function h$base_tcsetattr(attr, val, termios, termios_off) {
    return 0;
}
function h$base_utime(file, file_off, timbuf, timbuf_off, c) {
    ;
    if(h$isNode) {
        h$fs.fstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, 0, -1, c);
            } else {
                var atime = goog.math.Long.fromNumber(fs.atime.getTime());
                var mtime = goog.math.Long.fromNumber(fs.mtime.getTime());
                var ctime = goog.math.Long.fromNumber(fs.ctime.getTime());
                timbuf.i3[0] = atime.getHighBits();
                timbuf.i3[1] = atime.getLowBits();
                timbuf.i3[2] = mtime.getHighBits();
                timbuf.i3[3] = mtime.getLowBits();
                timbuf.i3[4] = ctime.getHighBits();
                timbuf.i3[5] = ctime.getLowBits();
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_waitpid(pid, stat, stat_off, options, c) {
    throw "h$base_waitpid";
}
              var h$base_o_rdonly = 0x00000;
              var h$base_o_wronly = 0x00001;
              var h$base_o_rdwr = 0x00002;
              var h$base_o_accmode = 0x00003;
              var h$base_o_append = 0x00008;
              var h$base_o_creat = 0x00200;
              var h$base_o_trunc = 0x00400;
              var h$base_o_excl = 0x00800;
              var h$base_o_noctty = 0x20000;
              var h$base_o_nonblock = 0x00004;
              var h$base_o_binary = 0x00000;
function h$base_c_s_isreg(mode) {
    return 1;
}
function h$base_c_s_ischr(mode) {
    return 0;
}
function h$base_c_s_isblk(mode) {
    return 0;
}
function h$base_c_s_isdir(mode) {
    return 0;
}
function h$base_c_s_isfifo(mode) {
    return 0;
}
function h$base_fillStat(fs, b, off) {
    if(off%4) throw "h$base_fillStat: not aligned";
    var o = off>>2;
    b.i3[o+0] = fs.mode;
    var s = goog.math.Long.fromNumber(fs.size);
    b.i3[o+1] = s.getHighBits();
    b.i3[o+2] = s.getLowBits();
    b.i3[o+3] = 0;
    b.i3[o+4] = 0;
    b.i3[o+5] = fs.dev;
    var i = goog.math.Long.fromNumber(fs.ino);
    b.i3[o+6] = i.getHighBits();
    b.i3[o+7] = i.getLowBits();
    b.i3[o+8] = fs.uid;
    b.i3[o+9] = fs.gid;
}
              var h$base_sizeof_stat = 40;
function h$base_st_mtime(stat, stat_off) {
    { h$ret1 = (stat.i3[(stat_off>>2)+4]); return (stat.i3[(stat_off>>2)+3]); };
}
function h$base_st_size(stat, stat_off) {
    { h$ret1 = (stat.i3[(stat_off>>2)+2]); return (stat.i3[(stat_off>>2)+1]); };
}
function h$base_st_mode(stat, stat_off) {
    return stat.i3[stat_off>>2];
}
function h$base_st_dev(stat, stat_off) {
    return stat.i3[(stat_off>>2)+5];
}
function h$base_st_ino(stat, stat_off) {
    { h$ret1 = (stat.i3[(stat_off>>2)+7]); return (stat.i3[(stat_off>>2)+6]); };
}
              var h$base_echo = 1;
              var h$base_tcsanow = 2;
              var h$base_icanon = 4;
              var h$base_vmin = 8;
              var h$base_vtime = 16;
              var h$base_sigttou = 0;
              var h$base_sig_block = 0;
              var h$base_sig_setmask = 0;
              var h$base_f_getfl = 0;
              var h$base_f_setfl = 0;
              var h$base_f_setfd = 0;
              var h$base_fd_cloexec = 0;
              var h$base_sizeof_termios = 4;
              var h$base_sizeof_sigset_t = 4;
function h$base_lflag(termios, termios_off) {
    return 0;
}
function h$base_poke_lflag(termios, termios_off, flag) {
    return 0;
}
function h$base_ptr_c_cc(termios, termios_off) {
    { h$ret1 = (0); return (h$newByteArray(8)); };
}
              var h$base_default_buffer_size = 32768;
function h$base_c_s_issock(mode) {
    return 0;
}
              var h$base_SEEK_SET = 0;
              var h$base_SEEK_CUR = 1;
              var h$base_SEEK_END = 2;
function h$base_set_saved_termios(a, b, c) {
    { h$ret1 = (0); return (null); };
}
function h$base_get_saved_termios(r) {
    { h$ret1 = (0); return (null); };
}
function h$lockFile(fd, dev, ino, for_writing) {
    ;
    return 0;
}
function h$unlockFile(fd) {
    ;
    return 0;
}
var h$base_readStdin , h$base_writeStderr, h$base_writeStdout;
var h$base_isattyStdin = false, h$base_isattyStdout = false, h$base_isattyStderr = false;
var h$base_closeStdin = null, h$base_closeStderr = null, h$base_closeStdout = null;
var h$base_readFile, h$base_writeFile, h$base_closeFile;
var h$base_stdin_waiting = new h$Queue();
var h$base_stdin_chunk = { buf: null
                           , pos: 0
                           , processing: false
                           };
var h$base_stdin_eof = false;
var h$base_process_stdin = function() {
    var c = h$base_stdin_chunk;
    var q = h$base_stdin_waiting;
    if(!q.length() || c.processing) return;
    c.processing = true;
    if(!c.buf) { c.pos = 0; c.buf = process.stdin.read(); }
    while(c.buf && q.length()) {
        var x = q.dequeue();
        var n = Math.min(c.buf.length - c.pos, x.n);
        for(var i=0;i<n;i++) {
            x.buf.u8[i+x.off] = c.buf[c.pos+i];
        }
        c.pos += n;
        x.c(n);
        if(c.pos >= c.buf.length) c.buf = null;
        if(!c.buf && q.length()) { c.pos = 0; c.buf = process.stdin.read(); }
    }
    while(h$base_stdin_eof && q.length()) q.dequeue().c(0);
    c.processing = false;
}
if(h$isNode) {
    h$base_closeFile = function(fd, fdo, c) {
        ;
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        h$fs.close(real_fd, function(err) {
            delete h$base_fds[fd];
            h$handleErrnoC(err, -1, 0, c);
        });
    }
    h$base_readFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        ;
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        h$fs.read(real_fd, Buffer.alloc(n), 0, n, pos, function(err, bytesRead, nbuf) {
            if(err) {
                h$setErrno(err);
                c(-1);
            } else {
                for(var i=bytesRead-1;i>=0;i--) buf.u8[buf_offset+i] = nbuf[i];
                if(typeof fdo.pos === 'number') fdo.pos += bytesRead;
                c(bytesRead);
            }
        });
    }
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        ;
        h$base_stdin_waiting.enqueue({buf: buf, off: buf_offset, n: n, c: c});
        h$base_process_stdin();
    }
    h$base_closeStdin = function(fd, fdo, c) {
        ;
        c(0);
    }
    h$base_writeFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        ;
        var nbuf = Buffer.alloc(n);
        for(var i=0;i<n;i++) nbuf[i] = buf.u8[i+buf_offset];
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        if(typeof fdo.pos === 'number') fdo.pos += n;
        h$fs.write(real_fd, nbuf, 0, n, pos, function(err, bytesWritten) {
            ;
            if(err) {
                h$setErrno(err);
                if(typeof fdo.pos === 'number') fdo.pos -= n;
                if(h$errno === 35)
                    setTimeout(function() { h$base_writeFile(fd, fdo, buf, buf_offset, n, c); }, 20);
                else c(-1);
            } else {
                if(typeof fdo.pos === 'number') fdo.pos += bytesWritten - n;
                c(bytesWritten);
            }
        });
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        ;
        h$base_writeFile(1, fdo, buf, buf_offset, n, c);
    }
    h$base_closeStdout = function(fd, fdo, c) {
        ;
        c(0);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        ;
        h$base_writeFile(2, fdo, buf, buf_offset, n, c);
    }
    h$base_closeStderr = function(fd, fdo, c) {
        ;
        c(0);
    }
    process.stdin.on('readable', h$base_process_stdin);
    process.stdin.on('end', function() { h$base_stdin_eof = true; h$base_process_stdin(); });
    h$base_isattyStdin = function() { return process.stdin.isTTY; };
    h$base_isattyStdout = function() { return process.stdout.isTTY; };
    h$base_isattyStderr = function() { return process.stderr.isTTY; };
} else if (h$isJsShell) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        putstr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        printErr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
} else if(h$isJsCore) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
 c(0);
    }
    var h$base_stdoutLeftover = { f: print, val: null };
    var h$base_stderrLeftover = { f: debug, val: null };
    var h$base_writeWithLeftover = function(buf, n, buf_offset, c, lo) {
 var lines = h$decodeUtf8(buf, n, buf_offset).split(/\r?\n/);
 if(lines.length === 1) {
     if(lines[0].length) {
  if(lo.val !== null) lo.val += lines[0];
  else lo.val = lines[0];
     }
 } else {
            lo.f(((lo.val !== null) ? lo.val : '') + lines[0]);
     for(var i=1;i<lines.length-1;i++) lo.f(lines[i]);
     if(lines[lines.length-1].length) lo.val = lines[lines.length-1];
     else lo.val = null;
 }
 c(n);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
 h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stdoutLeftover);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
 h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stderrLeftover);
    }
} else {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
}
var h$base_stdin_fd =
  { read: h$base_readStdin
  , close: h$base_closeStdin
  , isatty: h$base_isattyStdin
  , refs: 1
  };
var h$base_stdout_fd =
  { write: h$base_writeStdout
  , close: h$base_closeStdout
  , isatty: h$base_isattyStdout
  , refs: 1
  };
var h$base_stderr_fd =
  { write: h$base_writeStderr
  , close: h$base_closeStderr
  , isatty: h$base_isattyStderr
  , refs: 1
  };
var h$base_fdN = -2;
var h$base_fds = [h$base_stdin_fd, h$base_stdout_fd, h$base_stderr_fd];
function h$shutdownHaskellAndExit(code, fast) {
    h$exitProcess(code);
}
function h$rand() {
  return (32768 * Math.random()) & 32767;
}
function h$stg_sig_install(sigNo, actionCode, sigSet_d, sigSet_o) {
  return 0;
}
function h$mkdir(dir_d, dir_o, mode) {
  if(h$isNode) {
    return h$handleErrno(-1, function() {
      h$fs.mkdirSync(h$decodeUtf8z(dir_d, dir_o), mode);
      return 0;
     });
  } else
    return h$unsupported(-1);
}
function h$geteuid() {
  return 1;
}
function h$sysconf() {
  return 0;
}
function h$getpwuid_r(uid, pwd_d, pwd_o, buf_d, buf_o, buflen, result_d, result_o) {
  var i, name = h$encodeUtf8("user"), max = Math.min(72, pwd_d.len);
  if(!result_d.arr) result_d.arr = [];
  result_d.arr[0] = [pwd_d, pwd_o];
  if(!pwd_d.arr) pwd_d.arr = [];
  for(i = 0; i < max; i+=4) pwd_d.arr[i+pwd_o] = [name, 0];
  for(i = 0; i < (max>>2); i++) pwd_d.i3[i+(pwd_o>>2)] = 1;
  return 0;
}
function h$get_current_timezone_seconds(t, pdst_v, pdst_o, pname_v, pname_o) {
    var d = new Date(t * 1000);
    var now = new Date();
    var jan = new Date(now.getFullYear(),0,1);
    var jul = new Date(now.getFullYear(),6,1);
    var stdOff = Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
    var isDst = d.getTimezoneOffset() < stdOff;
    var tzo = d.getTimezoneOffset();
    pdst_v.dv.setInt32(pdst_o, isDst ? 1 : 0, true);
    if(!pname_v.arr) pname_v.arr = [];
    var offstr = tzo < 0 ? ('+' + (tzo/-60)) : ('' + (tzo/-60));
    pname_v.arr[pname_o] = [h$encodeUtf8("UTC" + offstr), 0];
    return (-60*tzo)|0;
}
function h$clock_gettime(when, p_d, p_o) {
  var is64 = p_d.i3.length == 4 && p_o == 0;
  var o = p_o >> 2,
      t = Date.now ? Date.now() : new Date().getTime(),
      tf = Math.floor(t / 1000),
      tn = 1000000 * (t - (1000 * tf));
  if(is64) {
    p_d.i3[o] = tf|0;
    p_d.i3[o+1] = 0;
    p_d.i3[o+2] = tn|0;
    p_d.i3[o+3] = 0;
  } else {
    p_d.i3[o] = tf|0;
    p_d.i3[o+1] = tn|0;
  }
  return 0;
}
function h$clock_getres(when, p_d, p_o) {
  return 0;
}
function h$_hs_text_memcpy(dst_v,dst_v_zero,dst_o2,src_v,src_o_zero,src_o2,n) {
  return h$memcpy(dst_v,2*dst_o2,src_v,2*src_o2,2*n);
}
function h$_hs_text_memcmp(a_v,a_o_zero,a_o2,b_v,b_o_zero,b_o2,n) {
  return h$memcmp(a_v,2*a_o2,b_v,2*b_o2,2*n);
}
var h$_text_utf8d =
   [
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12];
function h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero
                                         , destoff_v, destoff_o
                                         , src_v, src_o
                                         , src_end_v, src_end_o
                                         , s
                                         ) {
  if(src_v === null || src_end_v === null) {
    { h$ret1 = (src_end_o); return (null); };
  }
  var dsto = destoff_v.dv.getUint32(destoff_o,true) << 1;
  var srco = src_o;
  var state = s.state;
  var codepoint = s.codepoint;
  var ddv = dest_v.dv;
  var sdv = src_v.dv;
  function decode(b) {
    var type = h$_text_utf8d[b];
    codepoint = (state !== 0) ?
      (b & 0x3f) | (codepoint << 6) :
      (0xff >>> type) & b;
    state = h$_text_utf8d[256 + state + type];
    return state;
  }
  while (srco < src_end_o) {
    if(decode(sdv.getUint8(srco++)) !== 0) {
      if(state !== 12) {
        continue;
      } else {
        break;
      }
    }
    if (codepoint <= 0xffff) {
      ddv.setUint16(dsto,codepoint,true);
      dsto += 2;
    } else {
      ddv.setUint16(dsto,(0xD7C0 + (codepoint >>> 10)),true);
      ddv.setUint16(dsto+2,(0xDC00 + (codepoint & 0x3FF)),true);
      dsto += 4;
    }
    s.last = srco;
  }
  s.state = state;
  s.codepoint = codepoint;
  destoff_v.dv.setUint32(destoff_o,dsto>>1,true);
  { h$ret1 = (s.last); return (src_v); };
}
function h$_hs_text_decode_utf8_state( dest_v, dest_o_zero
                                     , destoff_v, destoff_o
                                     , src_v, src_o
                                     , srcend_v, srcend_o
                                     , codepoint0_v, codepoint0_o
                                     , state0_v, state0_o
                                     ) {
  var s = { state: state0_v.dv.getUint32(state0_o, true)
          , codepoint: codepoint0_v.dv.getUint32(codepoint0_o, true)
          , last: src_o
          };
  var ret, ret1;
  { (ret) = (h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero , destoff_v, destoff_o , src_v.arr[src_o][0], src_v.arr[src_o][1] , srcend_v, srcend_o , s )); (ret1) = h$ret1; };
  src_v.arr[src_o][1] = s.last;
  state0_v.dv.setUint32(state0_o, s.state, true);
  codepoint0_v.dv.setUint32(codepoint0_o, s.codepoint, true);
  { h$ret1 = (ret1); return (ret); };
}
function h$_hs_text_decode_utf8( dest_v, dest_o_zero
                               , destoff_v, destoff_o
                               , src_v, src_o
                               , srcend_v, srcend_o
                               ) {
  var s = { state: 0
          , codepoint: 0
          , last: src_o
          };
  var ret, ret1;
  { (ret) = (h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero , destoff_v, destoff_o , src_v, src_o , srcend_v, srcend_o , s )); (ret1) = h$ret1; };
  { h$ret1 = (ret1); return (ret); };
}
function h$_hs_text_decode_latin1(dest_d, dest_o_zero, src_d, src_o, srcend_d, srcend_o) {
  var p = src_o;
  var d = 0;
  var su8 = src_d.u8;
  var su3 = src_d.u3;
  var du1 = dest_d.u1;
  while(p != srcend_o && p & 3) {
    du1[d++] = su8[p++];
  }
  if(su3) {
    while (p < srcend_o - 3) {
      var w = su3[p>>2];
      du1[d++] = w & 0xff;
      du1[d++] = (w >>> 8) & 0xff;
      du1[d++] = (w >>> 16) & 0xff;
      du1[d++] = (w >>> 32) & 0xff;
      p += 4;
    }
  }
  while (p != srcend_o)
    du1[d++] = su8[p++];
}
function h$_hs_text_encode_utf8(destp_v, destp_o, src_v, src_o_zero, srcoff, srclen) {
  var dest_v = destp_v.arr[destp_o][0];
  var dest_o = destp_v.arr[destp_o][1];
  var src = srcoff;
  var dest = dest_o;
  var srcend = src + srclen;
  var srcu1 = src_v.u1;
  if(!srcu1) throw "h$_hs_text_encode_utf8: invalid alignment for source";
  var srcu3 = src_v.u3;
  var destu8 = dest_v.u8;
  while(src < srcend) {
    while(srcu3 && !(src & 1) && srcend - src >= 2) {
      var w = srcu3[src>>1];
      if(w & 0xFF80FF80) break;
      destu8[dest++] = w & 0xFFFF;
      destu8[dest++] = w >>> 16;
      src += 2;
    }
    while(src < srcend) {
      var w = srcu1[src++];
      if(w <= 0x7F) {
        destu8[dest++] = w;
        break;
      } else if(w <= 0x7FF) {
        destu8[dest++] = (w >> 6) | 0xC0;
        destu8[dest++] = (w & 0x3f) | 0x80;
      } else if(w < 0xD800 || w > 0xDBFF) {
        destu8[dest++] = (w >>> 12) | 0xE0;
        destu8[dest++] = ((w >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (w & 0x3F) | 0x80;
      } else {
        var c = ((w - 0xD800) << 10) + (srcu1[src++] - 0xDC00) + 0x10000;
        destu8[dest++] = (c >>> 18) | 0xF0;
        destu8[dest++] = ((c >> 12) & 0x3F) | 0x80;
        destu8[dest++] = ((c >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (c & 0x3F) | 0x80;
      }
    }
  }
  destp_v.arr[destp_o][1] = dest;
}
function h$hsprimitive_memcpy(dst_d, dst_o, doff, src_d, src_o, soff, len) {
  return h$primitive_memmove(dst_d, dst_o, doff, src_d, src_o, len);
}
function h$hsprimitive_memmove(dst_d, dst_o, doff, src_d, src_o, soff, len) {
  if(len === 0) return;
  var du8 = dst_d.u8, su8 = src_d.u8;
  for(var i=len-1;i>=0;i--) {
    du8[dst_o+i] = su8[src_o+i];
  }
}
function h$hsprimitive_memsetba_Word8 (p_d, off, n, x) { if(n > 0) { if(p_d.u8.fill) p_d.u8.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.u8[i] = x; } }
function h$hsprimitive_memsetba_Word16 (p_d, off, n, x) { if(n > 0) { if(p_d.u1.fill) p_d.u1.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.u1[i] = x; } }
function h$hsprimitive_memsetba_Word32 (p_d, off, n, x) { if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memsetba_Word (p_d, off, n, x) { if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memsetba_Float (p_d, off, n, x) { if(n > 0) { if(p_d.f3.fill) p_d.f3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.f3[i] = x; } }
function h$hsprimitive_memsetba_Double (p_d, off, n, x) { if(n > 0) { if(p_d.f6.fill) p_d.f6.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.f6[i] = x; } }
function h$hsprimitive_memsetba_Char (p_d, off, n, x) { if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memset_Word8 (p_d, p_o, off, n, x) { var start = (p_o >> 0) + off; if(n > 0) { if(p_d.u8.fill) p_d.u8.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.u8[i] = x; } }
function h$hsprimitive_memset_Word16 (p_d, p_o, off, n, x) { var start = (p_o >> 1) + off; if(n > 0) { if(p_d.u1.fill) p_d.u1.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.u1[i] = x; } }
function h$hsprimitive_memset_Word32 (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memset_Word (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memset_Float (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.f3.fill) p_d.f3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.f3[i] = x; } }
function h$hsprimitive_memset_Double (p_d, p_o, off, n, x) { var start = (p_o >> 3) + off; if(n > 0) { if(p_d.f6.fill) p_d.f6.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.f6[i] = x; } }
function h$hsprimitive_memset_Char (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memsetba_Word64(p_d, off, n, x_1, x_2) {
  h$hsprimitive_memset_Word64(p_d, 0, off, n, x_1, x_2);
}
function h$hsprimitive_memset_Word64(p_d, p_o, off, n, x_1, x_2) {
  var start = (p_o >> 3) + off;
  if(n > 0) {
    var pi3 = p_d.i3;
    for(var i = 0; i < n; i++) {
      var o = (start + i) << 1;
      pi3[o] = x_1;
      pi3[o+1] = x_2;
    }
  }
}
function h$hsprimitive_memset_Ptr(p_d, p_o, off, n, x_1, x_2) {
  if(n > 0) {
    if(!p_d.arr) p_d.arr = [];
    var a = p_d.arr;
    for(var i = 0; i < n; i++) {
      a[p_o + ((off + i) << 2)] = [x_1, x_2];
    }
  }
}
function h$hashable_fnv_hash_offset(str_a, str_o_zero, o, len, hash) {
  return h$hashable_fnv_hash(str_a, o, len, hash);
}
function h$hashable_fnv_hash(str_d, str_o, len, hash) {
  if(len > 0) {
    var d = str_d.u8;
    for(var i=0;i<len;i++) {
      hash = h$mulInt32(hash, 16777619) ^ d[str_o+i];
    }
  }
  return hash;
}
function h$hashable_getRandomBytes(dest_d, dest_o, len) {
  if(len > 0) {
    var d = dest_d.u8;
    for(var i=0;i<len;i++) {
      d[dest_o+i] = Math.floor(Math.random() * 256);
    }
  }
  return len;
}
function h$isFloat (n) {
  return n===+n && n!==(n|0);
}
function h$isInteger (n) {
  return n===+n && n===(n|0);
}
function h$typeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            return 5;
        } else if (!o) {
            return 0;
        } else {
            return 6;
        }
    }
}
function h$flattenObj(o) {
    var l = [], i = 0;
    for (var prop in o) {
        l[i++] = [prop, o[prop]];
    }
    return l;
}
function h$buildObject() {
    var r = {}, l = arguments.length;
    for(var i = 0; i < l; i += 2) {
        var k = arguments[i], v = arguments[i+1];
        r[k] = v;
    }
    return r;
}
function h$buildObjectFromList(xs) {
    var r = {}, k, v, t;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
        xs = ((xs).d2);
        t = ((xs).d2);
        if(((t).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
            k = ((xs).d1);
            v = ((t).d1);
            xs = ((t).d2);
            r[k] = v;
        } else {
            return r;
        }
    }
    return r;
}
function h$buildObjectFromTupList(xs) {
    var r = {};
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 var h = ((xs).d1);
 xs = ((xs).d2);
 r[((((h).d1)).d1)] = ((((h).d2)).d1);
    }
    return r;
}
function h$filepath_isWindows() {
    if(h$isNode && process.platform === 'win32') return true;
  return false;
}
function h$directory_getPermissions(file, c) {
    ;
    if(h$isNode) {
        h$fs.stat(file, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var m = fs.mode;
                var r = (m&4) || (m&32) || (m&256);
                var w = (m&2) || (m&16) || (m&128);
                var x = (m&1) || (m&8) || (m&64);
                var exe = x;
                var search = x;
                if(process.platform == 'win32') exe = true;
                c((r?1:0)|(w?2:0)|(exe?4:0)|(search?8:0));
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_setPermissions(file, perms, c) {
    ;
    if(h$isNode) {
        h$fs.stat(file, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var r = perms & 1;
                var w = perms & 2;
                var x = perms & 4;
                var search = perms & 8;
                var m = fs.mode;
                m = r ? (m | 292) : (m & ~292);
                m = w ? (m | 146) : (m & ~146);
                m = (x || search) ? (m | 73) : (m & ~73);
                h$fs.chmod(file, function(err) {
                    h$handleErrnoC(err, -1, 0, c);
                });
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_copyPermissions(file1, file2, c) {
    ;
    if(h$isNode) {
        h$fs.stat(file1, function(err1, fs) {
            if(err1) {
                h$handleErrnoC(err1, -1, 0, c);
            } else {
                h$fs.chmod(file2, fs.mode, function(err2) {
                    h$handleErrnoC(err2, -1, 0, c);
                });
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_createDirectory(dir, c) {
    ;
    if(h$isNode) {
        h$fs.mkdir(dir, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_removeDirectory(dir, c) {
    ;
    if(h$isNode) {
        h$fs.rmdir(dir, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_removeFile(file, c) {
    ;
    if(h$isNode) {
        h$fs.unlink(file, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_renamePath(file1, file2, c) {
    ;
    if(h$isNode) {
        h$fs.rename(file1, file2, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_canonicalizePath(path) {
    ;
    if(h$isNode) {
      try {
        return h$path.resolve(path);
      } catch(e0) { }
      if(h$path.isAbsolute(path)) {
        try {
          return h$path.normalize(path);
        } catch(e1) { }
      } else {
        var cwd;
        try {
          cwd = process.cwd();
        } catch(e2) {
          cwd = h$directory_cachedCurrentDirectory;
        }
        if(cwd) {
          try {
            return h$path.join(cwd, path);
          } catch(e3) { }
        }
      }
      return path;
    } else
        return path;
}
function h$directory_getDirectoryContents(dir,c) {
    ;
    if(h$isNode) {
        h$fs.readdir(dir, function(err, d) {
            h$handleErrnoC(err, null, d, c);
        });
    } else
        h$unsupported(null, c);
}
function h$directory_getCurrentDirectory() {
    ;
    if(h$isNode) {
        return h$handleErrno(null, function() {
            return process.cwd();
        });
    } else
        return "/";
}
var h$directory_cachedCurrentDirectory = null;
function h$directory_setCurrentDirectory(dir) {
    ;
    if(h$isNode) {
        return h$handleErrnoS(-1, 0, function() {
          process.chdir(dir);
          h$directory_cachedCurrentDirectory = process.cwd();
        });
    } else
        return h$unsupported(-1);
}
function h$directory_getPath() {
    if(h$isNode) {
        return process.env['PATH'] || null;
    } else
        return null
}
function h$directory_getHomeDirectory(dir) {
    ;
    if(h$isNode) {
        return process.env['HOME'] ||
            process.env['HOMEPATH'] ||
            process.env['USERPROFILE'] ||
            null;
    } else
        return "/"
}
function h$directory_getAppUserDataDirectory(appName) {
    ;
    if(h$isNode) {
        if(process.env['APPDATA'])
            return process.env['APPDATA'] + h$path.sep + appName;
        if(process.env['HOME'])
            return process.env['HOME'] + h$path.sep + "." + appName;
        ;
        return "/";
    } else
        return "/";
}
function h$directory_getUserDocumentsDirectory(appName) {
    ;
    if(h$isNode) {
        if(process.env['HOME'])
            return process.env['HOME'];
        ;
        return "/";
    } else
        return "/";
}
function h$directory_getTemporaryDirectory() {
    ;
    if(h$isNode) {
        return h$handleErrno(null, function() {
            return h$os.tmpdir();
        });
    } else
        return "/";
}
function h$directory_exeExtension() {
    ;
    if(h$isNode) {
        return (h$os.platform() === 'windows') ? 'exe' : '';
    } else
        return '';
}
function h$directory_getFileStatus(file, c) {
    ;
    if(h$isNode) {
        h$fs.stat(file, function(err, s) {
            h$handleErrnoC(err, null, s, c);
        });
    } else
        h$unsupported(null, c);
}
function h$directory_getFileOrSymlinkStatus(file, c) {
    ;
    if(h$isNode) {
        h$fs.lstat(file, function(err, s) {
            h$handleErrnoC(err, null, s, c);
        });
    } else
        h$unsupported(null, c);
}
function h$directory_getFileStatusAccessTime(fs) {
  ;
  return fs.atime.getTime();
}
function h$directory_getFileStatusModificationTime(fs) {
  ;
  return fs.mtime.getTime();
}
function h$directory_getFileStatusIsDirectory(fs) {
  ;
  return fs.isDirectory();
}
function h$directory_getFileStatusIsSymbolicLink(fs) {
  ;
  return fs.isSymbolicLink();
}
function h$directory_getFileStatusFileSize(fs) {
  ;
  return fs.size;
}
function h$directory_getFileStatusFileMode(fs) {
  ;
  return fs.mode;
}
function h$directory_getFileStatusGroup(fs) {
  ;
  return fs.group;
}
function h$directory_getFileStatusOwner(fs) {
  ;
  return fs.owner;
}
function h$directory_getFileAccess(path, r, w, x, cont) {
  ;
  h$fs.access(path
             , (r ? h$fs.constants.R_OK : 0) |
               (w ? h$fs.constants.W_OK : 0) |
               (x ? h$fs.constants.X_OK : 0) |
               h$fs.constants.F_OK
             , function(err) {
               cont(err ? false : true);
             }
           );
}
function h$directory_setFileMode(file, mode, c) {
    ;
    if(h$isNode) {
        h$fs.chmod(file, mode, function(err) {
          h$handleErrnoC(err, -1, 0, c)
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_setOwnerAndGroup(file, owner, group, c) {
    ;
    if(h$isNode) {
        h$fs.chown(file, owner, group, function(err) {
          h$handleErrnoC(err, -1, 0, c)
        });
    } else
        h$unsupported(-1, c);
}
function h$directory_readSymbolicLink(path, c) {
  ;
  if(h$isNode) {
      h$fs.readlink(path, function(err, linkString) {
        h$handleErrnoC(err, null, linkString, c)
      });
  } else
      h$unsupported(null, c);
}
function h$directory_setFileTimes(path, atime, set_atime, mtime, set_mtime, c) {
  ;
  if(h$isNode) {
      if(set_atime && set_mtime) {
        h$fs.utimes(path, atime, mtime, function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
      } else {
        h$fs.stat(path, function(err, fs) {
          if(err) {
            h$handleErrnoC(err, -1, 0, c);
          } else {
            if(!set_atime) {
              atime = fs.atimeMs ? fs.atimeMs : fs.atime.getTime();
            }
            if(!set_mtime) {
              mtime = fs.mtimeMs ? fs.mtimeMs : fs.mtime.getTime();
            }
            h$fs.utimes(path, atime, mtime, function(err) {
              h$handleErrnoC(err, -1, 0, c);
            })
          }
        });
      }
  } else
      h$unsupported(-1, c);
}
function h$directory_createSymbolicLink(target, link, c) {
  ;
  if(h$isNode) {
    h$fs.symlink(target, link, function(err) {
      h$handleErrnoC(err, -1, 0, c);
    });
  } else
    h$unsupported(-1, c);
}
function h$directory_copyFileContents(src, dst, c) {
  ;
  if(h$isNode) {
    h$fs.copyFile(src, dst, function(err) {
      h$handleErrnoC(err, -1, 0, c);
    });
  } else
    h$unsupported(-1, c);
}
function h$chmod(path_d, path_o, m) {
    if(h$isNode) {
        var path = h$decodeUtf8z(path_d, path_o);
        ;
        h$fs.chmodSync(path, m);
        return 0;
    } else
        return h$unsupported(-1);
}
function h$fps_reverse(a_v, a_o, b_v, b_o, n) {
    if(n > 0) {
        var au8 = a_v.u8, bu8 = b_v.u8;
        for(var i=0;i<n;i++) {
            au8[a_o+n-i-1] = bu8[b_o+i];
        }
    }
}
function h$fps_intersperse(a_v,a_o,b_v,b_o,n,c) {
    if(n > 0) {
        var au8 = a_v.u8, bu8 = b_v.u8, dst_o = a_o;
        for(var i=0;i<n-1;i++) {
            au8[dst_o] = bu8[b_o+i];
            au8[dst_o+1] = c;
            dst_o += 2;
        }
        au8[dst_o] = bu8[b_o+n-1];
    }
}
function h$fps_maximum(a_v,a_o,n) {
    if(n > 0) {
        var au8 = a_v.u8, max = au8[a_o];
        for(var i=1;i<n;i++) {
            var c = au8[a_o+i];
            if(c > max) { max = c; }
        }
        return max;
    }
    return 0;
}
function h$fps_minimum(a_v,a_o,n) {
    if(n > 0) {
        var au8 = a_v.u8, min = a_v.u8[a_o];
        for(var i=1;i<n;i++) {
            var c = au8[a_o+i];
            if(c < min) { min = c; }
        }
        return min;
    }
    return 255;
}
function h$fps_count(a_v,a_o,n,c) {
    if(n > 0) {
        var au8 = a_v.u8, count = 0;
        for(var i=0;i<n;i++) {
            if(au8[a_o+i] === c) { count++; }
        }
        return count|0;
    }
    return 0;
}
function h$fps_memcpy_offsets(dst_d, dst_o, dst_off
                              , src_d, src_o, src_off, n) {
    return memcpy(dst_d, dst_o + dst_off, src_d, src_o + src_off, n);
}
var h$_hs_bytestring_digits = [48,49,50,51,52,53,54,55,56,57,97,98,99,100,101,102];
var h$_hs_bytestring_l10 = goog.math.Long.fromBits(10, 0);
function h$_hs_bytestring_int_dec(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free, x_tmp;
    var bu8 = buf_d.u8;
    if(x < 0) {
        bu8[ptr++] = 45;
        buf_o++;
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x * 10 - x_tmp];
        if(x === 0) {
            { h$ret1 = (ptr); return (buf_d); };
        } else {
            x = -x;
        }
    }
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while (x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_long_long_int_dec(x_a, x_b, buf_d, buf_o) {
    var l10 = h$_hs_bytestring_l10;
    var x = goog.math.Long.fromBits(x_b, x_a);
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    if(x.isNegative()) {
        bu8[ptr++] = 45;
        buf_o++;
        x_tmp = x;
        x = x.div(l10);
        bu8[ptr++] = h$_hs_bytestring_digits[x.multiply(l10).subtract(x_tmp).getLowBits()];
        if(x.isZero()) {
            { h$ret1 = (ptr); return (buf_d); };
        } else {
            x = x.negate();
        }
    }
    do {
        x_tmp = x;
        x = x.div(l10);
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp.subtract(x.multiply(l10))];
    } while (!x.isZero());
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_uint_dec(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    var x_tmp;
    if(x < 0) x += 4294967296;
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while(x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_long_long_uint_dec(x_a, x_b, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    var x = h$ghcjsbn_mkBigNat_ww(x_a, x_b), q = [], r;
    do {
        r = h$ghcjsbn_quotRem_bw(q, x, 10);
        x = q;
        q = [];
        bu8[ptr++] = h$_hs_bytestring_digits[r];
    } while(!h$ghcjsbn_isZero_b(x));
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_int_dec_padded9(x, buf_d, buf_o) {
    var max_width_int32_dec = 9;
    var ptr = buf_o + max_width_int32_dec;
    var bu8 = buf_d.u8;
    var x_tmp;
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[--ptr] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while(x);
    while (buf_o < ptr) { bu8[--ptr] = 48; }
}
function h$_hs_bytestring_long_long_int_dec_padded18(x_a, x_b, buf_d, buf_o) {
    var l10 = h$_hs_bytestring_l10;
    var max_width_int64_dec = 18;
    var ptr = buf_o + max_width_int64_dec;
    var bu8 = buf_d.u8;
    var x = goog.math.Long.fromBits(x_b, x_a);
    do {
        x_tmp = x;
        x = x.div(l10);
        bu8[--ptr] = h$_hs_bytestring_digits[x_tmp.subtract(x.multiply(l10))];
    } while (!x.isZero());
    while (buf_o < ptr) { bu8[--ptr] = 48; }
}
function h$_hs_bytestring_uint_hex(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    do {
        bu8[ptr++] = h$_hs_bytestring_digits[x & 0xf];
        x >>>= 4;
    } while(x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_long_long_uint_hex(x_a, x_b, buf_d, buf_o) {
    var c, i, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    if(x_a === 0 && x_b === 0) {
        bu8[ptr++] = 48;
    } else if(x_a === 0) {
      while(x_b !== 0) {
          bu8[ptr++] = h$_hs_bytestring_digits[x_b & 0xf];
          x_b >>>= 4;
      }
    } else {
        for(i=0;i<8;i++) {
            bu8[ptr++] = h$_hs_bytestring_digits[x_b & 0xf];
            x_b >>>= 4;
        }
        while(x_a !== 0) {
            bu8[ptr++] = h$_hs_bytestring_digits[x_a & 0xf];
            x_a >>>= 4;
        }
    }
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
