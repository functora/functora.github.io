var h$bitcoin_hash=function(t){var n={};function i(r){if(n[r])return n[r].exports;var e=n[r]={i:r,l:!1,exports:{}};return t[r].call(e.exports,e,e.exports,i),e.l=!0,e.exports}return i.m=t,i.c=n,i.d=function(t,n,r){i.o(t,n)||Object.defineProperty(t,n,{enumerable:!0,get:r})},i.r=function(t){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},i.t=function(t,n){if(1&n&&(t=i(t)),8&n)return t;if(4&n&&"object"==typeof t&&t&&t.__esModule)return t;var r=Object.create(null);if(i.r(r),Object.defineProperty(r,"default",{enumerable:!0,value:t}),2&n&&"string"!=typeof t)for(var e in t)i.d(r,e,function(n){return t[n]}.bind(null,e));return r},i.n=function(t){var n=t&&t.__esModule?function(){return t.default}:function(){return t};return i.d(n,"a",n),n},i.o=function(t,n){return Object.prototype.hasOwnProperty.call(t,n)},i.p="",i(i.s=7)}([function(t,n,i){"use strict";var r=i(1),e=i(8);function h(t,n){return 55296==(64512&t.charCodeAt(n))&&(!(n<0||n+1>=t.length)&&56320==(64512&t.charCodeAt(n+1)))}function s(t){return(t>>>24|t>>>8&65280|t<<8&16711680|(255&t)<<24)>>>0}function o(t){return 1===t.length?"0"+t:t}function u(t){return 7===t.length?"0"+t:6===t.length?"00"+t:5===t.length?"000"+t:4===t.length?"0000"+t:3===t.length?"00000"+t:2===t.length?"000000"+t:1===t.length?"0000000"+t:t}n.inherits=e,n.toArray=function(t,n){if(Array.isArray(t))return t.slice();if(!t)return[];var i=[];if("string"==typeof t)if(n){if("hex"===n)for((t=t.replace(/[^a-z0-9]+/gi,"")).length%2!=0&&(t="0"+t),e=0;e<t.length;e+=2)i.push(parseInt(t[e]+t[e+1],16))}else for(var r=0,e=0;e<t.length;e++){var s=t.charCodeAt(e);s<128?i[r++]=s:s<2048?(i[r++]=s>>6|192,i[r++]=63&s|128):h(t,e)?(s=65536+((1023&s)<<10)+(1023&t.charCodeAt(++e)),i[r++]=s>>18|240,i[r++]=s>>12&63|128,i[r++]=s>>6&63|128,i[r++]=63&s|128):(i[r++]=s>>12|224,i[r++]=s>>6&63|128,i[r++]=63&s|128)}else for(e=0;e<t.length;e++)i[e]=0|t[e];return i},n.toHex=function(t){for(var n="",i=0;i<t.length;i++)n+=o(t[i].toString(16));return n},n.htonl=s,n.toHex32=function(t,n){for(var i="",r=0;r<t.length;r++){var e=t[r];"little"===n&&(e=s(e)),i+=u(e.toString(16))}return i},n.zero2=o,n.zero8=u,n.join32=function(t,n,i,e){var h=i-n;r(h%4==0);for(var s=new Array(h/4),o=0,u=n;o<s.length;o++,u+=4){var a;a="big"===e?t[u]<<24|t[u+1]<<16|t[u+2]<<8|t[u+3]:t[u+3]<<24|t[u+2]<<16|t[u+1]<<8|t[u],s[o]=a>>>0}return s},n.split32=function(t,n){for(var i=new Array(4*t.length),r=0,e=0;r<t.length;r++,e+=4){var h=t[r];"big"===n?(i[e]=h>>>24,i[e+1]=h>>>16&255,i[e+2]=h>>>8&255,i[e+3]=255&h):(i[e+3]=h>>>24,i[e+2]=h>>>16&255,i[e+1]=h>>>8&255,i[e]=255&h)}return i},n.rotr32=function(t,n){return t>>>n|t<<32-n},n.rotl32=function(t,n){return t<<n|t>>>32-n},n.sum32=function(t,n){return t+n>>>0},n.sum32_3=function(t,n,i){return t+n+i>>>0},n.sum32_4=function(t,n,i,r){return t+n+i+r>>>0},n.sum32_5=function(t,n,i,r,e){return t+n+i+r+e>>>0},n.sum64=function(t,n,i,r){var e=t[n],h=r+t[n+1]>>>0,s=(h<r?1:0)+i+e;t[n]=s>>>0,t[n+1]=h},n.sum64_hi=function(t,n,i,r){return(n+r>>>0<n?1:0)+t+i>>>0},n.sum64_lo=function(t,n,i,r){return n+r>>>0},n.sum64_4_hi=function(t,n,i,r,e,h,s,o){var u=0,a=n;return u+=(a=a+r>>>0)<n?1:0,u+=(a=a+h>>>0)<h?1:0,t+i+e+s+(u+=(a=a+o>>>0)<o?1:0)>>>0},n.sum64_4_lo=function(t,n,i,r,e,h,s,o){return n+r+h+o>>>0},n.sum64_5_hi=function(t,n,i,r,e,h,s,o,u,a){var c=0,f=n;return c+=(f=f+r>>>0)<n?1:0,c+=(f=f+h>>>0)<h?1:0,c+=(f=f+o>>>0)<o?1:0,t+i+e+s+u+(c+=(f=f+a>>>0)<a?1:0)>>>0},n.sum64_5_lo=function(t,n,i,r,e,h,s,o,u,a){return n+r+h+o+a>>>0},n.rotr64_hi=function(t,n,i){return(n<<32-i|t>>>i)>>>0},n.rotr64_lo=function(t,n,i){return(t<<32-i|n>>>i)>>>0},n.shr64_hi=function(t,n,i){return t>>>i},n.shr64_lo=function(t,n,i){return(t<<32-i|n>>>i)>>>0}},function(t,n){function i(t,n){if(!t)throw new Error(n||"Assertion failed")}t.exports=i,i.equal=function(t,n,i){if(t!=n)throw new Error(i||"Assertion failed: "+t+" != "+n)}},function(t,n,i){"use strict";var r=i(0),e=i(1);function h(){this.pending=null,this.pendingTotal=0,this.blockSize=this.constructor.blockSize,this.outSize=this.constructor.outSize,this.hmacStrength=this.constructor.hmacStrength,this.padLength=this.constructor.padLength/8,this.endian="big",this._delta8=this.blockSize/8,this._delta32=this.blockSize/32}n.BlockHash=h,h.prototype.update=function(t,n){if(t=r.toArray(t,n),this.pending?this.pending=this.pending.concat(t):this.pending=t,this.pendingTotal+=t.length,this.pending.length>=this._delta8){var i=(t=this.pending).length%this._delta8;this.pending=t.slice(t.length-i,t.length),0===this.pending.length&&(this.pending=null),t=r.join32(t,0,t.length-i,this.endian);for(var e=0;e<t.length;e+=this._delta32)this._update(t,e,e+this._delta32)}return this},h.prototype.digest=function(t){return this.update(this._pad()),e(null===this.pending),this._digest(t)},h.prototype._pad=function(){var t=this.pendingTotal,n=this._delta8,i=n-(t+this.padLength)%n,r=new Array(i+this.padLength);r[0]=128;for(var e=1;e<i;e++)r[e]=0;if(t<<=3,"big"===this.endian){for(var h=8;h<this.padLength;h++)r[e++]=0;r[e++]=0,r[e++]=0,r[e++]=0,r[e++]=0,r[e++]=t>>>24&255,r[e++]=t>>>16&255,r[e++]=t>>>8&255,r[e++]=255&t}else for(r[e++]=255&t,r[e++]=t>>>8&255,r[e++]=t>>>16&255,r[e++]=t>>>24&255,r[e++]=0,r[e++]=0,r[e++]=0,r[e++]=0,h=8;h<this.padLength;h++)r[e++]=0;return r}},function(t,n,i){var r=n;r.utils=i(0),r.common=i(2),r.sha=i(9),r.ripemd=i(13),r.hmac=i(14),r.sha1=r.sha.sha1,r.sha256=r.sha.sha256,r.sha224=r.sha.sha224,r.sha384=r.sha.sha384,r.sha512=r.sha.sha512,r.ripemd160=r.ripemd.ripemd160},function(t,n,i){"use strict";var r=i(0).rotr32;function e(t,n,i){return t&n^~t&i}function h(t,n,i){return t&n^t&i^n&i}function s(t,n,i){return t^n^i}n.ft_1=function(t,n,i,r){return 0===t?e(n,i,r):1===t||3===t?s(n,i,r):2===t?h(n,i,r):void 0},n.ch32=e,n.maj32=h,n.p32=s,n.s0_256=function(t){return r(t,2)^r(t,13)^r(t,22)},n.s1_256=function(t){return r(t,6)^r(t,11)^r(t,25)},n.g0_256=function(t){return r(t,7)^r(t,18)^t>>>3},n.g1_256=function(t){return r(t,17)^r(t,19)^t>>>10}},function(t,n,i){"use strict";var r=i(0),e=i(2),h=i(4),s=i(1),o=r.sum32,u=r.sum32_4,a=r.sum32_5,c=h.ch32,f=h.maj32,l=h.s0_256,p=h.s1_256,g=h.g0_256,d=h.g1_256,_=e.BlockHash,v=[1116352408,1899447441,3049323471,3921009573,961987163,1508970993,2453635748,2870763221,3624381080,310598401,607225278,1426881987,1925078388,2162078206,2614888103,3248222580,3835390401,4022224774,264347078,604807628,770255983,1249150122,1555081692,1996064986,2554220882,2821834349,2952996808,3210313671,3336571891,3584528711,113926993,338241895,666307205,773529912,1294757372,1396182291,1695183700,1986661051,2177026350,2456956037,2730485921,2820302411,3259730800,3345764771,3516065817,3600352804,4094571909,275423344,430227734,506948616,659060556,883997877,958139571,1322822218,1537002063,1747873779,1955562222,2024104815,2227730452,2361852424,2428436474,2756734187,3204031479,3329325298];function m(){if(!(this instanceof m))return new m;_.call(this),this.h=[1779033703,3144134277,1013904242,2773480762,1359893119,2600822924,528734635,1541459225],this.k=v,this.W=new Array(64)}r.inherits(m,_),t.exports=m,m.blockSize=512,m.outSize=256,m.hmacStrength=192,m.padLength=64,m.prototype._update=function(t,n){for(var i=this.W,r=0;r<16;r++)i[r]=t[n+r];for(;r<i.length;r++)i[r]=u(d(i[r-2]),i[r-7],g(i[r-15]),i[r-16]);var e=this.h[0],h=this.h[1],_=this.h[2],v=this.h[3],m=this.h[4],y=this.h[5],b=this.h[6],S=this.h[7];for(s(this.k.length===i.length),r=0;r<i.length;r++){var k=a(S,p(m),c(m,y,b),this.k[r],i[r]),x=o(l(e),f(e,h,_));S=b,b=y,y=m,m=o(v,k),v=_,_=h,h=e,e=o(k,x)}this.h[0]=o(this.h[0],e),this.h[1]=o(this.h[1],h),this.h[2]=o(this.h[2],_),this.h[3]=o(this.h[3],v),this.h[4]=o(this.h[4],m),this.h[5]=o(this.h[5],y),this.h[6]=o(this.h[6],b),this.h[7]=o(this.h[7],S)},m.prototype._digest=function(t){return"hex"===t?r.toHex32(this.h,"big"):r.split32(this.h,"big")}},function(t,n,i){"use strict";var r=i(0),e=i(2),h=i(1),s=r.rotr64_hi,o=r.rotr64_lo,u=r.shr64_hi,a=r.shr64_lo,c=r.sum64,f=r.sum64_hi,l=r.sum64_lo,p=r.sum64_4_hi,g=r.sum64_4_lo,d=r.sum64_5_hi,_=r.sum64_5_lo,v=e.BlockHash,m=[1116352408,3609767458,1899447441,602891725,3049323471,3964484399,3921009573,2173295548,961987163,4081628472,1508970993,3053834265,2453635748,2937671579,2870763221,3664609560,3624381080,2734883394,310598401,1164996542,607225278,1323610764,1426881987,3590304994,1925078388,4068182383,2162078206,991336113,2614888103,633803317,3248222580,3479774868,3835390401,2666613458,4022224774,944711139,264347078,2341262773,604807628,2007800933,770255983,1495990901,1249150122,1856431235,1555081692,3175218132,1996064986,2198950837,2554220882,3999719339,2821834349,766784016,2952996808,2566594879,3210313671,3203337956,3336571891,1034457026,3584528711,2466948901,113926993,3758326383,338241895,168717936,666307205,1188179964,773529912,1546045734,1294757372,1522805485,1396182291,2643833823,1695183700,2343527390,1986661051,1014477480,2177026350,1206759142,2456956037,344077627,2730485921,1290863460,2820302411,3158454273,3259730800,3505952657,3345764771,106217008,3516065817,3606008344,3600352804,1432725776,4094571909,1467031594,275423344,851169720,430227734,3100823752,506948616,1363258195,659060556,3750685593,883997877,3785050280,958139571,3318307427,1322822218,3812723403,1537002063,2003034995,1747873779,3602036899,1955562222,1575990012,2024104815,1125592928,2227730452,2716904306,2361852424,442776044,2428436474,593698344,2756734187,3733110249,3204031479,2999351573,3329325298,3815920427,3391569614,3928383900,3515267271,566280711,3940187606,3454069534,4118630271,4000239992,116418474,1914138554,174292421,2731055270,289380356,3203993006,460393269,320620315,685471733,587496836,852142971,1086792851,1017036298,365543100,1126000580,2618297676,1288033470,3409855158,1501505948,4234509866,1607167915,987167468,1816402316,1246189591];function y(){if(!(this instanceof y))return new y;v.call(this),this.h=[1779033703,4089235720,3144134277,2227873595,1013904242,4271175723,2773480762,1595750129,1359893119,2917565137,2600822924,725511199,528734635,4215389547,1541459225,327033209],this.k=m,this.W=new Array(160)}function b(t,n,i,r,e){var h=t&i^~t&e;return h<0&&(h+=4294967296),h}function S(t,n,i,r,e,h){var s=n&r^~n&h;return s<0&&(s+=4294967296),s}function k(t,n,i,r,e){var h=t&i^t&e^i&e;return h<0&&(h+=4294967296),h}function x(t,n,i,r,e,h){var s=n&r^n&h^r&h;return s<0&&(s+=4294967296),s}function z(t,n){var i=s(t,n,28)^s(n,t,2)^s(n,t,7);return i<0&&(i+=4294967296),i}function w(t,n){var i=o(t,n,28)^o(n,t,2)^o(n,t,7);return i<0&&(i+=4294967296),i}function A(t,n){var i=s(t,n,14)^s(t,n,18)^s(n,t,9);return i<0&&(i+=4294967296),i}function H(t,n){var i=o(t,n,14)^o(t,n,18)^o(n,t,9);return i<0&&(i+=4294967296),i}function j(t,n){var i=s(t,n,1)^s(t,n,8)^u(t,n,7);return i<0&&(i+=4294967296),i}function L(t,n){var i=o(t,n,1)^o(t,n,8)^a(t,n,7);return i<0&&(i+=4294967296),i}function O(t,n){var i=s(t,n,19)^s(n,t,29)^u(t,n,6);return i<0&&(i+=4294967296),i}function B(t,n){var i=o(t,n,19)^o(n,t,29)^a(t,n,6);return i<0&&(i+=4294967296),i}r.inherits(y,v),t.exports=y,y.blockSize=1024,y.outSize=512,y.hmacStrength=192,y.padLength=128,y.prototype._prepareBlock=function(t,n){for(var i=this.W,r=0;r<32;r++)i[r]=t[n+r];for(;r<i.length;r+=2){var e=O(i[r-4],i[r-3]),h=B(i[r-4],i[r-3]),s=i[r-14],o=i[r-13],u=j(i[r-30],i[r-29]),a=L(i[r-30],i[r-29]),c=i[r-32],f=i[r-31];i[r]=p(e,h,s,o,u,a,c,f),i[r+1]=g(e,h,s,o,u,a,c,f)}},y.prototype._update=function(t,n){this._prepareBlock(t,n);var i=this.W,r=this.h[0],e=this.h[1],s=this.h[2],o=this.h[3],u=this.h[4],a=this.h[5],p=this.h[6],g=this.h[7],v=this.h[8],m=this.h[9],y=this.h[10],j=this.h[11],L=this.h[12],O=this.h[13],B=this.h[14],W=this.h[15];h(this.k.length===i.length);for(var P=0;P<i.length;P+=2){var T=B,C=W,M=A(v,m),U=H(v,m),E=b(v,m,y,j,L),q=S(v,m,y,j,L,O),I=this.k[P],$=this.k[P+1],D=i[P],F=i[P+1],G=d(T,C,M,U,E,q,I,$,D,F),J=_(T,C,M,U,E,q,I,$,D,F);T=z(r,e),C=w(r,e),M=k(r,e,s,o,u),U=x(r,e,s,o,u,a);var K=f(T,C,M,U),N=l(T,C,M,U);B=L,W=O,L=y,O=j,y=v,j=m,v=f(p,g,G,J),m=l(g,g,G,J),p=u,g=a,u=s,a=o,s=r,o=e,r=f(G,J,K,N),e=l(G,J,K,N)}c(this.h,0,r,e),c(this.h,2,s,o),c(this.h,4,u,a),c(this.h,6,p,g),c(this.h,8,v,m),c(this.h,10,y,j),c(this.h,12,L,O),c(this.h,14,B,W)},y.prototype._digest=function(t){return"hex"===t?r.toHex32(this.h,"big"):r.split32(this.h,"big")}},function(t,n,i){"use strict";i.r(n),i.d(n,"ripemd160",(function(){return e})),i.d(n,"sha256",(function(){return h})),i.d(n,"hmacSHA512",(function(){return s}));var r=i(3);function e(t){return new Uint8Array(r.ripemd160().update(t).digest())}function h(t){return new Uint8Array(r.sha256().update(t).digest())}function s(t,n){return new Uint8Array(r.hmac(r.sha512,t).update(n).digest())}},function(t,n){"function"==typeof Object.create?t.exports=function(t,n){n&&(t.super_=n,t.prototype=Object.create(n.prototype,{constructor:{value:t,enumerable:!1,writable:!0,configurable:!0}}))}:t.exports=function(t,n){if(n){t.super_=n;var i=function(){};i.prototype=n.prototype,t.prototype=new i,t.prototype.constructor=t}}},function(t,n,i){"use strict";n.sha1=i(10),n.sha224=i(11),n.sha256=i(5),n.sha384=i(12),n.sha512=i(6)},function(t,n,i){"use strict";var r=i(0),e=i(2),h=i(4),s=r.rotl32,o=r.sum32,u=r.sum32_5,a=h.ft_1,c=e.BlockHash,f=[1518500249,1859775393,2400959708,3395469782];function l(){if(!(this instanceof l))return new l;c.call(this),this.h=[1732584193,4023233417,2562383102,271733878,3285377520],this.W=new Array(80)}r.inherits(l,c),t.exports=l,l.blockSize=512,l.outSize=160,l.hmacStrength=80,l.padLength=64,l.prototype._update=function(t,n){for(var i=this.W,r=0;r<16;r++)i[r]=t[n+r];for(;r<i.length;r++)i[r]=s(i[r-3]^i[r-8]^i[r-14]^i[r-16],1);var e=this.h[0],h=this.h[1],c=this.h[2],l=this.h[3],p=this.h[4];for(r=0;r<i.length;r++){var g=~~(r/20),d=u(s(e,5),a(g,h,c,l),p,i[r],f[g]);p=l,l=c,c=s(h,30),h=e,e=d}this.h[0]=o(this.h[0],e),this.h[1]=o(this.h[1],h),this.h[2]=o(this.h[2],c),this.h[3]=o(this.h[3],l),this.h[4]=o(this.h[4],p)},l.prototype._digest=function(t){return"hex"===t?r.toHex32(this.h,"big"):r.split32(this.h,"big")}},function(t,n,i){"use strict";var r=i(0),e=i(5);function h(){if(!(this instanceof h))return new h;e.call(this),this.h=[3238371032,914150663,812702999,4144912697,4290775857,1750603025,1694076839,3204075428]}r.inherits(h,e),t.exports=h,h.blockSize=512,h.outSize=224,h.hmacStrength=192,h.padLength=64,h.prototype._digest=function(t){return"hex"===t?r.toHex32(this.h.slice(0,7),"big"):r.split32(this.h.slice(0,7),"big")}},function(t,n,i){"use strict";var r=i(0),e=i(6);function h(){if(!(this instanceof h))return new h;e.call(this),this.h=[3418070365,3238371032,1654270250,914150663,2438529370,812702999,355462360,4144912697,1731405415,4290775857,2394180231,1750603025,3675008525,1694076839,1203062813,3204075428]}r.inherits(h,e),t.exports=h,h.blockSize=1024,h.outSize=384,h.hmacStrength=192,h.padLength=128,h.prototype._digest=function(t){return"hex"===t?r.toHex32(this.h.slice(0,12),"big"):r.split32(this.h.slice(0,12),"big")}},function(t,n,i){"use strict";var r=i(0),e=i(2),h=r.rotl32,s=r.sum32,o=r.sum32_3,u=r.sum32_4,a=e.BlockHash;function c(){if(!(this instanceof c))return new c;a.call(this),this.h=[1732584193,4023233417,2562383102,271733878,3285377520],this.endian="little"}function f(t,n,i,r){return t<=15?n^i^r:t<=31?n&i|~n&r:t<=47?(n|~i)^r:t<=63?n&r|i&~r:n^(i|~r)}function l(t){return t<=15?0:t<=31?1518500249:t<=47?1859775393:t<=63?2400959708:2840853838}function p(t){return t<=15?1352829926:t<=31?1548603684:t<=47?1836072691:t<=63?2053994217:0}r.inherits(c,a),n.ripemd160=c,c.blockSize=512,c.outSize=160,c.hmacStrength=192,c.padLength=64,c.prototype._update=function(t,n){for(var i=this.h[0],r=this.h[1],e=this.h[2],a=this.h[3],c=this.h[4],m=i,y=r,b=e,S=a,k=c,x=0;x<80;x++){var z=s(h(u(i,f(x,r,e,a),t[g[x]+n],l(x)),_[x]),c);i=c,c=a,a=h(e,10),e=r,r=z,z=s(h(u(m,f(79-x,y,b,S),t[d[x]+n],p(x)),v[x]),k),m=k,k=S,S=h(b,10),b=y,y=z}z=o(this.h[1],e,S),this.h[1]=o(this.h[2],a,k),this.h[2]=o(this.h[3],c,m),this.h[3]=o(this.h[4],i,y),this.h[4]=o(this.h[0],r,b),this.h[0]=z},c.prototype._digest=function(t){return"hex"===t?r.toHex32(this.h,"little"):r.split32(this.h,"little")};var g=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,7,4,13,1,10,6,15,3,12,0,9,5,2,14,11,8,3,10,14,4,9,15,8,1,2,7,0,6,13,11,5,12,1,9,11,10,0,8,12,4,13,3,7,15,14,5,6,2,4,0,5,9,7,12,2,10,14,1,3,8,11,6,15,13],d=[5,14,7,0,9,2,11,4,13,6,15,8,1,10,3,12,6,11,3,7,0,13,5,10,14,15,8,12,4,9,1,2,15,5,1,3,7,14,6,9,11,8,12,2,10,0,4,13,8,6,4,1,3,11,15,0,5,12,2,13,9,7,10,14,12,15,10,4,1,5,8,7,6,2,13,14,0,3,9,11],_=[11,14,15,12,5,8,7,9,11,13,14,15,6,7,9,8,7,6,8,13,11,9,7,15,7,12,15,9,11,7,13,12,11,13,6,7,14,9,13,15,14,8,13,6,5,12,7,5,11,12,14,15,14,15,9,8,9,14,5,6,8,6,5,12,9,15,5,11,6,8,13,12,5,12,13,14,11,8,5,6],v=[8,9,9,11,13,15,15,5,7,7,8,11,14,14,12,6,9,13,15,7,12,8,9,11,7,7,12,7,6,15,13,11,9,7,15,11,8,6,6,14,12,13,5,14,13,13,7,5,15,5,8,11,14,14,6,14,6,9,12,9,12,5,15,8,8,5,12,9,12,5,14,6,8,13,6,5,15,13,11,11]},function(t,n,i){"use strict";var r=i(0),e=i(1);function h(t,n,i){if(!(this instanceof h))return new h(t,n,i);this.Hash=t,this.blockSize=t.blockSize/8,this.outSize=t.outSize/8,this.inner=null,this.outer=null,this._init(r.toArray(n,i))}t.exports=h,h.prototype._init=function(t){t.length>this.blockSize&&(t=(new this.Hash).update(t).digest()),e(t.length<=this.blockSize);for(var n=t.length;n<this.blockSize;n++)t.push(0);for(n=0;n<t.length;n++)t[n]^=54;for(this.inner=(new this.Hash).update(t),n=0;n<t.length;n++)t[n]^=106;this.outer=(new this.Hash).update(t)},h.prototype.update=function(t,n){return this.inner.update(t,n),this},h.prototype.digest=function(t){return this.outer.update(this.inner.digest()),this.outer.digest(t)}}]);