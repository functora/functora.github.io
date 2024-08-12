const scriptRel = "modulepreload";
const assetsURL = function(dep) {
  return "/" + dep;
};
const seen = {};
const __vitePreload = function preload(baseModule, deps, importerUrl) {
  let promise = Promise.resolve();
  if (deps && deps.length > 0) {
    document.getElementsByTagName("link");
    const cspNonceMeta = document.querySelector(
      "meta[property=csp-nonce]"
    );
    const cspNonce = (cspNonceMeta == null ? void 0 : cspNonceMeta.nonce) || (cspNonceMeta == null ? void 0 : cspNonceMeta.getAttribute("nonce"));
    promise = Promise.all(
      deps.map((dep) => {
        dep = assetsURL(dep);
        if (dep in seen) return;
        seen[dep] = true;
        const isCss = dep.endsWith(".css");
        const cssSelector = isCss ? '[rel="stylesheet"]' : "";
        if (document.querySelector(`link[href="${dep}"]${cssSelector}`)) {
          return;
        }
        const link = document.createElement("link");
        link.rel = isCss ? "stylesheet" : scriptRel;
        if (!isCss) {
          link.as = "script";
          link.crossOrigin = "";
        }
        link.href = dep;
        if (cspNonce) {
          link.setAttribute("nonce", cspNonce);
        }
        document.head.appendChild(link);
        if (isCss) {
          return new Promise((res, rej) => {
            link.addEventListener("load", res);
            link.addEventListener(
              "error",
              () => rej(new Error(`Unable to preload CSS for ${dep}`))
            );
          });
        }
      })
    );
  }
  return promise.then(() => baseModule()).catch((err) => {
    const e = new Event("vite:preloadError", { cancelable: true });
    e.payload = err;
    window.dispatchEvent(e);
    if (!e.defaultPrevented) {
      throw err;
    }
  });
};
var __extends = /* @__PURE__ */ function() {
  var e = function(t, n) {
    e = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(e2, t2) {
      e2.__proto__ = t2;
    } || function(e2, t2) {
      for (var n2 in t2) if (Object.prototype.hasOwnProperty.call(t2, n2)) e2[n2] = t2[n2];
    };
    return e(t, n);
  };
  return function(t, n) {
    if (typeof n !== "function" && n !== null) throw new TypeError("Class extends value " + String(n) + " is not a constructor or null");
    e(t, n);
    function r() {
      this.constructor = t;
    }
    t.prototype = n === null ? Object.create(n) : (r.prototype = n.prototype, new r());
  };
}();
var __awaiter = function(e, t, n, r) {
  function a(e2) {
    return e2 instanceof n ? e2 : new n(function(t2) {
      t2(e2);
    });
  }
  return new (n || (n = Promise))(function(n2, o) {
    function s(e2) {
      try {
        l(r.next(e2));
      } catch (e3) {
        o(e3);
      }
    }
    function i(e2) {
      try {
        l(r["throw"](e2));
      } catch (e3) {
        o(e3);
      }
    }
    function l(e2) {
      e2.done ? n2(e2.value) : a(e2.value).then(s, i);
    }
    l((r = r.apply(e, t || [])).next());
  });
};
var __generator = function(e, t) {
  var n = { label: 0, sent: function() {
    if (o[0] & 1) throw o[1];
    return o[1];
  }, trys: [], ops: [] }, r, a, o, s;
  return s = { next: i(0), throw: i(1), return: i(2) }, typeof Symbol === "function" && (s[Symbol.iterator] = function() {
    return this;
  }), s;
  function i(e2) {
    return function(t2) {
      return l([e2, t2]);
    };
  }
  function l(i2) {
    if (r) throw new TypeError("Generator is already executing.");
    while (s && (s = 0, i2[0] && (n = 0)), n) try {
      if (r = 1, a && (o = i2[0] & 2 ? a["return"] : i2[0] ? a["throw"] || ((o = a["return"]) && o.call(a), 0) : a.next) && !(o = o.call(a, i2[1])).done) return o;
      if (a = 0, o) i2 = [i2[0] & 2, o.value];
      switch (i2[0]) {
        case 0:
        case 1:
          o = i2;
          break;
        case 4:
          n.label++;
          return { value: i2[1], done: false };
        case 5:
          n.label++;
          a = i2[1];
          i2 = [0];
          continue;
        case 7:
          i2 = n.ops.pop();
          n.trys.pop();
          continue;
        default:
          if (!(o = n.trys, o = o.length > 0 && o[o.length - 1]) && (i2[0] === 6 || i2[0] === 2)) {
            n = 0;
            continue;
          }
          if (i2[0] === 3 && (!o || i2[1] > o[0] && i2[1] < o[3])) {
            n.label = i2[1];
            break;
          }
          if (i2[0] === 6 && n.label < o[1]) {
            n.label = o[1];
            o = i2;
            break;
          }
          if (o && n.label < o[2]) {
            n.label = o[2];
            n.ops.push(i2);
            break;
          }
          if (o[2]) n.ops.pop();
          n.trys.pop();
          continue;
      }
      i2 = t.call(e, n);
    } catch (e2) {
      i2 = [6, e2];
      a = 0;
    } finally {
      r = o = 0;
    }
    if (i2[0] & 5) throw i2[1];
    return { value: i2[0] ? i2[1] : void 0, done: true };
  }
};
var __spreadArray = function(e, t, n) {
  if (n || arguments.length === 2) for (var r = 0, a = t.length, o; r < a; r++) {
    if (o || !(r in t)) {
      if (!o) o = Array.prototype.slice.call(t, 0, r);
      o[r] = t[r];
    }
  }
  return e.concat(o || Array.prototype.slice.call(t));
};
var NAMESPACE = "ionicpwaelements";
var scopeId;
var hostTagName;
var isSvgMode = false;
var queuePending = false;
var createTime = function(e, t) {
  {
    return function() {
      return;
    };
  }
};
var uniqueTime = function(e, t) {
  {
    return function() {
      return;
    };
  }
};
var HYDRATED_CSS = "{visibility:hidden}.hydrated{visibility:inherit}";
var EMPTY_OBJ = {};
var SVG_NS = "http://www.w3.org/2000/svg";
var HTML_NS = "http://www.w3.org/1999/xhtml";
var isDef = function(e) {
  return e != null;
};
var isComplexType = function(e) {
  e = typeof e;
  return e === "object" || e === "function";
};
function queryNonceMetaTagContent(e) {
  var t, n, r;
  return (r = (n = (t = e.head) === null || t === void 0 ? void 0 : t.querySelector('meta[name="csp-nonce"]')) === null || n === void 0 ? void 0 : n.getAttribute("content")) !== null && r !== void 0 ? r : void 0;
}
var h = function(e, t) {
  var n = [];
  for (var r = 2; r < arguments.length; r++) {
    n[r - 2] = arguments[r];
  }
  var a = null;
  var o = false;
  var s = false;
  var i = [];
  var l = function(t2) {
    for (var n2 = 0; n2 < t2.length; n2++) {
      a = t2[n2];
      if (Array.isArray(a)) {
        l(a);
      } else if (a != null && typeof a !== "boolean") {
        if (o = typeof e !== "function" && !isComplexType(a)) {
          a = String(a);
        }
        if (o && s) {
          i[i.length - 1].$text$ += a;
        } else {
          i.push(o ? newVNode(null, a) : a);
        }
        s = o;
      }
    }
  };
  l(n);
  if (t) {
    {
      var u = t.className || t.class;
      if (u) {
        t.class = typeof u !== "object" ? u : Object.keys(u).filter(function(e2) {
          return u[e2];
        }).join(" ");
      }
    }
  }
  var c = newVNode(e, null);
  c.$attrs$ = t;
  if (i.length > 0) {
    c.$children$ = i;
  }
  return c;
};
var newVNode = function(e, t) {
  var n = { $flags$: 0, $tag$: e, $text$: t, $elm$: null, $children$: null };
  {
    n.$attrs$ = null;
  }
  return n;
};
var Host = {};
var isHost = function(e) {
  return e && e.$tag$ === Host;
};
var parsePropertyValue = function(e, t) {
  if (e != null && !isComplexType(e)) {
    if (t & 4) {
      return e === "false" ? false : e === "" || !!e;
    }
    if (t & 2) {
      return parseFloat(e);
    }
    if (t & 1) {
      return String(e);
    }
    return e;
  }
  return e;
};
var getElement = function(e) {
  return getHostRef(e).$hostElement$;
};
var createEvent = function(e, t, n) {
  var r = getElement(e);
  return { emit: function(e2) {
    return emitEvent(r, t, { bubbles: !!(n & 4), composed: !!(n & 2), cancelable: !!(n & 1), detail: e2 });
  } };
};
var emitEvent = function(e, t, n) {
  var r = plt.ce(t, n);
  e.dispatchEvent(r);
  return r;
};
var rootAppliedStyles = /* @__PURE__ */ new WeakMap();
var registerStyle = function(e, t, n) {
  var r = styles.get(e);
  if (supportsConstructableStylesheets && n) {
    r = r || new CSSStyleSheet();
    if (typeof r === "string") {
      r = t;
    } else {
      r.replaceSync(t);
    }
  } else {
    r = t;
  }
  styles.set(e, r);
};
var addStyle = function(e, t, n, r) {
  var a;
  var o = getScopeId(t);
  var s = styles.get(o);
  e = e.nodeType === 11 ? e : doc;
  if (s) {
    if (typeof s === "string") {
      e = e.head || e;
      var i = rootAppliedStyles.get(e);
      var l = void 0;
      if (!i) {
        rootAppliedStyles.set(e, i = /* @__PURE__ */ new Set());
      }
      if (!i.has(o)) {
        {
          {
            l = doc.createElement("style");
            l.innerHTML = s;
          }
          var u = (a = plt.$nonce$) !== null && a !== void 0 ? a : queryNonceMetaTagContent(doc);
          if (u != null) {
            l.setAttribute("nonce", u);
          }
          e.insertBefore(l, e.querySelector("link"));
        }
        if (i) {
          i.add(o);
        }
      }
    } else if (!e.adoptedStyleSheets.includes(s)) {
      e.adoptedStyleSheets = __spreadArray(__spreadArray([], e.adoptedStyleSheets, true), [s], false);
    }
  }
  return o;
};
var attachStyles = function(e) {
  var t = e.$cmpMeta$;
  var n = e.$hostElement$;
  var r = t.$flags$;
  var a = createTime("attachStyles", t.$tagName$);
  var o = addStyle(n.shadowRoot ? n.shadowRoot : n.getRootNode(), t);
  if (r & 10) {
    n["s-sc"] = o;
    n.classList.add(o + "-h");
  }
  a();
};
var getScopeId = function(e, t) {
  return "sc-" + e.$tagName$;
};
var setAccessor = function(e, t, n, r, a, o) {
  if (n !== r) {
    var s = isMemberInElement(e, t);
    var i = t.toLowerCase();
    if (t === "class") {
      var l = e.classList;
      var u = parseClassList(n);
      var c = parseClassList(r);
      l.remove.apply(l, u.filter(function(e2) {
        return e2 && !c.includes(e2);
      }));
      l.add.apply(l, c.filter(function(e2) {
        return e2 && !u.includes(e2);
      }));
    } else if (t === "style") {
      {
        for (var f in n) {
          if (!r || r[f] == null) {
            if (f.includes("-")) {
              e.style.removeProperty(f);
            } else {
              e.style[f] = "";
            }
          }
        }
      }
      for (var f in r) {
        if (!n || r[f] !== n[f]) {
          if (f.includes("-")) {
            e.style.setProperty(f, r[f]);
          } else {
            e.style[f] = r[f];
          }
        }
      }
    } else if (t === "ref") {
      if (r) {
        r(e);
      }
    } else if (!s && t[0] === "o" && t[1] === "n") {
      if (t[2] === "-") {
        t = t.slice(3);
      } else if (isMemberInElement(win, i)) {
        t = i.slice(2);
      } else {
        t = i[2] + t.slice(3);
      }
      if (n) {
        plt.rel(e, t, n, false);
      }
      if (r) {
        plt.ael(e, t, r, false);
      }
    } else {
      var $ = isComplexType(r);
      if ((s || $ && r !== null) && !a) {
        try {
          if (!e.tagName.includes("-")) {
            var d = r == null ? "" : r;
            if (t === "list") {
              s = false;
            } else if (n == null || e[t] != d) {
              e[t] = d;
            }
          } else {
            e[t] = r;
          }
        } catch (e2) {
        }
      }
      if (r == null || r === false) {
        if (r !== false || e.getAttribute(t) === "") {
          {
            e.removeAttribute(t);
          }
        }
      } else if ((!s || o & 4 || a) && !$) {
        r = r === true ? "" : r;
        {
          e.setAttribute(t, r);
        }
      }
    }
  }
};
var parseClassListRegex = /\s/;
var parseClassList = function(e) {
  return !e ? [] : e.split(parseClassListRegex);
};
var updateElement = function(e, t, n, r) {
  var a = t.$elm$.nodeType === 11 && t.$elm$.host ? t.$elm$.host : t.$elm$;
  var o = e && e.$attrs$ || EMPTY_OBJ;
  var s = t.$attrs$ || EMPTY_OBJ;
  {
    for (r in o) {
      if (!(r in s)) {
        setAccessor(a, r, o[r], void 0, n, t.$flags$);
      }
    }
  }
  for (r in s) {
    setAccessor(a, r, o[r], s[r], n, t.$flags$);
  }
};
var createElm = function(e, t, n, r) {
  var a = t.$children$[n];
  var o = 0;
  var s;
  var i;
  if (a.$text$ !== null) {
    s = a.$elm$ = doc.createTextNode(a.$text$);
  } else {
    if (!isSvgMode) {
      isSvgMode = a.$tag$ === "svg";
    }
    s = a.$elm$ = doc.createElementNS(isSvgMode ? SVG_NS : HTML_NS, a.$tag$);
    if (isSvgMode && a.$tag$ === "foreignObject") {
      isSvgMode = false;
    }
    {
      updateElement(null, a, isSvgMode);
    }
    if (isDef(scopeId) && s["s-si"] !== scopeId) {
      s.classList.add(s["s-si"] = scopeId);
    }
    if (a.$children$) {
      for (o = 0; o < a.$children$.length; ++o) {
        i = createElm(e, a, o);
        if (i) {
          s.appendChild(i);
        }
      }
    }
    {
      if (a.$tag$ === "svg") {
        isSvgMode = false;
      } else if (s.tagName === "foreignObject") {
        isSvgMode = true;
      }
    }
  }
  return s;
};
var addVnodes = function(e, t, n, r, a, o) {
  var s = e;
  var i;
  if (s.shadowRoot && s.tagName === hostTagName) {
    s = s.shadowRoot;
  }
  for (; a <= o; ++a) {
    if (r[a]) {
      i = createElm(null, n, a);
      if (i) {
        r[a].$elm$ = i;
        s.insertBefore(i, t);
      }
    }
  }
};
var removeVnodes = function(e, t, n) {
  for (var r = t; r <= n; ++r) {
    var a = e[r];
    if (a) {
      var o = a.$elm$;
      nullifyVNodeRefs(a);
      if (o) {
        o.remove();
      }
    }
  }
};
var updateChildren = function(e, t, n, r) {
  var a = 0;
  var o = 0;
  var s = t.length - 1;
  var i = t[0];
  var l = t[s];
  var u = r.length - 1;
  var c = r[0];
  var f = r[u];
  var $;
  while (a <= s && o <= u) {
    if (i == null) {
      i = t[++a];
    } else if (l == null) {
      l = t[--s];
    } else if (c == null) {
      c = r[++o];
    } else if (f == null) {
      f = r[--u];
    } else if (isSameVnode(i, c)) {
      patch(i, c);
      i = t[++a];
      c = r[++o];
    } else if (isSameVnode(l, f)) {
      patch(l, f);
      l = t[--s];
      f = r[--u];
    } else if (isSameVnode(i, f)) {
      patch(i, f);
      e.insertBefore(i.$elm$, l.$elm$.nextSibling);
      i = t[++a];
      f = r[--u];
    } else if (isSameVnode(l, c)) {
      patch(l, c);
      e.insertBefore(l.$elm$, i.$elm$);
      l = t[--s];
      c = r[++o];
    } else {
      {
        $ = createElm(t && t[o], n, o);
        c = r[++o];
      }
      if ($) {
        {
          i.$elm$.parentNode.insertBefore($, i.$elm$);
        }
      }
    }
  }
  if (a > s) {
    addVnodes(e, r[u + 1] == null ? null : r[u + 1].$elm$, n, r, o, u);
  } else if (o > u) {
    removeVnodes(t, a, s);
  }
};
var isSameVnode = function(e, t) {
  if (e.$tag$ === t.$tag$) {
    return true;
  }
  return false;
};
var patch = function(e, t) {
  var n = t.$elm$ = e.$elm$;
  var r = e.$children$;
  var a = t.$children$;
  var o = t.$tag$;
  var s = t.$text$;
  if (s === null) {
    {
      isSvgMode = o === "svg" ? true : o === "foreignObject" ? false : isSvgMode;
    }
    {
      {
        updateElement(e, t, isSvgMode);
      }
    }
    if (r !== null && a !== null) {
      updateChildren(n, r, t, a);
    } else if (a !== null) {
      if (e.$text$ !== null) {
        n.textContent = "";
      }
      addVnodes(n, null, t, a, 0, a.length - 1);
    } else if (r !== null) {
      removeVnodes(r, 0, r.length - 1);
    }
    if (isSvgMode && o === "svg") {
      isSvgMode = false;
    }
  } else if (e.$text$ !== s) {
    n.data = s;
  }
};
var nullifyVNodeRefs = function(e) {
  {
    e.$attrs$ && e.$attrs$.ref && e.$attrs$.ref(null);
    e.$children$ && e.$children$.map(nullifyVNodeRefs);
  }
};
var renderVdom = function(e, t) {
  var n = e.$hostElement$;
  var r = e.$vnode$ || newVNode(null, null);
  var a = isHost(t) ? t : h(null, null, t);
  hostTagName = n.tagName;
  a.$tag$ = null;
  a.$flags$ |= 4;
  e.$vnode$ = a;
  a.$elm$ = r.$elm$ = n.shadowRoot || n;
  {
    scopeId = n["s-sc"];
  }
  patch(r, a);
};
var attachToAncestor = function(e, t) {
  if (t && !e.$onRenderResolve$ && t["s-p"]) {
    t["s-p"].push(new Promise(function(t2) {
      return e.$onRenderResolve$ = t2;
    }));
  }
};
var scheduleUpdate = function(e, t) {
  {
    e.$flags$ |= 16;
  }
  if (e.$flags$ & 4) {
    e.$flags$ |= 512;
    return;
  }
  attachToAncestor(e, e.$ancestorComponent$);
  var n = function() {
    return dispatchHooks(e, t);
  };
  return writeTask(n);
};
var dispatchHooks = function(e, t) {
  var n = createTime("scheduleUpdate", e.$cmpMeta$.$tagName$);
  var r = e.$lazyInstance$;
  var a;
  if (t) {
    {
      e.$flags$ |= 256;
      if (e.$queuedListeners$) {
        e.$queuedListeners$.map(function(e2) {
          var t2 = e2[0], n2 = e2[1];
          return safeCall(r, t2, n2);
        });
        e.$queuedListeners$ = void 0;
      }
    }
  }
  n();
  return enqueue(a, function() {
    return updateComponent(e, r, t);
  });
};
var enqueue = function(e, t) {
  return isPromisey(e) ? e.then(t) : t();
};
var isPromisey = function(e) {
  return e instanceof Promise || e && e.then && typeof e.then === "function";
};
var updateComponent = function(e, t, n) {
  return __awaiter(void 0, void 0, void 0, function() {
    var r, a, o, s, i, l, u;
    return __generator(this, function(c) {
      a = e.$hostElement$;
      o = createTime("update", e.$cmpMeta$.$tagName$);
      s = a["s-rc"];
      if (n) {
        attachStyles(e);
      }
      i = createTime("render", e.$cmpMeta$.$tagName$);
      {
        callRender(e, t);
      }
      if (s) {
        s.map(function(e2) {
          return e2();
        });
        a["s-rc"] = void 0;
      }
      i();
      o();
      {
        l = (r = a["s-p"]) !== null && r !== void 0 ? r : [];
        u = function() {
          return postUpdateComponent(e);
        };
        if (l.length === 0) {
          u();
        } else {
          Promise.all(l).then(u);
          e.$flags$ |= 4;
          l.length = 0;
        }
      }
      return [2];
    });
  });
};
var callRender = function(e, t, n) {
  try {
    t = t.render();
    {
      e.$flags$ &= ~16;
    }
    {
      e.$flags$ |= 2;
    }
    {
      {
        {
          renderVdom(e, t);
        }
      }
    }
  } catch (t2) {
    consoleError(t2, e.$hostElement$);
  }
  return null;
};
var postUpdateComponent = function(e) {
  e.$cmpMeta$.$tagName$;
  var n = e.$hostElement$;
  var r = createTime();
  var a = e.$lazyInstance$;
  var o = e.$ancestorComponent$;
  if (!(e.$flags$ & 64)) {
    e.$flags$ |= 64;
    {
      addHydratedFlag(n);
    }
    {
      safeCall(a, "componentDidLoad");
    }
    r();
    {
      e.$onReadyResolve$(n);
      if (!o) {
        appDidLoad();
      }
    }
  } else {
    r();
  }
  {
    e.$onInstanceResolve$(n);
  }
  {
    if (e.$onRenderResolve$) {
      e.$onRenderResolve$();
      e.$onRenderResolve$ = void 0;
    }
    if (e.$flags$ & 512) {
      nextTick(function() {
        return scheduleUpdate(e, false);
      });
    }
    e.$flags$ &= ~(4 | 512);
  }
};
var forceUpdate = function(e) {
  {
    var t = getHostRef(e);
    var n = t.$hostElement$.isConnected;
    if (n && (t.$flags$ & (2 | 16)) === 2) {
      scheduleUpdate(t, false);
    }
    return n;
  }
};
var appDidLoad = function(e) {
  {
    addHydratedFlag(doc.documentElement);
  }
  nextTick(function() {
    return emitEvent(win, "appload", { detail: { namespace: NAMESPACE } });
  });
};
var safeCall = function(e, t, n) {
  if (e && e[t]) {
    try {
      return e[t](n);
    } catch (e2) {
      consoleError(e2);
    }
  }
  return void 0;
};
var addHydratedFlag = function(e) {
  return e.classList.add("hydrated");
};
var getValue = function(e, t) {
  return getHostRef(e).$instanceValues$.get(t);
};
var setValue = function(e, t, n, r) {
  var a = getHostRef(e);
  var o = a.$instanceValues$.get(t);
  var s = a.$flags$;
  var i = a.$lazyInstance$;
  n = parsePropertyValue(n, r.$members$[t][0]);
  var l = Number.isNaN(o) && Number.isNaN(n);
  var u = n !== o && !l;
  if ((!(s & 8) || o === void 0) && u) {
    a.$instanceValues$.set(t, n);
    if (i) {
      if ((s & (2 | 16)) === 2) {
        scheduleUpdate(a, false);
      }
    }
  }
};
var proxyComponent = function(e, t, n) {
  if (t.$members$) {
    var r = Object.entries(t.$members$);
    var a = e.prototype;
    r.map(function(e2) {
      var r2 = e2[0], o2 = e2[1][0];
      if (o2 & 31 || n & 2 && o2 & 32) {
        Object.defineProperty(a, r2, { get: function() {
          return getValue(this, r2);
        }, set: function(e3) {
          setValue(this, r2, e3, t);
        }, configurable: true, enumerable: true });
      } else if (n & 1 && o2 & 64) {
        Object.defineProperty(a, r2, { value: function() {
          var e3 = [];
          for (var t2 = 0; t2 < arguments.length; t2++) {
            e3[t2] = arguments[t2];
          }
          var n2 = getHostRef(this);
          return n2.$onInstancePromise$.then(function() {
            var t3;
            return (t3 = n2.$lazyInstance$)[r2].apply(t3, e3);
          });
        } });
      }
    });
    if (n & 1) {
      var o = /* @__PURE__ */ new Map();
      a.attributeChangedCallback = function(e2, t2, n2) {
        var r2 = this;
        plt.jmp(function() {
          var t3 = o.get(e2);
          if (r2.hasOwnProperty(t3)) {
            n2 = r2[t3];
            delete r2[t3];
          } else if (a.hasOwnProperty(t3) && typeof r2[t3] === "number" && r2[t3] == n2) {
            return;
          }
          r2[t3] = n2 === null && typeof r2[t3] === "boolean" ? false : n2;
        });
      };
      e.observedAttributes = r.filter(function(e2) {
        e2[0];
        var n2 = e2[1];
        return n2[0] & 15;
      }).map(function(e2) {
        var t2 = e2[0], n2 = e2[1];
        var r2 = n2[1] || t2;
        o.set(r2, t2);
        return r2;
      });
    }
  }
  return e;
};
var initializeComponent = function(e, t, n, r, a) {
  return __awaiter(void 0, void 0, void 0, function() {
    var e2, r2, o, s, i, l, u;
    return __generator(this, function(c) {
      switch (c.label) {
        case 0:
          if (!((t.$flags$ & 32) === 0)) return [3, 3];
          t.$flags$ |= 32;
          a = loadModule(n);
          if (!a.then) return [3, 2];
          e2 = uniqueTime();
          return [4, a];
        case 1:
          a = c.sent();
          e2();
          c.label = 2;
        case 2:
          if (!a.isProxied) {
            proxyComponent(a, n, 2);
            a.isProxied = true;
          }
          r2 = createTime("createInstance", n.$tagName$);
          {
            t.$flags$ |= 8;
          }
          try {
            new a(t);
          } catch (e3) {
            consoleError(e3);
          }
          {
            t.$flags$ &= ~8;
          }
          r2();
          if (a.style) {
            o = a.style;
            s = getScopeId(n);
            if (!styles.has(s)) {
              i = createTime("registerStyles", n.$tagName$);
              registerStyle(s, o, !!(n.$flags$ & 1));
              i();
            }
          }
          c.label = 3;
        case 3:
          l = t.$ancestorComponent$;
          u = function() {
            return scheduleUpdate(t, true);
          };
          if (l && l["s-rc"]) {
            l["s-rc"].push(u);
          } else {
            u();
          }
          return [2];
      }
    });
  });
};
var connectedCallback = function(e) {
  if ((plt.$flags$ & 1) === 0) {
    var t = getHostRef(e);
    var n = t.$cmpMeta$;
    var r = createTime("connectedCallback", n.$tagName$);
    if (!(t.$flags$ & 1)) {
      t.$flags$ |= 1;
      {
        var a = e;
        while (a = a.parentNode || a.host) {
          if (a["s-p"]) {
            attachToAncestor(t, t.$ancestorComponent$ = a);
            break;
          }
        }
      }
      if (n.$members$) {
        Object.entries(n.$members$).map(function(t2) {
          var n2 = t2[0], r2 = t2[1][0];
          if (r2 & 31 && e.hasOwnProperty(n2)) {
            var a2 = e[n2];
            delete e[n2];
            e[n2] = a2;
          }
        });
      }
      {
        initializeComponent(e, t, n);
      }
    } else {
      addHostEventListeners(e, t, n.$listeners$);
    }
    r();
  }
};
var disconnectedCallback = function(e) {
  if ((plt.$flags$ & 1) === 0) {
    var t = getHostRef(e);
    var n = t.$lazyInstance$;
    {
      if (t.$rmListeners$) {
        t.$rmListeners$.map(function(e2) {
          return e2();
        });
        t.$rmListeners$ = void 0;
      }
    }
    {
      safeCall(n, "disconnectedCallback");
    }
  }
};
var bootstrapLazy = function(e, t) {
  if (t === void 0) {
    t = {};
  }
  var n;
  var r = createTime();
  var a = [];
  var o = t.exclude || [];
  var s = win.customElements;
  var i = doc.head;
  var l = i.querySelector("meta[charset]");
  var u = doc.createElement("style");
  var c = [];
  var f;
  var $ = true;
  Object.assign(plt, t);
  plt.$resourcesUrl$ = new URL(t.resourcesUrl || "./", doc.baseURI).href;
  e.map(function(e2) {
    e2[1].map(function(t2) {
      var n2 = { $flags$: t2[0], $tagName$: t2[1], $members$: t2[2], $listeners$: t2[3] };
      {
        n2.$members$ = t2[2];
      }
      {
        n2.$listeners$ = t2[3];
      }
      var r2 = n2.$tagName$;
      var i2 = function(e3) {
        __extends(t3, e3);
        function t3(t4) {
          var r3 = e3.call(this, t4) || this;
          t4 = r3;
          registerHost(t4, n2);
          if (n2.$flags$ & 1) {
            {
              {
                t4.attachShadow({ mode: "open" });
              }
            }
          }
          return r3;
        }
        t3.prototype.connectedCallback = function() {
          var e4 = this;
          if (f) {
            clearTimeout(f);
            f = null;
          }
          if ($) {
            c.push(this);
          } else {
            plt.jmp(function() {
              return connectedCallback(e4);
            });
          }
        };
        t3.prototype.disconnectedCallback = function() {
          var e4 = this;
          plt.jmp(function() {
            return disconnectedCallback(e4);
          });
        };
        t3.prototype.componentOnReady = function() {
          return getHostRef(this).$onReadyPromise$;
        };
        return t3;
      }(HTMLElement);
      n2.$lazyBundleId$ = e2[0];
      if (!o.includes(r2) && !s.get(r2)) {
        a.push(r2);
        s.define(r2, proxyComponent(i2, n2, 1));
      }
    });
  });
  {
    u.innerHTML = a + HYDRATED_CSS;
    u.setAttribute("data-styles", "");
    var d = (n = plt.$nonce$) !== null && n !== void 0 ? n : queryNonceMetaTagContent(doc);
    if (d != null) {
      u.setAttribute("nonce", d);
    }
    i.insertBefore(u, l ? l.nextSibling : i.firstChild);
  }
  $ = false;
  if (c.length) {
    c.map(function(e2) {
      return e2.connectedCallback();
    });
  } else {
    {
      plt.jmp(function() {
        return f = setTimeout(appDidLoad, 30);
      });
    }
  }
  r();
};
var addHostEventListeners = function(e, t, n, r) {
  if (n) {
    n.map(function(n2) {
      var r2 = n2[0], a = n2[1], o = n2[2];
      var s = getHostListenerTarget(e, r2);
      var i = hostListenerProxy(t, o);
      var l = hostListenerOpts(r2);
      plt.ael(s, a, i, l);
      (t.$rmListeners$ = t.$rmListeners$ || []).push(function() {
        return plt.rel(s, a, i, l);
      });
    });
  }
};
var hostListenerProxy = function(e, t) {
  return function(n) {
    try {
      {
        if (e.$flags$ & 256) {
          e.$lazyInstance$[t](n);
        } else {
          (e.$queuedListeners$ = e.$queuedListeners$ || []).push([t, n]);
        }
      }
    } catch (e2) {
      consoleError(e2);
    }
  };
};
var getHostListenerTarget = function(e, t) {
  if (t & 16) return doc.body;
  return e;
};
var hostListenerOpts = function(e) {
  return (e & 2) !== 0;
};
var hostRefs = /* @__PURE__ */ new WeakMap();
var getHostRef = function(e) {
  return hostRefs.get(e);
};
var registerInstance = function(e, t) {
  return hostRefs.set(t.$lazyInstance$ = e, t);
};
var registerHost = function(e, t) {
  var n = { $flags$: 0, $hostElement$: e, $cmpMeta$: t, $instanceValues$: /* @__PURE__ */ new Map() };
  {
    n.$onInstancePromise$ = new Promise(function(e2) {
      return n.$onInstanceResolve$ = e2;
    });
  }
  {
    n.$onReadyPromise$ = new Promise(function(e2) {
      return n.$onReadyResolve$ = e2;
    });
    e["s-p"] = [];
    e["s-rc"] = [];
  }
  addHostEventListeners(e, n, t.$listeners$);
  return hostRefs.set(e, n);
};
var isMemberInElement = function(e, t) {
  return t in e;
};
var consoleError = function(e, t) {
  return (0, console.error)(e, t);
};
var cmpModules = /* @__PURE__ */ new Map();
var loadModule = function(e, t, n) {
  var r = e.$tagName$.replace(/-/g, "_");
  var a = e.$lazyBundleId$;
  var o = cmpModules.get(a);
  if (o) {
    return o[r];
  }
  {
    var s = function(e2) {
      cmpModules.set(a, e2);
      return e2[r];
    };
    switch (a) {
      case "pwa-action-sheet":
        return __vitePreload(() => import("./pwa-action-sheet.entry.js"), true ? [] : void 0).then(s, consoleError);
      case "pwa-camera-modal":
        return __vitePreload(() => import("./pwa-camera-modal.entry.js"), true ? [] : void 0).then(s, consoleError);
      case "pwa-toast":
        return __vitePreload(() => import("./pwa-toast.entry.js"), true ? [] : void 0).then(s, consoleError);
      case "pwa-camera-modal-instance":
        return __vitePreload(() => import("./pwa-camera-modal-instance.entry.js"), true ? [] : void 0).then(s, consoleError);
      case "pwa-camera":
        return __vitePreload(() => import("./pwa-camera.entry.js"), true ? [] : void 0).then(s, consoleError);
    }
  }
  return __vitePreload(() => import("./".concat(a, ".entry.js").concat("")), true ? [] : void 0).then(function(e2) {
    {
      cmpModules.set(a, e2);
    }
    return e2[r];
  }, consoleError);
};
var styles = /* @__PURE__ */ new Map();
var win = typeof window !== "undefined" ? window : {};
var doc = win.document || { head: {} };
var plt = { $flags$: 0, $resourcesUrl$: "", jmp: function(e) {
  return e();
}, raf: function(e) {
  return requestAnimationFrame(e);
}, ael: function(e, t, n, r) {
  return e.addEventListener(t, n, r);
}, rel: function(e, t, n, r) {
  return e.removeEventListener(t, n, r);
}, ce: function(e, t) {
  return new CustomEvent(e, t);
} };
var promiseResolve = function(e) {
  return Promise.resolve(e);
};
var supportsConstructableStylesheets = function() {
  try {
    new CSSStyleSheet();
    return typeof new CSSStyleSheet().replaceSync === "function";
  } catch (e) {
  }
  return false;
}();
var queueDomReads = [];
var queueDomWrites = [];
var queueTask = function(e, t) {
  return function(n) {
    e.push(n);
    if (!queuePending) {
      queuePending = true;
      if (plt.$flags$ & 4) {
        nextTick(flush);
      } else {
        plt.raf(flush);
      }
    }
  };
};
var consume = function(e) {
  for (var t = 0; t < e.length; t++) {
    try {
      e[t](performance.now());
    } catch (e2) {
      consoleError(e2);
    }
  }
  e.length = 0;
};
var flush = function() {
  consume(queueDomReads);
  {
    consume(queueDomWrites);
    if (queuePending = queueDomReads.length > 0) {
      plt.raf(flush);
    }
  }
};
var nextTick = function(e) {
  return promiseResolve().then(e);
};
var writeTask = queueTask(queueDomWrites);
var patchEsm = function() {
  return promiseResolve();
};
var defineCustomElements = function(e, o) {
  if (typeof window === "undefined") return Promise.resolve();
  return patchEsm().then(function() {
    return bootstrapLazy([["pwa-camera-modal", [[1, "pwa-camera-modal", { facingMode: [1, "facing-mode"], hidePicker: [4, "hide-picker"], present: [64], dismiss: [64] }]]], ["pwa-action-sheet", [[1, "pwa-action-sheet", { header: [1], cancelable: [4], options: [16], open: [32] }]]], ["pwa-toast", [[1, "pwa-toast", { message: [1], duration: [2], closing: [32] }]]], ["pwa-camera", [[1, "pwa-camera", { facingMode: [1, "facing-mode"], handlePhoto: [16], hidePicker: [4, "hide-picker"], handleNoDeviceError: [16], noDevicesText: [1, "no-devices-text"], noDevicesButtonText: [1, "no-devices-button-text"], photo: [32], photoSrc: [32], showShutterOverlay: [32], flashIndex: [32], hasCamera: [32], rotation: [32], deviceError: [32] }]]], ["pwa-camera-modal-instance", [[1, "pwa-camera-modal-instance", { facingMode: [1, "facing-mode"], hidePicker: [4, "hide-picker"], noDevicesText: [1, "no-devices-text"], noDevicesButtonText: [1, "no-devices-button-text"] }, [[16, "keyup", "handleBackdropKeyUp"]]]]]], o);
  });
};
(function() {
  if ("undefined" !== typeof window && void 0 !== window.Reflect && void 0 !== window.customElements) {
    var a = HTMLElement;
    window.HTMLElement = function() {
      return Reflect.construct(a, [], this.constructor);
    };
    HTMLElement.prototype = a.prototype;
    HTMLElement.prototype.constructor = HTMLElement;
    Object.setPrototypeOf(HTMLElement, a);
  }
})();
/*! Capacitor: https://capacitorjs.com/ - MIT License */
const createCapacitorPlatforms = (win2) => {
  const defaultPlatformMap = /* @__PURE__ */ new Map();
  defaultPlatformMap.set("web", { name: "web" });
  const capPlatforms = win2.CapacitorPlatforms || {
    currentPlatform: { name: "web" },
    platforms: defaultPlatformMap
  };
  const addPlatform = (name, platform) => {
    capPlatforms.platforms.set(name, platform);
  };
  const setPlatform = (name) => {
    if (capPlatforms.platforms.has(name)) {
      capPlatforms.currentPlatform = capPlatforms.platforms.get(name);
    }
  };
  capPlatforms.addPlatform = addPlatform;
  capPlatforms.setPlatform = setPlatform;
  return capPlatforms;
};
const initPlatforms = (win2) => win2.CapacitorPlatforms = createCapacitorPlatforms(win2);
const CapacitorPlatforms = /* @__PURE__ */ initPlatforms(typeof globalThis !== "undefined" ? globalThis : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : typeof global !== "undefined" ? global : {});
CapacitorPlatforms.addPlatform;
CapacitorPlatforms.setPlatform;
var ExceptionCode;
(function(ExceptionCode2) {
  ExceptionCode2["Unimplemented"] = "UNIMPLEMENTED";
  ExceptionCode2["Unavailable"] = "UNAVAILABLE";
})(ExceptionCode || (ExceptionCode = {}));
class CapacitorException extends Error {
  constructor(message, code, data) {
    super(message);
    this.message = message;
    this.code = code;
    this.data = data;
  }
}
const getPlatformId = (win2) => {
  var _a, _b;
  if (win2 === null || win2 === void 0 ? void 0 : win2.androidBridge) {
    return "android";
  } else if ((_b = (_a = win2 === null || win2 === void 0 ? void 0 : win2.webkit) === null || _a === void 0 ? void 0 : _a.messageHandlers) === null || _b === void 0 ? void 0 : _b.bridge) {
    return "ios";
  } else {
    return "web";
  }
};
const createCapacitor = (win2) => {
  var _a, _b, _c, _d, _e;
  const capCustomPlatform = win2.CapacitorCustomPlatform || null;
  const cap = win2.Capacitor || {};
  const Plugins = cap.Plugins = cap.Plugins || {};
  const capPlatforms = win2.CapacitorPlatforms;
  const defaultGetPlatform = () => {
    return capCustomPlatform !== null ? capCustomPlatform.name : getPlatformId(win2);
  };
  const getPlatform = ((_a = capPlatforms === null || capPlatforms === void 0 ? void 0 : capPlatforms.currentPlatform) === null || _a === void 0 ? void 0 : _a.getPlatform) || defaultGetPlatform;
  const defaultIsNativePlatform = () => getPlatform() !== "web";
  const isNativePlatform = ((_b = capPlatforms === null || capPlatforms === void 0 ? void 0 : capPlatforms.currentPlatform) === null || _b === void 0 ? void 0 : _b.isNativePlatform) || defaultIsNativePlatform;
  const defaultIsPluginAvailable = (pluginName) => {
    const plugin = registeredPlugins.get(pluginName);
    if (plugin === null || plugin === void 0 ? void 0 : plugin.platforms.has(getPlatform())) {
      return true;
    }
    if (getPluginHeader(pluginName)) {
      return true;
    }
    return false;
  };
  const isPluginAvailable = ((_c = capPlatforms === null || capPlatforms === void 0 ? void 0 : capPlatforms.currentPlatform) === null || _c === void 0 ? void 0 : _c.isPluginAvailable) || defaultIsPluginAvailable;
  const defaultGetPluginHeader = (pluginName) => {
    var _a2;
    return (_a2 = cap.PluginHeaders) === null || _a2 === void 0 ? void 0 : _a2.find((h2) => h2.name === pluginName);
  };
  const getPluginHeader = ((_d = capPlatforms === null || capPlatforms === void 0 ? void 0 : capPlatforms.currentPlatform) === null || _d === void 0 ? void 0 : _d.getPluginHeader) || defaultGetPluginHeader;
  const handleError = (err) => win2.console.error(err);
  const pluginMethodNoop = (_target, prop, pluginName) => {
    return Promise.reject(`${pluginName} does not have an implementation of "${prop}".`);
  };
  const registeredPlugins = /* @__PURE__ */ new Map();
  const defaultRegisterPlugin = (pluginName, jsImplementations = {}) => {
    const registeredPlugin = registeredPlugins.get(pluginName);
    if (registeredPlugin) {
      console.warn(`Capacitor plugin "${pluginName}" already registered. Cannot register plugins twice.`);
      return registeredPlugin.proxy;
    }
    const platform = getPlatform();
    const pluginHeader = getPluginHeader(pluginName);
    let jsImplementation;
    const loadPluginImplementation = async () => {
      if (!jsImplementation && platform in jsImplementations) {
        jsImplementation = typeof jsImplementations[platform] === "function" ? jsImplementation = await jsImplementations[platform]() : jsImplementation = jsImplementations[platform];
      } else if (capCustomPlatform !== null && !jsImplementation && "web" in jsImplementations) {
        jsImplementation = typeof jsImplementations["web"] === "function" ? jsImplementation = await jsImplementations["web"]() : jsImplementation = jsImplementations["web"];
      }
      return jsImplementation;
    };
    const createPluginMethod = (impl, prop) => {
      var _a2, _b2;
      if (pluginHeader) {
        const methodHeader = pluginHeader === null || pluginHeader === void 0 ? void 0 : pluginHeader.methods.find((m) => prop === m.name);
        if (methodHeader) {
          if (methodHeader.rtype === "promise") {
            return (options) => cap.nativePromise(pluginName, prop.toString(), options);
          } else {
            return (options, callback) => cap.nativeCallback(pluginName, prop.toString(), options, callback);
          }
        } else if (impl) {
          return (_a2 = impl[prop]) === null || _a2 === void 0 ? void 0 : _a2.bind(impl);
        }
      } else if (impl) {
        return (_b2 = impl[prop]) === null || _b2 === void 0 ? void 0 : _b2.bind(impl);
      } else {
        throw new CapacitorException(`"${pluginName}" plugin is not implemented on ${platform}`, ExceptionCode.Unimplemented);
      }
    };
    const createPluginMethodWrapper = (prop) => {
      let remove;
      const wrapper = (...args) => {
        const p = loadPluginImplementation().then((impl) => {
          const fn = createPluginMethod(impl, prop);
          if (fn) {
            const p2 = fn(...args);
            remove = p2 === null || p2 === void 0 ? void 0 : p2.remove;
            return p2;
          } else {
            throw new CapacitorException(`"${pluginName}.${prop}()" is not implemented on ${platform}`, ExceptionCode.Unimplemented);
          }
        });
        if (prop === "addListener") {
          p.remove = async () => remove();
        }
        return p;
      };
      wrapper.toString = () => `${prop.toString()}() { [capacitor code] }`;
      Object.defineProperty(wrapper, "name", {
        value: prop,
        writable: false,
        configurable: false
      });
      return wrapper;
    };
    const addListener = createPluginMethodWrapper("addListener");
    const removeListener = createPluginMethodWrapper("removeListener");
    const addListenerNative = (eventName, callback) => {
      const call = addListener({ eventName }, callback);
      const remove = async () => {
        const callbackId = await call;
        removeListener({
          eventName,
          callbackId
        }, callback);
      };
      const p = new Promise((resolve) => call.then(() => resolve({ remove })));
      p.remove = async () => {
        console.warn(`Using addListener() without 'await' is deprecated.`);
        await remove();
      };
      return p;
    };
    const proxy = new Proxy({}, {
      get(_, prop) {
        switch (prop) {
          case "$$typeof":
            return void 0;
          case "toJSON":
            return () => ({});
          case "addListener":
            return pluginHeader ? addListenerNative : addListener;
          case "removeListener":
            return removeListener;
          default:
            return createPluginMethodWrapper(prop);
        }
      }
    });
    Plugins[pluginName] = proxy;
    registeredPlugins.set(pluginName, {
      name: pluginName,
      proxy,
      platforms: /* @__PURE__ */ new Set([
        ...Object.keys(jsImplementations),
        ...pluginHeader ? [platform] : []
      ])
    });
    return proxy;
  };
  const registerPlugin2 = ((_e = capPlatforms === null || capPlatforms === void 0 ? void 0 : capPlatforms.currentPlatform) === null || _e === void 0 ? void 0 : _e.registerPlugin) || defaultRegisterPlugin;
  if (!cap.convertFileSrc) {
    cap.convertFileSrc = (filePath) => filePath;
  }
  cap.getPlatform = getPlatform;
  cap.handleError = handleError;
  cap.isNativePlatform = isNativePlatform;
  cap.isPluginAvailable = isPluginAvailable;
  cap.pluginMethodNoop = pluginMethodNoop;
  cap.registerPlugin = registerPlugin2;
  cap.Exception = CapacitorException;
  cap.DEBUG = !!cap.DEBUG;
  cap.isLoggingEnabled = !!cap.isLoggingEnabled;
  cap.platform = cap.getPlatform();
  cap.isNative = cap.isNativePlatform();
  return cap;
};
const initCapacitorGlobal = (win2) => win2.Capacitor = createCapacitor(win2);
const Capacitor = /* @__PURE__ */ initCapacitorGlobal(typeof globalThis !== "undefined" ? globalThis : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : typeof global !== "undefined" ? global : {});
const registerPlugin = Capacitor.registerPlugin;
Capacitor.Plugins;
class WebPlugin {
  constructor(config) {
    this.listeners = {};
    this.retainedEventArguments = {};
    this.windowListeners = {};
    if (config) {
      console.warn(`Capacitor WebPlugin "${config.name}" config object was deprecated in v3 and will be removed in v4.`);
      this.config = config;
    }
  }
  addListener(eventName, listenerFunc) {
    let firstListener = false;
    const listeners = this.listeners[eventName];
    if (!listeners) {
      this.listeners[eventName] = [];
      firstListener = true;
    }
    this.listeners[eventName].push(listenerFunc);
    const windowListener = this.windowListeners[eventName];
    if (windowListener && !windowListener.registered) {
      this.addWindowListener(windowListener);
    }
    if (firstListener) {
      this.sendRetainedArgumentsForEvent(eventName);
    }
    const remove = async () => this.removeListener(eventName, listenerFunc);
    const p = Promise.resolve({ remove });
    return p;
  }
  async removeAllListeners() {
    this.listeners = {};
    for (const listener in this.windowListeners) {
      this.removeWindowListener(this.windowListeners[listener]);
    }
    this.windowListeners = {};
  }
  notifyListeners(eventName, data, retainUntilConsumed) {
    const listeners = this.listeners[eventName];
    if (!listeners) {
      if (retainUntilConsumed) {
        let args = this.retainedEventArguments[eventName];
        if (!args) {
          args = [];
        }
        args.push(data);
        this.retainedEventArguments[eventName] = args;
      }
      return;
    }
    listeners.forEach((listener) => listener(data));
  }
  hasListeners(eventName) {
    return !!this.listeners[eventName].length;
  }
  registerWindowListener(windowEventName, pluginEventName) {
    this.windowListeners[pluginEventName] = {
      registered: false,
      windowEventName,
      pluginEventName,
      handler: (event) => {
        this.notifyListeners(pluginEventName, event);
      }
    };
  }
  unimplemented(msg = "not implemented") {
    return new Capacitor.Exception(msg, ExceptionCode.Unimplemented);
  }
  unavailable(msg = "not available") {
    return new Capacitor.Exception(msg, ExceptionCode.Unavailable);
  }
  async removeListener(eventName, listenerFunc) {
    const listeners = this.listeners[eventName];
    if (!listeners) {
      return;
    }
    const index = listeners.indexOf(listenerFunc);
    this.listeners[eventName].splice(index, 1);
    if (!this.listeners[eventName].length) {
      this.removeWindowListener(this.windowListeners[eventName]);
    }
  }
  addWindowListener(handle) {
    window.addEventListener(handle.windowEventName, handle.handler);
    handle.registered = true;
  }
  removeWindowListener(handle) {
    if (!handle) {
      return;
    }
    window.removeEventListener(handle.windowEventName, handle.handler);
    handle.registered = false;
  }
  sendRetainedArgumentsForEvent(eventName) {
    const args = this.retainedEventArguments[eventName];
    if (!args) {
      return;
    }
    delete this.retainedEventArguments[eventName];
    args.forEach((arg) => {
      this.notifyListeners(eventName, arg);
    });
  }
}
const encode = (str) => encodeURIComponent(str).replace(/%(2[346B]|5E|60|7C)/g, decodeURIComponent).replace(/[()]/g, escape);
const decode = (str) => str.replace(/(%[\dA-F]{2})+/gi, decodeURIComponent);
class CapacitorCookiesPluginWeb extends WebPlugin {
  async getCookies() {
    const cookies = document.cookie;
    const cookieMap = {};
    cookies.split(";").forEach((cookie) => {
      if (cookie.length <= 0)
        return;
      let [key, value] = cookie.replace(/=/, "CAP_COOKIE").split("CAP_COOKIE");
      key = decode(key).trim();
      value = decode(value).trim();
      cookieMap[key] = value;
    });
    return cookieMap;
  }
  async setCookie(options) {
    try {
      const encodedKey = encode(options.key);
      const encodedValue = encode(options.value);
      const expires = `; expires=${(options.expires || "").replace("expires=", "")}`;
      const path = (options.path || "/").replace("path=", "");
      const domain = options.url != null && options.url.length > 0 ? `domain=${options.url}` : "";
      document.cookie = `${encodedKey}=${encodedValue || ""}${expires}; path=${path}; ${domain};`;
    } catch (error) {
      return Promise.reject(error);
    }
  }
  async deleteCookie(options) {
    try {
      document.cookie = `${options.key}=; Max-Age=0`;
    } catch (error) {
      return Promise.reject(error);
    }
  }
  async clearCookies() {
    try {
      const cookies = document.cookie.split(";") || [];
      for (const cookie of cookies) {
        document.cookie = cookie.replace(/^ +/, "").replace(/=.*/, `=;expires=${(/* @__PURE__ */ new Date()).toUTCString()};path=/`);
      }
    } catch (error) {
      return Promise.reject(error);
    }
  }
  async clearAllCookies() {
    try {
      await this.clearCookies();
    } catch (error) {
      return Promise.reject(error);
    }
  }
}
registerPlugin("CapacitorCookies", {
  web: () => new CapacitorCookiesPluginWeb()
});
const readBlobAsBase64 = async (blob) => new Promise((resolve, reject) => {
  const reader = new FileReader();
  reader.onload = () => {
    const base64String = reader.result;
    resolve(base64String.indexOf(",") >= 0 ? base64String.split(",")[1] : base64String);
  };
  reader.onerror = (error) => reject(error);
  reader.readAsDataURL(blob);
});
const normalizeHttpHeaders = (headers = {}) => {
  const originalKeys = Object.keys(headers);
  const loweredKeys = Object.keys(headers).map((k) => k.toLocaleLowerCase());
  const normalized = loweredKeys.reduce((acc, key, index) => {
    acc[key] = headers[originalKeys[index]];
    return acc;
  }, {});
  return normalized;
};
const buildUrlParams = (params, shouldEncode = true) => {
  if (!params)
    return null;
  const output = Object.entries(params).reduce((accumulator, entry) => {
    const [key, value] = entry;
    let encodedValue;
    let item;
    if (Array.isArray(value)) {
      item = "";
      value.forEach((str) => {
        encodedValue = shouldEncode ? encodeURIComponent(str) : str;
        item += `${key}=${encodedValue}&`;
      });
      item.slice(0, -1);
    } else {
      encodedValue = shouldEncode ? encodeURIComponent(value) : value;
      item = `${key}=${encodedValue}`;
    }
    return `${accumulator}&${item}`;
  }, "");
  return output.substr(1);
};
const buildRequestInit = (options, extra = {}) => {
  const output = Object.assign({ method: options.method || "GET", headers: options.headers }, extra);
  const headers = normalizeHttpHeaders(options.headers);
  const type = headers["content-type"] || "";
  if (typeof options.data === "string") {
    output.body = options.data;
  } else if (type.includes("application/x-www-form-urlencoded")) {
    const params = new URLSearchParams();
    for (const [key, value] of Object.entries(options.data || {})) {
      params.set(key, value);
    }
    output.body = params.toString();
  } else if (type.includes("multipart/form-data") || options.data instanceof FormData) {
    const form = new FormData();
    if (options.data instanceof FormData) {
      options.data.forEach((value, key) => {
        form.append(key, value);
      });
    } else {
      for (const key of Object.keys(options.data)) {
        form.append(key, options.data[key]);
      }
    }
    output.body = form;
    const headers2 = new Headers(output.headers);
    headers2.delete("content-type");
    output.headers = headers2;
  } else if (type.includes("application/json") || typeof options.data === "object") {
    output.body = JSON.stringify(options.data);
  }
  return output;
};
class CapacitorHttpPluginWeb extends WebPlugin {
  /**
   * Perform an Http request given a set of options
   * @param options Options to build the HTTP request
   */
  async request(options) {
    const requestInit = buildRequestInit(options, options.webFetchExtra);
    const urlParams = buildUrlParams(options.params, options.shouldEncodeUrlParams);
    const url = urlParams ? `${options.url}?${urlParams}` : options.url;
    const response = await fetch(url, requestInit);
    const contentType = response.headers.get("content-type") || "";
    let { responseType = "text" } = response.ok ? options : {};
    if (contentType.includes("application/json")) {
      responseType = "json";
    }
    let data;
    let blob;
    switch (responseType) {
      case "arraybuffer":
      case "blob":
        blob = await response.blob();
        data = await readBlobAsBase64(blob);
        break;
      case "json":
        data = await response.json();
        break;
      case "document":
      case "text":
      default:
        data = await response.text();
    }
    const headers = {};
    response.headers.forEach((value, key) => {
      headers[key] = value;
    });
    return {
      data,
      headers,
      status: response.status,
      url: response.url
    };
  }
  /**
   * Perform an Http GET request given a set of options
   * @param options Options to build the HTTP request
   */
  async get(options) {
    return this.request(Object.assign(Object.assign({}, options), { method: "GET" }));
  }
  /**
   * Perform an Http POST request given a set of options
   * @param options Options to build the HTTP request
   */
  async post(options) {
    return this.request(Object.assign(Object.assign({}, options), { method: "POST" }));
  }
  /**
   * Perform an Http PUT request given a set of options
   * @param options Options to build the HTTP request
   */
  async put(options) {
    return this.request(Object.assign(Object.assign({}, options), { method: "PUT" }));
  }
  /**
   * Perform an Http PATCH request given a set of options
   * @param options Options to build the HTTP request
   */
  async patch(options) {
    return this.request(Object.assign(Object.assign({}, options), { method: "PATCH" }));
  }
  /**
   * Perform an Http DELETE request given a set of options
   * @param options Options to build the HTTP request
   */
  async delete(options) {
    return this.request(Object.assign(Object.assign({}, options), { method: "DELETE" }));
  }
}
registerPlugin("CapacitorHttp", {
  web: () => new CapacitorHttpPluginWeb()
});
const WebviewPrint = registerPlugin("WebviewPrint", {
  web: () => __vitePreload(() => import("./web.js"), true ? [] : void 0).then((m) => new m.WebviewPrintWeb())
});
const Preferences = registerPlugin("Preferences", {
  web: () => __vitePreload(() => import("./web2.js"), true ? [] : void 0).then((m) => new m.PreferencesWeb())
});
const Browser = registerPlugin("Browser", {
  web: () => __vitePreload(() => import("./web3.js"), true ? [] : void 0).then((m) => new m.BrowserWeb())
});
const Share = registerPlugin("Share", {
  web: () => __vitePreload(() => import("./web4.js"), true ? [] : void 0).then((m) => new m.ShareWeb())
});
const Toast = registerPlugin("Toast", {
  web: () => __vitePreload(() => import("./web5.js"), true ? [] : void 0).then((m) => new m.ToastWeb())
});
async function printCurrentPage(name) {
  return await WebviewPrint.print({ name });
}
async function selectStorage(key) {
  const { value } = await Preferences.get({ key });
  return value;
}
async function insertStorage(key, value) {
  return await Preferences.set({ key, value });
}
async function openBrowserPage(url) {
  try {
    return await Browser.open({ url, windowName: "_blank" });
  } catch (e) {
    return window.open(url, "_blank").focus();
  }
}
async function shareText(text) {
  const { value } = await Share.canShare();
  if (value) {
    return await Share.share({ text });
  } else {
    return await navigator.clipboard.writeText(text);
  }
}
async function popupText(text) {
  return await Toast.show({ text });
}
defineCustomElements();
globalThis.printCurrentPage = printCurrentPage;
globalThis.selectStorage = selectStorage;
globalThis.insertStorage = insertStorage;
globalThis.openBrowserPage = openBrowserPage;
globalThis.shareText = shareText;
globalThis.popupText = popupText;
export {
  Host as H,
  WebPlugin as W,
  createEvent as c,
  forceUpdate as f,
  getElement as g,
  h,
  registerInstance as r
};
