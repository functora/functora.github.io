import { h, r as registerInstance, c as createEvent } from "./main.js";
var __awaiter = function(e, t, n, r) {
  function i(e2) {
    return e2 instanceof n ? e2 : new n(function(t2) {
      t2(e2);
    });
  }
  return new (n || (n = Promise))(function(n2, o) {
    function a(e2) {
      try {
        c(r.next(e2));
      } catch (e3) {
        o(e3);
      }
    }
    function s(e2) {
      try {
        c(r["throw"](e2));
      } catch (e3) {
        o(e3);
      }
    }
    function c(e2) {
      e2.done ? n2(e2.value) : i(e2.value).then(a, s);
    }
    c((r = r.apply(e, t || [])).next());
  });
};
var __generator = function(e, t) {
  var n = { label: 0, sent: function() {
    if (o[0] & 1) throw o[1];
    return o[1];
  }, trys: [], ops: [] }, r, i, o, a;
  return a = { next: s(0), throw: s(1), return: s(2) }, typeof Symbol === "function" && (a[Symbol.iterator] = function() {
    return this;
  }), a;
  function s(e2) {
    return function(t2) {
      return c([e2, t2]);
    };
  }
  function c(s2) {
    if (r) throw new TypeError("Generator is already executing.");
    while (a && (a = 0, s2[0] && (n = 0)), n) try {
      if (r = 1, i && (o = s2[0] & 2 ? i["return"] : s2[0] ? i["throw"] || ((o = i["return"]) && o.call(i), 0) : i.next) && !(o = o.call(i, s2[1])).done) return o;
      if (i = 0, o) s2 = [s2[0] & 2, o.value];
      switch (s2[0]) {
        case 0:
        case 1:
          o = s2;
          break;
        case 4:
          n.label++;
          return { value: s2[1], done: false };
        case 5:
          n.label++;
          i = s2[1];
          s2 = [0];
          continue;
        case 7:
          s2 = n.ops.pop();
          n.trys.pop();
          continue;
        default:
          if (!(o = n.trys, o = o.length > 0 && o[o.length - 1]) && (s2[0] === 6 || s2[0] === 2)) {
            n = 0;
            continue;
          }
          if (s2[0] === 3 && (!o || s2[1] > o[0] && s2[1] < o[3])) {
            n.label = s2[1];
            break;
          }
          if (s2[0] === 6 && n.label < o[1]) {
            n.label = o[1];
            o = s2;
            break;
          }
          if (o && n.label < o[2]) {
            n.label = o[2];
            n.ops.push(s2);
            break;
          }
          if (o[2]) n.ops.pop();
          n.trys.pop();
          continue;
      }
      s2 = t.call(e, n);
    } catch (e2) {
      s2 = [6, e2];
      i = 0;
    } finally {
      r = o = 0;
    }
    if (s2[0] & 5) throw s2[1];
    return { value: s2[0] ? s2[1] : void 0, done: true };
  }
};
var cameraModalCss = ":host{z-index:1000;position:fixed;top:0;left:0;width:100%;height:100%;display:-ms-flexbox;display:flex;contain:strict}.wrapper{-ms-flex:1;flex:1;display:-ms-flexbox;display:flex;-ms-flex-align:center;align-items:center;-ms-flex-pack:center;justify-content:center;background-color:rgba(0, 0, 0, 0.15)}.content{-webkit-box-shadow:0px 0px 5px rgba(0, 0, 0, 0.2);box-shadow:0px 0px 5px rgba(0, 0, 0, 0.2);width:600px;height:600px}";
var PWACameraModal = function() {
  function e(e2) {
    registerInstance(this, e2);
    this.onPhoto = createEvent(this, "onPhoto", 7);
    this.noDeviceError = createEvent(this, "noDeviceError", 7);
    this.facingMode = "user";
    this.hidePicker = false;
  }
  e.prototype.present = function() {
    return __awaiter(this, void 0, void 0, function() {
      var e2;
      var t = this;
      return __generator(this, function(n) {
        e2 = document.createElement("pwa-camera-modal-instance");
        e2.facingMode = this.facingMode;
        e2.hidePicker = this.hidePicker;
        e2.addEventListener("onPhoto", function(e3) {
          return __awaiter(t, void 0, void 0, function() {
            var t2;
            return __generator(this, function(n2) {
              if (!this._modal) {
                return [2];
              }
              t2 = e3.detail;
              this.onPhoto.emit(t2);
              return [2];
            });
          });
        });
        e2.addEventListener("noDeviceError", function(e3) {
          return __awaiter(t, void 0, void 0, function() {
            return __generator(this, function(t2) {
              this.noDeviceError.emit(e3);
              return [2];
            });
          });
        });
        document.body.append(e2);
        this._modal = e2;
        return [2];
      });
    });
  };
  e.prototype.dismiss = function() {
    return __awaiter(this, void 0, void 0, function() {
      return __generator(this, function(e2) {
        if (!this._modal) {
          return [2];
        }
        this._modal && this._modal.parentNode.removeChild(this._modal);
        this._modal = null;
        return [2];
      });
    });
  };
  e.prototype.render = function() {
    return h("div", null);
  };
  return e;
}();
PWACameraModal.style = cameraModalCss;
export {
  PWACameraModal as pwa_camera_modal
};
