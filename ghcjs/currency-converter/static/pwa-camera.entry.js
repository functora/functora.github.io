import { f as forceUpdate, h, g as getElement, r as registerInstance } from "./main.js";
var __awaiter = function(e, t, i, n) {
  function r(e2) {
    return e2 instanceof i ? e2 : new i(function(t2) {
      t2(e2);
    });
  }
  return new (i || (i = Promise))(function(i2, a) {
    function o(e2) {
      try {
        c(n.next(e2));
      } catch (e3) {
        a(e3);
      }
    }
    function s(e2) {
      try {
        c(n["throw"](e2));
      } catch (e3) {
        a(e3);
      }
    }
    function c(e2) {
      e2.done ? i2(e2.value) : r(e2.value).then(o, s);
    }
    c((n = n.apply(e, t || [])).next());
  });
};
var __generator = function(e, t) {
  var i = { label: 0, sent: function() {
    if (a[0] & 1) throw a[1];
    return a[1];
  }, trys: [], ops: [] }, n, r, a, o;
  return o = { next: s(0), throw: s(1), return: s(2) }, typeof Symbol === "function" && (o[Symbol.iterator] = function() {
    return this;
  }), o;
  function s(e2) {
    return function(t2) {
      return c([e2, t2]);
    };
  }
  function c(s2) {
    if (n) throw new TypeError("Generator is already executing.");
    while (o && (o = 0, s2[0] && (i = 0)), i) try {
      if (n = 1, r && (a = s2[0] & 2 ? r["return"] : s2[0] ? r["throw"] || ((a = r["return"]) && a.call(r), 0) : r.next) && !(a = a.call(r, s2[1])).done) return a;
      if (r = 0, a) s2 = [s2[0] & 2, a.value];
      switch (s2[0]) {
        case 0:
        case 1:
          a = s2;
          break;
        case 4:
          i.label++;
          return { value: s2[1], done: false };
        case 5:
          i.label++;
          r = s2[1];
          s2 = [0];
          continue;
        case 7:
          s2 = i.ops.pop();
          i.trys.pop();
          continue;
        default:
          if (!(a = i.trys, a = a.length > 0 && a[a.length - 1]) && (s2[0] === 6 || s2[0] === 2)) {
            i = 0;
            continue;
          }
          if (s2[0] === 3 && (!a || s2[1] > a[0] && s2[1] < a[3])) {
            i.label = s2[1];
            break;
          }
          if (s2[0] === 6 && i.label < a[1]) {
            i.label = a[1];
            a = s2;
            break;
          }
          if (a && i.label < a[2]) {
            i.label = a[2];
            i.ops.push(s2);
            break;
          }
          if (a[2]) i.ops.pop();
          i.trys.pop();
          continue;
      }
      s2 = t.call(e, i);
    } catch (e2) {
      s2 = [6, e2];
      r = 0;
    } finally {
      n = a = 0;
    }
    if (s2[0] & 5) throw s2[1];
    return { value: s2[0] ? s2[1] : void 0, done: true };
  }
};
/**
* MediaStream ImageCapture polyfill
*
* @license
* Copyright 2018 Google Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*      http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
var ImageCapture = window.ImageCapture;
if (typeof ImageCapture === "undefined") {
  ImageCapture = function() {
    function e(e2) {
      var t = this;
      if (e2.kind !== "video") throw new DOMException("NotSupportedError");
      this._videoStreamTrack = e2;
      if (!("readyState" in this._videoStreamTrack)) {
        this._videoStreamTrack.readyState = "live";
      }
      this._previewStream = new MediaStream([e2]);
      this.videoElement = document.createElement("video");
      this.videoElementPlaying = new Promise(function(e3) {
        t.videoElement.addEventListener("playing", e3);
      });
      if (HTMLMediaElement) {
        this.videoElement.srcObject = this._previewStream;
      } else {
        this.videoElement.src = URL.createObjectURL(this._previewStream);
      }
      this.videoElement.muted = true;
      this.videoElement.setAttribute("playsinline", "");
      this.videoElement.play();
      this.canvasElement = document.createElement("canvas");
      this.canvas2dContext = this.canvasElement.getContext("2d");
    }
    Object.defineProperty(e.prototype, "videoStreamTrack", { get: function() {
      return this._videoStreamTrack;
    }, enumerable: false, configurable: true });
    e.prototype.getPhotoCapabilities = function() {
      return new Promise(function e2(t, i) {
        var n = { current: 0, min: 0, max: 0 };
        t({ exposureCompensation: n, exposureMode: "none", fillLightMode: ["none"], focusMode: "none", imageHeight: n, imageWidth: n, iso: n, redEyeReduction: false, whiteBalanceMode: "none", zoom: n });
        i(new DOMException("OperationError"));
      });
    };
    e.prototype.setOptions = function(e2) {
      return new Promise(function e3(t, i) {
      });
    };
    e.prototype.takePhoto = function() {
      var e2 = this;
      return new Promise(function t(i, n) {
        if (e2._videoStreamTrack.readyState !== "live") {
          return n(new DOMException("InvalidStateError"));
        }
        e2.videoElementPlaying.then(function() {
          try {
            e2.canvasElement.width = e2.videoElement.videoWidth;
            e2.canvasElement.height = e2.videoElement.videoHeight;
            e2.canvas2dContext.drawImage(e2.videoElement, 0, 0);
            e2.canvasElement.toBlob(i);
          } catch (e3) {
            n(new DOMException("UnknownError"));
          }
        });
      });
    };
    e.prototype.grabFrame = function() {
      var e2 = this;
      return new Promise(function t(i, n) {
        if (e2._videoStreamTrack.readyState !== "live") {
          return n(new DOMException("InvalidStateError"));
        }
        e2.videoElementPlaying.then(function() {
          try {
            e2.canvasElement.width = e2.videoElement.videoWidth;
            e2.canvasElement.height = e2.videoElement.videoHeight;
            e2.canvas2dContext.drawImage(e2.videoElement, 0, 0);
            i(window.createImageBitmap(e2.canvasElement));
          } catch (e3) {
            n(new DOMException("UnknownError"));
          }
        });
      });
    };
    return e;
  }();
}
window.ImageCapture = ImageCapture;
var cameraCss = ":host{--header-height:4em;--footer-height:9em;--header-height-landscape:3em;--footer-height-landscape:6em;--shutter-size:6em;--icon-size-header:1.5em;--icon-size-footer:2.5em;--margin-size-header:1.5em;--margin-size-footer:2.0em;font-family:-apple-system, BlinkMacSystemFont,\n    “Segoe UI”, “Roboto”, “Droid Sans”, “Helvetica Neue”, sans-serif;display:block;width:100%;height:100%}.items{-webkit-box-sizing:border-box;box-sizing:border-box;display:-ms-flexbox;display:flex;width:100%;height:100%;-ms-flex-align:center;align-items:center;-ms-flex-pack:center;justify-content:center}.items .item{-ms-flex:1;flex:1;text-align:center}.items .item:first-child{text-align:left}.items .item:last-child{text-align:right}.camera-wrapper{position:relative;display:-ms-flexbox;display:flex;-ms-flex-direction:column;flex-direction:column;width:100%;height:100%}.camera-header{color:white;background-color:black;height:var(--header-height)}.camera-header .items{padding:var(--margin-size-header)}.camera-footer{position:relative;color:white;background-color:black;height:var(--footer-height)}.camera-footer .items{padding:var(--margin-size-footer)}@media (max-height: 375px){.camera-header{--header-height:var(--header-height-landscape)}.camera-footer{--footer-height:var(--footer-height-landscape)}.camera-footer .shutter{--shutter-size:4em}}.camera-video{position:relative;-ms-flex:1;flex:1;overflow:hidden;background-color:black}video{width:100%;height:100%;max-height:100%;min-height:100%;-o-object-fit:cover;object-fit:cover;background-color:black}.pick-image{display:-ms-flexbox;display:flex;-ms-flex-align:center;align-items:center;position:absolute;left:var(--margin-size-footer);top:0;height:100%;width:var(--icon-size-footer);color:white}.pick-image input{visibility:hidden}.pick-image svg{cursor:pointer;fill:white;width:var(--icon-size-footer);height:var(--icon-size-footer)}.shutter{position:absolute;left:50%;top:50%;width:var(--shutter-size);height:var(--shutter-size);margin-top:calc(var(--shutter-size) / -2);margin-left:calc(var(--shutter-size) / -2);border-radius:100%;background-color:#c6cdd8;padding:12px;-webkit-box-sizing:border-box;box-sizing:border-box}.shutter:active .shutter-button{background-color:#9da9bb}.shutter-button{background-color:white;border-radius:100%;width:100%;height:100%}.rotate{display:-ms-flexbox;display:flex;-ms-flex-align:center;align-items:center;position:absolute;right:var(--margin-size-footer);top:0;height:100%;width:var(--icon-size-footer);color:white}.rotate img{width:var(--icon-size-footer);height:var(--icon-size-footer)}.shutter-overlay{z-index:5;position:absolute;width:100%;height:100%;background-color:black}.error{width:100%;height:100%;color:white;display:-ms-flexbox;display:flex;-ms-flex-pack:center;justify-content:center;-ms-flex-align:center;align-items:center}.no-device{background-color:black;-ms-flex:1;flex:1;display:-ms-flexbox;display:flex;-ms-flex-direction:column;flex-direction:column;-ms-flex-align:center;align-items:center;-ms-flex-pack:center;justify-content:center;color:white}.no-device label{cursor:pointer;background:#fff;border-radius:6px;padding:6px 8px;color:black}.no-device input{visibility:hidden;height:0;margin-top:16px}.accept{background-color:black;-ms-flex:1;flex:1;overflow:hidden}.accept .accept-image{width:100%;height:100%;max-height:100%;background-position:center center;background-size:cover;background-repeat:no-repeat}.close img{cursor:pointer;width:var(--icon-size-header);height:var(--icon-size-header)}.flash img{width:var(--icon-size-header);height:var(--icon-size-header)}.accept-use img{width:var(--icon-size-footer);height:var(--icon-size-footer)}.accept-cancel img{width:var(--icon-size-footer);height:var(--icon-size-footer)}.offscreen-image-render{top:0;left:0;visibility:hidden;pointer-events:none;width:100%;height:100%}";
var CameraPWA = function() {
  function e(e2) {
    var t = this;
    registerInstance(this, e2);
    this.hasMultipleCameras = false;
    this.hasFlash = false;
    this.flashModes = [];
    this.flashMode = "off";
    this.handlePickFile = function(e3) {
    };
    this.handleShutterClick = function(e3) {
      console.debug("shutter click");
      t.capture();
    };
    this.handleRotateClick = function(e3) {
      t.rotate();
    };
    this.handleClose = function(e3) {
      t.handlePhoto && t.handlePhoto(null);
    };
    this.handleFlashClick = function(e3) {
      t.cycleFlash();
    };
    this.handleCancelPhoto = function(e3) {
      var i = t.stream && t.stream.getTracks()[0];
      var n = i && i.getConstraints();
      t.photo = null;
      t.photoSrc = null;
      if (n) {
        t.initCamera({ video: { facingMode: n.facingMode } });
      } else {
        t.initCamera();
      }
    };
    this.handleAcceptPhoto = function(e3) {
      t.handlePhoto && t.handlePhoto(t.photo);
    };
    this.handleFileInputChange = function(e3) {
      return __awaiter(t, void 0, void 0, function() {
        var t2, i, n;
        return __generator(this, function(a) {
          switch (a.label) {
            case 0:
              t2 = e3.target;
              i = t2.files[0];
              a.label = 1;
            case 1:
              a.trys.push([1, 3, , 4]);
              return [4, this.getOrientation(i)];
            case 2:
              n = a.sent();
              console.debug("Got orientation", n);
              this.photoOrientation = n;
              return [3, 4];
            case 3:
              a.sent();
              return [3, 4];
            case 4:
              this.handlePhoto && this.handlePhoto(i);
              return [2];
          }
        });
      });
    };
    this.handleVideoMetadata = function(e3) {
      console.debug("Video metadata", e3);
    };
    this.facingMode = "user";
    this.handlePhoto = void 0;
    this.hidePicker = false;
    this.handleNoDeviceError = void 0;
    this.noDevicesText = "No camera found";
    this.noDevicesButtonText = "Choose image";
    this.photo = void 0;
    this.photoSrc = void 0;
    this.showShutterOverlay = false;
    this.flashIndex = 0;
    this.hasCamera = null;
    this.rotation = 0;
    this.deviceError = null;
  }
  e.prototype.componentDidLoad = function() {
    return __awaiter(this, void 0, void 0, function() {
      return __generator(this, function(e2) {
        switch (e2.label) {
          case 0:
            this.defaultConstraints = { video: { facingMode: this.facingMode } };
            return [4, this.queryDevices()];
          case 1:
            e2.sent();
            return [4, this.initCamera()];
          case 2:
            e2.sent();
            return [2];
        }
      });
    });
  };
  e.prototype.disconnectedCallback = function() {
    this.stopStream();
    this.photoSrc && URL.revokeObjectURL(this.photoSrc);
  };
  e.prototype.hasImageCapture = function() {
    return "ImageCapture" in window;
  };
  e.prototype.queryDevices = function() {
    return __awaiter(this, void 0, void 0, function() {
      var e2, t, i;
      return __generator(this, function(n) {
        switch (n.label) {
          case 0:
            n.trys.push([0, 2, , 3]);
            return [4, navigator.mediaDevices.enumerateDevices()];
          case 1:
            e2 = n.sent();
            t = e2.filter(function(e3) {
              return e3.kind == "videoinput";
            });
            this.hasCamera = !!t.length;
            this.hasMultipleCameras = t.length > 1;
            return [3, 3];
          case 2:
            i = n.sent();
            this.deviceError = i;
            return [3, 3];
          case 3:
            return [2];
        }
      });
    });
  };
  e.prototype.initCamera = function(e2) {
    return __awaiter(this, void 0, void 0, function() {
      var t, i;
      return __generator(this, function(n) {
        switch (n.label) {
          case 0:
            if (!e2) {
              e2 = this.defaultConstraints;
            }
            n.label = 1;
          case 1:
            n.trys.push([1, 3, , 4]);
            return [4, navigator.mediaDevices.getUserMedia(Object.assign({ video: true, audio: false }, e2))];
          case 2:
            t = n.sent();
            this.initStream(t);
            return [3, 4];
          case 3:
            i = n.sent();
            this.deviceError = i;
            this.handleNoDeviceError && this.handleNoDeviceError(i);
            return [3, 4];
          case 4:
            return [2];
        }
      });
    });
  };
  e.prototype.initStream = function(e2) {
    return __awaiter(this, void 0, void 0, function() {
      return __generator(this, function(t) {
        switch (t.label) {
          case 0:
            this.stream = e2;
            this.videoElement.srcObject = e2;
            if (!this.hasImageCapture()) return [3, 2];
            this.imageCapture = new window.ImageCapture(e2.getVideoTracks()[0]);
            return [4, this.initPhotoCapabilities(this.imageCapture)];
          case 1:
            t.sent();
            return [3, 3];
          case 2:
            this.deviceError = "No image capture";
            this.handleNoDeviceError && this.handleNoDeviceError();
            t.label = 3;
          case 3:
            forceUpdate(this.el);
            return [2];
        }
      });
    });
  };
  e.prototype.initPhotoCapabilities = function(e2) {
    return __awaiter(this, void 0, void 0, function() {
      var t;
      return __generator(this, function(i) {
        switch (i.label) {
          case 0:
            return [4, e2.getPhotoCapabilities()];
          case 1:
            t = i.sent();
            if (t.fillLightMode && t.fillLightMode.length > 1) {
              this.flashModes = t.fillLightMode.map(function(e3) {
                return e3;
              });
              if (this.flashMode) {
                this.flashMode = this.flashModes[this.flashModes.indexOf(this.flashMode)] || "off";
                this.flashIndex = this.flashModes.indexOf(this.flashMode) || 0;
              } else {
                this.flashIndex = 0;
              }
            }
            return [2];
        }
      });
    });
  };
  e.prototype.stopStream = function() {
    if (this.videoElement) {
      this.videoElement.srcObject = null;
    }
    this.stream && this.stream.getTracks().forEach(function(e2) {
      return e2.stop();
    });
  };
  e.prototype.capture = function() {
    return __awaiter(this, void 0, void 0, function() {
      var e2, t;
      return __generator(this, function(i) {
        switch (i.label) {
          case 0:
            if (!this.hasImageCapture()) return [3, 5];
            i.label = 1;
          case 1:
            i.trys.push([1, 4, , 5]);
            return [4, this.imageCapture.takePhoto({ fillLightMode: this.flashModes.length > 1 ? this.flashMode : void 0 })];
          case 2:
            e2 = i.sent();
            return [4, this.flashScreen()];
          case 3:
            i.sent();
            this.promptAccept(e2);
            return [3, 5];
          case 4:
            t = i.sent();
            console.error("Unable to take photo!", t);
            return [3, 5];
          case 5:
            this.stopStream();
            return [2];
        }
      });
    });
  };
  e.prototype.promptAccept = function(e2) {
    return __awaiter(this, void 0, void 0, function() {
      var t;
      return __generator(this, function(i) {
        switch (i.label) {
          case 0:
            this.photo = e2;
            return [4, this.getOrientation(e2)];
          case 1:
            t = i.sent();
            console.debug("Got orientation", t);
            this.photoOrientation = t;
            if (t) {
              switch (t) {
                case 1:
                case 2:
                  this.rotation = 0;
                  break;
                case 3:
                case 4:
                  this.rotation = 180;
                  break;
                case 5:
                case 6:
                  this.rotation = 90;
                  break;
                case 7:
                case 8:
                  this.rotation = 270;
                  break;
              }
            }
            this.photoSrc = URL.createObjectURL(e2);
            return [2];
        }
      });
    });
  };
  e.prototype.getOrientation = function(e2) {
    return new Promise(function(t) {
      var i = new FileReader();
      i.onload = function(e3) {
        var i2 = new DataView(e3.target.result);
        if (i2.getUint16(0, false) !== 65496) {
          return t(-2);
        }
        var n = i2.byteLength;
        var r = 2;
        while (r < n) {
          var a = i2.getUint16(r, false);
          r += 2;
          if (a === 65505) {
            if (i2.getUint32(r += 2, false) !== 1165519206) {
              return t(-1);
            }
            var o = i2.getUint16(r += 6, false) === 18761;
            r += i2.getUint32(r + 4, o);
            var s = i2.getUint16(r, o);
            r += 2;
            for (var c = 0; c < s; c++) {
              if (i2.getUint16(r + c * 12, o) === 274) {
                return t(i2.getUint16(r + c * 12 + 8, o));
              }
            }
          } else if ((a & 65280) !== 65280) {
            break;
          } else {
            r += i2.getUint16(r, false);
          }
        }
        return t(-1);
      };
      i.readAsArrayBuffer(e2.slice(0, 64 * 1024));
    });
  };
  e.prototype.rotate = function() {
    this.stopStream();
    var e2 = this.stream && this.stream.getTracks()[0];
    if (!e2) {
      return;
    }
    var t = e2.getConstraints();
    var i = t.facingMode;
    if (!i) {
      var n = e2.getCapabilities();
      if (n.facingMode) {
        i = n.facingMode[0];
      }
    }
    if (i === "environment") {
      this.initCamera({ video: { facingMode: "user" } });
    } else {
      this.initCamera({ video: { facingMode: "environment" } });
    }
  };
  e.prototype.setFlashMode = function(e2) {
    console.debug("New flash mode: ", e2);
    this.flashMode = e2;
  };
  e.prototype.cycleFlash = function() {
    if (this.flashModes.length > 0) {
      this.flashIndex = (this.flashIndex + 1) % this.flashModes.length;
      this.setFlashMode(this.flashModes[this.flashIndex]);
    }
  };
  e.prototype.flashScreen = function() {
    return __awaiter(this, void 0, void 0, function() {
      var e2 = this;
      return __generator(this, function(t) {
        return [2, new Promise(function(t2, i) {
          e2.showShutterOverlay = true;
          setTimeout(function() {
            e2.showShutterOverlay = false;
            t2();
          }, 100);
        })];
      });
    });
  };
  e.prototype.iconExit = function() {
    return "data:image/svg+xml,%3Csvg version='1.1' id='Layer_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 512 512' enable-background='new 0 0 512 512' xml:space='preserve'%3E%3Cg id='Icon_5_'%3E%3Cg%3E%3Cpath fill='%23FFFFFF' d='M402.2,134L378,109.8c-1.6-1.6-4.1-1.6-5.7,0L258.8,223.4c-1.6,1.6-4.1,1.6-5.7,0L139.6,109.8 c-1.6-1.6-4.1-1.6-5.7,0L109.8,134c-1.6,1.6-1.6,4.1,0,5.7l113.5,113.5c1.6,1.6,1.6,4.1,0,5.7L109.8,372.4c-1.6,1.6-1.6,4.1,0,5.7 l24.1,24.1c1.6,1.6,4.1,1.6,5.7,0l113.5-113.5c1.6-1.6,4.1-1.6,5.7,0l113.5,113.5c1.6,1.6,4.1,1.6,5.7,0l24.1-24.1 c1.6-1.6,1.6-4.1,0-5.7L288.6,258.8c-1.6-1.6-1.6-4.1,0-5.7l113.5-113.5C403.7,138.1,403.7,135.5,402.2,134z'/%3E%3C/g%3E%3C/g%3E%3C/svg%3E";
  };
  e.prototype.iconPhotos = function() {
    return h("svg", { xmlns: "http://www.w3.org/2000/svg", width: "512", height: "512", viewBox: "0 0 512 512" }, h("path", { d: "M450.29,112H142c-34,0-62,27.51-62,61.33V418.67C80,452.49,108,480,142,480H450c34,0,62-26.18,62-60V173.33C512,139.51,484.32,112,450.29,112Zm-77.15,61.34a46,46,0,1,1-46.28,46A46.19,46.19,0,0,1,373.14,173.33Zm-231.55,276c-17,0-29.86-13.75-29.86-30.66V353.85l90.46-80.79a46.54,46.54,0,0,1,63.44,1.83L328.27,337l-113,112.33ZM480,418.67a30.67,30.67,0,0,1-30.71,30.66H259L376.08,333a46.24,46.24,0,0,1,59.44-.16L480,370.59Z" }), h("path", { d: "M384,32H64A64,64,0,0,0,0,96V352a64.11,64.11,0,0,0,48,62V152a72,72,0,0,1,72-72H446A64.11,64.11,0,0,0,384,32Z" }));
  };
  e.prototype.iconConfirm = function() {
    return "data:image/svg+xml,%3Csvg version='1.1' id='Layer_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 512 512' enable-background='new 0 0 512 512' xml:space='preserve'%3E%3Ccircle fill='%232CD865' cx='256' cy='256' r='256'/%3E%3Cg id='Icon_1_'%3E%3Cg%3E%3Cg%3E%3Cpath fill='%23FFFFFF' d='M208,301.4l-55.4-55.5c-1.5-1.5-4-1.6-5.6-0.1l-23.4,22.3c-1.6,1.6-1.7,4.1-0.1,5.7l81.6,81.4 c3.1,3.1,8.2,3.1,11.3,0l171.8-171.7c1.6-1.6,1.6-4.2-0.1-5.7l-23.4-22.3c-1.6-1.5-4.1-1.5-5.6,0.1L213.7,301.4 C212.1,303,209.6,303,208,301.4z'/%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/svg%3E";
  };
  e.prototype.iconReverseCamera = function() {
    return "data:image/svg+xml,%3Csvg version='1.1' id='Layer_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 512 512' enable-background='new 0 0 512 512' xml:space='preserve'%3E%3Cg%3E%3Cpath fill='%23FFFFFF' d='M352,0H160C72,0,0,72,0,160v192c0,88,72,160,160,160h192c88,0,160-72,160-160V160C512,72,440,0,352,0z M356.7,365.8l-3.7,3.3c-27,23.2-61.4,35.9-96.8,35.9c-72.4,0-135.8-54.7-147-125.6c-0.3-1.9-2-3.3-3.9-3.3H64 c-3.3,0-5.2-3.8-3.2-6.4l61.1-81.4c1.6-2.1,4.7-2.1,6.4-0.1l63.3,81.4c2,2.6,0.2,6.5-3.2,6.5h-40.6c-2.5,0-4.5,2.4-3.9,4.8 c11.5,51.5,59.2,90.6,112.4,90.6c26.4,0,51.8-9.7,73.7-27.9l3.1-2.5c1.6-1.3,3.9-1.1,5.3,0.3l18.5,18.6 C358.5,361.6,358.4,364.3,356.7,365.8z M451.4,245.6l-61,83.5c-1.6,2.2-4.8,2.2-6.4,0.1l-63.3-83.3c-2-2.6-0.1-6.4,3.2-6.4h40.8 c2.5,0,4.4-2.3,3.9-4.8c-5.1-24.2-17.8-46.5-36.5-63.7c-21.2-19.4-48.2-30.1-76-30.1c-26.5,0-52.6,9.7-73.7,27.3l-3.1,2.5 c-1.6,1.3-3.9,1.2-5.4-0.3l-18.5-18.5c-1.6-1.6-1.5-4.3,0.2-5.9l3.5-3.1c27-23.2,61.4-35.9,96.8-35.9c38,0,73.9,13.7,101.2,38.7 c23.2,21.1,40.3,55.2,45.7,90.1c0.3,1.9,1.9,3.4,3.9,3.4h41.3C451.4,239.2,453.3,243,451.4,245.6z'/%3E%3C/g%3E%3C/svg%3E";
  };
  e.prototype.iconRetake = function() {
    return "data:image/svg+xml,%3Csvg version='1.1' id='Layer_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 512 512' enable-background='new 0 0 512 512' xml:space='preserve'%3E%3Ccircle fill='%23727A87' cx='256' cy='256' r='256'/%3E%3Cg id='Icon_5_'%3E%3Cg%3E%3Cpath fill='%23FFFFFF' d='M394.2,142L370,117.8c-1.6-1.6-4.1-1.6-5.7,0L258.8,223.4c-1.6,1.6-4.1,1.6-5.7,0L147.6,117.8 c-1.6-1.6-4.1-1.6-5.7,0L117.8,142c-1.6,1.6-1.6,4.1,0,5.7l105.5,105.5c1.6,1.6,1.6,4.1,0,5.7L117.8,364.4c-1.6,1.6-1.6,4.1,0,5.7 l24.1,24.1c1.6,1.6,4.1,1.6,5.7,0l105.5-105.5c1.6-1.6,4.1-1.6,5.7,0l105.5,105.5c1.6,1.6,4.1,1.6,5.7,0l24.1-24.1 c1.6-1.6,1.6-4.1,0-5.7L288.6,258.8c-1.6-1.6-1.6-4.1,0-5.7l105.5-105.5C395.7,146.1,395.7,143.5,394.2,142z'/%3E%3C/g%3E%3C/g%3E%3C/svg%3E";
  };
  e.prototype.iconFlashOff = function() {
    return "data:image/svg+xml,%3Csvg version='1.1' id='Layer_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 512 512' style='enable-background:new 0 0 512 512;' xml:space='preserve'%3E%3Cstyle type='text/css'%3E .st0%7Bfill:%23FFFFFF;%7D%0A%3C/style%3E%3Cg%3E%3Cpath class='st0' d='M498,483.7L42.3,28L14,56.4l149.8,149.8L91,293.8c-2.5,3-0.1,7.2,3.9,7.2h143.9c1.6,0,2.7,1.3,2.4,2.7 L197.6,507c-1,4.4,5.8,6.9,8.9,3.2l118.6-142.8L469.6,512L498,483.7z'/%3E%3Cpath class='st0' d='M449,218.2c2.5-3,0.1-7.2-3.9-7.2H301.2c-1.6,0-2.7-1.3-2.4-2.7L342.4,5c1-4.4-5.8-6.9-8.9-3.2L214.9,144.6 l161.3,161.3L449,218.2z'/%3E%3C/g%3E%3C/svg%3E";
  };
  e.prototype.iconFlashOn = function() {
    return "data:image/svg+xml,%3Csvg version='1.1' id='Layer_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 512 512' style='enable-background:new 0 0 512 512;' xml:space='preserve'%3E%3Cstyle type='text/css'%3E .st0%7Bfill:%23FFFFFF;%7D%0A%3C/style%3E%3Cpath class='st0' d='M287.2,211c-1.6,0-2.7-1.3-2.4-2.7L328.4,5c1-4.4-5.8-6.9-8.9-3.2L77,293.8c-2.5,3-0.1,7.2,3.9,7.2h143.9 c1.6,0,2.7,1.3,2.4,2.7L183.6,507c-1,4.4,5.8,6.9,8.9,3.2l242.5-292c2.5-3,0.1-7.2-3.9-7.2L287.2,211L287.2,211z'/%3E%3C/svg%3E";
  };
  e.prototype.iconFlashAuto = function() {
    return "data:image/svg+xml,%3Csvg version='1.1' id='Layer_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 512 512' style='enable-background:new 0 0 512 512;' xml:space='preserve'%3E%3Cstyle type='text/css'%3E .st0%7Bfill:%23FFFFFF;%7D%0A%3C/style%3E%3Cpath class='st0' d='M287.2,211c-1.6,0-2.7-1.3-2.4-2.7L328.4,5c1-4.4-5.8-6.9-8.9-3.2L77,293.8c-2.5,3-0.1,7.2,3.9,7.2h143.9 c1.6,0,2.7,1.3,2.4,2.7L183.6,507c-1,4.4,5.8,6.9,8.9,3.2l242.5-292c2.5-3,0.1-7.2-3.9-7.2L287.2,211L287.2,211z'/%3E%3Cg%3E%3Cpath class='st0' d='M321.3,186l74-186H438l74,186h-43.5l-11.9-32.5h-80.9l-12,32.5H321.3z M415.8,47.9l-27.2,70.7h54.9l-27.2-70.7 H415.8z'/%3E%3C/g%3E%3C/svg%3E";
  };
  e.prototype.render = function() {
    var e2 = this;
    var t = {};
    return h("div", { class: "camera-wrapper" }, h("div", { class: "camera-header" }, h("section", { class: "items" }, h("div", { class: "item close", onClick: function(t2) {
      return e2.handleClose(t2);
    } }, h("img", { src: this.iconExit() })), h("div", { class: "item flash", onClick: function(t2) {
      return e2.handleFlashClick(t2);
    } }, this.flashModes.length > 0 && h("div", null, this.flashMode == "off" ? h("img", { src: this.iconFlashOff() }) : "", this.flashMode == "auto" ? h("img", { src: this.iconFlashAuto() }) : "", this.flashMode == "flash" ? h("img", { src: this.iconFlashOn() }) : "")))), (this.hasCamera === false || !!this.deviceError) && h("div", { class: "no-device" }, h("h2", null, this.noDevicesText), h("label", { htmlFor: "_pwa-elements-camera-input" }, this.noDevicesButtonText), h("input", { type: "file", id: "_pwa-elements-camera-input", onChange: this.handleFileInputChange, accept: "image/*", class: "select-file-button" })), this.photoSrc ? h("div", { class: "accept" }, h("div", { class: "accept-image", style: Object.assign({ backgroundImage: "url(".concat(this.photoSrc, ")") }, t) })) : h("div", { class: "camera-video" }, this.showShutterOverlay && h("div", { class: "shutter-overlay" }), this.hasImageCapture() ? h("video", { ref: function(t2) {
      return e2.videoElement = t2;
    }, onLoadedMetaData: this.handleVideoMetadata, autoplay: true, playsinline: true }) : h("canvas", { ref: function(t2) {
      return e2.canvasElement = t2;
    }, width: "100%", height: "100%" }), h("canvas", { class: "offscreen-image-render", ref: function(t2) {
      return e2.offscreenCanvas = t2;
    }, width: "100%", height: "100%" })), this.hasCamera && h("div", { class: "camera-footer" }, !this.photo ? [!this.hidePicker && h("div", { class: "pick-image", onClick: this.handlePickFile }, h("label", { htmlFor: "_pwa-elements-file-pick" }, this.iconPhotos()), h("input", { type: "file", id: "_pwa-elements-file-pick", onChange: this.handleFileInputChange, accept: "image/*", class: "pick-image-button" })), h("div", { class: "shutter", onClick: this.handleShutterClick }, h("div", { class: "shutter-button" })), h("div", { class: "rotate", onClick: this.handleRotateClick }, h("img", { src: this.iconReverseCamera() }))] : h("section", { class: "items" }, h("div", { class: "item accept-cancel", onClick: function(t2) {
      return e2.handleCancelPhoto(t2);
    } }, h("img", { src: this.iconRetake() })), h("div", { class: "item accept-use", onClick: function(t2) {
      return e2.handleAcceptPhoto(t2);
    } }, h("img", { src: this.iconConfirm() })))));
  };
  Object.defineProperty(e, "assetsDirs", { get: function() {
    return ["icons"];
  }, enumerable: false, configurable: true });
  Object.defineProperty(e.prototype, "el", { get: function() {
    return getElement(this);
  }, enumerable: false, configurable: true });
  return e;
}();
CameraPWA.style = cameraCss;
export {
  CameraPWA as pwa_camera
};
