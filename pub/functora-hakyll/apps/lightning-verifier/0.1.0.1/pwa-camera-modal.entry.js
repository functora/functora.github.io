import{h,r as registerInstance,c as createEvent}from"./main.js";var __awaiter=function(e,t,n,r){return new(n||(n=Promise))((function(o,i){function a(e){try{s(r.next(e))}catch(e){i(e)}}function c(e){try{s(r.throw(e))}catch(e){i(e)}}function s(e){e.done?o(e.value):function(e){return e instanceof n?e:new n((function(t){t(e)}))}(e.value).then(a,c)}s((r=r.apply(e,t||[])).next())}))},__generator=function(e,t){var n,r,o,i,a={label:0,sent:function(){if(1&o[0])throw o[1];return o[1]},trys:[],ops:[]};return i={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(i[Symbol.iterator]=function(){return this}),i;function c(c){return function(s){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;i&&(i=0,c[0]&&(a=0)),a;)try{if(n=1,r&&(o=2&c[0]?r.return:c[0]?r.throw||((o=r.return)&&o.call(r),0):r.next)&&!(o=o.call(r,c[1])).done)return o;switch(r=0,o&&(c=[2&c[0],o.value]),c[0]){case 0:case 1:o=c;break;case 4:return a.label++,{value:c[1],done:!1};case 5:a.label++,r=c[1],c=[0];continue;case 7:c=a.ops.pop(),a.trys.pop();continue;default:if(!(o=a.trys,(o=o.length>0&&o[o.length-1])||6!==c[0]&&2!==c[0])){a=0;continue}if(3===c[0]&&(!o||c[1]>o[0]&&c[1]<o[3])){a.label=c[1];break}if(6===c[0]&&a.label<o[1]){a.label=o[1],o=c;break}if(o&&a.label<o[2]){a.label=o[2],a.ops.push(c);break}o[2]&&a.ops.pop(),a.trys.pop();continue}c=t.call(e,a)}catch(e){c=[6,e],r=0}finally{n=o=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,s])}}},cameraModalCss=":host{z-index:1000;position:fixed;top:0;left:0;width:100%;height:100%;display:-ms-flexbox;display:flex;contain:strict}.wrapper{-ms-flex:1;flex:1;display:-ms-flexbox;display:flex;-ms-flex-align:center;align-items:center;-ms-flex-pack:center;justify-content:center;background-color:rgba(0, 0, 0, 0.15)}.content{-webkit-box-shadow:0px 0px 5px rgba(0, 0, 0, 0.2);box-shadow:0px 0px 5px rgba(0, 0, 0, 0.2);width:600px;height:600px}",PWACameraModal=function(){function e(e){registerInstance(this,e),this.onPhoto=createEvent(this,"onPhoto",7),this.noDeviceError=createEvent(this,"noDeviceError",7),this.facingMode="user",this.hidePicker=!1}return e.prototype.present=function(){return __awaiter(this,void 0,void 0,(function(){var e,t=this;return __generator(this,(function(n){return(e=document.createElement("pwa-camera-modal-instance")).facingMode=this.facingMode,e.hidePicker=this.hidePicker,e.addEventListener("onPhoto",(function(e){return __awaiter(t,void 0,void 0,(function(){var t;return __generator(this,(function(n){return this._modal?(t=e.detail,this.onPhoto.emit(t),[2]):[2]}))}))})),e.addEventListener("noDeviceError",(function(e){return __awaiter(t,void 0,void 0,(function(){return __generator(this,(function(t){return this.noDeviceError.emit(e),[2]}))}))})),document.body.append(e),this._modal=e,[2]}))}))},e.prototype.dismiss=function(){return __awaiter(this,void 0,void 0,(function(){return __generator(this,(function(e){return this._modal?(this._modal&&this._modal.parentNode.removeChild(this._modal),this._modal=null,[2]):[2]}))}))},e.prototype.render=function(){return h("div",null)},e}();PWACameraModal.style=cameraModalCss;export{PWACameraModal as pwa_camera_modal};