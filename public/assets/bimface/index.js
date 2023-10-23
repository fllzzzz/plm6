"use strict";

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

var HTTP = new Object();
HTTP.url = window.location.href;

HTTP.cleanUrl = function () {
  var index = function (url) {
    var i = url.indexOf('?');
    return -1 < i ? i : url.indexOf('#');
  }(this.url);

  return -1 == index ? this.url : this.url.substr(0, index);
};

HTTP._get = function () {
  var index = this.url.indexOf('?');

  if (-1 == index) {
    return null;
  }

  var paramStr = this.url.substr(index + 1);

  if (1 > paramStr.length) {
    return null;
  }

  return function (paramStr) {
    var kvStrs = paramStr.split('&');

    if (1 > kvStrs.length) {
      return null;
    }

    var kvs = new Object();
    var i = 0;

    for (; i < kvStrs.length; i++) {
      var kv = kvStrs[i].split('=');

      if (2 == kv.length) {
        kvs[kv[0]] = kv[1];
      }
    }

    return kvs;
  }(paramStr);
};

var Cookie =
/*#__PURE__*/
function () {
  function Cookie() {
    _classCallCheck(this, Cookie);
  }

  _createClass(Cookie, [{
    key: "set",
    value: function set(name, value) {
      var exp = new Date();
      exp.setTime(exp.getTime() + 12 * 60 * 60 * 1000);
      document.cookie = name + "=" + escape(value) + ";expires=" + exp.toGMTString();
    }
  }, {
    key: "get",
    value: function get(name) {
      var arr,
          reg = new RegExp("(^| )" + name + "=([^;]*)(;|$)");

      if (arr = document.cookie.match(reg)) {
        return unescape(arr[2]);
      } else {
        return null;
      }
    }
  }]);

  return Cookie;
}();

var cookie = new Cookie();
var hostDefault = {
  local: {
    SDKLoaderPath: './static/BimfaceSDKLoader/BimfaceSDKLoader@latest-debug.js',
    staticHost: './static'
  },
  dev: {
    SDKLoaderPath: 'http://10.5.3.5/Packages/jssdk-dev/web/BimfaceSDKLoader/BimfaceSDKLoader@latest-debug.js',
    staticHost: 'http://10.5.3.5/Packages/jssdk-dev/web'
  },
  portal: {
    SDKLoaderPath: 'https://static.bimface.com/api/BimfaceSDKLoader/BimfaceSDKLoader@latest-release.js',
    staticHost: 'https://static.bimface.com/api'
  }
};

var host = host || hostDefault;

function initBimfaceApp({ viewToken }) {
  var params = HTTP._get();

  var mode;

  if (params) {
    mode = params.mode || 'portal';
  } else {
    mode = 'portal';
  }
  return new Promise(function (resolve, reject) {
    getBimfaceSDKLoader(host[mode].SDKLoaderPath).then(function (response) {
      console.log(response,'ff');
      var BimfaceLoaderConfig = new BimfaceSDKLoaderConfig();
      BimfaceLoaderConfig = Object.assign({}, BimfaceLoaderConfig, params, {
        build:"Debug",
        staticHost: host[mode].staticHost
      });
      console.log(BimfaceLoaderConfig,'BimfaceLoaderConfig');
      if(mode === 'portal'){
        BimfaceLoaderConfig.build = 'Release';
      }

      if (BimfaceLoaderConfig.path) {
        BimfaceLoaderConfig.dataEnvType = BimfaceEnvOption.Local;
      }

      if (!BimfaceLoaderConfig.viewToken) {
        BimfaceLoaderConfig.viewToken = viewToken
      }

      BimfaceLoaderConfig.version = '3.6.260';

      if (BimfaceLoaderConfig.viewToken || BimfaceLoaderConfig.resourcePath || BimfaceLoaderConfig.path || BimfaceLoaderConfig.dataPath) {
        BimfaceSDKLoader.load(BimfaceLoaderConfig, function (data) {
          resolve(data);
        }, reject);
      }
    });
  });
}


function getBimfaceSDKLoader(url) {
  return new Promise(function (resolve, reject) {
    var element = document.createElement('script');
    element.src = url;
    document.head.appendChild(element);
    element.addEventListener("load", function () {
      resolve('success');
    },{ passive: false });
    element.addEventListener("error", function (error) {
      reject(error);
    },{ passive: false });
  });
}

window.initBimfaceApp = initBimfaceApp;

export {
  initBimfaceApp
};
