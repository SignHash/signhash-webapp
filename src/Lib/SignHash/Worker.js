var Worker = require("worker-loader!./hashWorker.js");


exports.hashWorker = function () {
  return new Worker();
};


exports._calcHash = function (worker) {
  return function (file) {
    return function (onSuccess) {
      return function () {
        worker.onmessage = function (event) { onSuccess(event.data.hash)(); };
        worker.postMessage({ file: file });
      };
    };
  };
};
