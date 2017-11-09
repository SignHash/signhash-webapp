var Worker = require("worker-loader?inline=true!./hashWorker.js");


exports.hashWorker = function () {
  return new Worker();
};


exports._calcHash = function (worker) {
  return function (file) {
    return function (onError, onSuccess) {
      worker.onmessage = function (event) { onSuccess(event.data.hash); };
      worker.postMessage({ file: file });
      return function (cancelError, cancelerError, cancelerSuccess) {
        worker.terminate();
        cancelerSuccess();
      };
    };
  };
};
