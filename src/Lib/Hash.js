var sjcl = require('sjcl');

exports.sha256 = function () { return new sjcl.hash.sha256(); };
exports.update = function (sha256) {
  return function (content) {
    return function () {
      sha256.update(content);
    };
  };
};

exports.finalize = function (sha256) {
  return function () {
    return sjcl.codec.hex.fromBits(sha256.finalize());
  };
};
