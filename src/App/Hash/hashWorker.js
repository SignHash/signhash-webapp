var sjcl = require('sjcl');


onmessage = function (msg) {
  var reader = new FileReader();

  reader.onload = function (event) {
    var sha256 = new sjcl.hash.sha256();
    sha256.update(event.target.result);
    var hash = sjcl.codec.hex.fromBits(sha256.finalize());
    postMessage({
      hash: hash
    });
  };
  reader.readAsText(msg.data.file);
};
