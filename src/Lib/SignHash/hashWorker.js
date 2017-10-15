var asmCrypto = require('asmcrypto.js');


onmessage = function (msg) {
  var reader = new FileReader();

  reader.onload = function (event) {
    var hash = asmCrypto.SHA256.hex(event.target.result);
    postMessage({ hash: hash });
  };
  reader.readAsArrayBuffer(msg.data.file);
};
