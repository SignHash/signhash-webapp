var R = require('ramda');


exports._sign = R.curry(function(contract, checksum, address) {
  return function () {
    return contract.sign(checksum, { from: address });
  };
});


exports._addProof = R.curry(function(contract, key, value, address) {
  return function () {
    return contract.addProof(key, value, { from: address });
  };
});
