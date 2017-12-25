var R = require('ramda');


exports._sign = R.curry(function(contract, checksum, address) {
  return function () {
    return contract.sign(checksum, { from: address });
  };
});
