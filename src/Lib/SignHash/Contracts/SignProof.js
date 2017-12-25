var R = require('ramda');


exports._addProof = R.curry(function(contract, key, value, address) {
  return function () {
    return contract.add(key, value, { from: address });
  };
});
