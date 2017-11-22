var R = require('ramda');


function requireContractData (name) {
  return require('signhash-contracts/build/contracts/' + name + '.json');
}


exports.signerContractData = requireContractData('SignHash');


exports._sign = R.curry(function(contract, checksum, address) {
  return function () {
    return contract.sign(checksum, { from: address });
  };
});
