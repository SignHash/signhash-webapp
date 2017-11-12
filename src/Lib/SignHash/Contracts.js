var R = require('ramda');
var buildContract = require('truffle-contract');


function requireABI (name) {
  return require('signhash-contracts/build/contracts/' + name + '.json');
}


var abis = R.map(requireABI, {
  SignHash: 'SignHash',
});


var getContract = R.curry(function (abi, web3) {
  var contract = buildContract(abi);
  contract.setProvider(web3.currentProvider);
  return contract;
});


exports.loadSignerContract = getContract(abis.SignHash);

exports._sign = R.curry(function(contract, checksum, address) {
  return function () {
    return contract.sign(checksum, { from: address });
  };
});
