R = require('ramda');


function requireImg (name) {
  return require('../../static/images/' + name);
}


exports.images = R.map(requireImg, {
  logo: 'logo.png',
});
