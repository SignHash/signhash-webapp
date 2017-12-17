function requireImg (name) {
  return require('../../static/images/' + name);
}


exports.images = {
  logo: requireImg('logo.svg'),
  ethIcon: requireImg('fa-icon-ethereum.svg'),
};
