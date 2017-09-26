exports.getEventFiles = function (ev) {
  return ev.target.files || [];
};

exports.getFileData = function (file) {
  return {
    name: file.name,
  };
};
