exports._sliceFile = function (file, start, end) {
  return file.slice(start, end);
};


exports._readBlobAsync = function (onSuccess) {
  return function (blob) {
    return function (reader) {
      return function () {
        reader.onload = function (event) {
          onSuccess(event.target.result)();
        };
        reader.readAsText(blob);
      };
    };
  };
};
