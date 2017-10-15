# SignHash Web UI

[![Codeship](https://img.shields.io/codeship/87c04150-8dac-0135-07c8-724a96aff59c.svg)](https://app.codeship.com/projects/249705)

SignHash dApp

## Installation

Clone the repository and run `npm install` to get started:

```sh
npm install
npm start
```

After compiling the app should be available at `http://localhost:8080`.

### Directory structure

- `src`: Application source code.
- `static`: Static files served with application.
- `support`: Support files for building.
  - `support/entry.js`: Webpack entry point. Handles hot reloading.
- `bower.json`: Bower package configuration.
- `package.json`: Node package configuration.
- `webpack.config.js`: Webpack configuration.

### NPM scripts

#### watch

`npm start` or `npm run watch` will start a development server, which
hot-reloads your application when sources changes.

#### serve

`NODE_ENV=production npm run serve` builds your application and starts a
production server.

#### build

`npm run build` builds application client and server bundles.
