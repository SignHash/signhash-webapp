FROM node:8.5

RUN mkdir -p /usr/src/app
COPY . /usr/src/app
WORKDIR /usr/src/app

RUN rm -rf node_modules bower_components

ENV PATH="/usr/src/app/node_modules/.bin:${PATH}"

RUN npm install
# Somehow, the postinstall hook is not executed, so do it manually
RUN bower --allow-root install
