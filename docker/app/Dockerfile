FROM node:8.5

RUN mkdir -p /usr/src/app

WORKDIR /usr/src/app

COPY package.json .
COPY bower.json .

RUN npm install

ENV PATH="/usr/src/app/node_modules/.bin:${PATH}"

RUN bower install --allow-root

COPY . .
