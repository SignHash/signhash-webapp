FROM mhart/alpine-node

RUN apk add --update curl
RUN apk --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ add \
 chromium xwininfo xvfb dbus eudev ttf-freefont fluxbox

RUN mkdir -p /work/node_modules
WORKDIR /work

RUN adduser -D user
RUN chown -R user:user /work

USER user

EXPOSE 1337 1338

COPY docker/entrypoint.sh /testcafe-docker.sh
ENTRYPOINT ["/testcafe-docker.sh"]
