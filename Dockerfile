FROM heroku/heroku:20-build
COPY . /webapp
WORKDIR /webapp
EXPOSE 8080
ENTRYPOINT dist-newstyle/build/x86_64-linux/ghc-8.10.4/reading-list-0.1.0.0/x/reading-list/build/reading-list/reading-list

