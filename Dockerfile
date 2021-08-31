FROM heroku/heroku:20-build
COPY . /webapp
WORKDIR /webapp
