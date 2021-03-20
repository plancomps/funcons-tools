FROM haskell 

RUN mkdir /tmp/project

WORKDIR /tmp/project

RUN cabal update 

COPY . .

RUN apt-get update
RUN apt-get install libreadline-dev

RUN cabal install

RUN cp /root/.cabal/bin/funcons-repl /usr/bin/funcons-repl

RUN rm -rf /tmp/project
