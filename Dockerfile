FROM haskell 

RUN mkdir /tmp/project

WORKDIR /tmp/project

RUN cabal update 

COPY . .

RUN cabal install

RUN cp /root/.cabal/bin/funcons-repl /usr/bin/funcons-repl

RUN rm -rf /tmp/project
