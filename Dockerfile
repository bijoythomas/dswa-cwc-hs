FROM heroku/heroku:16

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create /opt/servant-on-heroku/bin and /opt/servant-on-heroku/src.  Set
# /opt/servant-on-heroku/src as the working directory.
RUN mkdir -p /opt/sample-project/src
RUN mkdir -p /opt/sample-project/bin
WORKDIR /opt/sample-project/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/sample-project/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/sample-project/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./sample-project.cabal /opt/sample-project/src/sample-project.cabal
RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/sample-project/src
RUN stack --no-terminal build

# Install application binaries to /opt/sample-project/bin.
RUN stack --no-terminal --local-bin-path /opt/sample-project/bin install

# Remove source code.
#RUN rm -rf /opt/sample-project/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/sample-project
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/sample-project/bin"

# Set the working directory as /opt/sample-project/.
WORKDIR /opt/sample-project

CMD /opt/sample-project/bin/sample-project