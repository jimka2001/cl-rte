FROM lokedhs/sbcl-quicklisp

RUN apt-get update                              \
  && RUNLEVEL=1 DEBIAN_FRONTEND=noninteractive  \
     apt-get install -y --no-install-recommends \
       graphviz \
       gnuplot \
       git \
       csh \
       curl \
       gnupg \
       default-jdk \
  && echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list \
  && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add \
  && apt-get update \
  && RUNLEVEL=1 DEBIAN_FRONTEND=noninteractive \
     apt-get install -y --no-install-recommends \
       sbt \
  && apt-get autoremove                         \
  && apt-get clean \
  && sbt sbtVersion
