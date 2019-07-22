FROM ubuntu:19.04

WORKDIR /usr/src/myapp

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install xorg \
  openbox \
  openjdk-12-jdk \
  clojure \
  leiningen

COPY project.clj /usr/src/myapp/
RUN lein deps

COPY . /usr/src/myapp

# Let this run as non-root ownership
RUN useradd dummy
RUN yes dummy | passwd dummy
RUN mkdir -p /home/dummy

RUN mv "$(lein uberjar | sed -n 's/^Created \(.*standalone\.jar\)/\1/p')" myapp-standalone.jar

COPY ./resources /usr/src/myapp/resources/

WORKDIR /usr/src/myapp

ENTRYPOINT ["java", "-jar", "/usr/src/myapp/myapp-standalone.jar"]
