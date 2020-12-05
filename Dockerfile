# to create this container, run: "docker build -t menard ." in this directory.
FROM clojure:openjdk-11-lein
RUN mkdir -p /menard
WORKDIR /menard
COPY project.clj /menard
RUN lein deps
COPY src /menard/src
COPY resources /menard/resources
RUN mv "$(lein uberjar | sed -n 's/^Created \(.*standalone\.jar\)/\1/p')" menard-standalone.jar
RUN rm -rf /menard/target
CMD ["lein","run"]


