default: docker-build docker-run

docker-build:
	docker build -t counter-gui .

docker-run:
	sh ./run-docker.sh

alpine-build:
	docker build -f Dockerfile_alpine -t my-alpine-clojure .

alpine-run:
	sh ./run-alpine.sh
