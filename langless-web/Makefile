all: docker-build docker-run

docker-build:
	docker build -t langless-web .

docker-run:
	docker run --rm -it -p 6666:80 langless-web
