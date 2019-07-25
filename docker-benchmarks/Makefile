alpine-build:
	docker build \
	--no-cache \
	-f Dockerfile_alpine \
	-t bench-alpine .

alpine-run:
	docker run \
	--rm \
	-it \
	-p 3001:3000 \
	bench-alpine
