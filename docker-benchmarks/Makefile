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

ubuntu-build:
	docker build \
	--no-cache \
	-f Dockerfile_ubuntu \
	-t bench-ubuntu .

ubuntu-run:
	docker run \
	--rm \
	-it \
	-p 3002:3000 \
	bench-ubuntu
