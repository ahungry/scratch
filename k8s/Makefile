# Step 1: make build
# Step 2: make start
# Step 3: make up
# Step 4: make forward

build:
	eval $$(minikube docker-env); docker build -t my-image .

up: kube-deployment
down: kube-down
stop: kube-stop
start: kube-start

kube-start:
	minikube start --vm-driver=kvm2 \
	--insecure-registry localhost:5000 \
	--memory 2048M \
	--disk-size 3G
	minikube dashboard

kube-stop:
	minikube stop

# This is a sample that manually creates a single pod
# via the CLI instead of yaml, it likely won't serve any requests or be of any use.
# as it is not defining port forwarding like the deployment.yaml is
kube-up: build
	kubectl run --generator=run-pod/v1 my-image \
	--image=my-image:latest \
	--image-pull-policy=Never
	kubectl get pods

kube-down:
	kubectl delete service blub
	kubectl delete pods --all
	kubectl delete deployments --all

kube-deployment:
	kubectl create -f deployment.yaml
	kubectl get deployments
	#kubectl expose deployment blub --type=LoadBalancer --port=8080
	minikube service blub

kube-apply:
	kubectl apply -f deployment.yaml

forward:
	kubectl port-forward deployment/blub 12345:8080

info:
	kubectl get pods
	kubectl get deployments
	kubectl get services
	minikube ip

#minikube start --insecure-registry localhost:5000 --memory 4096
#docker build . -f docker.collection -t localhost:5000/collection:latest -t localhost:5000/collection:dev
#docker push localhost:5000/collection
#cat deployment.yaml | kubectl apply -f -

.PHONY: up down kube-deployment kube-down kube-start kube-stop build info
