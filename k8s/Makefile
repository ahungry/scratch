build:
	eval $(minikube docker-env)
	docker build -t my-image .

kube-start:
	minikube start --vm-driver=kvm2 \
	--insecure-registry localhost:5000 \
	--memory 1024M \
	--disk-size 1G
	minikube dashboard

kube-stop:
	minikube stop

kube-up: build
	kubectl run --generator=run-pod/v1 my-image \
	--image=my-image:latest \
	--image-pull-policy=Never
	kubectl get pods

kube-down:
	kubectl delete deployments --all
	kubectl delete pods --all

kube-deployment:
	kubectl create -f deployment.yaml
	kubectl get deployments
	kubectl expose deployment blub --type=LoadBalancer --port=8080
	minikube service blub

kube-apply:
	kubectl apply -f deployment-pod.yaml

#minikube start --insecure-registry localhost:5000 --memory 4096
#docker build . -f docker.collection -t localhost:5000/collection:latest -t localhost:5000/collection:dev
#docker push localhost:5000/collection
#cat deployment.yaml | kubectl apply -f -
