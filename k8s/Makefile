# https://kubernetes.io/docs/tutorials/hello-minikube/

# Step 1: make build    (create the properly tagged docker image)
# Step 2: make start    (boot up minikube env)
# Step 3: make up       (deployment creation, made up of a pod (replicas settings), of containers)

# -- basically optional/manual ways of doing what we would actually do via yaml files
# Step 4: make forward  (non-load balanced way to hit a single container in pod)
# Step 5: make expose   (load balanced way to make a deployment accessible on the external net)

# -- Diagnostic output
# Step 6: make service  (minikube way to get a callable local IP pointed to the expose)


build:
	eval $$(minikube docker-env); docker build -t my-image .
	eval $$(minikube docker-env); docker build -f Dockerfile_secondary -t my-secondary .

up: kube-deployment
down: kube-down
stop: kube-stop
start: kube-start

# https://medium.com/codex/setup-grafana-loki-on-local-k8s-cluster-minikube-90450e9896a8
install-addons:
	minikube addons enable ingress

# do this first, as it adds the grafana/loki-stack
install-grafana:
	-kubectl create namespace monitoring
	helm repo add grafana 'https://grafana.github.io/helm-charts'
	helm repo update
	helm search repo grafana
	helm upgrade --install grafana --namespace monitoring -f grafana-values.yaml grafana/grafana
	kubectl get secret --namespace monitoring grafana -o jsonpath="{.data.admin-password}" | base64 --decode ; echo
	kubectl get svc -A
	kubectl get ingress -A

install-loki:
	-kubectl create namespace monitoring
	helm repo add loki 'https://grafana.github.io/loki/charts'
	helm repo update
	helm search repo loki
	helm upgrade --install loki --namespace=monitoring grafana/loki-stack
	kubectl get statefulset -A
	kubectl get daemonset -A

# https://kubernetes.github.io/ingress-nginx/deploy/
install-nginx-ingress:
	minikube addons enable ingress
	kubectl apply -f example-ingress.yaml
	kubectl get ingress
	minikube ip

get-grafana-password:
	kubectl get secret --namespace monitoring grafana -o jsonpath="{.data.admin-password}" | base64 --decode ; echo
	$(info Add output from 'minikube ip' into /etc/hosts as grafana.minikube - username is 'admin')
	$(info When going into dashboard, add "Loki" as a datasource with http://loki:3100 in URL)

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
	kubectl delete services --all
	kubectl delete pods --all
	kubectl delete deployments --all

kube-deployment:
	make build
	kubectl create -f deployment.yaml
	kubectl create -f deployment_secondary.yaml
	kubectl create -f service.yaml
	kubectl create -f service_secondary.yaml
	kubectl get deployments
	minikube service my-service # Get a local callable url to hit load balanced pods

expose:
	kubectl expose deployment blub --type=LoadBalancer --port=8080

kube-apply-service:
	kubectl apply -f service.yaml

kube-apply:
	kubectl apply -f deployment.yaml

forward:
	kubectl port-forward deployment/blub 12345:8080

get-dns:
	kubectl get services kube-dns --namespace=kube-system

get-service:
	minikube service list
	minikube service my-service

rollout:
	eval $$(minikube docker-env); docker build -t my-image .
	eval $$(minikube docker-env); docker build -f Dockerfile_secondary -t my-secondary .
	kubectl rollout restart deployment/blub
	kubectl rollout restart deployment/secondary-api

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
