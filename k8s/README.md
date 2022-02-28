# Cheatsheet

https://kubernetes.io/docs/reference/kubectl/cheatsheet/

# Usage

See the Makefile for some shortcuts / things.

# Setup

Follow this guide:

http://blog.programmableproduction.com/2018/03/08/Archlinux-Setup-Minikube-using-KVM/

Mostly as root, do the following:

```sh
pikaur -Ss kubeadm
pacman -Ss kubernetes
pacman -Sy libvirt qemu-headless ebtables dnsmasq
systemctl enable libvirtd.service
systemctl enable virtlogd.service
systemctl start libvirtd.service
systemctl start virtlogd.service
pacman -Sy docker-machine
pikaur -Sy minikube-bin kubectl-bin
pikaur -Sy docker-machine-driver-kvm2
```

If on Ubuntu, do this:

```sh
# As root
apt-get install qemu-kvm
apt-get install libvirt-daemon libvirt-daemon-system libvirt-clients
apt-get install libvirt-daemon-driver-qemu libvirt-daemon-system-systemd
systemctl enable libvirtd.service
systemctl enable virtlogd.service
apt-get install docker

# As main user
sudo adduser `id -un` libvirt
sudo adduser `id -un` kvm
```

https://help.ubuntu.com/community/KVM/Installation
https://minikube.sigs.k8s.io/docs/drivers/kvm2/
https://minikube.sigs.k8s.io/docs/start/

(some gist script)
https://gist.github.com/ahungry/47f5e22c8c39a6a99d43b37fa9aa5af1

Then as user:

```sh
minikube start --vm-driver kvm2
```

It will likely error out, so also set up the network and permissions properly (as root).


```sh
virsh net-list --all
virsh net-start default
virsh net-autostart default
virsh net-list --all
usernmod -aG wheel mcarter
usermod -aG wheel mcarter
usermod -aG libvirt mcarter
usermod -aG kvm mcarter
shutdown -r now
virsh net-list --all
```

Then when going back to do `minikube start --vm-driver=kvm2` you should see it succeed.

Then resume here:

https://kubernetes.io/docs/tutorials/hello-minikube/

To start building a sample app or something (as root, `virsh list` should show minikube running).
Also, kubectl should have mentioned it is pointed to minikube in kvm2.

To see how the kvm is doing, check status with `virt-host-validate`

#  Loading up local file

https://stackoverflow.com/questions/42564058/how-to-use-local-docker-images-with-minikube#42564211

Add to the minikube docker via:

```sh
eval $(minikube docker-env)
docker build -t my-image .
```

# Notes

## Inner pod connectivity

Just call localhost:port for whatever the actual container is
listening on.

## Pod to Pod network connectivity

To make a pod to pod call, it looks like we must expose the pod via a
service, such as:

```yaml
# -*- mode: k8s -*-

# https://kubernetes.io/docs/concepts/services-networking/service/

---
apiVersion: v1
kind: Service
metadata:
  name: secondary-api
spec:
  type: LoadBalancer
  selector:
    app: secondary-app
  ports:
    - protocol: TCP
      port: 12346
      targetPort: 8080
```

Then, to request this API from another pod, it'd be:

```sh
curl http://secondary-api:12346
```

Note that the domain is the metadata.name, and the port maps to spec.ports[0].port

This will *only* work if k8s is running a DNS in cluster:

`kubectl get services kube-dns --namespace=kube-system`
