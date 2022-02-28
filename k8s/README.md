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
apt-get install qemu-kvm
apt-get install libvirt-daemon libvirt-daemon-system libvirt-clients
apt-get install libvirt-daemon-driver-qemu libvirt-daemon-system-systemd
systemctl enable libvirtd.service
systemctl enable virtlogd.service
apt-get install docker
```

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
