# Setup

Follow this guide:

http://blog.programmableproduction.com/2018/03/08/Archlinux-Setup-Minikube-using-KVM/

Mostly as root, do the following:

```sh
pacman -Ss kubeamd
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
