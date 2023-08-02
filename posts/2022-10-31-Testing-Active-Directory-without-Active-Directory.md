---

title: Testing Active Directory without Active Directory
lastmod: 2022-10-31

---

Active Directory (AD) is a directory service developed by Microsoft. It is 
widely used by most medium to big company to store information about employees 
and manage access rights to resources such as shared directories, printers and 
so on.

AD stores and serves its data using [Lightweight Directory Access 
Protocol](https://en.wikipedia.org/wiki/Lightweight_Directory_Access_Protocol) 
or LDAP for short, the open standard for this kind of stuff.

Now let's say you develop a service and you want to offer the possibility to 
import data from an AD. How would you do ?

The most straight forward way would be to setup an AD lab following such [a 
tutorial](https://www.youtube.com/watch?v=xftEuVQ7kY0). It's great because it 
gives you access to an actual AD. But in my opinion, it has a couple of 
problem:
- It is not a really lightweight solution. It takes a lot of time to setup 
  something like this.
- It requires at least one Windows license which is not a good deal but still, 
  it would be better to do without.
- This is not an easily hackable and scriptable solution which is usually 
  needed when developing.

Looking into OSS alternatives, the most obvious stop would be to give a try to 
[openldap]. Indeed AD serves its data using the LDAP protocol, [openldap] is an 
OSS LDAP server, that should work. Unfortunately, it is not really widely 
knows, but [openldap] cannot easily simulate the LDAP side of an AD server. But 
[samba] does, and this is the right tool to reach out for testing AD 
integration in a software.

Now all we need to do is to wrap [samba] in some kind of VM or container. For 
this kind of stuff I like to use [vagrant] along [ansible]. I find these two 
tools a great combination.

Here is an example of a basic `Vagrantfile`.

```ruby
Vagrant.configure("2") do |config|

  config.vm.box = "peru/ubuntu-20.04-server-amd64"

  config.vm.provision :ansible do |ansible|
    ansible.extra_vars = {
      realm: "somewhere.local",
      domain: "somewhere",
      adminpass: "VerySecretPassword;",
    }
    ansible.playbook = "samba.yml"
  end

end

```

It creates an Ubuntu VM and provision it using this [ansible] playbook (save it 
as `samba.yml`):

```yaml
---
- hosts: all
  become: yes

  tasks:

  - name: "Install tools"
    apt:
      pkg:
      - ldap-utils
      - samba
      update_cache: yes

  - name: "Stop and disable smbd"
    systemd:
      name: smbd
      enabled: no
      state: stopped

  - block:

    - name: "Remove the original smb.conf"
      file:
        path: /etc/samba/smb.conf
        state: absent

    - name: "Provision the domain and create a new smb.conf"
      command:
        cmd: >
          samba-tool domain provision
          --server-role=dc
          --dns-backend=NONE
          --realm={{ realm }}
          --domain={{ domain }}
          --adminpass={{ adminpass }}
        creates: ldb.stat.path

  - name: "Enable only the ldap service"
    lineinfile:
      path: /etc/samba/smb.conf
      line: "server services = ldap, cldap"
      search_string: 'server services ='

  - name: "Disable strong auth for ldap"
    lineinfile:
      path: /etc/samba/smb.conf
      line: "ldap server require strong auth = no"
      insertafter: "server services ="

  - name: "Start samba-ad-dc"
    systemd:
      name: samba-ad-dc
      masked: no
      enabled: yes
      state: started

  - name: "Add a few users"
    shell: |
      samba-tool user create --random-password user-1
      samba-tool user create --random-password user-2
      samba-tool user create --random-password user-3
```

Then we can create and provision the VM using:

```
$ vagrant up
...
```

We need to lookup the IP address of the newly created VM.
```
$ vagrant ssh-config
Host default
  HostName 192.168.121.174
  User vagrant
  Port 22
  ...

```

Once the server is up and running we can search something in it just like we 
would have done with an actual AD server:

```
$ ldapsearch -H ldap://192.168.121.174:389 -D Administrator@somewhere.local -w "VerySecretPassword;" -b "cn=Users,dc=somewhere,dc=local" "(&(objectCategory=person)(objectClass=user))" | grep dn:
dn: CN=Guest,CN=Users,DC=somewhere,DC=local
dn: CN=user-1,CN=Users,DC=somewhere,DC=local
dn: CN=Administrator,CN=Users,DC=somewhere,DC=local
dn: CN=krbtgt,CN=Users,DC=somewhere,DC=local
dn: CN=user-3,CN=Users,DC=somewhere,DC=local
dn: CN=user-2,CN=Users,DC=somewhere,DC=local
dn: CN=dns-localhost,CN=Users,DC=somewhere,DC=local
```

And there you go! This demonstrates how to replace an AD by an easy deployable 
OSS solution. This can be used for testing or various development tasks. I have 
found this information difficult to find so hopefully that will help somebody 
out there !

[ansible]: https://www.ansible.com/
[openldap]: https://www.openldap.org/
[samba]: https://www.samba.org/
[vagrant]: https://www.vagrantup.com/
