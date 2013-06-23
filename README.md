vado
====

Quick way to run a command on a remote ssh server.

Mount the directory you want to run the command in using sshfs.
In that directory (or a sub directory) run vado like this...

    vado ls -l

vado will run 'mount' to identify the user account, server name
and the remote directory to run the command in.  It will then
run ssh to connect to the server and run the command.

You can pass ssh options like this...

    vado -t vim

This tells vado to pass -t to ssh (forces pseudo-tty allocation
and makes vim work nicely).


vagrant
=======

This is not tied to vagrant, but can be used with it and is
faster than 'vagrant ssh'.   If the user detected in 'mount'
is 'vagrant' and the host is '127.0.0.1' then it will default to
port 2222 and use '~/.vagrant.d/insecure_private_key' for
authentication.


install
=======

First install the Haskell Platform, then run...

    git clone https://github.com/hamishmack/vado.git
    cd vado
    cabal install

