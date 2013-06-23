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
and makes vim work nicely)


install
=======

First install the Haskell Platform, then run...

    git clone https://github.com/hamishmack/vado.git
    cd vado
    cabal install

