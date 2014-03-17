#!/usr/bin/env bash

# PostgreSQL
sidsrc=/etc/apt/sources.list.d/sid-src.list
echo "deb-src http://ftp.fr.debian.org/debian/ sid main" | sudo tee $sidsrc

pgdg=/etc/apt/sources.list.d/pgdg.list
pgdgkey=https://www.postgresql.org/media/keys/ACCC4CF8.asc
echo "deb http://apt.postgresql.org/pub/repos/apt/ wheezy-pgdg main" | sudo tee $pgdg

wget --quiet -O - ${pgdgkey} | sudo apt-key add -

iolib=/etc/apt/sources.list.d/iolib.list
iolibkey=http://download.opensuse.org/repositories/home:/sionescu/Debian_Wheezy/Release.key
echo "deb http://download.opensuse.org/repositories/home:/sionescu/Debian_Wheezy/ ./" |sudo tee $iolib

wget --quiet -O - ${iolibkey} | sudo apt-key add -

sudo apt-get update
sudo apt-get install -y postgresql-9.3 postgresql-contrib-9.3 \
                        postgresql-9.3-ip4r                   \
                        sbcl                                  \
                        git patch unzip                       \
                        devscripts pandoc                     \
                        postgresql-server-dev-all             \
                        libarchive-dev libfixposix-dev        \
                        libjansson-dev libcurl4-openssl-dev   \
                        libbsd-dev

sudo apt-get -y build-dep postgresql-9.3

# SBCL
#
# we need to backport SBCL from sid to have a recent enough version of the
# compiler and run time we depend on
sudo apt-get -y build-dep sbcl
sudo apt-get source -b sbcl > /dev/null 2>&1 # too verbose
sudo dpkg -i *.deb

HBA=/etc/postgresql/9.3/main/pg_hba.conf
echo "local all all trust"              | sudo tee $HBA
echo "host  all all 127.0.0.1/32 trust" | sudo tee -a $HBA

sudo pg_ctlcluster 9.3 main reload
createuser -U postgres -SdR `whoami`
createdb -U `whoami` pginstall

make -C /vagrant pginstall
