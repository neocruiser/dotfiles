# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi


# set PATH for Tex Live environment variables
PATH=/usr/local/texlive/2014/bin/x86_64-linux:$PATH; export PATH 
  MANPATH=/usr/local/texlive/2014/texmf-dist/doc/man:$MANPATH; export MANPATH 
  INFOPATH=/usr/local/texlive/2014/texmf-dist/doc/info:$INFOPATH; export INFOPATH

# set PATH to BLAST bin commands
#export PATH="$PATH:/home/neocruiser/ncbi-blast-2.2.29+/bin"
PATH=/home/neo/blast/bin:$PATH; export PATH
BLASTDB=/home/neo/blast/db:$BLASTDB; export BLASTDB

# Plink
PATH=/media/Data/Dropbox/plink2:$PATH; export PATH

# R
PATH=/media/Data/Dropbox/R/install.sh:$PATH; export PATH


# BLAT
PATH=/home/neo/blat:$PATH; export PATH


# HMMER
PATH=/home/neo/hmmer/bin:$PATH; export PATH

# VCFTOOLS
PERL5LIB=/home/neo/vcftools/perl:$PATH; export PATH
PATH=/home/neo/vcftools/bin:$PATH; export PATH
