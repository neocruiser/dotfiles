# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

#
#

## MISCELLEANEOUS
#=================
# updates & cleans
alias update="sudo apt-get update && sudo apt-get upgrade"
alias clean="sudo apt-get autoremove && sudo apt-get autoclean"

# Watch updated every second for free
 alias wn="watch -n 1 -d free -m"

# clear terminal from its output
alias c='clear'

# download TORRENT
alias torrent="transmission-cli -ep -D -u 50 -w /home/neo/Downloads"
#alias torrent="transmission-cli -ep -D -u 50 "

# untar multiple files
tarM(){
  ls $1 | xargs -i tar xzvf {}
}

# FTP to ncbi database
alias ftp_ncbi="ftp ftp.ncbi.nlm.nih.gov"
#
#

## DIRECTORY WORK
#=================
# List top ten largest files/directories in current directory
alias ducks='du -cks *|sort -rn|head -11'

# Paginated color tree
treeC(){
  tree -C $* | less -R
}

# random facts
alias fact="elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"

# reload shell
alias reload='source $HOME/.zshrc 1>/dev/null'
#
#

## LIST with ls
#===============
# ls last changed file
alias lsc='ls -altcrgGh'

# ls sort extension
alias lsx='ls -lXBGgh'

# Only list directories, including hidden ones, changed descending order
alias lsd='ls -altcrgGh | grep "^d"'

#
#

## SEARCH inside files
#=====================
# ack help less
alias ackh='ack --help | less'
alias agh='ag --help | less'

# search for pattern and print less
agp(){
  ag -aiH -A$1 $2 -G $3
}

# look inside alias for a term
alias aga="alias | ag "


# search history
alias aghs='history | ag '

#
#

## GIT
#======
# git log output in color
alias gl="git log --graph --full-history --pretty=format:'%Cred%h%Creset -%Cgreen%d%Creset %s %Cblue(%cr)%Creset ' --abbrev-commit --date=relative"

#
#

## SYSTEM
#=========
# sudo install 
alias install='sudo apt-get install'

# open anyfile with the default command for that file
alias open='xdg-open'

# take screenshot with scrot
alias scrot_="scrot '%Y-%m-%d_scrot.png' -q '100'"


# get the number of rows and columns in a file
dim(){
        cat $1 | wc -l && awk '{ if(NF>max) max=NF } END {print max}' $1
}

# Quiet R
alias R='R --quiet'
alias sR='sudo R --quiet'

## download data from bluemoon
alias sblue='sftp sbassim@bluemoon-user1'

## upload data to bluemoon
upblue(){ # path to the file
        scp $1 sbassim@bluemoon-user1:/users/s/b/sbassim/
}

## stream torrent with peerflix and vlc
peer(){
peerflix $1 --vlc -- --fullscreen
}

## autoclean and autoremove
alias autoclean='sudo apt-get autoclean && sudo apt-get autoremove'