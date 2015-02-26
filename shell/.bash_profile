#
# ~/.bash_profile
#
SOURCED_BASH_PROFILE=true
export EDITOR="emacs"

if [ -f ~/.bashrc ] && [ ! $SOURCED_BASHRC ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs
PATH=$PATH:$HOME/bin:$HOME/projects/pololu/system2/bin
BASH_ENV=$HOME/.bashrc

export BASH_ENV PATH
unset USERNAME

# Track assembly
export TRACK_ASSEMBLY_DIRECTORY=~/projects/pololu/track_assembly
function track_assembly {
 pushd .
 cd ~/projects/pololu/system2_for_track_assembly/website
 RAILS_ENV=preview bundle exec rails runner "script/track/track_assembly.rb" 
 popd
}

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
if [ -e /home/ryantm/.nix-profile/etc/profile.d/nix.sh ]; then . /home/ryantm/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
