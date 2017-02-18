#
# ~/.bash_profile
#
SOURCED_BASH_PROFILE=true
export EDITOR="emacs"

if [ -f ~/.bashrc ] && [ ! $SOURCED_BASHRC ]; then
    . ~/.bashrc
fi

POLOLU_DIR=$HOME/p/pololu

# User specific environment and startup programs
PATH=$PATH:$HOME/bin:$POLOLU_DIR/system2/bin:$HOME/.local/bin
BASH_ENV=$HOME/.bashrc

export BASH_ENV PATH
unset USERNAME

# Track assembly
export TRACK_ASSEMBLY_DIRECTORY=$POLOLU_DIR/track_assembly
function track_assembly {
 pushd .
 cd $POLOLU_DIR/system2_for_track/website
 /opt/pololu_rails_env/preview rails runner "script/track/track_assembly.rb"
 popd
}

# Track users permissions
export TRACK_USERS_PERMISSIONS_DIRECTORY=$POLOLU_DIR/track_users_permissions
function track_users_permissions {
 pushd .
 cd $POLOLU_DIR/system2_for_track/website
 /opt/pololu_rails_env/preview rails runner "script/track/track_users_permissions.rb"
 popd
}

if [ -e /home/ryantm/.nix-profile/etc/profile.d/nix.sh ]; then . /home/ryantm/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

byobu_sourced=1 . byobu-launch 2>/dev/null || true
