function recreate-build-dir()
{
   rm -r build
   mkdir build
   cd build
}

function remake-autogen-project()
{
    ./autogen.sh --prefix=$HOME/staging --enable-debug
    make clean && make && make install
}

function remake-unity()
{
    recreate-build-dir
    cmake .. -DCMAKE_BUILD_TYPE=Debug -DCOMPIZ_PLUGIN_INSTALL_TYPE=local -DCMAKE_INSTALL_PREFIX=$HOME/staging/ -DGSETTINGS_LOCALINSTALL=ON
    make  && make install
}

function unity-env
{
 export PATH=~/staging/bin:$PATH
 export XDG_DATA_DIRS=~/.config/compiz-1/gsettings/schemas:~/staging/share:/usr/share:/usr/local/share
 export LD_LIBRARY_PATH=~/staging/lib:${LD_LIBRARY_PATH}
 export LD_RUN_PATH=~/staging/lib:${LD_RUN_PATH}
 export PKG_CONFIG_PATH=~/staging/lib/pkgconfig:${PKG_CONFIG_PATH}
 export PYTHONPATH=~/staging/lib/python2.7/site-packages:$PYTHONPATH
}

function go-gpg
{
  keychain --dir ~/.ssh/.keychain ~/.ssh/id_rsa D2CA61D6
  source ~/.ssh/.keychain/$HOSTNAME-sh
  source ~/.ssh/.keychain/$HOSTNAME-sh-gpg
}

# Git user configuration scripts
function git_config_user {
  git config --replace-all user.name "$1"
  git config --replace-all user.email "$2"
}
function git_check_user_config {
  echo "Name: `git config user.name`"
  echo "Email: `git config user.email`"
}
function git_work_private {
  git_config_user "Ryan Mulligan" "ryantm@pololu.com"
  git_check_user_config
}
function git_work_public {
  git_config_user "RyanTM (Pololu)" "dev-ryantm@pololu.com"
  git_check_user_config
}
function git_personal {
  git_config_user "Ryan Mulligan" "ryan@ryantm.com"
  git_check_user_config
}

function nix? {
  sudo nix-env -qa \* -P | fgrep -i "$1"
}