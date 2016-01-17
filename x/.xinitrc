[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

# BACKGROUND_IMAGE=~/background_images/20150510_sterling_vinyard.jpg
# [[ -f "$BACKGROUND_IMAGE" ]] && feh --bg-fill $BACKGROUND_IMAGE

exec xmonad # /home/ryantm/.xmonad/result/xmonad
