[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

BACKGROUND_IMAGE=~/background_images/pololu-valley-hawaii.jpg
[[ -f "$BACKGROUND_IMAGE" ]] && feh --bg-scale $BACKGROUND_IMAGE

exec xmonad
