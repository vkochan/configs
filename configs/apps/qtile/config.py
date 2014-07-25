# Note that since qtile configs are just python scripts, you can check for
# syntax and runtime errors by just running this file as is from the command
# line, e.g.:
#
#    python config.py

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook

# The screens variable contains information about what bars are drawn where on
# each screen. If you have multiple screens, you'll need to construct multiple
# Screen objects, each with whatever widgets you want.
#
# Below is a screen with a top bar that contains several basic qtile widgets.

top_bar = bar.Bar([
    # This is a list of our virtual desktops.
    # A prompt for spawning processes or switching groups. This will be
    # invisible most of the time.
    widget.Prompt(prompt = ">"),

    # Current window name.
    # widget.WindowName(foreground = "a0a0a0",),
    # widget.sep.Sep(foreground='7b5830'), #add separator bars where deemed necessary
    widget.Notify(),
    # widget.CPUGraph(width=42, line_width=2,
    #     graph_color='0066FF', fill_color='001188'),
    widget.TextBox(text = u"\u231a", foreground = "008000", fontsize = 30, padding = 0),
    widget.Clock('%H:%M | %d-%m-%Y %a', foreground = "008000"),
    widget.TextBox(text = u"\u25c0)", foreground = "70ff70", padding = 0),
    widget.Volume(foreground = "70ff70", update_interval = 1),
    widget.TextBox(text = u" \u26a1", foreground = "0066ff", fontsize = 30, padding = 0),
    widget.Battery(
        energy_now_file='charge_now',
        energy_full_file='charge_full',
        power_now_file='current_now',
        foreground = "0066ff",
        ),
    widget.ThermalSensor(foreground = "ff0000", update_interval = 5),
    widget.TextBox(text = u"\u2328", fontsize = 30, padding = 0),
    widget.KeyboardLayout(configured_keyboards=["us", "ua"]),
    # widget.Wlan(interface = "wlp3s0", update_interval = 5),
    widget.Systray(),
    ], 30)

bottom_bar = bar.Bar([
    widget.GroupBox(
        urgent_alert_method='text',
        fontsize=12,
        this_current_screen_border='7b5830'),
    ], 25)

screens = [
        Screen(top=top_bar, bottom=bottom_bar),
        Screen(),
]

# Super_L (the Windows key) is typically bound to mod4 by default, so we use
# that here.
mod = "mod4"
alt = "mod1"

# The keys variable contains a list of all of the keybindings that qtile will
# look through each time there is a key pressed.
keys = [
    # Log out; note that this doesn't use mod4: that's intentional in case mod4
    # gets hosed (which happens if you unplug and replug your usb keyboard
    # sometimes, or on system upgrades). This way you can still log back out
    # and in gracefully.
    Key(["shift", "mod1"], "q",  lazy.shutdown()),

    Key([mod], "k",              lazy.layout.down()),
    Key([mod], "j",              lazy.layout.up()),
    Key([mod], "h",              lazy.layout.previous()),
    Key([mod], "l",              lazy.layout.previous()),
    Key([mod, "shift"], "space", lazy.layout.rotate()),
    Key([mod, "shift"], "Return",lazy.layout.toggle_split()),
    Key([alt], "Tab",           lazy.nextlayout()),
    Key([mod], "x",              lazy.window.kill()),

    # interact with prompts
    Key([mod], "r",              lazy.spawncmd()),
    Key([mod], "g",              lazy.switchgroup()),

    # start specific apps
    Key([mod], "n",              lazy.spawn("firefox")),
    Key([mod], "Return",         lazy.spawn("terminator")),

    # Change the volume if your keyboard has special volume keys.
    Key(
        [], "XF86AudioRaiseVolume",
        lazy.spawn("amixer -c 0 -q set Master 2dB+")
    ),
    Key(
        [], "XF86AudioLowerVolume",
        lazy.spawn("amixer -c 0 -q set Master 2dB-")
    ),
    Key(
        [], "XF86AudioMute",
        lazy.spawn("amixer -c 0 -q set Master toggle")
    ),

    # Also allow changing volume the old fashioned way.
    Key([mod], "equal", lazy.spawn("amixer -c 0 -q set Master 2dB+")),
    Key([mod], "minus", lazy.spawn("amixer -c 0 -q set Master 2dB-")),

    # restart qtile
    Key([mod, "control"], "r", lazy.restart()),

    # Move windows up or down in current stack
    Key([mod, "control"], "k", lazy.layout.shuffle_down()),
    Key([mod, "control"], "j", lazy.layout.shuffle_up()),

    # Switch screens
    Key([mod], "1",
        lazy.to_screen(0), lazy.group.to_screen(0)),
    Key([mod], "2",
        lazy.to_screen(1), lazy.group.to_screen(1)),

    # Print screen
    Key([mod], "F10", lazy.spawn("import -window root ~/screenshot.png")),

    # Full screen
    Key([mod], "F11",
        lazy.window.toggle_fullscreen()),
]

# This allows you to drag windows around with the mouse if you want.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
        start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
        start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

class GroupIndex(Group):
    def __init__(self, name="", code=""):
        self.code = code

        Group.__init__(self, name)

# Next, we specify group names, and use the group name list to generate an appropriate
# set of bindings for group switching.
groups = [
    GroupIndex("Shell", "s"),
    GroupIndex("Www", "w"),
    GroupIndex("Chat", "c"),
    GroupIndex("Mail", "m"),
    GroupIndex("Video", "v"),
    GroupIndex("MuZic", "z"),
    GroupIndex("Docs", "d"),
    GroupIndex("VirtUalBox", "u"),
    GroupIndex("Torrent", "t"),
]

for g in groups:
    keys.append(
        Key([mod], g.code, lazy.group[g.name].toscreen())
    )
    keys.append(
        Key([mod, alt], g.code, lazy.window.togroup(g.name))
    )

# Two basic layouts.
layouts = [
    layout.Max(),
    layout.Stack(stacks=2, border_width=2),
]

import subprocess, re

def is_running(process):
    s = subprocess.Popen(["ps", "axuw"], stdout=subprocess.PIPE)
    for x in s.stdout:
        if re.search(process, x):
            return True
    return False

def exec_once(process):
    if not is_running(process):
        return subprocess.Popen(process.split())

@hook.subscribe.startup
def startup():
    exec_once("terminator")
    exec_once("setxkbmap -option ctrl:nocaps")

# vim: tabstop=4 shiftwidth=4 expandtab
