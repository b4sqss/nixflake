Config { font    = "xft:Iosevka:weight=bold:pixelsize=12:antialias=true:hinting=true,fontawesome:pixelsize=12"
       , borderColor = "#f0c674"
       , border = BottomB
       , bgColor = "#1d1f21"
       , fgColor = "#969896"
       -- , position = topSize C 99 20
       , position = Static { xpos = 10, ypos = 10, width = 1900, height = 20}
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/basqs/.config/nixpkgs/configs/xmonad"
       , commands = [
                    -- Time and date
           Run Date "<fc=#b5bd68><fn=5> </fn> %b %d %Y</fc> <fc=#1d1f21> | |</fc> <fc=#cc6666><fn=5></fn> %H:%M </fc>" "date" 50

                    -- UnsafeStdinReader
           , Run UnsafeStdinReader

           -- Battery
           , Run Battery        [ "--template" , "<acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#cc6666"
                             , "--normal"   , "#b294bb"
                             , "--high"     , "#b294bb"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "  <left>% - <timeleft>"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#b294bb>  <left>% </fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#b294bb>  <left>%</fc>"
                             ] 50
                    ]

       , sepChar = "%"
       , alignSep = "}{"
       , template = "<action=`rofi -show drun`> <icon=nixos.xpm/> </action> %UnsafeStdinReader% } \
       \{<fc=#1d1f21> | </fc>  <fc=#b294bb> %battery% </fc> \
       \<fc=#1d1f21> | </fc>  %date% <fc=#1d1f21> | | </fc>"
       }
