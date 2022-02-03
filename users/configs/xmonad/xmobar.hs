Config { font    = "xft:JetBrainsMonoNerdFont:weight=bold:pixelsize=12:antialias=true:hinting=true"
       , bgColor = "#282c34"
       , fgColor = "#5c6370"
       -- , position = topSize C 99 20
       , position = Static { xpos = 10, ypos = 10, width = 1900, height = 20}
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/basqs/.xmonad"
       , commands = [
                    -- Time and date
           Run Date "<fc=#56b6c2><fn=5> </fn> %b %d %Y</fc> <fc=#282c34> | |</fc> <fc=#e06c75><fn=5></fn> %H:%M </fc>" "date" 50

                    -- UnsafeStdinReader
           , Run UnsafeStdinReader

           -- Battery
           , Run Battery        [ "--template" , "<acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#e06c75"
                             , "--normal"   , "#c678dd"
                             , "--high"     , "#c678dd"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "  <left>% - <timeleft>"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#c678dd>  <left>% </fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#c678dd>  <left>%</fc>"
                             ] 50
                    ]

       , sepChar = "%"
       , alignSep = "}{"
       , template = " <fc=#282c34> | </fc> %UnsafeStdinReader% } \
       \{<fc=#282c34> | </fc>  <fc=#c678dd> %battery% </fc> \
       \<fc=#282c34> | </fc>  %date% <fc=#282c34> | </fc>"
       }
