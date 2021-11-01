Config { font    = "xft:Iosevka:weight=bold:pixelsize=12:antialias=true:hinting=true,fontawesome:pixelsize=12"
       , bgColor = "#323d43"
       , fgColor = "#4b565c"
       -- , position = BottomSize C 99 20
       , position = Static { xpos = 10, ypos = 1050, width = 1900, height = 20}
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "~/.config/nixpkgs/configs/xmonad/"
       , commands = [
                    -- Time and date
           Run Date "<fc=#a7c080><fn=5> </fn> %b %d %Y</fc> <fc=#323d43> | |</fc> <fc=#e67e80><fn=5></fn> %H:%M </fc>" "date" 50

                    -- UnsafeStdinReader
           , Run UnsafeStdinReader

           -- Battery
           , Run Battery        [ "--template" , "<acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#e67e80"
                             , "--normal"   , "#d699b6"
                             , "--high"     , "#d699b6"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "  <left>% - <timeleft>"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#d699b6>  <left>% </fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#d699b6>  <left>%</fc>"
                             ] 50
           
             -- Weather https://skyvector.com/airport/EVRA/Riga-Airport
           , Run WeatherX "SBBH"
                             [ ("clear", "")
                             , ("sunny", "")
                             , ("mostly clear", "")
                             , ("mostly sunny", "")
                             , ("partly sunny", "")
                             , ("fair", "")
                             , ("cloudy","")
                             , ("overcast","")
                             , ("partly cloudy", "")
                             , ("mostly cloudy", "")
                             , ("considerable cloudiness", "")]
                             ["-t", "<fn=2><skyConditionS> </fn> <tempC> °C" --  <rh>%  <windKmh> (<hour>)
                             -- , "-L","10", "-H", "25", "--normal", "black"
                             -- , "--high", "lightgoldenrod4", "--low", "darkseagreen4"
                             ] 18000
                    ]

       , sepChar = "%"
       , alignSep = "}{"
       , template = "<action=`rofi -show drun`> <icon=haskell.xpm/> </action> %UnsafeStdinReader% } \
       \{<fc=#323d43> | </fc>  <fc=#7fbbb3> %SBBH% </fc> \
       \<fc=#323d43> | </fc>  <fc=#d699b6> %battery% </fc> \
       \<fc=#323d43> | </fc>  %date% <fc=#323d43> | | </fc>"
       }
