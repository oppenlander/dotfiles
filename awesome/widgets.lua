-- Widgets
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local lain = require("lain")

local markup = lain.util.markup

widgets = {}

-- {{{ Statusbar Widget
-- Textclock
widgets.clockicon = wibox.widget.imagebox(beautiful.widget_clock)
widgets.mytextclock = awful.widget.textclock(markup("#7788af", "%A %d %B ") .. markup("#343639", ">") .. markup("#de5e1e", " %I:%M %p "))

-- Calendar
--lain.widgets.calendar:attach(widgets.mytextclock, { font_size = 10 })

-- Weather
-- widgets.weathericon = wibox.widget.imagebox(beautiful.widget_weather)
-- widgets.yawn = lain.widgets.yawn(123456, {
--                             settings = function()
--                                widget:set_markup(markup("#eca4c4", forecast:lower() .. " @ " .. units .. "°C "))
--                             end
-- })

-- / fs
widgets.fsicon = wibox.widget.imagebox(beautiful.widget_fs)
widgets.fswidget = lain.widgets.fs({
      settings  = function()


      end
})

-- CPU
widgets.cpuicon = wibox.widget.imagebox()
widgets.cpuicon:set_image(beautiful.widget_cpu)
widgets.cpuwidget = lain.widgets.cpu({
    timeout = 2,
    settings = function()
        widget:set_markup(markup("#e33a6e", cpu_now.usage .. "% "))
    end
})

-- Battery
-- widgets.baticon = wibox.widget.imagebox(beautiful.widget_bat)
-- widgets.batwidget = lain.widgets.bat({
--     battery = "BAT1",
--     timeout = 3,
--     settings = function()
--        local arrow
--        if bat_now.status == "Discharging" then
--           arrow = ""
--        else
--           arrow = ""
--        end

--        if bat_now.perc == "N/A" then
--           bat_now.perc = "AC "
--        else
--           bat_now.perc = arrow .. " ".. bat_now.perc .. "% "
--        end
--        widget:set_text(bat_now.perc)
--     end
-- })

-- ALSA volume
-- widgets.volicon = wibox.widget.imagebox(beautiful.widget_vol)
-- widgets.volumewidget = lain.widgets.alsa({
--     settings = function()
--         if volume_now.status == "off" then
--             volume_now.level = volume_now.level .. "M"
--         end
--         widget:set_markup(markup("#7493d2", volume_now.level .. "% "))
--     end
-- })

-- Net
widgets.netdownicon = wibox.widget.imagebox(beautiful.widget_netdown)
--netdownicon.align = "middle"
widgets.netdowninfo = wibox.widget.textbox()
widgets.netupicon = wibox.widget.imagebox(beautiful.widget_netup)
--netupicon.align = "middle"
widgets.netupinfo = lain.widgets.net({
    settings = function()
        widget:set_markup(markup("#e54c62", net_now.sent .. " "))
        widgets.netdowninfo:set_markup(markup("#87af5f", net_now.received .. " "))
    end
})

-- MEM
widgets.memicon = wibox.widget.imagebox(beautiful.widget_mem)
widgets.memwidget = lain.widgets.mem({
    settings = function()
       mem_percent = math.floor((mem_now.used / mem_now.total * 100) + .5)
       widget:set_markup(markup("#e0da37", mem_percent .. "% "))
    end
})

-- MPD
-- widgets.mpdicon = wibox.widget.imagebox()
-- widgets.mpdwidget = lain.widgets.mpd({
--     settings = function()
--         mpd_notification_preset = {
--             text = string.format("%s [%s] - %s\n%s", mpd_now.artist,
--                    mpd_now.album, mpd_now.date, mpd_now.title)
--         }

--         if mpd_now.state == "play" then
--             artist = mpd_now.artist .. " > "
--             title  = mpd_now.title .. " "
--             widgets.mpdicon:set_image(beautiful.widget_note_on)
--         elseif mpd_now.state == "pause" then
--             artist = "mpd "
--             title  = "paused "
--         else
--             artist = ""
--             title  = ""
--             widgets.mpdicon:set_image(nil)
--         end
--         widget:set_markup(markup("#e54c62", artist) .. markup("#b2b2b2", title))
--     end
-- })
-- }}}

return widgets
