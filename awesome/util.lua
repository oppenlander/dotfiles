-- util functions

local awful = require("awful")


util = {}

util.run_once = function(prg, arg_string, pname, screen)
   if not prg then
      do return nil end
   end

   if not pname then
      pname = prg
   end

   if not arg_string then
      awful.util.spawn_with_shell("pgrep -u $USER -x '" .. pname .. "' ; or " .. prg .. "",screen)
   else
      awful.util.spawn_with_shell("pgrep -u $USER -x '" .. pname .. " ".. arg_string .."' ; or " .. prg .. " " .. arg_string,screen)
   end
end

return util
