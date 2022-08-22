function basename(str)
  local name = string.gsub(str, "(.*/)(.*)", "%2")
  return name
end

function chatterino()
  local path = mp.get_property("path")
  local title = mp.get_property("media-title")
  if string.match(path, "^https?://twitch.tv") then
    os.execute("chatterino -c "..basename(path))
  elseif string.match(title, "^https?://twitch.tv") then
    os.execute("chatterino -c "..basename(title))
  else
    return mp.osd_message("not a twitch stream")
  end
  mp.osd_message("chatterino opened")
end
mp.add_key_binding("alt+c", "chatterino", chatterino)
