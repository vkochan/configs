-- load standard vis module, providing parts of the Lua API
local plugins = {
	['https://repo.or.cz/vis-goto-file.git'] = 'init',
}

require('vis')
require('plugins/vis-plug').init(plugins, true)

vis.events.subscribe(vis.events.INIT, function()
	-- Your global configuration options
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Your per window configuration options e.g.
	vis:command('set number')
end)
