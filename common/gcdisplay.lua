local gcdisplay = {};
--[[
	This module predominantly is for routines dealing with the display bar, but is also sets
	up toggles and cycles. (It's still predominanytly what GetAwayCoxn wrote, but I will try
	to comment it, for clarity.
--]]

local fonts = require('fonts');
local Toggles = {};
local Cycles = {};
local Def = 0;
local Attk = 0;
local MainLV = 0;
local SubLV = 0;
local Main = 'FOO';
local Sub = 'BAR';
local JobSpecific = {			-- Need to update to support job specific toggles/cycles
	['BST'] = 'ajug',
};

local fontSettings = T{
	visible = true,
	font_family = 'Arial',
	font_height = 12,
	color = 0xFFFFFFFF,
	position_x = 300,
	position_y = 0,
	background = T{
		visible = true,
		color = 0xFF000000,
	}
};

--[[
	AdvanceCycle moves the pointer in the Cycle to the next available value. If at the last value,
	it cycles back to the beginning of the list
--]]

function gcdisplay.AdvanceCycle(name)
	local ctable = Cycles[name];
	if (type(ctable) ~= 'table') then
		return;
	end
	
	ctable.Index = ctable.Index + 1;
	if (ctable.Index > #ctable.Array) then
		ctable.Index = 1;
	end
end

--[[
	SetCycle explicitly sets which value should be current in the cycle variable
--]]

function gcdisplay.SetCycle(name,val)
	local ctable = Cycles[name];
	if (type(ctable) ~= 'table') then
		return;
	end
	
	for k,v in pairs(ctable.Array) do
		if val == v then
			ctable.Index = k
			return true
		end
	end
	return false
end

--[[
	SetToggle explicitly sets the value of the passed binary variable
--]]

function gcdisplay.SetToggle(name,val)
	if (type(Toggles[name]) ~= 'boolean' or type(val) ~= 'boolean') then
		return;
	else
		Toggles[name] = val;
	end
end

--[[
	AdvanceToggle just flips the binary setting of the passed toggle variable
--]]

function gcdisplay.AdvanceToggle(name)
	if (type(Toggles[name]) ~= 'boolean') then
		return;
	elseif Toggles[name] then
		Toggles[name] = false;
	else
		Toggles[name] = true;
	end
end

--[[
	Update updates the current identifying aspects of the display bar data (specifically the
	player specific information)
--]]

function gcdisplay.Update()
	local player = AshitaCore:GetMemoryManager():GetPlayer();
	
	local MID = player:GetMainJob();
	local SID = player:GetSubJob();
	Def = player:GetDefense();
	Attk = player:GetAttack();
	MainLV =player:GetMainJobLevel();
	SubLV = player:GetSubJobLevel();
	Main = AshitaCore:GetResourceManager():GetString("jobs.names_abbr", MID);
	Sub = AshitaCore:GetResourceManager():GetString("jobs.names_abbr", SID);
	
end

--[[
	CreateToggle creates a binary variable that can be turrned on or off
--]]

function gcdisplay.CreateToggle(name, default)
	Toggles[name] = default;
end

--[[
	GetToggle returns the name of the current setting of the passed toggle variable
--]]

function gcdisplay.GetToggle(name)
	if (Toggles[name] ~= nil) then
		return Toggles[name];
	else
		return false;
	end
end

--[[
	CreateCycle creates a table variable with multiple defined values. The index identifies which value
	is currently selected
--]]

function gcdisplay.CreateCycle(name, values)
	local newCycle = {
		Index = 1,
		Array = values
	};
	Cycles[name] = newCycle;
end

--[[
		GetCycle returns the currently selected value of the cycle
--]]

function gcdisplay.GetCycle(name)
	local ctable = Cycles[name];
	if (type(ctable) == 'table') then
		return ctable.Array[ctable.Index];
	else
		return 'Unknown';
	end
end

--[[
	Unload removes the objects and commands created by the gcdisplay code
--]]

function gcdisplay.Unload()
	if (gcdisplay.FontObject ~= nil) then
		gcdisplay.FontObject:destroy();
	end
	ashita.events.unregister('d3d_present', 'gcdisplay_present_cb');
	ashita.events.unregister('command', 'gcdisplay_cb');
end

--[[
	bDisplayIt is a function that determines if the passed string should be displayed in the luashita
	display bar. (Some settings are job specific and should only be displayed if the main job is
	associated with the current main job.)
--]]

function gcdisplay.bDisplayIt(s)
	local ss = string.lower(s);
	
	for k,v in pairs(JobSpecific) do
		if string.find(string.lower(v),ss) ~= nil and k ~= Main then
			return false;
		end
	end
	return true;
end

--[[
	Initialize creates the display bar
--]]

function gcdisplay.Initialize()
	gcdisplay.Update();
	gcdisplay.FontObject = fonts.new(fontSettings);	
	ashita.events.register('d3d_present', 'gcdisplay_present_cb', function ()
		local display = MainLV .. Main .. '/' .. SubLV .. Sub ..'   Attk:' .. Attk .. '   Def:' .. Def;
		for k, v in pairs(Toggles) do
			if gcdisplay.bDisplayIt(k) == true then
				display = display .. '   ';
				if (v == true) then
					display = display .. '|cFF00FF00|' .. k .. '|r';
				else
					display = display .. '|cFFFF0000|' .. k .. '|r';
				end
			end
		end
		for key, value in pairs(Cycles) do
			if gcdisplay.bDisplayIt(key) == true then
				display = display .. '  ' .. key .. ': ' .. '|cFF00FF00|' .. value.Array[value.Index] .. '|r';
			end
		end
		gcdisplay.FontObject.text = display;
	end);
end

--[[
		gcdisplay_cb registers the command so that the display bar can be turned on or off
--]]

ashita.events.register('command', 'gcdisplay_cb', function (e)
	local args = e.command:args()
    if #args == 0 or args[1] ~= '/gcdisplay' then
        return
    end

    e.blocked = true

    if #args == 1 then
        gcdisplay.FontObject.visible = not gcdisplay.FontObject.visible;
    end
end)

return gcdisplay;