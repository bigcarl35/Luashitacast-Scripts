local gcdisplay = {};
--[[
	This module predominantly is for routines dealing with the display bar, but is also sets
	up toggles and cycles. (It's still predominanytly what GetAwayCoxn wrote, but I will try
	to comment it, for clarity.)
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
local Locks = 'None';
local AccType = 'F';
local AccTier = 'None';
local JobBar = T{['GSwap'] = {'ALL','MS'},
				 ['DT'] = {'ALL','MS'},
				 ['Kite'] = {'ALL','MS'},
				 ['Eva'] = {'ALL','MS'},
				 ['Tank'] = {'PLD,NIN,RUN,WAR,DRK,THF,RDM','M'},
				 ['Idle'] = {'PLD,NIN,RUN,WAR,DRK,THF,RDM','M'},
				 ['WSwap'] = {'-SMN,BLM','M'},		-- Some jobs swap weapons all the time
				 ['TH'] = {'THF','M'},
				 ['AJug'] = {'BST','M'},
				 ['DB'] = {'BST','M'},
				 ['Region'] = {'ALL','MS'},
				 ['Instrument'] = {'BRD','M'},
				 ['sBP'] = {'SMN','MS'}};

local fontSettings = T{
	visible = true,
	font_family = 'Arial',
	font_height = 14,
	color = 0xFFFFFFFF,			-- White
	position_x = 275, --300,
	position_y = 0,
	background = T{
		visible = true,
		color = 0xFF000000,		-- Black
	}
};
	
--[[
	ShowHelp Displays help for all the commands across jobs
--]]
function gcdisplay.ShowHelp()
	print(chat.header('Help'):append(chat.message('The following commands are available to use from within Luashitacast. These are targetting either your specific job or are available across all jobs.\n')));
	print(chat.header('Help'):append(chat.message('Commands for all jobs: ')));
	print(chat.header('Help'):append(chat.message('/gswap -- Toggles whether automatic gear swaps occur or not. Default is TRUE.')));
	print(chat.header('Help'):append(chat.message('/wsdistance [#] -- Toggles whether a distance check is done for non-ranged weaponskills and how far. Default TRUE at ' .. tostring(gcinclude.settings.WSdistance) .. ' yalms.')));
	print(chat.header('Help'):append(chat.message('/dt -- Indicates type of damage taken set should be used: Physical is assumed.')));
	print(chat.header('Help'):append(chat.message('/kite -- Equips defined movement set.')));
	print(chat.header('Help'):append(chat.message('/wswap -- Toggles whether weapons will be swapped as needed. Default is FALSE to preserve TP.')));
	print(chat.header('Help'):append(chat.message('/eva -- Toggles whether evasion set should be equipped or not. Default is FALSE.')));
	print(chat.header('Help'):append(chat.message('/acc -- Toggle whether accuracy gear should override melee/weapon skill gear. Default is FALSE')));
	print(chat.header('Help'):append(chat.message('/gearset|gs name [on]-- Will equip the named gear set and then disable GSwap unless optional parameter set to ON')));
	print(chat.header('Help'):append(chat.message('          [ALC|BON|CTH|COOK|GSM|LTH|SMT|WW] -- Equips the specified crafting gear.')));
	print(chat.header('Help'):append(chat.message('          [HELM|DIG|CLAM|FISH] -- Equips the specified gathering gear.')));
	print(chat.header('Help'):append(chat.message('/lock [all|#\'s|names] -- Locks specified equipment slots disabling luashitacast from changing gear in those slots')));
	print(chat.header('Help'):append(chat.message('/unlock [all|#\'s|names] -- Unlocks specified equipment slots enabling luashitacast on those slots')));
	print(chat.header('Help'):append(chat.message('/maxspell name -- Determines the highest level spell your current jobs can cast that has the passed name')));
	print(chat.header('Help'):append(chat.message('/maxsong name [back] -- Determines the highest level song your current jobs can cast that has the passed name or next to highest')));
	print(chat.header('Help'):append(chat.message('/equipit code|name [slot] [1|2] --Equips specified item in the specified slot and turns off /gswap.')));
	print(chat.header('Help'):append(chat.message('/rc -- Displays who controls what region')));
	print(chat.header('Help'):append(chat.message('/petfood name --Equips the specified pet food.')));

	if string.find('SMN,BLM',Main) == nil then
		print(chat.header('Help'):append(chat.message('/wswap -- Toggles whether automatic weapon swapping is permitted. Default is FALSE.')));
	end

	if string.find('PLD,NIN,DRK,WAR,THF,RDM,RUN',Main) ~= nil then
		print(chat.header('Help'):append(chat.message('/tank -- Toggles whether tanking TP gear set should be equipped. Default is TRUE for PLD,NIN,RUN and FALSE for DRK,WAR,THF,RDM.')));
		print(chat.header('Help'):append(chat.message('/idle -- Toggles whether \'Travel\' gear is equipped when idle. Default is TRUE.')));
	end
	
	if Main == 'BST' then
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Command(s) specific for BST/ or /BST:')));
		print(chat.header('Help'):append(chat.message('/ajug -- Toggles whether the automated jug pet system is enabled. Default is TRUE. (BST/ only)'))); 
		print(chat.header('Help'):append(chat.message('/db [Norm|BPP|WSS] --Indicates body piece wanted for for debuffing your pet.')));
	end
	
	if Main == 'THF' then
		print(chat.header('Help'):append(chat.message('/th -- Toggles whether treasure hunter gear should be equipped. Default is FALSE.')));
	end
	
	if Main == 'SMN' or Sub == 'SMN' then
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Command(s) specific for SMN/ or /SMN:')));
		print(chat.header('Help'):append(chat.message('/sbp -- Toggles whether offensive blood pacts will show a message in party chat. Default is True.')));
	end
	
	if Main == 'BRD' then
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Command(s) specific for BRD/:')));
		print(chat.header('Help'):append(chat.message('/horn -- Indicates that the instrument should be a wind instrument.')));	
		print(chat.header('Help'):append(chat.message('/string -- Indicates that the instrument should be a stringed instrument.')));			
	end
	
	print(chat.header('Help'):append(chat.message('/help [command] -- Display this listing or specific details on the specified command.')));	
	print(chat.header('Help'):append(chat.message(' ')));
	print(chat.header('Help'):append(chat.message('Some /lac commands of note:')));
	print(chat.header('Help'):append(chat.message('/lac disable -- Disables all gear slots so that no automatic gear changes can occur.')));
	print(chat.header('Help'):append(chat.message('/lac enable -- Enables all gear slots so automatic gear changes can occur.')));
	print(chat.header('Help'):append(chat.message('/lac load -- Loads the Luashitacast BST definitions')));
	print(chat.header('Help'):append(chat.message('/lac unload -- Unloads the Luashitacast BST definitions')));
	print(chat.header('Help'):append(chat.message('/lac reload -- Unloads and reloads the Luashitacast BST definition')));
	print(chat.header('Help'):append(chat.message('/lac addset \"name\" -- Saves the current equipped gear into Luashitacast\'s DRK definition file. Don\'t include the \"\'s.')));
	print(chat.header('Help'):append(chat.message('/lac list -- Lists all the defined gear sets from your BST definition.')));
	print(chat.header('Help'):append(chat.message(' ')));
	print(chat.header('Help'):append(chat.message('Please note that if you use style lock, you will not see the gear changing, but it is changing')))
end		-- gcdisplay.ShowHelp

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
end		-- gcdisplay.AdvanceCycle

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
end		-- gcdisplay.SetCycle

--[[
	SetToggle explicitly sets the value of the passed binary variable
--]]

function gcdisplay.SetToggle(name,val)
	if (type(Toggles[name]) ~= 'boolean' or type(val) ~= 'boolean') then
		return;
	else
		Toggles[name] = val;
	end
end		-- gcdisplay.SetToggle

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
end		-- gcdisplay.AdvanceToggle

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
end		-- gcdisplay.Update

--[[
	CreateToggle creates a binary variable that can be turrned on or off
--]]

function gcdisplay.CreateToggle(name, default)
	Toggles[name] = default;
end		-- gcdisplay.CreateToggle

--[[
	GetToggle returns the name of the current setting of the passed toggle variable
--]]

function gcdisplay.GetToggle(name)
	if (Toggles[name] ~= nil) then
		return Toggles[name];
	else
		return false;
	end
end		-- gcdisplay.GetToggle

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
end		-- gcdisplay.CreateCycle

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
end		-- gcdisplay.GetCycle

--[[
	SetSlots stores the list of locks or accuracy slots, for displaying
--]]

function gcdisplay.SetSlots(sTarget,sLList)
	if sTarget == 'locks' then
		Locks = sLList;
	else
		AccTier = sLList;
	end
end		-- gcdisplay.SetSlots

--[[
	Unload removes the objects and commands created by the gcdisplay code
--]]

function gcdisplay.Unload()
	if (gcdisplay.FontObject ~= nil) then
		gcdisplay.FontObject:destroy();
	end
	ashita.events.unregister('d3d_present', 'gcdisplay_present_cb');
	ashita.events.unregister('command', 'gcdisplay_cb');
end		-- gcdisplay.Unload

--[[
	bDisplayIt is a function that determines if the passed string should be displayed in the luashita
	display bar. 
--]]

function gcdisplay.bDisplayIt(s)

	if s == nil or JobBar[s][1] == nil or JobBar[s][1] == 'ALL' then	-- Missing from table or applies to all jobs, assume it should be displayed
		return true;
	else
		-- Something specific about the entry. Parse it out
		if string.sub(JobBar[s][1],1,1) == '-' then			-- Indicates ALL but the jobs mentioned
			if string.find(JobBar[s][2],'M') ~= nil and string.find(JobBar[s][1],Main) ~= nil then
				return false;
			end
			if string.find(JobBar[s][2],'S') ~= nil and string.find(JobBar[s][1],Sub) ~= nil then
				return false;
			end	
			return true;
		else	-- Only valid for the explicit jobs mentioned
			if string.find(JobBar[s][2],'M') ~= nil and string.find(JobBar[s][1],Main) ~= nil then
				return true;
			end
			if string.find(JobBar[s][2],'S') ~= nil and string.find(JobBar[s][1],Sub) ~= nil then
				return true;
			end
		end
	end
	return false;
end		-- gcdisplay.bDisplayIt

--[[
	fColor will return the colorized string according to the keyword's color. All
	colors are denoted in hex
--]]

function fColor(skw,sMsg)
	local tkwEle = {	-- cOORRGGBB where OO is opacity, RR red, GG green, BB blue
		{ ['kw'] = 'firesday',		 ['color'] = '|cFFFF0000|' }, -- red
		{ ['kw'] = 'earthsday',		 ['color'] = '|cFFC19A6B|' }, -- camel
		{ ['kw'] = 'watersday',		 ['color'] = '|cFF1F51FF|' }, -- neon blue
		{ ['kw'] = 'windsday',		 ['color'] = '|cFF4CBB17|' }, -- kelly green
		{ ['kw'] = 'iceday',		 ['color'] = '|cFF00FFFF|' }, -- aqua
		{ ['kw'] = 'lightningday',   ['color'] = '|cE1C16EFF|' }, -- "light purple"
		{ ['kw'] = 'lightsday', 	 ['color'] = '|cFFFFFFFF|' }, -- white
		{ ['kw'] = 'darksday', 		 ['color'] = '|cFF71797E|' }, -- steel gray
		{ ['kw'] = 'clear', 		 ['color'] = '|cFFA7C7E7|' }, -- pastel blue
		{ ['kw'] = 'sunshine', 		 ['color'] = '|cFFFFEA00|' }, -- bright yellow
		{ ['kw'] = 'clouds', 		 ['color'] = '|cFFFFFDD0|' }, -- cream
		{ ['kw'] = 'fog',	 		 ['color'] = '|cFFB2BEB5|' }, -- ash gray
		{ ['kw'] = 'fire', 			 ['color'] = '|cFFFF0000|' }, -- red
		{ ['kw'] = 'fire x2', 		 ['color'] = '|cFFFF0000|' }, -- red
		{ ['kw'] = 'water', 		 ['color'] = '|cFF1F51FF|' }, -- neon blue
		{ ['kw'] = 'water x2', 		 ['color'] = '|cFF1F51FF|' }, -- neon blue
		{ ['kw'] = 'earth', 		 ['color'] = '|cFFC19A6B|' }, -- camel
		{ ['kw'] = 'earth x2', 		 ['color'] = '|cFFC19A6B|' }, -- camel
		{ ['kw'] = 'wind',	 		 ['color'] = '|cFF4CBB17|' }, -- kelly green
		{ ['kw'] = 'wind x2', 		 ['color'] = '|cFF4CBB17|' }, -- kelly green
		{ ['kw'] = 'ice',	 		 ['color'] = '|cFF00FFFF|' }, -- aqua
		{ ['kw'] = 'ice x2', 		 ['color'] = '|cFF00FFFF|' }, -- aqua
		{ ['kw'] = 'thunder', 		 ['color'] = '|cE1C16EFF|' }, -- "light purple"
		{ ['kw'] = 'thunder x2',	 ['color'] = '|cE1C16EFF|' }, -- "light purple"
		{ ['kw'] = 'light', 		 ['color'] = '|cFFFFFFFF|' }, -- white
		{ ['kw'] = 'light x2', 		 ['color'] = '|cFFFFFFFF|' }, -- white
		{ ['kw'] = 'dark',	 		 ['color'] = '|cFF71797E|' }, -- steel gray
		{ ['kw'] = 'dark x2', 		 ['color'] = '|cFF71797E|' }, -- steel gray
		{ ['kw'] = 'full moon',		 ['color'] = '|cFFFFFFFF|' }, -- white
		{ ['kw'] = 'waning gibbous', ['color'] = '|cFFE5E4E2|' }, -- platinum
		{ ['kw'] = 'last quarter', 	 ['color'] = '|cFFC0C0C0|' }, -- silver
		{ ['kw'] = 'waning crescent',['color'] = '|cFF848884|' }, -- smoke
		{ ['kw'] = 'new moon', 		 ['color'] = '|cFF71797E|' }, -- steel gray
		{ ['kw'] = 'waxing crescent',['color'] = '|cFF848884|' }, -- smoke
		{ ['kw'] = 'first quarter',  ['color'] = '|cFFC0C0C0|' }, -- silver
		{ ['kw'] = 'waxing gibbous', ['color'] = '|cFFE5E4E2|' }, -- platinum
		{ ['kw'] = 'green',			 ['color'] = '|cFF00FF00|' }, -- green
		{ ['kw'] = 'red',			 ['color'] = '|cFFFF0000|' }, -- red
	};
	local sEnd = '|r';
	local sfColor;
	local sColor = nil;
	
	if skw == nil then
		return ' ';
	end
	
	skw = string.lower(skw);
	for i,j in pairs(tkwEle) do
		if j['kw'] == skw then
			sColor = j['color'];
			break;
		end
	end
	
	if sColor == nil then
		sfColor = sMsg;
	else
		sfColor = sColor .. sMsg .. sEnd;
	end
	return sfColor;
end		-- fColor

--[[
	Initialize creates the display bar
--]]

function gcdisplay.Initialize()
	gcdisplay.Update();
	gcdisplay.FontObject = fonts.new(fontSettings);	
	ashita.events.register('d3d_present', 'gcdisplay_present_cb', function ()
		local display = MainLV .. Main .. '/' .. SubLV .. Sub ..'   Attk:' .. Attk .. '   Def:' .. Def .. ' |';
		for k, v in pairs(Toggles) do
		
			if gcdisplay.bDisplayIt(k) == true then
				display = display .. '   ';
				if (v == true) then
					display = display .. fColor('green',k);
				else
					display = display .. fColor('red',k);
				end
			end
		end
		display = display .. ' |';
		for key, value in pairs(Cycles) do
			if gcdisplay.bDisplayIt(key) == true then
				display = display .. '  ' .. key .. ': ' .. fColor('green',value.Array[value.Index]);
			end
		end
		
		-- Accuracy slots
		if AccTier ~= 'None' then
			display = display .. ' | Acc: ' .. fColor('green',AccTier);
		else
			display = display .. ' | Acc: ' .. fColor('red',AccTier);
		end
		
		-- Locks
		if Locks ~= 'None' then
			display = display .. ' | Locks: ' .. fColor('green',Locks);
		else
			display = display .. ' | Locks: ' .. fColor('red',Locks);
		end

		local env = gData.GetEnvironment();
		display = display .. string.format(' | %s | %02d:%02d | %d%% %s | %s ',
			fColor(env.Day,env.Day),env.Timestamp.hour,env.Timestamp.minute,env.MoonPercent,fColor(env.MoonPhase,env.MoonPhase),fColor(env.RawWeather,env.RawWeather));
		--display = fColor('full moon') ..' | ' .. fColor('waning gibbous') ..' | ' .. fColor('last quarter') ..' | ' .. fColor('waning crescent') .. ' | ' .. fColor('new moon') .. ' | ' .. fColor('waxing crescent') .. ' | ' .. fColor('first quarter') .. ' | ' .. fColor('waxing gibbous') .. ' | ' .. fColor('full moon');
		gcdisplay.FontObject.text = display;
	end);
end		-- gcdisplay.Initialize

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