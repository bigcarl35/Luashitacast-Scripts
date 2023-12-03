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
local JobBar = T{['GSwap'] = {'ALL','MS'},
				 ['DT'] = {'ALL','MS'},
				 ['Kite'] = {'ALL','MS'},
				 ['Acc'] = {'ALL','MS'},
				 ['Eva'] = {'ALL','MS'},
				 ['WSwap'] = {'-SMN,BLM','M'},		-- Some jobs swap weapons all the time
				 ['TH'] = {'ALL','MS'},
				 ['AJug'] = {'BST','M'},			-- BST field, only valid if BST is main job
				 ['DT_Type'] = {'ALL','MS'},
				 ['Region'] = {'ALL','MS'},
				 ['Enmity'] = {'ALL','MS'}, 
				 ['sBP'] = {'SMN','MS'}};			-- DRK field

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
	ShowHelp Displays help for all the commands across jobs
--]]
function gcdisplay.ShowHelp(args)
	
	if #args == 1 then
		print(chat.header('Help'):append(chat.message('The following commands are available to use from within Luashitacast. These are targetting either your specific job or are available across all jobs.\n')));
		print(chat.header('Help'):append(chat.message('Commands for all jobs: ')));
		print(chat.header('Help'):append(chat.message('/gswap --Toggles whether automatic gear swaps occur or not. Default is TRUE.')));
		print(chat.header('Help'):append(chat.message('/wsdistance [#] --Toggles whether a distance check is done for non-ranged weaponskills and how far. Default TRUE at ' .. tostring(gcinclude.settings.WSdistance) .. ' yalms.')));
		print(chat.header('Help'):append(chat.message('/dt --Indicates if a damage taken set should be used')));
		print(chat.header('Help'):append(chat.message('/dt_type [P|M|B] --Determines the type of damage taken set to use. Physical is assumed.')));
		print(chat.header('Help'):append(chat.message('/kite --Equips defined movement set.')));
		print(chat.header('Help'):append(chat.message('/wswap --Toggles whether weapons will be swapped as needed. Default is FALSE to preserve TP.')));
		print(chat.header('Help'):append(chat.message('/eva --Toggles whether evasion set should be equipped or not. Default is FALSE.')));
		print(chat.header('Help'):append(chat.message('/acc --Toggle whether accuracy gear should override melee/weapon skill gear. Default is FALSE')));
		print(chat.header('Help'):append(chat.message('/gearset name [on]--Will equip the named gear set and then disable GSwap unless optional parameter set to ON')));
		print(chat.header('Help'):append(chat.message('/craftset [AL|BN|CL|CO|GS|LT|SM|WW] --Equips the specified crafting gear and turns GSwap off.')));
		print(chat.header('Help'):append(chat.message('/gatherset [HELM|DIG|CLAM] --Equips the specified gathering gear and turns GSwap off.')));
		print(chat.header('Help'):append(chat.message('/fishset --Equips the fishing set and turns off GSwap.')));
		print(chat.header('Help'):append(chat.message('/region --Toggles whether the area you\'re adventuring in is controlled by your nation or not.')));
		print(chat.header('Help'):append(chat.message('/maxspell name -- Determines the highest level spell your current jobs can cast that has the passed name')));
		print(chat.header('Help'):append(chat.message('/maxsong name [back] -- Determines the highest level song your current jobs can cast that has the passed name or next to highest')));
		print(chat.header('Help'):append(chat.message('/th --Toggles on whether treasure hunter gear should be equipped. Default is FALSE.')));
		print(chat.header('Help'):append(chat.message('/enmity [OFF|PLUS|MINUS] --Determines if/type of enmity gear to equip. Default is OFF')));
		print(chat.header('Help'):append(chat.message('/equipit code|name [slot] [1|2] --Equips specified item in the specified slot and turns off /gswap.')));
		print(chat.header('Help'):append(chat.message('/dowep code|name --Equips specified weapon and turns off /gswap.')));
		print(chat.header('Help'):append(chat.message('/help [command] --Display this listing or specific details on the specified command.')));
		if Main == 'BST' then
			print(chat.header('Help'):append(chat.message(' ')));
			print(chat.header('Help'):append(chat.message('Command(s) specific for BST/ or /BST:')));
			print(chat.header('Help'):append(chat.message('/ajug -- Toggles whether the automated jug pet system is enabled. Default is TRUE. (BST/ only)'))); 
			print(chat.header('Help'):append(chat.message('/petfood [name|ALL|MAX|MIN] --Equips the specified pet food or determines best food and equips it.')));
		end
		if Main == 'SMN' or Sub == 'SMN' then
			print(chat.header('Help'):append(chat.message(' ')));
			print(chat.header('Help'):append(chat.message('Command(s) specific for SMN/ or /SMN:')));
			print(chat.header('Help'):append(chat.message('/sbp -- Toggles whether offensive blood pacts will show a message in party chat. Default is True.')));
		end
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Some /lac commands of note:')));
		print(chat.header('Help'):append(chat.message('/lac disable --Disables all gear slots so that no automatic gear changes can occur.')));
		print(chat.header('Help'):append(chat.message('/lac enable --Enables all gear slots so automatic gear changes can occur.')));
		print(chat.header('Help'):append(chat.message('/lac load --Loads the Luashitacast BST definitions')));
		print(chat.header('Help'):append(chat.message('/lac unload --Unloads the Luashitacast BST definitions')));
		print(chat.header('Help'):append(chat.message('/lac reload --Unloads and reloads the Luashitacast BST definition')));
		print(chat.header('Help'):append(chat.message('/lac addset \"name\" --Saves the current equipped gear into Luashitacast\'s DRK definition file. Don\'t include the \"\'s.')));
		print(chat.header('Help'):append(chat.message('/lac list --Lists all the defined gear sets from your BST definition.')));
		print(chat.header('Help'):append(chat.message(' ')));
		print(chat.header('Help'):append(chat.message('Please note that if you use style lock, you will not see the gear changing, but it is changing')))
	else
		local cmd = string.lower(args[2]);
		
		if cmd == 'gswap' then
			print(chat.header('Help'):append(chat.message('/gswap --This toggles whether automatic gear swapping is enabled or not. Default is TRUE')));
		elseif cmd == 'wsdistance' then
			print(chat.header('Help'):append(chat.message('wsdistance # --Non-ranged weapon skills require you to be no more than a certain distance from your target. By default, the maximum distance a tarutaru can hit a target is ' .. tostring(gcinclude.settings.WSdistance) ..' yalms. If your character\'s race is larger, you might want to change the distance to a larger number. This check prevents TP lost when you\'re too far away.')));
		elseif cmd == 'dt' then
			print(chat.header('Help'):append(chat.message('/dt --This toggles whether the damage taken gear set takes priority over the current gear set. Default is FALSE')));
		elseif cmd == 'dt_type' then
			print(chat.header('Help'):append(chat.message('/dt_type indicates the type of damage taken gear to use. P for physical, M for magical, and B for breath. Default is Physical')));
		elseif cmd == 'kite' then
			print(chat.header('Help'):append(chat.message('/kite --This toggles whether movement gear should be equipped. Default is FALSE')));
		elseif cmd == 'wswap' then
			print(chat.header('Help'):append(chat.message('/wswap --Toggles whether weapon swapping is permissible. Weapon swapping causes the loss of tp, but there are advantages too. Default is FALSE')));
		elseif cmd == 'gearset' then
			print(chat.header('Help'):append(chat.message('/gearset name [on]--This forcibly loads the indicated gear set and turns off GSwap unless optional parameter is on. Then GSwap remains enabled.')));
		elseif cmd == 'acc' then
			print(chat.header('Help'):append(chat.message('/acc --This toggles whether accuracy gear takes priority over normal melee gear. Casting and ranged accuracy are handled automatically. If Acc is true, then the accuracy set will be loaded over the TP set and the appropriate weaponskill set. Default is FALSE.')));
		elseif cmd == 'eva' then
			print(chat.header('Help'):append(chat.message('/eva --This toggles whether evasion gear takes priority over normal melee gear. If Eva is true, then the evasion set will be loaded over the TP set and the appropriate weaponskill set. Default is FALSE')));
		elseif cmd == 'craftset' then
			print(chat.header('Help'):append(chat.message('/craftset [AL,BN,CL,CO,GS,LT,SM,WW] equips specified crafting gear and turns GSwap off.')));
			print(chat.header('Help'):append(chat.message('AL - Alchemy, BN - Bonecraft, CL - Clothcraft, CO - Cooking, GS - Goldsmithing, LT - Leathercraft, SM - Smithing, and WW - Woodworking')));			
		elseif cmd == 'gatherset' then
			print(chat.header('Help'):append(chat.message('/gatherset [HELM,DIG,CLAM] --This equips specified gathering gear and turns GSwap off.')));
			print(chat.header('Help'):append(chat.message('HELM - harvest,excavation,logging,mining, DIG - digging, and CLAM - clamming')));
		elseif cmd == 'region' then
			print(chat.header('Help'):append(chat.message('/region --This indicates if the current area where you\'re playing is controlled by your nation. Default is TRUE')));
		elseif cmd == 'fishset' then
			print(chat.header('Help'):append(chat.message('/fishset --This command loads up your fishing gear and turns off GSwap.')));
		elseif cmd == 'maxspell' then
			print(chat.header('Help'):append(chat.message('/maxspell name --This determines the highest level spell that matches the name you indicated that your current job can cast.')));
		elseif cmd == 'maxsong' then
			print(chat.header('Help'):append(chat.message('/maxsong name [back] --This determines the highest level song that matches the name you indicated to cast or one of the max if asked for.')));			
		elseif cmd == 'th' then
			print(chat.header('Help'):append(chat.message('/th --Toggles whether TH gear should be equipped or not. Default is FALSE.')));
		elseif cmd == 'emmity' then
			print(chat.header('Help'):append(chat.message('/enmity [OFF|PLUS|MINUS] --Lets you enable whether enmity gear should be equipped and type. Default is OFF.')));			
		elseif cmd == 'equipit' then
			print(chat.header('Help'):append(chat.message('/equipit code|name [slot] [1|2] --Equips specified item (either coded or full name) in the specified slot and turns off /GSWAP. Note that if the item contains a space in the name, you have to surround the name with double quotes.')));
		elseif cmd == 'help' then
			print(chat.header('Help'):append(chat.message('/help [[all]|command] --This command displays help for all Luashitacast commands or the specified command.')));
		elseif cmd == 'petfood' then
			print(chat.header('Help'):append(chat.message('/petfood [alpha|beta|gamma|delta|epsilon|zeta|ALL|MIN|MAX] --This command either equips the specified pet food in the ammo slot or lists all the pet food that\'s accessible or determines the best or the weakest pet food that can be equipped.')));
		elseif cmd == 'ajug' then
			print(chat.header('Help'):append(chat.message('/ajug --Toggles whether the automated jug pet system is enabled. This loads a jug pet if the ammo slot doesn\'t have a jug pet in it. Default is TRUE')));
		elseif cmd == 'sbp' then
			print(chat.header('Help'):append(chat.message('/sbp --Toggles whether a party chat message should be displayed when an offensive blood pact is executed. Default is TRUE')));
		elseif cmd == 'lac' then
			print(chat.header('Help'):append(chat.message('/lac action ... --This command is native to Luashitacast and requires an action (ex: load, unload, list, etc.) and possibly further arguments. Further details are beyond what this help section can cover.')));
		else
			print(chat.header('Help'):append(chat.message('The command you specified either does not exist or is not supported for BST.')));
		end
	end
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