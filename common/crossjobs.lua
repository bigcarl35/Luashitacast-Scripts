local crossjobs = {};

--[[
	This component contains all functions that are used across all jobs. Further, it contains
	gear sets that are job independent.

	List of routines-
		Subroutines:
			HandlleAbility				Coordinate all abilities gear sets equipping
			HandleCommands				Process all luashitacast commands that are not job specific
			HandleItem					Coordinate all item gear equipping
			HandleMidshot				Coordinate all Mids-hot gear equipping
			HandlePreshot				Coordinate all Pre-Shot gear equipping
			Weaponskill					Coordinate all Weapon Skill gear equipping
			packet_in_callback1			Dissects packed for region control info
			ProcessAccuracy				Process the /acc or /racc commands
			ProcessCap					Enables/disables an artificial level cap
			ProcessCC					Displays custom conditionals or enable/disables the cc
			ProcessDB					Sets the debuff indicator to the appropriate type, BST only
			ProcessDBar					Processes the passed /dbar command
			ProcessDT					Sequences or assigns directly the damage taken gear type
			ProcessHornString			Determines the type of bard instrument should be default
			ProcessMACC					Enables/disables magic accuracy gear when casting magic
			ProcessMode					Toggles between perpetuation cost and attack power, SMN only
			ProcessSW					Equips the starting weapons
			ProcessToggle				Toggles the specified toggle
			ProcessWSDISTANCE			Changes WSDISTANCE or turns it on/off
			ProcessWSWAP				Enables/Disables whether weapon swapping permitted
			ProgressiveAccuracy			Equips the appropriate accuracy stage
			SetVariables				Sets runtime displaybar variables
			t1							Test procedure for trying out new ideas

		Functions:
			fHandleWeaponskill			Determines what gear set to equip and does so
			fValidCustomCommand			Determines if passed command is a custom conditional command
--]]

crossjobs.sets = {

--[[
	There are currently eight crafts: alchemy, bonecraft, clothcraft, cooking, goldsmithing, leathercraft,
	smithing, and woodworking. It's possible that a player will have gear for more than one craft. Each
	craft is identified by an acronym: ALC, BONE, CLOTH, COOK, GSM, LTH, BSM, and WW. Unlike previous
	versions of luashitacast, this version has explicit sets for each of these crafting types. That way
	if you have multiple crafting gear, you have sets definied for whichever type of craft you have gear
	for.
--]]

	['ALC'] = {		-- Alchemy
	},

	['BONE'] = {	-- Bone Crafting
	},

	['CLOTH'] = {	-- Clothcraft
	},

	['COOK'] = {	-- Cooking
	},

	['GSM'] = {		-- Goldsmithing
	},

	['LTH'] = {		-- Leatherworking
	},

	['BSM'] = {		-- Blacksmithing
	},

	['WW'] = {		-- Woodworking
		Hands = 'Carpenter\'s gloves',
	},

--[[
	There are four gathering types: harvesting, excavtion, logging, and mining which are grouped in the H.E.L.M.
	set. The other three types of gathering: digging, clamming and fishing, have their own gear. Unlike previous
	versions of luashitacast, this version has explicit sets for each of these gathering types.
--]]

	['FISH'] = {	-- Fishing
		Range = 'Lu Shang\'s F. Rod',
		Ammo  = 'Sinking Minnow',
		Neck  = 'Justice Badge',	-- Included to block out Default's neck definition
		Body  = 'Angler\'s Tunica',
		Rings = 'Albatross Ring',
		Legs  = 'Fisherman\'s Hose',
		Feet  = 'Waders'
	},

	['HELM'] = {	-- Harvesting, Excavation, Logging, Mining
		Body  = 'Field Tunica',
		Hands = 'Field Gloves',
		Legs  = 'Field Hose',
		Feet  = 'Field Boots'
	},

	['DIG'] = {		-- Digging
		Head = 'Egg Helm//CC2',
		Body = 'Choc. Jack Coat'
	},
	['CLAM']  = {	-- Clamming
		Body = 'Tarutaru Top +1',
		Legs = 'Taru. Shorts +1'
	},

--[[
	The Riding set is used to extend the duration of a chocobo ride. You equip it prior
	to mounting a chocobo (or one of the other mounts if they're ever added.)
--]]

	['Riding'] = {
		Body = 'Choc. Jack Coat',
	}

--[[
	The Sneaky set is equipped and the slots are locked. It's a set intended to equip gear
	to help the player sneak around.
--]]

	['Sneaky'] = {
		Hands = 'Dream Mittens +1',
		Feet  = 'Dream Boots +1',
	},

--[[
	The dispense set is used to equip items that have an ability to daily dispense items.
	They're grouped here as a convenience. Note that since Sub is specified, something has
	to be specified in Main. If the Job file sees an empty Main, it assigns a default
	weapon. This was needed to get around a strange bug that sometimes occurred when
	entering a level capped zone.
--]]

	['Dispense'] = {
		Head = 'Dream Hat +1',
		Main = 'Dream Bell',
		Sub  = 'Hatchling Shield',
	},

--[[
	The following set is used to dynamically create a gear set to be displayed once rather
	than in a piecemeal manner. This set is to be left empty by the player  Please do not
	modify it.
--]]

	['CurrentGear'] = { },
};

-- Indicates what nation your character is from, -1 is unassigned
crossjobs.OwnNation = -1;
crossjobs.Sets = crossjobs.sets;
crossjobs.WeaponTypes = {};
crossjobs.ZoneList = {};
crossjobs.CurrentZone = 0;

--[[
	The following event is used to capture the ownership of the regions.
	Conquest updates are sent whenever the player zones and periodically.
	The display bar's region is updated accordingly
--]]

ashita.events.register('packet_in', 'packet_in_callback1', function (e)

	if (e.id == 0x05E) then
		gVars.RegionControl['Ronfaure']['own'] 			= struct.unpack('B', e.data, 0X1E)
		gVars.RegionControl['Zulkheim']['own'] 			= struct.unpack('B', e.data, 0x22)
		gVars.RegionControl['Norvallen']['own'] 		= struct.unpack('B', e.data, 0x26)
		gVars.RegionControl['Gustaberg']['own'] 		= struct.unpack('B', e.data, 0x2A)
		gVars.RegionControl['Derfland']['own'] 			= struct.unpack('B', e.data, 0x2E)
		gVars.RegionControl['Sarutabaruta']['own'] 		= struct.unpack('B', e.data, 0x32)
		gVars.RegionControl['Kolshushu']['own'] 		= struct.unpack('B', e.data, 0x36)
		gVars.RegionControl['Argoneau']['own'] 			= struct.unpack('B', e.data, 0x3A)
		gVars.RegionControl['Fauregandi']['own'] 		= struct.unpack('B', e.data, 0x3E)
		gVars.RegionControl['Valdeaunia']['own'] 		= struct.unpack('B', e.data, 0x42)
		gVars.RegionControl['QuifimIsland']['own'] 		= struct.unpack('B', e.data, 0x46)
		gVars.RegionControl['LiTelor']['own'] 			= struct.unpack('B', e.data, 0x4A)
		gVars.RegionControl['Kuzotz']['own'] 			= struct.unpack('B', e.data, 0x4E)
		gVars.RegionControl['Vollbow']['own'] 			= struct.unpack('B', e.data, 0x52)
		gVars.RegionControl['ElshimoLowlands']['own'] 	= struct.unpack('B', e.data, 0x56)
		gVars.RegionControl['ElshimoUplands']['own'] 	= struct.unpack('B', e.data, 0x5A)
		gVars.RegionControl['Tulia']['own'] 			= struct.unpack('B', e.data, 0x5E)
		gVars.RegionControl['Movapolos']['own'] 		= struct.unpack('B', e.data, 0x62)
		gVars.RegionControl['Tavnazia']['own'] 			= struct.unpack('B', e.data, 0x66)
		utilities.UpdateRegionalLabel();
		e.blocked = false;
	end
end);

--[[
	t1 is a test procedure, used in trying out new things. It is not intended for players to use

	Pararameter
		args		list of passed in arguments
--]]

function crossjobs.t1(args)

	print(displaybar.fHTMLColor('red','This is red'));
	print(displaybar.fHTMLColor('yellow','This is yellow'));
	print(displaybar.fHTMLColor('blue','This is blue'));
	print(displaybar.fHTMLColor('unknown','This is unknown'));
--	for i=1,20,1 do
--		print(chat.color1(i,'color: ' .. tostring(i)));
--	end
end		-- crossjobs.t1

--[[
	SetVariables defines run settings for luashitacast.

	Note: Boxcaar introduces the concept of player agency. Instead of assuming the setting
	of these toggles/cycles, the settings block in the job file contains all valid references
	so the player can decide what they want to default to.
--]]

function crossjobs.SetVariables(bRefresh)
	local player = gData.GetPlayer();

	if bRefresh == nil then
		bRefresh = false;
	end

	-- General toggles
	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._GSWAP] ~= nil then	-- Gear Swap
		utilities.CreateToggle(gVars._GSWAP, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._GSWAP]);
	else
		utilities.CreateToggle(gVars._GSWAP, true);		-- Default value since not found in the DisplayBar's initialize area
	end

	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._KITE] ~= nil then		-- Kiting
		utilities.CreateToggle(gVars._KITE, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._KITE]);
	else
		utilities.CreateToggle(gVars._KITE, false);		-- Default value since not found in the DisplayBar's initialize area
	end

	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._EVASION] ~= nil then	-- Evasion
		utilities.CreateToggle(gVars._EVASION, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._EVASION]);
	else
		utilities.CreateToggle(gVars._EVASION, false);	-- Default value since not found in the DisplayBar's initialize area
	end

	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._IDLE] ~= nil then		-- Idling
		utilities.CreateToggle(gVars._IDLE, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._IDLE]);
	else
		utilities.CreateToggle(gVars._IDLE, true);		-- Default value since not found in the DisplayBar's initialize area
	end

	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SGS] ~= nil then		-- Show Gear Sets
		utilities.CreateToggle(gVars._SGS, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SGS]);
	else
		utilities.CreateToggle(gVars._SGS, false);		-- Default value since not found in the DisplayBar's initialize area
	end

	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SPF] ~= nil then		-- Show Pull Feedback
		utilities.CreateToggle(gVars._SPF, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SPF]);
	else
		utilities.CreateToggle(gVars._SPF, false);		-- Default value since not found in the DisplayBar's initialize area
	end

	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._TH] ~= nil then		-- Treasure Hunter
		utilities.CreateToggle(gVars._TH, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._TH]);
	else
		utilities.CreateToggle(gVars._TH, false);		-- Default value since not found in the DisplayBar's initialize area
	end

	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._WSWAP] ~= nil then	-- Weapon Swap
		utilities.CreateToggle(gVars._WSWAP, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._WSWAP]);
	else
		utilities.CreateToggle(gVars._WSWAP, false);	-- Default value since not found in the DisplayBar's initialize area
	end

	-- Tanking is now available to all jobs. In most cases jobs that people don't assume tank
	-- will never touch the setting, but the Player can toggle tanking if that's appropriate.
	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._TANK] ~= nil then		-- Tanking
		utilities.CreateToggle(gVars._TANK, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._TANK]);
	else
		utilities.CreateToggle(gVars._TANK, false);		-- Default value since not found in the DisplayBar's initialize area
	end

	-- Now deal with the job specific variables. Please note though that if data is still downloading, the
	-- subjob will be either nil or NON. That means if you're matching on subjob, it won't set correctly.

	-- Magic Accuracy (Macc). Only created if main job or sub job supports magic
	-- (Before checking, remove toggle if it exists. This can occur when changing subjobs.)
	if gVars.Toggles[gVars._MACC] ~= nil then
		gVars.Toggles[gVars._MACC] = nil;
	end
	-- Now process MACC "freshly"
	if utilities.fMagicalJob() == true then
		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._MACC] ~= nil then	-- Macc
			utilities.CreateToggle(gVars._MACC, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._MACC]);
		else
			utilities.CreateToggle(gVars._MACC, false);		-- Default
		end
	end

	-- BST only, AJug is an automated system to equip jug pets. DB is a setting to determine the
	-- type of debuff wanted from the Jackcoat. Either BPP or WSS (blind,poison,parallize) or
	-- (weighted,silence,slow). Please note that the Beast Jackoat +1 (AF +1) can dispel all
	-- six debuffs from the pet whereas Beast Jackcoat does BPP and Monster Jackcoat/Monster
	-- Jackcoat+1 only dispels WSS.
	if player.MainJob == 'BST' then
		-- Because AJug can be optional, need to check to see if it is definied in the display_bar
		-- settings before checking for an 'init' value.
		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._AJUG] ~= nil then
			utilities.CreateToggle(gVars._AJUG, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._AJUG]);
		else
			utilities.CreateToggle(gVars._AJUG, true);
		end

		-- Create the cycle, no default so treat as is
		utilities.CreateCycle(gVars._DB, {[1] = gVars._sDB_NORM, [2] = gVars._sDB_BPP, [3] = gVars._sDB_WSS});

		-- and then check for an initial setting. Like AJug this is an optional setting
		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._DB] ~= nil then
			utilities.fSetCycle(gVars._DB, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._DB]);
		else
			utilities.fSetCycle(gVars._DB, gVars._sDB_NORM);
		end
	end

	-- BRD main only, Instrument indicates what default type of instrument should be equipped,
	-- Horn or String
	if player.MainJob == 'BRD' then
		utilities.CreateCycle(gVars._INSTRUMENT, {[1] = gVars._HORN, [2] = gVars._STRING});

		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._INSTRUMENT] ~= nil then
			utilities.fSetCycle(gVars._INSTRUMENT, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._INSTRUMENT]);
		else
			utilities.fSetCycle(gVars._INSTRUMENT, gVars._HORN);
		end
	end

	-- SMN only, sBP indicates if the a message should be printed in the party chat when
	-- the pet does an offensive blood pact.
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SBP] ~= nil then
			utilities.CreateToggle(gVars._SBP, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SBP]);
		else
			utilities.CreateToggle(gVars._SBP, true);
		end

		utilities.CreateCycle(gVars._MODE, {[1] = gVars._MODE_PERPETUATION, [2] = gVars._MODE_ATTACK, [3] = gVars._MODE_ENMITY_MINUS});
		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._MODE] ~= nil then
			utilities.fSetCycle(gVars._MODE, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._MODE]);
		else
			utilities.fSetCycle(gVars._MODE, gVars._MODE_PERPETUATION);
		end
	end

	-- DRG only, HPPlus indicates if the DRG's HPPlus set should be equipped prior to healing breath
	if player.MainJob == 'DRG' or player.SubJob == 'DRG' then
		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._HPPLUS] ~= nil then
			utilities.CreateToggle(gVars._HPPLUS, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._HPPLUS]);
		else
			utilities.CreateToggle(gVars._HPPLUS, false);
		end
	end

	-- THF only, SS indicates that when the player steals, a message should be displayed.
	-- This is used to coordinate thieve's stealing in activities like Dynamis.
	if player.MainJob == 'THF' or player.SubJob == 'THF' then
		if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SS] ~= nil then
			utilities.CreateToggle(gVars._SS, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._SS]);
		else
			utilities.CreateToggle(gVars._SS, false);
		end
	end

	-- Damage Taken
	utilities.CreateCycle(gVars._DT, {[1] = gVars._DT_OFF, [2] = gVars._DT_PHY, [3] = gVars._DT_MAG, [4] = gVars._DT_BRE});
	if gProfile.settings.DisplayBar[gVars._INITIALIZE] ~= nil and gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._DT] ~= nil then
		utilities.fSetCycle(gVars._DT, gProfile.settings.DisplayBar[gVars._INITIALIZE][gVars._DT]);
	else
		utilities.fSetCycle(gVars._DT, gVars._DT_OFF);
	end


	-- Lastly, make sure all custom conditionals are defined
	if gProfile.CustomConditionals ~= nil then
		for _,j in ipairs(gProfile.CustomConditionals) do
			j['code'] = string.upper(j['code']);
			utilities.CreateToggle(j['code'],j['init']);
		end
	end

	if bRefresh == true and gProfile.settings.bConfirmation == true then
		print(chat.message('Info: Variables are refreshed'));
	end
end		-- crossjobs.SetVariables

--[[
	ProcessAccuracy performs the task requested dealing with Accuracy or Ranged Accuracy.

	Pararameter
		args		Passed in list of args from the command line

	Invocation: 	/acc [#|MAX] [help]
--]]

local function ProcessAccuracy(args)
	local bTank = utilities.fGetToggle(gVars._TANK);
	local tmp,narg;
	local num = 0;		-- 0 means turn off that type of accuracy

	if args[1] == 'acc' then
		if bTank == true then
			tmp = gVars._Progressive_TACC;
		else
			tmp = gVars._Progressive_ACC;
		end
	elseif args[1] == 'racc' then
		if bTank == true then
			tmp = gVars._Progressive_TRACC;
		else
			tmp = gVars._Progressive_RACC;
		end
	end

	if #args == 1 then
		-- This is an unload acc command. Make sure both the appropriate acc and the tank versions are unloaded
		if args[1] == 'acc' then
			displaybar.SetAccCur(gVars._Progressive_ACC,0);
			displaybar.SetAccCur(gVars._Progressive_TACC,0);
		else
			displaybar.SetAccCur(gVars._Progressive_RACC,0);
			displaybar.SetAccCur(gVars._Progressive_TRACC,0);
		end

		if gProfile.settings.bConfirmation then
			if tmp == gVars._Progressive_ACC or tmp == gVars._Progressive_TACC then
				print(chat.message('Info: Accuracy has been turned off'));
			else
				print(chat.message('Info: Ranged Accuracy has been turned off'));
			end
		end
	else
		for _,j in pairs(args) do
			if string.upper(j) == 'MAX' then	-- Maximum (R)Acc stage
				narg = gear.fGetAccStage(tmp,'MAX')
			elseif string.find('visible,invisible',j) == nil then	-- This is a stage
				narg = tonumber(args[2]);
			end
		end

		if narg < 0 or narg > gear.fGetAccStage(tmp,'MAX') then
			print(chat.message('Info: Invalid stage. Number must be between 0 and ' .. tostring(gear.fGetAccStage(tmp,'MAX'))));
			return;
		else
			num = narg;
		end

		if num == 0 then
			displaybar.SetAccCur(tmp,0);
			-- Make sure that both tank and non-tank versions are turned off
			if string.find(gVars._Progressive_ACC..','..gVars._Progressive_RACC,tmp) ~= nil then
				tmp = 'T' .. tmp;				-- Add a 'T' to the beginning of tmp
			else
				tmp = string.sub(tmp,2,-1);		-- Remove the 'T' from the beginning of tmp
			end
			displaybar.SetAccCur(tmp,0);
			if gProfile.settings.bConfirmation == true then
				if tmp == gVars._Progressive_ACC or tmp == gVars._Progressive_TACC then
					print(chat.message('Info: Accuracy has been turned off'));
				else
					print(chat.message('Info: Ranged Accuracy has been turned off'));
				end
			end
		else
			displaybar.SetAccCur(tmp,num);
			if gProfile.settings.bConfirmation == true then
				print(chat.message(string.format('Info: %s stage set to %d',tmp,num)));
			end
		end
	end
end		-- ProcessAccuracy

--[[
	ProcessCap enables/disables an artificial level cap

	Invocation: /cap [#] {help}
--]]

local function ProcessCap(args);
	local player = gData.GetPlayer();
	local ss;

	-- Find the cap number
	for _,j in pairs(args) do
		if string.find('cap,visible,invisible',j) == nil then
			ss = tonumber(j);
			break;
		end
	end

	if ss ~= nil then
		if ss < player.MainJobSync and ss >= 0 then
			gProfile.system_settings.PlayerCappedLevel = ss;
			if gProfile.settings.bConfirmation == true then
				print(chat.message(string.format('Info: Player gear level capped at level %d',gProfile.system_settings.PlayerCappedLevel)));
			end
		else
			print(chat.message('Info: Invalid level cap specified: ' .. tostring(s)));
		end
	else
		gProfile.system_settings.PlayerCappedLevel = 0;
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Player gear level cap disabled'));
		end
	end
end		-- ProcessCap

--[[
	ProcessCC enables/disables the indicated custom conditional or lists the commands

	Invocation: /cc[#] [help]
--]]

local function ProcessCC(args)

	if args[1] == 'cc' then
		reporting.DisplayCC();
	else
		if string.find(args[1],'cc') ~= nil then
			local us = string.upper(args[1]);
			utilities.AdvanceToggle(us);
			if gProfile.settings.bConfirmation == true then
				print(chat.message('Info: /' .. us .. ', ' .. utilities.fGetCCDescription(us) .. ', is set to ' .. tostring(utilities.fGetToggle(us))));
			end
		end
	end
end		-- ProcessCC

--[[
	ProcessDT either advances /DT setting or assigns the setting directly

	Invocation: /dt [M|B|P|O] [help]
--]]

local function ProcessDT(args)
	local bFound = false;

	for i,j in pairs(args) do
		local cType = string.upper(string.sub(j,1,1));
		if cType == gVars._DT_M then
			utilities.fSetCycle(gVars._DT,gVars._DT_MAG);
			bFound = true;
		elseif cType == gVars._DT_B then
			utilities.fSetCycle(gVars._DT,gVars._DT_BRE);
			bFound = true;
		elseif cType == gVars._DT_P then
			utilities.fSetCycle(gVars._DT,gVars._DT_PHY);
			bFound = true;
		elseif cType == gVars._DT_O then
			utilities.fSetCycle(gVars._DT,gVars._DT_PHY);
			bFound = true;
		end
	end

	if bFound == false then
		utilities.AdvanceCycle(gVars._DT);
		bFound = true;
	end

	if gProfile.settings.bConfirmation == true and bFound == true then
		print(chat.message('Info: /DT is set to ' .. utilities.fGetCycle(gVars._DT)));
	end
end		-- ProcessDT

--[[
	ProcessHornString sets which type of bard instrument should be the default

	Invocation: /horn [help]
				/string [help]
--]]

local function ProcessHornString(args)
	local player = gData.GetPlayer();
	local bFound = false;

	if player.MainJob == 'BRD' then
		for _,j in pairs(args) do
			j = string.lower(j);
			if j == 'horn' then
				utilities.fSetCycle(gVars._INSTRUMENT,gVars._HORN);
				bFound = true;
			elseif j == 'string' then
				utilities.fSetCycle(gVars._INSTRUMENT,gVars._STRING);
				bFound = true;
			end
		end

		if bFound == true and gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Instrument is set to ' .. utilities.fGetCycle(gVars._INSTRUMENT)));
		end
	else
		print(chat.message('Info: Only bards use this command. Ignoring.'));
	end
end		-- ProcessHornString

--[[
	ProcessMode indicates whether perpetuation cost or attack power should be emphasized, SMN only

	Invocation: /mode [PERP|ATTK|ENMM] [help]
--]]

local function ProcessMode(args)
	local player = gData.GetPlayer();
	local u2;

	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		for _,j in  pairs(args) do
			j = string.upper(j);
			if  string.find('ATTK,PERP,ENMM',j) ~= nil then
				if string.len(j) >= 1 then
					u2 = string.sub(u2,1,1);
				else
					u2 = j;
				end
			end
		end

		if u2 == gVars._MODE_A then
			utilities.fSetCycle(gVars._MODE,gVars._MODE_ATTACK);
		elseif u2 == gVars._MODE_P then
			utilities.fSetCycle(gVars._MODE,gVars._MODE_PERPETUATION);
		elseif u2 == gVars._MODE_E then
			utilities.fSetCycle(gVars._MODE,gVars._MODE_ENMITY_MINUS);
		else
			utilities.AdvanceCycle(gVars._MODE);
		end
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Mode is set to ' .. utilities.fGetCycle(gVars._MODE)));
		end
	else
		print(chat.message('Info: /mode is only available to summoners'));
	end
end		-- ProcessMode

--[[
	ProcessSW will equip the starting weapons. This is a method to force the correct
	weapons to be equipped
--]]

local function ProcessSW()
	local player = gData.GetPlayer();

	utilities.ClearSet(crossjobs.Sets.CurrentGear);
	gear.MoveToDynamicGS(gProfile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,false,'Start_Weapons');
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);
end		-- ProcessSW

--[[
	ProcessWSDISTANCE either sets a new weapon skil distance or turns the check on or off

	Form: WSDISTANCE [#] [help]
--]]

local function ProcessWSDISTANCE(args)

	if args[2] ~= nil then
		gProfile.settings.WScheck = true;
		gProfile.settings.WSdistance = tonumber(args[2]);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: WS Distance is now on and set to ' .. tostring(gProfile.settings.WSdistance)));
		end
	else
		gProfile.settings.WScheck = not gProfile.settings.WScheck;
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: WS distance check is now set to ' .. tostring(gProfile.settings.WScheck)));
		end
	end
end		-- ProcessWSDISTANCE

--[[
	ProcessToggle sets the passed toggle appropriately

	Params:
		args		Passed arguments
		id			Id wanted flipped
		job			Job to limit it to or nil
		bSJ			Should the subjob be included
		bMacc		Check on magic ability

	Invocation:
		/aJug [help]
		/eva [help]
		/gswap [help]
		/idle [help]
		/kite [help]
		/macc [help]
		/sbp [help]
		/sgs [help]
		/spf [help]
		/ss [help]
		/tank [help]
		/th [help]
		/wswap [help]
--]]

local function ProcessToggle(args,id,job,bSJ,bMagic)
	local player = gData.GetPlayer();

	if bSJ == nil then
		bSJ = false;
	end

	if bMagic == nil then
		bMagic = false;
	end

	if (bMagic == true and utilities.fCheckMagicJob() == true) or bMagic == false then
		if job == nil or player.MainJob == job or (bSJ == true and player.SubJob == job) then
			utilities.AdvanceToggle(id);
			if gProfile.settings.bConfirmation == true then
				print(chat.message('Info: /' .. string.lower(id) .. ' is set to ' .. tostring(utilities.fGetToggle(id))));
			end
		else
			if bSJ == true then
				print(chat.message('Warning: /' .. string.lower(id) .. ' requires either your main job or sub job be ' .. job));
			else
				print(chat.message('Warning: /' .. string.lower(id) .. ' requires either your main job be ' .. job));
			end
		end
	else
		print(chat.message('Warning: /' .. string.lower(id) .. ' requires a magic job'));
	end
end		-- ProcessToggle

--[[
	ProcessDB sets the DeBuff conditional setting appropriately

	Invocation: /db [BPP|WSS] [help]
--]]

local function ProcessDB(args)
	local s = nil;

	if player.MainJob == 'BST' then
		for _,j in pairs(args) do
			j = string.upper(j);
			if string.find(gVars._sDB_Debuffs,j) ~= nil then
				s = j;
				break;
			end
		end

		if s ~= nil then
			utilities.SetCycle(gVars._DB,s);
		else
			utilities.AdvanceCycle(gVars._DB);
		end

		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: /' .. gVars._DB .. ' is set to ' .. utilities.fGetCycle(gVars._DB)));
		end
	else
		print(chat.message('Warning: This command is only valid if your main job is BST'));
	end
end		-- ProcessDB

--[[
	ProcessDBar is used to setup/modify the display bar

	Invocation: /dbar [show] [bar=i|v] [vis=[all|none|name,name,...] ] [invis=[all|none|name,name,...] ] [save|reset] [file[=name] ][+]
--]]

local function ProcessDBar(args)
	local bShow = false;
	local sVis,sInvis,iPos,sFile,sBar;
	local bAppend = false;
	local bReset = false;
	local bSave = false;

	print(chat.message('/dbar is not implemented yet'));

	-- Process the inline options
	if #args == 1 then
		bShow = true;
	else
		for i = 2,#args,1 do
			args[i] = string.lower(args[i]);
			if args[i] == 'show' then
				bShow = true;
			elseif args[i] == 'save' then
				bSave = true;
			elseif args[i] == 'reset' then
				bReset = true;
			elseif string.find(args[i],'bar') ~= nil and string.find(args[i],'bar') == 1 then
				iPos = string.find(args[i],'=');
				if iPos ~= nil and iPos > 0 then
					sBar = string.sub(args[i],iPos+1,iPos+1);
				end
			elseif string.find(args[i],'invis') ~= nil and string.find(args[i],'invis') == 1 then
				iPos = string.find(args[i],'=');
				if iPos ~= nil then
					sInvis = string.sub(args[i],iPos+1,-1);
				end
			elseif string.find(args[i],'vis') ~= nil and string.find(args[i],'vis') == 1 then
				iPos = string.find(args[i],'=');
				if iPos ~= nil then
					sVis = string.sub(args[i],iPos+1,-1);
				end
			elseif string.find(args[i],'file') ~= nil and string.find(args[i],'file') == 1 then
				iPos = string.find(args[i],'=');
				if iPos ~= nil then
					sFile = string.sub(args[i],iPos+1,-1);
					iPos = string.find(sFile,'%+');
					if iPos ~= nil then
						bAppend = true;
						sFile = string.sub(sFile,1,iPos-1);
					end
				end
			elseif args[i] == '+' then
				bAppend = true;
			end
		end

		-- Now process what was found
		if bSave == true then
			print(chat.message('"/dbar save" is not implemented yet'));
		end

		if sFile ~= nil then
			print(chat.message('"/dbar file" is not implemented yet'));
		end

		if bShow == true then
			print(chat.message('"/dbar show" is not implemented yet'));
		end

		if sBar ~= nil then
			displaybar.FontObject.visible = (sBar == 'v');
		end

		if bReset == true then
			displaybar.ResetVisibility();
		end

		if sVis ~= nil then
			displaybar.SetVisibility(sVis,true);
		end

		if sInvis ~= nil then
			displaybar.SetVisibility(sInvis,false);
		end
	end
end		-- ProcessDBar

--[[
	HandleCommands processes any commands typed into luashitacast as defined in this file

	Pararameters
		args		List of arguments passed with the command    print(chat.message('Resultant gear breakdown:')));
--]]

function crossjobs.HandleCommands(args)
	local player = gData.GetPlayer();
	local bTank = utilities.fGetToggle(gVars._TANK);
	local sList, sKey, sSet;

	args[1] = string.lower(args[1]);

	-- Clear out the local copy of current gear
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	-- Now, process the commands. They're listed in alphabetical order
	if args[1] == '911' then
		-- /911, SMN Only. Emmergency call to summon an appropriate spirit
		pets.Call911();
	elseif args[1] == 'acc' or args[1] == 'racc' then
		-- /ACC or /RACC, sets the level for the accuracy/ranged accuracy
		ProcessAccuracy(args);
	elseif args[1] == 'ajug' then
		-- /AJUG, BST only, Turns on/off Automatic Pet Jug assignment is enabled
		ProcessToggle(args,gVars._AJUG,'BST',false,false);
	elseif args[1] == 'cap' then
		-- /CAP, Sets an artificial level cap
		ProcessCap(args);
	elseif string.find(args[1],'cc') ~= nil then
		-- /CC#, Either displays Custom conditionals or turns them on/off
		ProcessCC(args);
	elseif args[1] == 'db' then
		-- /db, BST only, sets the cycle to the appropriate value
		ProcessDB(args);
	elseif args[1] == 'dbar' then
		-- /dbar, manipulates the settings on the display bar
		ProcessDBar(args);
	elseif args[1] == 'dt' then
		-- /DT, Indicates the type of damage taken gear that will be equipped if enabled
		ProcessDT(args);
	elseif string.find('equipit,ei',args[1]) ~= nil then
		-- /EQUIPIT or /EI, equip specified item or coded item
		gear.EquipItem(args);
	elseif args[1] == 'eva' then
		-- /EVA, turns on/off whether evasion gear should be equipped
		ProcessToggle(args,gVars._EVASION,nil,false,false);
	elseif args[1] == 'gc' then
		-- /GC, Invoke the Gear Check command
		gear.ProcessGC(args);
	elseif string.find('gearset,gs',args[1]) ~= nil then
		-- /GS, equips the specified gear and locks slots appropriately
		gear.ProcessGS(args);
	elseif args[1] == 'gswap' then
		-- /GSWAP, turns gear swapping on/off
		ProcessToggle(args,gVars._GSWAP,nil,false,false);
	elseif string.find('horn,string',args[1]) ~= nil then
		-- /STRING or /HORN, BRD only, indicates type of default instrument
		ProcessHornString(args);
	elseif args[1] == 'hpp' then
		-- /HPP, DRG only, indicates if HP+ set should be equipped prior to healing breath
		ProcessToggle(args,gVars._HPPLUS,nil,false,false);
	elseif args[1] == 'idle' then
		-- /IDLE, Turns on/off whether idle gear should be equipped during idle
		ProcessToggle(args,gVars._IDLE,nil,false,false);
	elseif args[1] == 'kite' then
		-- /KITE, Turns on/off whether movement gear is equipped
		ProcessToggle(args,gVars._KITE,nil,false,false);
	elseif args[1] == 'lachelp' then
		-- /LACHELP, Shows the help system, either a list of valid commands or details on a specific command
		help.ShowHelp(args);	-- needs more work
	elseif string.find('lock,unlock',args[1]) ~= nil then
		-- /LOCK, Lock/unlock gear slots
		locks.ProcessLocks(args);
	elseif args[1] == 'macc' then
		-- /MACC, Turns on/off whether magic accuracy gear is equipped when casting magic
		ProcessToggle(args,gVars._MACC,nil,false);
	elseif args[1] == 'maxsong' then
		-- MAXSONG, Determines highest level song of the type you specified and casts it
		magic.MaxCast(args[2],false,args[3],true);
	elseif args[1] == 'maxspell' then
		-- MAXSPELL, Determines highest level spell of the type you specified and casts it
		magic.MaxCast(args[2],true,args[3],true);
	elseif args[1] == 'mode' then
		-- /MODE, SMN only, Turns on/off smn emphasis for gear when pet out
		ProcessMode(args);
	elseif args[1] == 'petfood' then
		-- /PETFOOD, equips the appropriate pet food
		pets.fPetReward(args[2],true);
	elseif args[1] == 'pull' then
		-- /PULL, pull the target
		utilities.PullTarget();
	elseif args[1] == 'rc' then
		-- /RC, Display region controls
		reporting.RegionControlDisplay(args);
	elseif args[1] == 'rv' then		-- Refresh variables
		-- /RV, refresh the starting variables
		crossjobs.SetVariables(true);
	elseif args[1] == 'sbp' then
		-- /SBP, Turns on/off whether the SMN blood pact message is shown
		ProcessToggle(args,gVars._SBP,'SMN',true,false);
	elseif args[1] == 'sgs' then
		-- /SGS, Turns on/off whether feedback when gear sets are processed should be given
		ProcessToggle(args,gVars._SGS,nil,false,false);
	elseif args[1] == 'showit' then
		-- /SHOWIT, Shows debug info
		reporting.DB_ShowIt();
	elseif args[1] == 'smg' then
		-- /SMG, Show my gear
		reporting.ProcessSMG(args);
	elseif args[1] == 'spf' then
		-- /SPF, Turns on/off whether Show Pull feedback should be displayed
		ProcessToggle(args,gVars._SPF,nil,false,false);
	elseif args[1] == 'ss' then
		-- /SS, THF only, enables/disables whether attempted steals should be announced
		ProcessToggle(args,gVars._SS,'THF',true,false);
	elseif args[1] == 'sw' then
		-- /sw, Equips the start weapons
		ProcessSW();
	elseif args[1] == 'tank' then
		-- /TANK, Turns on/off whether tanking gear is equipped
		ProcessToggle(args,gVars._TANK,nil,false,false);
	elseif args[1] == 't1' then
		-- /T1, debug command. Used to test code
		crossjobs.t1(args);
	elseif args[1] == 'th' then
		-- /TH, Turns on/off whether TH gear should be equipped
		ProcessToggle(args,gVars._TH,nil,false,false);
	elseif args[1] == 'val' then
		print(chat.message('Info: command is not implemented yet'));
	elseif args[1] == 'ver' then
		-- /VER, Display version/change log
		reporting.DisplayVersion();
	elseif args[1] == 'wsdistance' then
		-- /WSDISTANCE, Turns on/off the check for weapons skill distance or sets the distance
		ProcessWSDISTANCE(args);
	elseif args[1] == 'wswap' then
		-- /WSWAP, Turns on/off whether weapon swapping is permitted
		ProcessToggle(args,gVars._WSWAP,nil,false,false);
	else
		-- Unrecognized command
		print(chat.message('Info: Unrecognized command specified: ' .. args[1]));
		return;
	end
end		-- crossjobs.HandleCommands

--[[
	HandleAbility is the general use version of said routine that coordinates all ability gear equipping.
	It was originally part of each job file.
--]]

function crossjobs.HandleAbility()
	local player = gData.GetPlayer();
	local ability = gData.GetAction();
	local eq = gData.GetEquipment();
	local n,ts,bValid;
	local bFound = false;
	local bPreBP = false;

	-- Store the name of the ammo. This is used when the ammo slot is automatically
	-- populated so that the original ammo can be re-equipped.
	if eq.Ammo ~= nil then
		gProfile.system_settings.sAmmo = eq.Ammo.Name;
	else
		gProfile.system_settings.sAmmo = nil;
	end

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	-- The Job_Ability gear set handles all abilities, so if something
	-- extra has to occur, do it here.

	-- Check for summoner's blood pact, to load the pre-blood pact gearset (PreBP)
	if table.find(pets.SmnBPRageList,ability.Name) ~= nil or
			table.find(pets.SmnBPWardList,ability.Name) ~= nil then
		gear.MoveToDynamicGS(gProfile.Sets.PC_Pre_BloodPact,crossjobs.Sets.CurrentGear,false,'PC_Pre_BloodPact');
		bPreBP = true;
	elseif string.match(ability.Name, 'Reward') then
		-- Obviously a BST or /BST doing a reward. Make sure a pet food has been loaded
		if gProfile.system_settings.sAmmo == nil or string.find(string.lower(gProfile.system_settings.sAmmo),'pet f') == nil then
			-- either empty or something else equipped
			gProfile.system_settings.bAmmo = pets.PetReward(gProfile.settings.defaultPetFood,'max');
			if gProfile.system_settings.bAmmo == true then
				crossjobs.Sets.CurrentGear['Ammo'] = gProfile.system_settings.sAmmo;
			end
		end
	elseif string.match(ability.Name, 'Call Beast') then
		-- see if a pet jug already equipped. If not and the automatic jug function is enabled,
		-- determine jug to use and equip it.
		if utilities.fGetToggle(gVars._AJUG) == true then
			local current = gData.GetCurrentSet();
			local bValid = false;

			if current['Ammo'] ~= nil or current['Ammo'] ~= '' then
				-- since something is in the ammo slot, check that it's a jug pet
				bValid = pets.fIsValidJugPet(current['Ammo']);
			end

			if bValid == false then
				-- since not a jug pet, determine which jug to equip
				local x = pets.fWhichJugToEquip();
				if x ~= nil then
					crossjobs.Sets.CurrentGear['Ammo'] = x;
				end
			end
		end
	end

	if bPreBP == false then
		gear.MoveToDynamicGS(gProfile.Sets.Job_Ability,crossjobs.Sets.CurrentGear,false,'Job_Ability');
	end

	-- Now, equip the appropriate gear
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);
end		-- crossjobs.HandleAbility

--[[
	HandleItem is the general use version of said routine that coordinates all item gear equipping.
	It was originally part of each job file.
--]]

function crossjobs.HandleItem()
	local item = gData.GetAction();
	local bShow = false;

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(sets.CurrentGear);

	if string.match(item.Name, 'Silent Oil') then
		gear.MoveToDynamicGS(gProfile.Sets.Sneak,crossjobs.Sets.CurrentGear,false,'Sneak');
		bShow = true;
	elseif string.match(item.Name, 'Prism Powder') then
		gear.MoveToDynamicGS(gProfile.Sets.Invisible,crossjobs.Sets.CurrentGear,false,'Invisible');
		bShow = true;
	end

	if bShow == true then
		gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);
	end
end		-- crossjobs.HandleItem

--[[
	HandlePreshot is the general use version of said routine that coordinates all preshot item gear
	equipping.	It was originally part of each job file.
--]]

function crossjobs.HandlePreshot()

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	gear.MoveToDynamicGS(gProfile.Sets.Preshot,crossjobs.Sets.CurrentGear,false,'PreShot');
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);
end		-- crossjobs.HandlePreshot

--[[
	HandleMidshot is the general use version of said routine that coordinates all midshot item gear
	equipping.	It was originally part of each job file.
--]]

function crossjobs.HandleMidshot()

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	gear.MoveToDynamicGS(gProfile.Sets.Midshot,crossjobs.Sets.CurrentGear,false,'Midshot');
	crossjobs.ProgressiveAccuracy('RAcc');

	gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);
end		-- HandleMidshot

--[[
	HandleWeaponskill is the general use version of said routine that coordinates all weapon skill
	item gear equipping. It was originally part of each job file.
--]]

function crossjobs.HandleWeaponskill()
	local canWS = utilities.fCheckWsBailout();

	-- If conditions would cause the weaponskill to fail, the action will be
	-- cancelled so you do not lose your TP.
	if canWS == false then
		gFunc.CancelAction();
		return;
	end

	-- Clear out the CurrentGear in case of leftovers
	gear.ClearSet(crossjobs.Sets.CurrentGear);

	-- Call the common weaponskill handler
	crossjobs.fHandleWeaponskill();

	-- Equip the composited weaponskill set
	gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);
end		-- crossjobs.HandleWeaponskill

--[[
	fHandleWeaponskill loads the appropriate gear for the weapon skill you're doing

	-- Transition not complete --
--]]

function crossjobs.fHandleWeaponskill()
	local t = {};
	local ws = gData.GetAction();
	local s,sWSName;
	local sName,statName;

	if ws.Name == nil then
		return;
	end

	sName = string.lower(string.gsub(ws.Name,' ','_'));
	-- First see if there's a custom weaponskill definition
	for i,j in pairs(gProfile.Custom_Weaponskills) do
		if string.lower(i) == sName then
			-- It's a custom weaponskill definition
			t = j;
			break;
		end
	end

	if t == nil then
		-- Not custom, so look for what stat(s) are emphasized
		statName,sWSName = utilities.fWhichWSStat(sName);
		if statName == nil then
			s = 'Warning: Weaponskill type for ' .. sWSName .. ' is undefined.'
			reporting.DisplayOnce(s,false);
			return;
		else
			t = gProfile.Sets.Weaponskill;
		end
	end

	if t == nil then		-- Should never occur
		return;
	end

	-- Must have been found
	gProfile.system_settings.WSTypeName = 'WS_' .. string.upper(statName);
	gear.MoveToDynamicGS(t,gProfile.Sets.CurrentGear,false,'Weaponskill');

	-- Now, process the other gearsets that affects weapon skills based on the order
	-- provided by the player
	for _,j in ipairs(gProfile.settings.postGSWeaponSkill) do
		j = string.lower(j);
		if j == 'eGorget' and gProfile.settings.EmbedOnly.eGorget == false then
			-- An elemental gorget will add the fTP (at least 10% more damage) to the first hit
			-- of an elemental weapon skill (and many multi-hit weapon skills replicate the fTP
			-- for all the hits.) Also, they give +10 Accuracy to all of the weapon skill's hits
			-- and a 1% chance of not depleting the player's TP after the weapon skill.
			local sGorget,sEle = gear.fCheckForElementalGearByValue('gorget','eleWS',ws.Name);
			if sGorget ~= nil then
				crossjobs.Sets.CurrentGear['Neck'] = sGorget;
			end
		elseif j == 'eObi' and gProfile.settings.EmbedOnly.eObi == false then
--[[
			If the weaponskill is elemental and is closing a skillchain, then if
			the conditions for equipping an elemental obi are advantageous, it
			should be equipped now. Unfortunately I have no idea how to detect
			the closing of a skillchain and the automatic equipping of an elemental
			obi could adversely affect the damage, so this section is not
			implemented. If I can ever figure out how to detect closing a
			skillchain, I will readdress this.

			- CCF, 1/12/2024
--]]
		elseif j == 'Acc' and gProfile.settings.EmbedOnly.Accuracy == false then
			if table.find(gVars.tWeaponSkills['RANGED_AGI'],sName) ~= nil or
				table.find(gVars.tWeaponSkills['RANGED_STRAGI'],sNname) ~= nil then
				crossjobs.ProgressiveAccuracy('RAcc');
			else
				crossjobs.ProgressiveAccuracy('Acc');
			end
		end
	end

	-- Certain weapon skills can take advantage of magic attack bonus. Check here and equip gear
	-- appropriately. (Note: even though rMAB is a reference gear set, in this particular instance
	-- it is treated like it is a normal gear set.)
	if string.find('red lotus blade,sanguine blade',sName) ~= nil then
		gear.MoveToDynamicGS(gProfile.Sets.rMAB,gProfile.Sets.CurrentGear,false,'rMAB');
	end
end		-- crossjobs.fHandleWeaponskill

--[[
	ProgressiveAccuracy is a new form of applying accuracy gear which depends on a list of
	successive stages. The Player predefines the stages and based on the stage specified, all
	stages prior and up to that stage will be equipped.

	Note: If TANK enabled, but the appropriate Tank_"set" is not defined in the Progressive
	structure, the non-Tank version will be used. (In this case it is assumed that inline
	conditionals will distinguish between Tank_ and non-Tank_ gear.)
--]]

function crossjobs.ProgressiveAccuracy(sType)
	local bTank = utilities.fGetToggle('Tank');
	local tmp,field;
	local tField = {
		['Acc']   = 'Accuracy',
		['TAcc']  = 'Tank_Accuracy',
		['RAcc']  = 'Ranged_Accuracy',
		['TRAcc'] = 'Tank_Ranged_Accuracy'
	};

	if sType == nil then
		sType = 'Acc';		-- The other valid type is RAcc
	end

	if bTank == nil then
		bTank = false;
	end

	-- See if an accuracy stage has been set and determine the correct
	-- reference code based on passed in type and whether Tank in on.
	if sType == 'Acc' then
		field = tField['Acc'];
		if bTank == true and gear.fGetAccStage('TAcc','MAX') > 0 then
			tmp = 'TAcc';
			if gProfile.Sets.Progressive[field] == nil then
				field = tField['Acc'];
			end
		else
			tmp = 'Acc';
		end

		if gear.fGetAccStage(tmp,'CUR') == 0 then
			return;
		end
	elseif sType == 'RAcc' then
		field = tField['RAcc'];
		if bTank == true and gear.fGetAccStage('TRAcc','MAX') > 0 then
			tmp = 'TRAcc';
			if gProfile.Sets.Progressive[field] == nil then
				field = tField['RAcc'];
			end
		else
			tmp = 'RAcc';
		end

		if gear.fGetAccStage(tmp,'CUR') == 0 then
			return;
		end
	else
		return;
	end

	if gProfile.Sets.Progressive[field] ~= nil then
		local maxStage = gear.fGetAccStage(tmp,'CUR');
		for i,j in ipairs(gProfile.Sets.Progressive[field]) do
			if i <= maxStage then
				gear.MoveToDynamicGS(j,crossjobs.Sets.CurrentGear,false,tmp);
			else
				break;
			end
		end
	else
		local msg = field .. ' undefined in the Progressive structure';
		reporting.DisplayOnce(msg,false);
	end
end		-- crossjobs.ProgressiveAccuracy

--[[
	Unload ensures that the display settings are saved, the aliases are removed,
	and any registered function is unregistered. This routine either addresses
	all of the module's unloads or invokes individual unloads as needed.
--]]

function crossjobs.Unload()
	-- Remove the command and custom conditional command alias
	utilities.ClearAliasAll();
	utilities.ClearAliasCC();

	-- Unregister the region control sensor
	ashita.events.unregister('packet_in', 'packet_in_callback1');

	-- Remove all objects associated with the display bar
	displaybar.Unload();

	-- unload dynamic arrays
	gVars.AverageMaxStats = {};

end		-- crossjobs.Unload

return crossjobs;
