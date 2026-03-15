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
			ProcessAjug					Enables/disables automatic pet jug, BST only
			ProcessCap					Enables/disables an artificial level cap
			ProcessCC					Displays custom conditionals or enable/disables the cc
			ProcessDB					Sets the debuff indicator to the appropriate type, BST only
			ProcessDT					Sequences or assigns directly the damage taken gear type
			ProcessHornString			Determines the type of bard instrument should be default
			ProcessMACC					Enables/disables magic accuracy gear when casting magic
			ProcessMode					Toggles between perpetuation cost and attack power, SMN only
			ProcessSBP					Toggles whether a party message is shown during a blood pact, SMN only
			ProcessSS					Toggles whether attempted steals should be announced, THF only
			ProcessSW					Equips the starting weapons
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

	print(locks.fCompactLocks());
--[[
	for i,j in ipairs(locks.tSlotLocks) do
		print(j['slot'] .. ' - ' .. tostring(j['lock']));
	end

	for slot,name in pairs(gVars.tGearDetails) do
		print(chat.message('Slot: ' .. slot));

		for i,j in pairs(name) do
			if string.find('num,acc,vis',i) == nil then
				print(chat.message(i));
			end
		end
		print(' ');
	end
--]]
end		-- crossjobs.t1

--[[
	SetVariables defines run settings for luashitacast.

	Note: Boxcaar introduces the concept of player agency. Instead of assuming the setting
	of these toggles/cycles, the settings block in the job file contains all valid references
	so the player can decide what they want to default to.
--]]

function crossjobs.SetVariables()
	local player = gData.GetPlayer();

	-- General toggles
	if gProfile.settings.DisplayBar[gVars._GSWAP]['init'] ~= nil then				-- Gear Swap
		utilities.CreateToggle(gVars._GSWAP, gProfile.settings.DisplayBar[gVars._GSWAP]['init']);
	else
		utilities.CreateToggle(gVars._GSWAP, true);
	end

	if gProfile.settings.DisplayBar[gVars._KITE]['init'] ~= nil then				-- Kiting
		utilities.CreateToggle(gVars._KITE, gProfile.settings.DisplayBar[gVars._KITE]['init']);
	else
		utilities.CreateToggle(gVars._KITE, false);
	end

	if gProfile.settings.DisplayBar[gVars._EVASION]['init'] ~= nil then					-- Evasion
		utilities.CreateToggle(gVars._EVASION, gProfile.settings.DisplayBar[gVars._EVASION]['init']);
	else
		utilities.CreateToggle(gVars._EVASION, false);
	end

	if gProfile.settings.DisplayBar[gVars._IDLE]['init'] ~= nil then				-- Should Default set equip when idling
		utilities.CreateToggle(gVars._IDLE, gProfile.settings.DisplayBar[gVars._IDLE]['init']);
	else
		utilities.CreateToggle(gVars._IDLE, false);
	end

	if gProfile.settings.DisplayBar[gVars._SPF]['init'] ~= nil then					-- Show Pull Feedback
		utilities.CreateToggle(gVars._SPF, gProfile.settings.DisplayBar[gVars._SPF]['init']);
	else
		utilities.CreateToggle(gVars._SPF, false);
	end

	if gProfile.settings.DisplayBar[gVars._TH]['init'] ~= nil then					-- Treasure Hunter
		utilities.CreateToggle(gVars._TH, gProfile.settings.DisplayBar[gVars._TH]['init']);
	else
		utilities.CreateToggle(gVars._TH, false);
	end

	-- Weapon swapping WSWAP is now available for all jobs, previous assumptions have been removed
	if gProfile.settings.DisplayBar[gVars._WSWAP]['init'] ~= nil then				-- Weapon Swap
		utilities.CreateToggle(gVars._WSWAP, gProfile.settings.DisplayBar[gVars._WSWAP]['init']);
	else
		utilities.CreateToggle(gVars._WSWAP, false);
	end

	-- Tanking is now available to all jobs. In most cases jobs that people don't assume tank
	-- will never touch the setting, but the Player can toggle tanking if that's appropriate.
	if gProfile.settings.DisplayBar[gVars._TANK]['init'] ~= nil then				-- Tanking
		utilities.CreateToggle(gVars._TANK, gProfile.settings.DisplayBar[gVars._TANK]['init']);
	else
		utilities.CreateToggle(gVars._TANK, false);
	end

	-- If data is still downloading, then job specific variables cannot be set since there's no
	-- hint as to what job the player is playing unless we explicitly store the job type in the
	-- job file. (That's a fine answer for the main job, but impractical for the sub job.)
	if not (player.MainJob == nil or player.MainJob == 'NON') then
		-- Magic Accuracy (Macc)
		if string.find(gVars._sMagicJobs,player.MainJob) ~= nil or string.find(gVars._sMagicJobs,player.SubJob) ~= nil then
			if gProfile.settings.DisplayBar[gVars._MACC]['init'] ~= nil then				-- Macc
				utilities.CreateToggle(gVars._MACC, gProfile.settings.DisplayBar[gVars._MACC]['init']);
			else
				utilities.CreateToggle(gVars._MACC, false);
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
			if gProfile.settings.DisplayBar[gVars._AJUG] ~= nil and gProfile.settings.DisplayBar[gVars._AJUG]['init'] ~= nil then
				utilities.CreateToggle(gVars._AJUG, gProfile.settings.DisplayBar[gVars._AJUG]['init']);
			else
				utilities.CreateToggle(gVars._AJUG, true);
			end

			-- Create the cycle
			utilities.CreateCycle(gVars._DB, {[1] = gVars._sDB_NORM, [2] = gVars._sDB_BPP, [3] = gVars._sDB_WSS});

			-- and then check for an initial setting. Like AJug this is an optional setting
			if gProfile.settings.DisplayBar[gVars._DB] ~= nil and gProfile.settings.DisplayBar[gVars._DB]['init'] ~= nil then
				utilities.fSetCycle(gVars._DB, gProfile.settings.DisplayBar[gVars._DB]['init']);
			else
				utilities.fSetCycle(gVars._DB, gVars._sDB_NORM);
			end
		end

		-- BRD main only, Instrument indicates what default type of instrument should be equipped,
		-- Horn or String
		if player.MainJob == 'BRD' then
			utilities.CreateCycle(gVars._INSTRUMENT, {[1] = gVars._HORN, [2] = gVars._STRING});

			if gProfile.settings.DisplayBar[gVars._INSTRUMENT] ~= nil and gProfile.settings.DisplayBar[gVars._INSTRUMENT]['init'] ~= nil then
				utilities.fSetCycle(gVars._INSTRUMENT, gProfile.settings.DisplayBar[gVars._INSTRUMENT]['init']);
			else
				utilities.fSetCycle(gVars._INSTRUMENT, gVars._HORN);
			end
		end

		-- SMN only, sBP indicates if the a message should be printed in the party chat when
		-- the pet does an offensive blood pact.
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			if gProfile.settings.DisplayBar[gVars._SBP] ~= nil and gProfile.settings.DisplayBar[gVars._SBP]['init'] ~= nil then
				utilities.CreateToggle(gVars._SBP, gProfile.settings.DisplayBar[gVars._SBP]['init']);
			else
				utilities.CreateToggle(gVars._SBP, true);
			end

			utilities.CreateCycle(gVars._MODE, {[1] = gVars._MODE_PERPETUATION, [2] = gVars._MODE_ATTACK, [3] = gVars._MODE_ENMITY_MINUS});
			if gProfile.settings.DisplayBar[gVars._MODE]['init'] ~= nil then
				utilities.fSetCycle(gVars._MODE, gProfile.settings.DisplayBar[gVars._MODE]['init']);
			else
				utilities.fSetCycle(gVars._MODE, gVars._MODE_PERPETUATION);
			end
		end

		-- THF only, SS indicates that when the player steals, a message should be displayed.
		-- This is used to coordinate thieve's stealing in activities like Dynamis.
		if player.MainJob == 'THF' or player.SubJob == 'THF' then
			if gProfile.settings.DisplayBar[gVars._SS] ~= nil and gProfile.settings.DisplayBar[gVars._SS]['init'] ~= nil then
				utilities.CreateToggle(gVars._SS, gProfile.settings.DisplayBar[gVars._SS]['init']);
			else
				utilities.CreateToggle(gVars._SS, false);
			end
		end
	else
		print(chat.message('Warning: Unable to create job-specific variables while data downloading hasn\'t finished. Run /rv when no longer the case'));
	end

	-- General cycles: Damage Taken and Region
	utilities.CreateCycle(gVars._DT, {[1] = gVars._DT_OFF, [2] = gVars._DT_PHY, [3] = gVars._DT_MAG, [4] = gVars._DT_BRE});
	if gProfile.settings.DisplayBar[gVars._DT] ~= nil and gProfile.settings.DisplayBar[gVars._DT]['init'] ~= nil then
		utilities.fSetCycle(gVars._DT, gProfile.settings.DisplayBar[gVars._DT]['init']);
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
end		-- crossjobs.SetVariables

--[[
	ProcessAccuracy performs the task requested dealing with Accuracy or Ranged Accuracy.

	Pararameter
		args		Passed in list of args from the command line
--]]

function ProcessAccuracy(args)
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

	if args[2] ~= nil and args[2] == '?' then
		print(' ');
		if string.find(gVars._Progressive_ACC..','..gVars._Progressive_TACC,tmp) ~= nil then
			print(chat.message(string.format('Info: Accuracy at stage: %d',gear.fGetAccStage(gVars._Progressive_ACC,'CUR'))));
			if bTank == true then
				print(chat.message(string.format('Info: Tank Accuracy at stage: %d',gear.fGetAccStage(gVars._Progressive_TACC,'CUR'))));
			end
		else
			print(chat.message(string.format('Info: Ranged Accuracy at stage: %d',gear.fGetAccStage(gVars._Progressive_RACC,'CUR'))));
			if bTank == true then
				print(chat.message(string.format('Info: Tank Ranged Accuracy at stage: %d',gear.fGetAccStage(gVars._Progressive_TRACC,'CUR'))));
			end
			return;
		end
	elseif args[2] ~= nil then
		narg = tonumber(args[2]);
		if narg < 0 or narg > gear.fGetAccStage(tmp,'MAX') then
			print(chat.message('Warning: Invalid stage. Number must be between 0 and ' .. tostring(gear.fGetAccStage(tmp,'MAX'))));
			return;
		else
			num = narg;
		end
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
		if tmp == gVars._Progressive_ACC or tmp == gVars._Progressive_TACC then
			print(chat.message('Info: Accuracy has been turned off'));
		else
			print(chat.message('Info: Ranged Accuracy has been turned off'));
		end
	else
		displaybar.SetAccCur(tmp,num);
		print(chat.message(string.format('Info: %s stage set to %d',tmp,num)));
	end
end		-- ProcessAccuracy

--[[
	ProcessAjug enables/disables whether automatic pet jug occurs, BST only
--]]

function ProcessAjug()
	local player = gData.GetPlayer();

	if player.MainJob == 'BST' then
		utilities.AdvanceToggle(gVars._AJUG);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: AJug is ' .. tostring(utilities.fGetToggle(gVars._AJUG))));
		end
	else
		print(chat.message('Info: /AJug is only available to beastmasters'));
	end
end		-- ProcessAjug

--[[
	ProcessCap enables/disables an artificial level cap
--]]

function ProcessCap(s);
	local player = gData.GetPlayer();
	local ss = tonumber(s);

	if s ~= nil then
		if ss < player.MainJobSync and ss >= 0 then
			gProfile.settings.PlayerCappedLevel = ss;
			if gProfile.settings.bConfirmation == true then
				print(chat.message(string.format('Info: Player gear level capped at level %d',gProfile.settings.PlayerCappedLevel)));
			end
		else
			print(chat.message('Info: Invalid level cap specified: ' .. tostring(s)));
		end
	else
		gProfile.settings.PlayerCappedLevel = 0;
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Player gear level cap disabled'));
		end
	end
end		-- ProcessCap

--[[
	ProcessCC enables/disables the indicated custom conditional or lists the commands
--]]

function ProcessCC(s)

	if s == 'cc' then
		reporting.DisplayCC();
	else
		local us = string.upper(s);
		utilities.AdvanceToggle(us);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: ' .. us .. ' is ' .. tostring(utilities.fGetToggle(us))));
		end
	end
end		-- ProcessCC

--[[
	ProcessDB sets the debuff indicator to the appropriate value
--]]

function ProcessDB(s)
	local player = gData.GetPlayer();

	if player.MainJob == 'BST' then
		if s ~= nil then
			s = string.upper(s);
			if string.find(gVars._sDB_Debuffs,s) ~= nil then
				utilities.fSetCycle(gVars._DB,s);
			end
		else
			utilities.AdvanceCycle(gVars._DB);
		end
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: DB is set to ' .. utilities.fGetCycle(us)));
		end
	else
		print(chat.message('Info: /DB is only available to beastmasters and not /BST'));
	end
end		-- ProcessDB

--[[
	ProcessDT either advances /DT setting or assigns the setting directly
--]]

function ProcessDT(s)

	if s == nil then
		utilities.AdvanceCycle(gVars._DT);
	else
		local cType = string.upper(string.sub(s,1,1));
		if  cType == gVars._DT_M then
			utilities.fSetCycle(gVars._DT,gVars._DT_MAG);
		elseif cType == gVars._DT_B then
			utilities.fSetCycle(gVars._DT,gVars._DT_BRE);
		elseif cType == gVars._DT_P then
			utilities.fSetCycle(gVars._DT,gVars._DT_PHY);
		else
			utilities.fSetCycle(gVars._DT,gVars._DT_OFF);
		end
	end
	if gProfile.settings.bConfirmation == true then
		print(chat.message('Info: DT is set to ' .. utilities.fGetCycle(gVars._DT)));
	end
end		-- ProcessDT

--[[
	ProcessHornString sets which type of bard instrument should be the default
--]]

function ProcessHornString(s)
	local player = gData.GetPlayer();

	if player.MainJob == 'BRD' then
		if s == 'horn' then
			utilities.fSetCycle(gVars._INSTRUMENT,gVars._HORN);
		else
			utilities.fSetCycle(gVars._INSTRUMENT,gVars._STRING);
		end
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Instrument is set to ' .. utilities.fGetCycle(gVars._INSTRUMENT)));
		end
	else
		print(chat.message('Info: Only bards use this command. Ignoring.'));
	end
end		-- ProcessHornString

--[[
	ProcessMACC enables/disables magic accuracy gear when casting magic
--]]

function ProcessMACC()
	local player = gData.GetPlayer();

	if string.find(gVars._sMagicjobs,player.MainJob) ~= nil or
	   string.find(gVars._sMagicjobs,player.SubJob) ~= nil then
		utilities.AdvanceToggle(gVars._MACC);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: MAcc is set to ' .. tostring(utilities.fGetToggle(gVars._MACC))));
		end
	else
		print(chat.message('Info: Your job does not need magic accuracy'));
	end
end		-- ProcessMACC

--[[
	ProcessMode indicates whether perpetuation cost or attack power should be emphasized, SMN only
--]]

function ProcessMode(args)
	local player = gData.GetPlayer();

	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		if args[2] ~= nil then
			local u2 = string.upper(args[2]);

			if string.len(args[2]) >= 1 then
				u2 = string.sub(u2,1,1);
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
		else
			utilities.AdvanceCycle(gVars._MODE);
		end

		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Mode is set to ' .. utilities.fGetCycle(gVars._MODE)));
		end
	else
		print(chat.message('Info: /Mode is only available to summoners'));
	end
end		-- ProcessMode

--[[
	ProcessSBP enables/disables whether blood pacts should display a party message when enacted, SMN only
--]]

function ProcessSBP()
	local player = gData.GetPlayer();

	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		utilities.AdvanceToggle(gVars._SBP);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: sBP is set to ' .. tostring(utilities.fGetToggle(gVars._SBP))));
		end
	else
		print(chat.message('Info: /sBP is only available to summoners'));
	end
end		-- ProcessSBP

--[[
	ProcessSS enables/disables whether attempted steals should be announced, THF only
--]]

function ProcessSS()
	local player = gData.GetPlayer();

	if player.MainJob == 'THF' or player.SubJob == 'THF' then
		utilities.AdvanceToggle(gVars._SS);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: SS is set to ' .. tostring(utilities.fGetToggle(gVars._SS))));
		end
	else
		print(chat.message('Info: /SS is only available to thieves'));
	end
end		-- ProcessSS

--[[
	ProcessSW will equip the starting weapons. This is a method to force the correct
	weapons to be equipped
--]]

function ProcessSW()
	local player = gData.GetPlayer();

	utilities.ClearSet(crossjobs.Sets.CurrentGear);
	gear.MoveToDynamicGS(gProfile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,false,'Start_Weapons');
	gear.EquipTheGear(crossjobs.sets.CurrentGear,false,false);
end		-- ProcessSW

--[[
	ProcessWSDISTANCE either sets a new weapon skil distance or turns the check on or off
--]]

function ProcessWSDISTANCE(args)

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
	utilities.ClearSet(crossjobs.sets.CurrentGear);

	-- Now, process the commands. They're listed in alphabetical order
	if args[1] == '911' then
		-- /911, SMN Only. Emmergency call to summon an appropriate spirit
		pets.Call911();
	elseif string.find('acc,racc',args[1]) ~= nil then
		-- /ACC or /RACC, sets the level for the accuracy/ranged accuracy
		ProcessAccuracy(args);
	elseif args[1] == 'ajug' then
		-- /AJUG, BST only, Turns on/off Automatic Pet Jug assignment is enabled
		ProcessAjug();
	elseif args[1] == 'cap' then
		-- /CAP, Sets an artificial level cap
		ProcessCap(args[2]);
	elseif string.find(args[1],'cc') ~= nil then
		-- /CC#, Either displays Custom conditionals or turns them on/off
		ProcessCC(args[1]);
	elseif args[1] == 'dt' then
		-- /DT, Indicates the type of damage taken gear that will be equipped if enabled
		ProcessDT(args[2]);
	elseif string.find('equipit,ei',args[1]) ~= nil then
		-- /EQUIPIT or /EI, equip specified item or coded item
		gear.EquipItem(args);
	elseif args[1] == 'eva' then
		-- /EVA, Turns on/off whether evasion gear should be equipped
		utilities.AdvanceToggle(gVars._EVASION);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Evasion is set to ' .. tostring(utilities.fGetToggle(gVars._EVASION))));
		end
	elseif args[1] == 'gc' then
		-- /GC, Invoke the Gear Check command
		gear.ProcessGC(args);
	elseif string.find('gearset,gs',args[1]) ~= nil then
		-- /GS, equips the specified gear and locks slots appropriately
		gear.ProcessGS(args);
	elseif args[1] == 'gswap' then
		-- /GSWAP, turns gear swapping on/off
		utilities.AdvanceToggle(gVars._GSWAP);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: GSwap is set to ' .. tostring(utilities.fGetToggle(gVars._GSWAP))));
		end
	elseif string.find('horn,string',args[1]) ~= nil then
		-- /STRING or /HORN, BRD only, indicates type of default instrument
		ProcessHornString(args[1]);
	elseif args[1] == 'idle' then
		-- /IDLE, Turns on/off whether idle gear should be equipped during idle
		utilities.AdvanceToggle(gVars._IDLE);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Idle is set to ' .. tostring(utilities.fGetToggle(gVars._IDLE))));
		end
	elseif args[1] == 'kite' then
		-- /KITE, Turns on/off whether movement gear is equipped
		utilities.AdvanceToggle(gVars._KITE);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Kite is set to ' .. tostring(utilities.fGetToggle(gVars._KITE))));
		end
	elseif string.find('lock,unlock',args[1]) ~= nil then
		-- /LOCK, Lock/unlock gear slots
		locks.ProcessLocks(args);
	elseif args[1] == 'macc' then
		-- /MACC, Turns on/off whether magic accuracy gear is equipped when casting magic
		ProcessMACC();
	elseif args[1] == 'man' then
		-- /MAN, Shows the help system, either a list of valid commands or details on a specific command
		help.ShowHelp(args[2]);
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
	elseif args[1] == 'ptt' then
		-- /PTT, proof of concept command, Displays distance from Pet To Target
		pets.ptt();
	elseif args[1] == 'pull' then
		-- /PULL, pull the target
		utilities.PullTarget();
	elseif args[1] == 'rc' then
		-- /RC, Display region controls
		reporting.RegionControlDisplay(args);
	elseif args[1] == 'rv' then		-- Refresh variables
		-- /RV, refresh the starting variables
		crossjobs.SetVariables();
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Variables are refreshed'));
		end
	elseif args[1] == 'sbp' then
		-- /SBP, Turns on/off whether the SMN blood pact message is shown
		ProcessSBP();
	elseif args[1] == 'showit' then
		-- /SHOWIT, Shows debug info
		reporting.DB_ShowIt();
	elseif args[1] == 'smg' then
		-- /SMG, Show my gear
		reporting.ProcessSMG(args);
	elseif args[1] == 'spf' then
		-- /SPF, Turns on/off whether Show Pull feedback should be displayed
		utilities.AdvanceToggle(gVars._SPF);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: SPF is set to ' .. tostring(utilities.fGetToggle(gVars._SPF))));
		end
	elseif args[1] == 'ss' then
		-- /SS, THF only, enables/disables whether attempted steals should be announced
		ProcessSS();
	elseif args[1] == 'sw' then
		-- /sw, Equips the start weapons
		ProcessSW();
	elseif args[1] == 'tank' then
		-- /TANK, Turns on/off whether tanking gear is equipped
		utilities.AdvanceToggle(gVars._TANK);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: Tank is set to ' .. tostring(utilities.fGetToggle(gVars._TANK))));
		end
	elseif args[1] == 't1' then
		-- /T1, debug command. Used to test code
		crossjobs.t1(args);
	elseif args[1] == 'th' then
		-- /TH, Turns on/off whether TH gear should be equipped
		utilities.AdvanceToggle(gVars._TH);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: TH is set to ' .. tostring(utilities.fGetToggle(gVars._TH))));
		end
	elseif args[1] == 'ver' then
		-- /VER, Display version/change log
		reporting.DisplayVersion();
	elseif args[1] == 'wsdistance' then
		-- /WSDISTANCE, Turns on/off the check for weapons skill distance or sets the distance
		ProcessWSDISTANCE(args);
	elseif args[1] == 'wswap' then
		-- /WSWAP, Turns on/off whether weapon swapping is permitted
		utilities.AdvanceToggle(gVars._WSWAP);
		if gProfile.settings.bConfirmation == true then
			print(chat.message('Info: WSwap is set to ' .. tostring(utilities.fGetToggle(gVars._WSWAP))));
		end
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
	local n,ts;
	local bFound = false;

	-- Store the name of the ammo. This is used when the ammo slot is automatically
	-- populated so that the original ammo can be re-equipped.
	if eq.Ammo ~= nil then
		gProfile.settings.sAmmo = eq.Ammo.Name;
	else
		gProfile.settings.sAmmo = nil;
	end

	-- Clear out the CurrentGear in case of leftovers
	utilities.ClearSet(crossjobs.Sets.CurrentGear);

	if player.MainJob == 'BST' or player.SubJob == 'BST' then
		n = nil;
		if string.match(ability.Name, 'Reward') then
			-- Pet reward. Make sure that pet food already equipped
			if gProfile.settings.sAmmo == nil or string.find(string.lower(gProfile.settings.sAmmo),'pet f') == nil then		-- something else equipped
				gProfile.settings.bAmmo = pets.PetReward(gProfile.settings.defaultPetFood,'max');
			end
			n = 'A_Reward';
		elseif string.find('Sic,Ready',ability.Name) ~= nil then
			-- Sic and Ready load the same set
			n = 'A_Sic_Ready';
		elseif string.match(ability.name, 'Call Beast') then
			-- see if there's already a jug in the ammo slot
			if utilities.fGetToggle(gVars._AJUG) == true then
				local current = gData.GetCurrentSet();
				local bValid = false;

				if current['Ammo'] ~= nil or current['Ammo'] ~= '' then
					bValid = pets.fIsValidJugPet(current['Ammo']);
				end

				if bValid == false then
					local x = pets.fWhichJugToEquip();
					if x ~= nil then
						crossjobs.Sets.CurrentGear['Ammo'] = x;
					end
				end
			end
			n = 'A_Call_Beast';
		else
			n = 'A_' .. string.gsub(ability.Name,' ','_');
		end

		if n ~= nil then
			ts = utilities.fGetTableByName(n);
			if ts ~= nil then
				gear.MoveToDynamicGS(ts,crossjobs.Sets.CurrentGear,false,n);
				-- Special case on Charm. Check for "light" staff
				if n == 'A_Charm' then
					local sStave = gear.fCheckForEleGear('staff','light');
					if sStave ~= nil then
						gear.fSwapToStave(sStave,false,crossjobs.Sets.CurrentGear);
					end
				end
				bFound = true;
			end
		end
	-- Check for summoner's blood pact, to load the PreBP
	elseif table.find(pets.SmnBPRageList,ability.Name) ~= nil or
		   table.find(pets.SmnBPWardList,ability.Name) ~= nil then
		gear.MoveToDynamicGS(gProfile.Sets.PreBP,crossjobs.Sets.CurrentGear,false,'PreBP');
		bFound = true;
	end

	if bFound == false then
		if string.find(pets._PetCommands,string.upper(ability.Name)) ~= nil then
			-- Assume it's a pet command
			n = 'PC_' .. string.gsub(ability.Name,' ','_');
		else
			-- Assume it's an ability
			n = 'A_' .. string.gsub(ability.Name,' ','_');
		end

		ts = utilities.fGetTableByName(n);

		if ts ~= nil then
			gear.MoveToDynamicGS(ts,crossjobs.Sets.CurrentGear,false,n);
			bFound = true;
		end
	end

	if bFound == true then
		gear.EquipTheGear(crossjobs.Sets.CurrentGear,false,false);		-- Equip the composited HandleAbility set
	end
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
	gcinclude.ClearSet(sets.CurrentGear);

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
--]]

function crossjobs.fHandleWeaponskill()
	local ws = gData.GetAction();
	local lName = string.lower(ws.Name);
	local sName,sEle,n;
	local t = {};

	-- See if there's a custom gear set defined for this weapon skill
	n = 'WS_' .. string.gsub(lName,' ','_');
	t = utilities.fGetTableByName(n);
	if t ~= nil then
		gear.MoveToDynamicGS(t,gProfile.Sets.CurrentGear,false,lName);
	else
		-- No custom set, look for the stat set
		for i,j in pairs(gVars.tWeaponSkills) do
			if table.find(j,lName) ~= nil then
				sName = 'WS_' .. i;
				t = utilities.fGetTableByName(sName);
				if t ~= nil then
					gear.MoveToDynamicGS(t,gProfile.Sets.CurrentGear,false,sName);
				end
				break;
			end
		end
	end

	-- Now, process the other gearsets that affects weapon skills based on the order
	-- provided by the player
	for _,j in ipairs(gProfile.settings.postGSWeaponSkill) do
		j = string.lower(j);
		if j == 'egorget' and gProfile.settings.EmbedOnly.eGorget == false then
			-- An elemental gorget will add the fTP (at least 10% more damage) to the first hit
			-- of an elemental weapon skill (and many multi-hit weapon skills replicate the fTP
			-- for all the hits.) Also, they give +10 Accuracy to all of the weapon skill's hits
			-- and a 1% chance of not depleting the player's TP after the weapon skill.
			local sGorget,sEle = gear.fCheckForElementalGearByValue('gorget','eleWS',ws.Name);
			if sGorget ~= nil then
				crossjobs.Sets.CurrentGear['Neck'] = sGorget;
			end
		elseif j == 'eobi' and gProfile.settings.EmbedOnly.eObi == false then
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
		elseif j == 'acc' and gProfile.settings.EmbedOnly.Accuracy == false then
			if table.find(gVars.tWeaponSkills['RANGED_AGI'],lname) ~= nil or
				table.find(gVars.tWeaponSkills['RANGED_STRAGI'],lname) ~= nil then
				crossjobs.ProgressiveAccuracy('RAcc');
			else
				crossjobs.ProgressiveAccuracy('Acc');
			end
		end
	end

	-- Certain weapon skills can take advantage of magic attack bonus. Check here and equip gear
	-- appropriately. (Note: even though rMAB is a reference gear set, in this particular instance
	-- it is treated like it is a normal gear set.)
	if string.find('red lotus blade,sanguine blade',lName) ~= nil then
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
end		-- crossjobs.Unload

return crossjobs;
