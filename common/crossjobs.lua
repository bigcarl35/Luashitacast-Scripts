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
			lProcessAccuracy			Process the /acc or /racc commands
			ProgressiveAccuracy			Equips the appropriate accuracy stage
			local SetVariables			Sets runtime displaybar variables
			t1							Test procedure for trying out new ideas

		Functions:
			fHandleWeaponskil			Determines what gear set to equip and does so
			fValidCustomCommand			Determines if passed command is a custom conditional command
--]]

crossjobs.sets = {

--[[
	There are currently eight crafts: alchemy, bonecraft, clothcraft, cooking, goldsmithing, leathercraft,
	smithing, and woodworking. It's possible that a player will have gear for more than one craft. There's
	only one Crafting gear set, so you need to qualify each piece with what type of crafting the piece is
	used for. (Ex: Body = 'Weaver\'s Apron//CR:CLOTH).

	Please note that Crafting sets ignore the /WSWAP setting.
--]]

	['Crafting'] = {
		Group//CR:WW = {					-- Woodworking
			Hands = 'Carpenter\'s gloves',
		},
	},

--[[
	There are seven gathering types: harvesting, excavtion, logging, and mining which are grouped in the H.E.L.M.
	set. The other three types of gathering: digging, clamming and fishing, have their own gear.

	Please note that Gathering sets ignore the /WSWAP setting.
--]]

	['Gathering'] = {
		Group//GA:FISH = {					-- Fishing
			Range = 'Lu Shang\'s F. Rod',
			Ammo  = 'Sinking Minnow',
			Body  = 'Angler\'s Tunica',
			Rings = 'Albatross Ring//CC2',
			Legs  = 'Fisherman\'s Hose',
			Feet  = 'Waders'
		},
		Group//GA:HELM = {					-- H.E.L.M.
			Body  = 'Field Tunica',
			Hands = 'Field Gloves',
			Legs  = 'Field Hose',
			Feet  = 'Field Boots'
		},
		Group//GA:DIG = {					-- Digging
			Head = 'Egg Helm//CC2',
			Body = 'Choc. Jack Coat'
		},
		Group//GA:CLAM = {					-- Clamming
			Body = 'Tarutaru Top +1',
			Legs = 'Taru. Shorts +1'
		}
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

crossjobs.settings = {
	-- You can also set any of these on a per job basis in the job file in the OnLoad function. See my BST job file
	--to see how this is done.
	Messages = false; 	 			-- set to true if you want chat log messages to appear on any /gs command used such as DT, or KITE gear toggles, certain messages will always appear
	WScheck = true; 	 			-- set to false if you don't want to use the WSdistance safety check
	WSdistance = 4.7; 	 			-- default max distance (yalms) to allow non-ranged WS to go off at if the above WScheck is true
	bWSOverride = false; 			-- is the player playing a job where weapon swapping always happens, it is not optional?
	Tolerance = 97;					-- Comparison value %, cut-off for certain comparisons
	DefaultSpellTarget = 't'; 		-- What to use in MaxSpell if no target specified
	DefaultSongTarget = 't';  		-- What to use in MaxSong if no target specified
	--
	RegenGearHPP = 97;				-- default HP% limit
	RefreshGearMPP = 97;			-- default MP% limit
	--
	bAutoStaveSwapping = true;		-- indicates if elemental stave swapping should occur automatically
	--
	bMinBasetime = 15;				-- minimum wait before reminding player to run /gc
	bMaxBasetime = 300;				-- once reminder shown, switch to every 5 minutes
	bGCReminder = false;			-- Has GC reminder been displayed yet
};

-- List of all supported commands
crossjobs.AliasList = {
	'911','acc','ajug','cc','db','dt','ei','equipit','eva','gc','gcmessages','gearset','gs','gswap','horn','idle','kite',
	'lock','macc','man','maxsong','maxspell','petfood','ptt','pull','racc','rc','rv','sbp','showit','smg','spf','ss',
	'string','sw','tank','th','unlock','val','ver','wsdistance','wswap','t1'
};

-- Indicates what nation your character is from, -1 is unassigned
crossjobs.OwnNation = -1;
crossjobs.Sets = crossjobs.sets;
crossjobs.WeaponTypes = {};
crossjobs.ZoneList = {};
crossjobs.CurrentZone = 0;

-- Define constants for DT so typos aren't made
local DT.OFF = 'Off';
local DT.PHY = 'Physical';
local DT.MAG = 'Magical';
local DT.BRE = 'Breath';

-- define constants for Instrument so typos aren't made
local _HORN = 'Horn';
local _STRING = 'String';

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
		displaybar.RegionDisplay();
		e.blocked = false;
	end
end);

--[[
	t1 is a test procedure, used in trying out new things. It is not intended for players to use

	Pararameter
		args		list of passed in arguments
--]]

function crossjobs.t1(args)

	for i,j in pairs(gVars.tGearsetDetails['rAccuracy']['Head']) do
		print(j['display'],j[2],#j['iref']);
		for ii,jj in pairs(gVars.tGearsetDetails['rAccuracy']['Head']['iref'] do
			print(jj['id'],jj['item_ptr']['id'],jj['item_ptr']['name']);
		end
	end
end		-- crossjobs.t1

--[[
	SetVariables defines run settings for luashitacast

	** revisit **, re: cycles
--]]

function SetVariables()
	local player = gData.GetPlayer();

	-- General toggles
	utilities.CreateToggle(gVars._GSWAP, true);		-- Gear Swap
	utilities.CreateToggle(gVars._KITE, false);		-- Kiting
	utilities.CreateToggle(gVars._EVASION, false);	-- Evasion
	utilities.CreateToggle(gVars._IDLE, true);		-- Should Default set equip when idling
	utilities.CreateToggle(gVars._SPF, true);		-- Show Pull Feedback
	utilities.CreateToggle(gVars._RARE, true);		-- Include rare items in pool

	-- Job specific toggles

	-- Weapon swapping WSWAP. SMN and BLM always can weaponswap. WHM, RDM, and BRD you
	-- want to assume WSWAP is enabled. Every other job defaults to false
	if string.find('SMN,BLM',player.MainJob) == nil then
		utilities.CreateToggle(gVars._WSWAP,(string.find('WHM,RDM,BRD',player.MainJob) ~= nil));
	end

	-- Tanking: PLD, NIN, and RUN default to TANK enabled. DRK, WAR, RDM, and BLU
	-- default to TANK being disabled.
	if string.find(gVars._Tankjobs,player.MainJob) ~= nil then
		utilities.CreateToggle(gVars._TANK, (string.find('PLD,NIN,RUN',player.MainJob) ~= nil));
	end

	-- Magic Accuracy (Macc)
	if string.find(gVars._sMagicjobs,player.MainJob) ~= nil or
		 string.find(gVars._sMagicjobs,player.SubJob) ~= nil then
		utilities.CreateToggle(gVars._MACC, false);
	end

	-- BST only, AJug is an automated system to equip jug pets. DB is a setting to determine the
	-- type of debuff wanted from the Jackcoat. Either BPP or WSS (blind,poison,parallize) or
	-- (weighted,silence,slow). Please note that the Beast Jackoat +1 (AF +1) can dispel all
	-- six debuffs from the pet whereas Beast Jackcoat does BPP and Monster Jackcoat/Monster
	-- Jackcoat+1 only dispels WSS.
	if player.MainJob == 'BST' then
		utilities.CreateToggle(gVars._AJUG, true);
		if utilities.fGetCycle(gVars._DB) == 'Unknown' then
			utilities.CreateCycle(gVars._DB, {[1] = 'Norm', [2] = 'BPP', [3] = 'WSS'});
		end
	end

	-- BRD main only, Instrument indicates what default type of instrument should be equipped,
	-- Horn or String
	if player.MainJob == 'BRD' then
		utilities.CreateCycle(gVars._INSTRUMENT, {[1] = _HORN, [2] = _STRING});
	end

	-- SMN only, sBP indicates if the a message should be printed in the party chat when
	-- the pet does an offensive blood pact.
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		utilities.CreateToggle(gVars._SBP, true);
		utilities.CreateCycle(gVars._MODE, {[1] = 'PERP', [2] = 'ATTK'});
	end

	-- THF only, SS indicates that when the player steals, a message should be displayed.
	-- This is used to coordinate thieve's stealing in activities like Dynamis.
	if player.MainJob == 'THF' or player.SubJob == 'THF' then
		-- While it's true that all jobs can now equip TH gear through a 'Tinfoil Hat',
		-- only THF/ or /THF will have the TH command available. Others will have to
		-- use /gs TH to load TH gear.
		utilities.CreateToggle(gVars._TH,(player.MainJob == 'THF' or player.SubJob == 'THF'));
		utilities.CreateToggle(gVars._SS, false);
	end

	-- General cycles: Damage Taken and Region
	utilities.CreateCycle(gVars._DT, {[1] = DT.OFF, [2] = DT.PHY, [3] = DT.MAG, [4] = DT.BRE});
	utilities.CreateCycle(gVars._REGION, {[1] = 'Owned', [2] = 'Not Owned', [3] = 'N/A'});

	-- Lastly, make sure all custom conditionals are defined
	if gProfile.CustomConditionals ~= nil then
		for _,j in ipairs(gProfile.CustomConditionals) do
			j['code'] = string.upper(j['code']);
			utilities.CreateToggle(j['code'],j['init']);
		end
	end
end		-- SetVariables

--[[
	lProcessAccuracy performs the task requested dealing with Accuracy or Ranged Accuracy.

	Pararameter
		args		Passed in list of args from the command line
					[1] -- Acc or RAcc
--]]

function lProcessAccuracy(args)
	local bTank = utilities.fGetToggle(gVars._TANK);
	local tmp,narg;
	local num = 0;		-- 0 means turn off that type of accuracy

	if args[1] == 'acc' then
		if bTank == true then
			tmp = 'TAcc';
		else
			tmp = 'Acc';
		end
	elseif args[1] == 'racc' then
		if bTank == true then
			tmp = 'TRAcc';
		else
			tmp = 'RAcc';
		end
	end

	if args[2] ~= nil and args[2] == '?' then
		print(' ');
		if string.find('Acc,TAcc',tmp) ~= nil then
			print(chat.message(string.format('Info: Accuracy at stage: %d',gear.fGetAccStage('Acc','CUR'))));
			if bTank == true then
				print(chat.message(string.format('Info: Tank Accuracy at stage: %d',gear.fGetAccStage('TAcc','CUR'))));
			end
		else
			print(chat.message(string.format('Info: Ranged Accuracy at stage: %d',gear.fGetAccStage('RAcc','CUR'))));
			if bTank == true then
				print(chat.message(string.format('Info: Tank Ranged Accuracy at stage: %d',gear.fGetAccStage('TRAcc','CUR'))));
			end
			return;
		end
	else
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
		if string.find('Acc,Racc',tmp) ~= nil then
			tmp = 'T' .. tmp;				-- Add a 'T' to the beginning of tmp
		else
			tmp - string.sub(tmp,2,-1);		-- Remove the 'T' from the beginning of tmp
		end
		displaybar.SetAccCur(tmp,0);
		if tmp == 'Acc' or tmp == 'TAcc' then
			print(chat.message('Info: Accuracy has been turned off'));
		else
			print(chat.message('Info: Ranged Accuracy has been turned off'));
		end
	else
		displaybar.SetAccCur(tmp,num);
		print(chat.message(string.format('Info: %s stage set to %d',tmp,num)));
	end
end		-- lProcessAccuracy

--[[
	HandleCommands processes any commands typed into luashitacast as defined in this file

	Pararameters
		args		List of arguments passed with the command
--]]

function crossjobs.HandleCommands(args)
	local player = gData.GetPlayer();
	local bTank = utilities.fGetToggle(gVars._TANK);
	local sList, sKey, sSet;

	-- Make sure it's a recognised command
	if not crossjobs.AliasList:contains(args[1]) or utilities.fValidCustomCommand(args[1]) == true then
		print(chat.message('Info: Unknown command specified: ' .. args[1]));
		return;
	end

	-- Clear out the local copy of current gear
	utilities.ClearSet(crossjobs.sets.CurrentGear);
	args[1] = string.lower(args[1]);

	-- Now process the commands
	if (args[1] == 'gswap') then			-- turns gear swapping on or off
		-- /GSWAP
		utilities.AdvanceToggle(gVars._GSWAP);
	elseif args[1] == 't1' then				-- This is a test invoker
		-- /T1
		crossjobs.t1(args);
	elseif args[1] == '911' then
		pets.Call911();
	elseif args[1] == 'gc' then				-- Invoke the Gear Check command
		-- /GC [list]
		if args[2] ~= nil and string.lower(args[2]) == 'list' then
			reporting.GearCheckList();
		else
			gear.GearCheck();
			gVars.bGC = true;
		end
	elseif args[1] == 'gcmessages' then		-- turns feedback on/off for all commands
		-- /GCMESSAGES
		crossjobs.settings.Messages = not crossjobs.settings.Messages;
		if crossjobs.settings.Messages == true then
			print(chat.message('Info: Chat messages are enabled'));
		else
			print(chat.message('Info: Chat messages are disabled'));
		end
	elseif (args[1] == 'wsdistance') then	-- Turns on/off the check for weapons skill distance or sets the distance
		-- /WSDISTANCE [#]
		local i = tonumber(args[2]);
		if i ~= nil then
			crossjobs.settings.WScheck = true;
			crossjobs.settings.WSdistance = i
			print(chat.message('Info: WS Distance is now on and set to ' .. tostring(crossjobs.settings.WSdistance)));
		else
			crossjobs.settings.WScheck = not crossjobs.settings.WScheck;
			print(chat.message('Info: WS distance check is now set to ' .. tostring(crossjobs.settings.WScheck)));
		end
	elseif (args[1] == 'cap') then		-- sets an artificial level cap to gearing
		-- /CAP [#]
		if args[2] ~= nil then
			local inum = tonumber(args[2]);
			if inum < player.MainJobSync and inum >= 0 then
				gProfile.settings.PlayerCappedLevel = inum;
			else
				print(chat.message('Info: Invalid level cap specified: ' .. tostring(inum)));
			end
		else
			gProfile.settings.PlayerCappedLevel = 0;
		end
	elseif (string.sub(args[1],1,2) = 'cc') then
		-- Custom conditional
		if args[1] == 'cc' then
			reporting.DisplayCC();
		else
			utilities.AdvanceToggle(string.upper(args[1]));
		end
	elseif (args[1] == 'dt') then		-- Indicates the type of damage taken gear that will be equipped if desired
		-- /DT [M|P|B]
		if #args == 1 then				-- No qualifier, assume next in set
			utilities.AdvanceCycle(gVars._DT);
		else
			local cType = string.upper(string.sub(args[2],1,1));
			if  cType == 'M' then
				utilities.fSetCycle(gVars._DT,DT.MAG);
			elseif cType == 'B' then
				utilities.fSetCycle(gVars._DT,DT.BRE);
			elseif cType == 'P' then
				utilities.fSetCycle(gVars._DT,DT.PHY);
			else
				utilities.fSetCycle(gVars._DT,DT.OFF);
			end
		end
	elseif (args[1] == 'kite') then			-- Turns on/off whether movement gear is equipped
		-- /KITE
		utilities.AdvanceToggle(gVars._KITE);
	elseif (args[1] == 'idle') then			-- Turns on/off whether movement gear is equipped
		-- /IDLE
		utilities.AdvanceToggle(gVars._IDLE);
	elseif (args[1] == 'macc') then			-- Turns on/off whether tanking gear is equipped
		-- /MACC
		if string.find(gVars._sMagicjobs,player.MainJob) ~= nil or
			string.find(gVars._sMagicjobs,player.SubJob) ~= nil then
			utilities.AdvanceToggle(gVars._MACC);
		else
			print(chat.message('Warning: Your job does not need magic accuracy'));
		end
	elseif (args[1] == 'mode') then			-- Turns on/off smn emphasis for gear when pet out
		-- /MODE
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			utilities.AdvanceCycle(gVars._MODE);
		else
			print(chat.message('Warning: /Mode is only available to summoners'));
		end
	elseif (args[1] == 'ptt') then			-- Displays distance from Pet To Target
		-- /PTT
			pets.ptt();
	elseif (args[1] == 'tank') then			-- Turns on/off whether tanking gear is equipped
		-- /TANK
		if string.find(utilities._TankJobList,player.MainJob) ~= nil then
			utilities.AdvanceToggle(gVars._TANK);
		else
			print(chat.message('Warning: Your job does not support the tanking option'));
		end
	elseif (args[1] == 'eva') then			-- Turns on/off whether evasion gear should be equipped
		-- /EVA
		utilities.AdvanceToggle(gVars._EVASION);
	elseif (args[1] == 'wswap') then		-- Turns on/off whether weapon swapping is permitted
		-- /WSWAP
		if crossjobs.settings.bWSOverride == false then
			utilities.AdvanceToggle(gVars._WSWAP);
		else
			print(chat.message('Warning: Weapon swapping always enabled on ' .. player.MainJob));
		end
	elseif (args[1] == 'sbp') then			-- Turns on/off whether the blood pact message is shown
		-- /SBP
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			utilities.AdvanceToggle(gVars._SBP);
		else
			print(chat.message('Warning: /sBP is only available to summoners'));
		end
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automatic Jug assignment enabled
		-- /AJUG
		if player.MainJob == 'BST' then
			utilities.AdvanceToggle(gVars._AJUG);
		else
			print(chat.message('Warning: /AJug is only available to beastmasters'));
		end
	elseif (args[1] == 'th') then			-- Turns on/off whether TH gear should be equipped
		-- /TH
		if player.MainJob == 'THF' and player.SubJob == 'THF' then
			utilities.AdvanceToggle(gVars._TH);
		else
			print(chat.message('Warning: /TH is only available to thieves'));
		end
	elseif (args[1] == 'ss') then			-- Turns on/off whether Show Action feedback should be displayed
		-- /SS
		if player.MainJob == 'THF' or player.SubJob == 'THF' then
			utilities.AdvanceToggle(gVars._SS);
		else
			print(chat.message('Warning: /SS is only available to thieves'));
		end
	elseif (args[1] == 'spf') then			-- Turns on/off whether Show Pull feedback should be displayed
		-- /SPF
		utilities.AdvanceToggle(gVars._SPF);
	elseif (args[1] == 'sw') then
		-- Loads the start weapons
		utilities.ClearSet(crossjobs.Sets.CurrentGear);
		gear.MoveToDynamicGS(profile.Sets.Start_Weapons,crossjobs.Sets.CurrentGear,false,'Start_Weapons');
		gear.EquipTheGear(crossjobs.sets.CurrentGear,false);
	elseif (args[1] == 'db') then			-- Sets DeBuff (for BST) to the appropriate setting
		-- /DB [BPP|WSS]
		if player.MainJob == 'BST' then
			if args[2] ~= nil  and string.find('BPP,WSS',args[2]) ~= nil then
				utilities.fSetCycle(gVars._DB,string.upper(args[2]));
			else
				utilities.AdvanceCycle(gVars._DB);
			end
		else
			print(chat.message('Warning: Your job cannot use /DB command'));
		end
	elseif (string.find('acc,racc',args[1]) ~= nil) then
		-- /ACC [#]
		-- Sets the level for the accuracy/ranged accuracy
		lProcessAccuracy(args);
	elseif (args[1] == 'lock' or args[1] == 'unlock') then		-- Lock/unlock gear slots
		-- /LOCK [#|slot name,...] or /UNLOCK [#,slot name, ...]
		locks.ProcessLocks(args);
	elseif (args[1] == 'rc') then		-- Display region controls
		-- /RC
		reporting.RegionControlDisplay();
	elseif (args[1] == 'rv') then		-- Refresh variables
		-- /RV
		SetVariables();					-- No need for a special routine, just set the variables again
	elseif (args[1] == 'pull') then		-- Pull the target
		-- /PULL
		utilities.PullTarget();
	elseif (args[1] == 'showit') then	-- Shows debug info for specified type
		-- /SHOWIT
		reporting.DB_ShowIt();
	elseif (args[1] == 'smg') then		-- Show My Gear
		-- /SMG [gs=|slot=]
		reporting.ProcessSMG(args);
	elseif (string.find('gearset,gs') ~= nil) then	-- Forces a gear set to be loaded and turns GSWAP off
		-- /GS name
		gear.ProcessGS(args);
	elseif (string.find('horn,string',args[1]) ~= nil) then		-- String or Horn instrument
		-- /STRING or /HORN
		if player.MainJob == 'BRD' then
			if args[1] == 'horn' then
				utilities.fSetCycle(gVars._INSTRUMENT,_HORN);
			else
				utilities.fSetCycle(gVars._INSTRUMENT,_STRING);
			end
		else
			print(chat.message('Warning: Your job does not support that command. Ignoring.'));
		end

	elseif (args[1] == 'maxspell') then			-- Determines highest level spell to cast
		-- MAXSPELL root
		magic.MaxCast(args[2],true,args[3],true);
	elseif (args[1] == 'maxsong') then			-- Determines highest level song to cast
		-- MAXSONG root
		magic.MaxCast(args[2],false,args[3],true);
	elseif (string.find('equipit,ei',args[1]) ~= nil) then	-- Equip specified item
		-- /EQUIPIT keyword or /EI keyword
		gear.EquipItem(args);
	elseif args[1] == 'ver' then				-- Display version/change log
		-- /VER
		reporting.DisplayVersion();
	end

	if crossjobs.settings.Messages then
		utilities.Message(toggle, status)
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
	crossjobs.ClearSet(crossjobs.Sets.CurrentGear);

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
				bFound = true;
			end
		end
	-- Check for summoner's blood pact, to load the PreBP
	elseif string.find(pets.SmnBPRageList,ability.Name) ~= nil or
		   string.find(pets.SmnBPWardList,ability.Name) ~= nil then
		gear.MoveToDynamicGS(gProfile.Sets.PreBP,crossjobs.Sets.CurrentGear,false,'PreBP');
		bFound = true;
	end

	if bFound == false then
		if string.find(pets._PetCommands,string.upper(ability.Name)) ~= nil then
			-- Assume it's an ability
			n = 'A_' .. string.gsub(ability.Name,' ','_');
		else
			-- Assume it's a pet command
			n = 'PC_' .. string.gsub(ability.Name,' ','_');
		end

		ts = utilities.fGetTableByName(n);

		if ts ~= nil then
			gear.MoveToDynamicGS(ts,crossjobs.Sets.CurrentGear,false,n);
			bFound = true;
		end
	end

	if bFound == true then
		gear.EquipTheGear(crossjobs.Sets.CurrentGear);		-- Equip the composited HandleAbility set
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
	crossjobs.ClearSet(sets.CurrentGear);

	if string.match(item.Name, 'Silent Oil') then
		gear.MoveToDynamicGS(gProfile.Sets.Sneak,crossjobs.Sets.CurrentGear,false,'Sneak');
		bShow = true;
	elseif string.match(item.Name, 'Prism Powder') then
		gear.MoveToDynamicGS(gProfile.Sets.Invisible,crossjobs.Sets.CurrentGear,false,'Invisible');
		bShow = true;
	end

	if bShow == true then
		gear.EquipTheGear(crossjobs.Sets.CurrentGear);
	end
end		-- crossjobs.HandleItem

--[[
	HandlePreshot is the general use version of said routine that coordinates all preshot item gear
	equipping.	It was originally part of each job file.
--]]

function crossjobs.HandlePreshot()

	-- Clear out the CurrentGear in case of leftovers
	crossjobs.ClearSet(crossjobs.Sets.CurrentGear);

	gear.MoveToDynamicGS(gProfile.Sets.Preshot,crossjobs.Sets.CurrentGear,false,'PreShot');
	gear.EquipTheGear(crossjobs.Sets.CurrentGear);
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

	gear.EquipTheGear(crossjobs.Sets.CurrentGear);
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
	gear.EquipTheGear(crossjobs.Sets.CurrentGear);
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
		if j == 'egorget' and gProfile.settings.EmbedOnlyeGorget == false then
			-- An elemental gorget will add the fTP (at least 10% more damage) to the first hit
			-- of an elemental weapon skill (and many multi-hit weapon skills replicate the fTP
			-- for all the hits.) Also, they give +10 Accuracy to all of the weapon skill's hits
			-- and a 1% chance of not depleting the player's TP after the weapon skill.
			local sGorget,sEle = gear.fCheckForElementalGearByValue('gorget','eleWS',ws.Name);
			if sGorget ~= nil then
				crossjobs.Sets.CurrentGear['Neck'] = sGorget;
			end
		elseif j == 'eobi' and gProfile.settings.EmbedOnlyeObi == false then
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
		elseif j == 'acc' and gProfile.settings.EmbedOnlyAccuracy == false then
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
	local bTank = utilities.GetToggle('Tank');
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
