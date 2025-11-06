local crossjobs = {};

local utilities = require('common.utilities');
local reporting = require('common.reporting');
local displaybar = require('common.displaybar');
local locks = require('common.locks');
local gear = require('common.gear');
local pets = require('common.pets');
local magic = require('common.magic');

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
			HandleWeaponskill			Coordinate all Weapon Skill gear equipping
			packet_in_callback1			Dissects packed for region control info
			ProcessAccuracy				Process the /acc or /racc commands
			local RefreshVariables		Recreates variable definitions
			local SetVariables			Sets runtime displaybar variables
			t1							Test procedure for trying out new ideas

		Functions:
			fHandleWeaponskil			Determines what gear set to equip and does so
--]]

local crossjobs.version = {
	['author']	= 'Paiine',
	['name']	= 'Luashitacast (Boxcar)',
	['version']	= '3.0.alpha'
};

crossjobs.sets = {

--[[
	There are currently eight crafts: alchemy, bonecraft, clothcraft, cooking, goldsmithing, leathercraft,
	smithing, and woodworking. It's possible that a player will have gear for more than one craft. There's
	only one Crafting gear set, so you need to qualify each piece with what type of crafting the piece is
	used for. (Ex: Body = 'Weaver\'s Apron//CR:CLOTH).

	Please note that Crafting sets ignore the /WSWAP setting.
--]]

	['Crafting'] = {
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

-- Holding variable for all of the messages that should only be displayed once
crossjobs.GearWarnings = nil;

-- The following is used in the /GC nag reminder
crossjobs.basetime = os.time();

-- These two variables are used to store the invoked type of craft/gather type
crossjobs.Craft=nil;
crossjobs.Gather=nil;

-- The following is used to track regional control. Listed is a region, who has
-- conquest control, and what zone id's are associated with the region. This
-- structure is populated programmatically. 1 - San d'Orian, 2 - Bastokian, 3 -
-- Windurstian, 0 - not applicable, -1 unassigned.
crossjobs.RegionControl = {
	['Argoneau'] 		= { ['own'] = -1, ['zones'] = {152,7,8,151,200,119,120}},
	['Bastok'] 			= { ['own'] =  2, ['zones'] = {234,235,236,237}},
	['Derfland']		= { ['own'] = -1, ['zones'] = {147,197,109,148,110}},
	['ElshimoLowlands']	= { ['own'] = -1, ['zones'] = {250,252,176,123}},
	['ElshimoUplands']	= { ['own'] = -1, ['zones'] = {207,211,160,205,163,159,124}},
	['Fauregandi']		= { ['own'] = -1, ['zones'] = {111,203,204,9,206,166,10}},
	['Gustaberg']		= { ['own'] = -1, ['zones'] = {191,173,106,143,107,144,172}},
	['Jeuno']			= { ['own'] =  0, ['zones'] = {243,244,245,246}},
	['Kolshushu']		= { ['own'] = -1, ['zones'] = {4,118,213,3,198,249,117}},	-- Purgonorgo Isle doesn't have a separate ID
	['Kuzotz']			= { ['own'] = -1, ['zones'] = {209,114,168,208,247,125}},
	['LiTelor']			= { ['own'] = -1, ['zones'] = {153,202,154,251,122,121}},
	['Movapolos']		= { ['own'] = -1, ['zones'] = {13,12,11}},
	['Norvallen']		= { ['own'] = -1, ['zones'] = {105,104,2,150,149,1,195}},
	['QuifimIsland']	= { ['own'] = -1, ['zones'] = {127,184,157,126,179,158}},
	['Ronfaure']		= { ['own'] = -1, ['zones'] = {167,101,141,140,139,190,100,142}},
	['Sandoria']		= { ['own'] =  1, ['zones'] = {230,231,232,233}},
	['Sarutabaruta']	= { ['own'] = -1, ['zones'] = {146,116,170,145,192,194,169,115}},
	['Tavnazia']		= { ['own'] = -1, ['zones'] = {24,25,31,27,30,29,28,32,26}},
	['Tulia']			= { ['own'] = -1, ['zones'] = {181,180,130,178,177}},
	['Valdeaunia']		= { ['own'] = -1, ['zones'] = {6,161,162,165,5,112}},
	['Vollbow']			= { ['own'] = -1, ['zones'] = {113,201,212,174,128}},
	['Windurst']		= { ['own'] =  3, ['zones'] = {238,239,240,241,242}},
	['Zulkheim']		= { ['own'] = -1, ['zones'] = {196,108,102,193,248,103}},
	['Dynamis']			= { ['own'] =  0, ['zones'] = {39,40,41,42,134,135,185,186,187,188}},
	['Lumoria']			= { ['own'] =  0, ['zones'] = {33,34,35,36,37,38}},
	['Promyvion']		= { ['own'] =  0, ['zones'] = {16,17,18,19,20,21,22,23,39,40,41,42}}
};

-- Indiactes what nation your character is from, -1 is unassigned
crossjobs.OwnNation = -1;
crossjobs.Sets = crossjobs.sets;

--[[
	The following event is used to capture the ownership of the regions.
	Conquest updates are sent whenever the player zones and periodically.
	The display bar's region is updated accordingly
--]]

ashita.events.register('packet_in', 'packet_in_callback1', function (e)

	if (e.id == 0x05E) then
		crossjobs.RegionControl['Ronfaure']['own'] = struct.unpack('B', e.data, 0X1E)
		crossjobs.RegionControl['Zulkheim']['own'] = struct.unpack('B', e.data, 0x22)
		crossjobs.RegionControl['Norvallen']['own'] = struct.unpack('B', e.data, 0x26)
		crossjobs.RegionControl['Gustaberg']['own'] = struct.unpack('B', e.data, 0x2A)
		crossjobs.RegionControl['Derfland']['own'] = struct.unpack('B', e.data, 0x2E)
		crossjobs.RegionControl['Sarutabaruta']['own'] = struct.unpack('B', e.data, 0x32)
		crossjobs.RegionControl['Kolshushu']['own'] = struct.unpack('B', e.data, 0x36)
		crossjobs.RegionControl['Argoneau']['own'] = struct.unpack('B', e.data, 0x3A)
		crossjobs.RegionControl['Fauregandi']['own'] = struct.unpack('B', e.data, 0x3E)
		crossjobs.RegionControl['Valdeaunia']['own'] = struct.unpack('B', e.data, 0x42)
		crossjobs.RegionControl['QuifimIsland']['own'] = struct.unpack('B', e.data, 0x46)
		crossjobs.RegionControl['LiTelor']['own'] = struct.unpack('B', e.data, 0x4A)
		crossjobs.RegionControl['Kuzotz']['own'] = struct.unpack('B', e.data, 0x4E)
		crossjobs.RegionControl['Vollbow']['own'] = struct.unpack('B', e.data, 0x52)
		crossjobs.RegionControl['ElshimoLowlands']['own'] = struct.unpack('B', e.data, 0x56)
		crossjobs.RegionControl['ElshimoUplands']['own'] = struct.unpack('B', e.data, 0x5A)
		crossjobs.RegionControl['Tulia']['own'] = struct.unpack('B', e.data, 0x5E)
		crossjobs.RegionControl['Movapolos']['own'] = struct.unpack('B', e.data, 0x62)
		crossjobs.RegionControl['Tavnazia']['own'] = struct.unpack('B', e.data, 0x66)
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
	local pEntity = AshitaCore:GetMemoryManager():GetEntity();
	local targetIndex = gData.GetTargetIndex();
	local x = pEntity:GetRace(targetIndex);

	print(chat.message('Info: ID - ' .. tostring(pEntity.Id)));
end		-- crossjobs.t1

--[[
	RefreshVariables is a routine that let's the player manually make sure
	the job-dependent variables are set. Sometimes when logging in, the
	SetVariables routine is run before the client is done downloading. In
	this case, sometimes some variables are accidentally omitted.

	Note: if the variables already exist, this will not corrupt them.
--]]

function RefreshVariables()
	local player = gData.GetPlayer();

	-- Now, simple toggles, those not dependent on the player's characteristics
	-- can be ignored. The problem ones are the ones specific to a player's job.
	-- They are the ones that sometimes don't get created.

	-- WSwap	-- Weapon Swap
	if string.find('SMN,BLM',player.MainJob) == nil then
		utilities.CreateToggle('WSwap', (string.find('WHM,BRD,RDM',player.MainJob) ~= nil));
	end

	-- SPF		-- Show Pull Feedback
	utilities.CreateToggle('sPF', true);

	-- TH - only assuming that a main THF wants this enabled by default
	if player.MainJob == 'THF' then
		utilities.CreateToggle('TH',(player.MainJob=='THF'));
	end

	-- Macc
	if string.find(utilities._sMagicJobs,player.MainJob) ~= nil or
		 string.find(utilities._sMagicJobs,player.SubJob) ~= nil then
		utilities.CreateToggle('Macc', false);
	end

	-- Tank
	if string.find(utilities._TankJobs,player.MainJob) ~= nil then
		utilities.CreateToggle('Tank', (string.find('PLD,NIN,RUN',player.MainJob) ~= nil));
	end

	-- THF: SS
	if player.MainJob ==  'THF' or player.SubJob == 'THF' then
		utilities.CreateToggle('SS', false);
	end

	-- BST: AJug and DB		-- Automatic Jugs, Damage type (used in pet debuff mitigation)
	if player.MainJob == 'BST' then
		utilities.CreateToggle('AJug',true);
		if utilities.fGetCycle('DB') == 'Unknown' then
			utilities.CreateCycle('DB', {[1] = 'Norm', [2] = 'BPP', [3] = 'WSS'});
		end
	end

	-- BRD main only: Instrument
	if utilities.fGetCycle('Instrument') == 'Unknown' and player.MainJob == 'BRD' then
		utilities.CreateCycle('Instrument', {[1] = 'Horn', [2] = 'String'});
	end

	-- SMN: sBP		-- Show Blood Pact
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		utilities.CreateToggle('sBP', true);
		utilities.CreateCycle('Mode', {[1] = 'PERP', [2] = 'ATTK'});
	end

	-- General cycles: Damage Taken and Region
	utilities.CreateCycle('DT', {[1] = utilities.OFF, [2] = utilities.PHY, [3] = utilities.MAG, [4] = utilities.BRE});
	utilities.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned', [3] = 'N/A'});
end		-- RefreshVariables

--[[
	SetVariables defines run settings for luashitacast
--]]

function SetVariables()
	local player = gData.GetPlayer();

	-- General toggles
	utilities.CreateToggle('GSwap', true);		-- Gear Swap
	utilities.CreateToggle('Kite', false);		-- Kiting
	utilities.CreateToggle('Eva', false);		-- Evasion
	utilities.CreateToggle('Idle', true);		-- Should Default set equip when idling
	utilities.CreateToggle('sPF', true);		-- Show Pull Feedback

	-- Job specific toggles

	-- Weapon swapping WSWAP. SMN and BLM always can weaponswap. WHM, RDM, and BRD you
	-- want to assume WSWAP is enabled. Every other job defaults to false
	if string.find('SMN,BLM',player.MainJob) == nil then
		utilities.CreateToggle('WSwap',(string.find('WHM,RDM,BRD',player.MainJob) ~= nil));
	end

	-- Tanking: PLD, NIN, and RUN default to TANK enabled. DRK, WAR, RDM, and BLU
	-- default to TANK being disabled.
	if string.find(utilities._TankJobs,player.MainJob) ~= nil then
		utilities.CreateToggle('Tank', (string.find('PLD,NIN,RUN',player.MainJob) ~= nil));
	end

	-- Magic Accuracy (Macc)
	if string.find(utilities._sMagicJobs,player.MainJob) ~= nil or
		 string.find(utilities._sMagicJobs,player.SubJob) ~= nil then
		utilities.CreateToggle('Macc', false);
	end

	-- BST only, AJug is an automated system to equip jug pets. DB is a setting to determine the
	-- type of debuff wanted from the Jackcoat. Either BPP or WSS (blind,poison,parallize) or
	-- (weighted,silence,slow). Please note that the Beast Jackoat +1 (AF +1) can dispel all
	-- six debuffs from the pet whereas Beast Jackcoat does BPP and Monster Jackcoat/Monster
	-- Jackcoat+1 only dispels WSS.
	if player.MainJob == 'BST' then
		utilities.CreateToggle('AJug', true);
		if utilities.fGetCycle('DB') == 'Unknown' then
			utilities.CreateCycle('DB', {[1] = 'Norm', [2] = 'BPP', [3] = 'WSS'});
		end
	end

	-- BRD main only, Instrument indicates what default type of instrument should be equipped,
	-- Horn or String
	if player.MainJob == 'BRD' then
		utilities.CreateCycle('Instrument', {[1] = 'Horn', [2] = 'String'});
	end

	-- SMN only, sBP indicates if the a message should be printed in the party chat when
	-- the pet does an offensive blood pact.
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		utilities.CreateToggle('sBP', true);
		utilities.CreateCycle('Mode', {[1] = 'PERP', [2] = 'ATTK'});
	end

	-- THF only, SS indicates that when the player steals, a message should be displayed.
	-- This is used to coordinate thieve's stealing in activities like Dynamis.
	if player.MainJob == 'THF' or player.SubJob == 'THF' then
		-- While it's true that all jobs can now equip TH gear through a 'Tinfoil Hat',
		-- only THF/ or /THF will have the TH command available. Others will have to
		-- use /gs TH to load TH gear.
		utilities.CreateToggle('TH',(player.MainJob == 'THF' or player.SubJob == 'THF'));
		utilities.CreateToggle('SS', false);
	end

	-- General cycles: Damage Taken and Region
	utilities.CreateCycle('DT', {[1] = utilities.OFF, [2] = utilities.PHY, [3] = utilities.MAG, [4] = utilities.BRE});
	utilities.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned', [3] = 'N/A'});
end		-- SetVariables

--[[
	ProcessAccuracy performs the task requested dealing with Accuracy or Ranged Accuracy.
	It's a function called by crossjobs.HandleCommands.

	Pararameter
		args		Passed in list of args from the command line
					[1] -- Acc or RAcc
--]]

function ProcessAccuracy(args)
	local bTank = utilities.fGetToggle('Tank');
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
end		-- ProcessAccuracy

--[[
	HandleCommands processes any commands typed into luashitacast as defined in this file

	Pararameters
		args		List of arguments passed with the command
--]]

function crossjobs.HandleCommands(args)

	if not crossjobs.AliasList:contains(args[1]) then
		return;
	end

	local player = gData.GetPlayer();
	local bTank = utilities.fGetToggle('Tank');
	local sList, sKey, sSet;

	-- Clear out the local copy of current gear
	utilities.ClearSet(crossjobs.sets.CurrentGear);
	args[1] = string.lower(args[1]);

	if (args[1] == 'gswap') then			-- turns gear swapping on or off
		-- /GSWAP
		utilities.AdvanceToggle('GSwap');
	elseif args[1] == 't1' then				-- This is a test invoker
		-- /T1
		crossjobs.t1(args);
	elseif args[1] == 'gc' then				-- Invoke the Gear Check command
		-- /GC [list]
		if args[2] ~= nil and string.lower(args[2]) == 'list' then
			gear.GearCheckList();
		else
			gear.GearCheck();
			gear.bGC = true;
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
	elseif (args[1] == 'dt') then		-- Indicates the type of damage taken gear that will be equipped if desired
		-- /DT [M|P|P]
		if #args == 1 then				-- No qualifier, assume next in set
			utilities.AdvanceCycle('DT');
		else
			local cType = string.upper(string.sub(args[2],1,1));
			if  cType == 'M' then
				utilities.fSetCycle('DT',utilities.MAG);
			elseif cType == 'B' then
				utilities.fSetCycle('DT',utilities.BRE);
			elseif cType == 'P' then
				utilities.fSetCycle('DT',utilities.PHY);
			else
				utilities.fSetCycle('DT',utilities.OFF);
			end
		end
	elseif (args[1] == 'kite') then			-- Turns on/off whether movement gear is equipped
		-- /KITE
		utilities.AdvanceToggle('Kite');
	elseif (args[1] == 'idle') then			-- Turns on/off whether movement gear is equipped
		-- /IDLE
		utilities.AdvanceToggle('Idle');
	elseif (args[1] == 'macc') then			-- Turns on/off whether tanking gear is equipped
		-- /MACC
		if string.find(utilities._sMagicJobs,player.MainJob) ~= nil or
			string.find(utilities._sMagicJobs,player.SubJob) ~= nil then
			utilities.AdvanceToggle('Macc');
		else
			print(chat.message('Warning: Your job does not need magic accuracy'));
		end
	elseif (args[1] == 'mode') then			-- Turns on/off smn emphasis for gear when pet out
		-- /MODE
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			utilities.AdvanceCycle('Mode');
		else
			print(chat.message('Warning: /Mode is only available to summoners'));
		end
	elseif (args[1] == 'ptt') then			-- Displays distance from Pet To Target
		-- /PTT
			pets.ptt();
	elseif (args[1] == 'tank') then			-- Turns on/off whether tanking gear is equipped
		-- /TANK
		if string.find(utilities._TankJobList,player.MainJob) ~= nil then
			utilities.AdvanceToggle('Tank');
		else
			print(chat.message('Warning: Your job does not support the tanking option'));
		end
	elseif (args[1] == 'eva') then			-- Turns on/off whether evasion gear should be equipped
		-- /EVA
		utilities.AdvanceToggle('Eva');
	elseif (args[1] == 'wswap') then		-- Turns on/off whether weapon swapping is permitted
		-- /WSWAP
		if crossjobs.settings.bWSOverride == false then
			utilities.AdvanceToggle('WSwap');
		else
			print(chat.message('Warning: Weapon swapping always enabled on ' .. player.MainJob));
		end
	elseif (args[1] == 'sbp') then			-- Turns on/off whether the blood pact message is shown
		-- /SBP
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			utilities.AdvanceToggle('sBP');
		else
			print(chat.message('Warning: /sBP is only available to summoners'));
		end
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automatic Jug assignment enabled
		-- /AJUG
		if player.MainJob == 'BST' then
			utilities.AdvanceToggle('AJug');
		else
			print(chat.message('Warning: /AJug is only available to beastmasters'));
		end
	elseif (args[1] == 'th') then			-- Turns on/off whether TH gear should be equipped
		-- /TH
		if player.MainJob == 'THF' and player.SubJob == 'THF' then
			utilities.AdvanceToggle('TH');
		else
			print(chat.message('Warning: /TH is only available to thieves'));
		end
	elseif (args[1] == 'ss') then			-- Turns on/off whether Show Action feedback should be displayed
		-- /SS
		if player.MainJob == 'THF' or player.SubJob == 'THF' then
			utilities.AdvanceToggle('SS');
		else
			print(chat.message('Warning: /SS is only available to thieves'));
		end
	elseif (args[1] == 'spf') then			-- Turns on/off whether Show Pull feedback should be displayed
		-- /SPF
		utilities.AdvanceToggle('sPF');
	elseif (args[1] == 'db') then			-- Sets DeBuff (for BST) to the appropriate setting
		-- /DB [BPP|WSS]
		if player.MainJob == 'BST' then
			if args[2] ~= nil  and string.find('BPP,WSS',args[2]) ~= nil then
				utilities.fSetCycle('DB',string.upper(args[2]));
			else
				utilities.AdvanceCycle('DB');
			end
		else
			print(chat.message('Warning: Your job cannot use /DB command'));
		end
	elseif (args[1] == 'acc' or args[1] == 'racc') then
		-- /ACC [#]
		-- Sets the level for the accuracy/ranged accuracy
		ProcessAccuracy(args);
	elseif (args[1] == 'lock' or args[1] == 'unlock') then		-- Lock/unlock gear slots
		-- /LOCK [#|slot name,...] or /UNLOCK [#,slot name, ...]
		locks.ProcessLocks(args);
	elseif (args[1] == 'rc') then		-- Display region controls
		-- /RC
		reporting.RegionControlDisplay();
	elseif (args[1] == 'rv') then		-- Refresh variables
		-- /RV
		RefreshVariables();
	elseif (args[1] == 'pull') then		-- Pull the target
		-- /PULL
		utilities.PullTarget();
	elseif (args[1] == 'showit') then	-- Shows debug info for specified type
		-- /SHOWIT
		reporting.DB_ShowIt();
	elseif (args[1] == 'smg') then		-- Show My Gear
		-- /SMG [gs=|slot=]
		reporting.ProcessSMG(args);
	elseif (args[1] == 'gearset' or args[1] == 'gs') then	-- Forces a gear set to be loaded and turns GSWAP off
		-- /GS name
		gear.ProcessGS(args);
	elseif (args[1] == 'horn' or args[1] == 'string') then		-- String or Horn instrument
		-- /STRING or /HORN
		if player.MainJob == 'BRD' then
			if args[1] == 'horn' then
				utilities.fSetCycle('Instrument',utilities.HORN);
			else
				utilities.fSetCycle('Instrument',utilities.STRING);
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
	elseif args[1] == 'equipit' or args[1] == 'ei' then			-- Equip specified item
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
				gProfile.settings.bAmmo = pets.PetReward(gProfile.settings.DefaultPetFood,'max');
			end
			n = 'PC:Reward';
		elseif string.find('Sic,Ready',ability.Name) ~= nil then
			-- Sic and Ready load the same set
			n = 'PC:Sic_Ready';
		end

		if n ~= nil then
			ts = utilities.fGetTableByName(n);
			gear.MoveToDynamicGS(ts,crossjobs.Sets.CurrentGear,false,n);
			bFound = true;
		end
	-- Check for summoner's blood pact, to load the PreBP
	elseif string.find(pets.SmnBPRageList,ability.Name) ~= nil or
		   string.find(pets.SmnBPWardList,ability.Name) ~= nil then
		gear.MoveToDynamicGS(gProfile.Sets.PreBP,crossjobs.Sets.CurrentGear,false,'PreBP');
		bFound = true;
	end

	if bFound == false then
		if string.find(utilities._PetCommands,string.upper(ability.Name)) ~= nil then
			-- Pet command
			n = 'PC:' .. string.upper(string.gsub(ability.Name,' ','_'));
		else
			-- Assume it's an ability
			n = 'A:' .. string.upper(string.gsub(ability.Name,' ','_'));
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

	gear.MoveToCurrent(gProfile.Sets.Midshot,crossjobs.Sets.CurrentGear,false,'Midshot');
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

	gProfile.settings.priorityWeaponSkill = string.upper(gProfile.settings.priorityWeaponSkill);
	for i = 1,string.len(gProfile.settings.priorityWeaponSkill),1 do
		cKey = string.sub(gProfile.settings.priorityWeaponSkill,i,i);
		if cKey == 'A' then			-- weaponskill set
			-- See if there's a custom gear set defined for this weapon skill
			n = 'WS:' .. string.gsub(lName,' ','_');
			t = utilities.fGetTableByName(n);
			if t ~= nil then
				gear.MoveToDynamicGS(t,gProfile.Sets.CurrentGear,false,lName);
			else
				-- No custom set, look for the stat set
				for i,j in pairs(utilities.tWeaponSkills) do
					if table.find(j,lName) ~= nil then
						sName = 'WS:' .. i;
						t = utilities.fGetTableByName(sName);
						if t ~= nil then
							gear.MoveToDynamicGS(t,gProfile.Sets.CurrentGear,false,sName);
						end
						break;
					end
				end
			end
		end
	elseif cKey == 'B' then		-- elemental gorget
		-- An elemental gorget will add the fTP (at least 10% more damage) to the first hit
		-- of an elemental weapon skill (and many multi-hit weapon skills replicate the fTP
		-- for all the hits.) Also, they give +10 Accuracy to all of the weapon skill's hits
		-- and a 1% chance of not depleting the player's TP after the weapon skill.

		local sGorget,sEle = gear.fCheckForElementalGearByValue('gorget','eleWS',ws.Name);
		if sGorget ~= nil then
			crossjobs.Sets.CurrentGear['Neck'] = sGorget;
		end
	elseif cKey == 'D' then		-- accuracy
		-- Next check on accuracy. Use Tank_accuracy if /tank = true
		if table.find(utilities.tWeaponSkills['RANGED_AGI'],lname) ~= nil or
			table.find(utilities.tWeaponSkills['RANGED_STRAGI'],lname) ~= nil then
			gear.ProgressiveAccuracy('RAcc');
		else
			gear.ProgressiveAccuracy('Acc');
		end
	elseif cKey == 'E' then		-- elemental obi
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
	end

	-- Certain weapon skills can take advantage of magic attack bonus. Check here and equip gear
	-- appropriately. (Note: even though rMAB is a reference gear set, it this particular instance
	-- it is treated like it is a normal gear set.)
	if string.find('red lotus blade,sanguine blade',lName) ~= nil then
		gear.MoveToDynamicGS(gProfile.Sets.rMAB,gProfile.Sets.CurrentGear,false,'rMAB');
	end
end		-- crossjobs.fHandleWeaponskill
