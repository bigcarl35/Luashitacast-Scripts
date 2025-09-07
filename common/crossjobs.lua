local crossjobs = T{};

local version = {
	['author']	= 'Paiine',
	['name']	= 'Luashitacast (Karma)',
	['version']	= '2.0.alpha'
};

local gcdisplay  = require('gcdisplay');
local utilities  = require('utilities');
local reporting  = require('reporting');
local displaybar = require('displaybar');
local locks      = require('locks');
local gear 	     = require('gear');

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
	Messages = false; 	 -- set to true if you want chat log messages to appear on any /gs command used such as DT, or KITE gear toggles, certain messages will always appear
	WScheck = true; 	 -- set to false if you don't want to use the WSdistance safety check
	WSdistance = 4.7; 	 -- default max distance (yalms) to allow non-ranged WS to go off at if the above WScheck is true
	bWSOverride = false; -- is the player playing a job where weapon swapping always happens, it is not optional?
	Tolerance = 97;		 -- Comparison value %, cut-off for certain comparisons
	DefaultSpellTarget = 't'; -- What to use in MaxSpell if no target specified
	DefaultSongTarget = 't';  -- What to use in MaxSong if no target specified
	--
	priorityEngaged = 'CEF'; 		-- indicates order of steps for engagement
	priorityWeaponSkill = 'ADBE';	-- indicates order of steps for a weapon skill
	--
	bAutoStaveSwapping = true;		-- indicates if elemental stave swapping should occur automatically
	--
	bMinBasetime = 15;		-- minimum wait before reminding player to run /gc
	bMaxBasetime = 300;		-- once reminder shown, switch to every 5 minutes
	bGCReminder = false;	-- Has GC reminder been displayed yet
};

-- List of all supported commands
crossjobs.AliasList = {
	'acc','ajug','db','dt','ei','equipit','eva','gc','gcmessages','gearset','gs','gswap','help','horn','idle','kite',
	'lock','macc','maxsong','maxspell','petfood','ptt','pull','racc','rc','rv','sbp','showit','smg','spf','ss','string',
	'tank','th','unlock','ver','wsdistance','wswap','t1'
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

crossjobs.OwnNation = -1;
crossjobs.Sets = crossjobs.sets;

--[[
***
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
		if gcdisplay ~= nil then
			displaybar.RegionDisplay();
		end
		e.blocked = false;
	end
end);

--[[
***
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
***
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
	gcdisplay.CreateToggle('WSwap', (string.find('WHM,BRD,RDM',player.MainJob) ~= nil));

	-- SPF		-- Show Pull Feedback
	gcdisplay.CreateToggle('sPF', true);

	-- TH - only assuming that a main THF wants this enabled by default
	gcdisplay.CreateToggle('TH',(player.MainJob=='THF' or player.SubJob == 'THF'));

	-- Macc
	if string.find(utilities._sMagicJobs,player.MainJob) ~= nil or
			string.find(utilities._sMagicJobs,player.SubJob) ~= nil then
		gcdisplay.CreateToggle('Macc', false);
	end

	-- Tank
	if string.find(utilities._TankJobs,player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank', (string.find('PLD,NIN,RUN',player.MainJob) ~= nil));
	end

	-- THF: SS
	if player.MainJob ==  'THF' or player.SubJob == 'THF' then
		gcdisplay.CreateToggle('SS', false);
	end

	-- BST: AJug and DB		-- Automatic Jugs, Damage type (used in pet debuff mitigation)
	if player.MainJob == 'BST' then
		gcdisplay.CreateToggle('AJug',true);
		if gcdisplay.GetCycle('DB') == 'Unknown' then
			gcdisplay.CreateCycle('DB', {[1] = 'Norm', [2] = 'BPP', [3] = 'WSS'});
		end
	end

	-- BRD main only: Instrument
	if gcdisplay.GetCycle('Instrument') == 'Unknown' and player.MainJob == 'BRD' then
		gcdisplay.CreateCycle('Instrument', {[1] = 'Horn', [2] = 'String'});
	end

	-- SMN: sBP		-- Show Blood Pact
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		gcdisplay.CreateToggle('sBP', true);
	end

	-- General cycles: Damage Taken and Region
	gcdisplay.CreateCycle('DT', {[1] = utilities.OFF, [2] = utilities.PHY, [3] = utilities.MAG, [4] = utilities.BRE});
	gcdisplay.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned', [3] = 'N/A'});
end		-- RefreshVariables

--[[
***
	SetVariables defines run settings for luashitacast
--]]

function SetVariables()
	local player = gData.GetPlayer();

	-- General toggles
	gcdisplay.CreateToggle('GSwap', true);		-- Gear Swap
	gcdisplay.CreateToggle('Kite', false);		-- Kiting
	gcdisplay.CreateToggle('Eva', false);		-- Evasion
	gcdisplay.CreateToggle('Idle', true);		-- Should Default set equip when idling
	gcdisplay.CreateToggle('sPF', true);		-- Show Pull Feedback

	-- With the addition of the Tin Foil Hat, everyone can get TH now. Not sure the
	-- TH from this item is really wanted though...
	gcdisplay.CreateToggle('TH',(player.MainJob == 'THF' or player.SubJob == 'THF'));

	-- Job specific toggles

	-- Weapon swapping WSWAP. SMN and BLM always can weaponswap. WHM, RDM, and BRD you
	-- want to assume WSWAP is enabled. Every other job defaults to false
	if string.find('SMN,BLM',player.MainJob) == nil then
		gcdisplay.CreateToggle('WSwap',(string.find('WHM,RDM,BRD',player.MainJob) ~= nil));
	end

	-- Tanking: PLD, NIN, and RUN default to TANK enabled. DRK, WAR, RDM, and BLU
	-- default to TANK being disabled.
	if string.find(utilities._TankJobs,player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank', (string.find('PLD,NIN,RUN',player.MainJob) ~= nil));
	end

	-- Magic Accuracy (Macc)
	if string.find(utilities._sMagicJobs,player.MainJob) ~= nil or
			string.find(utilities._sMagicJobs,player.SubJob) ~= nil then
		gcdisplay.CreateToggle('Macc', false);
	end

	-- BST only, AJug is an automated system to equip jug pets. DB is a setting to determine the
	-- type of debuff wanted from the Jackcoat. Either BPP or WSS (blind,poison,parallize) or
	-- (weighted,silence,slow). Please note that the Beast Jackoat +1 (AF +1) can dispel all
	-- six debuffs from the pet whereas Beast Jackcoat does BPP and Monster Jackcoat/Monster
	-- Jackcoat+1 only dispels WSS.
	if player.MainJob == 'BST' then
		gcdisplay.CreateToggle('AJug', true);
		if gcdisplay.GetCycle('DB') == 'Unknown' then
			gcdisplay.CreateCycle('DB', {[1] = 'Norm', [2] = 'BPP', [3] = 'WSS'});
		end
	end

	-- BRD main only, Instrument indicates what default type of instrument should be equipped,
	-- Horn or String
	if player.MainJob == 'BRD' then
		gcdisplay.CreateCycle('Instrument', {[1] = 'Horn', [2] = 'String'});
	end

	-- SMN only, sBP indicates if the a message should be printed in the party chat when
	-- the pet does an offensive blood pact.
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		gcdisplay.CreateToggle('sBP', true);
	end

	-- THF only, SS indicates that when the player steals, a message should be displayed.
	-- This is used to coordinate thieve's stealing in activities like Dynamis.
	if player.MainJob == 'THF' or player.SubJob == 'THF' then
		gcdisplay.CreateToggle('SS', false);
	end

	-- General cycles: Damage Taken and Region
	gcdisplay.CreateCycle('DT', {[1] = utilities.OFF, [2] = utilities.PHY, [3] = utilities.MAG, [4] = utilities.BRE});
	gcdisplay.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned', [3] = 'N/A'});
end		-- SetVariables

--[[
***
	ProcessAccuracy performs the task requested dealing with Accuracy or Ranged Accuracy.
	It's a function called by crossjobs.HandleCommands.

	Pararameter
		args		Passed in list of args from the command line
					[1] -- Acc or RAcc
--]]

function ProcessAccuracy(args)
	local bTank = gcdisplay.GetToggle('Tank');
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
			print(chat.message(string.format('Info: Accuracy at stage: %d',gcdisplay.GetAccCur('Acc'))));
			if bTank == true then
				print(chat.message(string.format('Info: Tank Accuracy at stage: %d',gcdisplay.GetAccCur('TAcc'))));
			end
		else
			print(chat.message(string.format('Info: Ranged Accuracy at stage: %d',gcdisplay.GetAccCur('RAcc'))));
			if bTank == true then
				print(chat.message(string.format('Info: Tank Ranged Accuracy at stage: %d',gcdisplay.GetAccCur('TRAcc'))));
			end
			return;
		end
	else
		narg = tonumber(args[2]);
		if narg < 0 or narg > gcdisplay.GetAccMax(tmp) then
			print(chat.message('Warning: Invalid stage. Number must be between 0 and ' .. tostring(gcdisplay.GetAccMax(tmp))));
			return;
		else
			num = narg;
		end
	end

	if num == 0 then
		gcdisplay.SetAccCur(tmp,0);
		-- Make sure that both tank and non-tank versions are turned off
		if string.find('Acc,Racc',tmp) ~= nil then
			tmp = 'T' .. tmp;				-- Add a 'T' to the beginning of tmp
		else
			tmp - string.sub(tmp,2,-1);		-- Remove the 'T' from the beginning of tmp
		end
		gcdisplay.SetAccCur(tmp,0);
		if tmp == 'Acc' or tmp == 'TAcc' then
			print(chat.message('Info: Accuracy has been turned off'));
		else
			print(chat.message('Info: Ranged Accuracy has been turned off'));
		end
	else
		gcdisplay.SetAccCur(tmp,num);
		print(chat.message(string.format('Info: %s stage set to %d',tmp,num)));
	end
end		-- ProcessAccuracy

--[[
***
	ProcessLocks processes the invocation of lock/unlock command

	Pararameter
		args		Passed argument list
--]]

function ProcessLocks(args)

	if args[1] == utilities._LOCK then
		if args[2] ~= nil then
			locks.LockUnlock(utilities._LOCK,args[2]);
			sList = locks.fGetLockedList();
			if sList ~= nil then
				print(chat.message('The following slot(s) are locked: ' .. sList));
			else
				print(chat.message('All slots are unlocked'));
			end
			gcdisplay.SetSlots('locks',locks.LocksNumeric);
		end
	else		-- unlock
		if args[2] == nil then
			args[2] = 'all';
		end
		locks.LockUnlock(utilities._UNLOCK,args[2]);
		if string.lower(args[2]) == 'all' then
			print(chat.message('All slots are unlocked'));
		else
			print(chat.message('\'' .. args[2] .. '\' have been unlocked'));
		end
	sList = locks.fGetLockedList();
	gcdisplay.SetSlots('locks',locks.LocksNumeric);
	end
end		-- ProcessLocks

--[[
***
	ProcessSMGs processes the invocation of Show My Gear reporting command

	Pararameter
		args		Passed argument list
--]]

function ProcessSMG(args)

	if #args == 1 then				-- Show a list of all gear
		reporting.DisplayGD_AW(nil);
	elseif args[2] ~= nil then
		local ls = string.lower(args[2]);
		if ls == 'noac' then		-- Show a list of gear where accessible is false
			reporting.DisplayGD_AW('noac');
		elseif string.len(ls) > 5 and string.sub(ls,1,5) == 'slot=' then
			reporting.DisplayGD_S(string.sub(ls,6,-1));
		elseif string.len(ls) > 3 and string.sub(ls,1,3) == 'gs=' then
			reporting.DisplayGD_Gs(string.sub(ls,4,-1));
		end
	end
end		-- ProcessSMG

--[[
	ProcessGS processes the specified Gear Set

	Pararameter
		args		Passed arguement list
--]]

function ProcessGS(args)

	if #args > 1 then
		local sArg = string.upper(args[2]);
		local sTmp = ',' .. crossjobs.Crafting_Types .. ',';
		local sTmp2 = ',' .. crossjobs.Gathering_Types .. ',';
		if string.find(sTmp,sArg) ~= nil or string.find(sTmp2,sArg) ~= nil then
			-- gather or crafting set
			if string.find(sTmp,sArg) then
				-- Crafting set
				crossjobs.Craft = sArg;
				gear.MoveToDynamicGS(crossjobs.sets.Crafting,crossjobs.sets.CurrentGear,false,true);		-- revisit
			else
				-- Gather set
				crossjobs.Gather = sArg;
				gear.MoveToDynamicGS(gcinclude.sets.Gathering,gcinclude.sets.CurrentGear,false,true);		-- revisit
			end
		else
			local tTable = utilities.fGetTableByName(sArg);	-- Change string to table
			if tTable ~= nil then
				gear.MoveToDynamicGS(tTable,crossjobs.sets.CurrentGear,true);	-- revisit
			else
				print(chat.message('Warning: Gear set not found: ' .. sArg));
				return;
			end
		end

		gear.EquipTheGear(crossjobs.sets.CurrentGear,true);							-- revisit
		locks.LockByGearSet(crossjobs.sets.CurrentGear,nil,false,bIgnoreWSWAP,bDisplay)
	else
		print(chat.message('Error: No set specified for /gearset. Command ignored.'));
	end
end		-- ProcessGS

--[[
	HandleCommands processes any commands typed into luashitacast as defined in this file

	Pararameters
		args		List of arguments passed with the command
--]]

function gcinclude.HandleCommands(args)
	if not crossjobs.AliasList:contains(args[1]) then return end

	local player = gData.GetPlayer();
	local bTank = gcdisplay.GetToggle('Tank');
	local sList, sKey, sSet;

	-- Clear out the local copy of current gear
	utilities.ClearSet(crossjobs.sets.CurrentGear);
	args[1] = string.lower(args[1]);

	if (args[1] == 'gswap') then			-- turns gear swapping on or off
		gcdisplay.AdvanceToggle('GSwap');
	elseif args[1] == 't1' then				-- This is a test invoker
		gcinclude.t1(args);
	elseif args[1] == 'gc' then				-- Invoke the Gear Check command
		if args[2] ~= nil and string.lower(args[2]) == 'list' then
			gear.GearCheckList();
		else
			gear.GearCheck();
			gcdisplay.SetGC(true);
		end
	elseif args[1] == 'gcmessages' then		-- turns feedback on/off for all commands
		crossjobs.settings.Messages = not crossjobs.settings.Messages;
		if crossjobs.settings.Messages == true then
			print(chat.message('Info: Chat messages are enabled'));
		else
			print(chat.message('Info: Chat messages are disabled'));
		end
	elseif (args[1] == 'wsdistance') then	-- Turns on/off the check for weapons skill distance or sets the distance
		local i = tonumber(args[2]);
		if i ~= nil then
			crossjobs.settings.WScheck = true;
			crossjobs.settings.WSdistance = i
			print(chat.message('Info: WS Distance is now on and set to ' .. tostring(crossjobs.settings.WSdistance)));
		else
			crossjobs.settings.WScheck = not crossjobs.settings.WScheck;
			print(chat.message('Info: WS distance check is now set to ' .. tostring(crossjobs.settings.WScheck)));
		end
	elseif (args[1] == 'dt') then		-- Indicates the type of damage taken gear that will be equipped if desired
		if #args == 1 then				-- No qualifier, assume next in set
			gcdisplay.AdvanceCycle('DT');
		else
			local cType = string.upper(string.sub(args[2],1,1));
			if  cType == 'M' then
				gcdisplay.SetCycle('DT',crossjobs.MAG);
			elseif cType == 'B' then
				gcdisplay.SetCycle('DT',crossjobs.BRE);
			elseif cType == 'P' then
				gcdisplay.SetCycle('DT',crossjobs.PHY);
			else
				gcdisplay.SetCycle('DT',crossjobs.OFF);
			end
		end
	elseif (args[1] == 'kite') then			-- Turns on/off whether movement gear is equipped
		gcdisplay.AdvanceToggle('Kite');
	elseif (args[1] == 'idle') then			-- Turns on/off whether movement gear is equipped
		gcdisplay.AdvanceToggle('Idle');
	elseif (args[1] == 'macc') then			-- Turns on/off whether tanking gear is equipped
		if string.find(crossjobs._sMagicJobs,player.MainJob) ~= nil or
			string.find(crossjobs._sMagicJobs,player.SubJob) ~= nil then
			gcdisplay.AdvanceToggle('Macc');
		else
			print(chat.message('Warning: Your job does not need magic accuracy'));
		end
	elseif (args[1] == 'ptt') then			-- Displays distance from Pet To Target
			ptt();
	elseif (args[1] == 'tank') then			-- Turns on/off whether tanking gear is equipped
		if string.find(crossjobs._TankJobList,player.MainJob) ~= nil then
			gcdisplay.AdvanceToggle('Tank');
		else
			print(chat.message('Warning: Your job does not support the tanking option'));
		end
	elseif (args[1] == 'eva') then			-- Turns on/off whether evasion gear should be equipped
		gcdisplay.AdvanceToggle('Eva');
	elseif (args[1] == 'wswap') then		-- Turns on/off whether weapon swapping is permitted
		if gcinclude.settings.bWSOverride == false then
			gcdisplay.AdvanceToggle('WSwap');
		else
			print(chat.message('Warning: Weapon swapping always enabled on ' .. player.MainJob));
		end
	elseif (args[1] == 'sbp') then			-- Turns on/off whether the blood pact message is shown
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			gcdisplay.AdvanceToggle('sBP');
		else
			print(chat.message('Warning: /sBP is only available to summoners'));
		end
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automatic Jug assignment enabled
		if player.MainJob == 'BST' then
			gcdisplay.AdvanceToggle('AJug');
		else
			print(chat.message('Warning: /AJug is only available to beastmasters'));
		end
	elseif (args[1] == 'th') then			-- Turns on/off whether TH gear should be equipped
		if player.MainJob == 'THF' and player.SubJob == 'THF' then
			gcdisplay.AdvanceToggle('TH');
		else
			print(chat.message('Warning: /TH is only available to thieves'));
		end
	elseif (args[1] == 'ss') then			-- Turns on/off whether Show Action feedback should be displayed
		if player.MainJob == 'THF' or player.SubJob == 'THF' then
			gcdisplay.AdvanceToggle('SS');
		else
			print(chat.message('Warning: /SS is only available to thieves'));
		end
	elseif (args[1] == 'spf') then			-- Turns on/off whether Show Pull feedback should be displayed
		gcdisplay.AdvanceToggle('sPF');
	elseif (args[1] == 'db') then			-- Sets DeBuff (for BST) to the appropriate setting
		if player.MainJob == 'BST' then
			if args[2] ~= nil  and string.find('BPP,WSS',args[2]) ~= nil then
				gcdisplay.SetCycle('DB',string.upper(args[2]));
			else
				gcdisplay.AdvanceCycle('DB');
			end
		else
			print(chat.message('Warning: Your job cannot use /DB command'));
		end
	elseif (args[1] == 'acc' or args[1] == 'racc') then
		-- Sets the level for the accuracy/ranged accuracy
		ProcessAccuracy(args);
	elseif (args[1] == 'lock' or args[1] == 'unlock') then		-- Lock/unlock gear slots
		ProcessLocks(args);
	elseif (args[1] == 'rc') then		-- Display region controls
		reporting.RegionControlDisplay();
	elseif (args[1] == 'rv') then		-- Refresh variables
		RefreshVariables();
	elseif (args[1] == 'pull') then		-- Pull the target
		utilities.PullTarget();
	elseif (args[1] == 'showit') then	-- Shows debug info for specified type
		reporting.DB_ShowIt();
	elseif (args[1] == 'smg') then		-- Show My Gear
		ProcessSMG(args);
	elseif (args[1] == 'gearset' or args[1] == 'gs') then	-- Forces a gear set to be loaded and turns GSWAP off
		ProcessGS(args);
	elseif (args[1] == 'horn' or args[1] == 'string') then		-- String or Horn instrument
		if player.MainJob == 'BRD' then
			if args[1] == 'horn' then
				gcdisplay.SetCycle('Instrument',crossjobs.HORN);
			else
				gcdisplay.SetCycle('Instrument',crossjobs.STRING);
			end
		else
			print(chat.message('Warning: Your job does not support that command. Ignoring.'));
		end

	elseif (args[1] == 'maxspell') then			-- Determines highest level spell to cast
		MaxSpell(args[2],args[3],true);
	elseif (args[1] == 'maxsong') then			-- Determines highest level song to cast
		MaxSong(args[2],args[3],true);
	elseif args[1] == 'equipit' or args[1] == 'ei' then			-- Equip specified item
		EquipItem(args);
	elseif args[1] == 'ver' then				-- Display version/change log
		reporting.DisplayVersion();
	end

	if crossjobs.settings.Messages then
		utilities.Message(toggle, status)
	end
end		-- gcinclude.HandleCommands
