local crossjobs = {};

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
		Range = 'Lu Shang\'s F. Rod//GA:FISH',
		Ammo  = 'Sinking Minnow//GA:FISH',
		Body  = { 'Field Tunica//GA:HELM', 'Choc. Jack Coat//GA:DIG', 'Tarutaru Top +1//GA:CLAM', 'Angler\'s Tunica//GA:FISH' },
		Hands = { 'Field Gloves//GA:HELM', 'Fsh. Gloves//GA:FISH' },
		Legs  = { 'Field Hose//GA:HELM', 'Taru. Shorts +1//GA:CLAM', 'Fisherman\'s Hose//GA:FISH' },
		Feet  = { 'Field Boots//GA:HELM', 'Waders//GA:FISH' },
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

-- These two variables are used to store the invoked type of craft/gather type
crossjobs.Craft=nil;
crossjobs.Gather=nil;

-- Define list of all valid jobs
crossjobs._validJobs = 'BLM,BLU,BRD,BST,COR,DNC,DRG,DRK,GEO,MNK,PLD,PUP,RDM,RNG,RUN,SAM,SCH,SMN,THF,WAR,WHM';

-- Define list of all magic using jobs
crossjobs._sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,BLU,SCH,GEO,RUN';

-- Define list of all jobs that can tank
crossjobs._TankJobs = 'PLD,NIN,RUN,DRK,WAR,THF,RDM,BLU';

-- Define lists of valid Weapon Types
crossjobs._WeaponTypes = 'ARCHERY,AXE,CLUB,DAGGER,GAXE,GKATANA,GSWORD,H2H,KATANA,MARKSMANSHIP,POLEARM,SCYTHE,STAVE,SWORD,THROWING';
crossjobs._WeaponMelee = 'AXE,CLUB,DAGGER,GAXE,GKATANA,GSWORD,H2H,KATANA,POLEARM,SCYTHE,STAVE,SWORD';
crossjobs._WeaponRange = 'ARCHERY,MARKSMANSHIP,THROWING';

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
			RegionDisplay();
		end
		e.blocked = false;
		end
	end);

--[[
	fRegionDisplay determines if the player's nation owns the area the character is in
	or not and updates the display bar accordingly.
--]]

function fRegionDisplay()
	local zoneId = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);

	-- Make sure the player's nation is known
	if gcinclude.OwnNation == -1 then
		gcinclude.OwnNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation() + 1;
	end

	-- Determine if current zone in region controlled by player's nation
	for i,j in pairs(gcinclude.RegionControl) do
		if table.find(j['zones'],zoneId) ~= nil then
			if j['own'] == gcinclude.OwnNation then
				gcdisplay.SetCycle('Region','Owned');
			elseif j['own'] == 0 and gcinclude.fBuffed('Signet') == false then
				gcdisplay.SetCycle('Region','N/A');
			else
				gcdisplay.SetCycle('Region','Not Owned');
			end
			break;
		end
		end
	end		-- RegionDisplay
