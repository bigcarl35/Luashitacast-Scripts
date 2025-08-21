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
crossjobs._TankJobList = 'PLD,NIN,RUN,DRK,WAR,THF,RDM,BLU';

-- Define lists of valid Weapon Types
crossjobs._WeaponTypes = 'ARCHERY,AXE,CLUB,DAGGER,GAXE,GKATANA,GSWORD,H2H,KATANA,MARKSMANSHIP,POLEARM,SCYTHE,STAVE,SWORD,THROWING';
crossjobs._WeaponMelee = 'AXE,CLUB,DAGGER,GAXE,GKATANA,GSWORD,H2H,KATANA,POLEARM,SCYTHE,STAVE,SWORD';
crossjobs._WeaponRange = 'ARCHERY,MARKSMANSHIP,THROWING';
