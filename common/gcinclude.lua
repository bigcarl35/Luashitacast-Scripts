local gcinclude = T{};

require 'common'

version = { ['author']	= 'Paiine',
 		    ['name']	= 'Luashitacast (Karma)',
			['version']	= '1.5.6' };
	
--[[
	This file contains routines that are used with Luashitacast across any supported job.
	Job specific routines are found in the "Username"_job file (ex: Paiine_BST.lua)
-]]
gcinclude.sets = {

--[[
	There are currently eight crafts: alchemy, bonecraft, clothcraft, cooking, goldsmithing, leathercraft,
	smithing, and woodworking. It's possible that a player will have gear for more than one craft. There's
	only one Crafting gear set, so you need to qualify each piece with what type of crafting the piece is
	used for. (Ex: Body = 'Weaver\'s Apron//CR:CLOTH).
	
	Please note that Crafting sets ignore the /WSWAP	setting.
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
		Ammo  = 'Fly Lure//GA:FISH',
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
	than in a piecemeal manner. It is hoped that this will cut down on flickering gear and
	possibly speed up the code. *** This set is to be left empty by the player ***. Please
	do not modify it.
--]]	
	['CurrentGear'] = { },	
};

gcinclude.settings = {
--[[
	You can also set any of these on a per job basis in the job file in the OnLoad function. See my BST job file 
	to see how this is done.
--]]
	Messages = false; 	 -- set to true if you want chat log messages to appear on any /gs command used such as DT, or KITE gear toggles, certain messages will always appear
	WScheck = true; 	 -- set to false if you don't want to use the WSdistance safety check
	WSdistance = 4.7; 	 -- default max distance (yalms) to allow non-ranged WS to go off at if the above WScheck is true
	bWSOverride = false; -- is the player playing a job where weapon swapping always happens, it is not optional?
	Tolerance = 97;		 -- Comparison value %, cut-off for certain comparisons
	TH_hits = 2;		 -- How many hits til TH gear no longer needed
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

-- Please note that on HorizonXI, item.Name[1] contains the English name of the item
-- instead of item.Name[2] like it's suppose to. item.Name[2] has the Japanese name.
-- This is very confusing and the source of many frustrations.


-- The following arrays are used by the functions contained in this file. Probably best to leave them alone

gcdisplay = gFunc.LoadFile('common\\gcdisplay.lua');

gcinclude.AliasList = T{'acc','ajug','db','dt','ei','equipit','eva','gc','gcmessages','gearset','gs','gswap','help','horn','idle','kite','lock','macc','maxsong','maxspell','petfood','ptt','racc','rc','rv','sbp','showit','smg','string','tank','th','unlock','ver','wsdistance','wswap','t1'};
gcinclude.Towns = T{'Tavnazian Safehold','Al Zahbi','Aht Urhgan Whitegate','Nashmau','Southern San d\'Oria [S]','Bastok Markets [S]','Windurst Waters [S]','San d\'Oria-Jeuno Airship','Bastok-Jeuno Airship','Windurst-Jeuno Airship','Kazham-Jeuno Airship','Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille','Bastok Mines','Bastok Markets','Port Bastok','Metalworks','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower','Ru\'Lude Gardens','Upper Jeuno','Lower Jeuno','Port Jeuno','Rabao','Selbina','Mhaura','Kazham','Norg','Mog Garden','Celennia Memorial Library','Western Adoulin','Eastern Adoulin'};
gcinclude.Windy = T{'Windurst Waters [S]','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower'};
gcinclude.Sandy = T{'Southern San d\'Oria [S]','Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille'};
gcinclude.Bastok = T{'Bastok Markets [S]','Bastok Mines','Bastok Markets','Port Bastok','Metalworks'};
gcinclude.Jeuno = T{'Ru\'Lude Gardens','Upper Jeuno','Lower Jeuno','Port Jeuno'};
gcinclude.DistanceWS = T{'Flaming Arrow','Piercing Arrow','Dulling Arrow','Sidewinder','Blast Arrow','Arching Arrow','Empyreal Arrow','Refulgent Arrow','Apex Arrow','Namas Arrow','Jishnu\'s Randiance','Hot Shot','Split Shot','Sniper Shot','Slug Shot','Blast Shot','Heavy Shot','Detonator','Numbing Shot','Last Stand','Coronach','Wildfire','Trueflight','Leaden Salute','Myrkr','Dagan','Moonlight','Starlight','Mistral Axe'};
gcinclude.BstPetAttack = T{'Foot Kick','Whirl Claws','Big Scissors','Tail Blow','Blockhead','Sensilla Blades','Tegmina Buffet','Lamb Chop','Sheep Charge','Pentapeck','Recoil Dive','Frogkick','Queasyshroom','Numbshroom','Shakeshroom','Nimble Snap','Cyclotail','Somersault','Tickling Tendrils','Sweeping Gouge','Grapple','Double Claw','Spinning Top','Suction','Tortoise Stomp','Power Attack','Rhino Attack','Razor Fang','Claw Cyclone','Crossthrash','Scythe Tail','Ripper Fang','Chomp Rush','Pecking Flurry','Sickle Slash','Mandibular Bite','Wing Slap','Beak Lunge','Head Butt','Wild Oats','Needle Shot','Disembowel','Extirpating Salvo','Mega Scissors','Back Heel','Hoof Volley','Fluid Toss','Fluid Spread'};
gcinclude.BstPetMagicAttack = T{'Gloom Spray','Fireball','Acid Spray','Molting Plumage','Cursed Sphere','Nectarous Deluge','Charged Whisker','Nepenthic Plunge'};
gcinclude.BstPetMagicAccuracy = T{'Toxic Spit','Acid Spray','Leaf Dagger','Venom Spray','Venom','Dark Spore','Sandblast','Dust Cloud','Stink Bomb','Slug Family','Intimidate','Gloeosuccus','Spider Web','Filamented Hold','Choke Breath','Blaster','Snow Cloud','Roar','Palsy Pollen','Spore','Brain Crush','Choke Breath','Silence Gas','Chaotic Eye','Sheep Song','Soporific','Predatory Glare','Sudden Lunge','Numbing Noise','Jettatura','Bubble Shower','Spoil','Scream','Noisome Powder','Acid Mist','Rhinowrecker','Swooping Frenzy','Venom Shower','Corrosive Ooze','Spiral Spin','Infrasonics','Hi-Freq Field','Purulent Ooze','Foul Waters','Sandpit','Infected Leech','Pestilent Plume'};
gcinclude.SmnSkill = T{'Shining Ruby','Glittering Ruby','Crimson Howl','Inferno Howl','Frost Armor','Crystal Blessing','Aerial Armor','Hastega II','Fleet Wind','Hastega','Earthen Ward','Earthen Armor','Rolling Thunder','Lightning Armor','Soothing Current','Ecliptic Growl','Heavenward Howl','Ecliptic Howl','Noctoshield','Dream Shroud','Altana\'s Favor','Reraise','Reraise II','Reraise III','Raise','Raise II','Raise III','Wind\'s Blessing'};
gcinclude.SmnMagical = T{'Searing Light','Meteorite','Holy Mist','Inferno','Fire II','Fire IV','Meteor Strike','Conflag Strike','Diamond Dust','Blizzard II','Blizzard IV','Heavenly Strike','Aerial Blast','Aero II','Aero IV','Wind Blade','Earthen Fury','Stone II','Stone IV','Geocrush','Judgement Bolt','Thunder II','Thunder IV','Thunderstorm','Thunderspark','Tidal Wave','Water II','Water IV','Grand Fall','Howling Moon','Lunar Bay','Ruinous Omen','Somnolence','Nether Blast','Night Terror','Level ? Holy'};
gcinclude.SmnAccuracy = T{'Healing Ruby','Healing Ruby II','Whispering Wind','Spring Water','Diamond Storm','Sleepga','Shock Squall','Slowga','Tidal Roar','Pavor Nocturnus','Ultimate Terror','Nightmare','Mewing Lullaby','Eerie Eye'};
gcinclude.SmnHybrid = T{'Burning Strike','Flaming Crush'};
gcinclude.SmnBPRageList = 'Searing Light,Howling Moon,Inferno,Earthen Fury,Tidal Wave,Aerial Blast,Diamond Dust,Judgment Bolt,Ruinous Omen,Punch,Rock Throw,Barracuda Dive,Claw,Axe Kick,Shock Strike,Camisado,Poison Nails,Moonlit Charge,Crescent Fang,Fire II,Stone II,Water II,Blizzard II,Thunder II,Aero II,Thunderspark,Rock Buster,Burning Strike,Tail Whip,Double Punch,Megalith Throw,Double Slap,Meteorite,Fire IV,Stone IV,Water IV,Aero IV,Blizzard IV,Thunder IV,Eclipse Bite,Nether Blast,Flaming Crush,Mountain Buster,Spinning Dive,Predator Claws,Rush,Chaotic Strike';
gcinclude.BluMagPhys = T{'Foot Kick','Sprout Smack','Wild Oats','Power Attack','Queasyshroom','Battle Dance','Feather Storm','Helldive','Bludgeon','Claw Cyclone','Screwdriver','Grand Slam','Smite of Rage','Pinecone Bomb','Jet Stream','Uppercut','Terror Touch','Mandibular Bite','Sickle Slash','Dimensional Death','Spiral Spin','Death Scissors','Seedspray','Body Slam','Hydro Shot','Frenetic Rip','Spinal Cleave','Hysteric Barrage','Asuran Claws','Cannonball','Disseverment','Ram Charge','Vertical Cleave','Final Sting','Goblin Rush','Vanity Dive','Whirl of Rage','Benthic Typhoon','Quad. Continuum','Empty Thrash','Delta Thrust','Heavy Strike','Quadrastrike','Tourbillion','Amorphic Spikes','Barbed Crescent','Bilgestorm','Bloodrake','Glutinous Dart','Paralyzing Triad','Thrashing Assault','Sinker Drill','Sweeping Gouge','Saurian Slide'};
gcinclude.BluMagDebuff = T{'Filamented Hold','Cimicine Discharge','Demoralizing Roar','Venom Shell','Light of Penance','Sandspray','Auroral Drape','Frightful Roar','Enervation','Infrasonics','Lowing','CMain Wave','Awful Eye','Voracious Trunk','Sheep Song','Soporific','Yawn','Dream Flower','Chaotic Eye','Sound Blast','Blank Gaze','Stinking Gas','Geist Wall','Feather Tickle','Reaving Wind','Mortal Ray','Absolute Terror','Blistering Roar','Cruel Joke'};
gcinclude.BluMagStun = T{'Head Butt','Frypan','Tail Slap','Sub-zero Smash','Sudden Lunge'};
gcinclude.BluMagBuff = T{'Cocoon','Refueling','Feather Barrier','Memento Mori','Zephyr Mantle','Warm-Up','Amplification','Triumphant Roar','Saline Coat','Reactor Cool','Plasma Charge','Regeneration','Animating Wail','Battery Charge','Winds of Promy.','Barrier Tusk','Orcish Counterstance','Pyric Bulwark','Nat. Meditation','Restoral','Erratic Flutter','Carcharian Verve','Harden Shell','Mighty Guard'};
gcinclude.BluMagSkill = T{'Metallic Body','Diamondhide','Magic Barrier','Occultation','Atra. Libations'};
gcinclude.BluMagDiffus = T{'Erratic Flutter','Carcharian Verve','Harden Shell','Mighty Guard'};
gcinclude.BluMagCure = T{'Pollen','Healing Breeze','Wild Carrot','Magic Fruit','Plenilune Embrace'};
gcinclude.BluMagEnmity = T{'Actinic Burst','Exuviation','Fantod','Jettatura','Temporal Shift'};
gcinclude.BluMagTH = T{'Actinic Burst','Dream Flower','Subduction'};
gcinclude.Elements = T{'Thunder', 'Blizzard', 'Fire', 'Stone', 'Aero', 'Water', 'Light', 'Dark'};
gcinclude.HelixSpells = T{'Ionohelix', 'Cryohelix', 'Pyrohelix', 'Geohelix', 'Anemohelix', 'Hydrohelix', 'Luminohelix', 'Noctohelix'};
gcinclude.StormSpells = T{'Thunderstorm', 'Hailstorm', 'Firestorm', 'Sandstorm', 'Windstorm', 'Rainstorm', 'Aurorastorm', 'Voidstorm'};
gcinclude.Rolls = T{{'Fighter\'s Roll',5,9}, {'Monk\'s Roll',3,7}, {'Healer\'s Roll',3,7}, {'Corsair\'s Roll',5,9}, {'Ninja Roll',4,8},{'Hunter\'s Roll',4,8}, {'Chaos Roll',4,8}, {'Magus\'s Roll',2,6}, {'Drachen Roll',4,8}, {'Choral Roll',2,6},{'Beast Roll',4,8}, {'Samurai Roll',2,6}, {'Evoker\'s Roll',5,9}, {'Rogue\'s Roll',5,9}, {'Warlock\'s Roll',4,8},
	{'Puppet Roll',3,7}, {'Gallant\'s Roll',3,7}, {'Wizard\'s Roll',5,9}, {'Dancer\'s Roll',3,7}, {'Scholar\'s Roll',2,6},{'Naturalist\'s Roll',3,7}, {'Runeist\'s Roll',4,8}, {'Bolter\'s Roll',3,9}, {'Caster\'s Roll',2,7}, {'Courser\'s Roll',3,9},{'Blitzer\'s Roll',4,9}, {'Tactician\'s Roll',5,8}, {'Allies\' Roll',3,10}, {'Miser\'s Roll',5,7},
	{'Companion\'s Roll',2,10},{'Avenger\'s Roll',4,8},}; -- {name,lucky,unlucky}
gcinclude.Crafting_Types = 'ALC,BONE,CLOTH,COOK,GSM,LTH,BSM,WW';
gcinclude.Gathering_Types = 'HELM,DIG,CLAM,FISH';

--[[
	The following two variables are used to store the invoked type of craft/gather type
--]]
gcinclude.Craft=nil;
gcinclude.Gather=nil;

-- List of all valid slot names
gcinclude.SlotNames = { 'subset','main','sub','range','ammo','head',
	'neck','ear1','ear2','ears','body','hands','ring1','rings2',
	'rings','back','waist','legs','feet' };
	
--[[
	The following define all the weaponskills according to the desired stats
--]]

gcinclude.tWeaponSkills = {
	['CHR']    = { 'shadowstitch' },
	['DEX']    = { 'wasp sting', 'viper bite', 'blade: metsu', 'dancing edge' },
	['DEXAGI'] = { 'shark bite', 'coronach' },
	['DEXCHR'] = { 'eviseration' },
	['DEXINT'] = { 'gust slash', 'cyclone' },
	['INT']    = { 'gate of tartarus' },
	['INTMND'] = { 'spirit taker' },
	['MND']    = { 'energy steal', 'energy drain' },
	['RANGED_AGI']  = {		-- marksmanship
				   'hot shot', 'split shot', 'sniper shot', 'slugshot', 'blast shot', 
				   'heavy shot', 'detonator' 
				   },
	['RANGED_STRAGI'] = { -- archery
				   'flaming arrow', 'piercing arrow', 'dulling arrow', 'sidewinder', 
				   'blast arrow', 'arching arrow', 'empyreal arrow', 'namas arrow'
				   },
	['STR']    = { 'raging axe', 'smash axe', 'gale axe', 'avalanche axe', 'spinning axe',
				   'rampage', 'mistral axe', 'decimation', 'spinning attack', 'flat blade',
				   'circle blade', 'vorpal blade', 'hard slash', 'crescent moon', 
				   'mercy stroke', 'iron tempest', 'sturmwind', 'keen edge', 'raging rush',
				   'metatron torment', 'leg sweep', 'skewer', 'wheeling thrust',
				   'impulse drive', 'tachi: enpi', 'tachi: hobaku', 'tachi: goten',
				   'tachi: kagero', 'tachi: jinpu', 'tachi: yukikaze', 'tachi: gekko',
				   'tachi: kasha', 'tachi: kaiten', 'brainshaker', 'skullbreaker',
				   'true strike', 'heavy swing', 'shell crusher', 'full swing', 'onslaught',
				   'double thrust', 'spinning scythe', 'Vorpal Scythe' },
	['STRAGI'] = { 'sickle moon', 'vorpal thrust' },
	['STRDEX'] = { 'combo', 'backhand blow', 'raging fists', 'fast blade', 'penta thrust',
				   'blade: rin', 'blade: retsu', 'blade: jin', 'blade: ten', 'blade: ku',
				   'Geirskogul' },
	['STRINT'] = { 'dark harvest', 'shadow of death', 'nightmare scythe', 'spiral hell',
				   'burning blade', 'frostbite', 'freezebite', 'spinning slash',
				   'ground strike', 'thunder thrust', 'raiden thrust', 'blade: teki',
				   'blade: to', 'blade: chi', 'blade: ei', 'rock crusher', 'earth crusher',
				   'catastrophe' },
	['STRINT_30_20'] = { 'red lotus blade' },
	['STRMND'] = { 'guillotine', 'cross reaper', 'shining blade', 'seraph blade', 
				   'swift blade', 'savage blade', 'shockwave', 'tachi: koki', 
				   'shining strike', 'seraph strike', 'judgment', 'hexa strike', 'randgrith',
				   'retribution', 'knights of round' },
	['STRMND_30_50'] = { 'black halo' },
	['STRVIT'] = { 'shoulder tackle', 'one inch punch', 'final heaven' },
	['Skill']  = { 'starlight', 'moonlight' },
	['HP']     = { 'spirits within' }
};

-- Daily element and their Elemental weaknesses
gcinclude.tWeekDayElement = T{
	['Firesday'] =     { ['strong'] = 'fire',    ['weak'] = 'water' },
	['Earthsday'] =    { ['strong'] = 'earth',   ['weak'] = 'wind' },
	['Watersday'] =    { ['strong'] = 'water',   ['weak'] = 'thunder' },
	['Windsday'] =     { ['strong'] = 'wind',    ['weak'] = 'ice' },
	['Iceday'] =       { ['strong'] = 'ice',     ['weak'] = 'fire' },
	['Lightningday'] = { ['strong'] = 'thunder', ['weak'] = 'earth' },
	['Lightsday'] =    { ['strong'] = 'light',   ['weak'] = 'dark' },
	['Darksday'] =     { ['strong'] = 'dark',    ['weak'] = 'light' }
};

-- define coded lists
gcinclude.DaysOfTheWeek = { 
	'FIRESDAY', 'EARTHSDAY', 'WATERSDAY', 'WINDSDAY', 'ICEDAY', 'LIGHTNINGDAY', 
	'LIGHTSDAY', 'DARKSDAY' 
};
gcinclude.NotDaysOfTheWeek = { 
	'NOT_FIRESDAY', 'NOT_EARTHSDAY', 'NOT_WATERSDAY', 'NOT_WINDSDAY', 'NOT_ICEDAY', 
	'NORT_LIGHTNINGDAY', 'NOT_LIGHTSDAY', 'NOT_DARKSDAY' 
};
							   
-- define valid codes for weapon types
gcinclude.tWeapontypeMelee = { 
	'AXE', 'GAXE', 'SWORD', 'GSWORD', 'SCYTHE', 'STAVE', 'CLUB', 'H2H', 'DAGGER', 
	'KATANA', 'GKATANA', 'POLEARM' 
};
gcinclude.tWeapontypeRange = { 'ARCHERY', 'MARKSMANSHIP', 'THROWING' };
-- define constants for DT so typos aren't made
gcinclude.OFF = 'Off';
gcinclude.PHY = 'Physical';
gcinclude.MAG = 'Magical';
gcinclude.BRE = 'Breath';

-- define constants for Instrument so typos aren't made
gcinclude.HORN = 'Horn';
gcinclude.STRING = 'String';

-- Define job list that can tank
gcinclude._TankJobList = 'PLD,NIN,RUN,DRK,WAR,THF,RDM,BLU';

-- Define constants dealing with magic gear and jobs
gcinclude.ELEMENT = 'ele';
gcinclude.OBI = 'obi';
gcinclude._sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,BLU,SCH,GEO,RUN';

-- The following structure is used for locks
gcinclude.tLocks = { 
		 [1] = { ['slot'] = 'main', ['mask'] = {1,3}, ['lock'] = false },
		 [2] = { ['slot'] = 'sub', ['mask'] = {2,3}, ['lock'] = false }, 
		 [3] = { ['slot'] = 'range', ['mask'] = {4}, ['lock'] = false },
		 [4] = { ['slot'] = 'ammo', ['mask'] = {8}, ['lock'] = false },
		 [5] = { ['slot'] = 'head', ['mask'] = {16}, ['lock'] = false },  
		 [6] = { ['slot'] = 'neck', ['mask'] = {512}, ['lock'] = false },
		 [7] = { ['slot'] = 'ear1', ['mask'] = {2048,4096,6144}, ['lock'] = false },  
		 [8] = { ['slot'] = 'ear2', ['mask'] = {2048,4096,6144}, ['lock'] = false }, 
		 [9] = { ['slot'] = 'body', ['mask'] = {32}, ['lock'] = false },  
		[10] = { ['slot'] = 'hands', ['mask'] = {64}, ['lock'] = false }, 
		[11] = { ['slot'] = 'ring1', ['mask'] = {8192,16384,24576}, ['lock'] = false }, 
		[12] = { ['slot'] = 'ring2', ['mask'] = {8192,16384,24576}, ['lock'] = false },
		[13] = { ['slot'] = 'back', ['mask'] = {32768}, ['lock'] = false },  
		[14] = { ['slot'] = 'waist', ['mask'] = {1024}, ['lock'] = false },
		[15] = { ['slot'] = 'legs', ['mask'] = {128}, ['lock'] = false },  
		[16] = { ['slot'] = 'feet', ['mask'] = {256}, ['lock'] = false }
};
					
gcinclude.LocksNumeric = 'None';
gcinclude.AccNumeric = 'None';

gcinclude._AllElements = 'fire,ice,wind,earth,thunder,water,light,dark';

-- Structure for tracking elemental gear. The details that use to be in this table
-- are now found in gcinclude.GearDetails with a reference to the appropriate record
-- stored in REF
gcinclude.tElemental_gear = T{	
		['relic'] = {
			['level'] = 75,
			['type'] = 'STAVE',
			{ ['Name'] = 'Claustrum', ['Ref'] = {} }
		},
		['staff'] = {
			['level'] = 51,
			['fire'] = { 
				['Weak'] = 'water',
				['NQ'] = { ['Name'] = 'Fire staff', ['Ref'] = {} },
				['HQ'] = { ['Name'] = 'Vulcan\'s staff', ['Ref'] = {} }, 
				['Affinity'] = { 'blaze','burn','firaga','fire','flare','enfire','katon' },
				['SongAffinity'] = { 'ice threnody' },
				['Summons'] = { 'ifrit','fire spirit','firespirit','fire' }
			},
			['ice'] = {
				['Weak'] = 'fire',
				['NQ'] = { ['Name'] = 'Ice staff', ['Ref'] = {} },
				['HQ'] = {['Name'] = 'Aquilo\'s staff', ['Ref'] = {} },
				['Affinity'] = { 'blizzaga','blizzard','freeze','frost','ice','enblizzard','jubaku','hyoton','bind','distract','paralyze' },
				['SongAffinity'] = { 'wind threnody' },
				['Summons'] = { 'shiva','ice spirit','icespirit','ice' },
			},
			['wind'] = {
				['Weak'] = 'ice',
				['NQ'] = { ['Name'] = 'Wind staff', ['Ref'] = {} },
				['HQ'] = { ['Name'] = 'Auster\'s staff', ['Ref'] = {} },
				['Affinity'] = { 'aero','aeroga','choke','tornado','enaero','huton','gravity','silence' },
				['SongAffinity'] = { 'earth threnody' },
				['Summons'] = { 'garuda','air spirit','airspirit','air','siren' },
			},
			['earth'] = { 
				['Weak'] = 'wind',
				['NQ'] = { ['Name'] = 'Earth staff', ['Ref'] = {} },
				['HQ'] = { ['Name'] = 'Terra\'s staff', ['Ref'] = {} },
				['Affinity'] = { 'quake','rasp','stone','stonega','enstone','hojo','doton','slow' },
				['SongAffinity'] = { 'lightning threnody', 'battlefield elegy', 'carnage elegy' },
				['Summons'] = {'titan','earth spirit','earthspirit','earth' },
			},
			['thunder'] = {
				['Weak'] = 'earth',
				['NQ'] = { ['Name'] = 'Thunder staff', ['Ref'] = {} },
				['HQ'] = { ['Name'] = 'Jupiter\'s staff', ['Ref'] = {} },
				['Affinity'] = { 'burst','shock','thundaga','thunder','enthunder','raiton' },
				['SongAffinity'] = { 'water threnody' },
				['Summons'] = { 'ramuh','thunder spirit','thunderspirit','thunder' },
			},
			['water'] = {
				['Weak'] = 'thunder',
				['NQ'] = { ['Name'] = 'Water staff', ['Ref'] = {} },
				['HQ'] = { ['Name'] = 'Neptune\'s staff', ['Ref'] = {} },
				['Affinity'] = { 'drown','flood','poison','poisonga','water','waterga','enwater','dokumori','suiton' },
				['SongAffinity'] = { 'fire threnody' },
				['Summons'] = { 'leviathan','water spirit','waterspirit','water' },
			},
			['light'] = { 
				['Weak'] = 'dark',
				['NQ'] = { ['Name'] = 'Light staff', ['Ref'] = {} },
				['HQ'] = { ['Name'] = 'Apollo\'s staff', ['Ref'] = {} },
				['Affinity'] = { 'banish','banishga','curaga','cure','dia','diaga','flash','holy','enlight','repose','inundation' },
				['SongAffinity'] = { 'dark threnody', 'foe requiem', 'foe requiem ii', 'foe requiem iii', 'foe requiem iv', 'foe requiem v', 'foe requiem vi', 'foe lullaby', 'horde lullaby', 'magic finale', 'maiden\'s virelai' },
				['Summons'] = {'carbuncle','light spirit','lightspirit','light','cait sith','caitsith','alexander'},
			},
			['dark'] = {
				['Weak'] = 'light',
				['NQ'] = { ['Name'] = 'Dark staff', ['Ref'] = {} },
				['HQ'] = { ['Name'] = 'Pluto\'s staff', ['Ref'] = {} },
				['Affinity'] = { 'absorb','aspir','blind','bio','dispel','drain','dread','frazzle','sleep','sleepga','endark','kurayami' },
				['SongAffinity'] = { 'light threnody' },
				['Summons'] = { 'fenrir','diabolos','dark spirit','darkspirit','dark','atomos','odin' },
			},
		},
		['obi'] = {
			['level'] = 71,
			['fire'] = {
				['Weak'] = 'water',
				['Name'] = 'Karin obi', 
				['Ref'] = {},
				['MEacc'] = { 'burn','firaga','fire','flare','blaze','enfire','blaze','katon' },
				['eleWS'] = { 'burning blade','red lotus blade','tachi: Kagero','flaming arrow','hot shot','wildfire' },
			},
			['ice'] = {
				['Weak'] = 'fire',										
				['Name'] = 'Hyorin obi', 
				['Ref'] = {},
				['MEacc'] = { 'frost','blizzaga','blizzard','freeze','paralyze','bind','distract','ice','enblizzard','hyoton' },
				['eleWS'] = { 'frostbite','freezebite','herculean slash','blade: to' },
				['Other'] = 'elemental magic',
			},												 
			['wind'] = {
				['Weak'] = 'ice',
				['Name'] = 'Furin obi', 
				['Ref'] = {},
				['MEacc'] = { 'choke','aero','aeroga','tornado','silence','gravity','flurry','enaero','huton' },
				['eleWS'] = { 'gust slash','cyclone','aeolian edge','tachi: jinpu' },
			},												 
			['earth'] = { 
				['Weak'] = 'wind',
				['Name'] = 'Dorin obi', 
				['Ref'] = {},
				['MEacc'] = { 'rasp','quake','stone','stonega','slow','enstone','doton' },
				['eleWS'] = { 'blade: chi','rock crusher','earth crusher' },
			},
			['thunder'] = { 
				['Weak'] = 'earth',
				['Name'] = 'Rairin obi', 
				['Ref'] = {},
				['MEacc'] = { 'shock','burst','thundaga','thunder','stun','enthunder','raiton' },
				['eleWS'] = { 'cloudsplitter','thunder thrust','raiden thrust','tachi: goten' },
			},
			['water'] = { 
				['Weak'] = 'thunder',
				['Name'] = 'Suirin obi', 
				['Ref'] = {},
				['MEacc'] = { 'drown','flood','water','waterga','poison','enwater','suiton' },
				['eleWS'] = { 'blade: teki','blade: yu' },
				['Other'] = 'divine magic',
			},
			['light'] = {
				['Weak'] = 'dark',
				['Name'] = 'Korin obi', 
				['Ref'] = {},
				['MEacc'] = { 'banish','banishga','dia','diaga','flash','repose','holy','auspice','esuna','sacrifice','reprisal','cure','curaga','enlight' },
				['eleWS'] = { 'shining blade','seraph blade','primal rend','tachi: koki','shining strike','seraph strike','starburst','sunburst','garland of bliss','trueflight' },
				['Other'] = 'cure potency',
			},
			['dark'] = {
				['Weak'] = 'light',
				['Name'] = 'Anrin obi', 
				['Ref'] = {},
				['MEacc'] = { 'blind','bio','sleep','dispel','frazzle','drain','warp','tractor','aspir','escape','sleep','sleepga','retrace','endark' },
				['eleWS'] = { 'energy steal','energy drain','sanguine blade','dark harvest','shadow of death','infernal scythe','blade: ei','starburst','sunburst','cataclysm','vidohunir','omniscience','leaden suite' },
			},
		},
		['gorget'] = {
			['level'] = 72,
			['fire'] = { 
				['Weak'] = 'water',
				['Name'] = 'Flame gorget', 
				['Ref'] = {},
				['skillProp'] = { 'liquefaction','fusion' },
				['eleWS'] = { 'arching arrow','ascetic\'s fury','asuran fists','atonement','blade: shun','decimation','detonator','drakesbane','dulling arrow','empyreal arrow','final heaven','flaming arrow','full swing','garland of bliss','heavy shot','hexa strike','hot shot','insurgency','knights of round','last stand','mandalic stab','mistral axe','metatron torment','realmrazer','red lotus blade','scourge','shijin spiral','sniper shot','spinning attack','spinning axe','stringing pummel','tachi: kagero','tachi: kasha','upheaval','wheeling thrust' },
			},
			['ice'] = {
				['Weak'] = 'fire',
				['Name'] = 'Snow gorget', 
				['Ref'] = {},
				['skillProp'] = { 'induration','distortion' },
				['eleWS'] = { 'blade: to','blast arrow','cross reaper','death blossom','expiacion','freezebite','frostbite','full break','gate of tartarus','geirskogul','ground strike','guillotine','quietus','impulse drive','mordant rime','namas arrow','piercing arrow','pyrrhic kleos','rudra\'s storm','ruinator','raging rush','shadow of death','shattersoul','skullbreaker','smash axe','spiral hell','steel cyclone','tachi: gekko','tachi: hobaku','tachi: rana','tachi: yukikaze','tornado kick','vidohunir' },
			},
			['wind'] = {
				['Weak'] = 'ice',
				['Name'] = 'Breeze gorget', 
				['Ref'] = {},
				['skillProp'] = { 'detonation','fragmentation' },
				['eleWS'] = { 'aeolian edge','backhand blow','black halo','blade: jin','blade: kamu','blade: to','camlann\'s torment','coronach','cyclone','dancing edge','death blossom','dragon kick','earth crusher','exenterator','freezebite','gale axe','ground strike','gust slash','king\'s justice','mordant rime','raging axe','randgrith','red lotus blade','resolution','ruinator','savage blade','shark bite','shell crusher','sidewinder','slug shot','spinning slash','steel cyclone','tachi: jinpu','tachi: kaiten','taichi: shoha','taichi: yukikaze','tornado kick','trueflight','true strike','victory smite','vidohunir' },
			},
			['earth'] = {
				['Weak'] = 'wind',
				['Name'] = 'Soil gorget', 
				['Ref'] = {},
				['skillProp'] = { 'scission','gravitation' },
				['eleWS'] = { 'aeolian edge','asuran fists','avalanche axe','blade: ei','blade: ku','blade: ten','calamity','catastrophe','crescent moon','dancing edge','entropy','eviseration','exenterator','expiacion','fast blade','hard slash','impulse drive','iron tempest','king\'s justice','leaden salute','mercy stroke','nightmare scythe','omniscience','primal rend','pyrrhic kleos','rampage','requiscat','resolution','retibution','savage blade','seraph blade','shattersoul','shining blade','sickle moon','slice','spinning axe','spinning scythe','spiral hell','stardiver','stringing pummel','sturmwind','swift blade','tachi: enpi','tachi: jinpu','tachi: rana','trueflight','viper bite','vorpal blade','wasp sting' },
			},
			['thunder'] = {
				['Weak'] = 'earth',
				['Name'] = 'Thunder gorget', 
				['Ref'] = {},
				['skillProp'] = { 'impaction','fragmentation' },
				['eleWS'] = { 'aeolian edge','apex arrow','armor break','avalanche axe','black halo','blade: chi','blade: jin','blade: kamu','blade: shun','calamity','camlann\'s torment','circle blade','combo','cyclone','death blossom','dragon kick','earth crusher','exenterator','flat blade','full swing','ground strike','heavy swing','howling fist','judgement','king\'s justice','leg sweep','mordant rime','raging axe','raging fist','raiden thrust','realmrazer','resolution','rock crusher','savage blade','seraph strike','shark bite','shield break','shining strike','shoulder tackle','sickle moon','skewer','spinning attack','spinning axe','tachi: goten','tachi: koki','tachi: shoha','thunder thrust','true strike','victory smite','vidohunir','vorpal blade','weapon break' },
			},
			['water'] = {
				['Weak'] = 'thunder',
				['Name'] = 'Aqua gorget', 
				['Ref'] = {},
				['skillProp'] = { 'reverberation','distortion' },
				['eleWS'] = { 'atonement','blade: teki','brainshaker','circle blade','cross reaper','dark harvest','entropy','quietus','death blossom','decimation','expiacion','full break','garland of bliss','gate of tartarus','geirskogul','ground strike','last stand','mordant rime','namas arrow','piercing arrow','pyrrhic kleos','rudra\'s storm','primal rend','raging rush','retribution','ruinator','shadow of death','shockwave','shoulder tackle','sidewinder','skullbreaker','slug shot','smash axe','spinning scythe','spiral hell','split shot','steel cyclone','sturmwind','sunburst','tachi: gekko','tachi: koki','vidohunir','vorpal thrust' },
			},
			['light'] = {
				['Weak'] = 'dark',
				['Name'] = 'Light gorget', 
				['Ref'] = {},
				['skillProp'] = { 'transfixion','fusion','light' },
				['eleWS'] = { 'apex arrow','arching arrow','ascetic\'s fury','atonement','blade: chi','blade: ku','blade: rin','blade: shun','blast arrow','blast shot','camlann\'s torment','decimation','detonator','double thrust','drakesbane','dulling arrow','empyreal arrow','eviseration','final heaven','flaming arrow','garland of bliss','heavy shot','hexa strike','hot shot','howling fist','insurgency','knight\'s of round','leaden salute','last stand','mandalic stab','metatron torment','mistral axe','omniscience','piercing arrow','power slash','realmrazer','raiden thrust','scourge','shijin spiral','sidewinder','skewer','slug shot','sniper shot','split shot','stardiver','tachi: enpi','tachi: goten','tachi: kasha','thunder thrust','torcleaver','victory smite','upheaval','vorpal scythe','vorpal thrust','wheeling thrust' },
			},
			['dark'] = {
				['Weak'] = 'light',
				['Name'] = 'Shadow gorget', 
				['Ref'] = {},
				['skillProp'] = { 'compression','gravitation','darkness' },
				['eleWS'] = { 'asuran fists','black halo','blade: ei','blade: hi','blade: kamu','blade: ku','blade: ten','catastrophe','quietus','entropy','eviseration','impulse drive','insurgency','keen edge','leaden salute','mandalic stab','mercy stroke','requiscat','rundra\'s storm','nightmare scythe','omniscience','one inch punch','penta thrust','primal rend','retribution','shattersoul','starburst','stardiver','stringing pummel','sunburst','swift blade','tachi: kasha','tachi: rana','tachi: shoha','upheaval' },
			},
			['searched'] = false,	
		},
};

-- Listed below are spells grouped by a dependency or a type. These are 
-- root names
gcinclude.tSpell = {
	['int']		  =  { 
					'gravity','blind','sleep','sleepga','poison',
					'poisonga','bind','dispel','blaze','ice','shock', 
				     },
	['mnd']		   = {
					'paralyze','slow','slowga','frazzle','distract',
					'silence'
				     },
	['eDebuff']	   = { 'drown','burn','frost','choke','rasp','shock' },
	['barspell']   = { 
				['ele'] = {
						   'baraero','baraera','barblizzard','barblizzara',
						   'barfire','barfira','barstone','barstonera',
						   'barthunder','barthundra','barwater','barwatera' 
						  },
				['status'] = {
							'barsleep','barsleepra','barpoison','barpoisonra',
							'barparalyze','barparalyzra','barblind','barblindra',
							'barvirus','barvira','barpetrify','barpetra'
							}
					 },
	['enspell']    = {
					 'enthunder','enstone','enaero','enblizzard','enfire',
					 'enwater','enlight','endark'
				     },
	['spikes']	   = { 'blaze','ice','shock','dread' },
	['spirits']    = { 
					 'fire','firespirit','ice','icespirit','air','airspirit',
					 'earth','earthspirit','thunder','thunderspirit','water',
					 'waterspirit','light','lightspirit','dark','darkspirit' 
				     },
	['absorb']     = {
					 'absorb-agi','absorb-chr','absorb-dex','absorb-int',
					 'absorb-mnd','absorb-str','absorb-vit','absorb-acc',
					 'absorb-tp'
				     },
	['nin-buff']   = { 'tonko','utsusemi','monomi' },
	['nin-debuff'] = { 'kurayami','hojo','dokumori','jubaku' },
	['nin-ele']    = { 'katon','suiton','raiton','doton','huton','hyoton' },
	['brd-enh']	   = {
					 'minne','minuet','paeon','pastoral','madrigal','mambo',
					 'operetta','etude','ballad','march','prelude','aubade',
					 'carol','mazurka','gavotte','capriccio','fantasia',
					 'hymnus','round'
					 },
	['brd-enf']	   = { 'requiem','threnody','lullaby','finale','elegy','virelai' },
	
};

--[[
	This table contains a list of all of the spells that have multiple versions where
	the intensity is the only change. Included is what job can cast the spell and at 
	what level, MP cost, and Spell ID.
	
	Please note that entries that will be included when Treasures of Aht Urgan is released
	are currently commented out.
--]]

gcinclude.GearWarnings = nil;
gcinclude.TMtest = {
	['aero'] = {
		{ ['Name'] = 'Aero', ['Tier'] = 1, ['SID'] = 154, ['MP'] = 6, ['RDM'] = 14, ['DRK'] = 17, ['BLM'] = 9, ['SCH'] = 12, ['GEO'] = 14 },
		{ ['Name'] = 'Aero II', ['Tier'] = 2, ['SID'] = 155, ['MP'] = 22, ['RDM'] = 45, ['DRK'] = 54, ['BLM'] = 34, ['SCH'] = 38, ['GEO'] = 42 },
		{ ['Name'] = 'Aero III', ['Tier'] = 3, ['SID'] = 156, ['MP'] = 54, ['RDM'] = 69, ['BLM'] = 59, ['SCH'] = 60, ['GEO'] = 64 },
		{ ['Name'] = 'Aero IV', ['Tier'] = 4, ['SID'] = 157, ['MP'] = 115, ['BLM'] = 72, ['SCH'] = 72 }
		},
	['aeroga'] = {
		{ ['Name'] = 'Aeroga', ['Tier'] = 1, ['SID'] = 184, ['MP'] = 45, ['BLM'] = 23 },
		{ ['Name'] = 'Aeroga II', ['Tier'] = 2, ['SID'] = 185, ['MP'] = 131, ['BLM'] = 48 },
		{ ['Name'] = 'Aeroga III', ['Tier'] = 3, ['SID'] = 186, ['MP'] = 232, ['BLM'] = 67 }
		},		
	['banish'] = {
		{ ['Name'] = 'Banish', ['Tier'] = 1, ['SID'] = 28, ['MP'] = 15, ['WHM'] = 5, ['PLD'] = 7 },
		{ ['Name'] = 'Banish II', ['Tier'] = 2, ['SID'] = 29, ['MP'] = 57, ['WHM'] = 30, ['PLD'] = 34 },
		{ ['Name'] = 'Banish III', ['Tier'] = 3, ['SID'] = 30, ['MP'] = 96, ['WHM'] = 65 }
		},
	['banishga'] = {
		{ ['Name'] = 'Banishga', ['Tier'] = 1, ['SID'] = 38, ['MP'] = 41, ['WHM'] = 15, ['PLD'] = 30 },
		{ ['Name'] = 'Banishga II', ['Tier'] = 2, ['SID'] = 39, ['MP'] = 120, ['WHM'] = 40 }
		},		
	['blizzaga'] = {
		{ ['Name'] = 'Blizzaga', ['Tier'] = 1, ['SID'] = 179, ['MP'] = 80, ['BLM'] = 32 },
		{ ['Name'] = 'Blizzaga II', ['Tier'] = 2, ['SID'] = 180, ['MP'] = 175, ['BLM'] = 57 },
		{ ['Name'] = 'Blizzaga III', ['Tier'] = 3, ['SID'] = 181, ['MP'] = 297, ['BLM'] = 71 }
		},	
	['blizzard'] = {
		{ ['Name'] = 'Blizzard', ['Tier'] = 1, ['SID'] = 149, ['MP'] = 8, ['RDM'] = 24, ['DRK'] = 29, ['BLM'] = 17, ['SCH'] = 20, ['GEO'] = 24 },
		{ ['Name'] = 'Blizzard II', ['Tier'] = 2, ['SID'] = 150, ['MP'] = 31, ['RDM'] = 55, ['DRK'] = 66, ['BLM'] = 42, ['SCH'] = 46, ['GEO'] = 50 },
		{ ['Name'] = 'Blizzard III', ['Tier'] = 3, ['SID'] = 151, ['MP'] = 75, ['RDM'] = 73, ['BLM'] = 64, ['SCH'] = 66, ['GEO'] = 70 },
		{ ['Name'] = 'Blizzard IV', ['Tier'] = 4, ['SID'] = 152, ['MP'] = 162, ['BLM'] = 74, ['SCH'] = 74 }
		},	
	['cure'] = {
		{ ['Name'] = 'Cure', ['Tier'] = 1, ['SID'] = 1, ['MP'] = 8, ['WHM'] = 1, ['RDM'] = 3, ['PLD'] = 5, ['SCH'] = 5 },
		{ ['Name'] = 'Cure II', ['Tier'] = 2, ['SID'] = 2, ['MP'] = 24, ['WHM'] = 11, ['RDM'] = 14, ['PLD'] = 17, ['SCH'] = 17 },
		{ ['Name'] = 'Cure III', ['Tier'] = 3, ['SID'] = 3, ['MP'] = 46, ['WHM'] = 21, ['RDM'] = 26, ['PLD'] = 30, ['SCH'] = 30 },
		{ ['Name'] = 'Cure IV', ['Tier'] = 4, ['SID'] = 4, ['MP'] = 88, ['WHM'] = 41, ['RDM'] = 48, ['PLD'] = 55, ['SCH'] = 55 },
		{ ['Name'] = 'Cure V', ['Tier'] = 5, ['SID'] = 5, ['MP'] = 135, ['WHM'] = 61 }
		},
	['curaga'] = {
		{ ['Name'] = 'Curaga', ['Tier'] = 1, ['SID'] = 7, ['MP'] = 60, ['WHM'] = 16 },
		{ ['Name'] = 'Curaga II', ['Tier'] = 2, ['SID'] = 8, ['MP'] = 120, ['WHM'] = 31 },
		{ ['Name'] = 'Curaga III', ['Tier'] = 3, ['SID'] = 9, ['MP'] = 180, ['WHM'] = 51 },
		{ ['Name'] = 'Curaga IV', ['Tier'] = 4, ['SID'] = 10, ['MP'] = 260, ['WHM'] = 71 }
		},
	['dia'] = {
		{ ['Name'] = 'Dia', ['Tier'] = 1, ['SID'] = 23, ['MP'] = 7, ['WHM'] = 3, ['RDM'] = 1 },
		{ ['Name'] = 'Dia II', ['Tier'] = 2, ['SID'] = 24, ['MP'] = 30, ['WHM'] = 36, ['RDM'] = 31 },
		{ ['Name'] = 'Dia III', ['Tier'] = 3, ['SID'] = 25, ['MP'] = 45, ['RDM'] = 75 }
		},
	['fire'] = {
		{ ['Name'] = 'Fire', ['Tier'] = 1, ['SID'] = 144, ['MP'] = 7, ['RDM'] = 19, ['DRK'] = 23, ['BLM'] = 13, ['SCH'] = 42, ['GEO'] = 19 },
		{ ['Name'] = 'Fire II', ['Tier'] = 2, ['SID'] = 145, ['MP'] = 26, ['RDM'] = 50, ['DRK'] = 50, ['BLM'] = 38, ['SCH'] = 42, ['GEO'] = 46 },
		{ ['Name'] = 'Fire III', ['Tier'] = 3, ['SID'] = 146, ['MP'] = 63, ['RDM'] = 71, ['BLM'] = 62, ['SCH'] = 63, ['GEO'] = 67 },
		{ ['Name'] = 'Fire IV', ['Tier'] = 4, ['SID'] = 147, ['MP'] = 135, ['BLM'] = 73, ['SCH'] = 73 }
		},		
	['firaga'] = {
		{ ['Name'] = 'Firaga', ['Tier'] = 1, ['SID'] = 174, ['MP'] = 57, ['BLM'] = 28 },
		{ ['Name'] = 'Firaga II', ['Tier'] = 2, ['SID'] = 175, ['MP'] = 153, ['BLM'] = 53 },
		{ ['Name'] = 'Firaga III', ['Tier'] = 3, ['SID'] = 176, ['MP'] = 263, ['BLM'] = 69 }
		},
	['paralyze'] = {
		{ ['Name'] = 'Paralyze', ['Tier'] = 1, ['SID'] = 58, ['MP'] = 6, ['WHM'] = 4, ['RDM'] = 6 },
		{ ['Name'] = 'Paralyze II', ['Tier'] = 2, ['SID'] = 80, ['MP'] = 36, ['RDM'] = 75 }
		},
	['phalanx'] = {
		{ ['Name'] = 'Phalanx', ['Tier'] = 1, ['SID'] = 106, ['MP'] = 21, ['RDM'] = 33, ['RUN'] = 68 },
--		{ ['Name'] = 'Phalanx II', ['Tier'] = 2, ['SID'] = 107, ['MP'] = 42, ['RDM'] = 75 }
		},		
	['protect'] = {
		{ ['Name'] = 'Protect', ['Tier'] = 1, ['SID'] = 43, ['MP'] = 9, ['WHM'] = 7, ['RDM'] = 7, ['PLD'] = 10, ['SCH'] = 10, ['RUN'] = 20 },
		{ ['Name'] = 'Protect II', ['Tier'] = 2, ['SID'] = 44, ['MP'] = 28, ['WHM'] = 27, ['RDM'] = 27, ['PLD'] = 30, ['SCH'] = 30, ['RUN'] = 40 },
		{ ['Name'] = 'Protect III', ['Tier'] = 3, ['SID'] = 45, ['MP'] = 46, ['WHM'] = 47, ['RDM'] = 47, ['PLD'] = 50, ['SCH'] = 50, ['RUN'] = 60 },
		{ ['Name'] = 'Protect IV', ['Tier'] = 4, ['SID'] = 46, ['MP'] = 65, ['WHM'] = 63, ['RDM'] = 63, ['PLD'] = 70, ['SCH'] = 66 }
		},	
	['protectra'] = {
		{ ['Name'] = 'Protectra', ['Tier'] = 1, ['SID'] = 125, ['MP'] = 9, ['WHM'] = 7 },
		{ ['Name'] = 'Protectra II', ['Tier'] = 2, ['SID'] = 126, ['MP'] = 28, ['WHM'] = 27 },
		{ ['Name'] = 'Protectra III', ['Tier'] = 3, ['SID'] = 127, ['MP'] = 46, ['WHM'] = 47 },
		{ ['Name'] = 'Protectra IV', ['Tier'] = 4, ['SID'] = 128, ['MP'] = 65, ['WHM'] = 63 },
		{ ['Name'] = 'Protectra V', ['Tier'] = 5, ['SID'] = 129, ['MP'] = 84, ['WHM'] = 75 }
		},
	['raise'] = {
		{ ['Name'] = 'Raise', ['Tier'] = 1, ['SID'] = 12, ['MP'] = 150, ['WHM'] = 25, ['RDM'] = 35, ['PLD'] = 50, ['SCH'] = 35 },
		{ ['Name'] = 'Raise II', ['Tier'] = 2, ['SID'] = 13, ['MP'] = 150, ['WHM'] = 56, ['SCH'] = 70 },
		{ ['Name'] = 'Raise III', ['Tier'] = 3, ['SID'] = 140, ['MP'] = 150, ['WHM'] = 70 }
		},
	['regen'] = {
		{ ['Name'] = 'Regen', ['Tier'] = 1, ['SID'] = 108, ['MP'] = 15, ['WHM'] = 21, ['RDM'] = 21, ['SCH'] = 18, ['RUN'] = 23 },
		{ ['Name'] = 'Regen II', ['Tier'] = 2, ['SID'] = 110, ['MP'] = 36, ['WHM'] = 44, ['SCH'] = 37, ['RUN'] = 48 },
		{ ['Name'] = 'Regen III', ['Tier'] = 3, ['SID'] = 111, ['MP'] = 64, ['WHM'] = 66, ['SCH'] = 59, ['RUN'] = 70 }
		},
	['reraise'] = {
		{ ['Name'] = 'Reraise', ['Tier'] = 1, ['SID'] = 135, ['MP'] = 150, ['WHM'] = 25, ['PLD'] = 35 },
		{ ['Name'] = 'Reraise II', ['Tier'] = 2, ['SID'] = 141, ['MP'] = 150, ['WHM'] = 56, ['SCH'] = 70 },
		{ ['Name'] = 'Reraise III', ['Tier'] = 3, ['SID'] = 142, ['MP'] = 150, ['WHM'] = 70 }
		},
	['shell'] = {
		{ ['Name'] = 'Shell', ['Tier'] = 1, ['SID'] = 48, ['MP'] = 18, ['WHM'] = 17, ['RDM'] = 17, ['PLD'] = 20, ['SCH'] = 20, ['RUN'] = 10 },
		{ ['Name'] = 'Shell II', ['Tier'] = 2, ['SID'] = 49, ['MP'] = 37, ['WHM'] = 37, ['RDM'] = 37, ['PLD'] = 40, ['SCH'] = 40, ['RUN'] = 30 },
		{ ['Name'] = 'Shell III', ['Tier'] = 3, ['SID'] = 50, ['MP'] = 56, ['WHM'] = 57, ['RDM'] = 57, ['PLD'] = 60, ['SCH'] = 60, ['RUN'] = 50 },
		{ ['Name'] = 'Shell IV', ['Tier'] = 4, ['SID'] = 51, ['MP'] = 75, ['WHM'] = 68, ['RDM'] = 68, ['SCH'] = 71, ['RUN'] = 70 }
		},
	['shellra'] = {
		{ ['Name'] = 'Shellra', ['Tier'] = 1, ['SID'] = 130, ['MP'] = 18, ['WHM'] = 17 },
		{ ['Name'] = 'Shellra II', ['Tier'] = 2, ['SID'] = 131, ['MP'] = 37, ['WHM'] = 37 },
		{ ['Name'] = 'Shellra III', ['Tier'] = 3, ['SID'] = 132, ['MP'] = 56, ['WHM'] = 57 },
		{ ['Name'] = 'Shellra IV', ['Tier'] = 4, ['SID'] = 133, ['MP'] = 75, ['WHM'] = 68 },
		{ ['Name'] = 'Shellra V', ['Tier'] = 5, ['SID'] = 134, ['MP'] = 93, ['WHM'] = 75 }
		},
	['slow'] = {
		{ ['Name'] = 'Slow', ['Tier'] = 1, ['SID'] = 59, ['MP'] = 12, ['WHM'] = 13, ['RDM'] = 13 },
		{ ['Name'] = 'Slow II', ['Tier'] = 2, ['SID'] = 79, ['MP'] = 45, ['RDM'] = 75 }
		},
	['stonega'] = {
		{ ['Name'] = 'Stonega', ['Tier'] = 1, ['SID'] = 189, ['MP'] = 24, ['BLM'] = 15 },
		{ ['Name'] = 'Stonega II', ['Tier'] = 2, ['SID'] = 190, ['MP'] = 93, ['BLM'] = 40 },
		{ ['Name'] = 'Stonega III', ['Tier'] = 3, ['SID'] = 191, ['MP'] = 175, ['BLM'] = 63 }
		},		
	['stone'] = {
		{ ['Name'] = 'Stone', ['Tier'] = 1, ['SID'] = 159, ['MP'] = 4, ['RDM'] = 4, ['DRK'] = 5, ['BLM'] = 1, ['SCH'] = 4, ['GEO'] = 4 },
		{ ['Name'] = 'Stone II', ['Tier'] = 2, ['SID'] = 160, ['MP'] = 16, ['RDM'] = 35, ['DRK'] = 42, ['BLM'] = 26, ['SCH'] = 30, ['GEO'] = 34 },
		{ ['Name'] = 'Stone III', ['Tier'] = 3, ['SID'] = 161, ['MP'] = 40, ['RDM'] = 65, ['BLM'] = 51, ['SCH'] = 54, ['GEO'] = 58 },
		{ ['Name'] = 'Stone IV', ['Tier'] = 4, ['SID'] = 162, ['MP'] = 88, ['BLM'] = 68, ['SCH'] = 70 }
		},	
	['thundaga'] = {
		{ ['Name'] = 'Thundaga', ['Tier'] = 1, ['SID'] = 194, ['MP'] = 105, ['BLM'] = 36 },
		{ ['Name'] = 'Thundaga II', ['Tier'] = 2, ['SID'] = 195, ['MP'] = 200, ['BLM'] = 61 },
		{ ['Name'] = 'Thundaga III', ['Tier'] = 3, ['SID'] = 196, ['MP'] = 332, ['BLM'] = 73 }
		},
	['thunder'] = {
		{ ['Name'] = 'Thunder', ['Tier'] = 1, ['SID'] = 164, ['MP'] = 9, ['RDM'] = 29, ['DRK'] = 35, ['BLM'] = 21, ['SCH'] = 24, ['GEO'] = 23 },
		{ ['Name'] = 'Thunder II', ['Tier'] = 2, ['SID'] = 165, ['MP'] = 37, ['RDM'] = 60, ['DRK'] = 72, ['BLM'] = 46, ['SCH'] = 51, ['GEO'] = 54 },
		{ ['Name'] = 'Thunder III', ['Tier'] = 3, ['SID'] = 166, ['MP'] = 91, ['RDM'] = 75, ['BLM'] = 66, ['SCH'] = 69, ['GEO'] = 73 },
		{ ['Name'] = 'Thunder IV', ['Tier'] = 4, ['SID'] = 167, ['MP'] = 194, ['BLM'] = 75, ['SCH'] = 75 }
		},
	['water'] = {
		{ ['Name'] = 'Water', ['Tier'] = 1, ['SID'] = 169, ['MP'] = 5, ['RDM'] = 9, ['DRK'] = 11, ['BLM'] = 5, ['SCH'] = 8, ['GEO'] = 9 },
		{ ['Name'] = 'Water II', ['Tier'] = 2, ['SID'] = 170, ['MP'] = 19, ['RDM'] = 40, ['DRK'] = 48, ['BLM'] = 30, ['SCH'] = 34, ['GEO'] = 38 },
		{ ['Name'] = 'Water III', ['Tier'] = 3, ['SID'] = 171, ['MP'] = 46, ['RDM'] = 67, ['BLM'] = 55, ['SCH'] = 57, ['GEO'] = 61 },
		{ ['Name'] = 'Water IV', ['Tier'] = 4, ['SID'] = 172, ['MP'] = 99, ['BLM'] = 70, ['SCH'] = 71 }
		},
	['watera'] = {
		{ ['Name'] = 'Watera', ['Tier'] = 1, ['SID'] = 199, ['MP'] = 34, ['BLM'] = 19 },
		{ ['Name'] = 'Watera II', ['Tier'] = 2, ['SID'] = 200, ['MP'] = 112, ['BLM'] = 44 },
		{ ['Name'] = 'Watera III', ['Tier'] = 3, ['SID'] = 201, ['MP'] = 202, ['BLM'] = 65 }
		},
	['flare'] = {
		{ ['Name'] = 'Flare', ['Tier'] = 1, ['SID'] = 204, ['MP'] = 315, ['BLM'] = 60 },
--		{ ['Name'] = 'Flare II', ['Tier'] = 2, ['SID'] = 205, ['MP'] = 280, ['BLM'] = 75 }
		},
	['freeze'] = {
		{ ['Name'] = 'Freeze', ['Tier'] = 1, ['SID'] = 206, ['MP'] = 315, ['BLM'] = 50 },
--		{ ['Name'] = 'Freeze II', ['Tier'] = 2, ['SID'] = 207, ['MP'] = 280, ['BLM'] = 75 }
		},
	['tornado'] = {
		{ ['Name'] = 'Tornado', ['Tier'] = 1, ['SID'] = 208, ['MP'] = 315, ['BLM'] = 52 },
--		{ ['Name'] = 'Tornado II', ['Tier'] = 2, ['SID'] = 209, ['MP'] = 280, ['BLM'] = 75 }
		},
	['quake'] = {
		{ ['Name'] = 'Quake', ['Tier'] = 1, ['SID'] = 210, ['MP'] = 315, ['BLM'] = 54 },
--		{ ['Name'] = 'Quake II', ['Tier'] = 2, ['SID'] = 211, ['MP'] = 280, ['BLM'] = 75 }
		},
	['burst'] = {
		{ ['Name'] = 'Burst', ['Tier'] = 1, ['SID'] = 212, ['MP'] = 315, ['BLM'] = 56 },
--		{ ['Name'] = 'Burst II', ['Tier'] = 2, ['SID'] = 213, ['MP'] = 280, ['BLM'] = 75 }
		},
	['flood'] = {
		{ ['Name'] = 'Flood', ['Tier'] = 1, ['SID'] = 214, ['MP'] = 315, ['BLM'] = 58 },
--		{ ['Name'] = 'Flood II', ['Tier'] = 2, ['SID'] = 215, ['MP'] = 280, ['BLM'] = 75 }
		},
	['poison'] = {
		{ ['Name'] = 'Poison', ['Tier'] = 1, ['SID'] = 220, ['MP'] = 5, ['RDM'] = 5, ['BLM'] = 3, ['DRK'] = 6 },
		{ ['Name'] = 'Poison II', ['Tier'] = 2, ['SID'] = 221, ['MP'] = 38, ['RDM'] = 46, ['BLM'] = 43, ['DRK'] = 46 }
		},
	['poisonga'] = {
		{ ['Name'] = 'Poisonga', ['Tier'] = 1, ['SID'] = 225, ['MP'] = 5, ['BLM'] = 24, ['DRK'] = 26 },
		{ ['Name'] = 'Poison II', ['Tier'] = 2, ['SID'] = 226, ['MP'] = 112, ['BLM'] = 64, ['DRK'] = 66 }
		},
	['bio'] = {
		{ ['Name'] = 'Bio', ['Tier'] = 1, ['SID'] = 230, ['MP'] = 15, ['RDM'] = 10, ['BLM'] = 10, ['DRK'] = 15 },
		{ ['Name'] = 'Bio II', ['Tier'] = 2, ['SID'] = 231, ['MP'] = 36, ['RDM'] = 36, ['BLM'] = 35, ['DRK'] = 40 },
--		{ ['Name'] = 'Bio III', ['Tier'] = 3, ['SID'] = 232, ['MP'] = 36, ['RDM'] = 75 }
		},
	['drain'] = {
		{ ['Name'] = 'Drain', ['Tier'] = 1, ['SID'] = 245, ['MP'] = 21, ['SCH'] = 21, ['BLM'] = 12, ['DRK'] = 10 },
--		{ ['Name'] = 'Drain II', ['Tier'] = 2, ['SID'] = 246, ['MP'] = 37, ['DRK'] = 62 }
		},
	['sleep'] = {
		{ ['Name'] = 'Sleep', ['Tier'] = 1, ['SID'] = 253, ['MP'] = 19, ['RDM'] = 25, ['SCH'] = 30, ['BLM'] = 20, ['DRK'] = 30, ['GEO'] = 35 },
		{ ['Name'] = 'Sleep II', ['Tier'] = 2, ['SID'] = 259, ['MP'] = 29, ['RDM'] = 46, ['SCH'] = 65, ['BLM'] = 41, ['DRK'] = 56, ['GEO'] = 70 }
		},
	['sleepga'] = {
		{ ['Name'] = 'Sleepga', ['Tier'] = 1, ['SID'] = 273, ['MP'] = 38, ['BLM'] = 31 },
		{ ['Name'] = 'Sleepga II', ['Tier'] = 2, ['SID'] = 274, ['MP'] = 58, ['BLM'] = 56 }
		},
	['blind'] = {
		{ ['Name'] = 'Blind', ['Tier'] = 1, ['SID'] = 254, ['MP'] = 5, ['RDM'] = 8, ['BLM'] = 4 },
--		{ ['Name'] = 'Blind II', ['Tier'] = 2, ['SID'] = 276, ['MP'] = 31, ['BLM'] = 75 }
		},
	['enfire'] = {
		{ ['Name'] = 'Enfire', ['Tier'] = 1, ['SID'] = 100, ['MP'] = 12, ['RDM'] = 24 },
--		{ ['Name'] = 'Enfire II', ['Tier'] = 2, ['SID'] = 312, ['MP'] = 24, ['RDM'] = 58 }
		},
	['enblizzard'] = {
		{ ['Name'] = 'Enblizzard', ['Tier'] = 1, ['SID'] = 101, ['MP'] = 12, ['RDM'] = 22 },
--		{ ['Name'] = 'Enblizzard II', ['Tier'] = 2, ['SID'] = 313, ['MP'] = 24, ['RDM'] = 56 }
		},
	['enaero'] = {
		{ ['Name'] = 'Enaero', ['Tier'] = 1, ['SID'] = 102, ['MP'] = 12, ['RDM'] = 20 },
--		{ ['Name'] = 'Enaero II', ['Tier'] = 2, ['SID'] = 314, ['MP'] = 24, ['RDM'] = 52 }
		},
	['enstone'] = {
		{ ['Name'] = 'Enstone', ['Tier'] = 1, ['SID'] = 103, ['MP'] = 12, ['RDM'] = 18 },
--		{ ['Name'] = 'Enstone II', ['Tier'] = 2, ['SID'] = 315, ['MP'] = 24, ['RDM'] = 52 }
		},
	['enthunder'] = {
		{ ['Name'] = 'Enthunder', ['Tier'] = 1, ['SID'] = 104, ['MP'] = 12, ['RDM'] = 16 },
--		{ ['Name'] = 'Enthunder II', ['Tier'] = 2, ['SID'] = 316, ['MP'] = 24, ['RDM'] = 50 }
		},
	['enwater'] = {
		{ ['Name'] = 'Enwater', ['Tier'] = 1, ['SID'] = 105, ['MP'] = 12, ['RDM'] = 27 },
--		{ ['Name'] = 'Enwater II', ['Tier'] = 2, ['SID'] = 317, ['MP'] = 24, ['RDM'] = 60 }
		},
	['katon'] = {
		{ ['Name'] = 'Katon: Ichi', ['Tier'] = 1, ['SID'] = 320, ['MP'] = 0, ['NIN'] = 15 },
		{ ['Name'] = 'Katon: Ni', ['Tier'] = 2, ['SID'] = 321, ['MP'] = 0, ['NIN'] = 40 }
--		{ ['Name'] = 'Katon: San', ['Tier'] = 3, ['SID'] = 322, ['MP'] = 0, ['NIN'] = 75 }
		},
	['hyoton'] = {
		{ ['Name'] = 'Hyoton: Ichi', ['Tier'] = 1, ['SID'] = 323, ['MP'] = 0, ['NIN'] = 15 },
		{ ['Name'] = 'Hyoton: Ni', ['Tier'] = 2, ['SID'] = 324, ['MP'] = 0, ['NIN'] = 40 }
--		{ ['Name'] = 'Hyoton: San', ['Tier'] = 3, ['SID'] = 325, ['MP'] = 0, ['NIN'] = 75 }
		},
	['huton'] = {
		{ ['Name'] = 'Huton: Ichi', ['Tier'] = 1, ['SID'] = 326, ['MP'] = 0, ['NIN'] = 15 },
		{ ['Name'] = 'Huton: Ni', ['Tier'] = 2, ['SID'] = 327, ['MP'] = 0, ['NIN'] = 40 }
--		{ ['Name'] = 'Huton: San', ['Tier'] = 3, ['SID'] = 328, ['MP'] = 0, ['NIN'] = 75 }
		},
	['doton'] = {
		{ ['Name'] = 'Doton: Ichi', ['Tier'] = 1, ['SID'] = 329, ['MP'] = 0, ['NIN'] = 15 },
		{ ['Name'] = 'Doton: Ni', ['Tier'] = 2, ['SID'] = 330, ['MP'] = 0, ['NIN'] = 40 }
--		{ ['Name'] = 'Doton: San', ['Tier'] = 3, ['SID'] = 331, ['MP'] = 0, ['NIN'] = 75 }
		},
	['raiton'] = {
		{ ['Name'] = 'Raiton: Ichi', ['Tier'] = 1, ['SID'] = 332, ['MP'] = 0, ['NIN'] = 15 },
		{ ['Name'] = 'Raiton: Ni', ['Tier'] = 2, ['SID'] = 333, ['MP'] = 0, ['NIN'] = 40 }
--		{ ['Name'] = 'Raiton: San', ['Tier'] = 3, ['SID'] = 334, ['MP'] = 0, ['NIN'] = 75 }
		},
	['suiton'] = {
		{ ['Name'] = 'Suiton: Ichi', ['Tier'] = 1, ['SID'] = 335, ['MP'] = 0, ['NIN'] = 15 },
		{ ['Name'] = 'Suiton: Ni', ['Tier'] = 2, ['SID'] = 336, ['MP'] = 0, ['NIN'] = 40 }
--		{ ['Name'] = 'Suiton: San', ['Tier'] = 3, ['SID'] = 337, ['MP'] = 0, ['NIN'] = 75 }
		},
	['utsusemi'] = {
		{ ['Name'] = 'Utsusemi: Ichi', ['Tier'] = 1, ['SID'] = 338, ['MP'] = 0, ['NIN'] = 12 },
		{ ['Name'] = 'Utsusemi: Ni', ['Tier'] = 2, ['SID'] = 339, ['MP'] = 0, ['NIN'] = 37 }
		},
	['hojo'] = {
		{ ['Name'] = 'Hojo: Ichi', ['Tier'] = 1, ['SID'] = 344, ['MP'] = 0, ['NIN'] = 23 },
		{ ['Name'] = 'Hojo: Ni', ['Tier'] = 2, ['SID'] = 345, ['MP'] = 0, ['NIN'] = 48 }
		},
	['tonko'] = {
		{ ['Name'] = 'Tonko: Ichi', ['Tier'] = 1, ['SID'] = 344, ['MP'] = 0, ['NIN'] = 9 },
		{ ['Name'] = 'Tonko: Ni', ['Tier'] = 2, ['SID'] = 345, ['MP'] = 0, ['NIN'] = 34 }
		},
	};

--[[
	Like TMtest, TStest lists all of the songs that a bard can cast that have
	multiple tiers. The differences between spells and songs though necessitate
	splitting it out on it's own.
--]]

gcinclude.TStest = {
	['requiem'] = {
		{ ['Name'] = 'Foe Requiem', ['Tier'] = 1, ['SID'] = 368, ['Lvl'] = 7 },
		{ ['Name'] = 'Foe Requiem II', ['Tier'] = 2, ['SID'] = 369, ['Lvl'] = 17 },
		{ ['Name'] = 'Foe Requiem III', ['Tier'] = 3, ['SID'] = 370, ['Lvl'] = 37 },
		{ ['Name'] = 'Foe Requiem IV', ['Tier'] = 4, ['SID'] = 371, ['Lvl'] = 47 },
		{ ['Name'] = 'Foe Requiem V', ['Tier'] = 5, ['SID'] = 372, ['Lvl'] = 57 },
		{ ['Name'] = 'Foe Requiem VI', ['Tier'] = 6, ['SID'] = 373, ['Lvl'] = 67 }
		},
	['paeon'] = {
		{ ['Name'] = 'Army\'s Paeon', ['Tier'] = 1, ['SID'] = 378, ['Lvl'] = 5 },
		{ ['Name'] = 'Army\'s Paeon II', ['Tier'] = 2, ['SID'] = 379, ['Lvl'] = 15 },
		{ ['Name'] = 'Army\'s Paeon III', ['Tier'] = 3, ['SID'] = 380, ['Lvl'] = 35 },
		{ ['Name'] = 'Army\'s Paeon IV', ['Tier'] = 4, ['SID'] = 381, ['Lvl'] = 45 },
		{ ['Name'] = 'Army\'s Paeon V', ['Tier'] = 5, ['SID'] = 382, ['Lvl'] = 65 }
		},
	['ballad'] = {
		{ ['Name'] = 'Mage\'s Ballad', ['Tier'] = 1, ['SID'] = 386, ['Lvl'] = 25 },
		{ ['Name'] = 'Mage\'s Ballad II', ['Tier'] = 2, ['SID'] = 387, ['Lvl'] = 55 }
		},
	['minne'] = {
		{ ['Name'] = 'Knight\'s Minne', ['Tier'] = 1, ['SID'] = 389, ['Lvl'] = 1 },
		{ ['Name'] = 'Knight\'s Minne II', ['Tier'] = 2, ['SID'] = 390, ['Lvl'] = 21 },
		{ ['Name'] = 'Knight\'s Minne III', ['Tier'] = 3, ['SID'] = 391, ['Lvl'] = 41 },
		{ ['Name'] = 'Knight\'s Minne IV', ['Tier'] = 4, ['SID'] = 392, ['Lvl'] = 61 }
		},		
	['minuet'] = {
		{ ['Name'] = 'Valor Minuet', ['Tier'] = 1, ['SID'] = 394, ['Lvl'] = 3 },
		{ ['Name'] = 'Valor Minuet II', ['Tier'] = 2, ['SID'] = 395, ['Lvl'] = 23 },
		{ ['Name'] = 'Valor Minuet III', ['Tier'] = 3, ['SID'] = 396, ['Lvl'] = 43 },
		{ ['Name'] = 'Valor Minuet IV', ['Tier'] = 4, ['SID'] = 397, ['Lvl'] = 63 }
		},
	['madrigal'] = {
		{ ['Name'] = 'Sword Madrigal', ['Tier'] = 1, ['SID'] = 399, ['Lvl'] = 1 },
		{ ['Name'] = 'Blade Madrigal', ['Tier'] = 2, ['SID'] = 400, ['Lvl'] = 51 }
		},
	['mambo'] = {
		{ ['Name'] = 'Sheepfoe Mambo', ['Tier'] = 1, ['SID'] = 403, ['Lvl'] = 13 },
		{ ['Name'] = 'Dragonfoe Mambo', ['Tier'] = 2, ['SID'] = 404, ['Lvl'] = 53 }
		},
	['elegy'] = {
		{ ['Name'] = 'Battlefield Elegy', ['Tier'] = 1, ['SID'] = 421, ['Lvl'] = 39 },
		{ ['Name'] = 'Carnage Elegy', ['Tier'] = 2, ['SID'] = 422, ['Lvl'] = 59 }
		},
	['march'] = {
		{ ['Name'] = 'Advancing March', ['Tier'] = 1, ['SID'] = 419, ['Lvl'] = 29 },
		{ ['Name'] = 'Victory March', ['Tier'] = 2, ['SID'] = 420, ['Lvl'] = 60 }
		},
	};

-- Temporary holding variables for the main hand and off hand weapons
gcinclude.weapon = nil;
gcinclude.offhand = nil;

-- Table of all BST pet food
gcinclude.tPetFood = {
	[1] = { ['name'] = 'pet food alpha',  ['lvl'] = 12, ['have'] = false },
	[2] = { ['name'] = 'pet food beta',   ['lvl'] = 24, ['have'] = false },
	[3] = { ['name'] = 'pet fd. gamma',   ['lvl'] = 36, ['have'] = false },
	[4] = { ['name'] = 'pet food delta',  ['lvl'] = 48, ['have'] = false },
	[5] = { ['name'] = 'pet fd. epsilon', ['lvl'] = 60, ['have'] = false },
	[6] = { ['name'] = 'pet food zeta',   ['lvl'] = 72, ['have'] = false }
};
gcinclude._PetFoodCount = 6;

-- This is a list of all player storage containers available in FFXI.
-- Quite a number of them are not valid on HorizonXI yet.

gcinclude.STORAGES = {
    [1] = { ['id'] = 0,  ['name'] = 'Inventory' },
    [2] = { ['id'] = 1,  ['name'] = 'Safe' },
    [3] = { ['id'] = 2,  ['name'] = 'Storage' },
    [4] = { ['id'] = 3,  ['name'] = 'Temporary' },
    [5] = { ['id'] = 4,  ['name'] = 'Locker' },
    [6] = { ['id'] = 5,  ['name'] = 'Satchel' },
    [7] = { ['id'] = 6,  ['name'] = 'Sack' },
    [8] = { ['id'] = 7,  ['name'] = 'Case' },
    [9] = { ['id'] = 8,  ['name'] = 'Wardrobe' },
    [10]= { ['id'] = 9,  ['name'] = 'Safe 2' },
    [11]= { ['id'] = 10, ['name'] = 'Wardrobe 2' },
    [12]= { ['id'] = 11, ['name'] = 'Wardrobe 3' },
    [13]= { ['id'] = 12, ['name'] = 'Wardrobe 4' },
    [14]= { ['id'] = 13, ['name'] = 'Wardrobe 5' },
    [15]= { ['id'] = 14, ['name'] = 'Wardrobe 6' },
    [16]= { ['id'] = 15, ['name'] = 'Wardrobe 7' },
    [17]= { ['id'] = 16, ['name'] = 'Wardrobe 8' }
};

-- List of items that are commonly equipped for teleporting, exp boosts, reraise, etc
gcinclude.tEquipIt = {
	['emp']    = { ['Name'] = 'Empress Band', ['Slot'] = 'Ring' },
	['cha']    = { ['Name'] = 'Chariot Band', ['Slot'] = 'Ring' },
	['empo']   = { ['Name'] = 'Emperor Band', ['Slot'] = 'Ring' },
	['ann']    = { ['Name'] = 'Anniversary Ring', ['Slot'] = 'Ring' },
	['dem']    = { ['Name'] = 'Dem Ring', ['Slot'] = 'Ring' },
	['mea']    = { ['Name'] = 'Mea Ring', ['Slot'] = 'Ring' },
	['holla']  = { ['Name'] = 'Holla Ring', ['Slot'] = 'Ring' },
	['altep']  = { ['Name'] = 'Altep Ring', ['Slot'] = 'Ring' },	
	['yhoat']  = { ['Name'] = 'Yhoat Ring', ['Slot'] = 'Ring' },	
	['vahzl']  = { ['Name'] = 'Vahzl Ring', ['Slot'] = 'Ring' },
	['home']   = { ['Name'] = 'Homing Ring', ['Slot'] = 'Ring' },
	['ret']    = { ['Name'] = 'Return Ring', ['Slot'] = 'Ring' },
	['tav']    = { ['Name'] = 'Tavnazian Ring', ['Slot'] = 'Ring' },
	['dcl']    = { ['Name'] = 'Dcl.Grd. Ring', ['Slot'] = 'Ring' },
	['warp']   = { ['Name'] = 'Warp Cudgel', ['Slot'] = 'Main' },
	['trick2'] = { ['Name'] = 'Trick Staff II', ['Slot'] = 'Main' },
	['treat2'] = { ['Name'] = 'Treat Staff II', ['Slot'] = 'Main' },
	['purgo']  = { ['Name'] = 'Wonder Top +1', ['Slot'] = 'Body' },
	['rre']    = { ['Name'] = 'Reraise Earring', ['Slot'] = 'Ear' },		
	['rrg']    = { ['Name'] = 'Reraise Gorget', ['Slot'] = 'Neck' },
	['rrh']    = { ['Name'] = 'Reraise Hairpin', ['Slot'] = 'Head' },
	['mandy']  = { ['Name'] = 'Mandra. Suit', ['Slot'] = 'Body' },
	['gob']    = { ['Name'] = 'Goblin Suit', ['Slot'] = 'Body' },	
};

-- List of items that inhibit more than the obvious gear slot. Add entries as you
-- need to, to account for the gear you use. Please note that ears and rings are
-- not supported. Instead, you have to be explicit (eg. ring1, ring2, ear1, ear2)
gcinclude.multiSlot = {
	{ ['item'] = 'Vermillion Cloak', ['slot'] = 'Body', ['affected'] = 'Head' },
	{ ['item'] = 'Royal Cloak', 	 ['slot'] = 'Body', ['affected'] = 'Head' },
	{ ['item'] = 'Mandra. Suit',	 ['slot'] = 'Body', ['affected'] = 'Legs' },
	{ ['item'] = 'Taru. Shorts',	 ['slot'] = 'Legs', ['affected'] = 'Feet' },
	{ ['item'] = 'Taru. Shorts +1',  ['slot'] = 'Legs', ['affected'] = 'Feet' },
	{ ['item'] = 'Tarutaru Top',	 ['slot'] = 'Body', ['affected'] = 'Hands' },
	{ ['item'] = 'Tarutaru Top +1',  ['slot'] = 'Body', ['affected'] = 'Hands' },
	{ ['item'] = 'Goblin Suit',      ['slot'] = 'Body', ['affected'] = 'Hands,Feet' },
};

-- This is the list of storage containers that can be equipped from outside of a moghouse
gcinclude.EQUIPABLE = { 
			gcinclude.STORAGES[1],		-- Inventory
			gcinclude.STORAGES[9],		-- Wardrobe
			gcinclude.STORAGES[11],		-- Wardrobe 2
			gcinclude.STORAGES[17]		-- Wardrobe 8
};

gcinclude.EQUIPABLE_LIST = {
			gcinclude.STORAGES[1]['id'],	-- Inventory
			gcinclude.STORAGES[9]['id'],	-- Wardrobe
			gcinclude.STORAGES[11]['id'],	-- Wardrobe 2
			gcinclude.STORAGES[17]['id']	-- Wardrobe 8
};
			
gcinclude.EQUIPABLE_NONHOLIDAY = {
			gcinclude.STORAGES[1],		-- Inventory
			gcinclude.STORAGES[9],		-- Wardrobe
			gcinclude.STORAGES[11]		-- Wardrobe 2
};

gcinclude.NON_GEAR = {
			gcinclude.STORAGES[1],		-- Inventory
			gcinclude.STORAGES[2],		-- Safe
			gcinclude.STORAGES[3],		-- Storage
			gcinclude.STORAGES[5],		-- Locker
			gcinclude.STORAGES[6],		-- Satchel
			gcinclude.STORAGES[7],		-- Sack
			gcinclude.STORAGES[8],		-- Case
			gcinclude.STORAGES[10],		-- Safe 2
};

-- This is the job masks for gear that can be equipped. I have included all jobs
-- including those not yet in the game on Horizon XI and the place holder jobs
-- for future new jobs so that I don't have to keep the complete list around... lol.
gcinclude.JobMask = { ['None'] = 0x0,
		['WAR'] = 0x2, ['MNK'] = 0x4, ['WHM'] = 0x8, ['BLM'] = 0x10,
		['RDM'] = 0x20, ['THF'] = 0x40, ['PLD'] = 0x80, ['DRK'] = 0x100,
		['BST'] = 0x200, ['BRD'] = 0x400, ['RNG'] = 0x800, ['SAM'] = 0x1000,
		['NIN'] = 0x2000, ['DRG'] = 0x4000, ['SMN'] = 0x8000, ['BLU'] = 0x10000,
		['COR'] = 0x20000, ['PUP'] = 0x40000, ['DNC'] = 0x80000, ['SCH'] = 0x100000,
		['GEO'] = 0x200000, ['RUN'] = 0x400000, ['MON'] = 0x800000, 
		['JOB24'] = 0x1000000, ['JOB25'] = 0x2000000, ['JOB26'] = 0x4000000,
		['JOB27'] = 0x8000000, ['JOB28'] = 0x10000000, ['JOB29'] = 0x20000000,
		['JOB30'] = 0x30000000, ['JOB31'] = 0x80000000, ['Alljobs'] = 0x007FFFFE };

-- The following is used to track regional control. Listed is a region, who has
-- conquest control, and what zone id's are associated with the region. This
-- structure is populated programmatically. 1 - San d'Orian, 2 - Bastokian, 3 -
-- Windurstian, 0 - not applicable, -1 unassigned.
gcinclude.RegionControl = {
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

-- This structure tracks which storage slips you own and where they can be
-- found. This list is currently restricted to those porter slips that are
-- viable through ToAU. The storage slip IDs and item IDs were gotten from
-- Windower4 libs/slips.lua and were checked in the AirSkyBoat source (at
-- least the slip ids).
gcinclude.Slips = {
	[1]  = { 
			['name'] = 'Storage Slip 01', ['id'] = 29312, 	-- Salvage,Nyzul,Einherjar,Assault
			['location'] = nil,
			['items'] = {
			    16084,14546,14961,15625,15711,16085,14547,14962,15626,15712,16086, 
				14548,14963,15627,15713,16087,14549,14964,15628,15714,16088,14550,
				14965,15629,15715,16089,14551,14966,15630,15716,16090,14552,14967,
				15631,15717,16091,14553,14968,15632,15718,16092,14554,14969,15633,
				15719,16093,14555,14970,15634,15720,16094,14556,14971,15635,15721,
				16095,14557,14972,15636,15722,16096,14558,14973,15637,15723,16097,
				14559,14974,15638,15724,16098,14560,14975,15639,15725,16099,14561,
				14976,15640,15726,16100,14562,14977,15641,15727,16101,14563,14978,
				15642,15728,16102,14564,14979,15643,15729,16103,14565,14980,15644,
				15730,16106,14568,14983,15647,15733,16107,14569,14984,15648,15734,
				16108,14570,14985,15649,15735,16602,17741,18425,18491,18588,18717,
				18718,18850,18943,16069,14530,14940,15609,15695,16062,14525,14933,
				15604,15688,16064,14527,14935,15606,15690,18685,18065,17851,18686,
				18025,18435,18113,17951,17715,18485,18408,18365,18583,18417,18388, 
				16267,16268,16269,16228,16229,15911,15799,15800,15990,17745,18121,
				16117,14577,17857 }, -- 168 items
			['own'] = false, ['extra'] = nil },
	[2]  = { 
			['name'] = 'Storage Slip 02', ['id'] = 29313,	-- 73-75 Abjuratory,Tu'lia,Lumoria,Limbus,Unity Leader Shirts
			['location'] = nil,
			['items'] = { 
				12421,12549,12677,12805,12933,13911,14370,14061,14283,14163,12429,
				12557,12685,12813,12941,13924,14371,14816,14296,14175,13934,14387,
				14821,14303,14184,13935,14388,14822,14304,14185,13876,13787,14006,
				14247,14123,13877,13788,14007,14248,14124,13908,14367,14058,14280,
				14160,13909,14368,14059,14281,14161,16113,14573,14995,15655,15740,
				16115,14575,14997,15657,15742,16114,14574,14996,15656,15741,16116,
				14576,14998,15658,15743,12818,18198,12946,18043,12690,17659,12296,
				12434,15471,15472,15473,15508,15509,15510,15511,15512,15513,15514,
				17710,17595,18397,18360,18222,17948,18100,15475,15476,15477,15488,
				15961,14815,14812,14813,15244,15240,14488,14905,15576,15661,15241,
				14489,14906,15577,15662,13927,14378,14076,14308,14180,13928,14379,
				14077,14309,14181,10438,10276,10320,10326,10367,10439,10277,10321,
				10327,10368,10440,10278,10322,10328,10369,10441,10279,10323,10329,
				10370,25734,25735,25736,25737,25738,25739,25740,25741,25742,25743,
				25744 },	-- 155 items
			['own'] = false, ['extra'] = nil },
	[3]  = { 
			['name'] = 'Storage Slip 03', ['id'] = 29314,	-- Zeni,Campaign,Void Watch,Twilight,Vorac. Resurg. Rewards
			['location'] = nil,			
			['items'] = { 
			    16155,11282,15021,16341,11376,16156,11283,15022,16342,11377,16157,
				11284,15023,16343,11378,16148,14590,15011,16317,15757,16143,14591,
				15012,16318,15758,16146,14588,15009,16315,15755,16147,14589,15010,
				16316,15756,15966,15967,19041,17684,17685,11636,15844,15934,16258,
				18735,18734,16291,16292,19042,15935,16293,16294,15936,18618,11588,
				11545,16158,16159,16160,16149,14583,15007,16314,15751,16141,14581,
				15005,16312,15749,16142,14582,15006,16313,15750,10876,10450,10500,
				11969,10600,10877,10451,10501,11970,10601,10878,10452,10502,11971,
				10602,19132,18551,11798,11362,11363,11625,15959,16259,22299,26414,
				21623,26219,21886,23792,23793,23794,23795,23796,22032,22043 }, -- 109 items
			 ['own'] = false, ['extra'] = nil },
	[4]  = { 
			 ['name'] = 'Storage Slip 04', ['id'] = 29315,	-- AF
			 ['location'] = nil,
			 ['items'] = { 
				12511,12638,13961,14214,14089,12512,12639,13962,14215,14090,13855,
				12640,13963,14216,14091,13856,12641,13964,14217,14092,12513,12642,
				13965,14218,14093,12514,12643,13966,14219,14094,12515,12644,13967,
				14220,14095,12516,12645,13968,14221,14096,12517,12646,13969,14222,
				14097,13857,12647,13970,14223,14098,12518,12648,13971,14099,14224,
				13868,13781,13972,14225,14100,13869,13782,13973,14226,14101,12519,
				12649,13974,14227,14102,12520,12650,13975,14228,14103,15265,14521,
				14928,15600,15684,15266,14522,14929,15601,15685,15267,14523,14930,
				15602,15686,16138,14578,15002,15659,15746,16139,14579,15003,15660,
				15747,16140,14580,15004,16311,15748,16678,17478,17422,17423,16829,
				16764,17643,16798,16680,16766,17188,17812,17771,17772,16887,17532,
				17717,18702,17858,19203,21461,21124,20776,27786,27926,28066,28206,
				28346,27787,27927,28067,28207,28347 }, -- 138 items
			 ['own'] = false, ['extra'] = nil },
	[5]  = { 
			 ['name'] = 'Storage Slip 05', ['id'] = 29316, 	-- AF+1
			 ['location'] = nil,
			 ['items'] = { 
			    15225,14473,14890,15561,15352,15226,14474,14891,15562,15353,15227,
				14475,14892,15563,15354,15228,14476,14893,15564,15355,15229,14477,
				14894,15565,15356,15230,14478,14895,15566,15357,15231,14479,14896,
				15567,15358,15232,14480,14897,15568,15359,15233,14481,14898,15569,
				15360,15234,14482,14899,15570,15361,15235,14483,14900,15362,15571,
				15236,14484,14901,15572,15363,15237,14485,14902,15573,15364,15238,
				14486,14903,15574,15365,15239,14487,14904,15575,15366,11464,11291,
				15024,16345,11381,11467,11294,15027,16348,11384,11470,11297,15030,
				16351,11387,11475,11302,15035,16357,11393,11476,11303,15036,16358,
				11394,11477,11304,15037,16359,11395}, -- 105 items
			 ['own'] = false, ['extra'] = nil },
	[6]  = { 
			 ['name'] = 'Storage Slip 06', ['id'] = 29317, 	-- Relic
			 ['location'] = nil,
			 ['items'] = { 
				15072,15087,15102,15117,15132,15871,15073,15088,15103,15118,15133,
				15478,15074,15089,15104,15119,15134,15872,15075,15090,15105,15120,
				15135,15874,15076,15091,15106,15121,15136,15873,15077,15092,15107,
				15122,15137,15480,15078,15093,15108,15123,15138,15481,15079,15094,
				15109,15124,15139,15479,15080,15095,15110,15125,15140,15875,15081,
				15096,15111,15126,15141,15482,15082,15097,15112,15127,15142,15876,
				15083,15098,15113,15128,15143,15879,15084,15099,15114,15129,15144,
				15877,15085,15100,15115,15130,15145,15878,15086,15101,15116,15131,
				15146,15484,11465,11292,15025,16346,11382,16244,11468,11295,15028,
				16349,11385,15920,11471,11298,15031,16352,11388,16245,11478,11305,
				15038,16360,11396,16248,11480,11307,15040,16362,11398,15925}, -- 120 items			 
			 ['own'] = false, ['extra'] = nil },
	[7]  = { 
			 ['name'] = 'Storage Slip 07', ['id'] = 29318, 	-- Relic+1
			 ['location'] = nil,
			 ['items'] = {
				15245,14500,14909,15580,15665,15246,14501,14910,15581,15666,15247,
				14502,14911,15582,15667,15248,14503,14912,15583,15668,15249,14504,
				14913,15584,15669,15250,14505,14914,15585,15670,15251,14506,14915,
				15586,15671,15252,14507,14916,15587,15672,15253,14508,14917,15588,
				15673,15254,14509,14918,15589,15674,15255,14510,14919,15590,15675,
				15256,14511,14920,15591,15676,15257,14512,14921,15592,15677,15258,
				14513,14922,15593,15678,15259,14514,14923,15594,15679,11466,11293,
				15026,16347,11383,11469,11296,15029,16350,11386,11472,11299,15032,
				16353,11389,11479,11306,15039,16361,11397,11481,11308,15041,16363,
				11399}, -- 100 items
			 ['own'] = false, ['extra'] = nil },
	[8]  = { 
			 ['name'] = 'Storage Slip 11', ['id'] = 29322, 	-- Scenario Rewards,Events
			 ['location'] = nil,
			 ['items'] = {
				15297,15298,15299,15919,15929,15921,18871,16273,18166,18167,18256,
				13216,13217,13218,15455,15456,181,182,183,184,129,11499,18502,
				18855,19274,18763,19110,15008,17764,19101,365,366,367,15860,272,
				273,274,275,276,11853,11956,11854,11957,11811,11812,11861,11862,
				3676,18879,3647,3648,3649,3677,18880,18863,18864,15178,14519,
				10382,11965,11967,15752,15179,14520,10383,11966,11968,15753,10875,
				3619,3620,3621,3650,3652,10430,10251,10593,10431,10252,10594,
				10432,10253,10595,10433,10254,10596,10429,10250,17031,17032,10807,
				18881,10256,10330,10257,10331,10258,10332,10259,10333,10260,10334, 
				10261,10335,10262,10336,10263,10337,10264,10338,10265,10339,10266,
				10340,10267,10341,10268,10342,10269,10343,10270,10344,10271,10345,
				10446,10447,426,10808,3654,265,266,267,269,270,271,18464,18545,
				18563,18912,18913,10293,10809,10810,10811,10812,27803,28086,27804,
				28087,27805,28088,27806,28089,27765,27911,27760,27906,27759,28661,
				286,27757,27758,287,27899,28185,28324,27898,28655,27756,28511,
				21118,27902,100,21117,87,20953,21280,28652,28650,27726,28509,
				28651,27727,28510,27872,21113,27873,21114,20532,20533,27717,27718 }, -- 192 items
			 ['own'] = false, ['extra'] = nil },
	[9]  = { 
			 ['name'] = 'Storage Slip 12', ['id'] = 29323,	-- Relic-1
			 ['location'] = nil,
			 ['items'] = {
				2033,2034,2035,2036,2037,2038,2039,2040,2041,2042,2043,2044,2045,
				2046,2047,2048,2049,2050,2051,2052,2053,2054,2055,2056,2057,2058,
				2059,2060,2061,2062,2063,2064,2065,2066,2067,2068,2069,2070,2071,
				2072,2073,2074,2075,2076,2077,2078,2079,2080,2081,2082,2083,2084,
				2085,2086,2087,2088,2089,2090,2091,2092,2093,2094,2095,2096,2097,
				2098,2099,2100,2101,2102,2103,2104,2105,2106,2107,2662,2663,2664,
				2665,2666,2667,2668,2669,2670,2671,2672,2673,2674,2675,2676,2718,
				2719,2720,2721,2722,2723,2724,2725,2726,2727}, -- 100 items
			 ['own'] = false, ['extra'] = nil },
	[10]  = { 
			 ['name'] = 'Storage Slip 19', ['id'] = 29330,	-- Scenario Rewards II, Events
			 ['location'] = nil,
			 ['items'] = {
				27715,27866,27716,27867,278,281,284,3680,3681,27859,28149,27860,
				28150,21107,21108,27625,27626,26693,26694,26707,26708,27631,27632,
				26705,26706,27854,27855,26703,26704,3682,3683,3684,3685,3686,3687,
				3688,3689,3690,3691,3692,3693,3694,3695,21097,21098,26717,26718,
				26728,26719,26720,26889,26890,21095,21096,26738,26739,26729,26730,
				26788,26946,27281,27455,26789,3698,20713,20714,26798,26954,26799,
				26955,3706,26956,26957,3705,26964,26965,27291,26967,27293,26966,
				27292,26968,27294,21153,21154,21086,21087,25606,26974,27111,27296,
				27467,25607,26975,27112,27297,27468,14195,14830,14831,13945,13946,
				14832,13947,17058,13948,14400,14392,14393,14394,14395,14396,14397,
				14398,14399,11337,11329,11330,11331,11332,11333,11334,11335,11336,
				15819,15820,15821,15822,15823,15824,15825,15826,3670,3672,3661,
				3595,3665,3668,3663,3674,3667,191,28,153,151,198,202,142,134,137,
				340,341,334,335,337,339,336,342,338,3631,3632,3625,3626,3628,3630,
				3627,3633,3629,25632,25633,25604,25713,27325,25714,27326,3651,25711,
				25712,10925,10948,10949,10950,10951,10952,10953,10954,10955,25657,
				25756,25658,25757,25909}, -- 192 items
			 ['own'] = false, ['extra'] = nil },
	[11]  = { 
			 ['name'] = 'Storage Slip 22', ['id'] = 29333,	-- Scenario Rewards III, Events
			 ['location'] = nil,
			 ['items'] = {
				25639,25715,25638,3707,3708,21074,26406,25645,25726,25648,25649,
				25650,25758,25759,25672,25673,282,279,280,268,25670,25671,26520,
				25652,25669,22017,22018,25586,25587,10384,10385,22019,22020,25722,
				25585,25776,25677,25678,25675,25679,20668,20669,22069,25755,3722,
				21608,3713,3714,3715,3717,3727,3728,20577,3726,20666,20667,21741, 
				21609,3723,26410,26411,25850,21509,3725,3720,21658,26524,20665,
				26412,21965,21966,21967,25774,25838,25775,25839,3724,3721,21682,
				22072,21820,21821,23731,26517,23730,20573,20674,21742,21860,22065,
				22039,22124,22132,3719,3738,26518,27623,21867,21868,22283,26516,
				20933,20578,20568,3739,20569,20570,22288,26352,23737,22282,3740,
				26545,21977,21978,3742,26519,26514,26515,3743,21636,23753,23754,
				54,25910,20571,23790,23791,26489,22153,22154,20574,20675,21743,
				21861,22066,3748,21638,23800,23801,3749,3750,22045,22046,3751,
				26490,26546,22047,22048,22049,23803,23804,22051,22052,3752,23805,
				25911,25912,3753,23806,3754,23810,23811,3755,26496,26497,21786,
				21787,21996,21997,23802,25777,25778,21933,21934,21993,21994,23866,
				20670,21537,21538,21572,23870,21760,23871,23872,23873,23874,20691}, -- 189 Items
			 ['own'] = false, ['extra'] = nil },			 		 
--	['name'] = 'Storage Slip 08'	-- Empyrean
--	['name'] = 'Storage Slip 09'	-- Empyrean+1
--	['name'] = 'Storage Slip 10'	-- Empyrean+2
--	['name'] = 'Storage Slip 13'	-- Relic+2 if augmented
--	['name'] = 'Storage Slip 14'	-- 99 Nyzul,Einherjar,Salvage,Domain
--	['name'] = 'Storage Slip 15'	-- Reforged AF
--	['name'] = 'Storage Slip 16'	-- Reforged AF+1
--	['name'] = 'Storage Slip 17'	-- Reforged Relic
--	['name'] = 'Storage Slip 18'	-- Reforged Relic+1
--	['name'] = 'Storage Slip 20'	-- Reforged Empyrean
--	['name'] = 'Storage Slip 21'	-- Reforged Empyrean+1
--	['name'] = 'Storage Slip 23'	-- Ambuscade Equipment
--	['name'] = 'Storage Slip 24'	-- Reforged AF+2
--	['name'] = 'Storage Slip 25'	-- Reforged AF+3
--	['name'] = 'Storage Slip 26'	-- Reforged Relic+2
--	['name'] = 'Storage Slip 27'	-- Reforged Relic+3
--	['name'] = 'Storage Slip 28'	-- Ambuscade Weapons/Equipment
--	['name'] = 'Storage Slip 29'	-- Reforged Empyrean+2
--	['name'] = 'Storage Slip 30',	-- Reforged Empyrean+3
--	['name'] = 'Storage Slip 31'	-- Scenario Rewards IV, Events
};

-- This structure defines all the claim slips and what items are associated
-- with them through ToAU. It is used to determine if an item in a gear set
-- is stored on the Claim Storage NPC
gcinclude.ClaimSlips = {
	-- Level 24-30
	{ ['name'] = 'Iron Chainmail Set', ['kid'] = 1920, ['own'] = false, 
		['ids'] = {12424,12552,12680,12808,12936} },
	{ ['name'] = 'Shade Harness Set', ['kid'] = 1921, ['own'] = false, 
		['ids'] = {15165,14426,14858,14327,15315} },
	{ ['name'] = 'Brass Scale Armor Set', ['kid'] = 1922, ['own'] = false, 
		['ids'] = {12433,12561,12689,12817,12945} },
	{ ['name'] = 'Wool Robe Set', ['kid'] = 1923, ['own'] = false, 
		['ids'] = {12474,12602,12730,12858,12986} },
	{ ['name'] = 'Eisenplatte Set', ['kid'] = 1924, ['own'] = false, 
		['ids'] = {15167,14431,14860,14329,15317} },
	{ ['name'] = 'Soil Gi Set', ['kid'] = 1925, ['own'] = false, 
		['ids'] = {12458,12586,12714,12842,12970} },
	{ ['name'] = 'Seer\'s Tunic Set', ['kid'] = 1926, ['own'] = false, 
		['ids'] = {15163,14424,14856,14325,15313} },
	{ ['name'] = 'Studded Armor Set', ['kid'] = 1927, ['own'] = false, 
		['ids'] = {12442,12570,12698,12826,12954} },
	{ ['name'] = 'Centurion\'s Scale Mail Set', ['kid'] = 1928, ['own'] = false, 
		['ids'] = {12438,12566,12694,12822,12950} },
	{ ['name'] = 'Mercenary Captain\'s Doublet Set', ['kid'] = 1929, ['own'] = false, 
		['ids'] = {12470,12598,12726,12854,12982} },
	{ ['name'] = 'Garish Tunic Set', ['kid'] = 1930, ['own'] = false, 
		['ids'] = {15164,14425,14857,14326,15314} },
	{ ['name'] = 'Noct Doublet Set', ['kid'] = 1931, ['own'] = false, 
		['ids'] = {15161,14422,14854,14323,15311} },
	-- Level 31-40
	{ ['name'] = 'Custom Armor Set (H.M.)', ['kid'] = 1932, ['own'] = false, 
		['ids'] = {12654,12761,12871,13015} },
	{ ['name'] = 'Custom Armor Set (H.F.)', ['kid'] = 1933, ['own'] = false, 
		['ids'] = {12655,12762,12872,13016} },
	{ ['name'] = 'Magna Armor Set (E.M.)', ['kid'] = 1934, ['own'] = false, 
		['ids'] = {12656,12763,12873,13017} },
	{ ['name'] = 'Magna Armor Set (E.F.)', ['kid'] = 1935, ['own'] = false, 
		['ids'] = {12657,12764,12874,13018} },
	{ ['name'] = 'Wonder Armor Set (Taru)', ['kid'] = 1936, ['own'] = false, 
		['ids'] = {12658,12765,12875,13019} },
	{ ['name'] = 'Savage Armor Set (Mithra)', ['kid'] = 1937, ['own'] = false, 
		['ids'] = {12659,12766,12876,13020} },
	{ ['name'] = 'Elder Armor Set (Galka)', ['kid'] = 1938, ['own'] = false, 
		['ids'] = {12660,12767,12877,13021} },
	{ ['name'] = 'Linen Cloak Set', ['kid'] = 1939, ['own'] = false, 
		['ids'] = {12610,12738,12866,12994} },
	{ ['name'] = 'Padded Armor Set', ['kid'] = 1940, ['own'] = false, 
		['ids'] = {12450,12578,12706,12836,12962} },
	{ ['name'] = 'Silver Chainmail Set', ['kid'] = 1941, ['own'] = false, 
		['ids'] = {12425,12553,12681,12809,12937} },
	{ ['name'] = 'Gambison Set', ['kid'] = 1942, ['own'] = false, 
		['ids'] = {12466,12594,12722,12850,12978} },
	{ ['name'] = 'Iron Scail Mail Set', ['kid'] = 1943, ['own'] = false, 
		['ids'] = {13871,13783,14001,14243,14118} },
	{ ['name'] = 'Cuir Armor Set', ['kid'] = 1944, ['own'] = false, 
		['ids'] = {12443,12571,12699,12827,12955} },
	{ ['name'] = 'Velvet Robe Set', ['kid'] = 1945, ['own'] = false, 
		['ids'] = {12475,12603,12731,12859,12987} },
	{ ['name'] = 'Opaline Dress Set', ['kid'] = 1946, ['own'] = false, 
		['ids'] = {13931,14384,14249,14116} },
	{ ['name'] = 'Royal Squire\'s Chainmail Set', ['kid'] = 1947, ['own'] = false, 
		['ids'] = {12431,12559,12687,12815,12943} },
	{ ['name'] = 'Plate Armor Set', ['kid'] = 1948, ['own'] = false, 
		['ids'] = {12416,12544,12672,12800,12928} },
	{ ['name'] = 'Combat Castor\'s Set', ['kid'] = 1949, ['own'] = false, 
		['ids'] = {12614,12743,12870,12998} },
	{ ['name'] = 'Argent Set', ['kid'] = 1059, ['own'] = false, 
		['ids'] = {11310,16365} },
	-- Level 41-50
	{ ['name'] = 'Alumine Haubert Set', ['kid'] = 1950, ['own'] = false, 
		['ids'] = {15205,14444,14051,15402,15341} },
	{ ['name'] = 'Carapace Armor Set', ['kid'] = 1951, ['own'] = false, 
		['ids'] = {13711,13712,13713,12837,13715} },
	{ ['name'] = 'Banded Mail Set', ['kid'] = 1952, ['own'] = false, 
		['ids'] = {12426,12554,12682,12810,12938} },
	{ ['name'] = 'Hara-Ate Set', ['kid'] = 1953, ['own'] = false, 
		['ids'] = {12459,12587,12715,12843,12974} },
	{ ['name'] = 'Raptor Armor Set', ['kid'] = 1954, ['own'] = false, 
		['ids'] = {12444,12572,12700,12828,12956} },
	{ ['name'] = 'Steel Scale Mail Set', ['kid'] = 1955, ['own'] = false, 
		['ids'] = {13873,13785,14003,14245,14120} },
	{ ['name'] = 'Wool Gambison Set', ['kid'] = 1956, ['own'] = false, 
		['ids'] = {12467,12595,12723,12851,12979} },
	{ ['name'] = 'Shinobi Gi Set', ['kid'] = 1957, ['own'] = false, 
		['ids'] = {12460,12588,12716,12844,12972} },
	{ ['name'] = 'Mythril Plate Armor Set', ['kid'] = 1962, ['own'] = false, 
		['ids'] = {12417,12545,12673,12801,12929} },
	{ ['name'] = 'Iron Musketeer\'s Cuirass Set', ['kid'] = 1958, ['own'] = false, 
		['ids'] = {12422,12550,12678,12806,12934} },
	{ ['name'] = 'Tactician Magician\'s Cloak Set', ['kid'] = 1959, ['own'] = false, 
		['ids'] = {12478,12606,12734,12862,12990} },
	{ ['name'] = 'White Cloak Set', ['kid'] = 1960, ['own'] = false, 
		['ids'] = {12611, 12739, 12867, 12995} },
	{ ['name'] = 'Austere Robe Set', ['kid'] = 1961, ['own'] = false, 
		['ids'] = {13939,13814,14826,14310,14189} },
	{ ['name'] = 'Crow Jupon Set', ['kid'] = 1963, ['own'] = false, 
		['ids'] = {15242,14498,14907,15578,15663} },
	-- Artifact Armor/High Level Gear
	{ ['name'] = 'Fighter\'s Armor Set', ['kid'] = 654, ['own'] = false, 
		['ids'] = {12511,12638,13961,14214,14089} },
	{ ['name'] = 'Temple Attire Set', ['kid'] = 655, ['own'] = false, 
		['ids'] = {12512,12639,13962,14215,14090} },
	{ ['name'] = 'Healer\'s Attire Set', ['kid'] = 656, ['own'] = false, 
		['ids'] = {13855,12640,13963,14216,14091} },
	{ ['name'] = 'Wizard\'s Attire Set', ['kid'] = 657, ['own'] = false, 
		['ids'] = {13856,12641,13964,14217,14092} },
	{ ['name'] = 'Warlock\'s Attire Set', ['kid'] = 658, ['own'] = false, 
		['ids'] = {12513,12642,13965,14218,14093} },
	{ ['name'] = 'Rogue\'s Attire Set', ['kid'] = 659, ['own'] = false, 
		['ids'] = {12514,12643,13966,14219,14094} },
	{ ['name'] = 'Gallant Armor Set', ['kid'] = 660, ['own'] = false, 
		['ids'] = {12515,12644,13967,14220,14095} },
	{ ['name'] = 'Chaos Armor Set', ['kid'] = 661, ['own'] = false, 
		['ids'] = {12516,12645,13968,14221,14096} },
	{ ['name'] = 'Beast Armor Set', ['kid'] = 662, ['own'] = false, 
		['ids'] = {12517,12646,13969,14222,14097} },
	{ ['name'] = 'Choral Armor Set', ['kid'] = 663, ['own'] = false, 
		['ids'] = {13857,12647,13970,14223,14098} },
	{ ['name'] = 'Hunter\'s Attire Set', ['kid'] = 664, ['own'] = false, 
		['ids'] = {12518,12648,13971,14224,14099} },
	{ ['name'] = 'Myochin Armor Set', ['kid'] = 665, ['own'] = false, 
		['ids'] = {13868,13781,13972,14225,14100} },
	{ ['name'] = 'Ninja\'s Garb Set', ['kid'] = 666, ['own'] = false, 
		['ids'] = {13869,13782,13973,14226,14101} },
	{ ['name'] = 'Drachen Armor Set', ['kid'] = 667, ['own'] = false, 
		['ids'] = {12519,12649,13974,14227,14102} },
	{ ['name'] = 'Evoker\'s Attire Set', ['kid'] = 668, ['own'] = false, 
		['ids'] = {12520,12650,13975,14228,14103} },
	{ ['name'] = 'Magus Attire Set', ['kid'] = 1964, ['own'] = false, 
		['ids'] = {15265,14521,14928,15600,15684} },
	{ ['name'] = 'Corsair\'s Attire Set', ['kid'] = 1965, ['own'] = false, 
		['ids'] = {15266,14522,14929,15601,15685} },
	{ ['name'] = 'Puppetry Attire Set', ['kid'] = 1966, ['own'] = false, 
		['ids'] = {15267,14523,14930,15602,15686} },
	{ ['name'] = 'Dancer Attire Set (M)', ['kid'] = 1967, ['own'] = false, 
		['ids'] = {16138,14578,15002,15659,15746} },
	{ ['name'] = 'Dancer Attire Set (F)', ['kid'] = 1968, ['own'] = false, 
		['ids'] = {16139,14579,15003,15660,15747} },
	{ ['name'] = 'Scholar\'s Attire Set', ['kid'] = 1969, ['own'] = false, 
		['ids'] = {16140,14580,15004,16311,15748} },
	{ ['name'] = 'Amir Armor Set', ['kid'] = 1970, ['own'] = false, 
		['ids'] = {16062,14525,14933,15604,15688} },
	{ ['name'] = 'Pahluwan Armor Set', ['kid'] = 1971, ['own'] = false, 
		['ids'] = {16069,14530,14940,15609,15695} },
	{ ['name'] = 'Yigit Armor Set', ['kid'] = 1972, ['own'] = false, 
		['ids'] = {16064,14527,14935,15606,15690} },
	-- Relic Armor
	{ ['name'] = 'Warrior\'s Armor Set', ['kid'] = 861, ['own'] = false, 
		['ids'] = {15072,15087,15102,15117,15132} },
	{ ['name'] = 'Melee Attire Set', ['kid'] = 862, ['own'] = false, 
		['ids'] = {15073,15088,15103,15118,15133} },
	{ ['name'] = 'Cleric\'s Attire Set', ['kid'] = 863, ['own'] = false, 
		['ids'] = {15074,15089,15104,15119,15134} },
	{ ['name'] = 'Sorcerer\'s Attire Set', ['kid'] = 864, ['own'] = false, 
		['ids'] = {15075,15090,15105,15120,15135} },
	{ ['name'] = 'Duelist\'s Armor Set', ['kid'] = 865, ['own'] = false, 
		['ids'] = {15076,15091,15106,15121,15136} },
	{ ['name'] = 'Assassin\'s Attire Set', ['kid'] = 866, ['own'] = false, 
		['ids'] = {15077,15092,15107,15122,15137} },
	{ ['name'] = 'Valor Armor Set', ['kid'] = 867, ['own'] = false, 
		['ids'] = {15078,15093,15108,15123,15138} },
	{ ['name'] = 'Abyss Armor Set', ['kid'] = 868, ['own'] = false, 
		['ids'] = {15079,15094,15109,15124,15139} },
	{ ['name'] = 'Monster Armor Set', ['kid'] = 869, ['own'] = false, 
		['ids'] = {15080,15095,15110,15125,15140} },
	{ ['name'] = 'Bard\'s Attire Set', ['kid'] = 870, ['own'] = false, 
		['ids'] = {15081,15096,15111,15126,15141} },
	{ ['name'] = 'Scout\'s Attire Set', ['kid'] = 871, ['own'] = false, 
		['ids'] = {15082,15097,15112,15127,15142} },
	{ ['name'] = 'Saotome Armor Set', ['kid'] = 872, ['own'] = false, 
		['ids'] = {15083,15098,15113,15128,15143} },
	{ ['name'] = 'Koga Garb Set', ['kid'] = 873, ['own'] = false, 
		['ids'] = {15084,15099,15114,15129,15144} },
	{ ['name'] = 'Wyrm Armor Set', ['kid'] = 874, ['own'] = false, 
		['ids'] = {15085,15100,15115,15130,15145} },
	{ ['name'] = 'Summoner\'s Attire Set', ['kid'] = 875, ['own'] = false, 
		['ids'] = {15086,15101,15116,15131,15146} },
	{ ['name'] = 'Mirage Attire Set', ['kid'] = 1054, ['own'] = false, 
		['ids'] = {11465,11292,15025,16346,11382} },
	{ ['name'] = 'Commodore Attire Set', ['kid'] = 1055, ['own'] = false, 
		['ids'] = {11468,11295,15028,16349,11385} },
	{ ['name'] = 'Pantin Attire Set', ['kid'] = 1056, ['own'] = false, 
		['ids'] = {11471,11298,15031,16352,11388} },
	{ ['name'] = 'Etoile Attire Set', ['kid'] = 1057, ['own'] = false, 
		['ids'] = {11478,11305,15038,16360,11396} },
	{ ['name'] = 'Argute Attire Set', ['kid'] = 1058, ['own'] = false, 
		['ids'] = {11480,11307,15040,16362,11398} }
};

-- This structure will be dynamically populated by the GearCheck function.
-- The slots will have a set structure providing details about every gear
-- piece in the job file/gcinclude so that when checking for the piece of
-- gear, the details that would require looking up item details will already
-- be known, thus avoiding successive server requests. It is hoped this will
-- help alleviate some of the lag. (Each piece is identified by name and 
-- includes the item's level, if it can be worn by the player's job, and 
-- is the piece accessible out of the moghouse. The item ID number will
-- also be tracked for verification purposes, but not included in the search.)
gcinclude.GearDetails = {
	['main']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },		
	['sub']   = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
	['range'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
	['ammo']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
	['head']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
	['neck']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
	['ears']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
	['body']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
	['hands'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
	['rings'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
	['back']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
	['waist'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
	['legs']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
	['feet']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} }
};
	
gcinclude.OwnNation = -1; 
gcinclude.basetime = os.time();
gcinclude.tGearLine = {};

gcinclude.Sets = gcinclude.sets;

--[[
	The following event is used to capture the ownership of the regions.
	Conquest updates are sent whenever the player zones and periodiaclly.
	The display bar's region is updated accordingly
--]]

ashita.events.register('packet_in', 'packet_in_callback1', function (e)

	if (e.id == 0x05E) then
		gcinclude.RegionControl['Ronfaure']['own'] = struct.unpack('B', e.data, 0X1E)
		gcinclude.RegionControl['Zulkheim']['own'] = struct.unpack('B', e.data, 0x22)
		gcinclude.RegionControl['Norvallen']['own'] = struct.unpack('B', e.data, 0x26)
		gcinclude.RegionControl['Gustaberg']['own'] = struct.unpack('B', e.data, 0x2A)
		gcinclude.RegionControl['Derfland']['own'] = struct.unpack('B', e.data, 0x2E)
		gcinclude.RegionControl['Sarutabaruta']['own'] = struct.unpack('B', e.data, 0x32)
		gcinclude.RegionControl['Kolshushu']['own'] = struct.unpack('B', e.data, 0x36)
		gcinclude.RegionControl['Argoneau']['own'] = struct.unpack('B', e.data, 0x3A)
		gcinclude.RegionControl['Fauregandi']['own'] = struct.unpack('B', e.data, 0x3E)
		gcinclude.RegionControl['Valdeaunia']['own'] = struct.unpack('B', e.data, 0x42)
		gcinclude.RegionControl['QuifimIsland']['own'] = struct.unpack('B', e.data, 0x46)
		gcinclude.RegionControl['LiTelor']['own'] = struct.unpack('B', e.data, 0x4A)
		gcinclude.RegionControl['Kuzotz']['own'] = struct.unpack('B', e.data, 0x4E)
		gcinclude.RegionControl['Vollbow']['own'] = struct.unpack('B', e.data, 0x52)
		gcinclude.RegionControl['ElshimoLowlands']['own'] = struct.unpack('B', e.data, 0x56)
		gcinclude.RegionControl['ElshimoUplands']['own'] = struct.unpack('B', e.data, 0x5A)
		gcinclude.RegionControl['Tulia']['own'] = struct.unpack('B', e.data, 0x5E)
		gcinclude.RegionControl['Movapolos']['own'] = struct.unpack('B', e.data, 0x62)
		gcinclude.RegionControl['Tavnazia']['own'] = struct.unpack('B', e.data, 0x66)
		if gcdisplay ~= nil then
			RegionDisplay();
		end	
		e.blocked = false;
	end
end);

--[[
	DisplayVerion shows the name of the addon and the version along with the
	changelog since the last release.
--]]

function DisplayVersion()
	local bSkip = false;
	local rfn = gProfile.FilePath:reverse();
	
	-- remove the job file from path, add changelog
	rfn = string.sub(rfn,string.find(rfn,'\\'),-1);
	rfn = rfn:reverse() .. 'Documentation\\changelog.txt';

	print(chat.message(' '));	
	print(chat.message(version.name .. ' Version: ' .. tostring(version.version)));
	for line in io.lines (rfn) do
		if bSkip == false then
			print(chat.message(' '));
			bSkip = true;
		end
		print(chat.message(line));
	end
end		-- DisplayVersion

--[[
	StartReminder is a simple routine used to delay the printing of a reminder from
	the start of running this code. It compares a base time (is seconds) with "now"
	and after 15 secs prints the reminder, then disables itself.
--]]

function gcinclude.StartReminder()
	local iTestVal = gcinclude.settings.bMinBasetime;
	local iNow = os.time();
	
	if gcdisplay.GetGC() == true then
		return;
	end

	if gcinclude.settings.bGCReminder == true then
		-- Since reminder already shown once, change the wait
		-- interval from 15 seconds to 5 minutes
		iTestVal = gcinclude.settings.bMaxBasetime;
	end
	
	if os.difftime(iNow,gcinclude.basetime) >= iTestVal then
		print(chat.message('************'));
		if iTestVal == gcinclude.settings.bMinBasetime then
			print(chat.message('FYI: Remember to do a /gc once \'data download\' finishes'));
		else
			print(chat.message('FYI: Remember to do a /gc'));
		end
		print(chat.message('************'));
		gcinclude.settings.bGCReminder = true;
		-- Change the base to current so that comparison is from
		-- now forward
		gcinclude.basetime = iNow;
	end
end			-- gcinclude.StartReminder

--[[
	fReferenceCheck determines if any of the passed gear is actually
	a reference to another set's slot.
--]]

function fReferenceCheck(ts)
	local t = {};
	local bFound = false;
	
	if ts == nil then
		return false;
	end

	if type(ts) == 'string' then
		t[1] = ts;
	else
		t = ts;
	end
	
	for i,j in pairs(t) do
		if string.find(j,'::') then
			bFound = true;
			break;
		end
	end
	return bFound;
end		-- fReferenceCheck

--[[
	TallyProgressiveCaps determines how many stages are defined in the 
	Progressive entries structure: accuracy, tank accuracy, ranged
	accuracy, and tank ranged accuracy.
--]]

function gcinclude.TallyProgressiveCaps()
	local macc = 0;
	local mtacc = 0;
	local mracc = 0;
	local mtracc = 0;
	
	if gProfile.Sets.Progressive ~= nil then
		if gProfile.Sets.Progressive['Accuracy'] ~= nil then
			for i,j in pairs(gProfile.Sets.Progressive['Accuracy']) do
				macc = macc + 1;
			end	
		end

		if gProfile.Sets.Progressive['Tank_Accuracy'] ~= nil then
			for i,j in pairs(gProfile.Sets.Progressive['Tank_Accuracy']) do
				mtacc = mtacc + 1;
			end
		else
			mtacc = macc;	-- If tank_accuracy missing, use accuracy
		end		

		if gProfile.Sets.Progressive['Ranged_Accuracy'] ~= nil then
			for i,j in pairs(gProfile.Sets.Progressive['Ranged_Accuracy']) do
				mracc = mracc + 1;
			end
		end
		
		if gProfile.Sets.Progressive['Tank_Ranged_Accuracy'] ~= nil then
			for i,j in pairs(gProfile.Sets.Progressive['Tank_Ranged_Accuracy']) do
				mtracc = mtracc + 1;
			end
		else
			mtracc = mracc;	-- If tank_ranged_accuracy missing, use ranged_accuracy
		end
	end
	return macc,mtacc,mracc,mtracc;
end		-- gcinclude.TallyProgressiveCaps

--[[
	fExpandGearLine takes the passed in line from a gear set and copies
	it to the global tGearLine array. Subsets are ignored. If it encounters
	an inline gear line reference, assuming that the attached (if present)
	inline conditional is true, it will call itself again with that
	reference so that a single, complete set of gear can be processed
	from the calling routine.
--]]

function fExpandGearLine(sSlot,ts,sc)
	local iPos,sval,sCode;
	local t = {};

	if sSlot == nil or ts == nil then
		return false;
	end
	
	if type(ts) == 'string' then
		t[1] = ts;
	else
		t = ts;
	end
	
	for i,j in pairs(t) do
		iPos = string.find(j,'::');
		if iPos ~= nil then
			-- Found an inline reference
			sval = string.sub(j,1,iPos-1);
			-- Check for missing slot name, assume same as passed slot
			if iPos + 2 >= string.length(j) then
				s = sSlot;
			else
				s = string.sub(j,iPos+2,-1);
			end
			-- Check for valid conditional or lack of conditional
			bGood,x = fCheckInline(sval,s,tss);
			if bGood then
				-- Since good, remove (if present) the conditional
				iPos = string.find(s,'//');
				if iPos ~= nil then
					sCode = string.sub(s,iPos,-1);
					s = string.sub(s,1,iPos-1);
				else
					sCode = nil;
				end
				
				if sc ~= nil then
					if sCode == nil then
						sCode = sc;
					else
						sCode = sCode .. sc;
					end
				end

				-- Now recurse this newly found inline reference				
				x = fGetTableByName(sval);			
				-- Make sure slot name formatted correctly
				s = string.upper(string.sub(s,1,1)) .. string.sub(s,2,-1);				
				-- If definition for referenced slot there, recurse the 
				-- specified slot's definition
				if x[s] ~= nil then					
					bGood = fExpandGearLine(s,x[s],sCode);
				end
				-- Result is ignored since bad inline has no effect
				-- on the global tGearLine array and good result is
				-- already tallied.
			end				
		else
			-- Treat the item as-is
			local iCtr = #gcinclude.tGearLine + 1;
			gcinclude.tGearLine[iCtr] = j		
			if sc ~= nil then
				 gcinclude.tGearLine[iCtr] = gcinclude.tGearLine[iCtr] .. sc;
			end			
			iCtr = iCtr + 1;
		end
	end
	return true;
end		-- fExpandGearLine

--[[
	fSummonerPet determines if the player has a SMN summoned pet. 
	
	Returned: True/False
--]]

function gcinclude.fSummonerPet()
	local pet = gData.GetPet();
	
	return (pet ~= nil and fElementByPetName(pet.Name) ~= nil);
end

--[[
	RegionDisplay determines if the player's nation owns the area the character is in
	or not and updates the display bar accordingly.
--]]

function RegionDisplay()
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

--[[
	DB_ShowIt will display debug details
--]]

function DB_ShowIt()
	local player = gData.GetPlayer();
	local sSlip = fDisplaySlips(false);
	
	print(chat.message(' '));
	print(chat.message('Settings'));
	print(chat.message('--------'));
	print(chat.message('Job: ' .. player.MainJob .. '/' .. player.SubJob));
	print(chat.message('Level: ' .. tostring(player.MainJobSync) .. '(' .. tostring(player.MainJobLevel) .. ')'));	
	print(chat.message(' '));
	print(chat.message('WScheck: ' .. tostring(gcinclude.settings.WScheck)));
	print(chat.message('WSdistance: ' .. tostring(gcinclude.settings.WSdistance)));
	print(chat.message('bWSOverride: ' .. tostring(gcinclude.settings.bWSOverride)));
	print(chat.message('GC run? ' .. tostring(gcdisplay.GetGC())));
	if sSlip == nil then
		print(chat.message('Slips: None'));
	else
		print(chat.message('Slips: ' .. sSlip));
	end
end		-- DB_ShowIt
	
--[[
	Message toggles on/off a feedback mechanism for all luashitacast commands
--]]
	
function gcinclude.Message(toggle, status)
	if toggle ~= nil and status ~= nil then
		print(chat.header('Message'):append(chat.message(toggle .. ' is now ' .. tostring(status))))
	end
end		-- gcinclude.Message

--[[
	SetAlias registers all of the luashitacast commands that are defined in this file
--]]

function SetAlias()
	for _, v in ipairs(gcinclude.AliasList) do
		AshitaCore:GetChatManager():QueueCommand(-1, '/alias /' .. v .. ' /lac fwd ' .. v);
	end
end		-- SetAlias

--[[
	ClearAlias removes the luashitacast commands that were registered in this file
--]]

function ClearAlias()
	for _, v in ipairs(gcinclude.AliasList) do
		AshitaCore:GetChatManager():QueueCommand(-1, '/alias del /' .. v);
	end
end		-- ClearAlias

--[[
	fIsLocked determines if the passed slot is locked. Please note that only slot
	names are supported. Slot numbers will cause an error
	
	Returned: T/F
--]]

function gcinclude.fIsLocked(val)
	local index = nil;
	
	if val == nil then
		print(chat.header('fIsLocked'):append(chat.message('Error: "val" undefined')));
		return true;	-- This error should never occur. Assume it's locked.
	else
		if type(val) == "number" then
			print(chat.header('fIsLocked'):append(chat.message('Error: Only slot names recognized: val = ' .. val)));
			return true;	-- This error should never occur. Assume it's locked.
		else
			for j,k in ipairs(gcinclude.tLocks) do
				if k['slot'] == string.lower(val) then
					index = j;
					break;
				end
			end
			if index == nil then
				print(chat.header('fIsLocked'):append(chat.message('Error: Unrecognized "val": ' .. val)));
				return true;	-- This error should never occur. Assume it's locked.
			end
		end
			
		return gcinclude.tLocks[index]['lock'];
	end
end		-- gcinclude.fIsLocked

--[[
	fLockSlotsBySet walks the passed set and locks the slots based on
	which slots in the passed set have a value
--]]

function fLockSlotsBySet(gs)
	if gs == nil then
		return;
	end
	
	for i,j in pairs(gs) do
		for ii,jj in ipairs(gcinclude.tLocks) do	
			if j ~= nil and j ~= '' and jj['slot'] == string.lower(i) then	
				gcinclude.tLocks[ii]['lock'] = true;			
				break;
			end
		end
	end
	local sList = fGetLockedList('locks');
	gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);	
end		-- fLockSlotsBySet

--[[
	fGetLockedList returns a comma delimited list or nil if all unlocked
	
	Returned: List of locked slots
--]]

function fGetLockedList(sTarget)
	local sList = nil;
	local sWhich = 'lock';
	
	if sTarget == 'acc' then
		sWhich = 'acc';
	end

	for i,j in ipairs(gcinclude.tLocks) do
		if j[sWhich] == true then
			if sList == nil then		
				sList = gData.Constants.EquipSlotNames[gData.Constants.EquipSlotsLC[j['slot']]];
				if sTarget == 'locks' then
					gcinclude.LocksNumeric = tostring(i);
				else
					gcinclude.AccNumeric = tostring(i);
				end
			else	
				sList = sList .. ', ' .. gData.Constants.EquipSlotNames[gData.Constants.EquipSlotsLC[j['slot']]];
				if sTarget == 'locks' then
					gcinclude.LocksNumeric = gcinclude.LocksNumeric .. ',' .. tostring(i);
				else
					gcinclude.AccNumeric = gcinclude.AccNumeric .. ',' .. tostring(i);
				end
			end
		end
	end
	
	if sList == nil then
		if sTarget == 'locks' then
			gcinclude.LocksNumeric = 'None';
		else
			gcinclude.AccNumeric = 'None';
		end
	end
	return sList;
end		-- fGetLockedList

--[[
	LockUnlock either locks or unlocks the specified (or all) slots or enables/disables
	the specified slots for accuracy. Supported are either the slot name or the slot 
	number.
--]]

function LockUnlock(sTarget,sType,sWhich)
	local s = 'lock';
	
	if sWhich == nil then
		return;
	end 
	
	-- Determine field to address
	if sTarget == 'acc' then
		s = 'acc';
	end

	sWhich = ',' .. string.lower(sWhich) .. ',';
	for k,l in ipairs(gcinclude.tLocks) do
		local sk = ',' .. tostring(k) .. ',';
		if (sWhich == ',all,') or (string.find(sWhich,l['slot']) ~= nil) or (string.find(sWhich,sk) ~= nil) then
			gcinclude.tLocks[k][s] = (string.lower(sType) == 'lock');
		end
	end
	
	-- Special case for ears and rings
	for i=1,16,1 do
		if string.find(sWhich,'ears') and string.sub(gcinclude.tLocks[i]['slot'],1,-2) == 'ear' then
			gcinclude.tLocks[i][s] = (string.lower(sType) == 'lock');
		elseif string.find(sWhich,'rings') and string.sub(gcinclude.tLocks[i]['slot'],1,-2) == 'ring' then
			gcinclude.tLocks[i][s] = (string.lower(sType) == 'lock');
		end
	end
end		-- LockUnlock

--[[
	fElementByPetName determines what element is associated with the currently
	summoned avatar/spirit and returns it.
	
	Returned: element of current pet or nil
--]]

function fElementByPetName(pName)
	local lcName;
	local ele = nil;
	
	if pName == nil then
		return nil;
	end
	
	lcName = string.lower(pName);
	
	for i,j in pairs(gcinclude.tElemental_gear['staff']) do
		if string.find(gcinclude._AllElements,i) ~= nil then
			if table.find(j['Summons'],lcName) ~= nil then
				ele = i;
				break;
			end
		end
	end
	
	return ele;
end		-- fElementByPetName

--[[
	fCheckForEleGear determines if the player has the piece of elemental gear 
	indicated by type and if it is accessible
	
	Returned: reference to the piece of gear	
--]]

function gcinclude.fCheckForEleGear(sType,sElement)
	local player = gData.GetPlayer();
	local bGood,slot;
		
	-- Make sure player job defined and download not transitioning
	if player.MainJob == 'NON' then
		return nil;
	end

	-- Then check the level of the player vs the elemental piece of gear
	if player.MainJobSync < gcinclude.tElemental_gear[sType]['level'] then
		return nil;
	end

	-- The links for the dynamic table will be there if /gc was run. If not,
	-- then all elemental gear's ['Ref'] will be nil and skipped.
	
	-- Now process the reference accordingly. For staff, check for HQ before 
	-- looking at NQ
	if sType == 'staff' then
		if gcinclude.tElemental_gear[sType][sElement]['HQ']['Ref'] ~= nil and
			gcinclude.tElemental_gear[sType][sElement]['HQ']['Ref']['accessible'] == true then
			return gcinclude.tElemental_gear[sType][sElement]['HQ']['Name'];
		elseif gcinclude.tElemental_gear[sType][sElement]['NQ']['Ref'] ~= nil and
			gcinclude.tElemental_gear[sType][sElement]['NQ']['Ref']['accessible'] == true then
			return gcinclude.tElemental_gear[sType][sElement]['NQ']['Name'];
		else
			return nil;
		end
	else
		-- Obi and Gorget have the same structure, so handle the same way
		if gcinclude.tElemental_gear[sType][sElement]['Ref'] ~= nil and
			gcinclude.tElemental_gear[sType][sElement]['Ref']['accessible'] == true	then
			return gcinclude.tElemental_gear[sType][sElement]['Name'];
		else
			return nil;
		end
	end
	return nil;
end		-- gcinclude.fCheckForEleGear

--[[
	RefreshVariables is a routine that let's the player manually make sure
	the job-dependent variables are set. Sometimes when logging in, the
	SetVariables routine is run before the client is done downloading. In
	this case, sometimes some toggels are accidentally omitted.
--]]

function RefreshVariables()
	local player = gData.GetPlayer();
	
	-- Now, simple toggles, those not dependent on the player's characteristics
	-- can be ignored. The problem ones are the ones specific to a player's job.
	-- They are the ones that sometimes don't get created.
	
	-- WSwap
	gcdisplay.CreateToggle('WSwap',(string.find('WHM,BRD,RDM',player.MainJob) ~= nil));

	-- Macc
	if string.find(gcinclude._sMagicJobs,player.MainJob) ~= nil or
	   string.find(gcinclude._sMagicJobs,player.SubJob) ~= nil then
		gcdisplay.CreateToggle('Macc',false);
	end
	
	-- Tank
	if string.find('PLD,NIN,RUN',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',true);
	elseif string.find('DRK,WAR,THF,RDM,BLU',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',false);
	end

	-- THF: TH
	if player.MainJob ==  'THF' then
		gcdisplay.CreateToggle('TH',false);
	end

	-- BST: AJug and DB
	if player.MainJob == 'BST' then
		gcdisplay.CreateToggle('AJug',true);
		if gcdisplay.GetCycle('DB') == 'Unknown' then
			gcdisplay.CreateCycle('DB', {[1] = 'Norm', [2] = 'BPP', [3] = 'WSS'});
		end
	end

	-- BRD: Instrument
	if gcdisplay.GetCycle('Instrument') == 'Unknown' and player.MainJob == 'BRD' then
		gcdisplay.CreateCycle('Instrument', {[1] = 'Horn', [2] = 'String'});
	end	
	
	-- SMN: sBP
	if player.MainJob == 'SMN' then
		gcdisplay.CreateToggle('sBP',true);
	end	
end		-- RefreshVariables

--[[
	SetVariables defines run settings for luashitacast
--]]

function SetVariables()
	local player = gData.GetPlayer();

	-- General toggles
	gcdisplay.CreateToggle('GSwap', true);
	gcdisplay.CreateToggle('Kite', false);
	gcdisplay.CreateToggle('Eva', false);
	gcdisplay.CreateToggle('Idle',true);
		
	if string.find('SMN,BLM',player.MainJob) == nil then
		gcdisplay.CreateToggle('WSwap',(string.find('WHM,RDM',player.MainJob) ~= nil));
	end

	-- Job specific toggles	
	if string.find('PLD,NIN,RUN',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',true);
	elseif string.find('DRK,WAR,THF,RDM,BLU',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',false);
	end

	if string.find(gcinclude._sMagicJobs,player.MainJob) ~= nil or
	   string.find(gcinclude._sMagicJobs,player.SubJob) ~= nil then
		gcdisplay.CreateToggle('Macc',false);
	end
	
	if player.MainJob == 'THF' then
		gcdisplay.CreateToggle('TH',false);
	end	
	
	if player.MainJob == 'BST' then
		gcdisplay.CreateToggle('AJug',true);
		gcdisplay.CreateCycle('DB', {[1] = 'Norm', [2] = 'BPP', [3] = 'WSS'});
	end
	
	if player.MainJob == 'BRD' then
		gcdisplay.CreateCycle('Instrument', {[1] = 'Horn', [2] = 'String'});
	end
	
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		gcdisplay.CreateToggle('sBP',true);
	end
	
	-- General cycles
	gcdisplay.CreateCycle('DT', {[1] = gcinclude.OFF, [2] = gcinclude.PHY, [3] = gcinclude.MAG, [4] = gcinclude.BRE});
	gcdisplay.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned', [3] = 'N/A'});
end		-- SetVariables

--[[
	fRemoveConditional takes the passed in string and removes any inline
	conditional qualifier from it.
--]]

function fRemoveConditional(g)
	local iPos;
	if g == nil then
		return nil;
	end
	
	iPos = string.find(g,'//');

	if iPos ~= nil then
		return string.sub(g,1,iPos-1);
	else
		return g;
	end
end		-- fRemoveConditional

--[[
	fSetColorText accepts a boolean and returns a color code to represent the
	value: 2 - green, 8 - red, 107 - something else (probably yellow). bInvert
	inverts the colors: 2 - red and 8 - green
--]]

function fSetColorText(bVal,bInvert)
	local ic;
	
	if bInvert == nil then
		bInvert = false;
	end
	
	if bVal == nil then
		ic = 107;	-- Other color
	elseif (bVal == false and bInvert == false) or
		(bVal == true and bInvert == true) then
		ic = 8;		-- Red
	else 
		ic = 2;	-- Green
	end
	
	return ic;	
end		-- fSetColorText

--[[
	DisplayGD_AW lists either all the dynamic gear definitions or just the
	ones that are invalid or inaccessible.
--]]

function DisplayGD_AW(p1)
	local bShow;
	
	if p1 == nil then
		print(chat.message('Complete list of all gear'));
	elseif p1 == 'noac' then
		print(chat.message('Invalid or inaccessible gear'));
	end	

	for slot,name in pairs(gcinclude.GearDetails) do
		print(chat.message(' '));
		if p1 ~= nil and string.lower(p1) == 'noac' then
			print(chat.message('Slot: ' .. slot));
		else
			print(chat.message('Slot: ' .. slot .. '[' .. tostring(name['acc']) .. '/' .. tostring(name['num']) .. ']'));
		end
		
		for i,j in pairs(name) do
			if string.find('num,acc,vis',i) == nil then
				bShow = (p1 == nil or j['valid'] == false or j['accessible'] == false);
				if bShow == true and type(i) == 'string' then
					DisplayItemStats(i,slot);
				end
			end
		end
	end
end		-- DisplayGD_AW

--[[
	DisplayGD_S lists all dynamic gear definitions associated with specific slot(s).
--]]

function DisplayGD_S(p1)

	if p1 == nil then
		return;
	end
	
	print(chat.message('Gear associated with slot(s): ' .. p1));
	
	for slot,name in pairs(gcinclude.GearDetails) do
		if string.find(string.lower(p1),string.lower(slot)) ~= nil then
			print(chat.message(' '));
			print(chat.message('Slot: ' .. slot));

			for i,j in pairs(name) do
				if string.find('num,acc,vis',i) == nil then
					if type(i) == 'string' then
						DisplayItemStats(i,slot);
					end
				end
			end
		end
	end
end		-- DisplayGD_S

--[[
	DisplayGD_Gs lists all dynamic gear definitions associated with a specific gear set.
--]]

function DisplayGD_Gs(p1)
	local str,tmp;
	local tGs = {};
	local gg = {};
	local lPc = nil;

	if p1 == nil then
		return;
	end
		
	-- first check gProfile.Sets. If not found, look in gcinclude.Sets.
	tGs = fGetTableByName(p1);
	if tGs == nil then
		print(chat.message(p1 .. ': no such set exists!'));
		return;
	end
	
	print(' ');
	print(chat.message('Gear set: ' .. string.upper(p1)));

	-- Loop the entries first looking for subsets
	for slot,j in pairs(tGs) do
		if string.lower(slot) == 'subset' then
			if j ~= nil then
				-- then make sure that j is a table
				gg = {};
				if type(j) == 'string' then
					gg[1] = j;
				else
					gg = j
				end			
				tmp = nil;
				
				for _,g in ipairs(gg) do
					if tmp == nil then
						tmp = fRemoveConditional(g);
					else
						tmp = tmp .. ',' .. fRemoveConditional(g);
					end
				end
				print(' ');
				print(chat.message('Subset: ' .. tmp));
			end
		end
	end
				
	-- loop on the entries of the gear set
	lPc = ',';
	for slot,j in pairs(tGs) do	
		if string.lower(slot) ~= 'subset' then
			print(' ');
			print(chat.message('Slot: ' .. slot));
			-- make sure entry is not [slot] =
			if j ~= nil then
				gg = {};
				-- then make sure that j is a table
				if type(j) == 'string' then
					gg[1] = j;
				else
					gg = j
				end
				
				-- Now process the normal slot
				local t;
				for _,g in pairs(gg) do
					if string.find(g,'::') ~= nil then
						print(chat.message('   ' .. g));
					else
						t = string.upper(fRemoveConditional(g));
						if string.find(lPc,t) == nil then
							DisplayItemStats(t,slot);
							lPc = lPc .. ',' ..t;
						end
					end						
				end
			end
		end	
	end	
end		-- DisplayGD_Gs

--[[
	DisplayItemStats displays the item definition for the passed piece of gear
	from the dynamic GearDetails table.
--]]

function DisplayItemStats(sName,sSlot)
	local msg;
	local tWhat;
	local tTrans = { [true] = 'Yes', [false] = 'No'};
	
	if sSlot == nil or sName == nil then
		return;
	end
	
	sSlot = string.lower(sSlot);
	sName = string.lower(sName);
	
	if gcinclude.GearDetails[sSlot][sName] == nil then
		-- You get here if the item isn't a valid item
		print(sName .. ' - ' .. chat.color1(8,'Invalid item'));
		return;
	end
	
	tWhat = gcinclude.GearDetails[sSlot][sName];
	-- You get here if the item is valid or it's invalid because the slot
	-- is incorrect	
	msg = '   ' .. chat.color1(fSetColorText(nil), string.upper(sName)); 
	msg = msg .. ', Level: ' .. tostring(tWhat['level'],tostring(tWhat['level']));
	print(msg);
	msg = '      ' .. 'Own it? ' .. chat.color1(fSetColorText(tWhat['own']),tTrans[tWhat['own']]);
	msg = msg .. ', Accessible? ' .. chat.color1(fSetColorText(tWhat['accessible']),tTrans[tWhat['accessible']]);
	print(msg);
	print('      Code Breakdown-');
	msg = '         Valid? ' .. chat.color1(fSetColorText(tWhat['valid']),tTrans[tWhat['valid']]);
	msg = msg .. ' Slot? ' .. chat.color1(fSetColorText(tWhat['slot']),tTrans[tWhat['slot']]);
	msg = msg .. ' Job? ' .. chat.color1(fSetColorText(tWhat['job']),tTrans[tWhat['job']]);
	msg = msg .. ' Porter? ' .. chat.color1(fSetColorText(tWhat['porter'],true),tTrans[tWhat['porter']]);
	msg = msg .. ' Claim? ' .. chat.color1(fSetColorText(tWhat['claim'],true),tTrans[tWhat['claim']]);
	print(msg);
	if tWhat['locations'] ~= nil then
		msg = '         Location(s): ' .. tWhat['locations'];
	else
		msg = '         Location(s): ';
	end
	print(msg);
	print(' ');
end	-- DisplayItemStats

--[[
	fDisplaySlips shows which storage slips the player owns. The passed in 
	parameter indicates if just a list of slips are wanted or a detail of
	which slips and where they can be found are wanted.

	Returned: List of slips or nil	
--]]

function fDisplaySlips(bList)
	local sOut = nil;
	
	if bList == nil then
		bList = false;
	end
	
	for i,j in ipairs(gcinclude.Slips) do
		if j['own'] == true then
			if bList then
				print(j['name'] .. ' - ' .. j['location']['name']);
			else
				if sOut == nil then
					sOut = string.sub(j['name'],-2,-1);
				else
					sOut = sOut .. ',' .. string.sub(j['name'],-2,-1);
				end
			end
		end
	end
	return sOut;
end		-- fDisplaySlips

--[[
	fWhichSlip determines which storage slip is being asked for by checking the
	known list of storage slip names.

	Returned: Slip name	
--]]

function fWhichSlip(sId)
	local iWhich = nil;
	
	if sId ~= nil then
		for i,j in pairs(gcinclude.Slips) do
			if string.find(j['name'],sId) ~= nil then 
				iWhich = i;
				break;
			end				
		end
	end
	return iWhich;	
end		-- fWhichSlip

--[[
	FindSlips determines what storage slips the player owns and where they are 
	located. 
--]]

function FindSlips()
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local containerID,id;

	-- First, reset the Slips structure
	for i,j in ipairs(gcinclude.Slips) do
		j['own'] = false;
		j['location'] = nil;
		j['extra'] = nil;
	end
	
	-- Now search for storage slips
	for i,desc in pairs(gcinclude.NON_GEAR) do
		containerID = desc['id'];
		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
				item = resources:GetItemById(itemEntry.Id);
				if string.find(string.lower(item.Name[1]),'storage slip') ~= nil then			
					id = fWhichSlip(string.sub(item.Name[1],-2,-1));
					if id ~= nil then
						gcinclude.Slips[id]['own'] = true;
						gcinclude.Slips[id]['location'] = desc;
						gcinclude.Slips[id]['extra'] = itemEntry.Extra;
					else
						print(chat.header('FindSlips'):append('Unrecognized storage slip found: ' .. item.Name[1] .. '. Skipping'));
					end
				end
			end
		end
	end
end		-- FindSlips

--[[
	fSlotMatch determines if the passed slot the gear piece is being loaded into
	matches the item's slot designation. Returned is true or false
--]]

function fSlotMatch(sSlot,iSlot)
	local bGood = false;
	
	sSlot = string.lower(sSlot);
	
	-- Make sure the composite slots are represented by an actual slot
	if sSlot == 'rings' then
		sSlot = 'ring1';
	elseif sSlot == 'ears' then
		sSlot = 'ear1';
	end
	
	-- The lock list has all slots identified. The slot masks have been added
	-- to the tLocks structure. Even though the mask is a bit pattern, the
	-- composited value is included too. That's why I only need to look for
	-- a match.
	for i,j in ipairs(gcinclude.tLocks) do
		if j['slot'] == sSlot then
			bGood = (table.find(j['mask'],iSlot) ~= nil);
			break;
		end
	end

	return bGood;
end		-- fSlotMatch

--[[
	fGearCheckItem process the specific item sent to it and where appropriate, populates
	gcinclude.GearDetails. The details tracked are: item name, item level, can equip?, 
	and accessibility. Returned is a true/false which indicates that the item's level,
	job, and accessibility is valid for equipping. Please note that an invalid item 
	will return false.
	
		sSlot   - Name of the slot
		sName   - Name of the item to check
		bAccess - True = return accessibility, False = check job, access, and level
		bCreate - Create record is missing

	Returned: Accessibility,gear reference
--]]

function fGearCheckItem(sSlot,sName,bAccess,bCreate)
	local player = gData.GetPlayer();
	local bJob,bAccessible,bSlot;
	local iPos;
	local item = {};
	local tOwned = {};
	
	-- Required fields
	if sSlot == nil or sName == nil then
		return false,nil;
	end
	
	-- Subsets and inline reference definitions are skipped
	if string.lower(sSlot) == 'subset' 
	   or string.find(sName,'::') ~= nil then
		return false,nil;
	end
	
	-- Make sure "downloading data" is not in transition
	if player.MainJob == nil or player.MainJob == 'NON' then
		return false,nil;
	end

	-- Assume full check if absent
	if bAccess == nil then
		bAccess = false;
	end

	if bCreate == nil then
		bCreate = false;
	end
	
	sSlot = string.lower(sSlot);
	sName = string.lower(sName);
						
	-- Make sure all ear and ring variants represented by the generic category
	if string.find('ears,ear1,ear2',sSlot) ~= nil then
		sSlot = 'ears';
	elseif string.find('rings,ring1,ring2',sSlot) ~= nil then
		sSlot = 'rings';
	end

	-- Then remove any inline conditionals
	iPos = string.find(sName,'//');
	if iPos ~= nil then
		sName = string.sub(sName,1,iPos-1);
	end	
	
	-- If bCreate indicated, a new record will be created or an existing
	-- record will be overriden.
	if bCreate == true then
		-- Now process the item
		item = AshitaCore:GetResourceManager():GetItemByName(sName,2);
		if item ~= nil then	
			local bExist = (gcinclude.GearDetails[sSlot][sName] ~= nil); -- Note if existing record
			bJob = (bit.band(item.Jobs,gcinclude.JobMask[player.MainJob]) == gcinclude.JobMask[player.MainJob]) or
		  		   (bit.band(item.Jobs,gcinclude.JobMask['Alljobs']) == gcinclude.JobMask['Alljobs']);
			tOwned = fCheckItemOwned(item);
			bSlot = fSlotMatch(sSlot,item.Slots);
			bAccessible = (tOwned['own'] == true and tOwned['accessible'] == true);

			-- Save item w/details
			gcinclude.GearDetails[sSlot][sName] = { 
				['id']		   = item.Id;
				['valid']	   = true,
				['slot']	   = bSlot, 
				['level']	   = item.Level,
				['job']        = bJob, 
				['own']		   = tOwned['own'],
				['accessible'] = bAccessible, 
				['porter']	   = tOwned['porter'],
				['claim']	   = tOwned['claim'],
				['locations']  = tOwned['locations'],
				['desc'] 	   = item.Description[1]
			};
			if bSlot == false then
				gcinclude.GearDetails[sSlot][sName]['valid'] = false;
			end
			if not bExist then
				gcinclude.GearDetails[sSlot]['num'] = gcinclude.GearDetails[sSlot]['num'] + 1;
				if bAccessible then
					gcinclude.GearDetails[sSlot]['acc'] = gcinclude.GearDetails[sSlot]['acc'] + 1;			
				end
			end
		else
			-- This is an erroneous item	
			gcinclude.GearDetails[sSlot][sName] = { ['valid'] = false };
			return false,gcinclude.GearDetails[sSlot][sName];
		end
	end

	-- If it still doesn't exist, return that state
	if gcinclude.GearDetails[sSlot][sName] == nil then
		return false,nil;
	else
		if bAccess == true then
			return (gcinclude.GearDetails[sSlot][sName]['accessible'] == true),gcinclude.GearDetails[sSlot][sName];
		else
			return (gcinclude.GearDetails[sSlot][sName]['job'] == true and 
				gcinclude.GearDetails[sSlot][sName]['accessible'] == true and 
				gcinclude.GearDetails[sSlot][sName]['level'] <= player.MainJobSync),gcinclude.GearDetails[sSlot][sName];
		end
	end
end	-- fGearCheckItem

--[[
	GearCheck will search and extract all the items from all the gear sets in the
	job file and gcinclude, populating gcinclude.GearDetails. The details tracked are: 
	item name, item id, item level, can equip?, and accessibility. This information 
	will be used by the functions for picking which piece to equip, which should speed 
	up the gear swapping and hopefully cut down on the lag, especially in level capped 
	areas.
--]]

function GearCheck(sList,bForce)
	local player = AshitaCore:GetMemoryManager():GetPlayer();
	local tTarget = { gProfile.Sets, gcinclude.Sets };
	local ts = {};
	local ref = {};
	local iCnt = 0;
	local ctr = 0;
	local bGood;

	if bUpdate == nil then
		bUpdate = false;
	end

	if sList == nil then
		-- Tallying counts from the Progressive structure
		local macc,mtacc,mracc,mtracc = gcinclude.TallyProgressiveCaps();
		gcdisplay.SetAccMax(macc,mtacc,mracc,mtracc);
		
		-- Now start with storage slips
		print(chat.header('GearCheck'):append(chat.message('Starting to scan for storage slips')));
		FindSlips();
		print(chat.message('Found slips: ' .. fDisplaySlips(false)));

		-- then claim slips
		print(chat.header('GearCheck'):append(chat.message('Starting to scan for claim slips')));
		for i,j in pairs(gcinclude.ClaimSlips) do
			if 	player:HasKeyItem(j['kid']) then
				j['own'] = true;
				ctr = ctr + 1;
			end
		end
		print(chat.message('Found claim slips: ' .. tostring(ctr)));
		
		-- next is EquipIt items
		print(chat.header('GearCheck'):append(chat.message('Starting to scan EquipIt shortcut items')));
		for s,t in pairs(gcinclude.tEquipIt) do
			local sSlot = t['Slot'];
			if string.find('Ring,Ear',sSlot) ~= nil then
				sSlot = sSlot .. 's';
			end
			bGood,ref = fGearCheckItem(sSlot,t['Name'],false,true);
			if ref ~= nil and ref['valid'] == false then
				print(chat.header('GearCheck'):append(chat.message('Warning: Invalid EquipIt gear piece - ' .. t['Name'] .. ': ' .. s)));
			end			
		end

		-- next is pet food since any job can equip it
		print(chat.header('GearCheck'):append(chat.message('Starting to scan Pet Food items')));
		for s,t in pairs(gcinclude.tPetFood) do
			bGood,ref = fGearCheckItem('ammo',t['name'],false,true);
			if ref ~= nil and ref['valid'] == false then
				print(chat.header('GearCheck'):append(chat.message('Warning: Invalid Pet Food - ' .. t['Name'] .. ': ' .. s)));
			end			
		end		

		-- next is jug pets, but only BST can equip them
		if gData.GetPlayer().MainJob == 'BST' then
			print(chat.header('GearCheck'):append(chat.message('Starting to scan Jug Pets')));
			for s,t in pairs(gProfile.JugPets) do
				bGood,ref = fGearCheckItem('ammo',s,false,true);
				if ref ~= nil and ref['valid'] == false then
					print(chat.header('GearCheck'):append(chat.message('Warning: Invalid Jug Pet - ' .. s .. ': ' .. s)));
				end				
			end
		end
		
		-- now loop through the job file and gcinclude
		for s,t in pairs(tTarget) do
			if t == gProfile.Sets then
				print(chat.header('GearCheck'):append(chat.message('Starting to scan the Job file')));
			else
				print(chat.header('GearCheck'):append(chat.message('Starting to scan gcinclude')));
			end
		
			-- Loop the gear sets
			for j,k in pairs(t) do
				-- Process if not either 'CurrentGear' or 'Progressive'. CurrentGear 
				-- is a composite from other gear sets and Progressive has a 
				-- complelely different structure, it will be processed elsewhere
				if table.find({'CurrentGear','Progressive'},j) == nil then				
					-- Loop the gear set slots
					for jj,kk in pairs(k) do
						ts = {};
						-- Entries can be a table or a string. Make either case a table
						if type(kk) == 'table' then
							ts = kk;
						else
							ts[1] = kk;
						end
			
						if table.find(gcinclude.SlotNames,string.lower(jj)) == nil then
							print(chat.header('GearCheck'):append(chat.message('Warning: Invalid slot name - ' .. jj .. ' in ' .. j)));
						else
							-- Now walk the list of gear
							for ss,tt in pairs(ts) do						
								-- Save the details if appropriate. Returned results are
								-- ignored, but captured in case I change my mind. Please
								-- note that subsets are ignored in fGearCheckItem.
								bGood,ref = fGearCheckItem(jj,tt,false,true);
								if ref ~= nil then						
									if ref['valid'] == false and ref['slot'] == nil then
										print(chat.header('GearCheck'):append(chat.message('Warning: Invalid piece of gear - ' .. tt .. ' in ' .. j)));
									elseif ref['slot'] == false then
										print(chat.header('GearCheck'):append(chat.message('Warning: Invalid slot: ' .. jj .. ', gear - ' .. tt .. ' in ' .. j)));
									end
								end
								iCnt = iCnt +1;
								if math.floor(iCnt/50) == iCnt/50 then
									print(chat.message(tostring(iCnt) .. ' sets processed...'));
								end
							end
						end
					end
				elseif j == 'Progressive' then
					-- Loop on type of progressive set			
					for ij,ik in pairs(k) do
						-- Loop on the progressive stages
						for jj,jk in ipairs(ik) do
							-- Loop on the line elements
							for kj,kk in pairs(jk) do							
								ts = {};
								-- Entries can be a table or a string. Make either case a table
								if type(kk) == 'table' then
									ts = kk;
								else
									ts[1] = kk;
								end							
								
								if table.find(gcinclude.SlotNames,string.lower(kj)) == nil then
									print(chat.header('GearCheck'):append(chat.message('Warning: Invalid slot name - ' .. kj .. ' in Progressive ' .. ij)));
								else								
									-- Process the list of gear
									for ss,tt in pairs(ts) do							
										-- Subsets and inline references are ignored									
										bGood,ref = fGearCheckItem(kj,tt,false,true);
										if ref ~= nil then						
											if ref['valid'] == false and ref['slot'] == nil then
												print(chat.header('GearCheck'):append(chat.message('Warning: Invalid piece of gear - ' .. tt .. ' in Progressive:' .. ij .. ', Stage: ' .. tostring(jj) .. ', Slot: ' .. ss)));
											elseif ref['slot'] == false then
												print(chat.header('GearCheck'):append(chat.message('Warning: Invalid slot: ' .. ss .. ', gear - ' .. tt .. ' in Progressive:' .. ij)));
											end
										end
										iCnt = iCnt +1;
										if math.floor(iCnt/50) == iCnt/50 then
											print(chat.message(tostring(iCnt) .. ' sets processed...'));
										end
									end										
								end								
							end
							iCnt = iCnt +1;
							if math.floor(iCnt/50) == iCnt/50 then
								print(chat.message(tostring(iCnt) .. ' sets processed...'));
							end							
						end
					end
				end
			end
		end
		
		print(chat.header('GearCheck'):append(chat.message('Starting to scan special')));
		for i,j in pairs(gcinclude.tElemental_gear) do
			if i == 'staff' then
				for ii,jj in pairs(j) do
					if table.find({ 'fire','ice','wind','earth','thunder','water',
									'light','dark' },ii) ~= nil then
						bGood,jj['NQ']['Ref'] = fGearCheckItem('main',jj['NQ']['Name'],false,true);
						bGood,jj['HQ']['Ref'] = fGearCheckItem('main',jj['HQ']['Name'],false,true);
						iCnt = iCnt + 2;
					end
					if math.floor(iCnt/50) == iCnt/50 then
						print(chat.message(tostring(iCnt) .. ' sets processed...'));
					end
				end				
			elseif i == 'obi' or i == 'gorget' then
				for ii,jj in pairs(j) do
					if table.find({ 'fire','ice','wind','earth','thunder','water',
									'light','dark' },ii) ~= nil then
						if i == 'obi' then
							bGood,jj['Ref'] = fGearCheckItem('waist',jj['Name'],false,true);
						else
							bGood,jj['Ref'] = fGearCheckItem('neck',jj['Name'],false,true);
						end
						iCnt = iCnt + 1;
						if math.floor(iCnt/50) == iCnt/50 then
							print(chat.message(tostring(iCnt) .. ' sets processed...'));
						end
					end
				end
			end
		end
		
		print(chat.header('GearCheck'):append(chat.message('Scan completed')));
		print(chat.header('GearCheck'):append(chat.message(' ')));
		print(chat.header('GearCheck'):append(chat.message('Resultant gear breakdown:')));
	end
	
	for i,j in pairs(gcinclude.GearDetails) do
		print(chat.message('   [' .. i .. '] - ' .. tostring(j['num'])));
	end
end		-- GearCheck

--[[
	fIsPetNamed determines if that passed pet has the passed name
	
	Returned: T/F
--]]

function fIsPetNamed(sName)
	local pet = gData.GetPet();

	if pet == nil then
		return false;
	end
	
	if sName ~= nil then
		local sPetName = string.lower(pet.Name);
		local sMatch = string.lower(sName);
		
		return (string.find(sMatch,sPetName) ~= nil);
	else
		print(chat.header('fIsPetNamed'):append(chat.message('Error: Passed name is nil')));
		return false;
	end
end		-- fIsPetNamed

--[[
	fCheckPartyJob determines if the party has a member of the passed job.
	You can optionally state to not include yourself. By default, you're
	always listed in the first slot of the party.

	Returned: T/F	
--]]

function fCheckPartyJob(jobs,bNotMe)
	local pParty = AshitaCore:GetMemoryManager():GetParty();
	local bFound = false;
	local iStart = 1;
	
	if bNotMe ~= nil and bNotMe == true then
		iStart = 2;
	end
	 
	jobs = string.upper(jobs);
	 
	for i=iStart,6,1 do
		if (pParty:GetMemberIsActive(i - 1) == 1) then
			-- Player found
			local mainJob = pParty:GetMemberMainJob(i - 1);
			local job = AshitaCore:GetResourceManager():GetString("jobs.names_abbr", mainJob);
			if string.find(jobs,job) ~= nil then
				bFound = true;
			end
		end
	end
	return bFound;
end		-- fCheckPartyJob

--[[
	CheckTime determines if the current server time is found in the passed name time range.
	The following named time ranges are valid:
	
		Nighttime is 17:00 to 6:00, Daytime is 6:00 to 18:00, DUSK2DAWN: 17:00 to 7:00,
		Dawn: 6:00 to 7:00, Day: 7:00 to 17:00, Dusk: 17:00 to 18:00, Evening: 18:00 to 20:00, 
		DEADOFNIGHT: 20:00 to 4:00.

	Returned: T/F
--]]

function gcinclude.CheckTime(hr,t,bReport)

	local bGood = false;

	if t == 'Nighttime' then
		bGood = (hr >= 17 or hr <= 6);
	elseif t == 'Daytime' then
		bGood = (hr >= 6 and hr <= 18);
	elseif t == DUSK2DAWN then
		bGood = (hr >= 17 or hr <= 7);
	elseif t == 'Dawn' then
		bGood = (hr >= 6 and hr <= 7);
	elseif t == 'Day' then
		bGood = (hr >= 7 and hr <=17);
	elseif t == 'Dusk' then
		bGood = (hr >= 17 and hr <= 18);
	elseif t == 'Evening' then
		bGood = (hr >= 18 and hr <= 20);
	elseif t == 'DEADOFNIGHT' then
		bGood = (hr >= 20 and hr <= 4);
	else
		if bReport then
			print(chat.header('CheckTime'):append(chat.message('Error: Unknown named time: '.. t)));
		end
		bGood = false;
	end
	return bGood;
end		-- gcinclude.CheckTime

--[[
	ClearSet blanks out the passed gear set
--]]

function gcinclude.ClearSet(gSet)
	
	for k,v in pairs(gData.Constants.EquipSlots) do
		gSet[k] = '';
	end
end		-- gcinclude.ClearSet

--[[
	fMagicSubJob determines if the sub job can do magic

	Returned: T/F	
--]]

function gcinclude.fMagicalSubJob()
	local player = gData.GetPlayer();
	local sj = player.SubJob;
	
	return (string.find(gcinclude._sMagicJobs,sj) ~= nil);
end		-- gcinclude.fMagicalSubJob

--[[
	fTallyGear tallies up all the MP and HP manipulations on the gear
	currently equipped (and the passed gear, minus what is already in
	that slot), both visible and invisible slots so that 

	Returned: T/F/nil	
--]]

function fTallyGear(sGear,sSlot)
	local cur = gData.GetEquipment();
	local sPiece,lcii,sVis,bGood;
	local item = {};
	local ref = {};
	local rec = { 		-- define tracking structure
		['visible'] = { 
			['MP'] = 0, ['MPP'] = 0, ['HP'] = 0, ['HPP'] = 0, 
			['cHM'] = 0, ['cMH'] = 0 
		},
		['invisible'] = {
			['MP'] = 0, ['MPP'] = 0, ['HP'] = 0, ['HPP'] = 0, 
			['cHM'] = 0, ['cMH'] = 0
		}
	};	

	if sGear == nil or sSlot == nil then
		return nil;
	end
	
	sGear = string.lower(sGear);
	sSlot = string.lower(sSlot);
		
	-- loop through the current gear, tallying up totals
	for ii,jj in pairs(cur) do
		-- when dealing with rings and earrings, use the grouping mechanism.
		lcii = string.lower(ii);
		if string.find('ring1,ring2',lcii) ~= nil then
			lcii = 'rings';
		end
		if string.find('ear1,ear2',lcii) ~= nil then
			lcii = 'ears';
		end
		
		-- There's a special case. If the slot from the currently equipped
		-- gear matches the slot of the passed gear piece, use the passed
		-- in piece. Using the currently equipped one could result in an
		-- erroneous equipment of the gear piece.
		if lcii == sSlot then
			sPiece = sGear;
		else
			sPiece = string.lower(jj.Name);
		end
		
		if gcinclude.GearDetails[lcii][sPiece] ~= nil and 
			gcinclude.GearDetails[lcii][sPiece]['valid'] == true then
			item = fParseDescription(sPiece,gcinclude.GearDetails[lcii][sPiece]['desc']);
		
			-- Now tally the parsed description, divided between visible and invisible,
			-- accordingly
			if gcinclude.GearDetails[lcii]['vis'] == true then
				sVis = 'visible';
			else
				sVis = 'invisible';
			end
				
			rec[sVis]['MP'] = rec[sVis]['MP'] + item['MP'];
			rec[sVis]['MPP'] = rec[sVis]['MPP'] + item['MPP'];
			rec[sVis]['HP'] = rec[sVis]['HP'] + item['HP'];
			rec[sVis]['HPP'] = rec[sVis]['HPP'] + item['HPP'];
			rec[sVis]['cHM'] = rec[sVis]['cHM'] + item['cHM'];
			rec[sVis]['cMH'] = rec[sVis]['cMH'] + item['cMH'];
		
			local bOwn = (gcdisplay.GetCycle('Region') == 'Owned');

			if (item['own']['ctrl'] == 'T' and bOwn == true) or 
				(item['own']['ctrl'] == 'F' and bOwn == false) then
				rec[sVis]['MP'] = rec[sVis]['MP'] + item['own']['MP'];
				rec[sVis]['MPP'] = rec[sVis]['MPP'] + item['own']['MPP'];
				rec[sVis]['HP'] = rec[sVis]['HP'] + item['own']['HP'];
				rec[sVis]['HPP'] = rec[sVis]['HPP'] + item['own']['HPP'];
				rec[sVis]['cHM'] = rec[sVis]['cHM'] + item['own']['cHM'];
				rec[sVis]['cMH'] = rec[sVis]['cMH'] + item['own']['cMH'];
			end
		end
	end
	return rec;
end

--[[
	fParseDescriptionExceptions processes the descriptions for gear 
	that requires special processing. It could have been included in
 	fParseDescription, but was extracted so that function would not 
	be too long.
	
	Returned: T/F, item reference	
--]]

function fParseDescriptionExceptions(rec,sGear,sDesc)
	local player = gData.GetPlayer();
	local environ = gData.GetEnvironment();
	local bFound = true;

	sGear = string.lower(sGear);

	if sGear == 't.m. wand +1' then
		rec['own']['ctrl'] = 'F';
		rec['own']['MP'] = 18
		rec['MP'] = 5;
	elseif sGear == 't.m. wand +2' then
		rec['own']['ctrl'] = 'F';
		rec['own']['MP'] = 20
		rec['MP'] = 5;
	elseif sGear == 'ajase beads' then
		rec['HP'] = 20;	
	elseif string.find(sGear,'ryl.sqr. robe %+%d') ~= nil then
		rec['MP'] = 10;
	elseif sGear == 'sattva ring' then
		-- variable HP based on player's level
		rec['HP'] = math.floor((player.MainJobSync - 30)/15)*5 + 15;
	elseif sGear == 'tamas ring' then
		-- variable MP based on player's level
		rec['MP'] = math.floor((player.MainJobSync - 30)/15)*5 + 15;
	elseif table.find({'creek boxers +1','creek shorts +1',
			'dune boxers +1','magna shorts +1','marine boxers +1',
			'marine shorts +1','river shorts +1','woodsy boxers +1',
			'woodsy shorts +1'},sGear) ~= nil then
		if string.lower(environ.Weather) == 'sunshine' then
			rec['MP'] = 20;
		end
	elseif table.find({'custom shorts +1','custom trunks +1',
			'elder trunks +1','magna trunks +1','savage shorts +1',
			'wonder shorts +1','wonder trunks +1'},sGear) ~= nil then
		if string.lower(environ.Weather) == 'sunshine' then	
			rec['HP'] = 20;
		end			
	elseif sGear == 'wyvern perch' then
		-- This one will generate a false positive. The HP gain is
		-- for the Wyvern.
	elseif sGear == 'booster earring' then
		-- another player in the party (besides yourself) must be a BLU	
		if fCheckPartyJob('BLU',true) == true then
			rec['HP'] = 10;
			rec['MP'] = 10;
		end
	elseif sGear == 'ese earring' then
		-- another player in the party (besides yourself) must be a MNK
		if fCheckPartyJob('MNK',true) == true then
			rec['HP'] = 20;
		end	
	elseif sGear == 'multiple ring' then
		-- player level must be evenly divisible by 10
		if math.floor(player.MainJobSync/10) == player.MainJobSync/10 then
			rec['HP'] = 50;
			rec['MP'] = 20;
		end
	elseif sGear == 'diabolos\'s ring' then
		if string.lower(environ.Day) == 'darksday' then
			rec['MPP'] = -15;
		end
	elseif sGear == 'earth ring' then
		if string.lower(environ.Day) == 'earthsday' then
			rec['HPP'] = -15;
		end
	elseif sGear == 'fire ring' then
		if string.lower(environ.Day) == 'firesday' then
			rec['HPP'] = -15;
		end
	elseif sGear == 'ice ring' then
		if string.lower(environ.Day) == 'iceday' then
			rec['MPP'] = -15;
		end
	elseif sGear == 'lightning ring' then
		if string.lower(environ.Day) == 'lightningsday' then
			rec['HPP'] = -15;
		end
	elseif sGear == 'water ring' then
		if string.lower(environ.Day) == 'watersday' then
			rec['MPP'] = -15;
		end
	elseif sGear == 'wind\'s ring' then
		if string.lower(environ.Day) == 'windsday' then
			rec['HPP'] = -15;
		end	
	elseif sGear == 'storm mantle' then
		-- for now, the assault aspect, is assumed to be true
		rec['HP'] = 105;
	elseif sGear == 'cougar pendant' then
		-- for now, the assault aspect, is assumed to be true
		rec['HP'] = 230;
	elseif sGear == 'storm earring' then
		-- for now, the assault aspect, is assumed to be true
		rec['MP'] = 15;		
	elseif sGear == 'variable ring' then
		-- for now, the garrison aspect, is assumed to be true
		rec['MP'] = 28;
	else
		bFound = false;
	end
		
	return bFound,rec;
end

--[[
	fParseDescription parses the passed description for the stated item
	looking for HP/HPP/MP/MPP/CHPMP/CMPHP and nation control information
	if appropriate. The record is returned.
	
	Please note that this routine does not recognize out of era qualifiers
	like besieged, campaign, salvage, assault, etc. Will address as 
	expansions are released on Horizon XI
	
	Returned: tallied structure	
--]]

function fParseDescription(item,sDesc)
	local bFound,ic,sType,ipos,ival,bPct;
	local rec = { ['own'] = { ['ctrl'] = nil, ['HP'] = 0, ['HPP'] = 0,
					['MP'] = 0, ['MPP'] = 0, ['cHM'] = 0, ['cMH'] = 0 }, 
				  ['HP'] = 0, ['HPP'] = 0, ['MP'] = 0, ['MPP'] = 0, 
				  ['cHM'] = 0, ['cMH'] = 0 };

	if item == nil or sDesc == nil then
		return rec;
	end
	
	item = string.lower(item);
	
	bFound,rec = fParseDescriptionExceptions(rec,item,sDesc);
	if bFound == true then
		return rec;
	end
	
	-- Look for nation control settings
	if string.find(sDesc,'under own') ~= nil then
		rec['own']['ctrl'] = 'T';
	elseif string.find(sDesc,'outside own') ~= nil then	
		rec['own']['ctrl'] = 'F';
	end
	
	-- Then conversions
	ic = string.find(sDesc,'Converts');
	if ic ~= nil then
		ival = tonumber(string.match(string.sub(sDesc,ic), "%d+"));		
		if string.find(sDesc,' HP to MP') ~= nil then
			sType = 'cHM';
		else
			sType = 'cMH';
		end		
		if rec['own']['ctrl'] ~= nil then
			rec['own'][sType] = ival;
		else
			rec[sType] = ival;
		end
	end	

	-- See if it's HP%
	ipos = string.find(sDesc,'HP[%+%-]%d+%%');
	if ipos ~= nil then
		ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
		if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
			ival = 0 - ival;
		end
		
		if rec['own']['ctrl'] ~= nil then
			rec['own']['HPP'] = ival;
		else
			rec['HPP'] = ival;
		end
			
		-- remove the "HP" from the desc so that a non-HP% might be found
		local iposp = string.find(sDesc,'%%');
		if ipos == 1 then
			sDesc = string.sub(sDesc,iposp+1,-1);
		else
			sDesc = string.sub(sDesc,1,ipos-1) .. string.sub(sDesc,iposp+1,-1);
		end	
	end
	
	-- See if it's HP 
	ipos = string.find(sDesc,'HP[%+%-]%d+');
	if ipos ~= nil then
		ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
		if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
			ival = 0 - ival;
		end

		if rec['own']['ctrl'] ~= nil then
			rec['own']['HP'] = ival;					
		else
			rec['HP'] = ival;
		end
	end

	-- Now use the same type of logic with MP%
	ipos = string.find(sDesc,'MP[%+%-]%d+%%');
	if ipos ~= nil then
		ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
		if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
			ival = 0 - ival;
		end

		if rec['own']['ctrl'] ~= nil then
			rec['own']['MPP'] = ival;
		else
			rec['MPP'] = ival;
		end
		
		-- remove the "MP" from the desc so that a non-MP% might be found
		local iposp = string.find(sDesc,'%%');
		if ipos == 1 then
			sDesc = string.sub(sDesc,iposp+1,-1);
		else
			sDesc = string.sub(sDesc,1,ipos-1) .. string.sub(sDesc,iposp+1,-1);
		end	
	end
	
	ipos = string.find(sDesc,'MP[%+%-]%d+');
	if ipos ~= nil then
		ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
		if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
			ival = 0 - ival;
		end
		
		if rec['own']['ctrl'] ~= nil then
			rec['own']['MP'] = ival;				
		else
			rec['MP'] = ival;
		end
	end	
	return rec;
end

--[[
	fMakeCodeTable takes the passed, // delimited list and returns the
	individual codes in a table
	
	Returned: code table
--]]

function fMakeCodeTable(sList)
	local sTbl = { };
	local iPos;

	iPos = 1;		-- Assume start at first position
	while iPos ~= nil do
		iPos = string.find(string.sub(sList,3,-1),'//');
		if iPos ~= nil then		
			table.insert(sTbl,string.sub(sList,3,iPos+2-1));	-- skip the //, include up to next //
			sList = string.sub(sList,iPos+2,-1);		-- save portion from next // onwards
		else
			table.insert(sTbl,string.sub(sList,3,-1));		-- skip the //
		end
	end
	return sTbl;
end		-- fMakeCodeTable

--[[
	fValidInlineDynamicCode checks that the formatting of the dynamic code is correct.
	Returned is True or False and the individual pieces

	Returned: T/F	
--]]

function fValidInlineDynamicCode(suCode)
	local iOff = 0;	
	local sOperator,sRoot,ival;
	local tComparators = { 'EQ', 'LT', 'LE', 'GT', 'GE', 'NE'};
	
	if string.find('TP.,TPP,MP.,MPP,HP.,HPP,LVL',string.sub(suCode,1,3)) ~= nil then
		if string.sub(suCode,3,1) ~= '.' then
			iOff = 1;
		end
		
		sRoot = string.sub(suCode,1,2+iOff);		
		sOperator = string.sub(suCode,4+iOff,5+iOff);
		
		if table.find(tComparators,sOperator) ~= nil then
			ival = tonumber(string.sub(suCode,7+iOff,-1));
			return true,sRoot,sOperator,ival;
		else
			return false;
		end	
	else
		return false;
	end
end		-- fValidInlineCode

--[[
	fEvalComparison builds the comparison check and then evaluates it

	Returned: T/F	
--]]

function fEvalComparison(sOperator,ival,iP)
	local bGood = false;
	
	if sOperator == 'EQ' then
		bGood = (iP == ival);
	elseif sOperator == 'LT' then
		bGood = (iP < ival);
	elseif sOperator == 'LE' then
		bGood = (iP <= ival);
	elseif sOperator == 'GT' then
		bGood = (iP > ival);
	elseif sOperator == 'GE' then
		bGood = (iP >= ival);
	else
		bGood = (iP ~= ival);
	end
	return bGood;
end		-- fEvalComparison

--[[
	fEvalCodedComparison parses the passed conditional and determines the appropriate
	value to compare. Returned is true or not.
	
	Returned: T/F
--]]

function fEvalCodedComparison(sRoot,sOperator,ival)
	local player = gData.GetPlayer();
	local iP = -1;
	local bGood = false;
	
	if sRoot == 'TP' then
		iP = player.TP;
	elseif sRoot == 'TPP' then
		iP = player.TP/10;
	elseif sRoot == 'MP' then
		iP = player.MP;
	elseif sRoot == 'MPP' then
		iP = player.MPP;
	elseif sRoot == 'HP' then
		iP = player.HP;
	elseif sRoot == 'HPP' then
		iP = player.HPP;
	elseif sRoot == 'LVL' then
		iP = player.MainJobSync;
	end
	
	if iP > -1 then
		bGood = fEvalComparison(sOperator,ival,iP)
	end
	
	return bGood;
end		-- fEvalCodedComparison

--[[
	fValidSpecial sees if the passed gear's settings are valid. Returned is True
	or False.
	
	Returned: T/F	
--]]

function fValidateSpecial(sSlot,sGear)
	local player = gData.GetPlayer();
	local rec = {};
	local gear;
	local bGood = false;
	
	if sSlot == nil then
		return false;
	end

	if sGear == nil then
		return false;
	end
	
	gear = sGear;
	sGear = string.lower(sGear);
	sSlot = string.lower(sSlot);

	rec = fTallyGear(sGear,sSlot);	
	if rec == nil then
		return false;
	end

	-- Do specific calculations based on the name of the piece of gear
	if sGear == 'uggalepih pendant' then
		-- Condition: MP% < 51. MAB bonus. Only visible gear, ignore all 
		-- "Convert HP to MP" check outright first	
		if player.MPP < 51 then
			return true;
		else
			if rec['visible'] == nil then
				return false;
			else
				local iMP = player.MP - rec['visible']['cHM'];
				local imMP = player.MaxMP - rec['invisible']['MP'] - 
					rec['invisible']['cMH'] - rec['invisible']['cHM'];
				local iaMP = player.MaxMP * (rec['invisible']['MPP'] * 0.01);
				bGood = ((iMP/(imMP - iaMP))*100 < 51);
			end
		end
	elseif sGear == 'parade gorget' then
		-- Make sure player needs to have mp added
		if player.MPP - gcinclude.settings.Tolerance > 0 then
			return false;
		end
		
		-- Now, check condition: HP% >= 85. Adds "Refresh". Only visible gear
		-- Let's see if the invisible gear will make a difference
		local iHP = player.MaxHP - rec['invisible']['HP'] - rec['invisible']['cMH'];
		local iaHP = math.floor(rec['invisible']['HP'] * (rec['invisible']['HPP'] * 0.01));
		bGood = ((player.HP/(iHP - iaHP))*100 >= 85);
	elseif sGear == 'sorcerer\'s ring' then
		-- Condition: HP% < 76 and TP% < 100. 
		-- Ignore HP+ (flat and percent) and Convert HP to MP/MP to HP gear.
		
		-- Check outright first	
		if player.HPP < 76 and player.TP/10 < 100 then
			return true;
		else
			if rec['visible'] == nil then
				return false;
			else
				local fHP   = rec['visible']['HP'] + rec['invisible']['HP'];
				local fCH_M = (rec['visible']['cHM'] + rec['invisible']['cHM']) -
						  (rec['visible']['cMH'] + rec['invisible']['cMH']);
				local fHPP  = rec['visible']['HPP'] + rec['invisible']['HPP'];
				local tHP   = player.HP - fHP - fCH_M;
				local nHP   = tHP - (tHP * (fHPP * 0.01));

				if ((nHP/player.MaxHP) * 100) < 76 and player.TP/10 < 100 then
					return true;
				end
			end
		end
	elseif string.find('drake ring,shinobi ring,minstrel\'s ring',sGear) ~= nil then
		if player.HPP <= 75 and player.TP/10 < 100 then
			return true;
		end
	else
		print(chat.header('fValidateSpecial'):append(chat.message('Warning: No special code exists for ' .. gear .. '. Ignoring piece.')));		
	end
	return bGood;
end		-- fValidateSpecial

--[[
	fBuffed determines if the player has the buff/debuff or not. Returned is True/False.
	The passed buff name can be a substring, but that can also lead to miss identifications.

	Returned: T/F
--]]

function gcinclude.fBuffed(test,bStart)
	local buffs = AshitaCore:GetMemoryManager():GetPlayer():GetBuffs();
	local pos;

	if bStart == nil then
		bStart = false;
	end
	
	test = string.lower(test);
	for _, buff in pairs(buffs) do
		local buffString = AshitaCore:GetResourceManager():GetString("buffs.names", buff);
			
		if (buffString) then
			pos = string.find(string.lower(buffString),test);
			if pos ~= nil then
				if (bStart == true and pos == 1) or (bStart == false) then
					return true;
				end
			end
		end
	end
	return false;
end		--  gcinclude.fBuffed

function fBit(p)
    return 2 ^ (p - 1);
end		-- fBit

function fHasBit(x, p)
    return x % (p + p) >= p;
end		-- fHasBit

--[[
	fCheckItemOwned determines if the specified piece of gear is owned by 
	the player. bAccessible further restricts the search to containers 
	that are accessible when outside of your mog house. bOnce indicates 
	if the item only has to be found once before returning a result.
	
	Please note: currently searching storage slips are not supported
	
	Returned: T/F	
--]]

function fCheckItemOwned(gear)
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local containerID,itemEntry,item;
	local tOwned = {
		['own'] = false, ['accessible'] = false, 
		['porter'] = false, ['claim'] = false,
		['locations'] = nil, ['error'] = nil
	};
	
	-- Make sure a piece of gear specified
	if gear == nil then
		tOwned['error'] = 'Invalid gear item';
		return tOwned;
	end
	
	-- Loop through all searching for the passed gear piece
	for i,desc in pairs(gcinclude.STORAGES) do
		containerID = desc['id'];
		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
				item = resources:GetItemById(itemEntry.Id);
				if item.Name[1] == gear.Name[1] then
					tOwned['own'] = true;				
					if tOwned['locations'] == nil then
						tOwned['locations'] = ',' .. desc['name'] .. ',';
					elseif string.find(tOwned['locations'],','..desc['name']..',') == nil then
						tOwned['locations'] = tOwned['locations'] .. desc['name'] .. ',';
					end
					if table.find(gcinclude.EQUIPABLE_LIST,desc['id']) then
						tOwned['accessible'] = true;
					end
				end
			end
		end
	end

	-- if locations defined, remove the encasing commas
	if tOwned['locations'] ~= nil then
		tOwned['locations'] = string.sub(tOwned['locations'],2,-2);
	end

	-- Then loop through storage slips to see if item stored
	for i,desc in pairs(gcinclude.Slips) do
		-- If item slip owned by player...
		if desc['own'] == true then
			local iPos = table.find(desc['items'],gear.Id);
			-- See if the passed gear associated with that slip
			if iPos ~= nil then	
				-- Now figure out if the item is stored on that slip
				local byte = struct.unpack('B',desc['extra'],math.floor((iPos - 1) / 8) + 1);
				if byte < 0 then
                    byte = byte + 256;
                end
				if (fHasBit(byte, fBit((iPos - 1) % 8 + 1))) then
					-- Yup, add the slip name to the location
					tOwned['own'] = true;
					tOwned['porter'] = true;
					if tOwned['locations'] == nil then
						tOwned['locations'] = desc['name'];
					else
						tOwned['locations'] = tOwned['locations'] .. ', ' .. desc['name'];
					end
					break;
				end
			end			
		end			
	end
	
	-- Lastly, see if stored on a claim slip
	for i,desc in pairs(gcinclude.ClaimSlips) do
		if desc['own'] == true and table.find(desc['ids'],gear.Id) ~= nil then
			tOwned['own'] = true;
			tOwned['claim'] = true;
			if tOwned['locations'] == nil then
				tOwned['locations'] = desc['name'];
			else
				tOwned['locations'] = tOwned['locations'] .. ', ' .. desc['name'];
			end
			break;
		end
	end
	
	return tOwned;
end		-- fCheckItemOwned

--[[
	fBardSongType determines if bard song being cast is of the type being passed.

	Returned: T/F
--]]

function fBardSongType(sType)
	local spell = gData.GetAction();
	local bGood = false;
	
	if sType == nil or spell.Name == nil then
		return false;
	end
	
	sType = string.lower(sType);
	
	if sType == 'enh' then
		for i,j in pairs(gcinclude.tSpell['brd-enh']) do
			if string.find(string.lower(spell.Name),j) ~= nil then
				bGood = true;
				break;
			end			
		end
	else
		for i,j in pairs(gcinclude.tSpell['brd-enf']) do
			if string.find(string.lower(spell.Name),j) ~= nil then
				bGood = true;
				break;
			end			
		end
	end
	return bGood;
end		-- fBardSongType

--[[
	fCheckInline checks for a simple conditional on the item passed into it.
	Returned is whether the condition is met and the item's name (minus the
	conditional.

	If the slot name is 'subset', then any checks that are on the slot will 
	be ignored and the result passed back will be false
	
	Returned: T/F,gear name	
--]]

function fCheckInline(gear,sSlot,ts)
	local player = gData.GetPlayer();
	local party = gData.GetParty();
	local pet = gData.GetPet();
	local spell = gData.GetAction();
	local environ = gData.GetEnvironment();
	local timestamp = gData.GetTimestamp();
	local gSet = gData.GetCurrentSet();
	local iPos,ii,suCode;
	local sj = player.SubJob;
	local suCodeTbl = { };
	local bGood = true;
	
	if gear == nil then
		return false,gear;
	end
	if ts == nil then

		ts = gProfile.Sets.CurrentGear;
	end
	
	iPos = string.find(gear,'//');

	if iPos == nil then
		return true,gear;
	end
	
	sGear = string.sub(gear,1,iPos-1);
	suCodeTbl = fMakeCodeTable(string.upper(string.sub(gear,iPos,-1)));

	for ii,suCode in pairs(suCodeTbl) do
		if table.find(gcinclude.DaysOfTheWeek,suCode) ~= nil then
			bGood = (suCode == string.upper(environ.Day));					-- Is it the specified day
		elseif table.find(gcinclude.NotDaysOfTheWeek,suCode) ~= nil then
			bGood = (string.sub(suCode,5,-1) ~= string.upper(environ.Day));	-- Is it not the specified day
		elseif table.find(gcinclude.tSpell['enspell'],string.lower(suCode)) ~= nil then		-- en spells
			bGood = (gData.GetBuffCount(suCode) >= 1);
		elseif table.find(gcinclude.tWeapontypeMelee,suCode) ~= nil then						-- Is main weapon specified type
			bGood = (gSet['Main'] ~= nil and table.find(gProfile.WeaponType[suCode],gSet['Main']) ~= nil);
		elseif table.find(gcinclude.tWeapontypeRange,suCode) ~= nil then						-- Is ranged weapon specified type
			bGood = (gSet['Range'] ~= nil and table.find(gProfile.WeaponType[suCode],gSet['Range']) ~= nil);
		elseif suCode == 'ABSORB' then										-- Spell is an Absorb- type
			bGood = (table.find(gcinclude.tSpell['absorb'],string.lower(spell.Name)));
		elseif suCode == 'BARSPELL' then					--  Player has a "bar" buff
			bGood = (gcinclude.fBuffed('Bar',true));
		elseif suCode == 'BOUND' then						-- Player is bound
			bGood = gcinclude.fBuffed('Bind');
		elseif suCode == 'BLINDED' then						-- Player is blind
			bGood = gcinclude.fBuffed('Blind');
		elseif suCode == 'CARBY' then						-- Pet is carbuncle
			bGood = (fIsPetNamed('Carbuncle'));
		elseif suCode == 'COVER' then						-- Player has cast cover
			bGood = gcinclude.fBuffed('Cover');
		elseif string.sub(suCode,1,3) == 'CR:' then			-- Crafting
			bGood = (gcinclude.Craft == string.sub(suCode,4,-1));
		elseif suCode == 'CURSED' then						-- Player is cursed
			bGood = gcinclude.fBuffed('Curse');
		elseif suCode == 'DARK' then						-- See if spell is dark
			bGood = (spell.Skill == 'Dark Magic');
		elseif suCode == 'DAYTIME' then						-- Time is daytime
			bGood = gcinclude.CheckTime(timestamp.hour,'Daytime',false);
		elseif string.sub(suCode,1,3) == 'DB:' then
			bGood = (player.MainJob == 'BST' and string.upper(string.sub(suCode,4,-1)) == string.upper(gcdisplay.GetCycle('DB')));	
		elseif suCode == 'DIVINE' then						-- See if spell is a divine spell
			bGood = (spell.Skill == 'Divine Magic');
		elseif suCode == 'DOOMED' then						-- Player is doomed or baned
			bGood = (gcinclude.fBuffed('Doom') or gcinclude.fBuffed('Bane'));
		elseif suCode == 'DT_BREATH' then
			bGood = (gcdisplay.GetCycle('DT') == 'B');
		elseif suCode == 'DT_MAGICAL' then
			bGood = (gcdisplay.GetCycle('DT') == 'M');
		elseif suCode == 'DT_PHYSICAL' then
			bGood = (gcdisplay.GetCycle('DT') == 'P');			
		elseif suCode == 'DUSK2DAWN' then					-- Time between dusk and dawn
			bGood = gcinclude.CheckTime(timestamp.hour,DUSK2DAWN,false);
		elseif table.find(gcinclude.tSpell['enspell'],string.lower(suCode)) ~= nil then		-- En*
			bGood = gcinclude.fBuffed(suCode);
		elseif suCode == 'ELEMENTAL' then					-- See if spell is elemental
			bGood = (spell.Skill == 'Elemental Magic');
		elseif suCode == 'EMPTY' then
			bGood = (ts[sSlot] == nil or ts[sSlot] == '');
		elseif suCode == 'ENFEEBLING' then						-- See if spell is dark
			bGood = (spell.Skill == 'Enfeebling Magic');
		elseif suCode == 'ENANY' then						-- check for any en- spell
			bGood = false;
			for i,j in pairs(gcinclude.tSpell['enspell']) do
				if gcinclude.fBuffed(j) == true then
					bGood = true;
					break;
				end
			end
		elseif suCode == 'ENHANCING' then					-- See if spell is an enhancement
			bGood = (spell.Skill == 'Enhancing Magic');
		elseif suCode == 'EVASION' then
			bGood = (gcdisplay.GetToggle('Eva') == true);	
		elseif suCode == 'FULLMOON' then					-- Moon phase: Full Moon
			bGood = (environ.MoonPhase == 'Full Moon');
		elseif string.sub(suCode,1,3) == 'GA:' then			-- Gathering
			bGood = (gcinclude.Gather == string.sub(suCode,4,-1));
		elseif suCode == 'HEALING' then						-- See if spell is healing
			bGood = (spell.Skill == 'Healing Magic');
		elseif suCode == 'HORN' then						-- Is the bard's instrument a horn
			bGood = (gcdisplay.GetCycle('Instrument') == 'Horn');
			elseif suCode == 'IDLE' then
			bGood = gcdisplay.GetToggle('Idle');
		elseif string.find(suCode,'IF:') then				-- Equip if current item is the named item
			bGood = false;		
			if sSlot ~= 'subset' then
				local tCur = gData.GetEquipment();				
				if tCur[sSlot] == nil then					-- Data download issue
					bGood = false;
				else
					local sCur = tCur[sSlot].Name;
					local sItem = string.sub(suCode,4,-1);	
					bGood = (string.lower(sItem) == string.lower(sCur));
				end
			end	
		elseif string.find(suCode,'IFNOE:') then			-- Equip if current item is not the named item or the slot is empty
			bGood = false;		
			if sSlot ~= 'subset' then
				local tCur = gData.GetEquipment();
				local sCur = tCur[sSlot].Name;
				local sItem = string.sub(suCode,7,-1);
				if tCur[sSlot] ~= nil and 
					 string.lower(sCur) == string.lower(sItem) then					-- Data download issue
					bGood = false;
				else
					bGood = true;
				end
			end				
		elseif string.find(suCode,'IFOE:') then				-- Equip if current item is the named item or it's empty
			bGood = false;		
			if sSlot ~= 'subset' then
				local tCur = gData.GetEquipment();
				local sCur = tCur[sSlot].Name;
				if sCur == nil then
					bGood = true;
				else					
					local sItem = string.sub(suCode,6,-1);
					bGood = (string.lower(sItem) == string.lower(sCur));
				end
			end	
		elseif string.find(suCode,'LVLDIV') then			-- Player's level divisable by #
			local iDiv = tonumber(string.sub(suCode,7,-1));
			if iDiv > 0 then
				bGood = (math.floor(player.MainJobSync/iDiv) == player.MainJobSync/iDiv);
			else
				bGood = false;
			end	
		elseif suCode == 'MSJ' then							-- Magical subjob
			bGood = (string.find(gcinclude._sMagicJobs,sj) ~= nil);
		elseif suCode == 'NEWMOON' then						-- Moon phase: New Moon
			bGood = (environ.MoonPhase == 'New Moon');
		elseif suCode == 'NIGHTTIME' then					-- Time is nighttime
			bGood = gcinclude.CheckTime(timestamp.hour,'Nighttime',false);
		elseif suCode == 'NINJUTSU' then					-- See if spell is a ninjutsu
			bGood = (spell.Skill == 'Ninjutsu');
		elseif suCode == 'NO_PET' then						-- Player has no avatar out
			bGood = (pet == nil);
		elseif suCode == 'NO_SMNPET' then					-- Player has no or non-smn pet
			bgood = not gcinclude.fSummonerPet();
		elseif suCode == 'NOT_ME' then
			-- Equip if target is not me
			local me = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0); --t:GetMemberIndex(0);
			local tg = gData.GetTargetIndex();
			bGood = (tg ~= me);
		elseif suCode == 'NOT_OWN' then						-- Player in area not controlled by their nation
			bGood = (gcdisplay.GetCycle('Region') == 'Not Owned');
		elseif suCode == 'NOT_TANK' then					-- TANK is disabled
			local x = gcdisplay.GetToggle('Tank');
			if x == nil or x == false then
				bGood = true;
			else
				bGood = false;
			end		
		elseif suCode == 'NOT_TH' then
			local x = gcdisplay.GetToggle('TH');
			if x == nil or x == false then
				bGood = true;
			else
				bGood = false;
			end
		elseif suCode == 'NOT_UTSUSEMI' then				-- Utsusemi buff is absent
			bGood = (gcinclude.fBuffed('Copy') == false);
		elseif suCode == 'NOT_WSWAP' then					-- WSWAP is disabled
			bGood = (gcinclude.settings.bWSOverride == false and gcdisplay.GetToggle('WSwap') == false);
		elseif string.sub(suCode,1,8) == 'NOT_WTH:' then	-- Does the weather not match
			bGood = (string.find(string.upper(environ.Weather),string.sub(suCode,9,-1)) == nil);
		elseif suCode == 'NOT_WTH-DAY' then					-- Weather does not match day's element
			local sEle = string.upper(environ.DayElement) .. ',NONE';
			bGood = (string.find(sEle,string.upper(environ.WeatherElement)) == nil);
		elseif suCode == 'OWN' then							-- Player in area controlled by their nation
			bGood = (gcdisplay.GetCycle('Region') == 'Owned');
		elseif suCode == 'PARALYZED' then					-- Player is paralyzed
			bGood = gcinclude.fBuffed('Paralysis');
		elseif string.sub(suCode,1,5) == 'PARTY' then		-- is player in a party/alliance
			if suCode == 'PARTY' then
				bGood = (party.InParty == true);
			else
				-- if a number is specified, it means that number or lower
				local ival = 0;
				if string.find(string.sub(suCode,-1),'%d') then
					ival = tonumber(string.sub(suCode,-1));
				end
				bGood = (party.Count <= ival);
			end
		elseif suCode == 'PET' then							-- Does player have a pet
			bGood = (pet ~= nil);
		elseif suCode == 'PETF' then						-- Is player's pet fighting
			bGood = (pet ~= nil and pet.Status == 'Engaged');
		elseif suCode == 'PETNF' then						-- Is player's pet not fighting or they have no pet
			bGood = ((pet ~= nil and pet.Status ~= 'Engaged') or pet == nil);
		elseif suCode == 'PETFNPF' then						-- Is player's pet fighting, but not the player
			bGood = (pet ~= nil and pet.Status == 'Engaged' and player.Status ~= 'Engaged');
		elseif suCode == 'PETRIFIED' then					-- Player is petrified
			bGood = gcinclude.fBuffed('Petrify');
		elseif string.sub(suCode,1,3) == 'PJP' and string.len(suCode) == 6 then	
			local s = string.sub(suCode,4,-1);
			bGood=(fCheckPartyJob(s,false));		-- party has job: //PJP"job"
		elseif string.sub(suCode,1,3) == 'PJPNM' and string.len(suCode) == 8 then	
			local s = string.sub(suCode,6,-1);
			bGood=(fCheckPartyJob(s,true));		-- party has job: //PJPNM"job", not including player
		elseif suCode == 'POISONED' then					-- Player is poisoned
			bGood = gcinclude.fBuffed('Poison');
		elseif suCode == 'SHINING_RUBY' then				-- Player has shining ruby
			bGood = gcinclude.fBuffed('Shining');	
		elseif suCode == 'SINGING' then						-- See if spell is singing
			bGood = (spell.Skill == 'Singing');
		elseif suCode == 'SILENCED' then					-- Player is silenced
			bGood = gcinclude.fBuffed('Silence');
		elseif string.sub(suCode,1,2) == 'SJ' and string.len(suCode) == 5 then	
			bGood = (string.sub(suCode,3,-1) == sj);		-- subjob is: //SJ"job"
		elseif suCode == 'SLEPT' then						-- Player is slept
			bGood = gcinclude.fBuffed('Sleep');
		elseif string.sub(suCode,1,4) == 'SMN:' then
			bGood = (string.lower(spell.Name) == string.lower(string.sub(suCode,5,-1)));
		elseif suCode == 'SMNPET' then						-- Is player's pet a summoned avatar
			bGood = gcinclude.fSummonerPet();
		elseif suCode == 'SMNPETMD' then					-- Does the summoner pet's element match the day?
			if gcinclude.fSummonerPet() == true then
				bGood = (fElementByPetName(pet.Name) == string.lower(environ.DayElement));
			else
				bGood = false;
			end
		elseif suCode == 'SMNPETMW' then					-- Does the player's pet's element match the weather
			if pet ~= nil then
				local sElement = fElementByPetName(pet.Name);
				bGood = (sElement ~= nil and string.find(string.lower(environ.RawWeather),string.lower(sElement)) ~= nil);
			else
				bGood = false;
			end			
		elseif string.sub(suCode,1,3) == 'SP:' then			-- Is song/spell being cast of type
			local s = string.lower(string.sub(suCode,4,-1));
			bGood = (string.find(string.lower(spell.Name),s) ~= nil);			
		elseif suCode == 'SPECIAL' then
			-- Skip SPECIAL if /gc not run. (Sometimes errors.)
			if gcdisplay.GetGC() == false then
				bGood = false;
			else
				if sSlot ~= 'subset' then
					bGood = fValidateSpecial(sSlot,sGear);
				else	-- Invalid inline for a subset
					bGood = false;
				end
			end
		elseif string.sub(suCode,1,6) == 'SPELL:' then
			-- Make sure a spell is being cast
			if spell.Name == nil then
				return false;
			end
			bGood = (string.find(fGetRoot(spell.Name),string.lower(string.sub(suCode,7,-1))) ~= nil);
		elseif suCode == 'SPIKE' then						-- does player have a spike buff
			bGood = (gcinclude.fBuffed('Spike'));
		elseif suCode == 'SPIRIT:ES' then					-- Pet being summoned is a spirit
			bGood = (table.find(gcinclude.tSpell['spirits'],string.lower(spell.Name)) ~= nil);
		elseif suCode == 'SPIRIT:EP' then					-- Current pet is a spirit
			bGood = (pet ~= nil and table.find(gcinclude.tSpell['spirits'],string.lower(pet.Name)) ~= nil);
		elseif suCode == 'STRING' then						-- Is the bard's instrument a string instrument
			if player.MainJob == 'BRD' then
				bGood = (gcdisplay.GetCycle('Instrument') == 'String');
			else
				bGood = false;
			end
		elseif suCode == 'Summoning' then					-- See if spell is a summoning
			bGood = (spell.Skill == 'Summoning');
		elseif suCode == 'TANK' then
			if string.find(gcinclude._TankJobList,player.MainJob) ~= nil then
				bGood = (gcdisplay.GetToggle('Tank') == true);
			else
				bGood = false;
			end
		elseif suCode == 'TH' then
			if player.MainJob == 'THF' then
				bGood = (gcdisplay.GetToggle('TH') == true);
			else
				bGood = false;
			end
		elseif suCode == 'TOWN' then
			-- Equip if in town	
			bGood = (environ.Area ~= nil and table.find(gcinclude.Towns,environ.Area) ~= nil);
		elseif suCode == 'TOWN-AK' then						-- Equip national aketon if in the right town
			local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
			local slcGear = string.lower(sGear);

			if slcGear == 'ducal aketon' then			 
				bGood = (environ.Area ~= nil and 
						(table.find(gcinclude.Windy,environ.Area) ~= nil or
						 table.find(gcinclude.Sandy,environ.Area) ~= nil or 
						 table.find(gcinclude.Bastok,environ.Area) ~= nil or 
						 table.find(gcinclude.Jeuno,environ.Area) ~= nil));
			elseif slcGear == 'federation aketon' then
				bGood = (environ.Area ~= nil and table.find(gcinclude.Windy,environ.Area) and pNation == 2);
			elseif slcGear == 'republic aketon' then
				bGood = (environ.Area ~= nil and table.find(gcinclude.Bastok,environ.Area) and pNation == 1);
			elseif slcGear == 'kingdom aketon' then
				bGood = (environ.Area ~= nil and table.find(gcinclude.Sandy,environ.Area) and pNation == 0);
			else
				bGood = false;
			end						 
		elseif string.find('TP.,TPP,MP.,MPP,HP.,HPP,LVL',string.sub(suCode,1,3)) ~= nil then
			-- Note: LVLDIV will not proc since processed prior to hitting here
			local sRoot,sOperator,ival;
			bGood,sRoot,sOperator,ival = fValidInlineDynamicCode(suCode);		
			if bGood == true then
				bGood = fEvalCodedComparison(sRoot,sOperator,ival);
			else
				bGood = false;
			end
		elseif suCode == 'UTSUSEMI' then
			bGood = gcinclude.fBuffed('Copy');						-- copy image (#)
		elseif suCode == 'WEAKENED' then					-- Player is weakend
			bGood = (gcinclude.fBuffed('Weakness') or gcinclude.fBuffed('Weakened'));
		elseif suCode == 'WSWAP' then						-- Weapon swapping enabledB
			bGood = (gcinclude.settings.bWSOverride == true or gcdisplay.GetToggle('WSwap') == true);
		elseif string.sub(suCode,1,4) == 'WTH:' then		-- Does the weather match
			bGood = (string.find(string.upper(environ.Weather),string.sub(suCode,5,-1)) ~= nil);
		elseif suCode == 'WTH-DAY' then						-- Weather matches day's element
			local sEle = string.upper(environ.DayElement) .. ',NONE';
			bGood = (string.find(sEle,string.upper(environ.WeatherElement)) ~= nil);
--		elseif string.find(suCode,'XYZ_CHECK') then
--			Needs to be implemented		
		else
			print(chat.header('fCheckInline'):append(chat.message('Warning: Unknown code = ' .. suCode .. '. Ignoring piece of gear.')));
			bGood = false;
		end
		
		if bGood == false then
			return false,sGear;
		end
	end
	
	return true,sGear;
end		-- fCheckInline

--[[
	RegionControlDisplay will display all the regions under conquest control
	along with who currently controls them.
--]]

function RegionControlDisplay()
	local sAreas = { 
		[-1] = 'Unassigned',
		[0]  = 'N/A',
		[1]  = 'San d\'Orian',
		[2]  = 'Bastokian',
		[3]  = 'Windurstian',
		[4]  = 'Beastmen',
	};
	
	if gcinclude.OwnNation == -1 then
		gcinclude.OwnNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation() + 1;
	end
	
	if gcinclude.OwnNation < -1 or gcinclude.OwnNation > 4 then
		print(chat.message('Unknown player\'s nation = ' .. tostring(gcinclude.OwnNation)));
	else
		print(chat.message('Player\'s nation = ' .. sAreas[gcinclude.OwnNation]));
	end
	
	print(' ');
	for i,j in pairs(gcinclude.RegionControl) do
		if j['own'] < 0 or j['own'] > 4 then
			print(chat.message('Huh? ' .. i ..' = ' .. tostring(j['own'])));
		else
			for ii,jj in pairs(sAreas) do
				if ii == j['own'] then
					if j['own'] == 0 and gcinclude.fBuffed('Signet') == true then
						print(chat.message(i .. ' = ' .. jj .. ', but not owned gear works'));
					else
						print(chat.message(i ..' = ' .. jj));
					end
					break;
				end
			end
		end
	end
end		-- RegionControlDisplay

function gcinclude.t1(args)
end

--[[
	ptt provides a simple answer to a request until a better answer
	can be formulated. It's intended to help classes that control pets.
	It displays the distance between the player and the pet, the player
	and the target, and the pet and the target.
--]]

function ptt()
	local pEntity = AshitaCore:GetMemoryManager():GetEntity();
	local myIndex = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
    local petIndex = AshitaCore:GetMemoryManager():GetEntity():GetPetTargetIndex(myIndex);
	local targetIndex = gData.GetTargetIndex();
	local x,y,z;
	
	print(' ');
	if petIndex ~= nil and petIndex > 0 then
		x = math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(petIndex));
		print(chat.message(string.format('Player to pet: %.1f',x) .. 'm'));
	else		
		print(chat.message('You have no pet'));
	end

	if targetIndex ~= nil and targetIndex > 0 then
		x = math.sqrt(AshitaCore:GetMemoryManager():GetEntity():GetDistance(targetIndex));
		print(chat.message(string.format('Player to target: %.1f',x) .. 'm'));
	else
		print(chat.message('You have no target'));
	end	

	if petIndex ~= nil and petIndex > 0 and targetIndex ~= nil and targetIndex > 0 then
		x = math.pow(pEntity:GetLocalPositionX(petIndex) - pEntity:GetLocalPositionX(targetIndex),2);
		y = math.pow(pEntity:GetLocalPositionY(petIndex) - pEntity:GetLocalPositionY(targetIndex),2);	
		print(chat.message(string.format('Pet to target: %.1f', math.sqrt(x+y)) .. 'm'));
	end	
end		-- ptt
	
--[[
	MoveToCurrent copies the gear defined in the passed set to current master
	set. Nothing is displayed, this is just a transfer routine.
	
	Update: support for subsets have been added. The subsets are processed first
	(if present) then the rest of the set. Please note this is a recursive function.
	If nesting goes too deep, it will run out of memory.
	
	Note: added the bIgnoreWSWAP parameter. This was added to override WSWAP when
	crafting and gathering sets are loaded. This was needed since it's not uncommon
	for these sets to have weapons of some sort to aid in the craft/gathering.
--]]

function gcinclude.MoveToCurrent(tSet,tMaster,bOverride,bIgnoreWSWAP)
	local player = gData.GetPlayer();
	local item = {};
	local ref = {};
	local ts = {};
	local ts1 = {};
	local root,sK,vRoot,stK,sRoot;
	local bContinue,iNum,bGood,bSkip,bG;

	if tSet == nil or tMaster == nil then
		return;
	end

	if bIgnoreWSWAP == nil then
		bIgnoreWSWAP = false;
	end
	
	-- bOverride indicates that weapons, if specified, will be
	-- equipped regardless of the /WSWAP setting
	if bOverride == nil then
		bOverride = false;
	end

	-- Make sure player's transition between zones is complete
	if player.MainJob == nil or player.MainJob == 'NON' then
		return;
	end

	if type(tSet) == 'string' then
		ts1 = fGetTableByName(tSet);
	else
		ts1 = tSet;
	end
	
	if ts1 == nil then
		return;
	end
	
	-- First walk through the gear slots looking for "subset"
	for k,v in pairs(ts1) do
		sK = string.lower(k);
		
		if sK == 'subset' then
			for ki,vi in ipairs(v) do
				if type(vi) == 'table' then
					ts = vi;
				else
					ts[k] = vi;
				end
			
				-- Then determine the appropriate set to load
				for kk,vv in pairs(ts) do		
					bGood,vRoot = fCheckInline(vv,'subset',tMaster);			
					if bGood == true then
						gcinclude.MoveToCurrent(vRoot,tMaster,bOverride);
						break;
					end
				end
			end			
		end
	end

	-- Now walk the gear set slots again ignoring "subset"
	for k,v in pairs(ts1) do
		bContinue = false;
		sK = string.lower(k);

		-- Only process entries that are not a 'subset'
		if sK ~= 'subset' then	
			-- Check for special case, Ears or Rings
			if string.find('ears,rings',sK) ~= nil then	
				root = string.sub(k,1,-2);
				iNum = 1;
				bContinue = true;
			end
		
			-- if the slot to be populated is one that will reset the player's TP,
			-- make sure that /WSWAP is true or that gcinclude.settings.bWSOverride 
			-- is true  or bIgnoreWSWAP is true.	
			if string.find('main,sub,range',sK) ~= nil then
				bSkip = not (gcdisplay.GetToggle('WSwap') == true 
						or gcinclude.settings.bWSOverride == true
						or bOverride == true
						or bIgnoreWSWAP == true);
			else
				bSkip = false;
			end		

			if bSkip == false then
				ts = {};
				-- Make sure the piece to be processed is a table
				if type(v) == 'table' then
					ts = v;			
				else
					ts[k] = v;				
				end
		
				iNum = 1;
				
				-- Expand out external gear set slot definition (if present)
				local tsb = {};
				if fReferenceCheck(ts) == true then			
					table.clear(gcinclude.tGearLine);
					if fExpandGearLine(sK,ts) == true then
						tsb = gcinclude.tGearLine;
					else
						tsb = nil;	-- Erroneous inline reference: missing slot or ts
					end
				else
					tsb = ts;
				end
				
				-- Walk list of items
				for kk,vv in pairs(tsb) do
					-- Make sure the item is noted in gcinclude.GearDetails
					-- and that the level, job, and accessibility is good
					bG,ref = fGearCheckItem(sK,vv,false,false);
					if bG == true then
						-- See if there's an inline conditional to be checked.
						-- Note the need to distinguish which "ear" or "ring"
						if bContinue then
							stK = root .. tostring(iNum);
						else
							stK = k;
						end

						bGood,vRoot = fCheckInline(vv,stK,tMaster);

						-- If the inline check returns true, process the gear piece
						if bGood == true then
							if bContinue == true then
								stK = root .. tostring(iNum);
								-- Make sure not locked
								if gcinclude.fIsLocked(string.lower(stK)) == false then
									tMaster[stK] = vRoot;
									iNum = iNum + 1;
								else
									if iNum == 1 then
										stK = root .. tostring(iNum+1);								
										if gcinclude.fIsLocked(string.lower(stK)) == false then
											tMaster[stK] = vRoot;
										end
									end
									iNum = 3;	-- This forces the pairing to kick out								
								end						
							else							
								-- Normal single slot							
								if gcinclude.fIsLocked(string.lower(stK)) == false then
									tMaster[stK] = vRoot;									
								end
								break;
							end
						end							
						
						-- When iNum > 2, all special slots of "root" populated
						if iNum > 2 then					
							break;
						end
					end
				end
			end						
		end
	end	
end		-- gcinclude.MoveToCurrent

--[[
	CheckForExceptions makes sure that pieces that must remain in place will remain
	in place before equipping new gear
--]]

function CheckForExceptions(tSet)
	local msg;
	local sList = nil;
	local gear = gData.GetEquipment();
		
	if gcinclude.fBuffed('Enchantment') == true then	
	-- If 'High Brth. Mantle' enchantment going, keep equipped
		if gear.Back ~= nil then
			if gear.Back.Name == 'High Brth. Mantle' and tSet['Back'] ~= 'High Brth. Mantle' then
				tSet['Back'] = 'High Brth. Mantle';
				sList = 'High Breath Mantle';
			elseif gear.Back.Name == 'Breath Mantle' and tSet['Back'] ~= 'Breath Mantle' then
				tSet['Back'] = 'Breath Mantle';
				sList = 'Breath Mantle';
			end
		end
		
		if gear.Ring1 ~= nil then
			-- Albatross Ring can be on either finger. If enchant going, keep equipped
			if gear.Ring1.Name == 'Albatross Ring' and tSet['Ring1'] ~= 'Albatross Ring' then
				tSet['Ring1'] = 'Albatross Ring';
				if sList == nil then
					sList = 'Albatross Ring';
				else
					sList = sList .. ',' .. 'Albatross Ring';
				end
			elseif gear.Ring2.Name == 'Albatross Ring' and tSet['Ring2'] ~= 'Albatross Ring' then
				tSet['Ring2'] = 'Albatross Ring';
				if sList == nil then
					sList = 'Albatross Ring';
				else
					sList = sList .. ',' .. 'Albatross Ring';
				end
			end	
		end
		
		if gear.Main ~= nil then
			-- 'High Mana Wand' and 'Mana Wand' have to be equipped if enchantment going
			if gear.Main.Name == 'High Mana Wand' and tSet['Main'] ~= 'High Mana Wand' then
				tSet['Main'] = 'High Mana Wand';
				if sList == nil then
					sList = 'High Mana Wand';
				else
					sList = sList .. ',' .. 'High Mana Wand';
				end
			elseif gear.Main.Name == 'Mana Wand' and tSet['Main'] ~= 'Mana Wand' then
				tSet['Main'] = 'Mana Wand';
				if sList == nil then
					sList = 'Mana Wand';
				else
					sList = sList .. ',' .. 'Mana Wand';
				end			
			end
		end
		
		if sList ~= nil then
			msg = 'Because of enchantment, the following must be equipped: ' .. sList;
			fDisplayOnce(msg);
		end
	end	
end

--[[
	fMultiSlotLockCheck determines if the passed item is a multislotted item and
	whether there's a lock in place that would inhibit the equipping of the
	item

	Returned: good?,multi-slot?,list of slots
--]]

function fMultiSlotLockCheck(sName)
	local sAffected,sSlot,sMain;
	local bGood = true;
	local bMulti = false;
	local bFound = false;
	local sAllSlots = nil;
	
	if sName == nil then	-- Nothing specified, nothing to check
		return true,false,nil;
	end

	-- Walk the list of multi-slotted items
	for j,k in pairs(gcinclude.multiSlot) do
		-- if there's a match
		if string.lower(sName) == string.lower(k['item']) then
			bFound = true;
			bMulti = true;
			sMain = k['slot'];
			sAllSlots = k['slot'];
			
			-- Determine if any of the affected slots are locked
			sAffected = k['affected'];			
			while sAffected ~= nil and bGood do
				iPos = string.find(sAffected,',');
				if iPos ~= nil then
					sSlot = string.sub(sAffected,1,iPos-1);
					sAffected = string.sub(sAffected,iPos+1,-1);
				else
					sSlot = sAffected;
					sAffected = nil;
				end

				if gcinclude.fIsLocked(sSlot) then
					bGood = false;
					break;
				else
					sAllSlots = sAllSlots .. ',' .. sSlot;
				end
			end
			-- No need to check further items since we found a match
			break;
		end
	end

	-- Assuming a multislot item was matched and that the affected slots
	-- are not locked, make sure the main slot the item is equipped into
	-- is not locked.
	if bGood and bFound then
		bGood = not gcinclude.fIsLocked(sMain);
	end
	return bGood,bMulti,sAllSlots;
end		-- fMultiSlotLockCheck

--[[
	EquipTheGear makes sure that the passed gear set doesn't have an item in a slot
	that is being blocked by another item (e.g., no head gear if a vermillion cloak
	is in the body slot.) It the equips the gear set.
--]]

function gcinclude.EquipTheGear(tSet,bOverride)
	local sSlot,bGood,bMulti,sSlots;
	local iPos,sWhich;

	if tSet == nil then
		return;
	end
	
	if bOverride == nil then
		bOverride = false;
	end
	
	-- Then deal with the multislot items
	for j,k in pairs(gcinclude.multiSlot) do
		if tSet[k['slot']] ~= nil and 
				string.lower(tSet[k['slot']]) == string.lower(k['item']) then
			bGood,bMulti,sSlots = fMultiSlotLockCheck(k['item']);	
			if not bGood then
				tSet[k['slot']] = '';
			elseif bMulti then
				-- The list includes the slot the item is equipped to. Remove
				-- that and null out the affected slots.
				while sSlots ~= nil do
					iPos = string.find(sSlots,',');
					if iPos ~= nil then
						sWhich = string.sub(sSlots,1,iPos-1);
						sSlots = string.sub(sSlots,iPos+1,-1);
					else
						sWhich = sSlots;
						sSlots = nil;
					end				
					-- Empty the affected slots
					if sWhich ~= k['slot'] then
						tSet[sWhich] = '';
					end
				end			
			end
		end
	end
	
	-- And if weapon swapping is not enabled, clear out the top line (except ammo)
	if not (gcdisplay.GetToggle('WSwap') == true or
			gcinclude.settings.bWSOverride == true or bOverride == true) then
		tSet['Main']  = ''; 
		tSet['Sub']   = ''; 
		tSet['Range'] = ''; 
	end
		
	--[[
		There's a funky problem that can occur on rings or ears. If the ear/ring item you're 
		equipping is already equipped and the slot it's suppose to go to now is not the slot 
		that it's currently equipped in, then that slot will be left empty and the item won't
		be equipped. If this is the case, then don't try to move the item. Instead, change 
		which earring/ring goes where to match the position that it already occupies.
	--]]
	
	local current = gData.GetCurrentSet();
	if current ~= nil then
		-- First, check ears
		if (tSet['Ear1'] ~= nil and current['Ear2'] ~= nil and tSet['Ear1'] == current['Ear2']) or 
		   (tSet['Ear2'] ~= nil and current['Ear1'] ~= nil and tSet['Ear2'] == current['Ear1']) then
			local hold = tSet['Ear1'];
			tSet['Ear1'] = tSet['Ear2'];
			tSet['Ear2'] = hold;
		end

		-- Now check rings
		if (tSet['Ring1'] ~= nil and current['Ring2'] ~= nil and tSet['Ring1'] == current['Ring2']) or 
		   (tSet['Ring2'] ~= nil and current['Ring1'] ~= nil and tSet['Ring2'] == current['Ring1']) then
			local hold = tSet['Ring1'];
			tSet['Ring1'] = tSet['Ring2'];
			tSet['Ring2'] = hold;
		end
	end
	
	CheckForExceptions(tSet);
	
	gFunc.ForceEquipSet(tSet);
end			-- gcinclude.EquipTheGear

--[[
	ProgressiveAccuracy is a new form of applying accuracy gear which
	depends on a list of successive stages. The Player predefines the
	stages and based on the stage specified, all stages prior and up
	to that stage will be equipped.
	
	Note: If TANK enabled, but the appropriate Tank_"set" is not
	defined in the Progressive structure, the non-Tank version will
	be used. (In this case it is assumed that inline conditionals
	will distinguish between Tank_ and non-Tank_ gear.)
--]]

function gcinclude.ProgressiveAccuracy(sType)
	local bTank = gcdisplay.GetToggle('Tank');
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
		if bTank == true and gcdisplay.GetAccMax('TAcc') > 0 then
			tmp = 'TAcc';
			if gProfile.Sets.Progressive[field] == nil then
				field = tField['Acc'];
			end
		else
			tmp = 'Acc';
		end
		
		if gcdisplay.GetAccCur(tmp) == 0 then
			return;
		end
	elseif sType == 'RAcc' then
		field = tField['RAcc'];
		if bTank == true and gcdisplay.GetAccMax('TRAcc') > 0 then
			tmp = 'TRAcc';
			if gProfile.Sets.Progressive[field] == nil then
				field = tField['RAcc'];
			end			
		else
			tmp = 'RAcc';
		end
		
		if gcdisplay.GetAccCur(tmp) == 0 then
			return;
		end	
	else
		return;
	end

	if gProfile.Sets.Progressive[field] ~= nil then
		local maxStage = gcdisplay.GetAccCur(tmp);
		for i,j in ipairs(gProfile.Sets.Progressive[field]) do
			if i <= maxStage then
				gcinclude.MoveToCurrent(j,gProfile.Sets.CurrentGear);
			else
				break;
			end
		end		
	else
		local msg = field .. ' undefined in the Progressive structure';
		fDisplayOnce(msg);
	end	
end		-- ProgressiveAccuracy

--[[
	FractionalSet is similar to FractionalAccuracy in that is equips part of a 
	predefined set, but it's not based on accuracy. Instead, it's based on a
	list of slots. (Note that only names are supported and not slot numbers.)
	It creates a temporary set based on the specified slots and equips it. 
--]]

function gcinclude.FractionalSet(hs,sSlots)
	local i,t;
	local tAcc = {};
	local ts = {};
	local bGood,vRoot;
	local bFound = false;
	local bSubset = false;
	
	if hs == nil or sSlots == nil then
		return;
	end
	
	sSlots = string.lower(sSlots);

	if type(hs) == 'string' then
		ts = fGetTableByName(hs);
	else
		ts = hs;
	end
	
	for j,k in pairs(ts) do
		t = string.lower(j)
		if t == 'subset' then
			bSubset = true;
		else
			-- Since ears and rings are pseudo slots, if specified, make
			-- sure to match with the actual slot names
			if string.find(sSlots,'ear') ~= nil and
				(t == 'ears' or t == 'ear1' or t == 'ear2') then
					tAcc[j] = k;
			elseif string.find(sSlots,'ring') ~= nil and 
				(t == 'rings' or t == 'ring1' or t == 'ring2') then
					tAcc[j] = k;
			-- at this point it's an exact match
			elseif string.find(sSlots,t) ~= nil then
				tAcc[j] = k;
			end
			if bFound == false then
				-- This indicates there was a match copied and that the
				-- temporary set will need to be moved to current
				bFound = (tAcc[j] ~= nil);
			end
		end			
	end

	if bFound == true then
		gcinclude.MoveToCurrent(tAcc,gProfile.Sets.CurrentGear);
	else
		if bSubset == true then
			for j,k in pairs(ts) do
				t = string.lower(j)
				if t == 'subset' then
					for ji,ki in ipairs(k) do
						if type(ki) == 'table' then
							ts = ki;
						else
							ts[j] = ki;
						end
					
						-- Then determine the appropriate set to load
						for kk,vv in pairs(ts) do
							bGood,vRoot = fCheckInline(vv,'subset');
							if bGood == true then
								gcinclude.FractionalSet(vRoot,sSlots)
								break;
							end
						end
					end
				end
			end
		end
	end
end	-- gcinclude.FractionalSet

--[[
	MaxSpell determines if the passed in spell is in the tiered list and then which
	tier would be the highest that could be cast by the player. The routine checks to
	make sure you're high enough level to cast the spell, have enough MP, do you
	know the spell and whether that spell is off cool down. If indicated, the found
	spell can be cast
--]]

function MaxSpell(sSpell,sTarget,bCast)
	local player = gData.GetPlayer();
	local sMain = player.MainJob;
	local sSub = player.SubJob;
	local MainLvl = player.MainJobSync;
	local SubLvl = player.SubJobSync;
	local root,sCmd,iMax,bmCast;
	local tSpell = {};

	if sSpell == nil then 
		print(chat.header('MaxSpell'):append(chat.message('No spell specified. Aborting...')));
		return;
	end

	-- Make sure all parameters passed make sense	
	if bCast == nil then
		-- indicate that the found spell shouldn't be cast
		bCast = false;	
	end
	
	if sTarget == nil then
		-- indicates the target of the spell if cast. Note: if bCast is false,
		-- target has no meaning
		sTarget = '<' .. gcinclude.settings.DefaultSpellTarget .. '>';
	elseif string.find(sTarget,'<') == nil then
		sTarget = '<' .. sTarget .. '>';
	end
	
	root = fGetRoot(sSpell);
	
	-- See if in tiered magic structure.
	if gcinclude.TMtest[root] == nil then
		print(chat.header('MaxSpell'):append(chat.message('FYI: '..sSpell..' not found, probably not a tiered spell.')));
		if bCast == true then
			-- Let's try to cast it even so. Assuming everything, just try
			print(chat.header('MaxSpell'):append(chat.message('FYI: Trying to cast '.. sSpell.. ' as is')));
			sCmd = '/ma "' .. sSpell .. '" ' .. sTarget;
			AshitaCore:GetChatManager():QueueCommand(1, sCmd);
		end
	else
		iMax = 0;
		for i,j in pairs(gcinclude.TMtest[root]) do	
			-- Test for level. Level checks both main and sub jobs
			if (j[sMain] ~= nil and j[sMain] <= MainLvl) or 
			   (j[sSub] ~= nil and j[sSub] <= SubLvl) then
				-- Make sure the player knows the spell
				if AshitaCore:GetMemoryManager():GetPlayer():HasSpell(j['SID']) then
					-- Now save it. Processing happens after all level reqs are checked
					tSpell[j['Tier']] = { ['Name'] = j['Name'], ['SID'] = j['SID'], ['MP'] = j['MP'] };						
					iMax = iMax +  1;
				else
					print(chat.header('MaxSpell'):append(chat.message('FYI: You should be able to cast '.. j['Name'] .. ', but don\'t know it. Skipping')));
				end
			end
		end
		
		-- Figure out which one to call now
		bmCast = false;
		if iMax > 0 then
			for i = iMax, 1, -1 do
				if player.MP >= tSpell[i]['MP'] then
					if AshitaCore:GetMemoryManager():GetRecast():GetSpellTimer(tSpell[i]['SID']) == 0 then
						if bCast then
							print(chat.header('MaxSpell'):append(chat.message('Casting ' .. tSpell[i]['Name'])));
							sCmd = '/ma "' .. tSpell[i]['Name'] .. '" ' .. sTarget;
							AshitaCore:GetChatManager():QueueCommand(1, sCmd);
							bmCast = true;
							break;
						else
							print(chat.header('MaxSpell'):append(chat.message(tSpell[i]['Name'] .. ' is the maximum version you can cast now.')));
							break;
						end
					else
						print(chat.header('MaxSpell'):append(chat.message('FYI: ' .. tSpell[i]['Name'] .. ' is on cool down')));
					end
				else
					print(chat.header('MaxSpell'):append(chat.message('FYI: Insufficeint MP to cast ' .. tSpell[i]['Name'])));
				end
			end
		else
			print(chat.header('MaxSpell'):append(chat.message('FYI: unable to cast any ' .. root .. ' spells (at this time)')));
			bmCast = true; -- set to skip next warning message
		end
		if bmCast == false then
			print(chat.header('MaxSpell'):append(chat.message('FYI: No spell cast')));
		end
	end
end		-- MaxSpell

--[[
	MaxSong determines if the passed in song is in the tiered list and then which
	tier would be the highest that could be cast by the player. The routine checks to
	make sure you're high enough level to cast the song, do you know the song and 
	whether that song is off cool down. If indicated, the found	song can be cast
--]]

function MaxSong(sSong,sTarget,bCast)
	local player = gData.GetPlayer();
	local sMain = player.MainJob;
	local sSub = player.SubJob;
	local MainLvl = player.MainJobSync;
	local SubLvl = player.SubJobSync;
	local lSong,root,iMax,bmCast;
	local tSong = {};
	
	if sSong == nil then
		return;
	end
	lSong = string.lower(sSong);
	
	if bCast == nil then
		bCast = false;
	end
	
	if sTarget == nil then
		sTarget = '<' .. gcinclude.settings.DefaultSongTarget .. '>';
	elseif string.find(sTarget,'<') == nil then
		sTarget = '<' .. sTarget .. '>';
	end
	
	-- Now, determine the type based on the passed in spell
	root = nil;
	for i,j in pairs(gcinclude.TStest) do
		for ii,jj in pairs(j) do
			if string.find(i .. ',' ..string.lower(jj['Name']),lSong) ~= nil then
				root = i;				
				break;
			end
		end
		if root ~= nil then
			break;
		end
	end
	
	if root ~= nil then
		iMax = 0;
		for i,j in pairs(gcinclude.TStest[root]) do	
			-- Test for level. Level checks both main and sub jobs
			if (sMain == 'BRD' and j['Lvl'] <= MainLvl) or 
			   (sSub == 'BRD' and j['Lvl'] <= SubLvl) then
				-- Make sure the player knows the spell
				if AshitaCore:GetMemoryManager():GetPlayer():HasSpell(j['SID']) then
					-- Now save it. Processing happens after all level reqs are checked
					tSong[j['Tier']] = { ['Name'] = j['Name'], ['SID'] = j['SID'] };						
					iMax = iMax +  1;
				else
					print(chat.header('MaxSong'):append(chat.message('FYI: You should be able to cast'.. j['Name'] .. ', but don\'t know it. Skipping')));
				end
			end
		end
		
		-- Figure out which one to call now
		bmCast = false;
		if iMax > 0 then
			for i = iMax, 1, -1 do
				if AshitaCore:GetMemoryManager():GetRecast():GetSpellTimer(tSong[i]['SID']) == 0 then
					if bCast then
						print(chat.header('MaxSong'):append(chat.message('Casting ' .. tSong[i]['Name'])));
						sCmd = '/ma "' .. tSong[i]['Name'] .. '" ' .. sTarget;
						AshitaCore:GetChatManager():QueueCommand(1, sCmd);
						bmCast = true;
						break;
					else
						print(chat.header('MaxSong'):append(chat.message(tSong[i]['Name'] .. ' is the maximum version you can cast now.')));
						break;
					end
				else
					print(chat.header('MaxSong'):append(chat.message('FYI: ' .. tSong[i]['Name'] .. ' is on cool down')));
				end
			end
		else
			print(chat.header('MaxSong'):append(chat.message('FYI: unable to cast any ' .. root .. ' songs (at this time)')));
			bmCast = true; -- set to skip next warning message
		end
		if bmCast == false then
			print(chat.header('MaxSong'):append(chat.message('FYI: No song cast')));
		end
	else
		print(chat.header('MaxSong'):append(chat.message('FYI: '..sSong..' not found, probably not a tiered song.')));
		if bCast == true then
			-- Let's try to cast it even so. Assuming everything, just try
			print(chat.header('MaxSong'):append(chat.message('FYI: Trying to cast '.. sSong.. ' as is')));
			sCmd = '/ma "' .. sSong .. '" ' .. sTarget;
			AshitaCore:GetChatManager():QueueCommand(1, sCmd);
		end		
	end
end		-- MaxSong
	
--[[
	fCheckForElementalGearByValue is a generalized routine that searches to see 
	if the targetted elemental gear should be equipped (assuming you have the piece 
	accessible.)
	
	sWhat		type of elemental gear to check: staff,obi,gorget
	sWhich		which associated list to check: Affinity,Summons,MEacc,eleWS
	sElement	the key to match in the appropriate list
	
	returns: Record of the item,element
--]]

function gcinclude.fCheckForElementalGearByValue(sWhat,sWhich,sElement)
	local player = gData.GetPlayer();
	local sRoot,bGood,sTarget;

	-- Make sure locks won't block equipping the item
	if sWhat == 'staff' and (gcinclude.fIsLocked('main') or gcinclude.fIsLocked('sub')) then -- staff
		return nil,nil;
	elseif sWhat == 'obi' and gcinclude.fIsLocked('waist') then -- obi
		return nil,nil;
	elseif gcinclude.fIsLocked('neck') then -- gorget
		return nil,nil;
	end
		
	-- What's searched for is sometimes a "root" and other times an "as-is"
	if string.find('Affinity,MEacc',sWhich) ~= nil then
		sRoot = fGetRoot(sElement);	
	elseif string.find('Summons,eleWS,SongAffinity',sWhich) ~= nil then
		sRoot = string.lower(sElement);
	else
		print(chat.header('fCheckForElementalGearByValue'):append(chat.message('Unknown field to search: ' ..sWhich)));
		return nil,nil;
	end

	-- Determine target slot
	if sWhat == 'obi' then
		sTarget = 'waist';
	elseif sWhat == 'gorget' then
		sTarget = 'neck';
	else
		sTarget = 'main';
	end

	-- Then determine which gear is the appropriate one
	for i,j in pairs(gcinclude.tElemental_gear[sWhat]) do
		-- Looking for elemental entries. Ignore the rest

		if string.find(gcinclude._AllElements,i) ~= nil then
			-- Look for a match in the associated field			
			if sWhat == 'staff' then
				if table.find(gcinclude.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then			
					-- Make sure the link to the dynamic table is in place
					bGood,gcinclude.tElemental_gear[sWhat][i]['HQ']['Ref'] = 
						fGearCheckItem(sTarget,gcinclude.tElemental_gear[sWhat][i]['HQ']['Name'],false,false);
					bGood,gcinclude.tElemental_gear[sWhat][i]['NQ']['Ref'] = 
						fGearCheckItem(sTarget,gcinclude.tElemental_gear[sWhat][i]['NQ']['Name'],false,false);
					-- Make sure ref in place before checking accessibility
					if gcinclude.tElemental_gear[sWhat][i]['HQ']['Ref'] ~= nil and 
					   gcinclude.tElemental_gear[sWhat][i]['HQ']['Ref']['accessible'] == true then
						return gcinclude.tElemental_gear[sWhat][i]['HQ']['Name'],i;
					elseif gcinclude.tElemental_gear[sWhat][i]['NQ']['Ref'] ~= nil and 
					   gcinclude.tElemental_gear[sWhat][i]['NQ']['Ref']['accessible'] == true then
						return gcinclude.tElemental_gear[sWhat][i]['NQ']['Name'],i;
					else
						return nil,nil;
					end
				end
			elseif sWhat == 'obi' or sWhat == 'gorget' then
				if table.find(gcinclude.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
					bGood,gcinclude.tElemental_gear[sWhat][i]['Ref'] = 
						fGearCheckItem(sTarget,gcinclude.tElemental_gear[sWhat][i]['Name'],false,false);
				end
				
				-- Then determine if there's an obi or gorget that matches
				if gcinclude.tElemental_gear[sWhat][i]['Ref'] ~= nil and
					gcinclude.tElemental_gear[sWhat][i]['Ref']['accessible'] == true then
					return gcinclude.tElemental_gear[sWhat][i]['Name'],i;
				end
			end
		end
	end
	-- Since we got here, either the search string wasn't found in the appropriate
	-- area or it was found, but the player doesn't have the item or it's inaccessible.
	return nil,nil;
end		-- fCheckForElementalGearByValue

--[[
	fDisplayOnce will display the passed message if it hasn't been displayed
	before. If it has been displayed, the message isn't repeated.
--]]

function fDisplayOnce(msg,bOverride)
	local tmp;

	if msg == nil then
		return false;
	end

	if bOverride == nil then
		bOverride = false;
	end

	-- Let's deal with a limitation of LUA. (Wanna guess how long
	-- it took me to realize this was the problem? Yeah...)
	if string.length(msg) > 40 then
		tmp = string.sub(msg,1,40);
	else
		tmp = msg;
	end
	
	if gcinclude.GearWarnings == nil or 
		(gcinclude.GearWarnings ~= nil and 
			string.find(gcinclude.GearWarnings,tmp) == nil) or
		bOverride == true then
		print(chat.message(msg));
		if gcinclude.GearWarnings == nil then
			gcinclude.GearWarnings = msg;
		else
			gcinclude.GearWarnings = gcinclude.GearWarnings .. ',' .. msg;
		end
		return true;
	else
		return false;
	end
end		-- fDisplayOnce

--[[
	fSwapToStave determines if swapping your weapon out for one of the elemental staves makes
	sense and does it for you while remembering what weapon/offhand you had equipped.
--]]

function gcinclude.fSwapToStave(sStave,noSave,cs)
	local ew = gData.GetEquipment();
	local player = gData.GetPlayer();
	local msg = nil;
	local sGear;
	local eWeap = nil;
	local eOff = nil;

	-- This is needed for a timing issue
	if sStave == nil then
		return;
	end
	
	-- Make sure that auto staves enabled and that locks will not prevent equipping a staff
	-- Remember: both "main" and "sub" locks will cause a block
	if gcinclude.settings.bAutoStaveSwapping == false then
		msg = 'due to auto-swapping turned off!';
	elseif gcinclude.fIsLocked('main') == true or gcinclude.fIsLocked('sub') == true then
		msg = 'due to lock(s)!'
	end
	
	if msg ~= nil then
		msg = 'Warning: Unable to swap to a ' .. sStave .. ' ' .. msg;
		fDisplayOnce(msg);
		return;
	end
	
	-- Now, process the stave swap
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end
	
	if ew['Sub'] ~= nil then
		eOff = ew['Sub'].Name;
	end;

	if (gcdisplay.GetToggle('WSwap') or gcinclude.settings.bWSOverride) then	
		-- See if a current weapon is the one of the targetted staves
		if not (eWeap == nil or (eWeap ~= nil and string.lower(eWeap) == string.lower(sStave))) then
			-- save the weapon so it can be equipped again
			if eWeap ~= gcinclude.weapon and noSave == false and gcinclude.settings.bWSOverride == false then
				gcinclude.weapon = eWeap;
				gcinclude.offhand = eOff;
			end
		end

		-- Check versus level of player.
		if player.MainJobSync >= gcinclude.tElemental_gear['staff']['level'] then
			cs['Main'] = sStave;
		else
			msg = 'Warning: Unable to swap to a ' .. sStave .. ' due to level!';
			fDisplayOnce(msg);
		end
	end
end		-- gcinclude.fSwapToStave

--[[
	EquipItem processes the passed arguments and equips the specified item 
	(whether by coded entry or name) into the appropriate equipment slot, 
	then locks the appropriate slot
	
		/equipit|ei code|"item name" slot
--]]

function EquipItem(args)
	local iName,iSlot,ref,msg;
	local bMulti,sSlots,bGood;
		
	if #args > 1 then
		-- see if the item specified is a code	
		for k,v in pairs(gcinclude.tEquipIt) do
			if string.lower(k) == string.lower(args[2]) then
				iName = v['Name'];
				iSlot = v['Slot'];
				break;
			end
		end

		-- if it wasn't a code, the item should be explicitly identified and the slot
		if iName == nil then
			iName = args[2];
			if #args > 2 then
				if string.find('ears,rings',args[3]) ~= nil then
					args[3] = string.sub(args[3],1,-2);
				end
				iSlot = args[3];
			else
				print(chat.header('EquipItem'):append(chat.message('Error: incomplete /equipit command: /equipit code|name slot. Command ignored.')));
				return;
			end
		end

		-- First check that it's a valid item and it's accessible
		bGood,ref = fGearCheckItem(iSlot,iName,false,false);
		if not bGood then
			if ref ~= nil then
				if ref['valid'] == false then
					print(chat.header('EquipItem'):append(chat.message('Error: Invalid piece of gear specified - ' .. iName)));
				elseif ref['accessible'] == false then	
					print(chat.header('EquipItem'):append(chat.message('Error: Specified gear inaccessible - ' .. iName .. ': ' .. ref['locations'])));
				elseif ref['job'] == false then
					print(chat.header('EquipItem'):append(chat.message('Error: Specified gear not usable by your job - ' .. iName)));
				else
					print(chat.header('EquipItem'):append(chat.message('Error: Specified gear\'s level too high - ' .. iName .. ': ' .. tostring(ref['level']))));
				end
			else
				print(chat.header('EquipItem'):append(chat.message('Error: Either parameters missing, data downloading, or gear record not created.')));
				return;
			end
		end
		
		-- Now, see if this item is a multislot item. 
		bGood,bMulti,sSlots = fMultiSlotLockCheck(iName);
		if not bGood then
			-- There's a lock blocking the equipping of this item. Let the 
			-- User know.
			print(chat.header('EquipItem'):append(chat.message('Unable to equip ' .. iName .. ' due to locks!')));
			return;
		else
			-- If item is not a multislot item, then make sure the item
			-- slot is set.
			if sSlots == nil then
				sSlots = iSlot;
			end
		end

		-- ring and ear need a slot appended to it. Just assume "1"
		if not bMulti and string.find('ring,ear',string.lower(iSlot)) ~= nil then
			iSlot = iSlot .. '1';
			sSlots = iSlot;
		end

		-- Make sure the slot is formatted right (assuming it's just a case issue)
		-- Note that if the item is multislotted, it is already formatted correctly
		iSlot = string.upper(string.sub(iSlot,1,1)) .. string.lower(string.sub(iSlot,2));
				
		-- Now try and load the item	
		gFunc.ForceEquip(iSlot,iName);	
		LockUnlock('locks','lock',sSlots);
		local sList = fGetLockedList('locks');
		gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);	
	else
		print(chat.message('List of /equipit codes and items:'));
	
		for i,j in pairs(gcinclude.tEquipIt) do
			print(chat.message(string.format('%-s - %s',i,j['Name'])));
		end
	end
end		-- EquipItem

--[[
	fGetTableByName returns the gear set that is associated with the set name passed to it.
	It does this by walking the Sets (either gProfile.Sets or gcinclude.Sets)
	
	Returned: gearset/nil
--]]

function fGetTableByName(sName)
	local s,s2;
	local sName2;
	
	sName2 = string.lower(sName);
	s = string.find(sName2,'gcinclude');
	if s == nil then
		for k,l in pairs(gProfile.Sets) do
			if string.lower(k) == sName2 then
				return l;
			end
		end
	else
		s2 = string.sub(sName2,s+2,-1);
	end
	
	if s2 == nil then
		s2 = sName2;
	end
	
	for k,l in pairs(gcinclude.Sets) do
		if string.lower(k) == s2 then
			return l;
		end
	end
	
	return nil;
end		-- fGetTableByName

--[[
	SMGControl processes the /smg command line and coordinates the
	creation of the appropriate report. Unlike before multiple types
	of reports can be created in one invocation. The order of the
	parameters are not fixed. The player can input any or all of
	the parameters in one call.
	
	/smg {noac} gs=set name{,set name...} slot=slot name{,slot name...}
		>{file name}
	
	"/smg" with no parameters is a display all command to the screen.
	Limitations on the output window might clip this report if the
	job has a large number of gear sets.
	
	"noac" is an optional parameter. It indicates that items from sets 
	that can't be equipped should be displayed
	
	"gs=" indicates that the following one or more gear sets should
	have their item definitions displayed
	
	"slot=" is similar to "gs=" except the player is targetting specific
	slots across all sets although they can limit which sets by including
	a "gs=" definition.
	
	">{filename}" indicates that the report's output should be redirected
	to a file. This will specifically be in the Reports directory found
	under ...HorizonXI\Game\config\addons\luashitacast. If a file name is
	provided, it will be used. If one is not provided, a name will be
	created for the report: character_job_date.rpt. Using an existing
	file name will replace the contents.
--]]
--[[
function SMGControl(args)
	local bFile = false;
	local bNoac = false;
	local slots = {};
	local gs{};

-- The following is copied from the HandleCommands. It needs to be
-- reworked.	
	if #args == 1 then				-- Show a list of all gear
		DisplayGD_AW(nil,nil);
	else
		for i=2,#args,1 do
			args[i] = string.lower(args[1]));
			if args[i] == 'noac' then
				bNoac = true;
			elseif string.find(args[i],'gs=') ~= nil then
				args[i] = string.sub(args[i],4,-1);
				if string.find(args[i],',') then
				end
			end
			if args[2] ~= nil then
		local ls = string.lower(args[2]);
		if ls == 'noac' then		-- Show a list of gear where accessible is false
			DisplayGD_AW('noac');
		elseif string.len(ls) > 5 and string.sub(ls,1,5) == 'slot=' then
			DisplayGD_S(string.sub(ls,6,-1));
		elseif string.len(ls) > 3 and string.sub(ls,1,3) == 'gs=' then 
			DisplayGD_Gs(string.sub(ls,4,-1));
		end 
	end
end		-- SMGControl
--]]

--[[
	HandleCommands processes any commands typed into luashitacast as defined in this file
--]]

function gcinclude.HandleCommands(args)
	
	if not gcinclude.AliasList:contains(args[1]) then return end
	
	local player = gData.GetPlayer();
	local bTank = gcdisplay.GetToggle('Tank');
	local sList, sKey, sSet;
	
	-- Clear out the local copy of current gear
	gcinclude.ClearSet(gcinclude.sets.CurrentGear);
	
	args[1] = string.lower(args[1]);
	if (args[1] == 'gswap') then			-- turns gear swapping on or off
		gcdisplay.AdvanceToggle('GSwap');
	elseif args[1] == 't1' then				-- This is a test invoker
		gcinclude.t1(args);
	elseif args[1] == 'gc' then
		local bForce = false;
		local sList = nil;
		
		if args[2] == nil then
			bForce = true;	
		elseif string.lower(args[2]) == 'list' then
			sList = 'list';
			bForce = false;
		end
		GearCheck(sList,bForce);
		gcdisplay.SetGC(true);
    elseif args[1] == 'gcmessages' then		-- turns feedback on/off for all commands
		gcinclude.settings.Messages = not gcinclude.settings.Messages;
		if gcinclude.settings.Messages then
			s = 'enabled';
		else	
			s = 'disabled';
		end
		print(chat.header('HandleCommands'):append(chat.message('Chat messages are ' .. s)));
	elseif (args[1] == 'wsdistance') then	-- Turns on/off the check for weapons skill distance or sets the distance
		if (tonumber(args[2])) then 
			gcinclude.settings.WScheck = true;
			gcinclude.settings.WSdistance = tonumber(args[2]);
			print(chat.header('HandleCommands'):append(chat.message('WS Distance is on and set to ' .. gcinclude.settings.WSdistance)));
		else
			gcinclude.settings.WScheck = not gcinclude.settings.WScheck;
			print(chat.header('HandleCommands'):append(chat.message('WS distance check is now set to ' .. tostring(gcinclude.settings.WScheck))));
		end
	elseif (args[1] == 'dt') then		-- Indicates the type of damage taken gear that will be equipped if desired
		if #args == 1 then				-- No qualifier, assume next in set
			gcdisplay.AdvanceCycle('DT');
		else
			local cType = string.upper(string.sub(args[2],1,1));
			local sType = gcinclude.OFF;
			if  cType == 'M' then
				sType = gcinclude.MAG;
			elseif cType == 'B' then
				sType = gcinclude.BRE;
			elseif cType == 'P' then
				sType = gcinclude.PHY;			
			end				
			gcdisplay.SetCycle('DT',sType);
		end
	elseif (args[1] == 'kite') then			-- Turns on/off whether movement gear is equipped
		gcdisplay.AdvanceToggle('Kite');
	elseif (args[1] == 'idle') then			-- Turns on/off whether movement gear is equipped
		gcdisplay.AdvanceToggle('Idle');
	elseif (args[1] == 'macc') then			-- Turns on/off whether tanking gear is equipped
		if string.find(gcinclude._sMagicJobs,player.MainJob) ~= nil or
			string.find(gcinclude._sMagicJobs,player.SubJob) ~= nil then
			gcdisplay.AdvanceToggle('Macc');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Your job does not support the magic accuracy. Ignoring command')))
		end
	elseif (args[1] == 'ptt') then
		ptt();
	elseif (args[1] == 'tank') then			-- Turns on/off whether tanking gear is equipped
		if string.find(gcinclude._TankJobList,player.MainJob) ~= nil then
			gcdisplay.AdvanceToggle('Tank');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Your job does not support the tanking option. Ignoring command')))
		end				
	elseif (args[1] == 'eva') then			-- Turns on/off whether evasion gear should be equipped
		gcdisplay.AdvanceToggle('Eva');
	elseif (args[1] == 'wswap') then		-- Turns on/off whether weapon swapping is permitted
		if gcinclude.settings.bWSOverride == false then
			gcdisplay.AdvanceToggle('WSwap');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Weapon swapping always enabled on ' .. player.MainJob .. '. Ignoring command')))
		end		
	elseif (args[1] == 'sbp') then			-- Turns on/off whether the blood pact message is shown
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			gcdisplay.AdvanceToggle('sBP');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: /sBP is only available to summoners. Ignoring command')));
		end
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automatic Jug assignment enabled
		if player.MainJob == 'BST' then
			gcdisplay.AdvanceToggle('AJug');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: /AJug is only available to beastmasters. Ignoring command')));
		end	
	elseif (args[1] == 'th') then			-- Turns on/off whether TH gear should be equipped
		if player.MainJob == 'THF' then
			gcdisplay.AdvanceToggle('TH');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: /TH is only available to thieves. Ignoring command')));
		end			
	elseif (args[1] == 'db') then
		if player.MainJob == 'BST' then
			if args[2] ~= nil then
				gcdisplay.SetCycle('DB',string.upper(args[2]));
			else
				gcdisplay.AdvanceCycle('DB');
			end
		else
			print(chat.header('HandleCommands'):append(chat.message('Your job does not support that command. Ignoring.')));
		end
	elseif (args[1] == 'acc' or args[1] == 'racc') then
		local tmp,narg;
		local num = 0;		-- 0 means turn off that type of accuracy
		local narg;
		
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
		
		if args[2] ~= nil then
			if args[2] == '?' then
				print(' ');
				if string.find('Acc,TAcc',tmp) ~= nil then
					print(chat.message(string.format('Accuracy at stage: %d',gcdisplay.GetAccCur('Acc'))));
					if string.find(gcinclude._TankJobList,player.MainJob) ~= nil then
						print(chat.message(string.format('Tank Accuracy at stage: %d',gcdisplay.GetAccCur('TAcc'))));
					end
				else
					print(chat.message(string.format('Ranged Accuracy at stage: %d',gcdisplay.GetAccCur('RAcc'))));
					if string.find(gcinclude._TankJobList,player.MainJob) ~= nil then
						print(chat.message(string.format('Tank Ranged Accuracy at stage: %d',gcdisplay.GetAccCur('TRAcc'))));
					end	
				end
				return;
			end
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
			if tmp == 'Acc' then
				tmp = 'TAcc';
			elseif tmp == 'TAcc' then
				tmp = 'Acc';
			elseif tmp == 'RAcc' then
				tmp = 'TRAcc';
			elseif tmp == 'TRAcc' then
				tmp = 'RAcc';
			end
			gcdisplay.SetAccCur(tmp,0);
			if tmp == 'Acc' or tmp == 'TAcc' then
				print(chat.message('Accuracy has been turned off'));
			else
				print(chat.message('Ranged Accuracy has been turned off'));
			end
		else
			gcdisplay.SetAccCur(tmp,num);
			print(chat.message(string.format('%s stage set to %d',tmp,num)));
		end				
	elseif (args[1] == 'lock') then
		if args[2] ~= nil then
			LockUnlock('lock','lock',args[2]);
		end
		sList = fGetLockedList('locks');		
		if sList ~= nil then
			print(chat.message('The following slot(s) are locked: ' .. sList));
		else
			print(chat.message('All slots are unlocked'));
		end	
		gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);
	elseif (args[1] == 'unlock') then
		if args[2] == nil then
			args[2] = 'all';
		end
		
		if args[2] ~= nil then
			LockUnlock('locks','unlock',args[2]);
			if string.lower(args[2]) == 'all' then
				print(chat.message('All slots are unlocked'));
			else
				print(chat.message('\'' .. args[2] .. '\' have been unlocked'));
			end
		end
		sList = fGetLockedList('locks');
		gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);
	elseif (args[1] == 'rc') then							-- Display region controls
		RegionControlDisplay();
	elseif (args[1] == 'rv') then
		RefreshVariables();
	elseif (args[1] == 'showit') then						-- Shows debug info for specified type
		DB_ShowIt();
	elseif (args[1] == 'smg') then							-- Show My Gear			
		if #args == 1 then				-- Show a list of all gear
			DisplayGD_AW(nil);
		elseif args[2] ~= nil then
			local ls = string.lower(args[2]);
			if ls == 'noac' then		-- Show a list of gear where accessible is false
				DisplayGD_AW('noac');
			elseif string.len(ls) > 5 and string.sub(ls,1,5) == 'slot=' then
				DisplayGD_S(string.sub(ls,6,-1));
			elseif string.len(ls) > 3 and string.sub(ls,1,3) == 'gs=' then 
				DisplayGD_Gs(string.sub(ls,4,-1));
			end 
		end
	elseif (args[1] == 'gearset' or args[1] == 'gs') then	-- Forces a gear set to be loaded and turns GSWAP off
		if #args > 1 then
			local sArg = string.upper(args[2]);
			local sTmp = ',' .. gcinclude.Crafting_Types .. ',';
			local sTmp2 = ',' ..gcinclude.Gathering_Types .. ',';
			if string.find(sTmp,sArg) ~= nil or string.find(sTmp2,sArg) ~= nil then
				-- gather or crafting set
				if string.find(sTmp,sArg) then
					-- Crafting set
					gcinclude.Craft = sArg;
					gcinclude.MoveToCurrent(gcinclude.sets.Crafting,gcinclude.sets.CurrentGear,false,true);					
				else
				-- Gather set
					gcinclude.Gather = sArg;
					gcinclude.MoveToCurrent(gcinclude.sets.Gathering,gcinclude.sets.CurrentGear,false,true);
				end
			else
				local tTable = fGetTableByName(sArg);	-- Change string to table
				if tTable ~= nil then
					gcinclude.MoveToCurrent(tTable,gcinclude.sets.CurrentGear,true);
				else
					print(chat.header('HandleCommands'):append(chat.message('Gear set not found: ' .. sArg)));
				end
			end
			
			gcinclude.EquipTheGear(gcinclude.sets.CurrentGear,true);
			fLockSlotsBySet(gcinclude.sets.CurrentGear);
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: No set specified for /gearset. Command ignored.')));
		end	
	elseif (args[1] == 'horn' or args[1] == 'string') then
		if player.MainJob == 'BRD' then
			if args[1] == 'horn' then
				gcdisplay.SetCycle('Instrument',gcinclude.HORN);
			else
				gcdisplay.SetCycle('Instrument',gcinclude.STRING);
			end
		else
			print(chat.header('HandleCommands'):append(chat.message('Your job does not support that command. Ignoring.')));
		end
	elseif (args[1] == 'maxspell') then			-- Determines highest level spell to cast
		MaxSpell(args[2],args[3],true);
	elseif (args[1] == 'maxsong') then			-- Determines highest level song to cast
		MaxSong(args[2],args[3],true);
	elseif args[1] == 'equipit' or args[1] == 'ei' then			-- Equip specified item
		EquipItem(args);
	elseif args[1] == 'ver' then				-- Display version/change log
		DisplayVersion();
	end

	if gcinclude.settings.Messages then
		gcinclude.Message(toggle, status)
	end
end		-- gcinclude.HandleCommands

--[[
	This function returns the weak element to the passed in element.
	
	Returned: element passed element is weak to
--]]

function fEleWeak(ele)
	local sWeak = nil;
	
	ele = string.lower(ele);
	for i,v in pairs(gcinclude.tWeekDayElement) do
		if v['strong'] == ele then
			sWeak = v['weak'];
			break;
		end
	end		
	return sWeak;
end		-- fEleWeak

--[[
	fCheckObiDW determines if the weather/day element makes equiping an elemental 
	obi advantageous.
	
	Please note: Elemental obis can be useful when closing a skillchain with certain 
	weaponskills. This code does NOT track that opportunity, so it is not even considered.
--]]

function fCheckObiDW(ele)
	local sEnvironment = gData.GetEnvironment();
	local sWeak = fEleWeak(ele);
	local sDay = sEnvironment.DayElement;
	local PctDay = 0;
	local PctWeather = 0;
	local PctIridesecene = 0;

	ele = string.lower(ele);
	
	-- First, the day
	if string.lower(sDay) == ele then
		PctDay = 10;
	elseif string.lower(sWeak) == ele then
		PctDay = -10;
	end
	
	-- Next the weather
	if string.lower(sEnvironment.WeatherElement) == ele then
		if string.find(sEnvironment.Weather,'x2') ~= nil then 		-- There's a storm of the element
			PctWeather = 25
		else 
			PctWeather = 10;
		end
	else
		-- Weather doesn't match. Check to see if the weather weakens the element
		if sEnvironment.WeatherElement == sWeak then
			if string.find(sEnvironment.Weather,'x2') ~= nil then 	-- There's a storm of the element
				PctWeather = -25
			else 
				PctWeather = -10;
			end
		end
	end
	
	-- Lastly, check for iridescence/prismatic
	local g = gEquip.GetCurrentEquip(1);
	if AshitaCore:GetResourceManager():GetItemById(g.Item.Id).Name[1] == 'Claustrum' then	-- Only case I know of with prismatic
		if PctWeather < 0 then
			PctIridesecene = -10;
		else
			PctIridesecene = 10;
		end
	end
	PctWeather = PctWeather + PctIridesecene;
	
	return PctDay,PctWeather;
end		-- fCheckObiDW

--[[
	fGetRoot determines the "base" of a spell name. (The base is the first word in the spell name.)
	
	An optional parameter, bVersion, indicates if only the version should be cut off. (i.e., remove
	the I, II, III portion, etc.)
	
	Returned: root of spell name
--]]

function fGetRoot(sSpellName,bVersion)
	local i;
	local root = sSpellName;
	
	if bVersion == nil then
		bVersion = false;
	end
	
	sSpellName = string.lower(sSpellName);
	
	if bVersion == true then
		i = string.find(spellName, " [^ ]*$");
		if i ~= nil and string.find('i,ii,iii,iv,v,vi',string.sub(spellName,i+1,-1)) ~= nil then
			root = string.sub(spellName,1,i-1);
		else
			root = spellName;
		end
	else
		i = string.find(sSpellName,' ');
		if i ~= nil then
			root = string.sub(sSpellName,1,i-1);
		else
			root = sSpellName;
		end
		
		-- Only ninjutsu have a ":" in the name. Remove if found on the end
		if string.sub(root,-1,-1) == ':' then
			root = string.sub(root,1,-2);
		end
	end
	return root;
end		-- fGetRoot

--[[
	CheckWsBailout determines if there's a debuff that will inhibit automatic cancelling of a weapons
	skill or if insufficient TP exist to do a weapon skill
	
	Returned: T/F
--]]

function gcinclude.CheckWsBailout()
	local player = gData.GetPlayer();
	local ws = gData.GetAction();
	local target = gData.GetActionTarget();
	local sleep = gData.GetBuffCount('Sleep');
	local petrify = gData.GetBuffCount('Petrification');
	local stun = gData.GetBuffCount('Stun');
	local terror = gData.GetBuffCount('Terror');
	local amnesia = gData.GetBuffCount('Amnesia');
	local charm = gData.GetBuffCount('Charm');

	if gcinclude.settings.WScheck and not gcinclude.DistanceWS:contains(ws.Name) and (tonumber(target.Distance) > gcinclude.settings.WSdistance) then
		print(chat.header('CheckWsBailout'):append(chat.message('Distance to mob is too far! Move closer or increase WS distance')));
		print(chat.header('CheckWsBailout'):append(chat.message('Can change WS distance allowed by using /wsdistance ##')));
		return false;
	elseif (player.TP <= 999) or (sleep+petrify+stun+terror+amnesia+charm >= 1) then
		return false;
	end
		
	return true;
end		-- gcinclude.CheckWsBailout

--[[
	PetReward scans all equipable storage containers for all of the pet foods and
	tallies which ones the player has. Then, it picks the one likely to have the
	most benefit for the "reward" based on the level and what was passed in.
--]]

function gcinclude.fPetReward(sFood,bMax)
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local player = gData.GetPlayer();
	local tStorage = gcinclude.EQUIPABLE_NONHOLIDAY;
	local containerID;
	local i1,i2,step;
	local _ammo = 4;	-- Lock # for ammo slot
	
	if bMax == nil then
		bMax = true;
	end

	-- Make sure ammo slot isn't locked
	if gcinclude.tLocks[_ammo]['lock'] == true then
		print(chat.header('PetReward'):append(chat.message('Ammo slot locked. Unable to equip any pet food')));
		return false;
	end
		
	-- Reset the pet food indicators
	for i,j in ipairs(gcinclude.tPetFood) do
		j['have'] = false;;
	end
	
	-- Now, note which pet foods the player has
	for i,j in ipairs(tStorage) do
		containerID = j['id'];
		-- then loop through the container
		for k = 1, inventory:GetContainerCountMax(containerID), 1 do
			local itemEntry = inventory:GetContainerItem(containerID, k);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
				local item = resources:GetItemById(itemEntry.Id);
				if item ~= nil then
					local sName = string.lower(item.Name[1]);
					for ii,jj in ipairs(gcinclude.tPetFood) do
						if sName == jj['name'] then
							jj['have'] = true;
						end
					end
				end
			end
		end
	end
	
	-- Determine order to process
	if bMax == true then
		i1 = 1; i2 = gcinclude._PetFoodCount; step = 1;
	else
		i1 = gcinclude._PetFoodCount; i2 = 1; step = -1;
	end

	-- Then process what was found
	local iFound = -1;
	for i = i1,i2,step do
		if sFood ~= nil and string.lower(sFood) == gcinclude.tPetFood[i]['name'] and 
			gcinclude.tPetFood[i]['have'] == true and 
			gcinclude.tPetFood[i]['lvl'] <= player.MainJobSync then
			iFound = i;
		elseif gcinclude.tPetFood[i]['have'] == true and 
			gcinclude.tPetFood[i]['lvl'] <= player.MainJobSync then
			iFound = i;
		end
	end
	
	if iFound > 0 then
		local sName = gcinclude.tPetFood[iFound]['name'];
		gFunc.ForceEquip('Ammo', sName);
		print(chat.header('PetReward'):append(chat.message('Equipping: ' .. sName)));
		return true;
	elseif sName ~= nil then
		print(chat.header('PetReward'):append(chat.message('Error: ' .. sFood .. ' not found or you cannot equip it.')));
		return false;
	else
		print(chat.header('PetReward'):append(chat.message('Error: No equipable pet food found.')));
		return false;
	end
end		-- PetReward

--[[
	Unload ensures that the display settings are saved, the aliases are removed,
	and the display objects are removed
--]]

function gcinclude.Unload()
	--SaveSettingFile();
	ClearAlias();
	ashita.events.unregister('packet_in', 'packet_in_callback1');	
	gcdisplay.Unload();
end		-- gcinclude.Unload

--[[
	Initialize gives luashitacast it's initial settings
--]]

function gcinclude.Initialize()
	gcdisplay.Initialize:once(2);
	SetVariables:once(2);
	SetAlias:once(2);
	gcdisplay.SetGC(false);
end		-- gcinclude.Initialize

--[[
	HandlePrecast equips the appropriate precast gear
--]]

function gcinclude.HandlePrecast()
	local spell = gData.GetAction();
	local bTank = gcdisplay.GetToggle('Tank');
	
	if bTank == nil then
		bTank = false;
	end
	
	if spell.Skill == 'Singing' then
		gcinclude.MoveToCurrent(gProfile.Sets.SingingPrecast,gProfile.Sets.CurrentGear);
	else
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Precast,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Precast,gProfile.Sets.CurrentGear);
		end
	end
end		-- HandlePrecast

--[[
	MidcastSinging handles all of the equipment management when a song is cast.
--]]

function MidcastSinging()
	local spell = gData.GetAction();
	local bTank = gcdisplay.GetToggle('Tank');
	
	if bTank == nil then
		bTank = false;
	end
	
	if fBardSongType('enh') == true then
		-- Enhancement song
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnhancementSinging,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnhancementSinging,gProfile.Sets.CurrentGear);
		end
	elseif fBardSongType('enf') == true then
		-- Enfeebling song
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnfeeblingSinging,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnfeeblingSinging,gProfile.Sets.CurrentGear);
		end		
	end
end

--[[
	MidcastHealingMagic handles all of the equipment management when a healing spell
	is cast. There are three types of spells handled in 'healing magic': cures to
	heal players hit points, debuffs to remove status effects on players, and offensive
	cures to do damage to undead monsters. Each type is handled here.
--]]

function MidcastHealingMagic()
	local ti = gData.GetTargetIndex();
	local target = gData.GetEntity(ti);
	local spell = gData.GetAction();
	local root,sGear,pDay,pWeather;
	local bTank,sEle;
	
	bTank = gcdisplay.GetToggle('Tank');	
	root = fGetRoot(spell.Name);

	if string.find('curaga,cure',root) == nil then
		-- Start with the non-cure based spells. Even if magic accuracy
		-- indicated, these spells always hit and thus do not need magic
		-- accuracy. Further, an elemental stave will have no effect
		-- either.
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_HealingMagic,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.HealingMagic,gProfile.Sets.CurrentGear);
		end
	else
		if target ~= nil then
			-- Some type of cure
			if target.Type == 'Monster' then
				-- Until I figure out how to determine that a monster is undead, just assume
				-- that if targetting a monster, it is undead.
				if bTank == true then
					gcinclude.MoveToCurrent(gProfile.Sets.Tank_OffensiveCuring,gProfile.Sets.CurrentGear);
				else
					gcinclude.MoveToCurrent(gProfile.Sets.OffensiveCuring,gProfile.Sets.CurrentGear);
				end
			
				-- Check for an elemental obi since this is an offensive spell. First
				-- determine if a bonus is possible based on day's element and/or weather
				sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
				if sGear ~= nil then
					pDay,pWeather = fCheckObiDW(sEle);
					if pDay + pWeather > 0 then
						gProfile.Sets.CurrentGear['Waist'] = sGear;
					end
				end	

				-- See if Macc should be added
				if gcdisplay.GetToggle('Macc') then
					if bTank == true then
						gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
					else
						gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
					end	
				end				
			else
				-- This is the the type of curing magic most folks assume happens
				if bTank == true then
					gcinclude.MoveToCurrent(gProfile.Sets.Tank_CuringMagic,gProfile.Sets.CurrentGear);
				else
					gcinclude.MoveToCurrent(gProfile.Sets.CuringMagic,gProfile.Sets.CurrentGear);
				end
			end
		end
		
		-- While the reasoning is different, both types of "cures" can use an elemental
		-- stave. (Offensive cures take advantage of affinity while regular cures 
		-- appreciate the cure potency on a light-based staff.)
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
		if sGear ~= nil then
			gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
		end
	end
end		-- MidcastHealingMagic

--[[
	MidcastDarkMagic handles all of the equipment management when a dark spell
	is cast. Dark spells are dependent on the level of the dark spell magic. Most 
	dark spells extract a "strength" from the target and gives it to the player.
	The exception is bio/ii (which is a dot and lowers the target's attack), 
	tractor, and stun. 
	
	For drain/Aspir, Dark Magic Skill > all else. For absorb spells, 
	Dark=Macc=2xINT
--]]

function MidcastDarkMagic()
	local spell = gData.GetAction();
	local ew = gData.GetEquipment();
	local root,pDay,pWeather,sGear;
	local bTank,sEle;
	
	bTank = gcdisplay.GetToggle('Tank');
	root = fGetRoot(spell.Name);
	
	if table.find(gcinclude.tSpell['absorb'],root) ~= nil then
		-- Absorb spell
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Absorb,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Absorb,gProfile.Sets.CurrentGear);
		end
		
		-- See if Macc should be added
		if gcdisplay.GetToggle('Macc') then
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
			end		
		end
	elseif root == 'drain' then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Drain,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Drain,gProfile.Sets.CurrentGear);
		end	
		
		-- Check for an elemental obi. First determine if a bonus is possible 
		-- based on day's element and/or weather
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
		if sGear ~= nil then
			pDay,pWeather = fCheckObiDW(sEle);
			if pDay + pWeather > 0 then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end	

		-- See if Macc should be added
		if gcdisplay.GetToggle('Macc') then
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
			end		
		end
		
		-- And an elemental staff, for the affinity
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
		if sGear ~= nil then
			-- There's a wild exception that needs to be checked here. If /WSWAP is
			-- enabled and the player is wielding Y's Scythe (only equipable by DRK)
			-- and sGear = 'Dark Staff', then don't equip it. Y's scythe grants +1
			-- dark magic affinity which is what a dark staff does. There's no
			-- advantage to equipping the staff.
			if not (gcdisplay.GetToggle('WSwap') == true and
					ew['Main'] == 'Y\'s Scythe' and sGear == 'Dark Staff') then
				gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
			end
		end
		
	elseif root == 'aspir' then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Aspir,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Aspir,gProfile.Sets.CurrentGear);
		end	
		
		-- Check for an elemental obi. First determine if a bonus is possible 
		-- based on day's element and/or weather
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
		if sGear ~= nil then
			pDay,pWeather = fCheckObiDW(sEle);
			if pDay + pWeather > 0 then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end	
		
		-- Check for an elemental obi. First determine if a bonus is possible 
		-- based on day's element and/or weather
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
		if sGear ~= nil then
			pDay,pWeather = fCheckObiDW(sEle);
			if pDay + pWeather > 0 then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end	

		-- See if Macc should be added
		if gcdisplay.GetToggle('Macc') then
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
			end		
		end
		
		-- And an elemental staff, for the affinity
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
		if sGear ~= nil then
			-- There's a wild exception that needs to be checked here. If /WSWAP is
			-- enabled and the player is wielding Y's Scythe (only equipable by DRK)
			-- and sGear = 'Dark Staff', then don't equip it. Y's scythe grants +1
			-- dark magic affinity which is what a dark staff does. There's no
			-- advantage to equipping the staff.
			if not (gcdisplay.GetToggle('WSwap') == true and
					ew['Main'] == 'Y\'s Scythe' and sGear == 'Dark Staff') then
				gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
			end
		end		
	elseif root == 'dread' then
		-- Dread Spikes, out of era, but coming soonish
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Dreadspikes,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Dreadspikes,gProfile.Sets.CurrentGear);
		end			
	else
		-- All other dark magic spells 
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_DarkMagic,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.DarkMagic,gProfile.Sets.CurrentGear);
		end
		
		-- See if Macc should be added
		if gcdisplay.GetToggle('Macc') then
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
			end		
		end		
	end
end		-- MidcastDarkMagic

--[[
	MidcastDivineMagic handles all of the equipment management when a divine spell
	is cast. Divine Magic is highly dependent on the level of the divine magic skill
	and MND. This routine handles three types of divine magic: offensive "nukes",
	Enfeebling, and Enhancing.
--]]

function MidcastDivineMagic()
	local spell = gData.GetAction();
	local root,pDay,pWeather,sGear;
	local bTank,sEle;
	
	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end
	
	root = fGetRoot(spell.Name);
	
	if table.find({'banish','banishga','holy','enlight'},root) ~= nil then
		-- Offensive Divine
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_OffensiveDivine,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.OffensiveDivine,gProfile.Sets.CurrentGear);
		end
		
		-- Check for an elemental obi. First determine if a bonus is possible 
		-- based on day's element and/or weather
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
		if sGear ~= nil then
			pDay,pWeather = fCheckObiDW(sEle);
			if pDay + pWeather > 0 then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end	

		-- See if Macc should be added
		if gcdisplay.GetToggle('Macc') then
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
			end		
		end		
	elseif table.find({'flash','repose'},root) ~= nil then
		-- Enfeebling Divine
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnfeebleDivine,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnfeebleDivine,gProfile.Sets.CurrentGear);
		end
		
		-- See if Macc should be added
		if gcdisplay.GetToggle('Macc') then
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
			end		
		end		
	else
		-- Enhancing Divine
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnhanceDivine,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnhanceDivine,gProfile.Sets.CurrentGear);
		end
	end
	
	-- And see if an elemental staff would be useful, for the affinity
	sGear,sEle = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
	if sGear ~= nil then
		gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
	end			
end		-- MidcastDivineMagic

--[[
	MidcastEnfeeblingMagic handles all of the equipment management when a enfeeble
	spell is cast. There are two subdivisions of enfeeble spells, those that depend
	on INT and those that depend on MND. The rest of the gear details are fairly in
	common: you want high enfeeble magic skill, magical accuracy, etc.
--]]

function MidcastEnfeeblingMagic()
	local spell = gData.GetAction();
	local root,bTank,sGear,sEle;
	local pDay,pWeather;
	
	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end
	
	root = fGetRoot(spell.Name);
	
	if table.find(gcinclude.tSpell['int'],root) ~= nil then
		-- INT: gravity,bind,blind,dispel,sleep,sleepga,poison,poisonga
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnfeeblingINT,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnfeeblingINT,gProfile.Sets.CurrentGear);
		end		
	elseif table.find(gcinclude.tSpell['mnd'],root) ~= nil then
		-- MND: paralyze,silence,slow,slowga,frazzle,distract
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnfeeblingMND,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnfeeblingMND,gProfile.Sets.CurrentGear);
		end	
	else
		-- dia and diaga
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnfeeblingMagic,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnfeeblingMagic,gProfile.Sets.CurrentGear);
		end		
	end
	
	-- See if an elemental obi would make sense for the Magical Elemental accuracy
	sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
	if sGear ~= nil then
		pDay,pWeather = fCheckObiDW(sEle);
		if pDay + pWeather > 0 then
			gProfile.Sets.CurrentGear['Waist'] = sGear;
		end
	end

	-- See if Macc should be added
	if gcdisplay.GetToggle('Macc') then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
		end		
	end
		
	-- And then if an elemental staff would be useful, for the affinity
	sGear,sEle = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
	if sGear ~= nil then
		gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
	end
end		-- MidcastEnfeeblingMagic

--[[
	MidcastEnhancingMagic handles all of the equipment management when a enhancing
	spell is cast. Enhancing magic is sort of a catch-all category. It includes bar-
	spells, en-spells, protect/shell, regen, spikes, teleports, and warps.
--]]

function MidcastEnhancingMagic()
	local spell = gData.GetAction();
	local root,bTank,sGear,sEle;
	local pDay,pWeather;
	
	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end
	
	root = fGetRoot(spell.Name);
	
	if table.find(gcinclude.tSpell['barspell']['ele'],root) ~= nil or 
		table.find(gcinclude.tSpell['barspell']['status'],root) ~= nil then
		-- A bar spell. Use inline conditional to distinguish type, as needed
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Barspell,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Barspell,gProfile.Sets.CurrentGear);
		end	
	elseif table.find(gcinclude.tSpell['enspell'],root) ~= nil then
		-- En-spell: en"element". Sword enhancing gear applies to all melee
		-- weapons and is applied when the spell is cast. Damage is calculated
		-- after the weapon hits.
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Enspell,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Enspell,gProfile.Sets.CurrentGear);
		end
		-- See if an elemental obi would make sense for the Magical Elemental accuracy
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
		if sGear ~= nil then
			pDay,pWeather = fCheckObiDW(sEle);
			if pDay + pWeather > 0 then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end 
	elseif table.find(gcinclude.tSpell['spikes'],root) ~= nil then
		-- Spike spell: Blaze, Ice, and Shock. Damage based on INT (capped), MAB, 
		-- day/weather bonuses, Magic Affinity at time of hit. Enhancing spike gear 
		-- is equipped when cast
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Spike,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Spike,gProfile.Sets.CurrentGear);
		end	
		-- See if an elemental obi would make sense for the Magical Elemental accuracy
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
		if sGear ~= nil then
			pDay,pWeather = fCheckObiDW(sEle);
			if pDay + pWeather > 0 then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end
	elseif root == 'stoneskin' then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Stoneskin,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Stoneskin,gProfile.Sets.CurrentGear);
		end	
	elseif root == 'sneak' then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Sneak,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Sneak,gProfile.Sets.CurrentGear);
		end		
	elseif root == 'invisible' then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Invisible,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Invisible,gProfile.Sets.CurrentGear);
		end	
	elseif root == 'phalanx' then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Phalanx,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Phalanx,gProfile.Sets.CurrentGear);
		end			
	else
		-- Catch all for the rest of the enhancing spells
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnhancingMagic,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnhancingMagic,gProfile.Sets.CurrentGear);
		end	
	end
end		-- MidcastEnhancingMagic

--[[
	MidcastElementalMagic handles all of the equipment management when a elemental
	spell is cast. Elemental magic on HorizonXI falls into two categories: nukes and
	debuffs. (You could argue that ancient magic should be broken out too, but for
	now it falls under nukes.) All elemental spells are INT based. Nukes are based
	on the difference in INT between the caster and the target whereas for debuffs,
	the caster's INT is used to determine the amount of damage each tick the debuff
	does as well as how much the target stat is lowered. Elemental magic skill is
	used to determine accuracy and spell interruption rate.
--]]

function MidcastElementalMagic()
	local spell = gData.GetAction();
	local root,bTank,sGear,sEle;
	local pDay,pWeather;

	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end
	
	root = fGetRoot(spell.Name);
	
	if table.find(gcinclude.tSpell['eDebuff'],root) ~= nil then
		-- Elemental debuff
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_ElementalDebuff,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.ElementalDebuff,gProfile.Sets.CurrentGear);
		end			
	else
		-- Nuke
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_ElementalNuke,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.ElementalNuke,gProfile.Sets.CurrentGear);
		end				
	end
	
	-- See if an elemental obi would make sense for the Magical Elemental accuracy
	sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
	if sGear ~= nil then
		pDay,pWeather = fCheckObiDW(sEle);
		if pDay + pWeather > 0 then
			gProfile.Sets.CurrentGear['Waist'] = sGear;
		end
	end

	-- See if Macc should be added
	if gcdisplay.GetToggle('Macc') then
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Macc,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Macc,gProfile.Sets.CurrentGear);
		end		
	end
		
	-- And then if an elemental staff would be useful, for the affinity
	sGear,sEle = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
	if sGear ~= nil then
		gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
	end	
end		-- MidcastElementalMagic

--[[
	MidcastSummoning handles all of the equipment management when a avatar/spirit
	summoning spell is cast. This routine is straightforward. While some gear is
	more advantageous than others, that can all be handled with inline conditionals.
--]]

function MidcastSummoning()
	local bTank;

	bTank = gcdisplay.GetToggle('Tank');
	
	if bTank == nil then
		bTank = false;
	end
	
	if bTank == true then
		gcinclude.MoveToCurrent(gProfile.Sets.Tank_Summoning,gProfile.Sets.CurrentGear);
	else
		gcinclude.MoveToCurrent(gProfile.Sets.Summoning,gProfile.Sets.CurrentGear);
	end
end		-- MidcastSummoning

--[[
	MidcastBlueMagic handles all of the equipment management when a blue magic
	spell is cast.
	
	WIP: Until more details are released on how HorizonXI are implementing blue
	magic, this function is nothing but a stub function
--]]

function MidcastBlueMagic()
end		-- MidcastBlueMagic

--[[
	fMidcastGeomancy handles all of the equipment management when a geomancy magic
	spell is cast.
	
	WIP: Until more details are released on how HorizonXI is implementing geomancy
	magic, this function is nothing but a stub function
--]]

function MidcastGeomancy()
end		-- MidcastGeomancy

--[[
	MidcastNinjutsu handles all of the equipment management when a ninjutsu spell
	is cast.
--]]

function MidcastNinjutsu()
	local spell = gData.GetAction();
	local root,bTank,sGear,sEle;
	local pDay,pWeather;

	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end

	root = fGetRoot(spell.Name);

	-- There's three types of ninjutsu: buff, debuff and elemental. Anything
	-- else is a mystery and will be processed with the current gear.
	if table.find(gcinclude.tSpell['nin-buff'],root) ~= nil then
		-- Buff
	
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_NinjutsuBuff,gProfile.Sets.CurrentGear);
		else	
			gcinclude.MoveToCurrent(gProfile.Sets.NinjutsuBuff,gProfile.Sets.CurrentGear);
		end		
	else
		if table.find(gcinclude.tSpell['nin-debuff'],root) ~= nil then
			-- Debuff
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_NinjutsuDebuff,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.NinjutsuDebuff,gProfile.Sets.CurrentGear);
			end	
		elseif table.find(gcinclude.tSpell['nin-ele'],root) ~= nil then
			-- Elemental
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_NinjutsuElemental,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.NinjutsuElemental,gProfile.Sets.CurrentGear);
			end	
			
			-- See if an elemental obi would make sense for the Magical Elemental accuracy
			sGear,sEle = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
			if sGear ~= nil then
				pDay,pWeather = fCheckObiDW(sEle);
				if pDay + pWeather > 0 then
					gProfile.Sets.CurrentGear['Waist'] = sGear;
				end
			end			
		else
			return;
		end
		
		-- And then if an elemental staff would be useful, for the affinity
		sGear,sEle = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
		if sGear ~= nil then
			gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
		end		
	end
end		-- MidcastNinjutsu

--[[
	HandleMidcast is a coordinating routine that's used to call the independent types
	of magic routines. Unlike the previous implementation, the multitude of tiers are
	collapsed to help give the player agency.
--]]

function gcinclude.HandleMidcast()
	local spell = gData.GetAction();

	if spell.Skill == 'Singing' then
		MidcastSinging();
	elseif spell.Skill == 'Healing Magic' then
		MidcastHealingMagic();
	elseif spell.Skill == 'Dark Magic' then
		MidcastDarkMagic();
	elseif spell.Skill == 'Divine Magic' then
		MidcastDivineMagic();
	elseif spell.Skill == 'Enfeebling Magic' then
		MidcastEnfeeblingMagic();
	elseif spell.Skill == 'Enhancing Magic' then
		MidcastEnhancingMagic();
	elseif spell.Skill == 'Elemental Magic' then
		MidcastElementalMagic();
	elseif spell.Skill == 'Summoning' then
		MidcastSummoning();
	--elseif spell.Skill == 'Blue Magic' then
	--	MidcastBlueMagic();
	--elseif spell.Skill == 'Geomancy' then
	--	MidcastGeomancy();
	elseif spell.Skill == 'Ninjutsu' then
		MidcastNinjutsu();
	end
end		-- MidcastNinjutsu

--[[
	gcinclude.fHandleWeaponskill loads the appropriate gear for the weapon skill
	you're doing
--]]

function gcinclude.fHandleWeaponskill()
	local ws = gData.GetAction();
	local lName = string.lower(ws.Name);
	local sName,sEle;
	local t = {};
	
	gcinclude.settings.priorityWeaponSkill = string.upper(gcinclude.settings.priorityWeaponSkill);
	for i = 1,string.len(gcinclude.settings.priorityWeaponSkill),1 do
		cKey = string.sub(gcinclude.settings.priorityWeaponSkill,i,i);
		if cKey == 'A' then			-- weaponskill set
			for i,j in pairs(gcinclude.tWeaponSkills) do
				if table.find(j,lName) ~= nil then
					sName = 'WS_' .. i;
					t = fGetTableByName(sName);
					if t ~= nil then
						gcinclude.MoveToCurrent(t,gProfile.Sets.CurrentGear);
					end
				break;
				end
			end
		elseif cKey == 'B' then		-- elemental gorget
			-- An elemental gorget will add the fTP (at least 10% more damage) to the first hit 
			-- of an elemental weapon skill (and many multi-hit weapon skills replicate the fTP
			-- for all the hits.) Also, they give +10 Accuracy to all of the weapon skill's hits
			-- and a 1% chance of not depleting the player's TP after the weapon skill.
	
			local sGorget,sEle = gcinclude.fCheckForElementalGearByValue('gorget','eleWS',ws.Name);
			if sGorget ~= nil then
				gProfile.Sets.CurrentGear['Neck'] = sGorget;
			end
	
		elseif cKey == 'D' then		-- accuracy		
			-- Next check on accuracy. Use Tank_accuracy if /tank = true
			if table.find(gcinclude.tWeaponSkills['RANGED_AGI'],lname) ~= nil or
				table.find(gcinclude.tWeaponSkills['RANGED_STRAGI'],lname) ~= nil then
				gcinclude.ProgressiveAccuracy('RAcc');
			else
				gcinclude.ProgressiveAccuracy('Acc');
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
	end
	
	-- Certain weapon skills can take advantage of magic attack bonus. 
	-- Check here and equip gear appropriately
	if string.find('red lotus blade,sanguine blade',lName) ~= nil then
		gcinclude.MoveToCurrent(gProfile.Sets.MAB,gProfile.Sets.CurrentGear);	
	end		
end		-- gcinclude.fHandleWeaponskill

return gcinclude;