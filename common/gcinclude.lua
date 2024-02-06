local gcinclude = T{};

require 'common'

--[[
	This file contains routines that are used with Luashitacast across any supported job.
	Job specific routines are found in the "Username"_job file (ex: Paiine_BST.lua)

	These sets are universal for things like debuff conditions (doomed, asleep, etc); avoid 
	main/sub/range/ammo slot gear here unless it makes sense and you (potentially) don't mind
	losing your tp.
-]]
gcinclude.sets = {
	['Doomed'] = { 					-- this set will equip any time you have the doom status
    },
	['Doomed_Conditional'] = {
	},
	
	['Holy_Water'] = { 				-- update with whatever gear you use for the Holy Water item
    },
	['Holy_Water_Conditional'] = {
	},
	
	['Sleeping'] = { 				-- this set will auto equip if you are asleep
		Neck = 'Opo-opo necklace',	-- might as well gain tp
    },
	['Sleeping_Conditional'] = {
	},
	
	['Blind'] = {					-- this will autoequip if you're blind
		Ear2 = 'Bat Earring',		-- gain some evasion
	},
	['Blind_Conditional'] = {
	},
	
	['Weakened'] = {  				-- this set will try to auto equip if you are weakened
	},
	['Weakened_Conditional'] = {
	},
	
	['Paralyzed'] = {				-- this set will equip if you are paralyzed
	},
	['Paralyzed_Conditional'] = {
	},
	
	['Shining_Ruby'] = {			-- this will auto-equip when you have the shining ruby buff
		--Hands = 'Carbuncle\'s Cuffs',
	},
	['Shining_Ruby_Conditional'] = {
	},
	
--[[
	Unfortunately the Town conditional set has to be in gcinclude. I can't seem to get it recognized 
	if it's in the profile area of the job file.
--]]

	['Town_Conditional'] = {
		{'Federation Aketon','Movement gain in home nation city','Body',1,'ALL','AKETON','Windy'},
	},

--[[
	There are currently eight crafts: alchemy, bonecraft, clothcraft, cooking, goldsmithing, leathercraft,
	smithing, and woodworking. It's possible that a player will have gear for more than one craft. The 
	"crafting" set is for any crafting gear that's used regardless of the type of crafting. All other
	gear is specified in the Crafting_Conditional set. (At some point synergy will be added. I don't know
	if there's any synergy specific gear, but if there is, it will be added to the conditional gear set.)
	
	Please note that the crafting rings will not be automatically loaded since they inhibit HQ results.
--]]
	['Crafting'] = {
	},
	['Crafting_Conditional'] = {	-- Conditionally load gear based on type of crafting
	},

--[[
	There are six gathering types: harvesting, excavtion, logging, and mining which are grouped in the H.E.L.M.
	set. The other two types of gathering, digging and clamming, have their own gear. The "gathering" set is 
	for gear that is not specific to any one type of gathering skill. All the rest should be placed in the 
	Gathering_Conditional set.
--]]

	['Gathering'] = {				-- This is for if there's any generalized gathering gear
	},
	['Gathering_Conditional'] = {	-- Conditionally load gear based on type of gathering
		{'Field Tunica','Improves mining, logging and harvesting','Body',1,'ALL','GATHER','HELM'},
		{'Field Gloves','Improves mining and logging','Hands',1,'ALL','GATHER','HELM'},
		{'Field Hose','Improves logging and harvesting','Legs',1,'ALL','GATHER','HELM'},
		{'Field Boots','Improves mining and harvesting','Feet',1,'ALL','GATHER','HELM'},
		{'Choc. Jack Coat','Chocobo riding time +5 minutes','Body',1,'ALL','GATHER','DIG'},
		{'Tarutaru Top +1','Reduces clamming incidents for female tarutarus','Body',1,'ALL','GATHER','CLAM'},
		{'Taru. Shorts +1','Imnproves clamming results for female tarutarus','Legs',1,'ALL','GATHER','CLAM'},
	},

--[[
	Fishing is it's own type of gathering and is separate from the other gathering definitions. When the Fishing
	gear is equipped, GSwap will be turned off. (A default lure is included.) When done fishing, the player should 
	turn GSwap back on.
--]]
	['FishingGear'] = {
        Range = 'Lu Shang\'s F. Rod',
        Ammo = 'Shrimp Lure',
        Body = 'Angler\'s Tunica',
        Hands = 'Fsh. Gloves',
        Legs = 'Fisherman\'s Hose',
        Feet = 'Waders',
    },
	['FishingGear_Conditional'] = {
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
	to see how this is done but as an example you can just put 'gcinclude.settings.RefreshGearMPP = 50;' in your 
	job files OnLoad function to modify for that job only
--]]
	Messages = false; 	 -- set to true if you want chat log messages to appear on any /gc command used such as DT, or KITE gear toggles, certain messages will always appear
	WScheck = true; 	 -- set to false if you dont want to use the WSdistance safety check
	WSdistance = 4.7; 	 -- default max distance (yalms) to allow non-ranged WS to go off at if the above WScheck is true
	RegenGearHPP = 60; 	 -- idle regen set gets loaded if player's max HP is <= 60%
	RefreshGearMPP = 70; -- idle refresh set gets loaded if player's max MP <= 70%. Refresh takes priority over regen
	bMagic = false;		 -- does job combination support magic.
	sMJ = nil;			 -- What is the main job
	sSJ = nil;			 -- What is the sub job
	bMJ = false;		 -- does the main job use magic
	bSJ = false;		 -- does the sub job use magic
	b50 = false;		 -- does the player have more than 50 MP capacity
	bEleStaves = false;	 -- does the player have any elemental staves. 
	bEleObis = false;	 -- does the player have any elemental obis. 
	bEleGorgets = false; -- does the player have any elemental gorgets.
	bSummoner = false;	 -- is the player a summoner. /smn doesn't count
	bStave = false;		 -- indicates if the auto-detection of elemental staves has successfully occurred
	bObiGorget = false;	 -- indicates if the auto-detection of elemental obis/gorgets has successfully occurred
	bAketon = false;	 -- indicates if the auto-detection of aketons has successfully occurred
	bMagicCheck = false; -- indicates if the check on magic support has occurred
	--
	sCapped = false;	-- indicates if in a capped area/bcnm/etc
	bCapped = false;	-- indicates if the capped process has occurred
	iCapped = 0;		-- level determined by capped process
	priorityEngaged = 'BCEFGH'; 	-- indicates order of steps for engagement
	priorityMidCast = 'ABCDEFGH';	-- indicates order of steps for spell midcast
	priorityWeaponSkill = 'ABDE';	-- indicates order of steps for a weapon skill
};

-- The following arrays are used by the functions contained in this file. Probably best to leave them alone

gcdisplay = gFunc.LoadFile('common\\gcdisplay.lua');

gcinclude.AliasList = T{'gswap','gcmessages','wsdistance','dt','kite','acc','eva','gearset','th','help','wswap','petfood','maxspell','maxsong','region','ajug','sbp','showit','equipit','tank','solo','test'};
gcinclude.Towns = T{'Tavnazian Safehold','Al Zahbi','Aht Urhgan Whitegate','Nashmau','Southern San d\'Oria [S]','Bastok Markets [S]','Windurst Waters [S]','San d\'Oria-Jeuno Airship','Bastok-Jeuno Airship','Windurst-Jeuno Airship','Kazham-Jeuno Airship','Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille','Bastok Mines','Bastok Markets','Port Bastok','Metalworks','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower','Ru\'Lude Gardens','Upper Jeuno','Lower Jeuno','Port Jeuno','Rabao','Selbina','Mhaura','Kazham','Norg','Mog Garden','Celennia Memorial Library','Western Adoulin','Eastern Adoulin'};
gcinclude.Windy = T {'Windurst Waters [S]','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower'};
gcinclude.Sandy = T {'Southern San d\'Oria [S]','Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille'};
gcinclude.Bastok = T {'Bastok Markets [S]','Bastok Mines','Bastok Markets','Port Bastok','Metalworks'};
gcinclude.DistanceWS = T{'Flaming Arrow','Piercing Arrow','Dulling Arrow','Sidewinder','Blast Arrow','Arching Arrow','Empyreal Arrow','Refulgent Arrow','Apex Arrow','Namas Arrow','Jishnu\'s Randiance','Hot Shot','Split Shot','Sniper Shot','Slug Shot','Blast Shot','Heavy Shot','Detonator','Numbing Shot','Last Stand','Coronach','Wildfire','Trueflight','Leaden Salute','Myrkr','Dagan','Moonlight','Starlight','Mistral Axe'};
gcinclude.BstPetAttack = T{'Foot Kick','Whirl Claws','Big Scissors','Tail Blow','Blockhead','Sensilla Blades','Tegmina Buffet','Lamb Chop','Sheep Charge','Pentapeck','Recoil Dive','Frogkick','Queasyshroom','Numbshroom','Shakeshroom','Nimble Snap','Cyclotail','Somersault','Tickling Tendrils','Sweeping Gouge','Grapple','Double Claw','Spinning Top','Suction','Tortoise Stomp','Power Attack','Rhino Attack','Razor Fang','Claw Cyclone','Crossthrash','Scythe Tail','Ripper Fang','Chomp Rush','Pecking Flurry','Sickle Slash','Mandibular Bite','Wing Slap','Beak Lunge','Head Butt','Wild Oats','Needle Shot','Disembowel','Extirpating Salvo','Mega Scissors','Back Heel','Hoof Volley','Fluid Toss','Fluid Spread'};
gcinclude.BstPetMagicAttack = T{'Gloom Spray','Fireball','Acid Spray','Molting Plumage','Cursed Sphere','Nectarous Deluge','Charged Whisker','Nepenthic Plunge'};
gcinclude.BstPetMagicAccuracy = T{'Toxic Spit','Acid Spray','Leaf Dagger','Venom Spray','Venom','Dark Spore','Sandblast','Dust Cloud','Stink Bomb','Slug Family','Intimidate','Gloeosuccus','Spider Web','Filamented Hold','Choke Breath','Blaster','Snow Cloud','Roar','Palsy Pollen','Spore','Brain Crush','Choke Breath','Silence Gas','Chaotic Eye','Sheep Song','Soporific','Predatory Glare','Sudden Lunge','Numbing Noise','Jettatura','Bubble Shower','Spoil','Scream','Noisome Powder','Acid Mist','Rhinowrecker','Swooping Frenzy','Venom Shower','Corrosive Ooze','Spiral Spin','Infrasonics','Hi-Freq Field','Purulent Ooze','Foul Waters','Sandpit','Infected Leech','Pestilent Plume'};
gcinclude.SmnSkill = T{'Shining Ruby','Glittering Ruby','Crimson Howl','Inferno Howl','Frost Armor','Crystal Blessing','Aerial Armor','Hastega II','Fleet Wind','Hastega','Earthen Ward','Earthen Armor','Rolling Thunder','Lightning Armor','Soothing Current','Ecliptic Growl','Heavenward Howl','Ecliptic Howl','Noctoshield','Dream Shroud','Altana\'s Favor','Reraise','Reraise II','Reraise III','Raise','Raise II','Raise III','Wind\'s Blessing'};
gcinclude.SmnMagical = T{'Searing Light','Meteorite','Holy Mist','Inferno','Fire II','Fire IV','Meteor Strike','Conflag Strike','Diamond Dust','Blizzard II','Blizzard IV','Heavenly Strike','Aerial Blast','Aero II','Aero IV','Wind Blade','Earthen Fury','Stone II','Stone IV','Geocrush','Judgement Bolt','Thunder II','Thunder IV','Thunderstorm','Thunderspark','Tidal Wave','Water II','Water IV','Grand Fall','Howling Moon','Lunar Bay','Ruinous Omen','Somnolence','Nether Blast','Night Terror','Level ? Holy'};
gcinclude.SmnAccuracy = T{'Healing Ruby','Healing Ruby II','Whispering Wind','Spring Water','Diamond Storm','Sleepga','Shock Squall','Slowga','Tidal Roar','Pavor Nocturnus','Ultimate Terror','Nightmare','Mewing Lullaby','Eerie Eye'};
gcinclude.SmnHybrid = T{'Flaming Crush','Burning Strike'};
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
gcinclude.NinNukes = T{'Katon: Ichi', 'Katon: Ni', 'Katon: San', 'Hyoton: Ichi', 'Hyoton: Ni', 'Hyoton: San', 'Huton: Ichi', 'Huton: Ni', 'Huton: San', 'Doton: Ichi', 'Doton: Ni', 'Doton: San', 'Raiton: Ichi', 'Raiton: Ni', 'Raiton: San', 'Suiton: Ichi', 'Suiton: Ni', 'Suiton: San'};
gcinclude.Rolls = T{{'Fighter\'s Roll',5,9}, {'Monk\'s Roll',3,7}, {'Healer\'s Roll',3,7}, {'Corsair\'s Roll',5,9}, {'Ninja Roll',4,8},{'Hunter\'s Roll',4,8}, {'Chaos Roll',4,8}, {'Magus\'s Roll',2,6}, {'Drachen Roll',4,8}, {'Choral Roll',2,6},{'Beast Roll',4,8}, {'Samurai Roll',2,6}, {'Evoker\'s Roll',5,9}, {'Rogue\'s Roll',5,9}, {'Warlock\'s Roll',4,8},
	{'Puppet Roll',3,7}, {'Gallant\'s Roll',3,7}, {'Wizard\'s Roll',5,9}, {'Dancer\'s Roll',3,7}, {'Scholar\'s Roll',2,6},{'Naturalist\'s Roll',3,7}, {'Runeist\'s Roll',4,8}, {'Bolter\'s Roll',3,9}, {'Caster\'s Roll',2,7}, {'Courser\'s Roll',3,9},{'Blitzer\'s Roll',4,9}, {'Tactician\'s Roll',5,8}, {'Allies\' Roll',3,10}, {'Miser\'s Roll',5,7},
	{'Companion\'s Roll',2,10},{'Avenger\'s Roll',4,8},}; -- {name,lucky,unlucky}
gcinclude.Crafting_Types = 'AL,BN,CL,CO,GS,LT,SM,WW';
gcinclude.Gathering_Types = 'HELM,DIG,CLAM';
--[[
	The following define all the weaponskills according to the desired stats
--]]
gcinclude.WS_AGI = 'Hot Shot,Split Shot,Sniper Shot,Slugshot,Blast Shot,Heavy Shot,Detonator,Geirskogul';
gcinclude.WS_CHR = 'Shadowstitch';
gcinclude.WS_DEX = 'Wasp Sting,Viper Bite,Eviseration,Onslaught,Blade: Metsu';
gcinclude.WS_DEXAGI = 'Shark Bite,Coronach';
gcinclude.WS_DEXCHR = 'Dancing Edge';
gcinclude.WS_DEXINT = 'Gust Slash,Cyclone';
gcinclude.WS_INT = 'Gate of Tartarus';
gcinclude.WS_INTMND = 'Spirit Taker';
gcinclude.WS_MND = 'Energy Steal,Energy Drain';
gcinclude.WS_STR = 'Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,Mistral Axe,Decimation,Spinning Attack,Flat Blade,Circle Blade,Vorpal Blade,Hard Slash,Crescent Moon,Mercy Stroke,Iron Tempest,Sturmwind,Keen Edge,Raging Rush,Metatron Torment,Leg Sweep,Skewer,Wheeling Thrust,Impulse Drive,Tachi: Enpi,Tachi: Hobaku,Tachi: Goten,Tachi: Kagero,Tachi: Jinpu,Tachi: Yukikaze,Tachi: Gekko,Tachi: Kasha,Tachi:Kaiten,Brainshaker,Skullbreaker,True Strike,Heavy Swing,Shell Crusher,Full Swing';
gcinclude.WS_STRAGI = 'Sickle Moon,Vorpal Thrust,Flaming Arrow,Piercing Arrow,Dulling Arrow,Sidewinder,Blast Arrow,Arching Arrow,Empyreal Arrow,Namas Arrow';
gcinclude.WS_STRDEX = 'Combo,Backhand Blow,Raging Fists,Fast Blade,Knights of Round,Double Thrust,Penta Thrust,Blade: Rin,Blade: Retsu,Blade: Jin,Blade: Ten,Blade: Ku';
gcinclude.WS_STRINT = 'Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell,Burning Blade,Frostbite,Freezebite,Spinning Slash,Ground Strike,Thunder Thrust,Raiden Thrust,Blade: Teki,Blade: To,Blade: Chi,Blade: Ei,Rock Crusher,Earth Crusher,Catastrophe';
gcinclude.WS_STRINT_30_20 = 'Red Lotus Blade';
gcinclude.WS_STRMND = 'Guillotine,Cross Reaper,Shining Blade,Seraph Blade,Swift Blade,Savage Blade,Shockwave,Tachi: Koki,Shining Strike,Seraph Strike,Judgment,Hexastrike,Randgrith,Starburst,Sunburst,Retribution';
gcinclude.WS_STRMND_30_50 = 'Black Halo';
gcinclude.WS_STRVIT = 'Calamity,Slice,Spinning Scythe,Vorpal Scythe,Howling Fist,Dragon Kick,Asuran Fists,Power Slash,Scourge,Shield Break,Armor Break,Weapon Break,Full Break,Steel Cyclone';
gcinclude.WS_VIT = 'Shoulder Tackle,One Inch Punch,Final Heaven';
gcinclude.WS_Skill = 'Starlight,Moonlight';
gcinclude.WS_HP = 'Spirits Within';

--[[
	Define all weapon skills that are elemental in nature
--]]

gcinclude.eleWS = T{
	['fire'] = 'Arching Arrow,Ascetic\'s Fury,Asuran Fists,Atonement,Blade: Shun,Decimation,Detonator,Drakesbane,Dulling Arrow,Empyreal Arrow,Final Heaven,Flaming Arrow,Full Swing,Garland of Bliss,Heavy Shot,Hexa Strike,Hot Shot,Insurgency,Knights of Round,Last Stand,Mandalic Stab,Mistral Axe,Metatron Torment,Realmrazer,Red Lotus Blade,Scourge,Shijin Spiral,Sniper Shot,Spinning Attack,Spinning Axe,Stringing Pummel,Tachi: Kagero,Tachi: Kasha,Upheaval,Wheeling Thrust',
	['earth'] = 'Aeolian Edge,Asuran Fists,Avalanche Axe,Blade: Ei,Blade: Ku,Blade: Ten,Calamity,Catastrophe,Crescent Moon,Dancing Edge,Entropy,Eviseration,Exenterator,Expiacion,Fast Blade,Hard Slash,Impulse Drive,Iron Tempest,King\'s Justice,Leaden Salute,Mercy Stroke,Nightmare Scythe,Omniscience,Primal Rend,Pyrrhic Kleos,Rampage,Requiscat,Resolution,Retibution,Savage Blade,Seraph Blade,Shattersoul,Shining Blade,Sickle Moon,Slice,Spinning Axe,Spinning Scythe,Spiral Hell,Stardiver,Stringing Pummel,Sturmwind,Swift Blade,Tachi: Enpi,Tachi: Jinpu,Tachi: Rana,Trueflight,Viper Bite,Vorpal Blade,Wasp Sting',
	['water'] = 'Atonement,Blade: Teki,Brainshaker,Circle Blade,Cross Reaper,Dark Harvest,Entropy,Quietus,Death Blossom,Decimation,Expiacion,Full Break,Garland of Bliss,Gate of Tartarus,Geirskogul,Ground Strike,Last Stand,Mordant Rime,Namas Arrow,Piercing Arrow,Pyrrhic Kleos,Rudra\'s Storm,Primal Rend,Raging Rush,Retribution,Ruinator,Shadow of Death,Shockwave,Shoulder Tackle,Sidewinder,Skullbreaker,Slug Shot,Smash Axe,Spinning Scythe,Spiral Hell,Split Shot,Steel Cyclone,Sturmwind,Sunburst,Tachi: Gekko,Tachi: Koki,Vidohunir,Vorpal Thrust',
	['wind'] = 'Aeolian Edge,Backhand Blow,Black Halo,Blade: Jin,Blade: Kamu,Blade: To,Camlann\'s Torment,Coronach,Cyclone,Dancing Edge,Death Blossom,Dragon Kick,Earth Crusher,Exenterator,Freezebite,Gake Axe,Ground Strike,Gust Slash,King\'s Justice,Mordant Rime,Raging Axe,Randgrith,Red Lotus Blade,Resolution,Ruinator,Savage Blade,Shark Bite,Shell Crusher,Sidewinder,Slug Shot,Spinning Slash,Steel Cyclone,Tachi: Jinpu,Tachi: Kaiten,Taichi: Shoha,Taichi:Yukikaze,Tornado Kick,Trueflight,True Strike,Victory Smite,Vidohunir',
	['ice'] = 'Blade: To,Blast Arrow,Cross Reaper,Death Blossom,Expiacion,Freezebite,Frostbite,Full Break,Gate of Tartarus,Geirskogul,Ground Strike,Guillotine,Quietus,Impulse Drive,Mordant Rime,Namas Arrow,Piercing Arrow,Pyrrhic Kleos,Rudra\'s Storm,Ruinator,Raging Rush,Shadow of Death,Shattersoul,Skullbreaker,Smash Axe,Spiral Hell,Steel Cyclone,Tachi: Gekko,Tachi: Hobaku,Tachi: Rana,Tachi: Yukikaze,Tornado Kick,Vidohunir',
	['thunder'] = 'Aeolian Edge,Apex Arrow,Armor Break,Avalanche Axe,Black Halo,Blade: Chi,Blade: Jin,Blade: Kamu,Blade: Shun,Calamity,Camlann\'s Torment,Circle Blade,Combo,Cyclone,Death Blossom,Dragon Kick,Earth Crusher,Exenterator,Flat Blade,Full Swing,Ground Strike,Heavy Swing,Howling Fist,Judgement,King\'s Justice,Leg Sweep,Mordant Rime,Raging Axe,Raging Fist,Raiden Thrust,Realmrazer,Resolution,Rock Crusher,Savage Blade,Seraph Strike,Shark Bite,Shield Break,Shining Strike,Shoulder Tackle,Sickle Moon,Skewer,Spinning Attack,Spinning Axe,Tachi: Goten,Tachi: Koki,Tachi: Shoha,Thunder Thrust,True Strike,Victory Smite,Vidohunir,Vorpal Blade,Weapon Break',
	['light'] = 'Apex Arrow,Arching Arrow,Ascetic\'s Fury,Atonement,Blade: Chi,Blade: Ku,Blade: Rin,Blade: Shun,Blast Arrow,Blast Shot,Camlann\'s Torment,Decimation,Detonator,Double Thrust,Drakesbane,Dulling Arrow,Empyreal Arrow,Eviseration,Final Heaven,Flaming Arrow,Garland of Bliss,Heavy Shot,Hexa Strike,Hot Shot,Howling Fist,Insurgency,Knight\'s of Round,Leaden Salute,Last Stand,Mandalic Stab,Metatron Torment,Mistral Axe,Omniscience,Piercing Arrow,Power Slash,Realmrazer,Raiden Thrust,Scourge,Shijin Spiral,Sidewinder,Skewer,Slug Shot,Sniper Shot,Split Shot,Stardiver,Tachi: Enpi,Tachi: Goten,Tachi: Kasha,Thunder Thrust,Torcleaver,Victory Smite,Upheaval,Vorpal Scythe,Vorpal Thrust,Wheeling Thrust',
	['dark'] = 'Asuran Fists,Black Halo,Blade: Ei,Blade: Hi, Blade: Kamu,Blade: Ku, Blade: Ten,Catastrophe,Quietus,Entropy,Eviseration,Impulse Drive,Insurgency,Keen Edge,Leaden Salute,Mandalic Stab,Mercy Stroke,Requiscat,Rundra\'s Storm,Nightmare Scythe,Omniscience,One Inch Punch,Penta Thrust,Primal Rend,Retribution,Shattersoul,Starburst,Stardiver,Stringing Pummel,Sunburst,Swift Blade,Tachi: Kasha,Tachi: Rana,Tachi: Shoha,Upheaval',
	};
	
-- Daily element and their Elemental weaknesses
gcinclude.WeekDayElement = T{
	['Firesday'] = {'fire','water'},
	['Earthsday'] = {'earth','wind'},
	['Watersday'] = {'water','thunder'},
	['Windsday'] = {'wind','ice'},
	['Iceday'] = {'ice','fire'},
	['Lightningday'] = {'thunder','earth'},
	['Lightsday'] = {'light','dark'},
	['Darksday'] = {'dark','light'}
};

-- define constants for DT so typos aren't made
gcinclude.OFF = 'Off';
gcinclude.PHY = 'Physical';
gcinclude.MAG = 'Magical';
gcinclude.BRE = 'Breath';

-- Define constants dealing with magic gear and jobs
gcinclude.ELEMENT = 'ele';
gcinclude.OBI = 'obi';
gcinclude.aketon = {['Sandy'] = {'Kingdom Aketon',false}, ['Windy'] = {'Federation Aketon',false}, 
					['Bastok'] = {'Republic Aketon',false},['Omni'] = {'Ducal Aketon',false}};
gcinclude.sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,SCH,GEO,RUN';

-- Below indicates all the staves you own. (The settings are programmatically determined.)
gcinclude.elemental_staves = T{['fire'] = {'Fire staff',false,'Vulcan\'s staff',false},
							   ['ice'] = {'Ice staff',false,'Aquilo\'s staff',false},
							   ['wind'] = {'Wind staff',false,'Auster\'s staff',false},
							   ['earth'] = {'Earth staff',false,'Terra\'s staff',false},
							   ['thunder'] = {'Thunder staff',false,'Jupiter\'s staff',false},
							   ['water'] = {'Water staff',false,'Neptune\'s staff',false},
							   ['light'] = {'Light staff',false,'Apollo\'s staff',false},
							   ['dark'] = {'Dark staff',false,'Pluto\'s staff',false}};
							  
-- Below indicate all the elemental obis you own or elemental gorgets. (This is determined programmatically.)
gcinclude.elemental_obis = T{['fire'] = {'Karin obi',false},
							 ['earth'] = {'Dorin obi',false},
							 ['water'] = {'Suirin obi',false},
							 ['wind'] = {'Furin obi',false},
						     ['ice'] = {'Hyorin obi',false},
							 ['thunder'] = {'Rairin obi',false},
							 ['light'] = {'Korin obi',false},
							 ['dark'] = {'Anrin obi',false}};
							 
gcinclude.elemental_gorgets = T{['fire'] = {'Flame gorget',false},
								['earth'] = {'Soil gorget',false},
								['water'] = {'Aqua gorget',false},
								['wind'] = {'Breeze gorget',false},
							    ['ice'] = {'Snow gorget',false},
								['thunder'] = {'Thunder gorget',false},
								['light'] = {'Light gorget',false},
								['dark'] = {'Shadow gorget',false}}

--[[
	The "root" of spells is the first word in the spell name, all in lower case. 
	
	The following lists all "root" spells whose Magical Accuracy can be affected by day/weather
--]]

gcinclude.MagicEleAcc = T{
	['fire'] = 'burn,firaga,fire,flare,blaze',
    ['water'] = 'drown,flood,water,waterga,poison',
    ['wind'] = 'choke,aero,aeroga,tornado,silence,gravity,flurry',
    ['thunder'] = 'shock,burst,thundaga,thunder,Stun',
    ['earth'] = 'rasp,quake,stone,stonega,slow',
    ['ice'] = 'frost,blizzaga,blizzard,freeze,paralyze,bind,distract,ice',
    ['light'] = 'banish,banishga,dia,diaga,flash,repose,holy,auspice,esuna,sacrifice,reprisal,cure,curaga,cura',
    ['dark'] = 'blind,bio,sleep,dispel,frazzle,drain,warp,tractor,aspir,escape,sleepga,retrace,absorb-mnd,absorb-chr,absorb-vit,absorb-agi,absorb-int,absorb-dex,absorb-str'
};
	
-- The following lists all "root" spells that are elemental in nature and can affect Elemental Damage 
-- by the day/weather
	
gcinclude.MagicEleDmg = T{
	['fire'] = 'firaga,fire,flare',
	['water'] = 'flood,water,waterga',
	['wind'] = 'aero,aeroga,tornado',
	['thunder'] = 'burst,thundaga,thunder',
	['earth'] = 'quake,stone,stonega',
	['ice'] = 'blizzaga,blizzard,freeze',
	['light'] = 'banish,banishga,dia,diaga,holy,auspice,esuna,sacrifice,phalanx,refresh,reprisal,cure,curaga,cura',
	['dark'] = 'blind,bio,poison,sleep,dispel,frazzle,drain,aspir,escape,sleepga,retrace,absorb-mnd,absorb-chr,absorb-vit,absorb-agi,absorb-int,absorb-dex','absorb-str'
};

-- Listed below are all the spells that are affected by INT or MND
gcinclude.StatMagic = T{
	['int'] = {'INT','aero,aeroga,bind,blaze,blind,blizzaga,blizzard,burst,dread,firaga,fire,flare,flood,freeze,ice,quake,shock,stone,stonega,thundaga,thunder,tornado,water,waterga'},
	['mnd'] = {'MND','banish,distract,frazzle,paralyze,slow,cure,curaga,cura'},
};

-- This breaks out all spells based on type of magic skill. Please note that only the "root" of the spell name is listed
gcinclude.MagicSkill = T{
	['Healing'] = 'cure,curaga,cura,blindna,cursna,esuna,paralyna,poisona,raise,reraise,sacrifice,silena,stona,viruna',
	['Dark'] = 'bio,drain,aspir,absorb-agi,absorb-chr,absorb-dex,absorb-int,absorb-mnd,absorb-str,absorb-vit,absorb-tp,absorb-acc,tractor,stun,dread',
	['Divine'] = 'banish,holy,flash,repose,enlight',
	['Enfeebling'] = 'bind,blind,dia,diaga,distract,frazzle,gravity,paralyze,poison,poisonga,sleep,sleepga,silence,slow',
	['Enhancing'] = 'aquaveil,auspice,baraera,baraero,barblind,barblindra,barblizzard,barblizzara,barfira,barfire,barparalyze,barparalyzra,barpetra,barpetrify,barpoison,barpoisonra,barsilence,barsilenera,barsleep,barsleepra,barstone,barstonra,barthunder,barthundra,barvira,barvirus,barwater,barwatera,blaze,blink,deoderize,enaero,enblizzard,enfire,enstone,enthunder,enwater,erase,escape,flurry,haste,ice,invisible,phalanx,protect,protectra,refresh,regen,reprisal,retrace,shell,shellra,shock,sneak,stoneskin,teleport-altep,teleport-dem,teleport-holla,teleport-mea,teleport-vahzl,teleport-yhoat,warp',
	['Elemental'] = 'aero,aeroga,blizzaga,blizzard,burn,burst,drown,fira,firaga,fire,flare,flood,freeze,frost,quake,rasp,shock,stone,stonega,thundaga,thunder,tornado,water,waterga',
	['Ninjitsu'] = 'tonko:,utsusemi:,katon:,hyoton:,huton:,doton:,raiton:,suiton:,kurayami:,hojo:,monomi:,dokumori:,jubaku:',
	['Summoning'] = 'carbuncle,fenrir,ifrit,titan,leviathan,garuda,shiva,ramuh,diabolos,fire,firespirit,ice,icespirit,air,airspirit,earth,earthspirit,thunder,thunderspirit,water,waterspirit,light,lightspirit,dark,darkspirit,cait,caitsith,siren,atomos,alexander,odin',
};

-- This table associates a summoned avatar with an element so that the appropriate stave can be equipped
gcinclude.SummonStaves = T{
	['carbuncle'] = 'light', ['light spirit'] = 'light', ['lightspirit'] = 'light', ['cait sith'] = 'light', ['caitsith'] = 'light', ['alexander'] = 'light',
	['fenrir'] = 'dark', ['diabolos'] = 'dark', ['darks pirit'] = 'dark', ['darkspirit'] = 'dark', ['atomos'] = 'dark', ['odin'] = 'dark',
	['ifrit'] = 'fire', ['fire spirit'] = 'fire', ['firespirit'] = 'fire',
	['titan'] = 'earth', ['earth spirit'] = 'earth', ['earthspirit'] = 'earth',
	['leviathan'] = 'water', ['water spirit'] = 'water', ['waterspirit'] = 'water',
	['garuda'] = 'wind', ['air spirit'] = 'wind', ['airspirit'] = 'wind', ['siren'] = 'wind',
	['shiva'] = 'ice', ['ice spirit'] = 'ice', ['icespirit'] = 'ice',
	['ramuh'] = 'thunder', ['thunder spirit'] = 'thunder', ['thunderspirit'] = 'thunder'
};

--[[
	This table contains a list of all of the spells that have multiple versions where
	the intensity is the only change. Included is what job can cast the spell and at 
	what level.
	
	Columns: Spell name, spell id, root, tier, MP cost, WHM, RDM, PLD, SCH, BLM, DRK, BRD, GEO, RUN, NIN
	
--]]

gcinclude.TieredIndices = T {['SN'] = 1, ['ID'] = 2, ['RT'] = 3, ['TI'] = 4, ['MP'] = 5, ['WHM'] = 6, ['RDM'] = 7, 
							 ['PLD'] = 8, ['SCH'] = 9, ['BLM'] = 10, ['DRK'] = 11, ['BRD'] = 12, ['GEO'] = 13, 
							 ['RUN'] = 14 , ['NIN'] = 15};
gcinclude.TieredSongIndices = T {['SN'] = 1, ['ID'] = 2, ['RT'] = 3, ['TI'] = 4, ['BUF'] = 5};						 
gcinclude.TieredMagicJobs = 'WHM,RDM,PLD,SCH,BLM,DRK,BRD,GEO,RUN,NIN';

gcinclude.TieredMagic = T {
	{'Cure',1,'cure',1,8,1,3,5,5,nil,nil,nil,nil,nil,nil},
	{'Cure II',2,'cure',2,24,11,14,17,17,nil,nil,nil,nil,nil,nil},
	{'Cure III',3,'cure',3,46,21,26,30,30,nil,nil,nil,nil,nil,nil},
	{'Cure IV',4,'cure',4,88,41,48,55,55,nil,nil,nil,nil,nil,nil},
	{'Cure V',5,'cure',5,135,61,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Curaga',7,'curaga',1,60,16,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Curaga II',8,'curaga',2,120,31,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Curaga III',9,'curaga',3,180,51,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Curaga IV',10,'curaga',4,260,71,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Raise',12,'raise',1,150,25,35,50,35,nil,nil,nil,nil,nil,nil},
	{'Raise II',13,'raise',2,150,56,nil,nil,70,nil,nil,nil,nil,nil,nil},
	{'Raise III',140,'raise',3,150,70,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Dia',23,'dia',1,7,3,1,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Dia II',24,'dia',2,30,36,31,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Dia III',25,'dia',3,45,nil,75,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Banish',28,'banish',1,15,5,nil,7,nil,nil,nil,nil,nil,nil,nil},
	{'Banish II',29,'banish',2,57,30,nil,34,nil,nil,nil,nil,nil,nil,nil},
	{'Banish III',30,'banish',3,96,65,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Banishga',38,'banishga',1,41,15,nil,30,nil,nil,nil,nil,nil,nil,nil},
	{'Banishga II',39,'banishga',2,120,40,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Protect',43,'protect',1,9,7,7,10,10,nil,nil,nil,nil,20,nil},
	{'Protect II',44,'protect',2,28,27,27,30,30,nil,nil,nil,nil,40,nil},
	{'Protect III',45,'protect',3,46,47,47,50,50,nil,nil,nil,nil,60,nil},
	{'Protect IV',46,'protect',4,65,63,63,70,66,nil,nil,nil,nil,nil,nil},
	{'Shell',48,'shell',1,18,17,17,20,20,nil,nil,nil,nil,10,nil},
	{'Shell II',49,'shell',2,37,37,37,40,40,nil,nil,nil,nil,30,nil},
	{'Shell III',50,'shell',3,56,57,57,60,60,nil,nil,nil,nil,50,nil},
	{'Shell IV',51,'shell',4,75,68,68,nil,71,nil,nil,nil,nil,70,nil},
	{'Slow',56,'slow',1,12,13,13,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Slow II',79,'slow',2,45,nil,75,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Paralyze',58,'paralyze',1,6,4,6,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Paralyze II',80,'paralyze',2,36,nil,75,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Phalanx',106,'phalanx',1,21,nil,33,nil,nil,nil,nil,nil,nil,68,nil},
	{'Phalanx II',107,'phalanx',2,42,nil,75,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Regen',108,'regen',1,15,21,21,nil,18,nil,nil,nil,nil,23,nil},
	{'Regen II',110,'regen',2,36,44,nil,nil,37,nil,nil,nil,nil,48,nil},
	{'Regen III',111,'regen',3,64,66,nil,nil,59,nil,nil,nil,nil,70,nil},
	{'Protectra',125,'protectra',1,9,7,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Protectra II',126,'protectra',2,28,27,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Protectra III',127,'protectra',3,46,47,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Protectra IV',128,'protectra',4,65,63,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Protectra V',129,'protectra',5,84,75,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Shellra',130,'shellra',1,18,17,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Shellra II',131,'shellra',2,37,37,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Shellra III',132,'shellra',3,56,57,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Shellra IV',133,'shellra',4,75,68,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Shellra V',134,'shellra',5,93,75,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Reraise',135,'reraise',1,150,25,nil,35,nil,nil,nil,nil,nil,nil,nil},
	{'Reraise II',141,'reraise',2,150,56,nil,70,nil,nil,nil,nil,nil,nil,nil},
	{'Reraise III',142,'reraise',3,150,70,nil,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Fire',144,'fire',1,7,nil,19,nil,16,13,23,nil,19,nil,nil},
	{'Fire II',145,'fire',2,26,nil,50,nil,42,38,60,nil,46,nil,nil},
	{'Fire III',146,'fire',3,63,nil,71,nil,64,62,nil,nil,67,nil,nil},
	{'Fire IV',147,'fire',4,135,nil,nil,nil,73,73,nil,nil,nil,nil,nil},
	{'Blizzard',149,'blizzard',1,8,nil,24,nil,20,17,29,nil,24,nil,nil},
	{'Blizzard II',150,'blizzard',2,31,nil,55,nil,46,42,66,nil,50,nil,nil},
	{'Blizzard III',151,'blizzard',3,75,nil,73,nil,66,64,nil,nil,70,nil,nil},
	{'Blizzard IV',152,'blizzard',4,162,nil,nil,nil,74,74,nil,nil,nil,nil,nil},
	{'Aero',154,'aero',1,6,nil,14,nil,12,9,17,nil,14,nil,nil},
	{'Aero II',155,'aero',2,22,nil,45,nil,38,34,54,nil,42,nil,nil},
	{'Aero III',156,'aero',3,54,nil,69,nil,60,59,nil,nil,64,nil,nil},
	{'Aero IV',157,'aero',4,115,nil,nil,nil,72,72,nil,nil,nil,nil,nil},
	{'Stone',159,'stone',1,4,nil,4,nil,4,1,5,nil,4,nil,nil},
	{'Stone II',160,'stone',2,16,nil,35,nil,30,26,42,nil,34,nil,nil},
	{'Stone III',161,'stone',3,40,nil,65,nil,54,51,nil,nil,58,nil,nil},
	{'Stone IV',162,'stone',4,88,nil,nil,nil,70,68,nil,nil,nil,nil,nil},
	{'Thunder',164,'thunder',1,9,nil,29,nil,24,21,35,nil,29,nil,nil},
	{'Thunder II',165,'thunder',2,37,nil,60,nil,51,46,72,nil,54,nil,nil},
	{'Thunder III',166,'thunder',3,91,nil,75,nil,69,66,nil,nil,73,nil,nil},
	{'Thunder IV',167,'thunder',4,194,nil,nil,nil,69,66,nil,nil,nil,nil,nil},
	{'Water',169,'water',1,5,nil,9,nil,8,5,11,nil,8,nil,nil},
	{'Water II',170,'water',2,19,nil,40,nil,34,30,48,nil,38,nil,nil},
	{'Water III',171,'water',3,46,nil,67,nil,57,55,nil,nil,61,nil,nil},
	{'Water IV',172,'water',4,99,nil,nil,nil,71,70,nil,nil,nil,nil,nil},
	{'Firaga',174,'firaga',1,57,nil,nil,nil,nil,28,nil,nil,nil,nil,nil},
	{'Firaga II',175,'firaga',2,153,nil,nil,nil,nil,53,nil,nil,nil,nil,nil},
	{'Firaga III',174,'firaga',3,263,nil,nil,nil,nil,69,nil,nil,nil,nil,nil},
	{'Blizzaga',179,'blizzaga',1,80,nil,nil,nil,nil,32,nil,nil,nil,nil,nil},
	{'Blizzaga II',180,'blizzaga',2,175,nil,nil,nil,nil,57,nil,nil,nil,nil,nil},
	{'Blizzaga III',181,'blizzaga',3,297,nil,nil,nil,nil,71,nil,nil,nil,nil,nil},
	{'Aeroga',184,'aeroga',1,45,nil,nil,nil,nil,23,nil,nil,nil,nil,nil},
	{'Aeroga II',185,'aeroga',2,131,nil,nil,nil,nil,48,nil,nil,nil,nil,nil},
	{'Aeroga III',186,'aeroga',3,232,nil,nil,nil,nil,67,nil,nil,nil,nil,nil},
	{'Stonega',189,'stonega',1,24,nil,nil,nil,nil,15,nil,nil,nil,nil,nil},
	{'Stonega II',190,'stonega',2,93,nil,nil,nil,nil,40,nil,nil,nil,nil,nil},
	{'Stonega III',191,'stonega',3,175,nil,nil,nil,nil,63,nil,nil,nil,nil,nil},
	{'Thundaga',194,'thundaga',1,105,nil,nil,nil,nil,36,nil,nil,nil,nil,nil},
	{'Thundaga II',195,'thundaga',2,200,nil,nil,nil,nil,61,nil,nil,nil,nil,nil},
	{'Thundaga III',196,'thundaga',3,332,nil,nil,nil,nil,73,nil,nil,nil,nil,nil},
	{'Watera',199,'watera',1,34,nil,nil,nil,nil,19,nil,nil,nil,nil,nil},
	{'Watera II',200,'thundaga',2,112,nil,nil,nil,nil,44,nil,nil,nil,nil,nil},
	{'Watera III',201,'watera',3,202,nil,nil,nil,nil,65,nil,nil,nil,nil,nil},
	{'Flare',204,'flare',1,315,nil,nil,nil,nil,60,nil,nil,nil,nil,nil},
	{'Flare II',205,'flare',2,280,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Freeze',206,'freeze',1,315,nil,nil,nil,nil,50,nil,nil,nil,nil,nil},
	{'Freeze II',207,'freeze',2,280,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Tornado',208,'tornado',1,315,nil,nil,nil,nil,52,nil,nil,nil,nil,nil},
	{'Tornado II',209,'tornado',2,280,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Quake',210,'quake',1,315,nil,nil,nil,nil,54,nil,nil,nil,nil,nil},
	{'Quake II',211,'quake',2,280,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Burst',212,'burst',1,315,nil,nil,nil,nil,56,nil,nil,nil,nil,nil},
	{'Burst II',213,'burst',2,280,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Flood',214,'flood',1,315,nil,nil,nil,nil,58,nil,nil,nil,nil,nil},
	{'Flood II',215,'flood',2,280,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Poison',220,'poison',1,5,nil,5,nil,nil,3,6,nil,nil,nil,nil},
	{'Poison II',221,'poison',2,38,nil,46,nil,nil,43,46,nil,nil,nil,nil},
	{'Poisonga',225,'poisonga',1,44,nil,nil,nil,nil,24,26,nil,nil,nil,nil},
	{'Poisonga II',226,'poisonga',2,112,nil,nil,nil,nil,64,66,nil,nil,nil,nil},
	{'Bio',230,'bio',1,15,nil,10,nil,nil,10,15,nil,nil,nil,nil},
	{'Bio II',231,'bio',2,36,nil,36,nil,nil,35,40,nil,nil,nil,nil},
	{'Bio III',232,'bio',3,54,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Drain',245,'drain',1,21,nil,nil,nil,21,12,10,nil,nil,nil,nil},
	{'Drain II',246,'drain',2,37,nil,nil,nil,nil,nil,62,nil,nil,nil,nil},
	{'Sleep',253,'sleep',1,19,nil,25,nil,30,20,30,nil,35,nil,nil},
	{'Sleep II',259,'sleep',2,29,nil,46,nil,65,41,56,nil,70,nil,nil},
	{'Sleepga',273,'sleepga',1,38,nil,nil,nil,nil,31,nil,nil,nil,nil,nil},
	{'Sleepga II',274,'sleepga',2,58,nil,nil,nil,nil,56,nil,nil,nil,nil,nil},
	{'Blind',254,'blind',1,5,nil,8,nil,nil,4,nil,nil,nil,nil,nil},
	{'Blind II',276,'blind',2,31,nil,nil,nil,nil,75,nil,nil,nil,nil,nil},
	{'Enfire',100,'enfire',1,12,nil,24,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enfire II',312,'enfire',2,24,nil,58,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enblizzard',101,'enblizzard',1,12,nil,22,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enblizzard II',313,'enblizzard',2,24,nil,56,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enaero',102,'enaero',1,12,nil,20,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enaero II',314,'enaero',2,24,nil,52,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enstone',103,'enstone',1,12,nil,18,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enstone II',315,'enstone',2,24,nil,52,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enthunder',104,'enthunder',1,12,nil,16,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enthunder II',316,'enthunder',2,24,nil,50,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enwater',105,'enwater',1,12,nil,12,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Enwater II',317,'enwater',2,24,nil,60,nil,nil,nil,nil,nil,nil,nil,nil},
	{'Katon: Ichi',320,'katon',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,15},
	{'Katon: Ni',321,'katon',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,40},
	{'Hyoton: Ichi',323,'hyoton',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,15},
	{'Hyoton: Ni',324,'hyoton',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,40},
	{'Huton: Ichi',326,'huton',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,15},
	{'Huton: Ni',327,'huton',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,40},
	{'Doton: Ichi',329,'doton',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,15},
	{'Doton: Ni',330,'doton',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,40},
	{'Raiton: Ichi',332,'raiton',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,15},
	{'Raiton: Ni',333,'raiton',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,40},
	{'Suiton: Ichi',335,'suiton',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,15},
	{'Suiton: Ni',336,'suiton',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,40},
	{'Utsusemi: Ichi',338,'utsusemi',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,12},
	{'Utsusemi: Ni',339,'utsusemi',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,37},
	{'Hojo: Ichi',344,'hojo',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,23},
	{'Hojo: Ni',345,'hojo',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,48},
	{'Tonko: Ichi',353,'tonko',1,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,9},
	{'Tonko: Ni',354,'tonko',2,0,nil,nil,nil,nil,nil,nil,nil,nil,nil,34},
};

--[[

	Like TieredMagic TieredSongs lists all of the songs that a bard can cast that 
	has multiple tiers. It's split out to handle different parameters.
	
--]]
gcinclude.TieredSongIndices = T {['SN'] = 1, ['ID'] = 2, ['RT'] = 3, ['TI'] = 4, ['LVL'] = 5, ['BUF'] = 6};	
gcinclude.TieredSongs = T{
	{'Foe Requiem',368,'foe',1,1,'requiem'},
	{'Foe Requiem II',369,'foe',2,17,'requiem'},
	{'Foe Requiem III',370,'foe',3,37,'requiem'},
	{'Foe Requiem IV',371,'foe',4,47,'requiem'},
	{'Foe Requiem V',372,'foe',5,57,'requiem'},
	{'Foe Requiem VI',373,'foe',6,67,'requiem'},
	{'Army\'s Paeon',378,'armys',1,5,'paeon'},
	{'Army\'s Paeon II',379,'armys',2,15,'paeon'},
	{'Army\'s Paeon III',380,'armys',3,35,'paeon'},
	{'Army\'s Paeon IV',381,'armys',4,45,'paeon'},
	{'Army\'s Paeon V',382,'armys',5,65,'paeon'},
	{'Mage\'s Ballad',386,'mages',1,25,'ballad'},
	{'Mage\'s Ballad II',387,'mages',2,55,'ballad'},
	{'Knight\'s Minne',389,'knights',1,1,'minne'},
	{'Knight\'s Minne II',390,'knights',2,21,'minne'},
	{'Knight\'s Minne III',391,'knights',3,41,'minne'},
	{'Knight\'s Minne IV',392,'knights',4,61,'minne'},
	{'Valor Minuet',394,'valor',1,3,'minuet'},
	{'Valor Minuet II',395,'valor',2,23,'minuet'},
	{'Valor Minuet III',396,'valor',3,43,'minuet'},
	{'Valor Minuet IV',397,'valor',4,63,'minuet'},
	{'Sword Madrigal',399,'sword',1,11,'madrigal'},
	{'Blade Madrigal',400,'blade',2,51,'madrigal'},
	{'Sheepfoe Mambo',403,'sheepfoe',1,13,'mambo'},
	{'Dragonfoe Mambo',404,'dragonfoe',2,53,'mambo'},
	{'Battlefield Elegy',421,'battlefield',1,39,'elegy'},
	{'Carnage Elegy',422,'carnage',2,59,'elegy'},
	{'Advancing March',419,'advancing',1,29,'march'},
	{'Victory March',420,'victory',2,60,'march'}
};

-- Temporary holding variables for the MH and OH weapons
gcinclude.weapon = nil;
gcinclude.offhand = nil;

-- Table of all BST pet food including the minimum level needed to equip. 
-- The last column is programmatically populated, so don't change it.
gcinclude.petfood = {
	['alpha'] = {'Alpha','Pet Food Alpha',12,false,nil},
	['beta'] = {'Beta','Pet Food Beta',24,false,nil},
	['gamma'] = {'Gamma','Pet Fd. Gamma',36,false,nil},
	['delta'] = {'Delta','Pet Food Delta',48,false,nil},
	['epsilon'] = {'Epsilon','Pet Fd. Epsilon',60,false,nil},
	['zeta'] = {'Zeta','Pet Food Zeta',72,false,nil}
};

-- This is a list of all player storage containers available in FFXI.
-- Quite a number of them are not valid on HorizonXI yet.

gcinclude.STORAGES = {
    [1] = {0, 'Inventory' },
    [2] = {1, 'Safe' },
    [3] = {2, 'Storage' },
    [4] = {3, 'Temporary' },
    [5] = {4, 'Locker' },
    [6] = {5, 'Satchel' },
    [7] = {6, 'Sack' },
    [8] = {7, 'Case' },
    [9] = {8, 'Wardrobe' },
    [10]= {9, 'Safe 2' },
    [11]= {10, 'Wardrobe 2' },
    [12]= {11, 'Wardrobe 3' },
    [13]= {12, 'Wardrobe 4' },
    [14]= {13, 'Wardrobe 5' },
    [15]= {14, 'Wardrobe 6' },
    [16]= {15, 'Wardrobe 7' },
    [17]= {16, 'Wardrobe 8' }
};

-- List of items that are commonly equipped for teleporting, exp boosts, reraise, etc
gcinclude.equipIt = {
	['emp'] = {'Empress Band','Ring'},
	['cha'] = {'Chariot Band','Ring'},
	['empo'] = {'Emperor Band','Ring'},
	['ann'] = {'Anniversary Ring','Ring'},
	['dem'] = {'Dem Ring','Ring'},
	['mea'] = {'Mea Ring','Ring'},
	['holla'] = {'Holla Ring','Ring'},
	['altep'] = {'Altepa Ring','Ring'},
	['yhoat'] = {'Yhoat Ring','Ring'},
	['vahzl'] = {'Vahzl Ring','Ring'},
	['home'] = {'Homing Ring','Ring'},
	['ret'] = {'Return Ring','Ring'},
	['warp'] = {'Warp Ring','Ring'},
	['tav'] = {'Tavnazian Ring','Ring'},
	['dcl'] = {'Dcl.Grd. Ring','Ring'},
	['warpc'] = {'Warp Cudgel','Main'},
	['trick2'] = {'Trick Staff II','Main'},
	['treat2'] = {'Treat Staff II','Main'},
	['fork1'] = {'Pitchfork +1','Main'},
	['purgo'] = {'Wonder Top +1','Body'},
	['rre'] = {'Reraise Earring','Ear'},
};

-- This is the list of storage containers that can be equipped from outside of a moghouse
gcinclude.EQUIPABLE = {gcinclude.STORAGES[1],gcinclude.STORAGES[9],gcinclude.STORAGES[11],gcinclude.STORAGES[16]};

-- Gear set for holding conditional gear that's equipable along with associated levels
gcinclude.tGS = {Main=nil,Sub=nil,Range=nil,Ammo=nil,Head=nil,Neck=nil,Ear1=nil,Ear2=nil,Body=nil,Hands=nil,
				 Ring1=nil,Ring2=nil,Back=nil,Waist=nil,Legs=nil,Feet=nil};	-- Empty gearset for conditional gear
gcinclude.tGSL = {Main=0,Sub=0,Range=0,Ammo=0,Head=0,Neck=0,Ear1=0,Ear2=0,Body=0,Hands=0,
				  Ring1=0,Ring2=0,Back=0,Waist=0,Legs=0,Feet=0};			-- Empty gearset levels, for comparison

--[[
	DB_ShowIt will display debug details about the type passed.
--]]

function gcinclude.DB_ShowIt(sType)
	print(chat.message(' '));
	if sType == nil or string.lower(sType) == 'staff' then
		print(chat.message('Staff'));
		print(chat.message('-----'));
		print(chat.message('bStave = ' .. tostring(gcinclude.settings.bStave)));
		print(chat.message('bEleStaves = ' .. tostring(gcinclude.settings.bEleStaves)));
		print(chat.message(' '));
		for k,v in pairs(gcinclude.elemental_staves) do
			print(chat.message('['..k..'] ' .. v[1] .. ' = ' .. tostring(v[2]) .. ', ' .. v[3] .. ' = ' .. tostring(v[4])));	
		end
	elseif string.lower(sType) == 'obi' then
		print(chat.message('Obis and Gorgets'));
		print(chat.message('----------------'));
		print(chat.message('bObiGorget = ' .. tostring(gcinclude.settings.bObiGorget)));
		print(chat.message('bEleObis = ' .. tostring(gcinclude.settings.bEleObis)));
		print(chat.message('bEleGorgets = ' .. tostring(gcinclude.settings.bEleGorgets)));
		print(chat.message(' '));
		for k,v in pairs(gcinclude.elemental_obis) do
			print(chat.message('['..k..'] ' .. v[1] .. ' = ' .. tostring(v[2]) ));	
		end
		print(chat.message(' '));
		for k,v in pairs(gcinclude.elemental_gorgets) do
			print(chat.message('['..k..'] ' .. v[1] .. ' = ' .. tostring(v[2]) ));	
		end
	elseif string.lower(sType) == 'aketon' then
		print(chat.message('Aketon'));
		print(chat.message('------'));
		for k,v in pairs(gcinclude.aketon) do
			print(chat.message('[' .. k .. ']: ' .. v[1] .. ' = ' .. tostring(v[2])));
		end
	elseif string.lower(sType) == 'settings' then
		print(chat.message('Settings'));
		print(chat.message('--------'));
		print(chat.message('WScheck: ' .. tostring(gcinclude.settings.WScheck)));
		print(chat.message('WSdistance: ' .. tostring(gcinclude.settings.WSdistance)));
		print(chat.message('RegenGearHPP: ' .. tostring(gcinclude.settings.RegenGearHPP)));
		print(chat.message('RefreshGearMPP: ' .. tostring(gcinclude.settings.RefreshGearMPP)));
		print(chat.message('bMagic: ' .. tostring(gcinclude.settings.bMagic)));
		print(chat.message('sMJ: :' .. tostring(gcinclude.settings.sMJ)));
		print(chat.message('sSJ: :' .. tostring(gcinclude.settings.sSJ)));		
		print(chat.message('bMJ: ' .. tostring(gcinclude.settings.bMJ)));
		print(chat.message('bSJ: ' .. tostring(gcinclude.settings.bSJ)));
		print(chat.message('b50: ' .. tostring(gcinclude.settings.b50)));
		print(chat.message('bEleStaves: ' .. tostring(gcinclude.settings.bEleStaves)));
		print(chat.message('bEleObis: ' .. tostring(gcinclude.settings.bEleObis)));
		print(chat.message('bEleGorgets: ' .. tostring(gcinclude.settings.bEleGorgets)));
		print(chat.message('bSummoner: ' .. tostring(gcinclude.settings.bSummoner)));
		print(chat.message('bStave: ' .. tostring(gcinclude.settings.bStave)));
		print(chat.message('bObiGorget: ' .. tostring(gcinclude.settings.bObiGorget)));
		print(chat.message('bAketon: ' .. tostring(gcinclude.settings.bAketon)));
		print(chat.message('bMagicCheck: ' .. tostring(gcinclude.settings.bMagicCheck)));
	end
end		-- gcinclude.DB_ShowIt
	
--[[
	Message toggles on/off a feedback mechanism for all luashitacast commands
--]]
	
function gcinclude.Message(toggle, status)
	if toggle ~= nil and status ~= nil then
		print(chat.header('GCinclude'):append(chat.message(toggle .. ' is now ' .. tostring(status))))
	end
end		-- gcinclude.Message

--[[
	SetAlias registers all of the luashitacast commands that are defined in this file
--]]

function gcinclude.SetAlias()
	for _, v in ipairs(gcinclude.AliasList) do
		AshitaCore:GetChatManager():QueueCommand(-1, '/alias /' .. v .. ' /lac fwd ' .. v);
	end
end		-- gcinclude.SetAlias

--[[
	ClearAlias removes the luashitacast commands that were registered in this file
--]]

function gcinclude.ClearAlias()
	for _, v in ipairs(gcinclude.AliasList) do
		AshitaCore:GetChatManager():QueueCommand(-1, '/alias del /' .. v);
	end
end		-- gcinclude.ClearAlias

--[[
	CheckForAllNationalAketons determines if the player owns any of the national aketons
--]]

function gcinclude.CheckForAllNationalAketons()
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local tStorage = gcinclude.EQUIPABLE;
	local iTmp = 0;
	
	-- First, set all the entries to false, do not assume that what's there is correct
	for k,_ in pairs(gcinclude.aketon) do
		gcinclude.aketon[k][2] = false;
	end
	
	iCnt = 0;
	for _ in pairs(tStorage) do iCnt = iCnt + 1 end
	
	-- now, loop through the passed storage containers
	for i = 1,iCnt,1 do
		containerID = tStorage[i][1];

		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
				local sIN = string.lower(item.Name[1])
				iTmp = iTmp + 1;
				b,c = string.find(sIN,'aketon');
				if b ~= nil then
					for k,_ in pairs(gcinclude.aketon) do
						if sIN == string.lower(gcinclude.aketon[k][1]) then
							gcinclude.aketon[k][2] = true;
							break;
						end
					end
				end
			end
		end
	end

	-- Below indicates that the inventories really were check and it's not a loading issue
	gcinclude.settings.bAketon = (iTmp > 10);
end		-- gcinclude.CheckForAllNationalAketons

--[[
	CheckForStaves determines if the player has any elemental staves and updates the master listing
	accordingly.
--]]

function gcinclude.CheckForStaves()
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local tStorage = gcinclude.EQUIPABLE;
	local iTmp = 0;
	
	-- First, set all the entries to false, do not assume that what's there is correct
	for k,_ in pairs(gcinclude.elemental_staves) do
		gcinclude.elemental_staves[k][2] = false;
		gcinclude.elemental_staves[k][4] = false;
	end
	
	iCnt = 0;
	for _ in pairs(tStorage) do iCnt = iCnt + 1 end

	-- now, loop through the passed storage containers
	for i = 1,iCnt,1 do
		containerID = tStorage[i][1];

		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
				local sIN = string.lower(item.Name[1])
				iTmp = iTmp + 1;
				b,c = string.find(sIN,'staff');
				if b ~= nil then
					for k,_ in pairs(gcinclude.elemental_staves) do
						if sIN == string.lower(gcinclude.elemental_staves[k][1]) then
							gcinclude.elemental_staves[k][2] = true;
							gcinclude.settings.bEleStaves = true
						elseif sIN == string.lower(gcinclude.elemental_staves[k][3]) then
							gcinclude.elemental_staves[k][4] = true;
							gcinclude.settings.bEleStaves = true
						end
					end
				end
			end
		end
	end
	-- Below indicates that the inventories really were check and it's not a loading issue
	gcinclude.settings.bStave = (iTmp > 10);
end		-- gcinclude.CheckForStaves

--[[
	CheckForObisGorgets determines if the player has any elemental obis/gorgets and updates the master listing
	accordingly.
--]]

function gcinclude.CheckForObisGorgets()
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local tStorage = gcinclude.EQUIPABLE;
	local iTmp = 0;
	
	-- First, set all the entries to false. Both obis and gorgets have same number of elements
	for k,_ in pairs(gcinclude.elemental_obis) do
		gcinclude.elemental_obis[k][2] = false;
		gcinclude.elemental_gorgets[k][2] = false;
	end
	
	iCnt = 0;
	for _ in pairs(tStorage) do iCnt = iCnt + 1 end
	
	-- now, loop through the passed storage containers
	for i = 1,iCnt,1 do
		containerID = tStorage[i][1];

		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
				local sIN = string.lower(item.Name[1]);
				iTmp = iTmp + 1;
						
				-- Start with the obis
				for k,_ in pairs(gcinclude.elemental_obis) do
					local sOIN = string.lower(gcinclude.elemental_obis[k][1]);
					if sIN == sOIN then
						gcinclude.elemental_obis[k][2] = true;
						gcinclude.settings.bEleObis = true;
					end
				end
				
				-- Then check the gorgets
				for k,_ in pairs(gcinclude.elemental_gorgets) do
					local sOIN = string.lower(gcinclude.elemental_gorgets[k][1]);
					if sIN == sOIN then
						gcinclude.elemental_gorgets[k][2] = true;
						gcinclude.settings.bEleGorgets = true;
					end
				end
			end
		end
	end
	
	gcinclude.settings.bObiGorget = (iTmp > 10);
end		-- gcinclude.CheckForObisGorgets

--[[
	SetVariables defines run settings for luashitacast
--]]

function gcinclude.SetVariables()
	local player = gData.GetPlayer();

	-- General toggles
	gcdisplay.CreateToggle('GSwap', true);
	gcdisplay.CreateToggle('Kite', false);
	gcdisplay.CreateToggle('Acc', false);
	gcdisplay.CreateToggle('Eva', false);
	gcdisplay.CreateToggle('WSwap',false);
	gcdisplay.CreateToggle('Tank',false);
	gcdisplay.CreateToggle('Solo',false);
	
	-- Job specific toggles
	if player.MainJob == 'THF' then
		gcdisplay.CreateToggle('TH',false);
	end	
	if player.MainJob == 'BST' then
		gcdisplay.CreateToggle('AJug',true);
	end
	if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
		gcdisplay.CreateToggle('sBP',true);
	end
	
	-- General cycles
	gcdisplay.CreateCycle('DT', {[1] = gcinclude.OFF, [2] = gcinclude.PHY, [3] = gcinclude.MAG, [4] = gcinclude.BRE});
	gcdisplay.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned'});
end		-- gcinclude.SetVariables

--[[
	isPetNamed determines if that passed pet has the passed name
--]]

function gcinclude.isPetNamed(sName,pet)

	if pet == nil then
		return false;
	end
	
	if sName ~= nil then
		local sPetName = string.lower(pet.Name);
		local sMatch = string.lower(sName);
		
		return (string.find(sMatch,sPetName) ~= nil);
	else
		print(chat.header('isPetNamed'):append(chat.message('Error: Passed name is nil')));
		return false;
	end
end

--[[
	BuildGear populates the holding gear set according to the passed table entries
--]]

function gcinclude.BuildGear(tMasCond)
	local pos = 0;
	local slot;

	-- There's a special case for RING and EARRING. We just need to find the
	-- first ones (ring1,earring1) and we can proceed from there.
	if tMasCond[2] == 'RING' then
		slot = 'Ring1';
	elseif tMasCond[2] == 'EARRING' then
		slot = 'Ear1';	
	else
		slot = tMasCond[2];
	end

	-- First, determine which slot is being addressed
	for i,j in pairs(gData.Constants.EquipSlotNames) do
		if string.lower(j) == string.lower(slot) then
			pos = i;
			break;
		end
	end

	if pos <1 then
		pos = 1;
	elseif pos > 16 then
		pos = 16;
	end
	
--[[	
	There's a bit more futzing to do if what was indicated was a 'RING' or 'EARRING', since there
	are two slots for the equipment. The logic for which slot to use goes as follows:
	
	- if the first slot is empty, use that. if occuppied, then check the second one. if empty use that
	- since both slots used, compare level of item
	- if the level of the item being checked is less than the level of the item there then check next position
	- if the level of the item being checked is more than the level of the item there then use that slot
	- if the level of the item being checked is less than the level of the item in the second slot, disregard item
--]]

	if tMasCond[2] == 'RING' or tMasCond[2] == 'EARRING' then
		if gcinclude.tGSL[pos] > 0 then				-- A conditional already processed for spot
			if gcinclude.tGSL[pos+1] > 0 then		-- Two already processed for slot. Trickier...
				if tMasCond[3] < gcinclude.tGSL[pos] then
					if tMasCond[3] > gcinclude.tGSL[pos+1] then
						pos = pos + 1;
					end
				else
					print(chat.header('ProcessConditional'):append(chat.message('Warning: '..tEntry[2] ..' ignored. Too many items conditionally specified for this slot.')));
					return false;
				end
			else						-- Empty second slot. Use that one
				pos = pos +1;
			end
		end
	end
	
	-- Now process normally. Check to see that the level of the item being populated is higher than what is there
	if tMasCond[3] > gcinclude.tGSL[pos] then
		-- Copy the name of the gear piece to the appropriate slot in the temporary gear set
		gcinclude.tGS[pos] = tMasCond[1];
		-- Copy the level of the gear piece to the associaed slot in the temporary gear set level list
		gcinclude.tGSL[pos] = tMasCond[3];
		return true;
	end
end		-- gcinclude.BuildGear

--[[
	CheckPartyJob determines if the party has a member of the passed job.
--]]

function gcinclude.CheckPartyJob(jobs)
	local pParty = AshitaCore:GetMemoryManager():GetParty();
	local bFound = false;
	 
	jobs = string.upper(jobs);
	 
	for i=1,6,1 do
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
end

--[[
	CheckTime determines if the current server time is found in the passed name time range.
	The following named time ranges are valid:
	
		Nighttime is 17:00 to 6:00, Daytime is 6:00 to 18:00, DUSK2DAWN: 17:00 to 7:00,
		Dawn: 6:00 to 7:00, Day: 7:00 to 17:00, Dusk: 17:00 to 18:00, Evening: 18:00 to 20:00, 
		DEADOFNIGHT: 20:00 to 4:00.
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
			print(chat.header('ProcessConditional'):append(chat.message('Error: Unknown named time: '.. t)));
		end
		bGood = false;
	end
	return bGood;
end		-- gcinclude.CheckTime
	
--[[
	ProcessConditional determines if any of the specified conditional equipment should be loaded. 
	tTable is the conditional gear.
	
	*** Note ***
	This procedure has been changed to treat all conditionals like they're User-Defined.
--]]

function gcinclude.ProcessConditional(tTest,sType,tMaster)
	local player = gData.GetPlayer();
	local pMJ = player.MainJob;
	local pLevel = player.MainJobSync;
	local timestamp = gData.GetTimestamp();
	local environ = gData.GetEnvironment();
	local zone = gData.GetEnvironment();
	local pet = gData.GetPet();
	local sKey;
	local tMatched = {};
				  
	-- clear out the holding table so no interference from a previous call
	gcinclude.tGS = {nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil};
	gcinclude.tGSL = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	
	-- Now process all the conditional gear
	local iPiece = 0;
	if tTest == nil then
		return
	end
	
	for k,v in ipairs(tTest) do
		tMatched[1] = v[1];						-- Gear piece name
		tMatched[2] = v[3];						-- Slot
		tMatched[3] = v[4];						-- Level
		tMatched[4] = string.upper(v[5]);		-- Job list
		tMatched[5] = string.upper(v[6]);		-- Conditional code
		-- What is filled in the next 3 statements depends on the conditional code
		tMatched[6] = v[7];
		tMatched[7] = v[8];
		tMatched[8] = v[9];

		-- Make sure current job can use the gear	
		if (string.find(tMatched[4],pMJ) ~= nil or tMatched[4] == 'ALL') then
			-- Check that the gear minimum level isn't too high			
			if tMatched[3] <= pLevel then
				bMatch = false;	-- Indicator to track if there's a match
				-- Now determine the type of condition and process
				if tMatched[5] == 'CRAFT' then
					sKey = string.upper(sType);
					if sKey ~= nil and tMatched[6] == sKey then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'GATHER' then	
					sKey = string.upper(sType);
					if sKey ~= nil and tMatched[6] == sKey then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'MOON' then
					if lower(environ.MoonPhase) == lower(tMatched[6]) then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'DAY' then
					if string.find(lower(v[6]),lower(environ.Day)) ~= nil then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'TIME' then
					if (gcinclude.CheckTime(timestamp.hour,tMatched[6],true)) then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'NATION' then
					bKey = (gcdisplay.GetCycle('Region') == 'Owned');
					if (bKey and tMatched[6]) or (bKey == false and tMatched[6] == false) then
						bMatch = gcinclude.BuildGear(tMatched);
					end					
				elseif tMatched[5] == 'AKETON' then
					if gcinclude.settings.bAketon == false then		-- Make sure all nation aketon's are tracked.
						gcinclude.CheckForAllNationalAketons();
					end
					
					-- Checks for the nation of the aketon, whether the zone is in that nation, and if the player is 
					-- from that nation (Sandy = 0, Bastok = 1, and Windy = 2). (player was not used to check nationality
					-- since luashitacast does not carry the home nation setting that is found in AshitaCore. Alse, I found 
					-- the nationality translations in campaign_nation.sql file that's part of AirSkyBoat Github source.)
					local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
					
					if (tMatched[6] == 'Windy' and zone.Area ~= nil and gcinclude.Windy:contains(zone.Area) and pNation == 2 
							and gcinclude.aketon['Windy'][2] == true) or
						(tMatched[6] == 'Sandy' and zone.Area ~= nil and gcinclude.Sandy:contains(zone.Area) and pNation == 0 
							and gcinclude.aketon['Sandy'][2] == true) or
						(tMatched[6] == 'Bastok' and zone.Area ~= nil and gcinclude.Bastok:contains(zone.Area) and pNation == 1 
							and gcinclude.aketon['Bastok'][2] == true) or
						(tMatched[6] == 'Omni') and zone.Area ~= nil and (gcinclude.Windy:contains(zone.Area) or gcinclude.Sandy:contains(zone.Area) or 
							gcinclude.Bastok:contains(zone.Area)) and gcinclude.aketon['Omni'][2] == true
					then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'WEATHER' then
					if string.find(lower(tMatched[6]),lower(environ.RawWeather)) ~= nil then 
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'MOON:DAY:NIGHT' then
					if tMatched[6] == environ.MoonPhase then
						if tMatched[7] == environ.Day then
							if gcinclude.CheckTime(timestamp.hour,tMatched[8],true) then
								bMatch = gcinclude.BuildGear(tMatched);
							end
						end
					end
				elseif tMatched[5] == 'DAY|TIME' then
					local bDayOfWeek = (string.find(v[6],environ.Day) ~= nil);
					local ts = timestamp.hour;
					local bNight = gcinclude.CheckTime(ts,'Nighttime',false);
					local bDay = gcinclude.CheckTime(ts,'Daytime',false);
				
					if bDayOfWeek or bNight or bDay then
						bMatch = gcinclude.BuildGear(tMatched);
					end				
				elseif tMatched[5] == 'MP<50' then						-- Equip if mp < 50 and total mp >= 50 and can do magic		
					if gcinclude.settings.bMagic and gcinclude.settings.b50 and player.MP < 50 then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'MP.LE.50P' then					-- Equip if MP <= 50%
					if player.MPP <= 50 then
						bMatch = gcinclude.BuildGear(tMatched);
					end			
				elseif tMatched[5] == 'SJ:MAGIC' then					-- Equip if subjob can do magic
					if gcinclude.settings.bSJ then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'HPP|TPP.LE.' then				-- HP% <= 'a' and TP <= 'b'
					if player.HPP <= tMatched[6] and player.TP <= tMatched[7] then
						bMatch = gcinclude.BuildGear(tMatched);
					end
				elseif tMatched[5] == 'PET_NAME' then
					if pet ~= nil then
						local s = string.lower(tMatched[6]);
						local sp = string.lower(pet.Name);
						if string.find(s,sp) ~= nil then
							bMatch = gcinclude.BuildGear(tMatched);
						end
					end	
				elseif tMatched[5] == 'NOT_PET_NAME' then
					if pet ~= nil then
						local s = string.lower(tMatched[6]);
						local sp = string.lower(pet.Name);
						if string.find(string.lower(s,sp)) == nil then
							bMatch = gcinclude.BuildGear(tMatched);
						end
					end
				elseif tMatched[5] == 'SJIS' then		-- subjob is
					local s = string.upper(tMatched[6]);
					if string.find(s,player.SubJob) ~= nil then
						bMatch = gcinclude.BuildGear(tMatched);
					end					
				elseif tMatched[5] == 'SJISN' then		-- subjob is not
					local s = string.upper(tMatched[6]);
					if string.find(s,player.SubJob) == nil then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end	
				else
					print(chat.header('ProcessConditional'):append(chat.message('Error: Unknown conditional: '.. tMatched[4])));
				end
				if bMatch then
					iPiece = iPiece + 1;	-- We have a piece since it processed fine
				end
			end
		end
	end
	-- If any piece was populated into the build table, equip the gear
	if iPiece > 0 then
		for i=1,16 do
			if gcinclude.tGSL[i] > 0 then
				tMaster[gData.Constants.EquipSlotNames[i]] = gcinclude.tGS[i];
			end
		end
	end
end		-- gcinclude.ProcessConditional

--[[
	ClearSet blanks out the passed gear set
--]]

function gcinclude.ClearSet(gSet)
	
	for k,v in pairs(gData.Constants.EquipSlots) do
		gSet[k] = '';
	end
end

--[[
	MoveToCurrent copies the gear defined in the passed set to current master
	set. Nothing is displayed, this is just a transfer routine.
--]]

function gcinclude.MoveToCurrent(tSet,tMaster)

	if tSet == nil then
		return;
	end

	for k,v in pairs(tSet) do
		tMaster[k] = v;
	end
end

--[[
	EquipTheGear makes sure that the passed gear set doesn't have an item in a slot
	that is being blocked by another item (e.g., no head gear if a vermillion cloak
	is in the body slot.) It the equips the gear set.
--]]

function gcinclude.EquipTheGear(tSet)

	if tSet['Body'] == 'Vermillion Cloak' then
		tSet['Head'] = '';
	end
	
	gFunc.ForceEquipSet(tSet);
end

--[[
	MaxSong determines what is the highest tier song that matches the passed root or buff name
	for a bard song that can be cast by the player and if indicated, it will cast it. Further,
	the invocation can indicate that one less tier should be cast. Only songs current in era are
	included. Songs not found in the associated lookup table do not have multiple tiers or are
	out of era. An appropriate message is displayed.	
--]]

function gcinclude.MaxSong(root,bBack,bCast)
	local player = gData.GetPlayer();
	local sMain = player.MainJob;
	local sSub = player.SubJob;
	local MainLvl = player.MainJobSync;
	local SubLvl = player.SubJobSync;
	local mp = player.MP;
	local iLvl;

	if bCast == nil then
		bCast = false;
	end

	if bBack == nil then
		bBack = false;
	end

	-- Make sure either the main job or sub job is a bard
	if not (sMain == 'BRD' or sSub == 'BRD') then
		print(chat.header('MaxSong'):append(chat.message('Current job is not a bard.')));
		return;
	else
		if sMain == 'BRD' then
			iLvl = MainLvl;
		else
			iLvl = SubLvl;
		end
	end
	
	bFound = false;
	iTier = T{0,0};		-- Tier of the found matching song and the previous
	sName = T{0,0};		-- Song name of the matching entry
	iSID = T{0,0};		-- Song ID
	
	root = gcinclude.GetRoot(string.lower(root));

	--[[
		Cycle through the table and find any matches. Then determine if castable and if it is a higher level
		than the one already found (if any). Save the previous and the current.
	--]]

	for i,v in pairs(gcinclude.TieredSongs) do
		if root == v[gcinclude.TieredIndices['RT']] or root == v[gcinclude.TieredIndices['BUF']] then
			bFound = true;
			-- See if matched entry is a higher level tier than what was found
			if v[gcinclude.TieredIndices['TI']] > iTier[1] then
				-- Make sure the spell isn't too high a level
				if v[gcinclude.TieredSongIndices['LVL']] <= iLvl  then
					if AshitaCore:GetMemoryManager():GetPlayer():HasSpell(v[gcinclude.TieredIndices['ID']]) then
						if iTier[1] > 0 then
							iTier[2] = iTier[1];
							sName[2] = sName[1];
							iSID[2] = iSID[1];
						end
						iTier[1] = v[gcinclude.TieredIndices['TI']];
						sName[1] = v[gcinclude.TieredIndices['SN']];
						iSID[1] = v[gcinclude.TieredIndices['ID']];
					end
				end
			end
		end
	end

	if not bFound then
		print(chat.header('MaxSong'):append(chat.message('Song root not found - ' .. root)));
	else
		if bBack then
			if iTier[2] > 0 then
				if bCast then
					print(chat.header('MaxSong'):append(chat.message('Casting ' .. sName[2] .. ' (max-1)')));
					sCmd = 'ma "' .. sName[2] .. '" <t>';
					AshitaCore:GetChatManager():QueueCommand(1, sCmd);
					return;
				else
					print(chat.header('MaxSong'):append(chat.message('Highest song of '.. root ..' that you can cast is ' .. sName[2] .. ' (max-1)')));
				end
			else
				print(chat.header('MaxSong'):append(chat.message('Only one song matched')));
			end
		else
			if bCast then
				print(chat.header('MaxSong'):append(chat.message('Casting ' .. sName[1])));
				sCmd = '/ma "' .. sName[1] .. '" <t>';
				AshitaCore:GetChatManager():QueueCommand(1, sCmd);
			else
				print(chat.header('MaxSong'):append(chat.message('Highest song of '.. root ..' that you can cast is ' .. sName[2])));
			end
		end
	end	
end		-- gcinclude.MaxSong
	
--[[
	MaxSpell determines what is the highest tier spell that matches the passed root that can
	be cast by the player and if indicated, will cast it. Please note that only spells that are
	currently in era and have multiple tiers will be checked. Spells not found in the lookup
	table or spells found but unable to be cast at the player's current level will not be cast 
	and an appropriate message will be displayed.
--]]

function gcinclude.MaxSpell(root,bCast)
	local player = gData.GetPlayer();
	local sMain = player.MainJob;
	local sSub = player.SubJob;
	local MainLvl = player.MainJobSync;
	local SubLvl = player.SubJobSync;
	local mp = player.MP;

	if bCast == nil then
		bCast = false;
	end
	
	-- Make sure either the main job or sub job can cast magic (according to our table)
	if string.find(gcinclude.TieredMagicJobs,sMain) == nil and string.find(gcinclude.TieredMagicJobs, sSub) == nil then
		print(chat.header('MaxSpell'):append(chat.message('Current job does not support macic.')));
		return;
	end
		
	bFound = false;
	bJob = false;
	bCanCast = false;
	iTier = 0;			-- Tier of the found matching spell
	sName = nil;		-- Spell name of the matching entry
	iSID = 0;			-- Spell ID
	
	root = gcinclude.GetRoot(string.lower(root));

	--[[
		Cycle through the table and find any matches. Then determine if castable and if it is a higher level
		than the one already found (if any).
	--]]

	for i,v in pairs(gcinclude.TieredMagic) do
		if root == v[gcinclude.TieredIndices['RT']] then
			bFound = true;
			-- See if matched entry is a higher level tier than what was found
			if v[gcinclude.TieredIndices['TI']] > iTier then
				-- Make sure the spell isn't too high a level
				if (v[gcinclude.TieredIndices[sMain]] ~= nil and v[gcinclude.TieredIndices[sMain]] <= MainLvl) or
					(v[gcinclude.TieredIndices[sSub]] ~= nil and v[gcinclude.TieredIndices[sSub]] <= SubLvl) then
					bCanCast = false;
					bJob = true;
					if v[gcinclude.TieredIndices[sMain]] ~= nil then
						if v[gcinclude.TieredIndices['MP']] <= mp then
							bCanCast = true;
						end
					end
					if not bCanCast and v[gcinclude.TieredIndices[sSub]] ~= nil then
						if v[gcinclude.TieredIndices['MP']] <= mp then
							bCanCast = true;
						end					
					end

					-- if it can be cast, save the particulars
					if bCanCast then
						if AshitaCore:GetMemoryManager():GetPlayer():HasSpell(v[gcinclude.TieredIndices['ID']]) then
							iTier = v[gcinclude.TieredIndices['TI']];
							sName = v[gcinclude.TieredIndices['SN']];
							iSID = v[gcinclude.TieredIndices['ID']];
						end
					end
				end
			end
		end
	end

	if not bFound then
		print(chat.header('MaxSpell'):append(chat.message('Spell root not found - ' .. root)));
	else
		if not bJob then
			print(chat.header('MaxSpell'):append(chat.message('You cannot cast that spell.')));
		elseif iTier == 0 then
			print(chat.header('MaxSpell'):append(chat.message('You have insufficient MP to cast that spell.')));
		else
			if bCast then
				print(chat.header('MaxSpell'):append(chat.message('Casting ' .. sName)));
				sCmd = '/ma "' .. sName .. '" <t>';
				AshitaCore:GetChatManager():QueueCommand(1, sCmd);
			else
				print(chat.header('MaxSpell'):append(chat.message('Highest tier of ' .. root .. ' is ' .. sName)));
			end
		end
	end	
end		-- gcinclude.MaxSpell

--[[
	CheckMagic50 determines if the player's mainjob and subjob uses MP and whether the maximum 
	MP the player has is greater than 50.
--]]

function gcinclude.CheckMagic50(player)

	-- First make sure player data is not in transition
	if gcinclude.settings.bMagicCheck == false and (player.MainJob == nil or player.SubJob == nil or player.MaxMP == nil) then
		return;
	end
	
	gcinclude.settings.sMJ = player.MainJob;
	gcinclude.settings.sSJ = player.SubJob;
	if (string.find(gcinclude.sMagicJobs,player.MainJob) ~= nil) then
		gcinclude.settings.bMJ = true;

	end
	if (string.find(gcinclude.sMagicJobs,player.SubJob) ~= nil) then
		gcinclude.settings.bSJ = true;
	end
	gcinclude.settings.bMagic = gcinclude.settings.bMJ or gcinclude.settings.bSJ;
	
	-- This is a special case, need to check that player has potentially more than 50 MP
	gcinclude.settings.b50 = (gcinclude.settings.bMagic and player.MaxMP > 50);
	gcinclude.settings.bMagicCheck = true;
	return;
end		-- gcinclude.CheckMagic50

--[[
	SwapToStave determines if swapping your weapon out for one of the elemental staves makes
	sense and does it for you while remembering what weapon/offhand you had equipped.
	
	Note: there's a timing issue when it comes to getting the two weapons currently equipped
	that has to do with zoning. I don't see any reason for this routine to run afoul of the
	problem, so I'm going to assume it won't occur here. If an error does arise though, what
	is needed to be done is to check if ew['Main'] == nil (the same is true for ew['Sub']).
--]]

function gcinclude.SwapToStave(sStave,noSave,cs)
	local ew = gData.GetEquipment();
	local eWeap = nil;
	local eOff = nil;

	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end
	
	if ew['Sub'] ~= nil then
		eOff = ew['Sub'].Name;
	end;

	if ((gcdisplay.GetToggle('WSwap') == true or gcinclude.settings.bSummoner) and 
		(gcinclude.elemental_staves[sStave][2] == true or gcinclude.elemental_staves[sStave][4] == true)) then

		-- See if a current weapon is the one of the targetted staves
		if not (eWeap == nil or
				string.lower(eWeap) == string.lower(gcinclude.elemental_staves[sStave][1]) or 
				string.lower(eWeap) == string.lower(gcinclude.elemental_staves[sStave][3])) then
			-- save the weapon so it can be equipped again
			if eWeap ~= gcinclude.weapon and noSave == false and gcinclude.settings.bSummoner == false then
				gcinclude.weapon = eWeap;
				gcinclude.offhand = eOff;
			end
		end

		-- Now, try the HQ and then the NQ
		if gcinclude.elemental_staves[sStave][4] == true then
			pos = 3;
		else
			pos = 1;
		end
		cs['Main'] = gcinclude.elemental_staves[sStave][pos];
	end
end		-- gcinclude.SwapToStave

--[[
	EquipItem processes the passed arguments and equips the specified item (whether by coded entry or name)
	into the appropriate equipment slot. Then turns /gswap off.
--]]

function gcinclude.EquipItem(args)
	local iName = nil;
	local iSlot = nil;
	local iPtr = nil;
		
	if #args > 1 then
		-- see if the item specified is a code	
		for k,v in pairs(gcinclude.equipIt) do
			if k == args[2] then
				iName = v[1];
				iSlot = v[2];
				iPtr = 2;
				break;
			end
		end

		-- if it wasn't a code, the item should be explicitly identified
		if iName == nil then
			iName = args[2];
			if #args > 2 then
				iSlot = args[3];
				iPtr = 3;
			else
				print(chat.header('EquipIt'):append(chat.message('Error: incomplete /equipit command: /equipit code|name slot|#. Command ignored.')));
				return;
			end
		end

		-- ring and ear need a slot appended to it. Either something specified or just assume "1"
		if string.find('ring,ear',string.lower(iSlot)) ~= nil then
			if iPtr ~= nil and #args > iPtr and string.find('1,2',args[iPtr+1]) ~= nil then
				iSlot = iSlot .. args[iPtr+1];
			else
				iSlot = iSlot .. '1';
			end
		end
		
		-- Make sure the slot is formatted right (assuming it's just a case issue)
		iSlot = string.upper(string.sub(iSlot,1,1)) .. string.lower(string.sub(iSlot,2));
		-- Now try and load the item
		gFunc.ForceEquip(iSlot,iName);
		gcdisplay.SetToggle('GSwap',false);
	else
		print(chat.header('EquipIt'):append(chat.message('Error: incomplete /equipit command: /equipit code|name slot|#. Command ignored.')));
	end
end		-- gcinclude.EquipItem

--[[
	HandleCommands processes any commands typed into luashitacast as defined in this file
--]]

function gcinclude.HandleCommands(args)

	if not gcinclude.AliasList:contains(args[1]) then return end

	local player = gData.GetPlayer();
	local toggle = nil;
	local status = nil;
	local sKey;
	local sSet;
	
	-- Clear out the local copy of current gear
	gcinclude.ClearSet(gcinclude.sets.CurrentGear);
	
	args[1] = string.lower(args[1]);
	if (args[1] == 'gswap') then			-- turns gear swapping on or off
		gcdisplay.AdvanceToggle('GSwap');
		toggle = 'Gear Swap';
		status = gcdisplay.GetToggle('GSwap');
	elseif args[1] == 'test' then
		local bTest = gcinclude.CheckPartyJob(args[2]);
		print(chat.header('Test'):append(chat.message('CheckPartyJob=' .. args[2] ..': ' ..tostring(bTest))));
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
		toggle = 'DT';
		status = gcdisplay.GetCycle('DT');
	elseif (args[1] == 'kite') then			-- Turns on/off whether movement gear is equipped
		gcdisplay.AdvanceToggle('Kite');
		toggle = 'Kite Set';
		status = gcdisplay.GetToggle('Kite');
	elseif (args[1] == 'solo') then			-- Turns on/off whether movement gear is equipped
		gcdisplay.AdvanceToggle('Solo');
		toggle = 'Solo Set';
		status = gcdisplay.GetToggle('Solo');		
	elseif (args[1] == 'tank') then			-- Turns on/off whether tanking gear is equipped
		if player.MainJob ~= 'SMN' then
			gcdisplay.AdvanceToggle('Tank');
			toggle = 'Tank Set';
			status = gcdisplay.GetToggle('Tank');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: SMN does not support tanking. Ignoring command')))
		end				
	elseif (args[1] == 'acc') then			-- Turns on/off whether accuracy gear should be equipped
		gcdisplay.AdvanceToggle('Acc');
		toggle = 'Accuracy';
		status = gcdisplay.GetToggle('Acc');
	elseif (args[1] == 'eva') then			-- Turns on/off whether evasion gear should be equipped
		gcdisplay.AdvanceToggle('Eva');
		toggle = 'Evasion';
		status = gcdisplay.GetToggle('Eva');
	elseif (args[1] == 'wswap') then		-- Turns on/off whether weapon swapping is permitted
		if player.MainJob ~= 'SMN' then
			gcdisplay.AdvanceToggle('WSwap');
			toggle = 'Weapon Swap';
			status = gcdisplay.GetToggle('WSwap');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Weapon swapping always enabled on summoners. Ignoring command')))
		end		
	elseif (args[1] == 'sbp') then			-- Turns on/off whether the blood pact message is shown
		if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
			gcdisplay.AdvanceToggle('sBP');
			toggle = 'Show Blood Pact';
			status = gcdisplay.GetToggle('sBP');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: /sBP is only available to summoners. Ignoring command')))
		end
	elseif (args[1] == 'th') then			-- Turns on/off whether TH gear should be equipped
		gcdisplay.AdvanceToggle('TH');
		toggle = 'Treasure Hunter';
		status = gcdisplay.GetToggle('TH');		
	elseif (args[1] == 'showit') then			-- Shows debug info for specified type: staff
		gcinclude.DB_ShowIt(args[2]);
	elseif (args[1] == 'gearset') then			-- Forces a gear set to be loaded and turns GSWAP off
		if #args > 1 then
			local sArg = string.upper(args[2]);
			local sTemp = ',' .. gcinclude.Crafting_Types .. ',' ..gcinclude.Gathering_Types .. ',FISH,';
			if string.find(sTemp,sArg) ~= nils then
				-- gather or crafting set
				local sCraft = ',' .. gcinclude.Crafting_Types .. ',';
				if string.find(sCraft,sArg) then
					-- crafting set
					gcinclude.MoveToCurrent(gcinclude.sets.Crafting,gcinclude.sets.CurrentGear);
					gcinclude.ProcessConditional(gcinclude.sets.Crafting_Conditional,sArg,gcinclude.sets.CurrentGear);					
				else
					if sArg == 'FISH' then
						--fish set
						gcinclude.MoveToCurrent(gcinclude.sets.FishingGear,gcinclude.sets.CurrentGear);
					else
						-- HELM, DIG or CLAM set
						gcinclude.MoveToCurrent(gcinclude.sets.Gathering,gcinclude.sets.CurrentGear);
						gcinclude.ProcessConditional(gcinclude.sets.Gathering_Conditional,sArg,gcinclude.sets.CurrentGear);
					end
				end
				gcinclude.EquipTheGear(gcinclude.sets.CurrentGear);
			else
				gFunc.ForceEquipSet(sArg);
			end
			if #args == 2 or string.lower(args[3]) ~= 'on' then
				gcdisplay.SetToggle('GSwap',false);
			else
				gcdisplay.SetToggle('GSwap',true);			
			end
			toggle = 'Gear Swap';
			status = gcdisplay.GetToggle('GSwap');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: No set specified for /gearset. Command ignored.')));
		end	
	elseif (args[1] == 'maxspell') then			-- Determines highest level spell to cast
		if #args >= 2 then
			gcinclude.MaxSpell(args[2],true);
		end
		toggle = 'MaxSpell';
	elseif (args[1] == 'maxsong') then			-- Determines highest level song to cast
		gcinclude.MaxSong(args[2],(#args > 2),true);
		toggle = 'MaxSong';
	elseif args[1] == 'equipit' then			-- Equip specified item
		gcinclude.EquipItem(args);
	elseif (args[1] == 'region') then			-- Toggles the region setting
		gcdisplay.AdvanceCycle('Region');
		toggle = 'Region';
		status = gcdisplay.GetCycle('Region');
    end

	if gcinclude.settings.Messages then
		gcinclude.Message(toggle, status)
	end
end		-- gcinclude.HandleCommands

--[[
	CheckCommonDebuffs determines if certain debuffs are on the player and loads an appropriate
	gear set. Please note that none of these gear sets will remove the debuff, that would be 
	against the TOS.
--]]

function gcinclude.CheckCommonDebuffs()
	local weakened = gData.GetBuffCount('Weakened');
	local sleep = gData.GetBuffCount('Sleep');
	local blind = gData.GetBuffCount('Blind');
	local para = gData.GetBuffCount('Paralysis');
	local doom = (gData.GetBuffCount('Doom'))+(gData.GetBuffCount('Bane'));
	local shiningRuby = gData.GetBuffCount('Shining Ruby');

	if (sleep >= 1) then 
		gFunc.EquipSet(gcinclude.sets.Sleeping);
	end
	if (doom >= 1) then	
		gFunc.EquipSet(gcinclude.sets.Doomed);
	end
	if (weakened >= 1) then
		gFunc.EquipSet(gcinclude.sets.Weakened);
	end;
	if (blind >= 1) then
		gFunc.EquipSet(gcinclude.sets.Blind);
	end
	if (para >= 1) then
		gFunc.EquipSet(gcinclude.sets.Paralyzed);
	end	
	if (shiningRuby >= 1) then
		gFunc.EquipSet(gcinclude.sets.Shining_Ruby);
	end
end		-- gcinclude.CheckCommonDebuffs

--[[
	WsStat determines which stats are emphasized when using the passed weaponskill name and
	returns an appropriate gear set name. All valid weaponskills are checked against.
--]]

function gcinclude.WsStat(ws_name,ws_default)
	local ws_stat;
	
	if string.find(ws_name,gcinclude.WS_AGI) ~= nil then
		ws_stat = 'WS_AGI';
	elseif string.find(ws_name,gcinclude.WS_CHR) ~= nil then
		ws_stat = 'WS_CHR';
	elseif string.find(ws_name,gcinclude.WS_DEX) ~= nil then
		ws_stat = 'WS_DEX';
	elseif string.find(ws_name,gcinclude.WS_DEXAGI) ~= nil then
		ws_stat = 'WS_DEXAGI';
	elseif string.find(ws_name,gcinclude.WS_DEXCHR) ~= nil then
		ws_stat = 'WS_DEXCHR';
	elseif string.find(ws_name,gcinclude.WS_DEXINT) ~= nil then
		ws_stat = 'WS_DEXINT';
	elseif string.find(ws_name,gcinclude.WS_INT) ~= nil then
		ws_stat = 'WS_INT';
	elseif string.find(ws_name,gcinclude.WS_INTMND) ~= nil then
		ws_stat = 'WS_INTMND';
	elseif string.find(ws_name,gcinclude.WS_MND) ~= nil then
		ws_stat = 'WS_MND';
	elseif string.find(ws_name,gcinclude.WS_STR) ~= nil then
		ws_stat = 'WS_STR';
	elseif string.find(ws_name,gcinclude.WS_STRAGI) ~= nil then
		ws_stat = 'WS_STRAGI';
	elseif string.find(ws_name,gcinclude.WS_STRDEX) ~= nil then
		ws_stat = 'WS_STRDEX';
	elseif string.find(ws_name,gcinclude.WS_STRMND) ~= nil then
		ws_stat = 'WS_STRMND';
	elseif string.find(ws_name,gcinclude.WS_STRMND_30_50) ~= nil then
		ws_stat = 'WS_STRMND_30_50';		
	elseif string.find(ws_name,gcinclude.WS_STRINT) ~= nil then
		ws_stat = 'WS_STRINT';
	elseif string.find(ws_name,gcinclude.WS_STRINT_30_20) ~= nil then
		ws_stat = 'WS_STRINT_30_20';		
	elseif string.find(ws_name,gcinclude.WS_STRVIT) ~= nil then
		ws_stat = 'WS_STRVIT';
	elseif string.find(ws_name,gcinclude.WS_VIT) ~= nil then
		ws_stat = 'WS_VIT';
	elseif string.find(ws_name,gcinclude.WS_Skill) ~= nil then
		ws_stat = 'WS_Skill';
	elseif string.find(ws_name,gcinclude.WS_HP) ~= nil then
		ws_stat = 'WS_HP';
	else
		ws_stat = 'WS_' .. ws_default;
	end
	return ws_stat;
end		-- gcinclude.WsStat

--[[
	These functions determines if the passed element is a good fit/bad fit for an elemental obi 
	based on the day and the weather
--]]

function gcinclude.EleWeak(ele)
	local sWeak = nil;
	
	ele = string.lower(ele);
	for i,v in pairs(gcinclude.WeekDayElement) do
		if v[1] == ele then
			sWeak = v[2];
			break;
		end
	end		
	return sWeak;
end		-- gcinclude.EleWeak

--[[
	CheckObiDW determines if the weather/day element makes equiping an elemental obi advantageous.
	
	Please note: Elemental obis can be useful when closing a skillchain with certain weaponskills.
	This code does NOT track that opportunity, so it is not even considered.
--]]

function gcinclude.CheckObiDW(ele)
	local PctDay = 0;
	local PctWeather = 0;
	local PctIridesecene = 0;
	local sEnvironment = gData.GetEnvironment();
	local sWeak = gcinclude.EleWeak(ele);
	local sDay = sEnvironment.DayElement;

	-- First, the day
	if string.lower(sDay) == string.lower(ele) then
		PctDay = 10;
	elseif string.lower(sWeak) == string.lower(ele) then
		PctDay = -10;
	end
	
	-- Next the weather
	if string.lower(sEnvironment.WeatherElement) == string.lower(ele) then
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
end		-- gcinclude.CheckObiDW

--[[
	CheckEleGorget determines if the passed weaponskill is elemental and whether the player
	owns the appropriate elemental gorget. Some weaponskills have multiple elements associated
	with them, so the first match where the player has the gorget will be used. One gorget is
	not considered stronger than another.
--]]

function gcinclude.CheckEleGorget(ws)
	
	if ws == nil then
		print(chat.header('CheckEleGorget'):append(chat.message('Error: weaponskill name is nil')));
		return;
	end
	
	-- Loop through the list of elemental gorgets to see what the player owns
	for i,v in pairs(gcinclude.elemental_gorgets) do
		-- The second value indicates ownership
		if v[2] == true then
			local swss = string.lower(gcinclude.eleWS[i]);
			local sws = string.lower(ws);
			-- Seach for the weaponskill in the weaponskill list associated with the element
			if string.find(swss,sws) ~= nil then
				-- Since found return the gorget's name
				return v[1];
			end
		end
	end	
	return;	
end		-- gcinclude.CheckEleGorget

--[[
	GetRoot determines the "base" of a spell name. (The base is the first word in the spell name.)
--]]

function gcinclude.GetRoot(spellName)
	local i;
	local root = spellName;
	
	spellName = string.lower(spellName);
	i = string.find(spellName,' ');
	if i ~= nil then
		root = string.sub(spellName,1,i-1);
	else
		root = spellName;
	end
	return root;
end		-- gcinclude.GetRoot

--[[
	CheckEleSpells determines if the passed in spell name is elemental in nature and returns
	the elemental type.
--]]

function gcinclude.CheckEleSpells(spellName,listName,sWhat)
	local root = nil;
	local k;
	local str;
	local pctDay = 0;
	local pctWeather = 0;
	
	if spellName == nil or listName == nil or sWhat == nil then
		print(chat.header('CheckEleSpells'):append(chat.message('Error: invalid passed parameters')));
		return;
	end

	root = gcinclude.GetRoot(string.lower(spellName));
	for k, str in pairs(listName) do						-- search the list
		if string.find(str,root) ~= nil then		-- if not nil then the "root" was found
			if sWhat == gcinclude.OBI and gcinclude.elemental_obis[k][2] then		
				pctDay,pctWeather = gcinclude.CheckObiDW(k);	-- determine if the day/weather is advantageous
				if (pctDay + pctWeather) > 0 then
					return gcinclude.elemental_obis[k][1];		-- return the obi's name
				end
			elseif sWhat == gcinclude.ELEMENT then
				return k;
			end
			break;
		end
	end
	return
end		-- gcinclude.CheckEleSpells

--[[
	CheckSummons is a simple routine that determines the element of the summoned avatar
--]]

function gcinclude.CheckSummons(spellName)
	local sn;
	
	if spellName == nil then
		print(chat.header('CheckSummons'):append(chat.message('Error: invalid passed parameter')));
		return nil;
	else
		sn = string.lower(spellName);
	end
	
	if gcinclude.SummonStaves[sn] ~= nil then
		return gcinclude.SummonStaves[sn];
	end
	return nil;
end		-- gcinclude.CheckSummons

--[[
	WhichStat determines if the passed spell has a stat associated with it that overrides the default gear
--]]

function gcinclude.WhichStat(spellName)
	local root = nil;
	local tbl;
	
	if spellName == nil then
		print('Debug: WhichStat - spellName is nil');
		return;
	end
	
	root = gcinclude.GetRoot(string.lower(spellName));
	for k, tbl in pairs(gcinclude.StatMagic) do						-- search the list
		if string.find(tbl[2],root) ~= nil then				-- if not nil then the "root" was found
			return tbl[1];
		end
	end
	return	
end		-- gcinclude.WhichStat

--[[
	WhichMagicSkill determines if the passed "spell" is associated with a specific magic skill set
--]]

function gcinclude.WhichMagicSkill(spellName)
	local root = nil;
	
	if spellName == nil then
		print(chat.header('WhichMagicSkill'):append(chat.message('Error: spellName is nil')));
		return;
	end
	
	root = gcinclude.GetRoot(string.lower(spellName));
	for k, str in pairs(gcinclude.MagicSkill) do				-- search the list
		if string.find(str,root) ~= nil then					-- if not nil then the "root" was found
			return k;
		end
	end
	return	
end		-- gcinclude.WhichMagicSkill

--[[
	CheckSpellBailout if the specified debuffs are in effect. If any are the player will be
	unable to automatically cancel any spell
--]]

function gcinclude.CheckSpellBailout()
	local sleep = gData.GetBuffCount('Sleep');
	local petrify = gData.GetBuffCount('Petrification');
	local stun = gData.GetBuffCount('Stun');
	local terror = gData.GetBuffCount('Terror');
	local silence = gData.GetBuffCount('Silence');
	local charm = gData.GetBuffCount('Charm');

	if (sleep+petrify+stun+terror+silence+charm >= 1) then
		return false;
	else
		return true;
	end
end		-- gcinclude.CheckSpellBailout

--[[
	CheckWsBailout determines if there's a debuff that will inhibit automatic cancelling of a weapons
	skill or if insufficient TP exist to do a weapon skill
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
	findString is multi-functional, searching the passed storage containers (whether accessible or not) for any or
	all of the passed string. Depending on the passed arguments, either the found items will be listed or the accessible
	storage table will be updated.
	
	findString(tStorage,sString,bUpdate,sName)
		where	tStorage is a list of the storage containers to search
				bUpdate indicates if the accessible storage table should be updated (inhibits displaying what is found)
				sName indicates which pet food is being looked for. In most cases this is nil
				
	Please note that this code was originally findPetFood and has been generalized
--]]

function gcinclude.findString(tStorage,sString,bUpdate,sName)
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local iCount = 0;
	
	-- process passed parameters
	if tStorage == nil or tStorage == {} then
		print(chat.header('findString'):append(chat.message('No storage containers specified')));
		return false;
	end
	
	if sString == nil then
		print(chat.header('findString'):append(chat.message('No search string specified')));
	end
	
	if bUpdate == nil then
		bUpdate = false;		-- Assume this is just a listing
	end
	
	if sName ~= nil then
		sName = string.lower(sName);
	end

	for k,_ in pairs(gcinclude.petfood) do
		gcinclude.petfood[k][4] = false;
		gcinclude.petfood[k][5] = nil;
	end

	iCnt = 0;
	for _ in pairs(tStorage) do iCnt = iCnt + 1 end
	
	-- now, loop through the passed storage containers
	for i = 1,iCnt,1 do
		bFound = false;
		containerID = gcinclude.STORAGES[i][1];
		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
				b,c = string.find(string.lower(item.Name[1]),sString);	
				if b ~= nil then
					if bUpdate then
						for k,tpf in pairs(gcinclude.petfood) do
							if string.lower(tpf[2]) == string.lower(item.Name[1]) then
								if gcinclude.petfood[k][4] == false then
									gcinclude.petfood[k][4] = true;
									gcinclude.petfood[k][5] = gcinclude.STORAGES[i][2];							
								end
							end
						end
						iCount = iCount + 1;
					else
						iCt = itemEntry.Count
						if iCt ~= nil and iCt > 0 then
							iCount = iCount + 1;
							if not bFound then
								for l,sl in pairs(gcinclude.STORAGES) do
									if containerID == sl[1] then
										print(chat.header('findString'):append(chat.message(sl[2])));
										bFound = true;
										break;
									end
								end
							end
							print(chat.header('findString'):append(chat.message('   ' .. item.Name[1] .. ' ('..tostring(iCt) .. ')')));
						end
					end
				end
			end
		end
	end

	return (iCount > 0);	
end		-- gcinclude.findString

--[[
	findMaxEquipablePetFood searches all accessible player storage containers (regardless of location)
	and equips the highest level pet food that can be equipped that's found.
--]]

function gcinclude.findMaxEquipablePetFood()
	
	-- see if any pet food is accessible (inventory, wardrobe, wardrobe 2)
	return gcinclude.findString(gcinclude.EQUIPABLE,'pet f',true,nil);		
end		-- gcinclude.findMaxEquipablePetFood

--[[
	doPetFood does one of two things. It either equips the indicated food or it
	shows where the food can be found. What is equipped will either be indicated or
	the max level pet food that can be equipped.
	
	/petfood [all|max] [name]
--]]

function gcinclude.doPetFood(action, sType)
	local player = gData.GetPlayer();
	local ilvl;
	local sName = nil;
		
	if action == nil then
		sAction = 'max';
	else
		sAction = string.lower(action);
		if not (sAction == 'all' or sAction == 'max' or sAction == 'min') then
			if sType ~= nil then
				print(chat.header('doPetFood'):append(chat.message('Invalid action specified : ' .. action .. '. Ignoring command')));
				return false;
			end
		else
			sType = nil;
		end
	end
	
	if sAction == 'all' then
		-- Currently only 1=Inventory,2=Safe,3=storage,6=satchel,9=wardrobe,11=wardrobe 2 are used, but 
		-- have included all for future expansion. (Note that 17=Wardrobe 8 holds event gear and is accessible,
		-- but you can't store petfood in there.)
		if not gcinclude.findString({1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17},'pet f',false,sType) then
			print(chat.header('doPetFood'):append(chat.message('No pet food found')));
		end
	else
		if (sAction == 'max' or sAction == 'min') then
			if not gcinclude.findMaxEquipablePetFood() then
				print(chat.header('doPetFood'):append(chat.message('No equipable pet food found or found pet food is too high level')));
				return false;
			end
		else
			if not gcinclude.findString(gcinclude.EQUIPABLE,sAction,true,nil) then 
				print(chat.header('doPetFood'):append(chat.message(action .. ' not found in accessible storage')));
			end
			return false;
		end
		
		-- Now to process what was found
		if sAction == 'max' then
			ilvl = 0;
		else
			ilvl = player.MainJobLevel;
		end
		
		for k,tpf in pairs(gcinclude.petfood) do
			if sAction == 'max' then
				if tpf[4] and (tpf[3] > ilvl) and (tpf[3] <= player.MainJobLevel) then
					ilvl = tpf[3];
					sName = tpf[2];
				end
			elseif sAction == 'min' then
				if tpf[4] and tpf[3] <= ilvl then
					ilvl = tpf[3];
					sName = tpf[2];
				end
			end
		end
	end

	if sName ~= nil then
		gFunc.ForceEquip('Ammo', sName);
		print(chat.header('doPetFood'):append(chat.message('Equipping: ' .. sName)));
		return true;
	end				
end		-- gcinclude.doPetFood

--[[
	Unload ensures that the aliases are removed and the display objects are removed
--]]

function gcinclude.Unload()
	gcinclude.ClearAlias();
	gcdisplay.Unload();
end

--[[
	Initialize gives luashitacast it's initial settings
--]]

function gcinclude.Initialize()
	gcdisplay.Initialize:once(2);
	gcinclude.SetVariables:once(2);
	gcinclude.SetAlias:once(2);
end

return gcinclude;