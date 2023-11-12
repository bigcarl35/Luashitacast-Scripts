local gcinclude = T{};

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
	
--[[
	Unfortunately the Town conditional set has to be in gcinclude. I can't seem to get it recognized 
	if it's in the profile area of the job file.
--]]

	['Town_Conditional'] = {
		{'BD-2','Federation Aketon','Movement gain in home nation city'},
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
        Ring2 = 'Pelican Ring',
        Legs = 'Fisherman\'s Hose',
        Feet = 'Waders',
    },
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
};

-- The following arrays are used by the functions contained in this file. Probably best to leave them alone

gcdisplay = gFunc.LoadFile('common\\gcdisplay.lua');

gcinclude.AliasList = T{'gswap','gcmessages','wsdistance','dt','dt_type','kite','acc','eva','craftset','gatherset','fishset','gearset','th','help','wswap','petfood','maxspell','maxsong','region','ajug'};
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
gcinclude.WS_AGI = 'Hot Shot,Split Shot,Sniper Shot,Slugshot,Blast Shot,Heavy Shot,Detonator';
gcinclude.WS_CHR = 'Shadowstitch';
gcinclude.WS_DEX = 'Wasp Sting,Viper Bite,Eviseration,Onslaught,Geirskogul,Blade: Metsu';
gcinclude.WS_DEXAGI = 'Shark Bite,Coronach';
gcinclude.WS_DEXCHR = 'Dancing Edge';
gcinclude.WS_DEXINT = 'Gust Slash,Cyclone';
gcinclude.WS_INT = 'Gate of Tartarus';
gcinclude.WS_INTAGI = 'Catastrophe';
gcinclude.WS_INTMND = 'Spirit Taker';
gcinclude.WS_MND = 'Energy Steal,Energy Drain';
gcinclude.WS_STR = 'Raging Axe,Smash Axe,Gale Axe,Avalanche Axe,Spinning Axe,Rampage,Mistral Axe,Decimation,Spinning Attack,Flat Blade,Circle Blade,Vorpal Blade,Hard Slash,Crescent Moon,Mercy Stroke,Iron Tempest,Sturmwind,Keen Edge,Raging Rush,Metatron Torment,Leg Sweep,Skewer,Wheeling Thrust,Impulse Drive,Tachi: Enpi,Tachi: Hobaku,Tachi: Goten,Tachi: Kagero,Tachi: Jinpu,Tachi: Yukikaze,Tachi: Gekko,Tachi: Kasha,Tachi:Kaiten,Brainshaker,Skullbreaker,True Strike,Heavy Swing,Shell Crusher,Full Swing';
gcinclude.WS_STRAGI = 'Sickle Moon,Vorpal Thrust,Flaming Arrow,Piercing Arrow,Dulling Arrow,Sidewinder,Blast Arrow,Arching Arrow,Empyreal Arrow,Namas Arrow';
gcinclude.WS_STRDEX = 'Combo,Backhand Blow,Raging Fists,Fast Blade,Knights of Round,Double Thrust,Penta Thrust,Blade: Rin,Blade: Retsu,Blade: Jin,Blade: Ten,Blade: Ku';
gcinclude.WS_STRINT = 'Dark Harvest,Shadow of Death,Nightmare Scythe,Spiral Hell,Burning Blade,Frostbite,Freezebite,Spinning Slash,Ground Strike,Thunder Thrust,Raiden Thrust,Blade: Teki,Blade: To,Blade: Chi,Blade: Ei,Rock Crusher,Earth Crusher';
gcinclude.WS_STRINT_30_20 = 'Red Lotus Blade';
gcinclude.WS_STRMND = 'Guillotine,Cross Reaper,Shining Blade,Seraph Blade,Swift Blade,Savage Blade,Shockwave,Tachi: Koki,Shining Strike,Seraph Strike,Judgment,Hexastrike,Randgrith,Starburst,Sunburst,Retribution';
gcinclude.WS_STRMND_30_50 = 'Black Halo';
gcinclude.WS_STRVIT = 'Calamity,Slice,Spinning Scythe,Vorpal Scythe,Howling Fist,Dragon Kick,Asuran Fists,Power Slash,Scourge,Shield Break,Armor Break,Weapon Break,Full Break,Steel Cyclone';
gcinclude.WS_VIT = 'Shoulder Tackle,One Inch Punch,Final Heaven';

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

-- define constants for DT_Type so typos aren't made
gcinclude.PHY = 'Physical';
gcinclude.MAG = 'Magical';
gcinclude.BRE = 'Breath';

-- Define constants dealing with magic gear and jobs
gcinclude.ELEMENT = 'ele';
gcinclude.OBI = 'obi';
gcinclude.aketon = {['Sandy'] = {'Kingdom Aketon',false}, ['Windy'] = {'Federation Aketon',false}, ['Bastok'] = {'Republic Aketon',false}};
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

-- This breaks out all spells/songs based on type of magic skill. Please note that "singing" includes instrument 
-- skills too
gcinclude.MagicSkill = T{
	['Cure'] = 'cure,curaga,cura',
	['Dark'] = 'bio,drain,aspir,absorb-agi,absorb-chr,absorb-dex,absorb-int,absorb-mnd,absorb-str,absorb-vit,absorb-tp,absorb-acc,tractor,stun,dread',
	['Divine'] = 'banish,holy,flash,repose,enlight',
	['Enfeebling'] = 'bind,blind,dia,diaga,distract,frazzle,gravity,paralyze,poison,poisonga,sleep,sleepga,silence,slow',
	['Enhancing'] = 'aquaveil,auspice,baraera,baraero,barblind,barblindra,barblizzard,barblizzara,barfira,barfire,barparalyze,barparalyzra,barpetra,barpetrify,barpoison,barpoisonra,barsilence,barsilenera,barsleep,barsleepra,barstone,barstonra,barthunder,barthundra,barvira,barvirus,barwater,barwatera,blaze,blink,deoderize,enaero,enblizzard,enfire,enstone,enthunder,enwater,erase,escape,flurry,haste,ice,invisible,phalanx,protect,protectra,refresh,regen,reprisal,retrace,shell,shellra,shock,sneak,stoneskin,teleport-altep,teleport-dem,teleport-holla,teleport-mea,teleport-vahzl,teleport-yhoat,warp',
	['Elemental'] = 'aero,aeroga,blizzaga,blizzard,burn,burst,drown,fira,firaga,fire,flare,flood,freeze,frost,quake,rasp,shock,stone,stonega,thundaga,thunder,tornado,water,watera',
	['Ninjitsu'] = 'tonko:,utsusemi:,katon:,hyoton:,huton:,doton:,raiton:,suiton:,kurayami:,hojo:,monomi:,dokumori:,jubaku:',
	['Summoning'] = 'carbuncle,fenrir,ifrit,titan,leviathan,garuda,shiva,ramuh,diabolos,fire,ice,air,earth,thunder,water,light,dark,cait,siren,atomos,alexander,odin',
};

-- This table associates a summoned avatar with an element so that the appropriate stave can be equipped
gcinclude.SummonStaves = T{
	['carbuncle'] = 'light', ['light spirit'] = 'light', ['cait sith'] = 'light', ['alexander'] = 'light',
	['fenrir'] = 'dark', ['diabolos'] = 'dark', ['dark spirit'] = 'dark', ['atomos'] = 'dark', ['odin'] = 'dark',
	['ifrit'] = 'fire', ['fire spirit'] = 'fire',
	['titan'] = 'earth', ['earth spirit'] = 'earth',
	['leviathan'] = 'water', ['water spirit'] = 'water',
	['garuda'] = 'wind', ['air spirit'] = 'wind', ['siren'] = 'wind',
	['shiva'] = 'ice', ['ice spirit'] = 'ice',
	['ramuh'] = 'thunder', ['thunder spirit'] = 'thunder'
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

--[[
	This table contains all of the parameters needed to process the conditional gear to
	determine if it should be equipped. The columns are: ID, slot, level, job(s), [code, 
	param, Operator], ...	as needed. Param's separated by commas mean that one of the
	params must match to be counted.
	
	Note: Nighttime is 17:00 to 6:00, Daytime is 6:00 to 18:00, Dusk to Dawn: 17:00 to 7:00,
		  New Day: 4:00, Dawn: 6:00 to 7:00, Day: 7:00 to 17:00, Dusk: 17:00 to 18:00, 
		  Evening: 18:00 to 20:00, Dead of Night: 20:00 to 4:00.
--]]

gcinclude.MasterConditional = T {
	['WP-1'] = {'Main',1,'ALL','CRAFT','AL'},
	['HD-1'] = {'Head',1,'ALL','CRAFT','BN'},
	['HD-2'] = {'Head',1,'ALL','CRAFT','CL'},
	['HD-3'] = {'Head',1,'ALL','CRAFT','CO'},
	['HD-4'] = {'Head',1,'ALL','CRAFT','GS'},
	['HD-5'] = {'Head',1,'ALL','MOON:DAY:NIGHT','Full Moon','Darksday','Nighttime'},
	['HD-6'] = {'Head',1,'ALL','MOON:DAY:NIGHT','New Moon','Lightsday','Daytime'},
	['HD-7'] = {'Head',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday,Windsday'},
	['HD-8'] = {'Head',34,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['HD-9'] = {'Head',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['HD-10'] = {'Head',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['HD-11'] = {'Head',34,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['HD-12'] = {'Head',34,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['HD-13'] = {'Head',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['HD-14'] = {'Head',41,'ALL','WEATHER','Water'},
	['HD-15'] = {'Head',43,'WAR/PLD/DRK','NATION',true},
	['HD-16'] = {'Head',43,'WAR/PLD/DRK','NATION',true},
	['HD-17'] = {'Head',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['HD-18'] = {'Head',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['HD-19'] = {'Head',53,'MNK/SAM/NIN','WEATHER','Water'},
	['HD-20'] = {'Head',62,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','DAY','Watersday,Darksday'},
	['HD-21'] = {'Head',65,'MNK/WHM/RDM/THF/BST/BRD/NIN/DRG/DNC','NATION',true},
	['NK-1'] = {'Neck',30,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['NK-2'] = {'Neck',40,'ALL','WEATHER','Ice'},
	['NK-3'] = {'Neck',65,'ALL','NATION',false},
	['NK-4'] = {'Neck',65,'ALL','NATION',false},
	['NK-5'] = {'Neck',65,'ALL','NATION',false},
	['NK-6'] = {'Neck',71,'ALL','DAY|TIME'},
	['NK-7'] = {'Neck',71,'ALL','DAY|TIME'},
	['ER-1'] = {'EAR',40,'ALL','NATION',false},
	['ER-2'] = {'EAR',65,'ALL','WEATHER','Dark'},
	['ER-3'] = {'EAR',67,'WAR/PLD/DRK/BST/SAM/NIN','TIME','Nighttime'},
	['BD-1'] = {'Body',50,'BST','MP<50'},
	['BD-2'] = {'Body',1,'ALL','AKETON',true,'Windy'},
	['BD-3'] = {'Body',1,'ALL','CRAFT','AL'},
	['BD-4'] = {'Body',1,'ALL','CRAFT','BN'},
	['BD-5'] = {'Body',1,'ALL','CRAFT','CL'},
	['BD-6'] = {'Body',1,'ALL','CRAFT','CO'},
	['BD-7'] = {'Body',1,'ALL','CRAFT','GS'},
	['BD-8'] = {'Body',1,'ALL','CRAFT','LT'},
	['BD-9'] = {'Body',1,'ALL','CRAFT','SM'},
	['BD-10'] = {'Body',1,'ALL','CRAFT','WW'},
	['BD-11'] = {'Body',1,'ALL','GATHER','HELM'},
	['BD-12'] = {'Body',15,'ALL','GATHER','HELM'},
	['BD-21'] = {'Body',1,'ALL','GATHER','DIG'},
	['BD-22'] = {'Body',15,'ALL','GATHER','DIG'},
	['BD-23'] = {'Body',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday'},
	['BD-24'] = {'Body',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday'},
	['BD-25'] = {'Body',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday,Windsday'},
	['BD-26'] = {'Body',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['BD-27'] = {'Body',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COP/PUP/DNC/GEO/RUN','NATION',true},
	['BD-28'] = {'Body',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['BD-29'] = {'Body',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['BD-30'] = {'Body',43,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['BD-31'] = {'Body',43,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['BD-32'] = {'Body',43,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['BD-33'] = {'Body',43,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['BD-34'] = {'Body',43,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['BD-35'] = {'Body',43,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['BD-36'] = {'Body',43,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['BD-37'] = {'Body',52,'WAR/PLD/DRK','NATION',true},
	['BD-38'] = {'Body',52,'WAR/PLD/DRK','NATION',true},
	['BD-39'] = {'Body',52,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/PUP/SCH/GEO/RUN','NATION',true},
	['BD-40'] = {'Body',52,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/PUP/SCH/GEO/RUN','NATION',true},
	['BD-41'] = {'Body',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['BD-42'] = {'Body',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['BD-43'] = {'Body',1,'ALL','AKETON',true,'Sandy'},
	['BD-44'] = {'Body',1,'ALL','AKETON',true,'Bastok'},
	['BD-45'] = {'Body',43,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['HN-1'] = {'Hands',1,'CRAFT','LT'},
	['HN-2'] = {'Hands',1,'CRAFT','SM'},
	['HN-3'] = {'Hands',1,'CRAFT','WW'},
	['HN-4'] = {'Hands',1,'GATHER','HELM'},
	['HN-5'] = {'Hands',15,'GATHER','HELM'},
	['HN-6'] = {'Hands',1,'GATHER','DIG'},
	['HN-7'] = {'Hands',15,'GATHER','DIG'},
	['HN-8'] = {'Hands',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['HN-9'] = {'Hands',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['HN-10'] = {'Hands',34,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['HN-11'] = {'Hands',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['HN-12'] = {'Hands',34,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['HN-13'] = {'Hands',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['HN-14'] = {'Hands',36,'ALL','DAY','Firesday,Earthsday,Watersday,Windsday,Iceday,Lightningday'},
	['HN-15'] = {'Hands',43,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['HN-16'] = {'Hands',43,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['HN-17'] = {'Hands',43,'WAR/PLD/DRK','NATION',true},
	['HN-18'] = {'Hands',43,'WAR/PLD/DRK','NATION',true},
	['HN-19'] = {'Hands',43,'MNK/SAM/NIN','WEATHER','Water'},
	['HN-20'] = {'Hands',52,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['HN-21'] = {'Hands',52,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['HN-22'] = {'Hands',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['HN-23'] = {'Hands',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['HN-24'] = {'Hands',58,'MNK/SAM/NIN','WEATHER','Water'},
	['HN-25'] = {'Hands',58,'MNK/SAM/NIN','WEATHER','Water'},
	['HN-26'] = {'Hands',58,'WAR/RDM/THF/PLD/DRK/BST/BRD/RNG/SAM/NIN/DRG','WEATHER','Earth'},
	['HN-27'] = {'Hands',63,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','TIME','Daytime'},
	['HN-28'] = {'Hands',63,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','TIME','Daytime'},
	['HN-29'] = {'Hands',65,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',false},
	['HN-30'] = {'Hands',65,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',false},	
	['HN-31'] = {'Hands',65,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',false},
	['HN-32'] = {'Hands',65,'MNK/WHM/BLM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',false},
	['HD-33'] = {'Hands',65,'ALL','NATION',false},
	['RN-1'] = {'RING',60,'ALL','NATION',true},
	['RN-2'] = {'RING',60,'ALL','NATION',true},
	['RN-3'] = {'RING',60,'ALL','NATION',true},
	['RN-4'] = {'RING',65,'ALL','DAY','Darksday'},
	['RN-5'] = {'RING',65,'ALL','DAY','Earthsday'},
	['RN-6'] = {'RING',65,'ALL','DAY','Firesday'},
	['RN-7'] = {'RING',65,'ALL','DAY','Lightningday'},
	['RN-8'] = {'RING',65,'ALL','DAY','Watersday'},
	['RN-9'] = {'RING',65,'ALL','DAY','Windsday'},
	['RN-10'] = {'RING',75,'ALL','DAY','Darksday'},
	['RN-11'] = {'RING',30,'ALL','SJ:MAGIC'},
	['BK-1'] = {'Back',40,'WAR/MNK/RDM/THF/PLD/DRK/BST/BRD/RNG/SAM/NIN/DRG/BLU/COR/DNC/RUN','NATION',false},
	['BK-2'] = {'Back',62,'ALL','WEATHER','Earth'},
	['BK-3'] = {'Back',62,'ALL','WEATHER','Earth'},
	['WS-1'] = {'Waist',1,'ALL','CRAFT','AL'},
	['WS-2'] = {'Waist',1,'ALL','CRAFT','BN'},
	['WS-3'] = {'Waist',1,'ALL','CRAFT','CL'},
	['WS-4'] = {'Waist',1,'ALL','CRAFT','CO'},
	['WS-5'] = {'Waist',1,'ALL','CRAFT','GS'},
	['WS-6'] = {'Waist',1,'ALL','CRAFT','LT'},
	['WS-7'] = {'Waist',1,'ALL','CRAFT','SM'},
	['WS-8'] = {'Waist',1,'ALL','CRAFT','WW'},
	['WS-10'] = {'Waist',52,'WAR/RDM/THF/PLD/BST/BRD/RNG/SAM/NIN/DRG/BLU/COR/DNC/RUN','NATION',true},
	['WS-11'] = {'Waist',52,'WAR/RDM/THF/PLD/BST/BRD/RNG/SAM/NIN/DRG/BLU/COR/DNC/RUN','NATION',true},
	['WS-12'] = {'Waist',65,'ALL','DAY','Watersday'},
	['WS-13'] = {'Waist',65,'ALL','DAY','Windsday'},
	['LG-1'] = {'Legs',1,'ALL','GATHER','HELM'},
	['LG-2'] = {'Legs',15,'ALL','GATHER','HELM'},
	['LG-11'] = {'Legs',1,'ALL','GATHER','DIG'},
	['LG-12'] = {'Legs',15,'ALL','GATHER','DIG'},
	['LG-13'] = {'Legs',1,'ALL','TIME','Nighttime'},
	['LG-14'] = {'Legs',1,'ALL','WEATHER','Sunshine'},
	['LG-15'] = {'Legs',1,'ALL','WEATHER','Sunshine'},
	['LG-16'] = {'Legs',1,'ALL','WEATHER','Sunshine'},
	['LG-17'] = {'Legs',1,'ALL','WEATHER','Sunshine'},
	['LG-18'] = {'Legs',1,'ALL','WEATHER','Sunshine'},
	['LG-19'] = {'Legs',1,'ALL','WEATHER','Sunshine'},
	['LG-20'] = {'Legs',1,'ALL','WEATHER','Sunshine'},
	['LG-21'] = {'Legs',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday'},
	['LG-22'] = {'Legs',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday,Windsday'},
	['LG-23'] = {'Legs',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['LG-24'] = {'Legs',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['LG-25'] = {'Legs',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['LG-26'] = {'Legs',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['LG-27'] = {'Legs',43,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['LG-28'] = {'Legs',43,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['LG-29'] = {'Legs',43,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['LG-30'] = {'Legs',43,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['LG-31'] = {'Legs',52,'WAR/PLD/DRK','NATION',true},
	['LG-32'] = {'Legs',52,'WAR/PLD/DRK','NATION',true},
	['LG-33'] = {'Legs',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['LG-34'] = {'Legs',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['LG-35'] = {'Legs',71,'NIN','TIME','Nighttime'},
	['FT-1'] = {'Feet',1,'ALL','GATHER','HELM'},
	['FT-2'] = {'Feet',15,'ALL','GATHER','HELM'},
	['FT-3'] = {'Feet',1,'ALL','GATHER','DIG'},
	['FT-4'] = {'Feet',15,'ALL','GATHER','DIG'},
	['FT-5'] = {'Feet',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday'},
	['FT-6'] = {'Feet',30,'MNK/RDM/PLD/BRD/RNG/BLU/RUN','DAY','Earthsday,Windsday'},
	['FT-7'] = {'Feet',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['FT-8'] = {'Feet',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['FT-9'] = {'Feet',34,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['FT-10'] = {'Feet',34,'WAR/RDM/PLD/DRK/BST/RNG/SAM/DRG/BLU/RUN','NATION',true},
	['FT-11'] = {'Feet',34,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['FT-12'] = {'Feet',34,'MNK/WHM/RDM/THF/PLD/BST/BRD/DRG/SMN/BLU/COR/PUP/DNC/GEO/RUN','NATION',true},
	['FT-13'] = {'Feet',41,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','MOON','New Moon'},
	['FT-14'] = {'Feet',41,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','MOON','New Moon'},
	['FT-15'] = {'Feet',43,'MNK/WHM/BLM/RDM/PLD/BRD/RND/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['FT-16'] = {'Feet',43,'MNK/WHM/BLM/RDM/PLD/BRD/RND/SMN/BLU/PUP/SCH/GEO/RUN','NATION',true},
	['FT-17'] = {'Feet',43,'WAR/PLD/DRK','NATION',true},
	['FT-18'] = {'Feet',43,'WAR/PLD/DRK','NATION',true},
	['FT-19'] = {'Feet',47,'MNK/SAM/NIN','WEATHER','Water'},
	['FT-20'] = {'Feet',52,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['FT-21'] = {'Feet',52,'WAR/PLD/DRK/BST/SAM/NIN','NATION',true},
	['FT-22'] = {'Feet',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['FT-23'] = {'Feet',52,'MNK/WHM/BLM/RDM/THF/DRK/BRD/RNG/SMN/BLU/COR/PUP/DNC/SCH/GEO/RUN','NATION',true},
	['FT-24'] = {'Feet',59,'MNK/WHM/BLM/RDM/PLD/BRD/RNG/SMN/BLU/PUP/SCH/GEO/RUN','TIME','Nighttime'},
	['FT-25'] = {'Feet',74,'NIN','TIME','DUSK2DAWN'},
	['FT-26'] = {'Feet',75,'NIN','TIME','DUSK2DAWN'}
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

-- This is the list of storage containers that can be equipped from outside of a moghouse
gcinclude.EQUIPABLE = {gcinclude.STORAGES[1],gcinclude.STORAGES[9],gcinclude.STORAGES[11]};

-- Gear set for holding conditional gear that's equipable along with associated levels
gcinclude.tGS = {Main=nil,Sub=nil,Range=nil,Ammo=nil,Head=nil,Neck=nil,Ear1=nil,Ear2=nil,Body=nil,Hands=nil,
				 Ring1=nil,Ring2=nil,Back=nil,Waist=nil,Legs=nil,Feet=nil};	-- Empty gearset for conditional gear
gcinclude.tGSL = {Main=0,Sub=0,Range=0,Ammo=0,Head=0,Neck=0,Ear1=0,Ear2=0,Body=0,Hands=0,
				  Ring1=0,Ring2=0,Back=0,Waist=0,Legs=0,Feet=0};			-- Empty gearset levels, for comparison
	
--[[
	Message toggles on/off a feedback mechanism for all luashitacast commands
--]]
	
function gcinclude.Message(toggle, status)
	if toggle ~= nil and status ~= nil then
		print(chat.header('GCinclude'):append(chat.message(toggle .. ' is now ' .. tostring(status))))
	end
end

--[[
	SetAlias registers all of the luashitacast commands that are defined in this file
--]]

function gcinclude.SetAlias()
	for _, v in ipairs(gcinclude.AliasList) do
		AshitaCore:GetChatManager():QueueCommand(-1, '/alias /' .. v .. ' /lac fwd ' .. v);
	end
end

--[[
	ClearAlias removes the luashitacast commands that were registered in this file
--]]

function gcinclude.ClearAlias()
	for _, v in ipairs(gcinclude.AliasList) do
		AshitaCore:GetChatManager():QueueCommand(-1, '/alias del /' .. v);
	end
end

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
end

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
end

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
end

--[[
	SetVariables defines run settings for luashitacast
--]]

function gcinclude.SetVariables()
	local player = gData.GetPlayer();

	gcdisplay.CreateToggle('GSwap', true);
	gcdisplay.CreateToggle('DT', false);
	gcdisplay.CreateToggle('Kite', false);
	gcdisplay.CreateToggle('Acc', false);
	gcdisplay.CreateToggle('Eva', false);
	gcdisplay.CreateToggle('WSwap',false);
	gcdisplay.CreateToggle('TH',false);
	gcdisplay.CreateToggle('AJug',true);
	
	gcdisplay.CreateCycle('DT_Type', {[1] = gcinclude.PHY, [2] = gcinclude.MAG, [3] = gcinclude.BRE});
	gcdisplay.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned'});
	
	-- Initialize what weapons are equipped
	local ew = gData.GetEquipment();
	if ew.Main ~= nil then
		gcinclude.weapon = ew.Main.Name;
		if ew.Sub == nil then
			gcinclude.offhand = nil;
		else
			gcinclude.offhand = ew.Sub.Name;
		end
	end

	gcinclude.CheckForStaves();
	gcinclude.CheckForObisGorgets();
end

--[[
	BuildGear populates the holding gear set according to the passed table entries
--]]

function gcinclude.BuildGear(tMasCond,tEntry)
	local pos = 0;
	local slot;
	
	-- There's a special case for RING and EARRING. We just need to find the
	-- first ones (ring1,earring1) and we can proceed from there.
	if tMasCond[1] == 'RING' then
		slot = 'Ring1';
	elseif tMasCond[1] == 'EARRING' then
		slot = 'Ear1';	
	else
		slot = tMasCond[1];
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

	if tMasCond[1] == 'RING' or tMasCond == 'EARRING' then
		if gcinclude.tGSL[pos] > 0 then				-- A conditional already processed for spot
			if gcinclude.tGSL[pos+1] > 0 then		-- Two already processed for slot. Trickier...
				if tMasCond[2] < gcinclude.tGSL[pos] then
					if tMasCond[2] > gcinclude.tGSL[pos+1] then
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
	if tMasCond[2] > gcinclude.tGSL[pos] then
		-- Copy the name of the gear piece to the appropriate slot in the temporary gear set
		gcinclude.tGS[pos] = tEntry[2];
		-- Copy the level of the gear piece to the associaed slot in the temporary gear set level list
		gcinclude.tGSL[pos] = tMasCond[2];
		return true;
	end
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
end
	
--[[
	ProcessConditional determines if any of the specified consitional equipment should be loaded. 
	tTable is the conditional gear.
	
	WIP, need to rethink this some. Of concern is multiple conditions. Initially will only support
	one condition. Get that working and then maybe support multiple.
--]]

function gcinclude.ProcessConditional(tTest,sType)
	local player = gData.GetPlayer();
	local pMJ = player.MainJob;
	local pLevel = player.MainJobSync;
	local timestamp = gData.GetTimestamp();
	local environ = gData.GetEnvironment();
	local zone = gData.GetEnvironment();
	local sKey;
				  
	-- clear out the holding table so no interference from a previous call
	gcinclude.tGS = {nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil};
	gcinclude.tGSL = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	
	-- Now process all the conditional gear
	local iPiece = 0;
	if tTest == nil then
		return
	end
	
	for k,v in pairs(tTest) do
		local tMatched = gcinclude.MasterConditional[v[1]];
		-- Make sure current job can use the gear
		if (string.find(tMatched[3],pMJ) ~= nil or tMatched[3] == 'ALL') then
			-- Check that the gear minimum level isn't too high
			if tMatched[2] <= pLevel then
				bMatch = false;	-- Indicator to track if there's a match
				-- Now determine the type of condition and process
				if tMatched[4] == 'CRAFT' then
					sKey = string.upper(sType);
					if tMatched[5] == sKey then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end
				elseif tMatched[4] == 'GATHER' then
					sKey = string.upper(sType);
					if tMatched[5] == sKey then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end
				elseif tMatched[4] == 'MOON' then
					if lower(environ.MoonPhase) == lower(tMatched[5]) then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end
				elseif tMatched[4] == 'DAY' then
					if string.find(lower(v[5]),lower(environ.Day)) ~= nil then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end
				elseif tMatched[4] == 'TIME' then
					bMatch = gcinclude.CheckTime(timestamp.hour.tMatched[5],true);
				elseif tMatched[4] == 'NATION' then
					bKey = (gcdisplay.GetCycle('Region') == 'Owned');
					if (bKey and tMatched[5]) or (bKey == false and tMatched == false) then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end					
				elseif tMatched[4] == 'AKETON' then
					if gcinclude.settings.bAketon == false then		-- Make sure all nation aketon's are tracked.
						gcinclude.CheckForAllNationalAketons();
					end
					
					-- Checks for the nation of the aketon, whether the zone is in that nation, and if the player is 
					-- from that nation (Sandy = 0, Bastok = 1, and Windy = 2). (player was not used to check nationality
					-- since luashitacast does not carry the home nation setting that is found in AshitaCore. Alse, I found 
					-- the nationality translations in campaign_nation.sql file that's part of AirSkyBoat Github source.)
					local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
					
					if ((tMatched[6] == 'Windy') and (zone.Area ~= nil) and (gcinclude.Windy:contains(zone.Area)) and pNation == 2) or
						((tMatched[6] == 'Sandy') and (zone.Area ~= nil) and (gcinclude.Sandy:contains(zone.Area)) and pNation == 0) or
						((tMatched[6] == 'Bastok') and (zone.Area ~= nil) and (gcinclude.Bastok:contains(zone.Area)) and pNation == 1)
					then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end
				elseif tMatched[4] == 'WEATHER' then
					if string.find(lower(tMatched[5]),lower(environ.RawWeather)) ~= nil then 
						bMatch = gcinclude.BuildGear(tMatched,v);
					end
				elseif tMatched[4] == 'MOON:DAY:NIGHT' then
					if string.find(lower(tMatched[5]),lower(environ.MoonPhase)) ~= nil then
						if string.find(lower(tMatched[6]),lower(environ.Day)) ~= nil then
							bMatch = gcinclude.CheckTime(timestamp.hour,tMatched[7],true);
						end
					end
				elseif tMatched[4] == 'DAY|TIME' then	-- Funky test for Brisingamen
					local iDay = string.find(lower(v[4]),lower(environ.Day));
					local ts = timestamp.hour;
					local bNight = gcinclude.CheckTime(ts,'Nighttime',false);
					local bDay = gcinclude.CheckTime(ts,'Daytime',false);
				
					if iDay ~= nil or bNight or bDay then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end				
				elseif tMatched[4] == 'MP<50' then
					if (gcinclude.settings.bMagic and gcinclude.settings.b50 and player.MP < 50) then
						bMatch = gcinclude.BuildGear(tMatched,v);
					end
				elseif tMatched[4] == 'SJ:MAGIC' then
					if gcinclude.settings.bMagic then
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
				--local slot = gData.Constants.EquipSlotNames[i];
				gFunc.Equip(i,gcinclude.tGS[i]);
			end
		end
	end
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
end
	
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
end

--[[
	CheckMagic50 determines if the player's mainjob and subjob uses MP and whether the maximum 
	MP the player has is greater than 50.
--]]

function gcinclude.CheckMagic50(player)

	if (string.find(gcinclude.sMagicJobs,player.MainJob) ~= nil) then
		gcinclude.settings.bMJ = true;
	end
	if (string.find(gcinclude.sMagicJobs,player.SubJob) ~= nil) then
		gcinclude.settings.bSJ = true;
	end
	gcinclude.settings.bMagic = gcinclude.settings.bMJ or gcinclude.settings.bSJ;
	
	-- This is a special case, need to check that player has potentially more than 50
	gcinclude.settings.b50 = (gcinclude.settings.bMagic and player.MaxMP > 50);
	return;
end

--[[
	SwapToStave determines if swapping your weapon out for one of the elemental staves makes
	sense and does it for you while remembering what weapon/offhand you had equipped.
	
	Note: there's a timing issue when it comes to getting the two weapons currently equipped
	that has to do with zoning. I don't see any reason for this routine to run afoul of the
	problem, so I'm going to assume it won't occur here. If an error does arise though, what
	is needed to be done is to check if ew['Main'] == nil (the same is true for ew['Sub']).
--]]

function gcinclude.SwapToStave(sStave,noSave)
	local ew = gData.GetEquipment();
	local eWeap = ew['Main'].Name;
	local eOff = nil;

	if ew['Sub'] ~= nil then
		eOff = ew['Sub'].Name;
	end;

	if ((gcdisplay.GetToggle('WSwap') == true or gcinclude.settings.bSummoner) and 
		(gcinclude.elemental_staves[sStave][2] == true or gcinclude.elemental_staves[sStave][4] == true)) then

		-- See if a current weapon is the one of the targetted staves
		if not ((string.lower(eWeap) == string.lower(gcinclude.elemental_staves[sStave][1])) or 
			(string.lower(eWeap) == string.lower(gcinclude.elemental_staves[sStave][3]))) then
			-- save the weapon so it can be equipped again
			if eWeap ~= gcinclude.weapon and noSave == false and gcinclude.settings.bSummoner == false then
				gcinclude.weapon = eWeap;
				gcinclude.offhand = eOff;
			end

			-- Now, try the HQ and then the NQ
			if gcinclude.elemental_staves[sStave][4] == true then
				pos = 3;
			else
				pos = 1;
			end
			gFunc.ForceEquip('Main', gcinclude.elemental_staves[sStave][pos]);
			end
		end
	end

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
	
	args[1] = string.lower(args[1]);
	if (args[1] == 'gswap') then			-- turns gear swapping on or off
		gcdisplay.AdvanceToggle('GSwap');
		toggle = 'Gear Swap';
		status = gcdisplay.GetToggle('GSwap');
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
	elseif (args[1] == 'dt') then 			-- Indicates if damage taken gear needs to be equuipped
		gcdisplay.AdvanceToggle('DT');
		toggle = 'DT';
		status = gcdisplay.GetToggle('DT');
	elseif (args[1] == 'dt_type') then		-- Indicates the type of damage taken gear that will be equipped if desired
		if #args == 1 then			-- No qualifier, assume next in set
			gcdisplay.AdvanceCycle('DT_Type');
		else
			local cType = string.upper(string.sub(args[2],1,1));
			local sType = gcinclude.PHY;
			if  cType == 'M' then
				sType = gcinclude.MAG;
			elseif cType == 'B' then
				sType = gcinclude.BRE;
			end				
			gcdisplay.SetCycle('DT_Type',sType);
		end
		toggle = 'DT_Type';
		status = gcdisplay.GetToggle('DT_Type');
	elseif (args[1] == 'kite') then			-- Turns on/off whether movement gear is equipped
		gcdisplay.AdvanceToggle('Kite');
		toggle = 'Kite Set';
		status = gcdisplay.GetToggle('Kite');
	elseif (args[1] == 'acc') then			-- Turns on/off whether accuracy gear should be equipped
		gcdisplay.AdvanceToggle('Acc');
		toggle = 'Accuracy';
		status = gcdisplay.GetToggle('Acc');
	elseif (args[1] == 'wswap') then		-- Turns on/off whether weapon swapping is permitted
		gcdisplay.AdvanceToggle('WSwap');
		toggle = 'Weapon Swap';
		status = gcdisplay.GetToggle('WSwap');
	elseif (args[1] == 'th') then			-- Turns on/off whether TH gear should be equipped
		gcdisplay.AdvanceToggle('TH');
		toggle = 'Treasure Hunter';
		status = gcdisplay.GetToggle('TH');		
	elseif (args[1] == 'craftset') then		-- Equips the specified crafting set gear and turns off GSWAP
		bOk = true;
		
		if args[2] ~= nil then
			sKey = string.upper(args[2]);
		else
			sKey = nil;
		end
		
		if #args > 1 then
			if string.find(gcinclude.Crafting_Types,sKey) == nil then
				print(chat.header('HandleCommands'):append(chat.message('Error: Invalid crafting type specified: ' ..args[2])));
				print(chat.header('HandleCommands'):append(chat.message('Valid crafting types: '..gcinclude.Crafting_Types)))
				bOk = false;
			end
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: No craft type specified.')));
			print(chat.header('HandleCommands'):append(chat.message('Correct command: /craftset ['..gcinclude.Crafting_Types..']')));
			bOk = false;
		end

		if bOk then
			gFunc.ForceEquipSet(gcinclude.sets.Crafting);							-- Load the default set
			gcinclude.ProcessConditional(gcinclude.sets.Crafting_Conditional,sKey);	-- Then override w/any conditional that's true
			gcdisplay.SetToggle('GSwap',false);
			toggle = 'Crafting';
			status = gcdisplay.GetToggle('GSwap');
		end
	elseif (args[1] == 'gatherset') then			-- Equips the specified gathering gear and turns off GSWAP
		bOk = true;
		
		if args[2] ~= nil then
			sKey = string.upper(args[2]);
		else
			sKey = nil;
		end
		
		if #args > 1 then
			if string.find(gcinclude.Gathering_Types,sKey) == nil then
				print(chat.header('HandleCommands'):append(chat.message('Error: Invalid gathering type specified: ' ..args[2])));
				print(chat.header('HandleCommands'):append(chat.message('Valid gathering types: '..gcinclude.Gathering_Types)))
				bOk = false;
			end
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: No gathering type specified.')));
			print(chat.header('HandleCommands'):append(chat.message('Correct command: /gatherset ['..gcinclude.Gathering_Types..']')));
			bOk = false;
		end

		if bOk then
			gFunc.ForceEquipSet(gcinclude.sets.Gathering);								-- Load the default set
			gcinclude.ProcessConditional(gcinclude.sets.Gathering_Conditional,sKey);	-- Then override w/any conditional that's true
			gcdisplay.SetToggle('GSwap',false);
			toggle = 'Gathering';
			status = gcdisplay.GetToggle('GSwap');
		end
	elseif (args[1] == 'fishset') then			-- Equips fishing gear and turns GSWAP off
		gFunc.ForceEquipSet(gcinclude.sets.FishingGear);
		gcdisplay.SetToggle('GSwap',false);
		toggle = 'Fishing Set';
		status = gcdisplay.GetToggle('GSwap');
	elseif (args[1] == 'gearset') then			-- Forces a gear set to be loaded and turns GSWAP off
		if #args > 1 then
			gFunc.ForceEquipSet(args[2]);
			if not (#args == 3 and string.lower(args[3]) == 'on') then
				gcdisplay.SetToggle('GSwap',false);
			end
			toggle = 'Gear Swap';
			status = gcdisplay.GetToggle('GSwap');
		else
			print(chat.header('HandleCommands'):append(chat.message('No set specified for /gearset. Command ignored.')));
		end	
	elseif (args[1] == 'maxspell') then			-- Determines highest level spell to cast
		if #args >= 2 then
			gcinclude.MaxSpell(args[2],true);
		end
		toggle = 'MaxSpell';
	elseif (args[1] == 'maxsong') then			-- Determines highest level song to cast
		gcinclude.MaxSong(args[2],(#args > 2),true);
		toggle = 'MaxSong';
	elseif (args[1] == 'region') then			-- Toggles the region setting
		gcdisplay.AdvanceCycle('Region');
		toggle = 'Region';
		status = gcdisplay.GetCycle('Region');
    end

	if gcinclude.settings.Messages then
		gcinclude.Message(toggle, status)
	end
end

--[[
	CheckCommonDebuffs determines if certain debuffs are on the player and loads an appropriate
	gear set. Please note that none of these gear sets will remove the debuff, that would be 
	against the TOS.
--]]

function gcinclude.CheckCommonDebuffs()
	local weakened = gData.GetBuffCount('Weakened');
	local sleep = gData.GetBuffCount('Sleep');
	local blind = gData.GetBuffCount('Blind');
	local doom = (gData.GetBuffCount('Doom'))+(gData.GetBuffCount('Bane'));

	if (sleep >= 1) then 
		gFunc.EquipSet(gcinclude.sets.Sleeping);
		gcinclude.ProcessConditional(gcinclude.sets.Sleeping_Conditional,nil);
	end
	if (doom >= 1) then	
		gFunc.EquipSet(gcinclude.sets.Doomed);
		gcinclude.ProcessConditional(gcinclude.sets.Doomed_Conditional,nil);
	end
	if (weakened >= 1) then
		gFunc.EquipSet(gcinclude.sets.Weakened);
		gcinclude.ProcessConditional(gcinclude.sets.Weakened_Conditional,nil);
	end;
	if (blind >= 1) then
		gFunc.EquipSet(gcinclude.sets.Blind);
		gcinclude.ProcessConditional(gcinclude.sets.Blind_Conditional,nil);
	end
end

--[[
	CheckAbilityRecast determines if an ability can be cast. (It sees if the ability is
	currently cooling down.
--]]

function gcinclude.CheckAbilityRecast(check)
	local RecastTime = 0;

	for x = 0, 31 do
		local id = AshitaCore:GetMemoryManager():GetRecast():GetAbilityTimerId(x);
		local timer = AshitaCore:GetMemoryManager():GetRecast():GetAbilityTimer(x);

		if ((id ~= 0 or x == 0) and timer > 0) then
			local ability = AshitaCore:GetResourceManager():GetAbilityByTimerId(id);
			if ability == nil then return end
			if (ability.Name[1] == check) and (ability.Name[1] ~= 'Unknown') then
				RecastTime = timer;
			end
		end
	end

	return RecastTime;
end

--[[
	SetTownGear will equip the appropriate gear if the player is in a town
--]]

function gcinclude.SetTownGear()
	local zone = gData.GetEnvironment();
	if (zone.Area ~= nil) and (gcinclude.Towns:contains(zone.Area)) then
		gFunc.EquipSet('Town');
		gcinclude.ProcessConditional(gcinclude.sets.Town_Conditional,nil);
	end
end

--[[
	SetRegenRefreshGear equips gear based on whether refresh or regen is indicated
--]]

function gcinclude.SetRegenRefreshGear()
	if gcinclude.settings.AutoGear == false then return end

	local player = gData.GetPlayer();
	local pet = gData.GetPet();
	if (player.Status == 'Idle') then
		if (player.HPP < gcinclude.settings.RegenGearHPP ) then 
			gFunc.ForceEquipSet('Idle_Regen');
		end
		if (gcinclude.settings.bMagic and player.MPP < gcinclude.settings.RefreshGearMPP ) then 
			gFunc.ForceEquipSet('Idle_Refresh');
		end
	end
end

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
	elseif string.find(ws_name,gcinclude.WS_INTAGI) ~= nil then
		ws_stat = 'WS_INTAGI';
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
	else
		ws_stat = 'WS_' .. ws_default;
	end
	return ws_stat;
end

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
end;

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
end

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
end

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
end

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
end

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
end

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
end

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
end

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
end

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
end

--[[
	DoShadows is used by the NIN job. It determines if a higher form of utsesumi is currently active
	and cancels it so the passed utsesumi spell can be cast
--]]

function gcinclude.DoShadows(spell) -- 1000% credit to zach2good for this function, copy and paste (mostly) from his ashita discord post
	if spell.Name == 'Utsusemi: Ichi' then
		local delay = 2.4
		if gData.GetBuffCount(66) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 66') end):once(delay)
		elseif gData.GetBuffCount(444) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 444') end):once(delay)
		elseif gData.GetBuffCount(445) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 445') end):once(delay)
		elseif gData.GetBuffCount(446) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 446') end):once(delay)
		end
	end

	if spell.Name == 'Utsusemi: Ni' then
		local delay = 0.5
		if gData.GetBuffCount(66) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 66') end):once(delay)
		elseif gData.GetBuffCount(444) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 444') end):once(delay)
		elseif gData.GetBuffCount(445) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 445') end):once(delay)
		elseif gData.GetBuffCount(446) == 1 then
			(function() AshitaCore:GetChatManager():QueueCommand(-1, '/cancel 446') end):once(delay)
		end
	end
end

--[[
	CheckCancels determines if the spell that is being cast needs a previous version to be cancelled 
	first and does so as needed
--]]

function gcinclude.CheckCancels()--tossed Stoneskin in here too
	local action = gData.GetAction();
	local sneak = gData.GetBuffCount('Sneak');
	local stoneskin = gData.GetBuffCount('Stoneskin');
	local target = gData.GetActionTarget();
	local me = AshitaCore:GetMemoryManager():GetParty():GetMemberName(0);
	
	local function do_jig()
		AshitaCore:GetChatManager():QueueCommand(1, '/ja "Spectral Jig" <me>');
	end
	local function do_sneak()
		AshitaCore:GetChatManager():QueueCommand(1, '/ma "Sneak" <me>');
	end
	local function do_ss()
		AshitaCore:GetChatManager():QueueCommand(1, '/ma "Stoneskin" <me>');
	end

	if (action.Name == 'Spectral Jig' and sneak ~=0) then
		gFunc.CancelAction();
		AshitaCore:GetChatManager():QueueCommand(1, '/cancel Sneak');
		do_jig:once(2);
	elseif (action.Name == 'Sneak' and sneak ~= 0 and target.Name == me) then
		gFunc.CancelAction();
		AshitaCore:GetChatManager():QueueCommand(1, '/cancel Sneak');
		do_sneak:once(1);
	elseif (action.Name == 'Stoneskin' and stoneskin ~= 0) then
		gFunc.CancelAction();
		AshitaCore:GetChatManager():QueueCommand(1, '/cancel Stoneskin');
		do_ss:once(1);
	end
end

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
		--containerID = gcinclude.STORAGES[tStorage[i]][1];
		containerID = tStorage[i][1];
		
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
						iCnt = itemEntry.Count
						if iCnt ~= nil and iCnt > 0 then
							iCount = iCount + 1;
							if not bFound then
								print(chat.header('findString'):append(chat.message(gcinclude.STORAGES[tStorage[i]][2])));
								bFound = true;
							end
							print(chat.header('findString'):append(chat.message('   ' .. item.Name[1] .. ' ('..tostring(iCnt) .. ')')));
						end
					end
				end
			end
		end
	end

	return (iCount > 0);	
end

--[[
	findMaxEquipablePetFood searches all accessible player storage containers (regardless of location)
	and equips the highest level pet food that can be equipped that's found.
--]]

function gcinclude.findMaxEquipablePetFood()
	
	-- see if any pet food is accessible (inventory, wardrobe, wardrobe 2)
	return gcinclude.findString(gcinclude.EQUIPABLE,'pet f',true,nil);		
end

--[[
	doPetFood does one of two things. It either equips the indicated food or it
	shows where the food can be found. What is equipped will either be indicated or
	the max level pet food that can be equipped.
	
	/petfood [all|max] [name]
--]]

function gcinclude.doPetFood(action, sType)
	local player = gData.GetPlayer();
	local ilvl = 0;
	local sName = nil;
		
	if action == nil then
		sAction = 'max';
	else
		sAction = string.lower(action);
		if not (sAction == 'all' or sAction == 'max') then
			if sType ~= nil then
				print(chat.header('doPetFood'):append(chat.message('Invalid action specified : ' .. action .. '. Ignoring command')));
				return;
			end
		else
			sType = nil;
		end
	end
	
	if sAction == 'all' then
		-- Currently only 1=Inventory,2=Safe,3=storage,6=satchel,9=wardrobe,11=wardrobe 2 are used, but have included all for future expansion
		if not gcinclude.findString({1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17},'pet f',false,sType) then
			print(chat.header('doPetFood'):append(chat.message('No pet food found')));
		end
	else
		if sAction == 'max' then
			if not gcinclude.findMaxEquipablePetFood() then
				print(chat.header('doPetFood'):append(chat.message('No equipable pet food found or found pet food is too high level')));
				return;
			end
		else
			if not gcinclude.findString(gcinclude.EQUIPABLE,sAction,true,nil) then 
				print(chat.header('doPetFood'):append(chat.message(action .. ' not found in accessible storage')));
				return;
			end
		end
		
		-- Now to process what was found
		for k,tpf in pairs(gcinclude.petfood) do
			if tpf[4] and tpf[3] > ilvl and tpf[3] <= player.MainJobLevel then
				iLvl = tpf[3];
				sName = tpf[2];
			end
		end
	end
	if sName ~= nil then
		gFunc.ForceEquip('Ammo', sName);
		print(chat.header('doPetFood'):append(chat.message('Equipping: ' .. sName)));
	end				
end

--[[
	CheckDefaault is just a grouping routine to set common settings
--]]

function gcinclude.CheckDefault()
	gcinclude.SetRegenRefreshGear();
	gcinclude.SetTownGear();
    gcinclude.CheckCommonDebuffs();
	gcdisplay.Update();
end

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
