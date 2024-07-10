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
	['Blind'] = {			-- Equip set if you're blind. Note: BLIND is a conditional too
		Ears = 'Bat Earring',
	},

	['Bound'] = {			-- Equip set if you are bound
	},
	
	['Cursed'] = {			-- Equip set if you are cursed
	},
	
	['Doomed'] = { 			-- Equip set any time you have the doom/bane status
    },
	
	['Holy_Water'] = { 		-- Equip set with whatever gear you use for the Holy Water item
    },

	['Paralyzed'] = {		-- Equip set if you are paralyzed
	},

	['Petrified'] = {		-- Equip set if you are petrified
	},
	
	['Poisoned'] = {		-- Equip set if you are poisoned
	},
	
	['Silenced'] = {		-- Equip set if you are silenced
	},
	
	['Sleeping'] = { 		-- Equip set if you are asleep
		Neck = 'Opo-opo necklace',	-- might as well gain tp
    },
	
	['Weakened'] = {  		-- Equip set if you are weakened
	},
	
	-- Some buffs are not debuffs
	
	['Shining_Ruby'] = {	-- Equip set when you have the shining ruby buff
		--Hands = 'Carbuncle\'s Cuffs',
	},

--[[
	There are currently eight crafts: alchemy, bonecraft, clothcraft, cooking, goldsmithing, leathercraft,
	smithing, and woodworking. It's possible that a player will have gear for more than one craft. There's
	only one Crafting gear set, so you need to qualify each piece with what type of crafting the piece is
	used for. (Ex: Body = 'Weaver\'s Apron//CR:CLOTH).
--]]
	['Crafting'] = {
	},

--[[
	There are seven gathering types: harvesting, excavtion, logging, and mining which are grouped in the H.E.L.M.
	set. The other three types of gathering: digging, clamming and fishing, have their own gear.
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
	The Sneaky set is equipped and /gswap is turned off. It's a set intended to equip gear
	to help the player sneak around.
--]]	
	['Sneaky'] = {
		Hands = 'Dream Mittens +1',
		Feet  = 'Dream Boots +1',
	},

--[[
	The dispense set is used to equip items that have an ability to daily dispense items.
	They're grouped here as a convenience. Like Sneaky, once this set is loaded /gswap
	will be turned off.
--]]
	['Dispense'] = {
		Head = 'Dream Hat +1',
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
	to see how this is done but as an example you can just put 'gcinclude.settings.RefreshGearMPP = 50;' in your 
	job files OnLoad function to modify for that job only
--]]
	Messages = false; 	 -- set to true if you want chat log messages to appear on any /gc command used such as DT, or KITE gear toggles, certain messages will always appear
	WScheck = true; 	 -- set to false if you dont want to use the WSdistance safety check
	WSdistance = 4.7; 	 -- default max distance (yalms) to allow non-ranged WS to go off at if the above WScheck is true
	RegenGearHPP = 60; 	 -- idle regen set gets loaded if player's max HP is <= 60%
	RefreshGearMPP = 70; -- idle refresh set gets loaded if player's max MP <= 70%. Refresh takes priority over regen
	bWSOverride = false; -- is the player playing a job where weapon swapping always happens, it is not optional?
	Tolerance = 97;		 -- Comparison value %, cut-off for certain comparisons
	--
	priorityEngaged = 'BCEFGH'; 	-- indicates order of steps for engagement
	priorityMidCast = 'ABCDEFGH';	-- indicates order of steps for spell midcast
	priorityWeaponSkill = 'ABDE';	-- indicates order of steps for a weapon skill
};

-- The following arrays are used by the functions contained in this file. Probably best to leave them alone

gcdisplay = gFunc.LoadFile('common\\gcdisplay.lua');

gcinclude.AliasList = T{'acc','ajug','db','dt','ei','equipit','eva','gcmessages','gearset','gs','gswap','help','horn','idle','kite','lock','maxsong','maxspell','nac','petfood','rc','sbp','showit','slot','string','tank','th','unlock','wsdistance','wswap','t1'};
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
gcinclude.ExactBuff = T{'enthunder','enstone','enaero','enblizzard','enwater','enlight','endark','arcane circle','holy circle','ward circle'};
gcinclude.Crafting_Types = 'ALC,BONE,CLOTH,COOK,GSM,LTH,BSM,WW';
gcinclude.Gathering_Types = 'HELM,DIG,CLAM,FISH';

--[[
	The following two variables are used to store the invoked type of craft/gather type
--]]
gcinclude.Craft=nil;
gcinclude.Gather=nil;
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

-- define constants for Instrument so typos aren't made
gcinclude.HORN = 'Horn';
gcinclude.STRING = 'String';

-- Define job list that can tank
gcinclude.JobList = 'PLD,NIN,RUN,DRK,WAR,BLU';

-- Define constants dealing with magic gear and jobs
gcinclude.ELEMENT = 'ele';
gcinclude.OBI = 'obi';
gcinclude.sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,SCH,GEO,RUN';
gcinclude.sVisibleGear = 'Main,Sub,Head,Body,Hands,Legs,Feet';

-- The following structure is used for locks and accuracy
gcinclude.Locks = { [1] =  {'main', false,false}, [2] =  {'sub',false,false}, 
					[3] =  {'range',false,false}, [4] =  {'ammo',false,false}, 
					[5] =  {'head',false,false},  [6] =  {'neck',false,false},
					[7] =  {'ear1',false,false},  [8] =  {'ear2',false,false}, 
					[9] =  {'body',false,false},  [10] = {'hands',false,false}, 
					[11] = {'ring1',false,false}, [12] = {'ring2',false,false},
					[13] = {'back',false,false},  [14] = {'waist',false,false},
					[15] = {'legs',false,false},  [16] = {'feet',false,false}};
					
gcinclude.LocksNumeric = 'None';
gcinclude.AccNumeric = 'None';

-- Structure for tracking elemental gear. The "Job" entry is used to make sure the current Job matches
-- the job where the status was recorded.
gcinclude.elemental_gear = T{['job'] = 'NON',
							 ['staff'] = {
									['fire']    = { ['NQ'] = {['Name'] = 'Fire staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Vulcan\'s staff', ['Where'] = nil}, 
													['Searched'] = false },
									['ice']     = { ['NQ'] = {['Name'] = 'Ice staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Aquilo\'s staff', ['Where'] = nil},
													['Searched'] = false },
									['wind']    = { ['NQ'] = {['Name'] = 'Wind staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Auster\'s staff', ['Where'] = nil},
													['Searched'] = false },
									['earth']   = { ['NQ'] = {['Name'] = 'Earth staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Terra\'s staff', ['Where'] = nil},
													['Searched'] = false },
									['thunder'] = { ['NQ'] = {['Name'] = 'Thunder staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Jupiter\'s staff', ['Where'] = nil},
													['Searched'] = false },
									['water']   = { ['NQ'] = {['Name'] = 'Water staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Neptune\'s staff', ['Where'] = nil},
													['Searched'] = false },
									['light']   = { ['NQ'] = {['Name'] = 'Light staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Apollo\'s staff', ['Where'] = nil},
													['Searched'] = false },
									['dark']    = { ['NQ'] = {['Name'] = 'Dark staff', ['Where'] = nil},
												    ['HQ'] = {['Name'] = 'Pluto\'s staff', ['Where'] = nil},
													['Searched'] = false } 
									},
													
							 ['obi'] = {
									['fire']    = { ['Name'] = 'Karin obi', ['Where'] = nil, ['Searched'] = false },
									['ice']     = { ['Name'] = 'Hyorin obi', ['Where'] = nil, ['Searched'] = false },												 
									['wind']    = { ['Name'] = 'Furin obi', ['Where'] = nil, ['Searched'] = false },												 
									['earth']   = { ['Name'] = 'Dorin obi', ['Where'] = nil, ['Searched'] = false },
									['thunder'] = { ['Name'] = 'Rairin obi', ['Where'] = nil, ['Searched'] = false },
									['water']   = { ['Name'] = 'Suirin obi', ['Where'] = nil, ['Searched'] = false },
									['light']   = { ['Name'] = 'Korin obi', ['Where'] = nil, ['Searched'] = false },
									['dark']    = { ['Name'] = 'Anrin obi', ['Where'] = nil, ['Searched'] = false }
									},
									
							 ['gorget'] = {
									['fire']    = { ['Name'] = 'Flame gorget', ['Where'] = nil, ['Searched'] = false },
									['ice']     = { ['Name'] = 'Snow gorget', ['Where'] = nil, ['Searched'] = false },
									['wind']    = { ['Name'] = 'Breeze gorget', ['Where'] = nil, ['Searched'] = false },
									['earth']   = { ['Name'] = 'Soil gorget', ['Where'] = nil, ['Searched'] = false },
									['thunder'] = { ['Name'] = 'Thunder gorget', ['Where'] = nil, ['Searched'] = false },
									['water']   = { ['Name'] = 'Aqua gorget', ['Where'] = nil, ['Searched'] = false },
									['light']   = { ['Name'] = 'Light gorget', ['Where'] = nil, ['Searched'] = false },
									['dark']    = { ['Name'] = 'Shadow gorget', ['Where'] = nil, ['Searched'] = false }
									} 
};
--[[
	The "root" of spells is the first word in the spell name, all in lower case. 
	
	The following lists all "root" spells whose Magical Accuracy can be affected by day/weather
--]]

gcinclude.MagicEleAcc = T{
	['fire']    = 'burn,firaga,fire,flare,blaze',
    ['water']   = 'drown,flood,water,waterga,poison',
    ['wind']    = 'choke,aero,aeroga,tornado,silence,gravity,flurry',
    ['thunder'] = 'shock,burst,thundaga,thunder,Stun',
    ['earth']   = 'rasp,quake,stone,stonega,slow',
    ['ice']     = 'frost,blizzaga,blizzard,freeze,paralyze,bind,distract,ice',
    ['light']   = 'banish,banishga,dia,diaga,flash,repose,holy,auspice,esuna,sacrifice,reprisal,cure,curaga,cura',
    ['dark']    = 'blind,bio,sleep,dispel,frazzle,drain,warp,tractor,aspir,escape,sleepga,retrace,absorb-mnd,absorb-chr,absorb-vit,absorb-agi,absorb-int,absorb-dex,absorb-str'
};
	
-- The following lists all "root" spells that are elemental in nature and can affect Elemental Damage 
-- by the day/weather
	
gcinclude.MagicEleDmg = T{
	['fire']    = 'firaga,fire,flare',
	['water']   = 'flood,water,waterga',
	['wind']    = 'aero,aeroga,tornado',
	['thunder'] = 'burst,thundaga,thunder',
	['earth']   = 'quake,stone,stonega',
	['ice']     = 'blizzaga,blizzard,freeze',
	['light']   = 'banish,banishga,dia,diaga,holy,auspice,esuna,sacrifice,phalanx,refresh,reprisal,cure,curaga,cura',
	['dark']    = 'blind,bio,poison,sleep,dispel,frazzle,drain,aspir,escape,sleepga,retrace,absorb-mnd,absorb-chr,absorb-vit,absorb-agi,absorb-int,absorb-dex','absorb-str'
};

-- Listed below are all the spells that are affected by INT or MND
gcinclude.StatMagic = T{
	['int'] = {'INT','aero,aeroga,bind,blaze,blind,blizzaga,blizzard,burst,dread,firaga,fire,flare,flood,freeze,ice,quake,shock,stone,stonega,thundaga,thunder,tornado,water,waterga'},
	['mnd'] = {'MND','banish,distract,frazzle,paralyze,slow,cure,curaga,cura'},
};

-- List of summons
gcinclude.SummonSkill = 'carbuncle,fenrir,ifrit,titan,leviathan,garuda,shiva,ramuh,diabolos,fire,firespirit,ice,icespirit,air,airspirit,earth,earthspirit,thunder,thunderspirit,water,waterspirit,light,lightspirit,dark,darkspirit,cait,caitsith,siren,atomos,alexander,odin';

-- The following lists all "base" songs that are elemental in nature and can affect Elemental Damage 
-- by the day/weather

gcinclude.SongEleDmg = T{
	['fire']    = 'valor minuet,ice threnody,sinewy etude,ice carol,herculean etude',
	['water']   = 'fire threnody,spirited etude,fire carol,logical etude',
	['wind']    = 'sheepfoe mambo,earth threnody,quick etude,raptor mazurka,earth carol,dragonfoe mambo,gold capricco,swift etude,chocobo mazurka',
	['thunder'] = 'herb pastoral,sword madrigal,water threnody,advancing march,hunter\'s prelude,dextrous etude,water carol,blade madrigal,victory march.archer\'s prelude,uncanny etude',
	['earth']   = 'knight\'s minne,lightning threnody,vivacious etude,Battlefield elegy,carrnage elegy,vital etude',
	['ice']     = 'wind threnody,scop\'s operetta,learned etude,wind carol,sage etude,puppet\'s operetta',
	['light']   = 'army\'s paeon,foe lullaby,dark threnody,foe lullaby,enchanting etude,mage\'s ballad,horde lullaby,fowl aubade,magic finale,lightning carol,dark carol,shining fantasia,bewitching etude,goddess hymnus,warding round,maiden\'s virelai',
	['dark']    = 'light threnody,light carol,goblin gavotte',
};
	
-- List of elemental spirit avatars
gcinclude.Spirits = 'fire,firespirit,ice,icespirit,air,airspirit,earth,earthspirit,thunder,thunderspirit,water,waterspirit,light,lightspirit,dark,darkspirit';

-- This table associates a summoned avatar with an element so that the appropriate stave can be equipped
gcinclude.SummonStaves = T{
	['carbuncle'] = 'light', ['light spirit'] = 'light', ['lightspirit'] = 'light', ['cait sith'] = 'light', ['caitsith'] = 'light', ['alexander'] = 'light',
	['fenrir']    = 'dark', ['diabolos'] = 'dark', ['darks pirit'] = 'dark', ['darkspirit'] = 'dark', ['atomos'] = 'dark', ['odin'] = 'dark',
	['ifrit']     = 'fire', ['fire spirit'] = 'fire', ['firespirit'] = 'fire',
	['titan']     = 'earth', ['earth spirit'] = 'earth', ['earthspirit'] = 'earth',
	['leviathan'] = 'water', ['water spirit'] = 'water', ['waterspirit'] = 'water',
	['garuda']    = 'wind', ['air spirit'] = 'wind', ['airspirit'] = 'wind', ['siren'] = 'wind',
	['shiva']     = 'ice', ['ice spirit'] = 'ice', ['icespirit'] = 'ice',
	['ramuh']     = 'thunder', ['thunder spirit'] = 'thunder', ['thunderspirit'] = 'thunder'
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
gcinclude.GearWarnings = '';

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
	['emp'] = {'Empress Band','Ring',nil},
	['cha'] = {'Chariot Band','Ring',nil},
	['empo'] = {'Emperor Band','Ring',nil},
	['ann'] = {'Anniversary Ring','Ring',nil},
	['dem'] = {'Dem Ring','Ring',nil},
	['mea'] = {'Mea Ring','Ring',nil},
	['holla'] = {'Holla Ring','Ring',nil},
	['altep'] = {'Altepa Ring','Ring',nil},
	['yhoat'] = {'Yhoat Ring','Ring',nil},
	['vahzl'] = {'Vahzl Ring','Ring',nil},
	['home'] = {'Homing Ring','Ring',nil},
	['ret'] = {'Return Ring','Ring',nil},
	['warp'] = {'Warp Ring','Ring',nil},
	['tav'] = {'Tavnazian Ring','Ring',nil},
	['dcl'] = {'Dcl.Grd. Ring','Ring',nil},
	['warpc'] = {'Warp Cudgel','Main',nil},
	['trick2'] = {'Trick Staff II','Main',nil},
	['treat2'] = {'Treat Staff II','Main',nil},
	['fork1'] = {'Pitchfork +1','Main',nil},
	['purgo'] = {'Wonder Top +1','Body',nil},
	['rre'] = {'Reraise Earring','Ear',nil},
	['mandy'] = {'Mandra. Suit','Body','Body,Legs'},
};

-- This is the list of storage containers that can be equipped from outside of a moghouse
gcinclude.EQUIPABLE = {gcinclude.STORAGES[1],		-- Inventory
					   gcinclude.STORAGES[9],		-- Wardrobe
					   gcinclude.STORAGES[11],		-- Wardrobe 2
					   gcinclude.STORAGES[17]};		-- Wardrobe 8

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

-- Structure used to tally the MP +/-, HP +/-, and convert affects for all gear
-- whether invisible or visible.
TallyGear = {
	Visible      = { 'Main','Sub','Range','Ammo','Head','Body','Hands','Legs','Feet' },
	Invisible    = { 'Neck','Ear1','Ear2','Ring1','Ring2','Back','Waist' },
	VMP			 = 0,		-- Tally of MP +/- from visible gear
	IMP			 = 0,		-- Tally of MP +/- from invisible gear
	VMPP		 = 0,		-- Tally of MP +/- % from visible gear
	IMPP		 = 0,		-- Tally of MP +/- % from invisible gear
	VHP			 = 0,		-- Tally of HP +/- from visible gear
	IHP			 = 0,		-- Tally of HP +/- from invisible gear
	VHPP		 = 0,		-- Tally of HP +/- % from visible gear
	IHPP		 = 0,		-- Tally of HP +/- % from invisible gear
	VCHPMP 		 = 0,		-- Tally of Convert HP to MP from visible gear
	ICHPMP		 = 0,		-- Tally of Convert HP to MP from invisible gear
	VCMPHP		 = 0,		-- Tally of Convert MP to HP from visible gear
	ICMPHP		 = 0,		-- Tally of Convert MP to HP from invisible gear
};

-- The following is a list of all valid conditional operators used in the following
-- inline codes: //MP, //MPP, //HP, //HPP, //TP, and //TPP.
InlineConditionals = { '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.' };

-- The following is used to track regional control. Listed is a region, who has
-- conquest control, and what zone id's are associated with the region. This
-- structure is populated programmatically. 1 - San d'Orian, 2 - Bastokian, 3 -
-- Windurstian, 0 - not applicable, -1 unassigned.
gcinclude.RegionControl = {
	['Argoneau'] 		  = {-1, {152,7,8,151,200,119,120}},
	['Bastok'] 			  = { 2, {234,235,236,237}},
	['Derfland']		  = {-1, {147,197,109,148,110}},
	['ElshimoLowlands']	  = {-1, {250,252,176,123}},
	['ElshimoUplands']	  = {-1, {207,211,160,205,163,159,124}},
	['Fauregandi']		  = {-1, {111,203,204,9,206,166,10}},
	['Gustaberg']		  = {-1, {191,173,106,143,107,144,172}},
	['Jeuno']			  = { 0, {243,244,245,246}},
	['Kolshushu']		  = {-1, {4,118,213,3,198,249,117}},	-- Purgonorgo Isle doesn't have a separate ID
	['Kuzotz']			  = {-1, {209,114,168,208,247,125}},
	['LiTelor']			  = {-1, {153,202,154,251,122,121}},
	['Movapolos']		  = {-1, {13,12,11}},
	['Norvallen']		  = {-1, {105,104,2,150,149,1,195}},
	['QuifimIsland']	  = {-1, {127,184,157,126,179,158}},
	['Ronfaure']		  = {-1, {167,101,141,140,139,190,100,142}},
	['Sandoria']		  = { 1, {230,231,232,233}},
	['Sarutabaruta']	  = {-1, {146,116,170,145,192,194,169,115}},
	['Tavnazia']		  = {-1, {24,25,31,27,30,29,28,32,26}},
	['Tulia']			  = {-1, {181,180,130,178,177}},
	['Valdeaunia']		  = {-1, {6,161,162,165,5,112}},
	['Vollbow']			  = {-1, {113,201,212,174,128}},
	['Windurst']		  = { 3, {238,239,240,241,242}},
	['Zulkheim']		  = {-1, {196,108,102,193,248,103}},
	['Dynamis']			  = { 0, {39,40,41,42,134,135,185,186,187,188}},
	['Lumoria']			  = { 0, {33,34,35,36,37,38}}
};

gcinclude.OwnNation = -1; 

gcinclude.Sets = gcinclude.sets;

--[[
	The following event is used to capture the ownership of the regions.
	Conquest updates are sent whenever the player zones and periodiaclly.
	The display bar's region is updated accordingly
--]]

ashita.events.register('packet_in', 'packet_in_callback1', function (e)

	if (e.id == 0x05E) then
		gcinclude.RegionControl['Ronfaure'][1] = struct.unpack('B', e.data, 0X1E)
		gcinclude.RegionControl['Zulkheim'][1] = struct.unpack('B', e.data, 0x22)
		gcinclude.RegionControl['Norvallen'][1] = struct.unpack('B', e.data, 0x26)
		gcinclude.RegionControl['Gustaberg'][1] = struct.unpack('B', e.data, 0x2A)
		gcinclude.RegionControl['Derfland'][1] = struct.unpack('B', e.data, 0x2E)
		gcinclude.RegionControl['Sarutabaruta'][1] = struct.unpack('B', e.data, 0x32)
		gcinclude.RegionControl['Kolshushu'][1] = struct.unpack('B', e.data, 0x36)
		gcinclude.RegionControl['Argoneau'][1] = struct.unpack('B', e.data, 0x3A)
		gcinclude.RegionControl['Fauregandi'][1] = struct.unpack('B', e.data, 0x3E)
		gcinclude.RegionControl['Valdeaunia'][1] = struct.unpack('B', e.data, 0x42)
		gcinclude.RegionControl['QuifimIsland'][1] = struct.unpack('B', e.data, 0x46)
		gcinclude.RegionControl['LiTelor'][1] = struct.unpack('B', e.data, 0x4A)
		gcinclude.RegionControl['Kuzotz'][1] = struct.unpack('B', e.data, 0x4E)
		gcinclude.RegionControl['Vollbow'][1] = struct.unpack('B', e.data, 0x52)
		gcinclude.RegionControl['ElshimoLowlands'][1] = struct.unpack('B', e.data, 0x56)
		gcinclude.RegionControl['ElshimoUplands'][1] = struct.unpack('B', e.data, 0x5A)
		gcinclude.RegionControl['Tulia'][1] = struct.unpack('B', e.data, 0x5E)
		gcinclude.RegionControl['Movapolos'][1] = struct.unpack('B', e.data, 0x62)
		gcinclude.RegionControl['Tavnazia'][1] = struct.unpack('B', e.data, 0x66)
		gcinclude.RegionDisplay();
		e.blocked = false;
	end
end);


--[[
	RegionDisplay determines if the player's nation owns the area the character is in
	or not and updates the display bar accordingly.
--]]
function gcinclude.RegionDisplay()
	local zoneId = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);
	
	-- Make sure the player's nation is known
	if gcinclude.OwnNation == -1 then
		gcinclude.OwnNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation() + 1;
	end

	-- Determine if current zone in region controlled by player's nation
	for i,j in pairs(gcinclude.RegionControl) do
		if table.find(j[2],zoneId) ~= nil then
			if j[1] == gcinclude.OwnNation then
				gcdisplay.SetCycle('Region','Owned');
			else
				gcdisplay.SetCycle('Region','Not Owned');
			end
			break;
		end
	end
end

--[[
	DB_ShowIt will display debug details
--]]

function gcinclude.DB_ShowIt()
	local player = gData.GetPlayer();
	
	print(chat.message(' '));
	print(chat.message('Settings'));
	print(chat.message('--------'));
	print(chat.message('Job: ' .. player.MainJob .. '/' .. player.SubJob));
	print(chat.message('Level: ' .. tostring(player.MainJobSync) .. '(' .. tostring(player.MainJobLevel) .. ')'));	
	print(chat.message(' '));
	print(chat.message('WScheck: ' .. tostring(gcinclude.settings.WScheck)));
	print(chat.message('WSdistance: ' .. tostring(gcinclude.settings.WSdistance)));
	print(chat.message('RegenGearHPP: ' .. tostring(gcinclude.settings.RegenGearHPP)));
	print(chat.message('RefreshGearMPP: ' .. tostring(gcinclude.settings.RefreshGearMPP)));
	print(chat.message('MagicJob: ' .. tostring(gcinclude.MagicalJob('S'))));
	print(chat.message('bWSOverride: ' .. tostring(gcinclude.settings.bWSOverride)));
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
	GetLockedList returns a comma delimited list or nil if all unlocked
--]]

function gcinclude.GetLockedList(sTarget)
	local sList = nil;
	local iOffset;
	
	if sTarget == 'acc' then
		iOffset = 1;
	else
		iOffset = 0;
	end

	for i,j in ipairs(gcinclude.Locks) do
		if j[2+iOffset] == true then
			if sList == nil then		
				sList = gData.Constants.EquipSlotNames[gData.Constants.EquipSlotsLC[j[1]]];
				if sTarget == 'locks' then
					gcinclude.LocksNumeric = tostring(i);
				else
					gcinclude.AccNumeric = tostring(i);
				end
			else	
				sList = sList .. ', ' .. gData.Constants.EquipSlotNames[gData.Constants.EquipSlotsLC[j[1]]];
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
end		-- gcinclude.GetLockedList

--[[
	LockUnlock either locks or unlocks the specified (or all) slots or enables/disables
	the specified slots for accuracy. Supported are either the slot name or the slot 
	number.
--]]

function gcinclude.LockUnlock(sTarget,sType,sWhich)
	local iOffset;
	
	if sWhich == nil then
		return;
	end 
	
	-- Determine field to address
	if sTarget == 'acc' then
		iOffset = 1;
	else
		iOffset = 0;
	end
	
	sWhich = ',' .. string.lower(sWhich) .. ',';
	for k,l in ipairs(gcinclude.Locks) do
		local sk = ',' .. tostring(k) .. ',';
		if (sWhich == ',all,') or (string.find(sWhich,l[1]) ~= nil) or (string.find(sWhich,sk) ~= nil) then
			gcinclude.Locks[k][2+iOffset] = (string.lower(sType) == 'lock');
		end
	end
	
	-- Special case for ears and rings
	for i=1,16,1 do
		if string.find(sWhich,'ears') and string.sub(gcinclude.Locks[i][1],1,-2) == 'ear' then
			gcinclude.Locks[i][2+iOffset] = (string.lower(sType) == 'lock');
		elseif string.find(sWhich,'rings') and string.sub(gcinclude.Locks[i][1],1,-2) == 'ring' then
			gcinclude.Locks[i][2+iOffset] = (string.lower(sType) == 'lock');
		end
	end
end		-- gcinclude.LockUnlock

--[[
	InitializeEleStructure initializes the elemental gear structure
--]]

function InitializeEleStructure()
	local player = gData.GetPlayer();
	
	-- Job
	gcinclude.elemental_gear['Job'] = player.MainJob;

	-- Staves
	gcinclude.elemental_gear['staff']['fire']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['fire']['HQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['ice']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['ice']['HQ']['Where'] = nil;	
	gcinclude.elemental_gear['staff']['wind']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['wind']['HQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['earth']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['earth']['HQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['thunder']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['thunder']['HQ']['Where'] = nil;	
	gcinclude.elemental_gear['staff']['water']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['water']['HQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['light']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['light']['HQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['dark']['NQ']['Where'] = nil;
	gcinclude.elemental_gear['staff']['dark']['HQ']['Where'] = nil;

	gcinclude.elemental_gear['staff']['fire']['searched'] = false;
	gcinclude.elemental_gear['staff']['ice']['searched'] = false;
	gcinclude.elemental_gear['staff']['wind']['searched'] = false;
	gcinclude.elemental_gear['staff']['earth']['searched'] = false;
	gcinclude.elemental_gear['staff']['thunder']['searched'] = false;
	gcinclude.elemental_gear['staff']['water']['searched'] = false;
	gcinclude.elemental_gear['staff']['light']['searched'] = false;
	gcinclude.elemental_gear['staff']['dark']['searched'] = false;	

	-- Obis
	gcinclude.elemental_gear['obi']['fire']['Where'] = nil;
	gcinclude.elemental_gear['obi']['ice']['Where'] = nil;
	gcinclude.elemental_gear['obi']['wind']['Where'] = nil;
	gcinclude.elemental_gear['obi']['earth']['Where'] = nil;
	gcinclude.elemental_gear['obi']['thunder']['Where'] = nil;
	gcinclude.elemental_gear['obi']['water']['Where'] = nil;
	gcinclude.elemental_gear['obi']['light']['Where'] = nil;
	gcinclude.elemental_gear['obi']['dark']['Where'] = nil;

	gcinclude.elemental_gear['obi']['fire']['searched'] = false;
	gcinclude.elemental_gear['obi']['ice']['searched'] = false;
	gcinclude.elemental_gear['obi']['wind']['searched'] = false;
	gcinclude.elemental_gear['obi']['earth']['searched'] = false;
	gcinclude.elemental_gear['obi']['thunder']['searched'] = false;
	gcinclude.elemental_gear['obi']['water']['searched'] = false;
	gcinclude.elemental_gear['obi']['light']['searched'] = false;
	gcinclude.elemental_gear['obi']['dark']['searched'] = false;	
	
	-- Gorgets
	gcinclude.elemental_gear['gorget']['fire']['Where'] = nil;
	gcinclude.elemental_gear['gorget']['ice']['Where'] = nil;
	gcinclude.elemental_gear['gorget']['wind']['Where'] = nil;
	gcinclude.elemental_gear['gorget']['earth']['Where'] = nil;
	gcinclude.elemental_gear['gorget']['thunder']['Where'] = nil;
	gcinclude.elemental_gear['gorget']['water']['Where'] = nil;
	gcinclude.elemental_gear['gorget']['light']['Where'] = nil;
	gcinclude.elemental_gear['gorget']['dark']['Where'] = nil;
	
	gcinclude.elemental_gear['gorget']['fire']['searched'] = false;
	gcinclude.elemental_gear['gorget']['ice']['searched'] = false;
	gcinclude.elemental_gear['gorget']['wind']['searched'] = false;
	gcinclude.elemental_gear['gorget']['earth']['searched'] = false;
	gcinclude.elemental_gear['gorget']['thunder']['searched'] = false;
	gcinclude.elemental_gear['gorget']['water']['searched'] = false;
	gcinclude.elemental_gear['gorget']['light']['searched'] = false;
	gcinclude.elemental_gear['gorget']['dark']['searched'] = false;	
end			-- InitializeEleStructure

--[[
	CheckForEleGear determines if the player has accessible the piece of gear indicated by type
--]]

function gcinclude.CheckForEleGear(sType,sElement)
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local player = gData.GetPlayer();
	local tStorage = gcinclude.EQUIPABLE;
	
	-- Make sure structure initialized for the right job
	if player.MainJob ~= gcinclude.elemental_gear['job'] then
		InitializeEleStructure();
	-- Check to see if that Type/Element has been searched for already
	elseif gcinclude.elemental_gear[sType]['Searched'] == true then
		-- For staff, check for HQ before looking at NQ
		if sType == 'staff' then
			if gcinclude.elemental_gear[sType][sElement]['HQ']['Where'] ~= nil then
				return gcinclude.elemental_gear[sType][sElement]['HQ']['Name'];
			elseif gcinclude.elemental_gear[sType][sElement]['NQ']['Where'] ~= nil then
				return gcinclude.elemental_gear[sType][sElement]['NQ']['Name'];
			else
				return nil;
			end
		-- Obi and Gorget can be handled by one routine. Return the name if
		-- the location is identified
		else
			if gcinclude.elemental_gear[sType][sElement]['Where'] ~= nil then
				return gcinclude.elemental_gear[sType][sElement]['Name'];
			else
				return nil;
			end
		end
	end

	-- If you got here, then the sType/sElement combo has not been searched for yet
	-- Loop through all the accessible storages
	for i = 1,#tStorage,1 do
		containerID = tStorage[i][1];
		
		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			local itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                local item = resources:GetItemById(itemEntry.Id);
				local sIN = string.lower(item.Name[1]);
	
				-- Then, depending on type, see if the item has been found
				if sType == 'staff' then
					if sIN == string.lower(gcinclude.elemental_gear['staff'][sElement]['HQ']['Name']) then
						gcinclude.elemental_gear['staff'][sElement]['HQ']['Where'] = tStorage[i][2];
					elseif sIN == string.lower(gcinclude.elemental_gear['staff'][sElement]['NQ']['Name']) then
						gcinclude.elemental_gear['staff'][sElement]['NQ']['Where'] = tStorage[i][2];
					end	
				elseif sType == 'obi' then
					if sIN == string.lower(gcinclude.elemental_gear['obi'][sElement]['Name']) then
						gcinclude.elemental_gear['obi'][sElement]['Where'] = tStorage[i][2];
					end
				elseif sType == 'gorget' then
					if sIN == string.lower(gcinclude.elemental_gear['gorget'][sElement]['Name']) then
						gcinclude.elemental_gear['gorget'][sElement]['Where'] = tStorage[i][2];
					end				
				end
			end
		end
	end

	-- Here's where you mark that the item has been searched for. Returned is whether
	-- it was found or nil
	if sType == 'staff' then
		gcinclude.elemental_gear['staff'][sElement]['Searched'] = true;
		if gcinclude.elemental_gear['staff'][sElement]['HQ']['Where'] ~= nil then
			return gcinclude.elemental_gear['staff'][sElement]['HQ']['Name'];
		elseif gcinclude.elemental_gear['staff'][sElement]['NQ']['Where'] ~= nil then
			return gcinclude.elemental_gear['staff'][sElement]['NQ']['Name'];
		else
			return nil;
		end
	else	-- handles both "obi" and "gorget"
		gcinclude.elemental_gear[sType][sElement]['Searched'] = true;
		if gcinclude.elemental_gear[sType][sElement]['Where'] ~= nil then
			return gcinclude.elemental_gear[sType][sElement]['Name'];
		else
			return nil;
		end
	end
end		-- gcinclude.CheckForEleGear

--[[
	SetVariables defines run settings for luashitacast
--]]

function gcinclude.SetVariables()
	local player = gData.GetPlayer();

	-- General toggles
	gcdisplay.CreateToggle('GSwap', true);
	gcdisplay.CreateToggle('Kite', false);
	gcdisplay.CreateToggle('Eva', false);
	
	gcdisplay.CreateToggle('WSwap',(string.find('WHM,BRD',player.MainJob) ~= nil));

	-- Job specific toggles	
	if string.find('PLD,NIN,RUN',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',true);
		gcdisplay.CreateToggle('Idle',true);
	elseif string.find('DRK,WAR,THF,RDM,BLU',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',false);
		gcdisplay.CreateToggle('Idle',true);
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
	gcdisplay.CreateCycle('Region', {[1] = 'Owned', [2] = 'Not Owned'});
end		-- gcinclude.SetVariables

--[[
	isPetNamed determines if that passed pet has the passed name
--]]

function gcinclude.isPetNamed(sName)
	local pet = gData.GetPet();

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
end		-- gcinclude.isPetNamed

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
end		-- gcinclude.CheckPartyJob

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
	MagicalJob just determines if the main job or the sub job can do magic
--]]

function gcinclude.MagicalJob(sWhich)
	local player = gData.GetPlayer();
	local mj = player.MainJob;
	local sj = player.SubJob;
	local sList = gcinclude.sMagicJobs;
	
	if string.lower(sWhich) == 'T' then
		sList = gcinclude.TieredMagicJobs;
	end
	
	return (string.find(sList,mj) ~= nil or string.find(sList,sj) ~= nil);
end		-- gcinclude.MagicalJob

--[[
	ClearTallyGear zeroes out the tally indicators of the TallyGear structure
--]]

function ClearTallyGear()
	TallyGear.VMP    = 0;
	TallyGear.IMP    = 0;
	TallyGear.VMPP   = 0;
	TallyGear.IMPP   = 0;
	TallyGear.VHP    = 0;
	TallyGear.IHP    = 0;
	TallyGear.VHPP   = 0;
	TallyGear.IHPP   = 0;
	TallyGear.VCHPMP = 0;
	TallyGear.ICHPMP = 0;
	TallyGear.VCMPHP = 0;
	TallyGear.ICMPHP = 0;
end		-- ClearTallyGear

--[[
	TallyMP tallies all the MP increases and decreases across both visible and
	invisible gear. This include raw +'s and -'s and percentages. The appropriate
	fields in the TallyGear structure are updated if anything found.
--]]

function TallyMP()
	local cur = gData.GetEquipment();
	local player = gData.GetPlayer();
	local iposMP,iposMP;
	local ioff,bPlus;
	local sType,ival;
	local iStart,iEnd;
		
	for ii,jj in pairs(cur) do
		-- determine which grouping the current gear piece is in
		
		if table.find(TallyGear.Visible,ii) ~= nil then
			sType = 'V';
		elseif table.find(TallyGear.Invisible,ii) ~= nil then
			sType = 'I';
		else		-- This should never occur!!!
			print(chat.header('TallyMP'):append(chat.message('Error: Unrecognized Slot: ' .. ii .. '. Ignoring.')));
			sType = nil;
		end
		
		-- First deal with exceptions: Check for Tamas ring first
		if jj.Resource.Name[1] == 'Tamas Ring' then
			ival = math.floor((player.MainJobSync - 30)/15)*5 + 15;
			TallyGear.IMP = TallyGear.IMP + ival;
		else
			-- Now determine if there's an MP +/- qualifier in the description.
			-- bPlus tracks if the value is positive or negative. ioff tracks the
			-- offset. Since both + and - are basically found in the same manner,
			-- common code is used.
			bPlus = nil;
			ioff = 0;	
			iposMP = string.find(jj.Resource.Description[1],'MP%+');
			if iposMP ~= nil then
				bPlus = true;
				ioff = 3;
			end
		
			if bPlus == nil then
				iposMP = string.find(jj.Resource.Description[1],'MP%-');

				if iposMP ~= nil then
					bPlus = false;
					ioff = 3;
				end
			end
			
			-- If bPlus is true or false, an MP+/- was found and all that has to be
			-- determined is if the value is a percentage or a flat value
			if bPlus ~= nil then
				ipct = string.find(jj.Resource.Description[1],'%%',iposMP+ioff);		
				if ipct ~= nil and ipct - (iposMP+ioff) <= 2 then
					-- It's a percentage. In the 75 era, percentages are only
					-- single digits for MP
					ival = tonumber(string.sub(jj.Resource.Description[1],iposMP+ioff,ipct-1));
					if bPlus == true then
						-- MP+#%
						if sType == 'V' then
							TallyGear.VMPP = TallyGear.VMPP + ival;
						elseif sType == 'I' then
							TallyGear.IMPP = TallyGear.IMPP + ival;
						end
					elseif bPlus == false then
						-- MP-#%
						if sType == 'V' then
							TallyGear.VMPP = TallyGear.VMPP - ival;
						elseif sType == 'I' then
							TallyGear.IMPP = TallyGear.IMPP - ival;
						end
					end
				else
					-- Flat value
					iStart = iposMP + ioff;
					if iStart == string.len(jj.Resource.Description[1]) or
						string.find(string.sub(jj.Resource.Description[1],iStart+1,iStart+1),'%d') == nil then
						iEnd = iStart;
					else
						iEnd = iStart + 2;
					end
						
					ival = tonumber(string.sub(jj.Resource.Description[1],iStart,iEnd));

					if bPlus == true then
						-- MP +###
						if sType == 'V' then
							TallyGear.VMP = TallyGear.VMP + ival;
						elseif sType == 'I' then
							TallyGear.IMP = TallyGear.IMP + ival;
						end
					elseif bPlus == false then
						-- MP -###
						if sType == 'V' then
							TallyGear.VMP = TallyGear.VMP - ival;
						elseif sType == 'I' then
							TallyGear.IMP = TallyGear.IMP - ival;
						end
					end					
				end
			end
		end
	end
end		-- TallyMP

--[[
	TallyHP tallies all the HP increases and decreases across both visible and
	invisible gear. This include raw +'s and -'s and percentages. The appropriate
	fields in the TallyGear structure are updated if anything found.
--]]

function TallyHP()
	local cur = gData.GetEquipment();
	local player = gData.GetPlayer();
	local iposHP,iposHP;
	local ioff,bPlus;
	local sType,ival;
	local iStart,iEnd;
		
	for ii,jj in pairs(cur) do
		-- determine which grouping the current gear piece is in
		
		if table.find(TallyGear.Visible,ii) ~= nil then
			sType = 'V';
		elseif table.find(TallyGear.Invisible,ii) ~= nil then
			sType = 'I';
		else		-- This should never occur!!!
			print(chat.header('TallyMP'):append(chat.message('Error: Unrecognized Slot: ' .. ii .. '. Ignoring.')));
			sType = nil;
		end
		
		-- First deal with exceptions: Check for Tamas ring first
		if jj.Resource.Name[1] == 'Sattva Ring' then
			ival = math.floor((player.MainJobSync - 30)/15)*5 + 15;
			TallyGear.IHP = TallyGear.IHP + ival;
		else
			-- Now determine if there's an HP +/- qualifier in the description.
			-- bPlus tracks if the value is positive or negative. ioff tracks the
			-- offset. Since both + and - are basically found in the same manner,
			-- common code is used.
			bPlus = nil;
			ioff = 0;	
			iposHP = string.find(jj.Resource.Description[1],'HP%+');
			if iposHP ~= nil then
				bPlus = true;
				ioff = 3;
			end
		
			if bPlus == nil then
				iposHP = string.find(jj.Resource.Description[1],'HP%-');

				if iposHP ~= nil then
					bPlus = false;
					ioff = 3;
				end
			end
			
			-- If bPlus is true or false, an HP+/- was found and all that has to be
			-- determined is if the value is a percentage or a flat value
			if bPlus ~= nil then
				ipct = string.find(jj.Resource.Description[1],'%%',iposHP+ioff);		
				if ipct ~= nil and ipct - (iposHP+ioff) <= 2 then
					-- It's a percentage. In the 75 era, percentages are only
					-- single digits for HP
					ival = tonumber(string.sub(jj.Resource.Description[1],iposHP+ioff,ipct-1));
					if bPlus == true then
						-- HP+#%
						if sType == 'V' then
							TallyGear.VHPP = TallyGear.VHPP + ival;
						elseif sType == 'I' then
							TallyGear.IHPP = TallyGear.IHPP + ival;
						end
					elseif bPlus == false then
						-- HP-#%
						if sType == 'V' then
							TallyGear.VHPP = TallyGear.VHPP - ival;
						elseif sType == 'I' then
							TallyGear.IHPP = TallyGear.IHPP - ival;
						end
					end
				else
					-- Flat value
					iStart = iposHP + ioff;
					if iStart == string.len(jj.Resource.Description[1]) or
						string.find(string.sub(jj.Resource.Description[1],iStart+1,iStart+1),'%d') == nil then
						iEnd = iStart;
					else
						iEnd = iStart + 2;
					end
						
					ival = tonumber(string.sub(jj.Resource.Description[1],iStart,iEnd));

					if bPlus == true then
						-- HP +###
						if sType == 'V' then
							TallyGear.VHP = TallyGear.VHP + ival;
						elseif sType == 'I' then
							TallyGear.IHP = TallyGear.IHP + ival;
						end
					elseif bPlus == false then
						-- HP -###
						if sType == 'V' then
							TallyGear.VHP = TallyGear.VHP - ival;
						elseif sType == 'I' then
							TallyGear.IHP = TallyGear.IHP - ival;
						end
					end					
				end
			end
		end
	end
end		-- TallyHP
	
--[[
	TallyFromConvert will search the specified type of gear (visible, invisible) for 
	conversion attributes and tally them. How much of each is returned.
	sType = I (invisible), and V (visible)
--]]

function TallyFromConvert()
	local cur = gData.GetEquipment();
	local iposCMP,iposCHP;
	local sType;
	local tTemp = { };

	for i = 1,2,1 do
		if i == 1 then
			sType = 'V';
			tTemp = TallyGear.Visible;
		else
			sType = 'I';
			tTemp = TallyGear.Invisible;
		end
			
		for ii,jj in pairs(cur) do
			iposCMP = string.find(jj.Resource.Description[1],' HP to MP');
			iposCHP = string.find(jj.Resource.Description[1],' MP to HP');
			
			if iposCMP ~= nil then
				-- Found hp to mp conversion
				if table.find(tTemp,ii) ~= nil then
					if (jj.Name ~= 'Tortoise Shield') then		
						if sType == 'V' then
							TallyGear.VCHPMP = TallyGear.VCHPMP + tonumber(string.sub(jj.Resource.Description[1],iposCMP-2,iposCMP-1));
						else
							TallyGear.ICHPMP = TallyGear.ICHPMP + tonumber(string.sub(jj.Resource.Description[1],iposCMP-2,iposCMP-1));
						end
					end
				end
			end
		
			if iposCHP ~= nil then
				-- Found mp to hp conversion
				if table.find(tTemp,ii) ~= nil then	
					if sType == 'V' then
						TallyGear.VCMPHP = TallyGear.VCMPHP + tonumber(string.sub(jj.Resource.Description[1],iposCHP-2,iposCHP-1));
					else
						TallyGear.ICMPHP = TallyGear.ICMPHP + tonumber(string.sub(jj.Resource.Description[1],iposCHP-2,iposCHP-1));
					end
				end
			end
		end	
	end
end		-- TallyFromConvert

--[[
	MakeCodeTable takes the passed, // delimited list and returns the
	individual codes in a table
--]]

function gcinclude.MakeCodeTable(sList)
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
end		-- gcinclude.MakeCodeTable

--[[
	ValidInlineCode checks that the formatting of the dynamic code is correct.
	Returned is True or False and the individual pieces
--]]

function ValidInlineDynamicCode(suCode)
	local bPct = false;
	local iOff = 0;	
	local sOperator,sRoot,ival;
	local tComparators = { 'EQ', 'LT', 'LE', 'GT', 'GE', 'NE'};
	
	if string.find('TP.,TPP,MP.,MPP,HP.,HPP',string.sub(suCode,1,3)) ~= nil then
		if string.sub(suCode,3,1) ~= '.' then
			bPct = true;
			iOff = 1;
		end
		
		sRoot = string.sub(suCode,1,2+iOff);
		
		sOperator = string.sub(suCode,4+iOff,5+iOff);
		if table.find(tComparators,sOperator) ~= nil then
			if bVisual == true then
				ival = tonumber(string.sub(suCode,7+iOff,-2));
			else
				ival = tonumber(string.sub(suCode,7+iOff,-1));
			end
			return true,sRoot,sOperator,ival;
		else
			return false;
		end	
	else
		return false;
	end
end		-- ValidInlineCode

--[[
	EvalCodedComparison parses the passed conditional is true or not. Result is
	passed back
--]]

function EvalCodedComparison(sRoot,sOperator,ival,sGear)
	local player = gData.GetPlayer();
	local bGood = false;
	
	if sRoot == 'TP' or sRoot == 'TPP' then		-- TP is straightforward
		local iTP;
		if sRoot == 'TPP' then
			iTP = player.TP/10;
		else
			iTP = player.TP;
		end
		
		if sOperator == 'EQ' then
			bGood = (iTP == ival);
		elseif sOperator == 'LT' then
			bGood = (iTP < ival);
		elseif sOperator == 'LE' then
			bGood = (iTP <= ival);
		elseif sOperator == 'GT' then
			bGood = (iTP > ival);
		elseif sOperator == 'GE' then
			bGood = (iTP >= ival);
		else
			bGood = (iTP ~= ival);
		end
	elseif sCode == 'MP' then	
		local iMP;
		if sRoot == 'MPP' then
			iMP = player.MPP;
		else
			iMP = player.MP;
		end
			
		if sOperator == 'EQ' then
			bGood = (iMP == ival);
		elseif sOperator == 'LT' then
			bGood = (iMP < ival);
		elseif sOperator == 'LE' then
			bGood = (iMP <= ival);
		elseif sOperator == 'GT' then
			bGood = (iMP > ival);
		elseif sOperator == 'GE' then
			bGood = (iMP >= ival);
		else
			bGood = (iMP ~= ival);
		end
	elseif sCode == 'HP' then
		local iHP;
		if sRoot == 'HPP' then
			iMP = player.HPP;
		else
			iMP = player.HP;
		end
		
		if sOperator == 'EQ' then
			bGood = (iHP == ival);
		elseif sOperator == 'LT' then
			bGood = (iHP < ival);
		elseif sOperator == 'LE' then
			bGood = (iHP <= ival);
		elseif sOperator == 'GT' then
			bGood = (iHP > ival);
		elseif sOperator == 'GE' then
			bGood = (iHP >= ival);
		else
			bGood = (iHP ~= ival);
		end
	end
	return bGood;
end		-- EvalCodedComparison

--[[
	ValidSpecial sees if the passed gear's settings are valid. Returned is True
	or False.
--]]

function gcinclude.ValidateSpecial(sGear)
	local player = gData.GetPlayer();
	local gear;
	local bGood = false;
	
	if sGear == nil then
		return false;
	end
	
	gear = sGear;
	sGear = string.lower(sGear);
	
	-- The "SPECIAL" qualifier is for specialized gear. Initially run the routines
	-- that parse the item descriptions looking for MP+/HP+ and Convert attributes
	
	ClearTallyGear();
	TallyFromConvert();
	TallyMP();
	TallyHP();
	
	-- Now, do specific calculations based on the name of the piece of gear
	if sGear == 'uggalepih pendant' then
		-- Condition: MP% < 51. MAB bonus. Only visible gear, ignore all "Convert HP to MP"
		-- Check outright first	
		if (player.MPP < 51) then
			return true;
		else
			local iMP = player.MP - TallyGear.VCHPMP;
			local imMP = player.MaxMP - TallyGear.IMP - TallyGear.ICMPHP- TallyGear.ICHPMP;
			local iaMP = player.MaxMP * (TallyGear.IMPP * 0.01);
			bGood = ((iMP/(imMP - iaMP))*100 < 51);
		end
	elseif sGear == 'parade gorget' then
		-- Condition: HP% >= 85. Adds "Refresh". Only visible gear
		-- Check outright first	
		if (player.HP/player.MaxHP)*100 >= 85 then
			return((player.MP/player.MaxMP)*100 <= gcinclude.settings.Tolerance);
		else
			-- Let's see if the invisible gear will make a difference
			local iHP = player.MaxHP - TallyGear.IHP - TallyGear.ICMPHP;
			local iaHP = math.floor(iHP * (TallyGear.IHPP * 0.01));
			bGood = ((player.HP/(iHP - iaHP))*100 >= 85 and (player.MP/player.MaxMP)*100 <= gcinclude.settings.Tolerance);
		end
	elseif sGear == 'sorcerer\'s ring' then
		-- Condition: HP% < 76 and TP% < 100. Ignore HP+ (flat and percent) and
		-- Convert HP to MP gear.
		-- Check outright first	
		if (player.HP/player.MaxHP)*100 < 76 and player.TP/10 < 100 then
			return true;
		else
			local fHP = TallyGear.VHP + TallyGear.IHP;			
			local fCHPMP = TallyGear.VCHPMP + TallyGear.ICHPMP;
			local fHPP = math.floor((TallyGear.VHPP + TallyGear.IHPP) * (fHP + fCHPMP) * 0.01);
			if (player.HP - fHP - fHPP)/player.MaxHP < 76 and player.TP/10 < 100 then
				return true;
			end
		end
	else
		print(chat.header('ValidateSpecial'):append(chat.message('Warning: No special code exists for ' .. gear .. '. Ignoring piece.')));		
	end
	return bGood;
end		-- ValidateSpecial

--[[
	CheckAccessible is a replacement function for what was found in the "Validate"
	routines, which has been removed. It determines if the passed gear can be
	found in a storage container that is accessible out of town
--]]

function gcinclude.CheckAccessible(sGear)
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local containerID,itemEntry,item;
	
	for i,desc in pairs(gcinclude.EQUIPABLE) do
		containerID = desc[1];
		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
				item = resources:GetItemById(itemEntry.Id);
				if string.lower(item.Name[1]) == string.lower(sGear) then
					return true;
				end
			end
		end
	end
	return false;
end		-- CheckAccessible

--[[
	CheckAccuracySlots determines if the passed slot is one of the designated
	accuracy slots. Please note if the slot is named "ears" or "rings" both
	associated slots will be checked.
--]]

function gcinclude.CheckAccuracySlots(sSlot)

	sSlot = string.lower(sSlot);
	for i,j in ipairs(gcinclude.Locks) do
		if j[1] == sSlot or 
		   (sSlot == 'ears' and string.find('ear1,ear2',j[1]) ~= nil and j[3] == true) or
		   (sSlot == 'rings' and string.find('ring1,ring2',j[1]) ~= nil and j[3] == true) then
			return true;
		end
	end
	return false;
end		-- gcinclude.CheckAccuracySlots


--[[
	CheckInline checks for a simple conditional on the item passed into it.
	Returned is whether the condition is met and the item's name (minus the
	conditional.
	
	NoGear is an optional parameter. It indicates that the gear name should 
	not be returned
--]]

function gcinclude.CheckInline(gear,sSlot)
	local iPos,ii,suCode;
	local player = gData.GetPlayer();
	local party = gData.GetParty();
	local mj = player.MainJob;
	local sj = player.SubJob;
	local pet = gData.GetPet();
	local spell = gData.GetAction();
	local environ = gData.GetEnvironment();
	local timestamp = gData.GetTimestamp();
	local gSet = gData.GetCurrentSet();
	local wTypesM = 'AXE,GAXE,SWORD,GSWORD,SCYTHE,STAVE,CLUB,H2H,DAGGER,KATANA,GKATANA,POLEARM';
	local wTypesR = 'ARCHERY,MARKSMANSHIP,THROWING';
	local suCodeTbl = { };
	local bGood = true;
	
	if gear == nil then
		return false,gear;
	end
	
	iPos = string.find(gear,'//');

	if iPos == nil then
		return true,gear;
	end
	
	sGear = string.sub(gear,1,iPos-1);
	suCodeTbl = gcinclude.MakeCodeTable(string.upper(string.sub(gear,iPos,-1)));

	for ii,suCode in pairs(suCodeTbl) do
		if string.find('FIRESDAY,EARTHSDAY,WATERSDAY,WINDSDAY,ICEDAY,LIGHTNINGDAY,LIGHTSDAY,DARKSDAY',suCode) ~= nil then
			bGood = (suCode == string.upper(environ.Day));					-- Is it the specified day
		elseif string.find('NOT_FIRESDAY,NOT_EARTHSDAY,NOT_WATERSDAY,NOT_WINDSDAY,NOT_ICEDAY,NOT_LIGHTNINGDAY,NOT_LIGHTSDAY,NOT_DARKSDAY',suCode) ~= nil then
			bGood = (string.sub(suCode,5,-1) ~= string.upper(environ.Day));	-- Is it not the specified day
		elseif table.find(gcinclude.ExactBuff,string.lower(suCode)) ~= nil then		-- en spells
			bGood = (gData.GetBuffCount(suCode) >= 1);
		elseif string.find(wTypesM,suCode) ~= nil then						-- Is main weapon specified type
			bGood = (gSet['Main'] ~= nil and table.find(gProfile.WeaponType[suCode],gSet['Main']) ~= nil);
		elseif string.find(wTypesR,suCode) ~= nil then						-- Is ranged weapon specified type
			bGood = (gSet['Range'] ~= nil and table.find(gProfile.WeaponType[suCode],gSet['Range']) ~= nil);
		elseif suCode == 'ACCESSIBLE' then
			bGood = gcinclude.CheckAccessible(sGear); 
		elseif suCode == 'ACCURACY' then
			bGood = (gcinclude.CheckAccuracySlots(sSlot) == true);		
		elseif string.sub(suCode,1,3) == 'AK:' then			-- National Aketon
			local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
			local sWhich = string.sub(suCode,4,-1);
			if sWhich == 'WINDY' then
				bGood = (environ.Area ~= nil and gcinclude.Windy:contains(environ.Area) and pNation == 2);
			elseif sWhich == 'SANDY' then
				bGood = (environ.Area ~= nil and gcinclude.Sandy:contains(environ.Area) and pNation == 0);
			elseif sWhich == 'BASTOK' then
				bGood = (environ.Area ~= nil and gcinclude.Bastok:contains(environ.Area) and pNation == 1);
			elseif sWhich == 'OMNI' then
				bGood = (environ.Area ~= nil and 
						(gcinclude.Windy:contains(environ.Area) or
						 gcinclude.Sandy:contains(environ.Area) or 
						 gcinclude.Bastok:contains(environ.Area) or 
						 gcinclude.Jeuno:contains(environ.Area)));
			else
				bGood = false;
			end
		elseif suCode == 'BLIND' then						-- Player is blind
			bGood = (gData.GetBuffCount('Blind') >= 1);
		elseif suCode == 'CARBY' then						-- Pet is carbuncle
			bGood = (gcinclude.isPetNamed('Carbuncle'));
		elseif suCode == 'COVER' then						-- Player has cast cover
			bGood = (gData.GetBuffCount('Cover') >= 1);
		elseif string.sub(suCode,1,3) == 'CR:' then			-- Crafting
			bGood = (gcinclude.Craft == string.sub(suCode,4,-1));
		elseif suCode == 'DAYTIME' then						-- Time is daytime
			bGood = gcinclude.CheckTime(timestamp.hour,'Daytime',false);
		elseif string.sub(suCode,1,3) == 'DB:' then
			bGood = (player.MainJob == 'BST' and string.upper(string.sub(suCode,4,-1)) == string.upper(gcdisplay.GetCycle('DB')));	
		elseif suCode == 'DUSK2DAWN' then					-- Time between dusk and dawn
			bGood = gcinclude.CheckTime(timestamp.hour,DUSK2DAWN,false);
		elseif suCode == 'EVASION' then
			bGood = (gcdisplay.GetToggle('Eva') == true);	
		elseif suCode == 'FULLMOON' then					-- Moon phase: Full Moon
			bGood = (environ.MoonPhase == 'Full Moon');
		elseif string.sub(suCode,1,3) == 'GA:' then			-- Gathering
			bGood = (gcinclude.Gather == string.sub(suCode,4,-1));
		elseif suCode == 'HORN' then						-- Is the bard's instrument a horn
			if player.MainJob == 'BRD' then
				bGood = (gcdisplay.GetCycle('Instrument') == 'Horn');
			else
				bGood = false;
			end	
		elseif string.find(suCode,'IF:') then
			local sCur = gData.GetEquipSlot(sSlot);
			local sItem = string.sub(suCode,4,-1);
			bGood = (string.lower(sItem) == string.lower(sCur));
		elseif suCode == 'MSJ' then							-- Magical subjob
			bGood = (string.find(gcinclude.sMagicJobs,sj) ~= nil);
		elseif suCode == 'NEWMOON' then						-- Moon phase: New Moon
			bGood = (environ.MoonPhase == 'New Moon');
		elseif suCode == 'NIGHTTIME' then					-- Time is nighttime
			bGood = gcinclude.CheckTime(timestamp.hour,'Nighttime',false);
		elseif suCode == 'NOT_OWN' then						-- Player in area not controlled by their nation
			bGood = (gcdisplay.GetCycle('Region') ~= 'Owned');
		elseif string.sub(suCode,1,8) == 'NOT_WTH:' then	-- Does the weather not match
			bGood = (string.find(string.upper(environ.Weather),string.sub(suCode,9,-1)) == nil);
		elseif suCode == 'NOT_WTH-DAY' then					-- Weather does not match day's element
			local sEle = string.upper(environ.DayElement) .. ',NONE';
			bGood = (string.find(sEle,string.upper(environ.WeatherElement)) == nil);
		elseif suCode == 'OWN' then							-- Player in area controlled by their nation
			bGood = (gcdisplay.GetCycle('Region') == 'Owned');
		elseif string.sub(suCode,1,5) == 'PARTY' then	-- is player in a party/alliance
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
		elseif suCode == 'PETFNPF' then						-- Is player's pet fighting, but not the player
			bGood = (pet ~= nil and pet.Status == 'Engaged' and player.Status ~= 'Engaged');
		elseif string.sub(suCode,1,3) == 'PJP' and string.len(suCode) == 6 then	
			local s = string.sub(suCode,4,-1);
			bGood=(gcinclude.CheckPartyJob(s));				-- party has job: //PJP"job"
		elseif string.sub(suCode,1,2) == 'SJ' and string.len(suCode) == 5 then	
			bGood = (string.sub(suCode,3,-1) == sj);		-- subjob is: //SJ"job"
		elseif string.sub(suCode,1,4) == 'SMN:' then
			bGood = (string.lower(spell.Name) == string.lower(string.sub(suCode,5,-1)));
		elseif suCode == 'SMNPET' then						-- Is player's pet a summoned avatar
			bGood = (pet ~= nil and string.find(gcinclude.SummonSkill,string.lower(pet.Name)));
		elseif suCode == 'SMNPETMD' then					-- Does the summoner pet's element match the day?
			if pet ~= nil and string.find(gcinclude.SummonSkill,string.lower(pet.Name)) then
				bGood = (gcinclude.SummonStaves[string.lower(pet.Name)] == string.lower(environ.DayElement));
			else
				bGood = false;
			end
		elseif suCode == 'SMNPETMW' then					-- Does the player's pet's element match the weather
			if pet ~= nil then
				local sElement = gcinclude.SummonStaves[string.lower(pet.Name)];
				bGood = (sElement ~= nil and string.find(string.lower(environ.RawWeather),string.lower(sElement)) ~= nil);
			else
				bGood = false;
			end			
		elseif suCode == 'SPECIAL' then
			bGood = gcinclude.ValidateSpecial(sGear);
		elseif suCode == 'SPIRIT:ES' then					-- Pet being summoned is a spirit
			bGood = (string.find(gcinclude.Spirits,string.lower(spell.Name)) ~= nil);
		elseif suCode == 'SPIRIT:EP' then					-- Current pet is a spirit
			bGood = (pet ~= nil and string.find(gcinclude.Spirits,string.lower(pet.Name)) ~= nil);
		elseif suCode == 'STRING' then						-- Is the bard's instrument a string instrument
			if player.MainJob == 'BRD' then
				bGood = (gcdisplay.GetCycle('Instrument') == 'String');
			else
				bGood = false;
			end
		elseif suCode == 'TANK' then
			bGood = (gcdisplay.GetToggle('Tank') == true);
		elseif string.find('TP.,TPP,MP.,MPP,HP.,HPP',string.sub(suCode,1,3)) ~= nil then
			local sRoot,sOperator,ival;
			bGood,sRoot,sOperator,ival = ValidInlineDynamicCode(suCode);		
			if bGood == true then
				bGood = EvalCodedComparison(sRoot,sOperator,ival,sGear);
			else
				bGood = false;
			end
--		elseif suCode == 'UTSUSEMI' then
--			bGood = (gData.GetBuffCount('copy image (3)') >= 1);
		elseif suCode == 'WSWAP' then						-- Weapon swapping enabledB
			bGood = (gcinclude.settings.bWSOverride == true or gcdisplay.GetToggle('WSwap') == true);
		elseif string.sub(suCode,1,4) == 'WTH:' then		-- Does the weather match
			bGood = (string.find(string.upper(environ.Weather),string.sub(suCode,5,-1)) ~= nil);
		elseif suCode == 'WTH-DAY' then						-- Weather matches day's element
			local sEle = string.upper(environ.DayElement) .. ',NONE';
			bGood = (string.find(sEle,string.upper(environ.WeatherElement)) ~= nil);
		else
			print(chat.header('CheckInline'):append(chat.message('Warning: Unknown code = ' .. suCode .. '. Ignoring piece of gear.')));
			bGood = false;
		end
		
		if bGood == false then
			return false,sGear;
		end
	end
	
	return true,sGear;
end		-- gcinclude.CheckInline

function gcinclude.RegionControlDisplay()
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
		if j[1] < 0 or j[1] > 4 Then
			print(chat.message('Huh? ' .. i ..' = ' .. tostring(j[1])));
		else
			print(chat.message(i ..' = ' .. sArea(j[1])));
		end
	end
end		-- RegionControlDisplay

function gcinclude.t1()
	print(chat.message('Player\'s nation = ' .. tostring(gcinclude.OwnNation)));
	print(' ');
	for i,j in pairs(gcinclude.RegionControl) do
		print(chat.message(i ..' = ' .. tostring(j[1])));
	end
end		-- gcinclude.t1

--[[
	MoveToCurrent copies the gear defined in the passed set to current master
	set. Nothing is displayed, this is just a transfer routine. Note: Added
	inline conditional check
--]]

function gcinclude.MoveToCurrent(tSet,tMaster,bOverride)
	local player = gData.GetPlayer();
	local item = {};
	local root,sK,vRoot,vConditional;
	local bContinue,iNum,bGood,bSkip;

	if tSet == nil then
		return;
	end
	
	-- Make sure player's transition between zones is complete
	if player.MainJob == 'NON' then
		return;
	end

	-- Walk the gear set slots
	for k,v in pairs(tSet) do
		bContinue = false;
		sK = string.lower(k);
		
		-- Check for special case, Ears or Rings
		if string.find('ears,rings',sK) ~= nil then	
			root = string.sub(k,1,-2);
			iNum = 1;
			bContinue = true;
		end
		
		-- Quick check: if the slot to be populated is the 'Main', make sure
		-- that /WSWAP is true or that gcinclude.settings.bWSOverride is true.
		-- This should have been caught earlier, but just in case...
		
		if string.find('Main,Sub,Range',k) ~= nil then
			bSkip = not (gcdisplay.GetToggle('WSwap') == true 
						 or gcinclude.settings.bWSOverride == true
						 or (bOverride ~= nil and bOverride == true));					 
		else
			bSkip = false;
		end
	
		if bSkip == false then
			-- If definition is a table, then multiple pieces identified. Determine
			-- appropriate one to use.
		
			if type(v) == 'table' then
				iNum = 1;

				-- Walk list of items and equip level appropriate one
				for kk,vv in pairs(v) do			
					-- See if there's an inline conditional to be checked.
					-- Note the need to distinguish which "ear" or "ring"
					if bContinue then
						sK = root .. tostring(iNum);
						bGood,vRoot = gcinclude.CheckInline(vv,sk);
					else
						bGood,vRoot = gcinclude.CheckInline(vv,k);
					end
					if bGood then
						item = AshitaCore:GetResourceManager():GetItemByName(vRoot,2);					
						if item == nil then
							if string.find(gcinclude.GearWarnings,vRoot) == nil then
								print(chat.header('MoveToCurrent'):append(chat.message('Warning: ' .. vRoot .. ' not a valid piece of gear. Skipping.')));
								gcinclude.GearWarnings = gcinclude.GearWarnings .. vRoot .. ',';
							end
						else
							if (bit.band(item.Jobs,gcinclude.JobMask[player.MainJob]) == gcinclude.JobMask[player.MainJob]) or
							   (bit.band(item.Jobs,gcinclude.JobMask['Alljobs']) == gcinclude.JobMask['Alljobs']) then
								-- Check level of item vs level of player
								if item.Level <= player.MainJobSync then				
									-- Either an actual or a pseudo slot name: Ears or Rings. 
									-- Build slot name and equip item there. Bump counter for 
									--next of pair
									if bContinue then
										sK = root .. tostring(iNum);
										tMaster[sK] = vRoot;
										iNum = iNum + 1;						
									else
										-- Normal single slot
										tMaster[k] = vRoot;						
										break;
									end
								end
							end							
							-- When iNum > 2, all special slots of "root" populated
							if iNum > 2 then					
								break;
							end
						end
					end
				end	
			else
				-- Single value. See if there's an inline conditional to be checked
				if bContinue then
					sK = root .. tostring(iNum);
					bGood,vRoot = gcinclude.CheckInline(vv,sk);
				else				
					bGood,vRoot = gcinclude.CheckInline(v,k);
				end
				
				if bGood then
					item = AshitaCore:GetResourceManager():GetItemByName(vRoot,2);
					if item == nil then
						if string.find(gcinclude.GearWarnings,vRoot) == nil then
							print(chat.header('MoveToCurrent'):append(chat.message('Warning: ' .. vRoot .. ' not a valid piece of gear. Skipping.')));
							gcinclude.GearWarnings = gcinclude.GearWarnings .. vRoot .. ',';
						end
					else
						if (bit.band(item.Jobs,gcinclude.JobMask[player.MainJob]) == gcinclude.JobMask[player.MainJob]) or
						   (bit.band(item.Jobs,gcinclude.JobMask['Alljobs']) == gcinclude.JobMask['Alljobs']) then
							-- Check level of item vs level of player
							if item ~= nil and item.Level <= player.MainJobSync then
								if bContinue then
									sK = root .. tostring(iNum);
									tMaster[sK] = vRoot;
								else
									tMaster[k] = vRoot;
								end
							end
						end
					end
				end
			end
		end
	end
end		-- gcinclude.MoveToCurrent

--[[
	EquipTheGear makes sure that the passed gear set doesn't have an item in a slot
	that is being blocked by another item (e.g., no head gear if a vermillion cloak
	is in the body slot.) It the equips the gear set.
--]]

function gcinclude.EquipTheGear(tSet)
	local sSlot;
	
	-- First, deal with the v.cloak, make sure no head gear
	if tSet['Body'] == 'Vermillion Cloak' then
		tSet['Head'] = '';
	end
	
	-- Now, clear out the locked slots
	for i,j in pairs(gcinclude.Locks) do
		if j[2] == true then
			sSlot = gData.Constants.EquipSlotNames[gData.Constants.EquipSlotsLC[j[1]]];		
			tSet[sSlot] = '';	
		end
	end
	
	-- And if weapon swapping is not enabled, clear out the top line
	if (gcdisplay.GetToggle('WSwap') ~= true and gcinclude.settings.bWSOverride ~= true) then
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
	
	gFunc.ForceEquipSet(tSet);
end			-- gcinclude.EquipTheGear

--[[
	CheckLockAccCollision checks to see if any of the slots associated with locks
	clashes with slots associated with the accuracy. An appropriate warning is
	issued if there's a problem.
--]]

function gcinclude.CheckLockAccCollision(sFrom)
	sFrom = string.lower(sFrom);
	
	for i,j in ipairs(gcinclude.Locks) do
		if j[2] == true and j[3] == true then
			if sFrom == 'locks' then
				print(chat.message('Warning: one or more locks conflict with accuracy slots.'));
			else
				print(chat.message('Warning: one or more accuracy slots conflict with locks.'));			
			end
			return;
		end
	end
end		-- gcinclude.CheckLockAccCollision

--[[
	getPairedAccuracySlotValues returns whether the ear slots or ring slots have been
	degnated for accuracy gear. This is needed to determine if both associated slots
	need to be populated or just one of the slots.
--]]

function getPairedAccuracySlotValues(sSlot)
	local bS1 = false;
	local bS2 = false;
	local root = string.sub(string.lower(sSlot),1,-2);
	
	for i,j in ipairs(gcinclude.Locks) do
		if j[1] == root .. '1' and j[3] == true then
			bS1 = true;
		elseif j[1] == root .. '2' and j[3] == true then
			bS2 = true;
		end
	end
	return bS1,bS2;
end		-- getPairedAccuracySlotValues

--[[
	FractionalAccuracy uses the stored accuracy slots and builds a equipment table
	(from the appropriate accuracy set) and then equips said table. It is a replacement
	for that On/Off accuracy implementation that was originally developed. This new
	approach lets the user (through the /acc and /nac commands) specify which slots
	accuracy gear should be equipped. This "fractional" approach lets the user decide
	how much accuracy gear should be equipped.
--]]

function gcinclude.FractionalAccuracy(accTbl,tankAccTbl)
	local src,s,t,root;
	local tAcc = {};
	
	if gcinclude.AccNumeric == 'None' then
		return;
	end
	
	if gcdisplay.GetToggle('Tank') ~= nil and 
	   gcdisplay.GetToggle('Tank') == true and 
	   tankAccTbl ~= nil then
		src = tankAccTbl;
	else
		src = accTbl;
	end
	
	for i,j in pairs(src) do
		-- Special case for ears and rings. Deal with then
		if string.lower(i) == 'ears' then
			local s1,s2 = getPairedAccuracySlotValues('ears');
			if s1 == true and s2 == true then
				tAcc[i] = j;
			elseif s1 == true then
				tAcc['Ear1'] = j;
			else
				tAcc['Ear2'] = j;
			end
		elseif string.lower(i) == 'rings' then
			local s1,s2 = getPairedAccuracySlotValues('rings');
			if s1 == true and s2 == true then
				tAcc[i] = j;
			elseif s1 == true then
				tAcc['Ring1'] = j;
			else
				tAcc['Ring2'] = j;
			end		
		else
			-- Normal slots. Match it up
			for ii,jj in pairs(gcinclude.Locks) do			
				if string.lower(i) == jj[1] and jj[3] == true then
					tAcc[i] = j;
				end
			end
		end
	end
	
	gcinclude.MoveToCurrent(tAcc,gProfile.Sets.CurrentGear);	
end		-- gcinclude.FractionalAccuracy

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
	
	-- Make sure either the main job or sub job can cast magic
	if gcinclude.MagicalJob('T') == false then
		print(chat.header('MaxSpell'):append(chat.message('Current job does not support magic.')));
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
	SwapToStave determines if swapping your weapon out for one of the elemental staves makes
	sense and does it for you while remembering what weapon/offhand you had equipped.
	
	Note: there's a timing issue when it comes to getting the two weapons currently equipped
	that has to do with zoning. I don't see any reason for this routine to run afoul of the
	problem, so I'm going to assume it won't occur here. If an error does arise though, what
	is needed to be done is to check if ew['Main'] == nil (the same is true for ew['Sub']).
--]]

function gcinclude.SwapToStave(sStave,noSave,cs)
	local ew = gData.GetEquipment();
	local player = gData.GetPlayer();
	local sGear;
	local eWeap = nil;
	local eOff = nil;

	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end
	
	if ew['Sub'] ~= nil then
		eOff = ew['Sub'].Name;
	end;

	-- This is needed for a timing issue
	if sStave == nil then
		return;
	end
	
	if (gcdisplay.GetToggle('WSwap') == true or gcinclude.settings.bWSOverride) then	
		sGear = gcinclude.CheckForEleGear('staff',sStave);		
		-- See if a current weapon is the one of the targetted staves
		if not (eWeap == nil or sGear == nil or (eWeap ~= nil and string.lower(eWeap) == string.lower(sGear))) then
			-- save the weapon so it can be equipped again
			if eWeap ~= gcinclude.weapon and noSave == false and gcinclude.settings.bWSOverride == false then
				gcinclude.weapon = eWeap;
				gcinclude.offhand = eOff;
			end
		end
		
		if sGear ~= nil then
			-- All elemental staves are level 51. Check versus level of player and that the
			-- position isn't locked.
			if player.MainJobSync >= 51 and gcinclude.Locks[1][2] == false then	-- Locks[1] is 'Main'
				cs['Main'] = sGear;		
			end
		end
	end
end		-- gcinclude.SwapToStave

--[[
	EquipItem processes the passed arguments and equips the specified item (whether by coded entry or name)
	into the appropriate equipment slot. Then locks the appropriate slot
	
		/equipset code|"item name" slot
--]]

function gcinclude.EquipItem(args)
	local iName = nil;
	local iSlot = nil;
	local sLocks = nil;
		
	if #args > 1 then
		-- see if the item specified is a code	
		for k,v in pairs(gcinclude.equipIt) do
			if k == args[2] then
				iName = v[1];
				iSlot = v[2];
				sLocks = v[3];
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
				print(chat.header('EquipIt'):append(chat.message('Error: incomplete /equipit command: /equipit code|name slot. Command ignored.')));
				return;
			end
		end

		-- ring and ear need a slot appended to it. Just assume "1"
		if string.find('ring,ear',string.lower(iSlot)) ~= nil then
			iSlot = iSlot .. '1';
		end
		
		-- Make sure the slot is formatted right (assuming it's just a case issue)
		iSlot = string.upper(string.sub(iSlot,1,1)) .. string.lower(string.sub(iSlot,2));
		-- Now try and load the item
		gFunc.ForceEquip(iSlot,iName);
		if sLocks ~= nil then
			gcinclude.LockUnlock('locks','lock',sLocks);
		else
			gcinclude.LockUnlock('locks','lock',iSlot);
		end
		local sList = gcinclude.GetLockedList('locks');
		gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);	
	else
		print(chat.header('EquipIt'):append(chat.message('Error: incomplete /equipit command: /equipit code|name slot|#. Command ignored.')));
	end
end		-- gcinclude.EquipItem

--[[
	GetTableByName returns the gear set that is associated with the set name passed to it.
	It does this by walking the Sets (either gProfile.Sets or gcinclude.Sets)
--]]

function gcinclude.GetTableByName(sName)
	local s,s2;
	local sName2;
	
	sName2 = string.lower(sName);
	s = string.find(sName2,'gcinclude');
	if s == nil then
		for k,l in pairs(gProfile.Sets) do
			if string.lower(k) == sName2 then
				return l,false;
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
			return l,true;
		end
	end
	
	return nil,false;
end		-- GetTableByName

--[[
	WhichSlot takes the passed slot #/name/code and converts it to a correctly
	formatted equipment slot name. If unable to determine, nil is returned.
--]]

function WhichSlot(sSlot)
	if sSlot == nil then
		return nil;
	end
	
	sSlot = string.lower(sSlot);
	
	-- First look for special cases, then determine if a slot name or
	-- number is specified
	if sSlot == 'rings' then
		return 'Ring1';
	elseif sSlot == 'ears' then
		return 'Ear1';
	else
		-- Locks are a convenient list to use to identify the slot name/number
		for j,k in ipairs(gcinclude.Locks) do
			if k[1] == sSlot or tostring(j) == sSlot then
				return string.upper(string.sub(k[1],1,1)) .. string.sub(k[1],2,-1);
			end
		end
		return nil;
	end
end		-- WhichSlot
--[[
	WhichAccuracySet searches the player's AccuracySet for the named set and
	returns the associated slots. If not found, an error message is displayed
	and nil is returned.
--]]

function WhichAccuracySet(sId)

	if sId == nil or gProfile.AccuracySet == nil then
		return nil;
	end
	
	for i,j in pairs(gProfile.AccuracySet) do
		if string.lower(sId) == string.lower(i) then
			return j;
		end
	end
	print(chat.header('WhichAccuracySet'):append(chat.message('Accuracy set: ' .. sId .. ' not found. Ignoring.')));
	return nil;
end

--[[
	HandleCommands processes any commands typed into luashitacast as defined in this file
--]]

function gcinclude.HandleCommands(args)

	if not gcinclude.AliasList:contains(args[1]) then return end

	local player = gData.GetPlayer();
	local toggle = nil;
	local status = nil;
	local sList, sKey, sSet;
	
	-- Clear out the local copy of current gear
	gcinclude.ClearSet(gcinclude.sets.CurrentGear);
	
	args[1] = string.lower(args[1]);
	if (args[1] == 'gswap') then			-- turns gear swapping on or off
		gcdisplay.AdvanceToggle('GSwap');
		toggle = 'Gear Swap';
		status = gcdisplay.GetToggle('GSwap');
	elseif args[1] == 't1' then				-- This is a test invoker
		gcinclude.t1();
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
	elseif (args[1] == 'idle') then			-- Turns on/off whether movement gear is equipped
		if string.find(gcinclude.JobList,player.MainJob) ~= nil then
			gcdisplay.AdvanceToggle('Idle');
			toggle = 'Idle';
			status = gcdisplay.GetToggle('Idle');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Your job does not support the idle command. Ignoring')));
		end
	elseif (args[1] == 'tank') then			-- Turns on/off whether tanking gear is equipped
		if string.find(gcinclude.JobList,player.MainJob) ~= nil then
			gcdisplay.AdvanceToggle('Tank');
			if gcdisplay.GetToggle('Tank') == false and gcdisplay.GetToggle('Idle') == false then
				gcdisplay.SetToggle('Idle',true);
				print(chat.header('HandleCommands'):append(chat.message('FYI: Since you disabled \'Tank\', \'Idle\' has been turned on.')));
			end
			toggle = 'Tank Set';
			status = gcdisplay.GetToggle('Tank');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Your job does not support the tanking option. Ignoring command')))
		end				
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
			print(chat.header('HandleCommands'):append(chat.message('Error: /sBP is only available to summoners. Ignoring command')));
		end
	elseif (args[1] == 'ajug') then			-- Turns on/off whether Automatic Jug assignment enabled
		if player.MainJob == 'BST' then
			gcdisplay.AdvanceToggle('AJug');
			toggle = 'Automated Jug Management';
			status = gcdisplay.GetToggle('AJug');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: /AJug is only available to beastmasters. Ignoring command')));
		end	
	elseif (args[1] == 'th') then			-- Turns on/off whether TH gear should be equipped
		if player.MainJob == 'THF' then
			gcdisplay.AdvanceToggle('TH');
			toggle = 'Treasure Hunter';
			status = gcdisplay.GetToggle('TH');
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
		toggle = 'Debuf';
		status = gcdisplay.GetCycle('DB');
	elseif (args[1] == 'lock' or args[1] == 'acc') then
		local sTarget = 'locks';
		if args[1] == 'acc' then
			sTarget = 'acc';
		end

		if args[2] ~= nil then
			if sTarget == 'acc' and string.sub(args[2],1,1) == '-' then
				args[2] = WhichAccuracySet(string.sub(args[2],2,-1));
			end
			gcinclude.LockUnlock(sTarget,'lock',args[2]);
		end
		sList = gcinclude.GetLockedList(sTarget);		
		if sList ~= nil then
			if sTarget == 'locks' then
				print(chat.message('The following slot(s) are locked: ' .. sList));
			else
				print(chat.message('The following slot(s) of accuracy are used: ' .. sList));
			end
		else
			if sTarget == 'locks' then
				print(chat.message('All slots are unlocked'));
			else
				print(chat.message('All accuracy slots are reset'));
			end
		end
		
		if sTarget == 'locks' then 
			gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);
		else
			gcdisplay.SetSlots('acc',gcinclude.AccNumeric);
		end
		gcinclude.CheckLockAccCollision(sTarget);
	elseif (args[1] == 'unlock' or args[1] == 'nac') then
		local sTarget = 'locks';
		if args[1] == 'nac' then
			sTarget = 'acc';
		end
		
		if args[2] ~= nil then
			if sTarget == 'nac' and string.sub(args[2],1,1) == '-' then
				args[2] = WhichAccuracySet(string.sub(args[2],2,-1));
			end
			gcinclude.LockUnlock(sTarget,'unlock',args[2]);
			if string.lower(args[2]) == 'all' then
				if sTarget == 'locks' then
					print(chat.message('All slots are unlocked'));
				else
					print(chat.message('All accuracy slots are reset'));
				end
			else
				if sTarget == 'locks' then
					print(chat.message('\'' .. args[2] .. '\' have been unlocked'));
				else
					print(chat.message('Accuracy slots: \'' .. args[2] .. '\' have been reset'));
				end
			end
		end
		sList = gcinclude.GetLockedList(sTarget);
		if sTarget == 'locks' then 
			gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);
		else
			gcdisplay.SetSlots('acc',gcinclude.AccNumeric);
		end
	elseif (args[1] == 'rc' then					-- Display region controls
		gcinclude.RegionControlDisplay();
	elseif (args[1] == 'slot') then					-- Locks specified slot and equips piece
		if #args == 3 then
			local sSlot = WhichSlot(args[2]);
			if sSlot ~= nil then		
				gcinclude.LockUnlock('locks','lock',sSlot);
				local sList = gcinclude.GetLockedList('locks');			
				gFunc.ForceEquip(sSlot,args[3]);
				print(chat.message(args[3] .. ' equipped in ' .. sSlot));
				gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);
			else
				print(chat.message('Invalid slot specified in /slot command. Ignoring'));
			end
		end
	elseif (args[1] == 'showit') then						-- Shows debug info for specified type
		gcinclude.DB_ShowIt();
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
					gcinclude.MoveToCurrent(gcinclude.sets.Crafting,gcinclude.sets.CurrentGear);					
				else
					-- Gather set
					gcinclude.Gather = sArg;
					gcinclude.MoveToCurrent(gcinclude.sets.Gathering,gcinclude.sets.CurrentGear);
				end
			else
				local tTable,bInc = gcinclude.GetTableByName(sArg);	-- Change string to table
				if tTable ~= nil then
					gcinclude.MoveToCurrent(tTable,gcinclude.sets.CurrentGear);
				else
					print(chat.header('HandleCommands'):append(chat.message('Gear set not found: ' .. sName)));
				end
			end
			
			gcinclude.EquipTheGear(gcinclude.sets.CurrentGear);
			
			gcdisplay.SetToggle('GSwap',(not(#args == 2 or string.lower(args[3]) ~= 'on')));
			toggle = 'Gear Swap';
			status = gcdisplay.GetToggle('GSwap');
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
			toggle = 'Toggle Instrument';
			status = gcdisplay.GetCycle('Instrument');
		else
			print(chat.header('HandleCommands'):append(chat.message('Your job does not support that command. Ignoring.')));
		end
	elseif (args[1] == 'maxspell') then			-- Determines highest level spell to cast
		if #args >= 2 then
			gcinclude.MaxSpell(args[2],true);
		end
		toggle = 'MaxSpell';
	elseif (args[1] == 'maxsong') then			-- Determines highest level song to cast
		gcinclude.MaxSong(args[2],(#args > 2),true);
		toggle = 'MaxSong';
	elseif args[1] == 'equipit' or args[1] == 'ei' then			-- Equip specified item
		gcinclude.EquipItem(args);
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

function gcinclude.CheckCommonDebuffs(tCur)
	local blinded = gData.GetBuffCount('Blind');
	local bound = gData.GetBuffCount('Bind');	
	local cursed = gData.GetBuffCount('Curse');	
	local doomed = (gData.GetBuffCount('Doom')) + (gData.GetBuffCount('Bane'));
	local para = gData.GetBuffCount('Paralysis');
	local petra = gData.GetBuffCount('Petrify');
	local poisoned = gData.GetBuffCount('Poison');
	local silenced = gData.GetBuffCount('Silence');
	local sleep = gData.GetBuffCount('Sleep');
	local weakened = gData.GetBuffCount('Weakened');	
	local shiningRuby = gData.GetBuffCount('Shining Ruby');

	if (blinded >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Blind,tCur,true);		
	end
	
	if (bound >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Bound,tCur,true);		
	end
	
	if (cursed >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Cursed,tCur,true);		
	end
	
	if (doomed >= 1) then	
		gcinclude.MoveToCurrent(gcinclude.sets.Doomed,tCur,true);
	end
	
	if (para >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Paralyzed,tCur,true);
	end	
	
	if (petra >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Petrified,tCur,true);
	end
	
	if (poisoned >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Poisoned,tCur,true);
	end	
	
	if (silenced >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Silenced,tCur,true);
	end	
	
	if (sleep >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Sleeping,tCur,true);	
	end

	if (weakened >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Weakened,tCur,true);	
	end;

	if (shiningRuby >= 1) then
		gcinclude.MoveToCurrent(gcinclude.sets.Shining_Ruby,tCur,true);
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
	GetRoot determines the "base" of a spell name. (The base is the first word in the spell name.)
	
	An optional parameter, bVersion, indicates if only the version should be cut off. (i.e., remove
	the I, II, III portion, etc.)
--]]

function gcinclude.GetRoot(spellName,bVersion)
	local i;
	local root = spellName;
	
	spellName = string.lower(spellName);
	
	if bVersion ~= nil and bVersion then
		i = string.find(spellName, " [^ ]*$");
		if i ~= nil and string.find('i,ii,iii,iv,v,vi',string.sub(spellName,i+1,-1)) ~= nil then
			root = string.sub(spellName,1,i-1);
		else
			root = spellName;
		end
	else
		spellName = string.lower(spellName);
		i = string.find(spellName,' ');
		if i ~= nil then
			root = string.sub(spellName,1,i-1);
		else
			root = spellName;
		end
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
	for k, str in pairs(listName) do								-- search the list
		if string.find(str,root) ~= nil then						-- if not nil then the "root" was found
			if sWhat == gcinclude.OBI then		
				local sGear = gcinclude.CheckForEleGear('obi',k);
				if sGear ~= nil then
					pctDay,pctWeather = gcinclude.CheckObiDW(k);	-- determine if the day/weather is advantageous
					if (pctDay + pctWeather) > 0 then
						return sGear;						-- return the obi's name
					end
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
			ilvl = player.MainJobSync;
		end
		
		for k,tpf in pairs(gcinclude.petfood) do
			if sAction == 'max' then
				if tpf[4] and (tpf[3] > ilvl) and (tpf[3] <= player.MainJobSync) then
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
		if gcinclude.Locks[4][2] == false then
			gFunc.ForceEquip('Ammo', sName);
			print(chat.header('doPetFood'):append(chat.message('Equipping: ' .. sName)));
			return true;
		else
			print(chat.header('doPetFood'):append(chat.message('Ammo slot locked. Unable to equip: ' .. sName)));
			return false;
		end
	end				
end		-- gcinclude.doPetFood

--[[
	Unload ensures that the aliases are removed and the display objects are removed
--]]

function gcinclude.Unload()
	gcinclude.ClearAlias();
	gcdisplay.Unload();
end		-- gcinclude.Unload

--[[
	Initialize gives luashitacast it's initial settings
--]]

function gcinclude.Initialize()
	gcdisplay.Initialize:once(2);
	gcinclude.SetVariables:once(2);
	gcinclude.SetAlias:once(2);
end		-- gcinclude.Initialize

--[[
	HandleMidcast is the second function invoked when a player casts a spell. It equips gear appropriate for 
	magic skill, duration, magic attack bonus, magic accuracy, and potency. There's an order to how the pieces 
	are loaded: INT/MND, spell specific, macc, magic skill, obi, ele swap. This routine is called from a stub
	function of the same name in the job file.
--]]

function gcinclude.HandleMidcast(bTank)
	local player = gData.GetPlayer();
	local spell = gData.GetAction();
	local obi;
	local sSet;
	local cKey;

	if bTank == nil then		-- Need to check because of transition state of change
		bTank = false;
	end
	
	gcinclude.settings.priorityMidCast = string.upper(gcinclude.settings.priorityMidCast);
	for i = 1,string.len(gcinclude.settings.priorityMidCast),1 do
		cKey = string.sub(gcinclude.settings.priorityMidCast,i,i);

		if cKey == 'A' then				-- midcast gear
			gcinclude.MoveToCurrent(gProfile.Sets.Midcast,gProfile.Sets.CurrentGear);
		elseif cKey == 'B' then			-- Spell Interruption Rate gear
			if spell.Skill ~= 'Singing' then
				gcinclude.MoveToCurrent(gProfile.Sets.SIR,gProfile.Sets.CurrentGear);
			end
		elseif cKey == 'C' then			-- INT/MND gear?
			sSet = gcinclude.WhichStat(spell.Name);
			if sSet ~= nil then
				if sSet == 'MND' then
					if bTank == true then
						gcinclude.MoveToCurrent(gProfile.Sets.Tank_MND,gProfile.Sets.CurrentGear);
					else
						gcinclude.MoveToCurrent(gProfile.Sets.MND,gProfile.Sets.CurrentGear);
					end
				elseif sSet == 'INT' then
					if bTank == true then
						gcinclude.MoveToCurrent(gProfile.Sets.Tank_INT,gProfile.Sets.CurrentGear);
					else
						gcinclude.MoveToCurrent(gProfile.Sets.INT,gProfile.Sets.CurrentGear);
					end
				end
			end
		elseif cKey == 'D' then			-- Magic Skill Type		
			-- Now process for the skill type
			if spell.Skill == 'Healing Magic' then
				gcinclude.MoveToCurrent(gProfile.Sets.Healing,gProfile.Sets.CurrentGear);
			elseif spell.Skill == 'Dark Magic' then
				gcinclude.MoveToCurrent(gProfile.Sets.Dark,gProfile.Sets.CurrentGear);
			elseif spell.Skill == 'Divine Magic' then
				gcinclude.MoveToCurrent(gProfile.Sets.Divine,gProfile.Sets.CurrentGear);
			elseif spell.Skill == 'Enfeebling Magic' then				
				gcinclude.MoveToCurrent(gProfile.Sets.Enfeebling,gProfile.Sets.CurrentGear);
			elseif spell.Skill == 'Enhancing Magic' then
				gcinclude.MoveToCurrent(gProfile.Sets.Enhancing,gProfile.Sets.CurrentGear);
			elseif spell.Skill == 'Elemental Magic' then
				gcinclude.MoveToCurrent(gProfile.Sets.Elemental,gProfile.Sets.CurrentGear);
			elseif spell.Skill == 'Ninjutsu' then
				gcinclude.MoveToCurrent(gProfile.Sets.Ninjutsu,gProfile.Sets.CurrentGear);
			elseif spell.Skill == 'Summoning' then	
				gcinclude.MoveToCurrent(gProfile.Sets.Summoning,gProfile.Sets.CurrentGear);
			end

			-- See if Magic Attack Bonus useful. It only affects offensive spells. (In the case
			-- of dia or bio, it only affects the initial hit and not the dot aspects of those
			-- spells.) Ninjutsu is affected by Ninjutsu Magic Attack Bonus. Filter out the
			-- easy ones even though, in certain circumstances, some of these would be positively
			-- affected by MAB.
			
			if string.find('Healing Magic,Enfeebling Magic,Enhancing Magic,Ninjutsu,Summoning,Singing',spell.Skill) == nil then
				gcinclude.MoveToCurrent(gProfile.Sets.MAB,gProfile.Sets.CurrentGear);
			end
		elseif cKey == 'E' then			--Magical accuracy
			gcinclude.FractionalAccuracy(gProfile.Sets.Macc,nil);
		elseif cKey == 'F' then			-- Spell specific gear
			if string.match(spell.Name, 'Stoneskin') then
				-- Mind has a large affect on Stoneskin, so equip it here
				if bTank == true then
					gcinclude.MoveToCurrent(gProfile.Sets.TANK_MND,gProfile.Sets.CurrentGear);
				else				
					gcinclude.MoveToCurrent(gProfile.Sets.MND,gProfile.Sets.CurrentGear);
				end
				-- Now load the specific stoneskin set	
				gcinclude.MoveToCurrent(gProfile.Sets.Stoneskin,gProfile.Sets.CurrentGear);
			elseif string.match(spell.Name, 'Drain') then
				gcinclude.MoveToCurrent(gProfile.Sets.Drain,gProfile.Sets.CurrentGear);
			elseif string.match(spell.Name, 'Aspir') then
				gcinclude.MoveToCurrent(gProfile.Sets.Aspir,gProfile.Sets.CurrentGear);
			elseif string.match(spell.Name, 'Sneak') then
				gcinclude.MoveToCurrent(gProfile.Sets.Sneak,gProfile.Sets.CurrentGear);
			elseif string.match(spell.Name, 'Invisible') then
				gcinclude.MoveToCurrent(gProfile.Sets.Invisible,gProfile.Sets.CurrentGear);
			end
		elseif cKey == 'G' then				-- Elemental Obi
			if spell.Skill ~= 'Summoning' then
				obi = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.OBI);
				if obi ~= nil then
					gProfile.Sets.CurrentGear['Waist'] = obi;
				end
			end
		elseif cKey == 'H' then				-- Elemental Stave
			if spell.Skill == 'Summoning' then
				stat = gcinclude.CheckSummons(spell.Name);
			else
				stat = gcinclude.CheckEleSpells(spell.Name,gcinclude.MagicEleDmg,gcinclude.ELEMENT);
			end
		
			if stat ~= nil then
				gcinclude.SwapToStave(stat,false,gProfile.Sets.CurrentGear);
			end
			stat = nil;
		end
	end
end		-- gcinclude.HandleMidcast
	
--[[
	HandleWeaponskill loads the gear appropriately for the weapon skill you're doing
--]]

function gcinclude.HandleWeaponskill(bTank)
	local ws = gData.GetAction();
	
	if bTank == nil then	-- Need to check because of transition state of change
		bTank = false;
	end
		
 	gcinclude.settings.priorityWeaponSkill = string.upper(gcinclude.settings.priorityWeaponSkill);
	for i = 1,string.len(gcinclude.settings.priorityWeaponSkill),1 do
		cKey = string.sub(gcinclude.settings.priorityWeaponSkill,i,i);
		if cKey == 'A' then			-- weaponskill set
			local sWS = gcinclude.WsStat(ws.Name,'STR');

			if sWS == 'WS_STR' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STR,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_STRAGI' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STRAGI,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_STRDEX' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STRDEX,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_STRINT' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STRINT,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_STRINT_30_20' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STRINT_30_20,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_STRMND' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STRMND,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_STRMND_30_50' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STRMND_30_50,gProfile.Sets.CurrentGear);					
			elseif sWS == 'WS_STRVIT' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_STRVIT,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_AGI' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_AGI,gProfile.Sets.CurrentGear);		
			elseif sWS == 'WS_CHR' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_CHR,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_DEX' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_DEX,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_DEXAGI' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_DEXAGI,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_DEXCHR' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_DEXCHR,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_DEXINT' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_DEXINT,gProfile.sets.CurrentGear);
			elseif sWS == 'WS_INT' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_INT,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_INTMND' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_INTMND,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_MND' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_MND,gProfile.Sets.CurrentGear);
			elseif sWS == 'WS_VIT' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_VIT,gProfile.Sets.CurrentGear);			
			elseif sWS == 'WS_HP' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_HP,gProfile.Sets.CurrentGear);						
			elseif sWS == 'WS_Skill' then
				gcinclude.MoveToCurrent(gProfile.Sets.WS_Skill,gProfile.Sets.CurrentGear);	
			end
		elseif cKey == 'B' then		-- elemental gorget	
			local bFound = false;
			for ii,jj in pairs(gcinclude.eleWS) do
				if string.find(jj,ws.Name) ~= nil then
					local sGorget = gcinclude.CheckForEleGear('gorget',ii);
					if sGorget ~= nil and bFound == false then
						gProfile.Sets.CurrentGear['Neck'] = sGorget;
						bFound = true;
					end
				end
			end		
		elseif cKey == 'D' then		-- accuracy	
			if gcdisplay.GetToggle('acc') == true then
				gcinclude.FractionalAccuracy(gProfile.Sets.Accuracy,gProfile.Sets.Tank_Accuracy);
			end
		elseif cKey == 'E' then		-- elemental obi
--[[
			If the weaponskill is elemental and is closing a skillchain, then if the
			conditions for equipping an elemental obi are advantageous, it should be
			equipped now. Unfortunately I have no idea how to detect the closing of
			a skillchain and the automatic equipment of an elemental obi could 
			adversely affect the damage, so this section is not implemented. If I can
			ever figure out how to detect closing a skillchain, I will readdress this.
															- CCF, 1/12/2024
--]]	
		end				
	end
	
	-- Special case(s) for specific weapon skills go here
	ws.Name = string.lower(ws.Name);
	if string.find('red lotus blade,sanguine blade',ws.Name) ~= nil then
		gcinclude.MoveToCurrent(gProfile.Sets.MAB,gProfile.Sets.CurrentGear);	
	end	
end		-- gcinclude.HandleWeaponskill

return gcinclude;