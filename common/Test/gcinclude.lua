local gcinclude = T{};

require 'common'
	
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
	The Sneaky set is equipped and the slots are locked. It's a set intended to equip gear
	to help the player sneak around.
--]]	
	['Sneaky'] = {
		Hands = 'Dream Mittens +1',
		Feet  = 'Dream Boots +1',
	},

--[[
	The dispense set is used to equip items that have an ability to daily dispense items.
	They're grouped here as a convenience.
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
	to see how this is done.
--]]
	Messages = false; 	 -- set to true if you want chat log messages to appear on any /gs command used such as DT, or KITE gear toggles, certain messages will always appear
	WScheck = true; 	 -- set to false if you don't want to use the WSdistance safety check
	WSdistance = 4.7; 	 -- default max distance (yalms) to allow non-ranged WS to go off at if the above WScheck is true
	bWSOverride = false; -- is the player playing a job where weapon swapping always happens, it is not optional?
	Tolerance = 97;		 -- Comparison value %, cut-off for certain comparisons
	DefaultSpellTarget = 't'; -- What to use in MaxSpell if no target specified
	DefaultSongTarget = 't';  -- What to use in MaxSong if no target specified
	--
	priorityEngaged = 'CEF'; 		-- indicates order of steps for engagement
	priorityMidCast = 'ABCDEFGH';	-- indicates order of steps for spell midcast
	priorityWeaponSkill = 'ADBE';	-- indicates order of steps for a weapon skill
};

-- The following arrays are used by the functions contained in this file. Probably best to leave them alone

gcdisplay = gFunc.LoadFile('common\\gcdisplay.lua');

gcinclude.AliasList = T{'acc','ajug','db','dt','ei','equipit','eva','gc','gcmessages','gearset','gs','gswap','help','horn','idle','kite','lock','maxsong','maxspell','nac','petfood','rc','rv','sbp','showit','string','tank','th','unlock','wsdistance','wswap','t1'};
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
gcinclude.SmnMagical = T{'Searing Light','Meteorite','Holy Mist','Inferno','Fire II','Fire IV','Meteor Strike','Conflag Strike','Diamond Dust','Blizzard II','Blizzard IV','Heavenly Strike','Aerial Blast','Aero II','Aero IV','Wind Blade','Earthen Fury','Stone II','Stone IV','Geocrush','Judgement Bolt','Thunder II','Thunder IV','Thunderstorm','Thunderspark','Tidal Wave','Water II','Water IV','Grand Fall','Howling Moon','Lunar Bay','Ruinous Omen','Somnolence','Nether Blast','Night Terror','Level ? Holy','Burning Strike'};
gcinclude.SmnAccuracy = T{'Healing Ruby','Healing Ruby II','Whispering Wind','Spring Water','Diamond Storm','Sleepga','Shock Squall','Slowga','Tidal Roar','Pavor Nocturnus','Ultimate Terror','Nightmare','Mewing Lullaby','Eerie Eye'};
gcinclude.SmnHybrid = T{'Flaming Crush'};
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

gcinclude.BarElementSpells = T{ 'baraero', 'baraera', 'barblizzard', 'barblizzara', 'barfire', 'barfira', 'barstone', 'barstonera', 'barthunder', 'barthundra', 'barwater', 'barwatera' };
gcinclude.AbsorbDarkSpells = T{ 'absorb-agi', 'absorb-chr', 'absorb-dex', 'absorb-int', 'absorb-mnd', 'absorb-str', 'absorb-vit', 'absorb-acc', 'absorb-tp' };
--[[
	The following two variables are used to store the invoked type of craft/gather type
--]]
gcinclude.Craft=nil;
gcinclude.Gather=nil;
--[[
	The following define all the weaponskills according to the desired stats
--]]

gcinclude.tWeaponSkills = {
	['AGI']    = { 'hot shot', 'split shot', 'sniper shot', 'slugshot', 'blast shot', 
				   'heavy shot', 'detonator' },
	['CHR']    = { 'shadowstitch' },
	['DEX']    = { 'wasp sting', 'viper bite', 'blade: metsu', 'dancing edge' },
	['DEXAGI'] = { 'shark bite', 'coronach' },
	['DEXCHR'] = { 'eviseration' },
	['DEXINT'] = { 'gust slash', 'cyclone' },
	['INT']    = { 'gate of tartarus' },
	['INTMND'] = { 'spirit taker' },
	['MND']    = { 'energy steal', 'energy drain' },
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
	['STRAGI'] = { 'sickle moon', 'vorpal thrust', 'flaming arrow', 'piercing arrow',
				   'dulling arrow', 'sidewinder', 'blast arrow', 'arching arrow',
				   'empyreal arrow', 'namas arrow' },
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
gcinclude._sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,SCH,GEO,RUN';

-- The following structure is used for locks and accuracy
gcinclude.tLocks = { 
		 [1] = { ['slot'] = 'main',  ['lock'] = false, ['acc'] = false },
		 [2] = { ['slot'] = 'sub',   ['lock'] = false, ['acc'] = false }, 
		 [3] = { ['slot'] = 'range', ['lock'] = false, ['acc'] = false },
		 [4] = { ['slot'] = 'ammo',  ['lock'] = false, ['acc'] = false },
		 [5] = { ['slot'] = 'head',  ['lock'] = false, ['acc'] = false },  
		 [6] = { ['slot'] = 'neck',  ['lock'] = false, ['acc'] = false },
		 [7] = { ['slot'] = 'ear1',  ['lock'] = false, ['acc'] = false },  
		 [8] = { ['slot'] = 'ear2',  ['lock'] = false, ['acc'] = false }, 
		 [9] = { ['slot'] = 'body',  ['lock'] = false, ['acc'] = false },  
		[10] = { ['slot'] = 'hands', ['lock'] = false, ['acc'] = false }, 
		[11] = { ['slot'] = 'ring1', ['lock'] = false, ['acc'] = false }, 
		[12] = { ['slot'] = 'ring2', ['lock'] = false, ['acc'] = false },
		[13] = { ['slot'] = 'back',  ['lock'] = false, ['acc'] = false },  
		[14] = { ['slot'] = 'waist', ['lock'] = false, ['acc'] = false },
		[15] = { ['slot'] = 'legs',  ['lock'] = false, ['acc'] = false },  
		[16] = { ['slot'] = 'feet',  ['lock'] = false, ['acc'] = false }
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
										['Affinity'] = { 'blaze','burn','firaga','fire','flare' },
										['SongAffinity'] = { 'ice threnody' },
										['Summons'] = { 'ifrit','fire spirit','firespirit','fire' }
										},
									['ice'] = {
										['Weak'] = 'fire',
										['NQ'] = { ['Name'] = 'Ice staff', ['Ref'] = {} },
										['HQ'] = {['Name'] = 'Aquilo\'s staff', ['Ref'] = {} },
										['Affinity'] = { 'blizzaga','blizzard','freeze','frost','ice' },
										['SongAffinity'] = { 'wind threnody' },
										['Summons'] = { 'shiva','ice spirit','icespirit','ice' },
										},
									['wind'] = {
										['Weak'] = 'ice',
										['NQ'] = { ['Name'] = 'Wind staff', ['Ref'] = {} },
										['HQ'] = { ['Name'] = 'Auster\'s staff', ['Ref'] = {} },
										['Affinity'] = { 'aero','aeroga','choke','tornado' },
										['SongAffinity'] = { 'earth threnody' },
										['Summons'] = { 'garuda','air spirit','airspirit','air','siren' },
										},
									['earth'] = { 
										['Weak'] = 'wind',
										['NQ'] = { ['Name'] = 'Earth staff', ['Ref'] = {} },
										['HQ'] = { ['Name'] = 'Terra\'s staff', ['Ref'] = {} },
										['Affinity'] = { 'quake','rasp','stone','stonega' },
										['SongAffinity'] = { 'lightning threnody', 'battlefield elegy', 'carnage elegy' },
										['Summons'] = {'titan','earth spirit','earthspirit','earth' },
										},
									['thunder'] = {
										['Weak'] = 'earth',
										['NQ'] = { ['Name'] = 'Thunder staff', ['Ref'] = {} },
										['HQ'] = { ['Name'] = 'Jupiter\'s staff', ['Ref'] = {} },
										['Affinity'] = { 'burst','shock','thundaga','thunder' },
										['SongAffinity'] = { 'water threnody' },
										['Summons'] = { 'ramuh','thunder spirit','thunderspirit','thunder' },
										},
									['water'] = {
										['Weak'] = 'thunder',
										['NQ'] = { ['Name'] = 'Water staff', ['Ref'] = {} },
										['HQ'] = { ['Name'] = 'Neptune\'s staff', ['Ref'] = {} },
										['Affinity'] = { 'drown','flood','poison','water','waterga' },
										['SongAffinity'] = { 'fire threnody' },
										['Summons'] = { 'leviathan','water spirit','waterspirit','water' },
										},
									['light'] = { 
										['Weak'] = 'dark',
										['NQ'] = { ['Name'] = 'Light staff', ['Ref'] = {} },
										['HQ'] = { ['Name'] = 'Apollo\'s staff', ['Ref'] = {} },
										['Affinity'] = { 'banish','banishga','curaga','cure','dia','diaga','flash','holy' },
										['SongAffinity'] = { 'dark threnody', 'foe requiem', 'foe requiem ii', 'foe requiem iii', 'foe requiem iv', 'foe requiem v', 'foe requiem vi', 'foe lullaby', 'horde lullaby', 'magic finale', 'maiden\'s virelai' },
										['Summons'] = {'carbuncle','light spirit','lightspirit','light','cait sith','caitsith','alexander'},
										},
									['dark'] = {
										['Weak'] = 'light',
										['NQ'] = { ['Name'] = 'Dark staff', ['Ref'] = {} },
										['HQ'] = { ['Name'] = 'Pluto\'s staff', ['Ref'] = {} },
										['Affinity'] = { 'aspir','blind','bio','drain','sleep','sleepga' },
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
										['MEacc'] = { 'burn','firaga','fire','flare','blaze' },
										['eleWS'] = { 'burning blade','red lotus blade','tachi: Kagero','flaming arrow','hot shot','wildfire' },
									},
									['ice'] = {
										['Weak'] = 'fire',										
										['Name'] = 'Hyorin obi', 
										['Ref'] = {},
										['MEacc'] = { 'frost','blizzaga','blizzard','freeze','paralyze','bind','distract','ice' },
										['eleWS'] = { 'frostbite','freezebite','herculean slash','blade: to' },
										['Other'] = 'elemental magic',
										},												 
									['wind'] = {
										['Weak'] = 'ice',
										['Name'] = 'Furin obi', 
										['Ref'] = {},
										['MEacc'] = { 'choke','aero','aeroga','tornado','silence','gravity','flurry' },
										['eleWS'] = { 'gust slash','cyclone','aeolian edge','tachi: jinpu' },
										},												 
									['earth'] = { 
										['Weak'] = 'wind',
										['Name'] = 'Dorin obi', 
										['Ref'] = {},
										['MEacc'] = { 'rasp','quake','stone','stonega','slow' },
										['eleWS'] = { 'blade: chi','rock crusher','earth crusher' },
										},
									['thunder'] = { 
										['Weak'] = 'earth',
										['Name'] = 'Rairin obi', 
										['Ref'] = {},
										['MEacc'] = { 'shock','burst','thundaga','thunder','stun' },
										['eleWS'] = { 'cloudsplitter','thunder thrust','raiden thrust','tachi: goten' },
										},
									['water'] = { 
										['Weak'] = 'thunder',
										['Name'] = 'Suirin obi', 
										['Ref'] = {},
										['MEacc'] = { 'drown','flood','water','waterga','poison' },
										['eleWS'] = { 'blade: teki','blade: yu' },
										['Other'] = 'divine magic',
										},
									['light'] = {
										['Weak'] = 'dark',
										['Name'] = 'Korin obi', 
										['Ref'] = {},
										['MEacc'] = { 'banish','banishga','dia','diaga','flash','repose','holy','auspice','esuna','sacrifice','reprisal','cure','curaga' },
										['eleWS'] = { 'shining blade','seraph blade','primal rend','tachi: koki','shining strike','seraph strike','starburst','sunburst','garland of bliss','trueflight' },
										['Other'] = 'cure potency',
										},
									['dark'] = {
										['Weak'] = 'light',
										['Name'] = 'Anrin obi', 
										['Ref'] = {},
										['MEacc'] = { 'blind','bio','sleep','dispel','frazzle','drain','warp','tractor','aspir','escape','sleep','sleepga','retrace' },
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

-- Listed below are all the spells that are affected by INT or MND
gcinclude.tStatMagic = T{
	['int'] = {'INT','aero,aeroga,bind,blaze,blind,blizzaga,blizzard,burst,dread,firaga,fire,flare,flood,freeze,ice,quake,shock,stone,stonega,thundaga,thunder,tornado,water,waterga,katon,hyoton,huton,doton,raiton,suiton'},
	['mnd'] = {'MND','banish,distract,frazzle,silence,paralyze,slow,cure,curaga,cura'},
};

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

--[[
	This table contains a list of all of the spells that have multiple versions where
	the intensity is the only change. Included is what job can cast the spell and at 
	what level, MP cost, and Spell ID.
	
	Please note that entries that will be included when Treasures of Aht Urgan is released
	are currently commented out.
--]]

gcinclude.GearWarnings = '';
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
gcinclude.tEquipIt = {
	['emp']    = { ['Name'] = 'Empress Band', ['Slot'] = 'Ring' },
	['cha']    = { ['Name'] = 'Chariot Band', ['Slot'] = 'Ring' },
	['empo']   = { ['Name'] = 'Emperor Band', ['Slot'] = 'Ring' },
	['ann']    = { ['Name'] = 'Anniversary Ring', ['Slot'] = 'Ring' },
	['dem']    = { ['Name'] = 'Dem Ring', ['Slot'] = 'Ring' },
	['mea']    = { ['Name'] = 'Mea Ring', ['Slot'] = 'Ring' },
	['holla']  = { ['Name'] = 'Holla Ring', ['Slot'] = 'Ring' },
	['altep']  = { ['Name'] = 'Altepa Ring', ['Slot'] = 'Ring' },	
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
	['mandy']  = { ['Name'] = 'Mandra. Suit', ['Slot'] = 'Body', ['aSlots'] = 'Body,Legs' }
};

-- List of items that inhibit more than the obvious gear slot. Add entries as you
-- need to, to account for the gear you use.
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

-- The following is a list of all valid conditional operators used in the following
-- inline codes: //MP, //MPP, //HP, //HPP, //TP, and //TPP.
InlineConditionals = { '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.' };

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
	['Lumoria']			= { ['own'] =  0, ['zones'] = {33,34,35,36,37,38}}
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
	['main']  = { ['num'] = 0, ['vis'] = true, {} },		
	['sub']   = { ['num'] = 0, ['vis'] = true, {} },
	['range'] = { ['num'] = 0, ['vis'] = true, {} },
	['ammo']  = { ['num'] = 0, ['vis'] = true, {} },
	['head']  = { ['num'] = 0, ['vis'] = true, {} },
	['neck']  = { ['num'] = 0, ['vis'] = false, {} },
	['ears']  = { ['num'] = 0, ['vis'] = false, {} },
	['body']  = { ['num'] = 0, ['vis'] = true, {} },
	['hands'] = { ['num'] = 0, ['vis'] = true, {} },
	['rings'] = { ['num'] = 0, ['vis'] = false, {} },
	['back']  = { ['num'] = 0, ['vis'] = false, {} },
	['waist'] = { ['num'] = 0, ['vis'] = false, {} },
	['legs']  = { ['num'] = 0, ['vis'] = true, {} },
	['feet']  = { ['num'] = 0, ['vis'] = true, {} }
};
	
gcinclude.OwnNation = -1; 
gcinclude.fb = false;

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
	fSummonerPet determines if the player has a SMN summoned pet. Returned is true
	or false
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
	
	print(chat.message(' '));
	print(chat.message('Settings'));
	print(chat.message('--------'));
	print(chat.message('Job: ' .. player.MainJob .. '/' .. player.SubJob));
	print(chat.message('Level: ' .. tostring(player.MainJobSync) .. '(' .. tostring(player.MainJobLevel) .. ')'));	
	print(chat.message(' '));
	print(chat.message('WScheck: ' .. tostring(gcinclude.settings.WScheck)));
	print(chat.message('WSdistance: ' .. tostring(gcinclude.settings.WSdistance)));
	print(chat.message('bWSOverride: ' .. tostring(gcinclude.settings.bWSOverride)));
	print(chat.message(' '));
	GearCheckList();
end		-- DB_ShowIt
	
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
	summoned avatar/spirit and returns it. If the pet is unknown or from another
	job, nil is returned
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
	
	-- Now make sure the reference to the dynamic gear table is set
	-- Remember, staves have two references and obi/gorget have one
	if sType == 'staff' then
		bGood,gcinclude.tElemental_gear[sType][sElement]['HQ']['Ref'] = 
			fGearCheckItem('main',gcinclude.tElemental_gear[sType][sElement]['HQ']['Name'],false,false);
		bGood,gcinclude.tElemental_gear[sType][sElement]['NQ']['Ref'] = 
			fGearCheckItem('main',gcinclude.tElemental_gear[sType][sElement]['NQ']['Name'],false,false);
	else	-- obis and gorgets have the same structure, so are combined here
		bGood,gcinclude.tElemental_gear[sType][sElement]['Ref'] = 
			fGearCheckItem('main',gcinclude.tElemental_gear[sType][sElement]['Name'],false,false);
	end

	-- Since we now know that the links to the dynamic table are there, process
	-- the reference accordingly. For staff, check for HQ before looking at NQ
	if sType == 'staff' then
		if gcinclude.tElemental_gear[sType][sElement]['HQ']['Ref']['accessible'] == true then
			return gcinclude.tElemental_gear[sType][sElement]['HQ']['Name'];
		elseif gcinclude.tElemental_gear[sType][sElement]['NQ']['Ref']['accessible'] == true then
			return gcinclude.tElemental_gear[sType][sElement]['NQ']['Name'];
		else
			return nil;
		end
	else
		-- Obi and Gorget have the same structure, so handle the same way
		if gcinclude.tElemental_gear[sType][sElement]['Ref']['accessible'] == true	then
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
	gcdisplay.CreateToggle('WSwap',(string.find('WHM,BRD',player.MainJob) ~= nil));
	
	-- Tank and Idle
	if string.find('PLD,NIN,RUN',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',true);
		gcdisplay.CreateToggle('Idle',true);
	elseif string.find('DRK,WAR,THF,RDM,BLU',player.MainJob) ~= nil then
		gcdisplay.CreateToggle('Tank',false);
		gcdisplay.CreateToggle('Idle',true);
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
	
	print(chat.message('FYI: Remember to do a /gc when \'data download\' finishes'));
end		-- SetVariables

--[[
	fGearCheckItem process the specific item sent to it and where appropriate, populates
	gcinclude.GearDetails. The details tracked are: item name, item level, can equip?, 
	and accessibility. Returned is a true/false which indicates that the item's level,
	job, and accessibility is valid for equipping. Please note that an invalid item 
	will return false.
--]]

function fGearCheckItem(sSlot,sName,bAccess,bForce)
	local player = gData.GetPlayer();
	local bJob,bAccessible,bThere;
	local iPos,iCnt;
	local item = {};
	local rec = {};
	
	-- Subsets are skipped
	if string.lower(sSlot) == 'subset' then
		return false,nil;
	end
	
	if player.MainJob == nil or player.MainJob == 'NON' then
		return false,nil;
	end
	
	if sSlot == nil or sName == nil then
		return false,nil;
	end
	
	if bAccess == nil then
		bAccess = false,nil;
	end

	if bForce == nil then
		bForce = false,nil;
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
	
	-- See if item already registered

	if gcinclude.GearDetails[sSlot][sName] == nil or 
		gcinclude.GearDetails[sSlot]['num'] == nil or 
		bForce == true then
		-- Now process the item
		item = AshitaCore:GetResourceManager():GetItemByName(sName,2);
		if item ~= nil then		
			bJob = (bit.band(item.Jobs,gcinclude.JobMask[player.MainJob]) == gcinclude.JobMask[player.MainJob]) or
		  		   (bit.band(item.Jobs,gcinclude.JobMask['Alljobs']) == gcinclude.JobMask['Alljobs']);
			bAccessible = fCheckItemOwned(sName,true,true);

			bThere = (gcinclude.GearDetails[sSlot][sName] ~= nil);
			
			-- Save item w/details
			gcinclude.GearDetails[sSlot][sName] = { 
				['level'] = item.Level, 
				['job'] = bJob, 
				['accessible'] = bAccessible, 
				['desc'] = item.Description[1]
			};
			-- Bump counter
			if not bThere then
				gcinclude.GearDetails[sSlot]['num'] = gcinclude.GearDetails[sSlot]['num'] + 1;
			end
		end
	end
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
	This function will search and extract all the items from all the gear sets in the
	job file and gcinclude, populating gcinclude.GearDetails. The details tracked are: 
	item name, item id, item level, can equip?, and accessibility. This information 
	will be used by the functions for picking which piece to equip, which should speed 
	up the gear swapping and hopefully cut down on the lag, especially in level capped 
	areas.
--]]

function GearCheck(sList,bForce)
	local tTarget = { gProfile.Sets, gcinclude.Sets };
	local ts = {};
	local ref = {};
	local iCnt = 0;
	local bGood;

	if bUpdate == nil then
		bUpdate = false;
	end

	if sList == nil then
		-- Loop the two files
		for s,t in pairs(tTarget) do
			if t == gProfile.Sets then
				print(chat.header('GearCheck'):append(chat.message('Starting to scan the Job file')));
			else
				print(chat.header('GearCheck'):append(chat.message('Starting to scan gcinclude')));
			end
		
			-- Loop the gear sets
			for j,k in pairs(t) do
				if j ~= 'CurrentGear' then			
					-- Loop the gear set slots
					for jj,kk in pairs(k) do
						ts = {};
						-- Entries can be a table or a string. Make either case a table
						if type(kk) == 'table' then
							ts = kk;
						else
							ts[1] = kk;
						end
			
						-- Now walk the list of gear
						for ss,tt in pairs(ts) do						
							-- Save the details if appropriate. Returned results are
							-- ignored, but captured in case I change my mind.
							bGood,ref = fGearCheckItem(jj,tt,false,bForce);
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
						bGood,jj['NQ']['Ref'] = fGearCheckItem('main',jj['NQ']['Name']);
						bGood,jj['HQ']['Ref'] = fGearCheckItem('main',jj['HQ']['Name']);
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
							bGood,jj['Ref'] = fGearCheckItem('waist',jj['Name']);
						else
							bGood,jj['Ref'] = fGearCheckItem('neck',jj['Name']);
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
	GearCheckList will list out the details of the dynamic table.
--]]

function GearCheckList()
	local sMsg;
	
	for i,j in pairs(gcinclude.GearDetails) do
		print(chat.message('Slot: ' .. i));
		for ii,jj in pairs(j) do
			if type(ii) ~= 'number' then
				if string.find('desc,num,vis',ii) == nil then
					print(chat.message('   ' .. ii));
				end
			end
			sMsg = nil;
			if type(jj) == 'table' then 
				for iii,jjj in pairs(jj) do
					if iii ~= 'desc' then
						if sMsg == nil then
							sMsg = iii .. ': '.. tostring(jjj);
						else
							sMsg = sMsg .. ', ' .. iii .. ': '.. tostring(jjj);
						end
					end
				end
				if sMsg ~= nil then
					print(chat.message('   ' .. sMsg));
				end
				print(chat.message(' '));
			end
		end
	end	
end		-- GearCheckList

--[[
	fIsPetNamed determines if that passed pet has the passed name
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
	fMagicSubJob just determines if the main job or the sub job can do magic
--]]

function gcinclude.fMagicalSubJob()
	local player = gData.GetPlayer();
	local mj = player.MainJob;
	local sj = player.SubJob;
	local sList = gcinclude._sMagicJobs;
	
	return (string.find(sList,sj) ~= nil);
end		-- gcinclude.fMagicalSubJob

--[[
	fTallyGear tallies up all the MP and HP manipulations on the gear
	currently equipped (and the passed gear, minus what is already in
	that slot), both visible and invisible slots so that 
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
	
	-- The passed item might not be in the dynamic table yet. Make
	-- sure it is. Also, if it fails the equippable check, then 
	-- there's no reason to go on
	bGood,ref = fGearCheckItem(sSlot,sGear,false);
	if bGood == false then
		return false;
	end
	
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
		-- Make sure that the item is in the dynamic table
		bGood,ref = fGearCheckItem(lcii,sPiece,false);
		if bGood == false then
			return false;
		end
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
	return rec;
end

--[[
	fParseDescriptionExceptions processes the descriptions for gear 
	that requires special processing. It could have been included in
 	fParseDescription, but was extracted so that function would not 
	be too long. Returned is the appropriate record.
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
--]]

function fValidInlineDynamicCode(suCode)
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
end		-- fValidInlineCode

--[[
	fEvalCodedComparison parses the passed conditional and determines if it is 
	true or not. Result is passed back
--]]

function fEvalCodedComparison(sRoot,sOperator,ival,sGear)
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
	elseif sRoot == 'MP'  or sRoot == 'MPP' then	
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
	elseif sRoot == 'HP' or sRoot == 'HPP' then
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
end		-- fEvalCodedComparison

--[[
	fValidSpecial sees if the passed gear's settings are valid. Returned is True
	or False.
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
		if (player.MPP < 51) then
			return true;
		else
			local iMP = player.MP - rec['visible']['cHM'];
			local imMP = player.MaxMP - rec['invisible']['MP'] - 
				rec['invisible']['cMH'] - rec['invisible']['cHM'];
			local iaMP = player.MaxMP * (rec['invisible']['MPP'] * 0.01);
			bGood = ((iMP/(imMP - iaMP))*100 < 51);		
		end
	elseif sGear == 'parade gorget' then
		-- Make sure player needs to have mp added
		if ((player.MP/player.MaxMP) * 100) - gcinclude.settings.Tolerance > 0 then
			return false;
		end
		
		-- Now, check condition: HP% >= 85. Adds "Refresh". Only visible gear
		-- Let's see if the invisible gear will make a difference
		local iHP = player.MaxHP - rec['invisible']['HP'] - rec['invisible']['cMH'];
		local iaHP = math.floor(rec['invisible']['HP'] * (rec['invisible']['HPP'] * 0.01));
		bGood = ((player.HP/(iHP - iaHP))*100 >= 85);
	elseif sGear == 'sorcerer\'s ring' then
		-- Condition: HP% < 76 and TP% < 100. Ignore HP+ (flat and percent) and
		-- Convert HP to MP gear.
		-- Check outright first	
		if (player.HP/player.MaxHP)*100 < 76 and player.TP/10 < 100 then
			return true;
		else
			local fHP = rec['visible']['HP'] + rec['invisible']['HP'];
			local fCHPMP = rec['visible']['cHM'] + rec['invisible']['cHM'];
			local fHPP = math.floor((rec['visible']['HPP'] + 
				rec['invisible']['HPP']) * (fHP + fCHPMP) * 0.01);
			if (player.HP - fHP - fHPP)/player.MaxHP < 76 and player.TP/10 < 100 then
				return true;
			end
		end
	else
		print(chat.header('fValidateSpecial'):append(chat.message('Warning: No special code exists for ' .. gear .. '. Ignoring piece.')));		
	end
	return bGood;
end		-- fValidateSpecial

--[[
	fBuffed determines if the player has the buff/debuff or not. Returned is True/False.
	The passed buff name can be a substring, but that can also lead to miss identifications.
--]]

function fBuffed(test)
	local buffs = AshitaCore:GetMemoryManager():GetPlayer():GetBuffs();

	test = string.lower(test);
	for _, buff in pairs(buffs) do
		local buffString = AshitaCore:GetResourceManager():GetString("buffs.names", buff);
			
		if (buffString) then
			if string.find(buffString,test) ~= nil then
				return true;
			end
		end
	end
	return false;
end

--[[
	fCheckItemOwned determines if the specified piece of gear is owned by 
	the player. bAccessible further restricts the search to containers 
	that are accessible when outside of your mog house. bOnce indicates 
	if the item only has to be found once before returning a result.
	
	Please note: currently searching storage slips are not supported
--]]

function fCheckItemOwned(sGear,bAccessible,bOnce)
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local containerID,itemEntry,item;
	local bFound = false;
	local tStorage = {};
	
	-- Make sure a piece of gear specified
	if sGear == nil then
		return false;
	end
	
	-- If bOnce not specified, then assume true, only one item found is good enough
	if bOnce == nil then
		bOnce = true;
	end
	
	-- If bAccessible not defined or is false, then search all containers
	if bAccessible == nil or bAccessible == false then
		tStorage = gcinclude.STORAGES;
	else
		tStorage = gcinclude.EQUIPABLE;
	end
	
	for i,desc in pairs(tStorage) do
		containerID = desc[1];
		-- then loop through the container
		for j = 1,inventory:GetContainerCountMax(containerID),1 do
			itemEntry = inventory:GetContainerItem(containerID, j);
			if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
				item = resources:GetItemById(itemEntry.Id);
				if string.lower(item.Name[1]) == string.lower(sGear) then
					bFound = true;
					if bOnce == true then
						return true;
					end
				end
			end
		end
	end
	
	return bFound;
end		-- fCheckItemOwned

--[[
	fCheckAccuracySlots determines if the passed slot is one of the designated
	accuracy slots. Please note if the slot is named "ears" or "rings" both
	associated slots will be checked.
--]]

function fCheckAccuracySlots(sSlot)

	sSlot = string.lower(sSlot);
	for i,j in ipairs(gcinclude.tLocks) do
		if j['slot'] == sSlot or 
		   (sSlot == 'ears' and string.find('ear1,ear2',j['slot']) ~= nil and j['acc'] == true) or
		   (sSlot == 'rings' and string.find('ring1,ring2',j['slot']) ~= nil and j['acc'] == true) then
			return true;
		end
	end
	return false;
end		-- fCheckAccuracySlots


--[[
	fCheckInline checks for a simple conditional on the item passed into it.
	Returned is whether the condition is met and the item's name (minus the
	conditional.

	If the slot name is 'subset', then any checks that are on the slot will 
	be ignored and the result passed back will be false
--]]

function fCheckInline(gear,sSlot)
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
		elseif table.find(gcinclude.ExactBuff,string.lower(suCode)) ~= nil then		-- en spells
			bGood = (gData.GetBuffCount(suCode) >= 1);
		elseif table.find(gcinclude.tWeapontypeMelee,suCode) ~= nil then						-- Is main weapon specified type
			bGood = (gSet['Main'] ~= nil and table.find(gProfile.WeaponType[suCode],gSet['Main']) ~= nil);
		elseif table.find(gcinclude.tWeapontypeRange,suCode) ~= nil then						-- Is ranged weapon specified type
			bGood = (gSet['Range'] ~= nil and table.find(gProfile.WeaponType[suCode],gSet['Range']) ~= nil);
		elseif suCode == 'ABSORB' then										-- Spell is an Absorb- type
			bGood = (table.find(gcinclude.AbsorbDarkSpells,string.lower(spell.Name)));
		elseif suCode == 'ACCURACY' then
			if sSlot ~= 'subset' then
				bGood = (fCheckAccuracySlots(sSlot) == true);
			else	-- Invalid inline for a subset
				bGood = false;
			end
		elseif suCode == 'BARSPELL' then					-- Spell is a Bar- type
			bGood = (table.find(gcinclude.BarElementSpells,string.lower(spell.Name)) ~= nil);
		elseif suCode == 'BOUND' then						-- Player is bound
			bGood = fBuffed('Bind');
		elseif suCode == 'BLINDED' then						-- Player is blind
			bGood = fBuffed('Blind');
		elseif suCode == 'CARBY' then						-- Pet is carbuncle
			bGood = (fIsPetNamed('Carbuncle'));
		elseif suCode == 'COVER' then						-- Player has cast cover
			bGood = fBuffed('Cover');
		elseif string.sub(suCode,1,3) == 'CR:' then			-- Crafting
			bGood = (gcinclude.Craft == string.sub(suCode,4,-1));
		elseif suCode == 'CURE' then						-- Cure spell cast
			bGood = (string.find('cure,curaga',fGetRoot(spell.Name)) ~= nil);
		elseif suCode == 'CURSED' then						-- Player is cursed
			bGood = fBuffed('Curse');
		elseif suCode == 'DAYTIME' then						-- Time is daytime
			bGood = gcinclude.CheckTime(timestamp.hour,'Daytime',false);
		elseif string.sub(suCode,1,3) == 'DB:' then
			bGood = (player.MainJob == 'BST' and string.upper(string.sub(suCode,4,-1)) == string.upper(gcdisplay.GetCycle('DB')));	
		elseif suCode == 'DOOMED' then						-- Player is doomed or baned
			bGood = (fBuffed('Doom') or fBuffed('Bane'));
		elseif suCode == 'DT_BREATH' then
			bGood = (gcdisplay.GetCycle('DT') == 'B');
		elseif suCode == 'DT_MAGICAL' then
			bGood = (gcdisplay.GetCycle('DT') == 'M');
		elseif suCode == 'DT_PHYSICAL' then
			bGood = (gcdisplay.GetCycle('DT') == 'P');			
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
		elseif suCode == 'IDLE' then
			if string.find(gcinclude._TankJobList,player.MainJob) ~= nil then
				bGood = gcdisplay.GetToggle('Idle');
			else
				bGood = false;
			end
		elseif string.find(suCode,'IF:') then
			-- Currently only tests the piece currently equipped. I probably
			-- should check the temporary set too at some point.
			if sSlot ~= 'subset' then
				local sCur = gData.GetEquipSlot(sSlot);
				local sItem = string.sub(suCode,4,-1);
				bGood = (string.lower(sItem) == string.lower(sCur));
			else	-- Invalid inline for a subset
				bGood = false;
			end		
--		elseif string.find(suCode,'IFC:') then
--			Needs to be implemented			
		elseif string.find(suCode,'LVLDIV') then			-- Player's level divisable by #
			local iDiv = tonumber(string.sub(suCode,7,-1));
			if iDiv > 0 then
				bGood = (math.floor(player.MainJobSync/iDiv) == player.MainJobSync/iDiv);
			else
				bGood = false;
			end
--		elseif string.find(suCode,'LVL') then
--			Needs to be implemented		
		elseif suCode == 'MSJ' then							-- Magical subjob
			bGood = (string.find(gcinclude._sMagicJobs,sj) ~= nil);
		elseif suCode == 'NEWMOON' then						-- Moon phase: New Moon
			bGood = (environ.MoonPhase == 'New Moon');
		elseif suCode == 'NIGHTTIME' then					-- Time is nighttime
			bGood = gcinclude.CheckTime(timestamp.hour,'Nighttime',false);
		elseif suCode == 'NO_PET' then						-- Player has no avatar out
			bGood = (pet == nil);
		elseif suCode == 'NO_SMNPET' then					-- Player has no or non-smn pet
			bgood = not gcinclude.fSummonerPet();
		elseif suCode == 'NOT_OWN' then						-- Player in area not controlled by their nation
			bGood = (gcdisplay.GetCycle('Region') ~= 'Owned');
		elseif suCode == 'NOT_WSWAP' then					-- WSWAP is disabled
			bGood = (gcinclude.settings.bWSOverride == false or gcdisplay.GetToggle('WSwap') == false);
		elseif string.sub(suCode,1,8) == 'NOT_WTH:' then	-- Does the weather not match
			bGood = (string.find(string.upper(environ.Weather),string.sub(suCode,9,-1)) == nil);
		elseif suCode == 'NOT_WTH-DAY' then					-- Weather does not match day's element
			local sEle = string.upper(environ.DayElement) .. ',NONE';
			bGood = (string.find(sEle,string.upper(environ.WeatherElement)) == nil);
		elseif suCode == 'OWN' then							-- Player in area controlled by their nation
			bGood = (gcdisplay.GetCycle('Region') == 'Owned');
		elseif suCode == 'PARALYZED' then					-- Player is paralyzed
			bGood = fBuffed('Paralysis');
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
			bGood = fBuffed('Petrify');
		elseif string.sub(suCode,1,3) == 'PJP' and string.len(suCode) == 6 then	
			local s = string.sub(suCode,4,-1);
			bGood=(fCheckPartyJob(s,false));		-- party has job: //PJP"job"
		elseif string.sub(suCode,1,3) == 'PJPNM' and string.len(suCode) == 8 then	
			local s = string.sub(suCode,6,-1);
			bGood=(fCheckPartyJob(s,true));		-- party has job: //PJPNM"job", not including player
		elseif suCode == 'POISONED' then					-- Player is poisoned
			bGood = fBuffed('Poison');
		elseif suCode == 'SHINING_RUBY' then				-- Player has shining ruby
			bGood = fBuffed('Shining');	
		elseif suCode == 'SILENCED' then					-- Player is silenced
			bGood = fBuffed('Silence');
		elseif string.sub(suCode,1,2) == 'SJ' and string.len(suCode) == 5 then	
			bGood = (string.sub(suCode,3,-1) == sj);		-- subjob is: //SJ"job"
		elseif suCode == 'SLEPT' then						-- Player is slept
			bGood = fBuffed('Sleep');
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
		elseif suCode == 'SPECIAL' then
			if sSlot ~= 'subset' then
				bGood = fValidateSpecial(sSlot,sGear);
			else	-- Invalid inline for a subset
				bGood = false;
			end		
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
		elseif string.find('TP.,TPP,MP.,MPP,HP.,HPP',string.sub(suCode,1,3)) ~= nil then
			local sRoot,sOperator,ival;
			bGood,sRoot,sOperator,ival = fValidInlineDynamicCode(suCode);		
			if bGood == true then
				bGood = fEvalCodedComparison(sRoot,sOperator,ival,sGear);
			else
				bGood = false;
			end
		elseif suCode == 'UTSUSEMI' then
			bGood = fBuffed('Copy');						-- copy image (#)
		elseif suCode == 'WEAKENED' then					-- Player is weakend
			bGood = (fBuffed('Weakness') or fBuffed('Weakened'));
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
					print(chat.message(i ..' = ' .. jj));
					break;
				end
			end
		end
	end
end		-- RegionControlDisplay

function gcinclude.t1()
	local t = gData.GetTargetIndex();
	local target = gData.GetEntity(t);
	print(target.Name,target.Type,target.Status);
end		-- gcinclude.t1

--[[
	MoveToCurrent copies the gear defined in the passed set to current master
	set. Nothing is displayed, this is just a transfer routine.
	
	Update: support for subsets have been added. The subsets are processed first
	(if present) then the rest of the set. Please note this is a recursive function.
	If nesting goes too deep, it will run out of memory.
--]]

function gcinclude.MoveToCurrent(tSet,tMaster,bOverride)
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
	
	-- First walk through the gear slots looking for "subset"
	for k,v in pairs(ts1) do
		sK = string.lower(k);
		
		if sK == 'subset' then		
			if type(v) == 'table' then
				ts = v;
			else
				ts[k] = v;
			end
			
			-- Then determine the appropriate set to load
			for kk,vv in pairs(ts) do		
				bGood,vRoot = fCheckInline(vv,'subset');			
				if bGood == true then
					gcinclude.MoveToCurrent(vRoot,tMaster,bOverride);
					break;
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
			-- is true.	
			if string.find('main,sub,range',sK) ~= nil then
				bSkip = not (gcdisplay.GetToggle('WSwap') == true 
						or gcinclude.settings.bWSOverride == true
						or bOverride == true);
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

				-- Walk list of items
				for kk,vv in pairs(ts) do
					-- Make sure the item is noted in gcinclude.GearDetails
					-- and that the level, job, and accessibility is good
					bG,ref = fGearCheckItem(sK,vv,false);
					if bG == true then
						-- See if there's an inline conditional to be checked.
						-- Note the need to distinguish which "ear" or "ring"
						if bContinue then
							stK = root .. tostring(iNum);
						else
							stK = k;
						end

						bGood,vRoot = fCheckInline(vv,stK);

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
	EquipTheGear makes sure that the passed gear set doesn't have an item in a slot
	that is being blocked by another item (e.g., no head gear if a vermillion cloak
	is in the body slot.) It the equips the gear set.
--]]

function gcinclude.EquipTheGear(tSet,bOverride)
	local sSlot;

	if tSet == nil then
		return;
	end
	
	if bOverride == nil then
		bOverride = false;
	end
	
	-- Then deal with the multislot items
	for j,k in pairs(gcinclude.multiSlot) do
		if tSet[k['slot']] ~= nil and string.lower(tSet[k['slot']]) == string.lower(k['item']) then
			tSet[k['affected']] = '';
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
	
	gFunc.ForceEquipSet(tSet);
end			-- gcinclude.EquipTheGear

--[[
	CheckLockAccCollision checks to see if any of the slots associated with locks
	clashes with slots associated with the accuracy. An appropriate warning is
	issued if there's a problem.
--]]

function gcinclude.CheckLockAccCollision(sFrom)
	sFrom = string.lower(sFrom);
	
	for i,j in ipairs(gcinclude.tLocks) do
		if j['lock'] == true and j['acc'] == true then
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
	designated for accuracy gear. This is needed to determine if both associated slots
	need to be populated or just one of the slots.
--]]

function getPairedAccuracySlotValues(sSlot)
	local bS1 = false;
	local bS2 = false;
	local root = string.sub(string.lower(sSlot),1,-2);
	
	for i,j in ipairs(gcinclude.tLocks) do
		if j['slot'] == root .. '1' and j['acc'] == true then
			bS1 = true;
		elseif j['slot'] == root .. '2' and j['acc'] == true then
			bS2 = true;
		end
	end
	return bS1,bS2;
end		-- getPairedAccuracySlotValues

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
					if type(k) == 'table' then
						ts = k;
					else
						ts[j] = k;
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
end	-- gcinclude.FractionalSet

--[[
	FractionalAccuracy uses the stored accuracy slots and builds an equipment table
	(from the appropriate accuracy set) and then equips said table. It is a replacement
	for that On/Off accuracy implementation that was originally developed. This new
	approach lets the user (through the /acc and /nac commands) specify which slots
	accuracy gear should be equipped. This "fractional" approach lets the user decide
	how much accuracy gear should be equipped.
--]]

function gcinclude.FractionalAccuracy(accTbl,tankAccTbl)
	local src,t,vRoot,bGood;
	local s1,s2;
	local tAcc = {};
	local ts = {};
	local bSubset = false;
	local bFound = false;
	
	if gcinclude.AccNumeric == 'None' then
		return;
	end
	
	-- Now determine which table to use
	if gcdisplay.GetToggle('Tank') ~= nil and 
	   gcdisplay.GetToggle('Tank') == true and 
	   tankAccTbl ~= nil then
		if type(tankAccTbl) == 'string' then
			src = fGetTableByName(tankAccTbl);
		else
			src = tankAccTbl;
		end
	else
		if type(tankAccTbl) == 'string' then
			src = fGetTableByName(accTbl);
		else
			src = accTbl;
		end
	end
	
	for i,j in pairs(src) do
		t = string.lower(i);
		if t == 'subset' then
			bSubset = true;
		else
			-- Special case for ears and rings. Deal with them
			if t == 'ears' then
				s1,s2 = getPairedAccuracySlotValues('ears');
				if s1 == true and s2 == true then
					tAcc[i] = j;
					bFound = true;
				elseif s1 == true then
					tAcc['Ear1'] = j;
					bFound = true;
				elseif s2 == true then
					tAcc['Ear2'] = j;
					bFound = true;
				end
			elseif t == 'rings' then
				s1,s2 = getPairedAccuracySlotValues('rings');
				if s1 == true and s2 == true then
					tAcc[i] = j;
					bFound = true;					
				elseif s1 == true then
					tAcc['Ring1'] = j;
					bFound = true;					
				elseif s2 == true then
					tAcc['Ring2'] = j;
					bFound = true;
				end					
			else
				-- Normal slots. Match it up
				for ii,jj in pairs(gcinclude.tLocks) do			
					if t == jj['slot'] and jj['acc'] == true then
						tAcc[i] = j;
						bFound = true;
					end
				end
			end
		end
	end
	
	if bFound == true then
		gcinclude.MoveToCurrent(tAcc,gProfile.Sets.CurrentGear);
	else
		if bSubset == true then
			for i,j in pairs(src) do
			t = string.lower(i);
			if t == 'subset' then
				if type(j) == 'table' then
						ts = k;
					else
						ts[j] = k;
					end
					
					-- Then determine the appropriate set to load
					for kk,vv in pairs(ts) do
						bGood,vRoot = fCheckInline(vv,'subset');
						if bGood == true then
							if gcdisplay.GetToggle('Tank') ~= nil and 
							   gcdisplay.GetToggle('Tank') == true then
								gcinclude.FractionalAccuracy(accTbl,vRoot);
							else
								gcinclude.FractionalAccuracy(vRoot,tankAccTbl);
							end
							break;
						end
					end
				end
			end
		end
	end
end		-- gcinclude.FractionalAccuracy

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
					print(chat.header('MaxSpell'):append(chat.message('FYI: You should be able to cast'.. j['Name'] .. ', but don\'t know it. Skipping')));
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
--]]

function gcinclude.fCheckForElementalGearByValue(sWhat,sWhich,sElement)
	local player = gData.GetPlayer();
	local sRoot,bGood,sTarget;

	-- Make sure locks won't block equipping the item
	if sWhat == 'staff' and (gcinclude.fIsLocked('main') == true or gcinclude.fIsLocked('sub') == true) then
		return nil;
	elseif sWhat == 'obi' and gcinclude.fIsLocked('waist') == true then
		return nil;
	elseif gcinclude.fIsLocked('neck') == true then -- gorget
		return nil;
	end
		
	-- What's searched for is sometimes a "root" and other times an "as-is"
	if string.find('Affinity,MEacc',sWhich) ~= nil then
		sRoot = fGetRoot(sElement);	
	elseif string.find('Summons,eleWS,SongAffinity',sWhich) ~= nil then
		sRoot = string.lower(sElement);
	else
		print(chat.header('fCheckForElementalGearByValue'):append(chat.message('Unknown field to search: ' ..sWhich)));
		return nil;
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
					-- Then determine if there's a staff that matches
					if gcinclude.tElemental_gear[sWhat][i]['HQ']['Ref']['accessible'] == true then
						return gcinclude.tElemental_gear[sWhat][i]['HQ']['Name'];
					elseif gcinclude.tElemental_gear[sWhat][i]['NQ']['Ref']['accessible'] == true then
						return gcinclude.tElemental_gear[sWhat][i]['NQ']['Name'];
					else
						return nil;
					end
				end
			elseif sWhat == 'obi' or sWhat == 'gorget' then
				if table.find(gcinclude.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
					bGood,gcinclude.tElemental_gear[sWhat][i]['Ref'] = 
						fGearCheckItem(sTarget,gcinclude.tElemental_gear[sWhat][i]['Name'],false,false);
				end
				
				-- Then determine if there's an obi or gorget that matches
				if gcinclude.tElemental_gear[sWhat][i]['Ref']['accessible'] == true then
					return gcinclude.tElemental_gear[sWhat][i]['Name'];
				end
			end
		end
	end
	-- Since we got here, either the search string wasn't found in the appropriate
	-- area or it was found, but the player doesn't have the item or it's inaccessible.
	return nil;
end		-- fCheckForElementalGearByValue

--[[
	fSwapToStave determines if swapping your weapon out for one of the elemental staves makes
	sense and does it for you while remembering what weapon/offhand you had equipped.
--]]

function gcinclude.fSwapToStave(sStave,noSave,cs)
	local ew = gData.GetEquipment();
	local player = gData.GetPlayer();
	local sGear;
	local eWeap = nil;
	local eOff = nil;

	-- This is needed for a timing issue
	if sStave == nil then
		return;
	end
	
	-- Now, make sure that locks will not prevent equipping a staff
	if gcinclude.fIsLocked('main') == true or gcinclude.fIsLocked('sub') == true then
		return;
	end
	
	-- Now, process the stave swap
	if ew['Main'] ~= nil then
		eWeap = ew['Main'].Name;
	end
	
	if ew['Sub'] ~= nil then
		eOff = ew['Sub'].Name;
	end;

	if (gcdisplay.GetToggle('WSwap') == true or gcinclude.settings.bWSOverride == true) then	
		-- See if a current weapon is the one of the targetted staves
		if not (eWeap == nil or (eWeap ~= nil and string.lower(eWeap) == string.lower(sStave))) then
			-- save the weapon so it can be equipped again
			if eWeap ~= gcinclude.weapon and noSave == false and gcinclude.settings.bWSOverride == false then
				gcinclude.weapon = eWeap;
				gcinclude.offhand = eOff;
			end
		end
		
		if sStave ~= nil then
			-- Check versus level of player.
			if player.MainJobSync >= gcinclude.tElemental_gear['staff']['level'] or
				gcinclude.fIsLocked('main')	== true then
				cs['Main'] = sStave;
			else
				local msg = 'Warning: Unable to swap to a ' .. sStave .. ' due to level or locks!';
				if string.find(gcinclude.GearWarnings,msg) == nil then
					print(chat.message(msg));
					if gcinclude.GearWarnings == '' then
						gcinclude.GearWarnings = msg;
					else
						gcinclude.GearWarnings = gcinclude.GearWarnings .. ',' .. msg;
					end
				end
			end
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
	local inventory = AshitaCore:GetMemoryManager():GetInventory();
	local resources = AshitaCore:GetResourceManager();
	local iName,iSlot,sLocks;
	local containerID,itemEntry,item;
	local bCharges,bCD;
		
	if #args > 1 then
		-- see if the item specified is a code	
		for k,v in pairs(gcinclude.tEquipIt) do
			if string.lower(k) == string.lower(args[2]) then
				iName = v['Name'];
				iSlot = v['Slot'];
				sLocks = v['aSlots'];		
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
				print(chat.header('fEquipIt'):append(chat.message('Error: incomplete /equipit command: /equipit code|name slot. Command ignored.')));
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
			LockUnlock('locks','lock',sLocks);
		else
			LockUnlock('locks','lock',iSlot);
		end
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
	fWhichAccuracySet searches the player's AccuracySet for the named set and
	returns the associated slots. If not found, an error message is displayed
	and nil is returned.
--]]

function fWhichAccuracySet(sId)

	if sId == nil or gProfile.AccuracySet == nil then
		return nil;
	end
	
	for i,j in pairs(gProfile.AccuracySet) do
		if string.lower(sId) == string.lower(i) then
			return j;
		end
	end
	print(chat.header('fWhichAccuracySet'):append(chat.message('Accuracy set: ' .. sId .. ' not found. Ignoring.')));
	return nil;
end		-- fWhichAccuracySet

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
		if string.find(gcinclude._TankJobList,player.MainJob) ~= nil then
			gcdisplay.AdvanceToggle('Idle');
			toggle = 'Idle';
			status = gcdisplay.GetToggle('Idle');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Your job does not support the idle command. Ignoring')));
		end
	elseif (args[1] == 'tank') then			-- Turns on/off whether tanking gear is equipped
		if string.find(gcinclude._TankJobList,player.MainJob) ~= nil then
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
		if gcinclude.settings.bWSOverride == false then
			gcdisplay.AdvanceToggle('WSwap');
			toggle = 'Weapon Swap';
			status = gcdisplay.GetToggle('WSwap');
		else
			print(chat.header('HandleCommands'):append(chat.message('Error: Weapon swapping always enabled on ' .. player.MainJob .. '. Ignoring command')))
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
				args[2] = fWhichAccuracySet(string.sub(args[2],2,-1));
			end
			LockUnlock(sTarget,'lock',args[2]);
		end
		sList = fGetLockedList(sTarget);		
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

		if args[2] == nil then
			args[2] = 'all';
		end
		
		if args[2] ~= nil then
			if sTarget == 'acc' and string.sub(args[2],1,1) == '-' then
				args[2] = fWhichAccuracySet(string.sub(args[2],2,-1));
			end
			LockUnlock(sTarget,'unlock',args[2]);
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
		sList = fGetLockedList(sTarget);
		if sTarget == 'locks' then 
			gcdisplay.SetSlots('locks',gcinclude.LocksNumeric);
		else
			gcdisplay.SetSlots('acc',gcinclude.AccNumeric);
		end
	elseif (args[1] == 'rc') then							-- Display region controls
		RegionControlDisplay();
	elseif (args[1] == 'rv') then
		RefreshVariables();
	elseif (args[1] == 'showit') then						-- Shows debug info for specified type
		DB_ShowIt();
	elseif (args[1] == 'gearset' or args[1] == 'gs') then	-- Forces a gear set to be loaded and turns GSWAP off
		if #args > 1 then
			--gcinclude.ClearSet(gcinclude.sets.CurrentGear);
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
				local tTable = fGetTableByName(sArg);	-- Change string to table
				if tTable ~= nil then
					gcinclude.MoveToCurrent(tTable,gcinclude.sets.CurrentGear,true);
				else
					print(chat.header('HandleCommands'):append(chat.message('Gear set not found: ' .. sArg)));
				end
			end
			
			gcinclude.EquipTheGear(gcinclude.sets.CurrentGear,true);
			fLockSlotsBySet(gcinclude.sets.CurrentGear);

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
		MaxSpell(args[2],args[3],true);
		toggle = 'MaxSpell';
	elseif (args[1] == 'maxsong') then			-- Determines highest level song to cast
		MaxSong(args[2],args[3],true);
		toggle = 'MaxSong';
	elseif args[1] == 'equipit' or args[1] == 'ei' then			-- Equip specified item
		EquipItem(args);
	end

	if gcinclude.settings.Messages then
		gcinclude.Message(toggle, status)
	end
end		-- gcinclude.HandleCommands

--[[
	This function returns the weak element to the passed in element.
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
		if string.sub(root,-1,1) == ':' then
			root = string.sub(root,1,-2);
		end
	end
	return root;
end		-- fGetRoot

--[[
	fWhichStat determines if the passed spell has a stat associated with it
--]]

function fWhichStat(sSpellName)
	local root = nil;
	local tbl;
	
	if sSpellName == nil then
		print('Warning: fWhichStat - spellName is nil');
		return;
	end
	
	root = fGetRoot(sSpellName);
	for k, tbl in pairs(gcinclude.tStatMagic) do	-- search the list
		if string.find(tbl[2],root) ~= nil then		-- if not nil then the "root" was found
			return tbl[1];
		end
	end
	return	
end		-- fWhichStat

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
		if gcinclude.fLocks[4]['lock'] == false then
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
end		-- gcinclude.Initialize

--[[
	fMidcastSinging handles all of the equipment management when a song is cast.
--]]

function fMidcastSinging()
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
	local bTank;
	
	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end
	
	root = fGetRoot(spell.Name);
	
	if string.find('curaga,cure',root) == nil then
		-- Start with the non-cure based spells
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_HealingMagic,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.HealingMagic,gProfile.Sets.CurrentGear);
		end
	else
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
			pDay,pWeather = fCheckObiDW('light');
			if pDay + pWeather > 0 then
				sGear = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
				if sGear ~= nil then
					gProfile.Sets.CurrentGear['Waist'] = sGear;
				end
			end			
		else
			-- This is the the type of curing magic most folks assume happens
			if bTank == true then
				gcinclude.MoveToCurrent(gProfile.Sets.Tank_Curing,gProfile.Sets.CurrentGear);
			else
				gcinclude.MoveToCurrent(gProfile.Sets.Curing,gProfile.Sets.CurrentGear);
			end
		end
		
		-- While the reasoning is different, both types of "cures" can use an elemental
		-- stave. (Offensive cures take advantage of affinity while regular cures 
		-- appreciate the cure potency on a light-based staff.)
		sGear = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
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
	local root,pDay,pWeather,sGear;
	local bTank;
	
	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end
	
	root = fGetRoot(spell.Name);
	
	if table.find(gcinclude.AbsorbDarkSpells,root) ~= nil then
		-- Absorb spell
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_Absorb,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.Absorb,gProfile.Sets.CurrentGear);
		end
	elseif root == 'drain' or root == 'aspir' then
		-- Drain/Aspir
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_DrainAspir,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.DrainAspir,gProfile.Sets.CurrentGear);
		end	
		
		-- Check for an elemental obi. First determine if a bonus is possible 
		-- based on day's element and/or weather
		pDay,pWeather = fCheckObiDW('dark');
		if pDay + pWeather > 0 then
			sGear = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
			if sGear ~= nil then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end	
		
		-- And an elemental staff, for the affinity
		sGear = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
		if sGear ~= nil then
			gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
		end		
	else
		-- All other dark magic spells 
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_DarkMagic,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.DarkMagic,gProfile.Sets.CurrentGear);
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
	local bTank;
	
	bTank = gcdisplay.GetToggle('Tank');
	if bTank == nil then
		bTank = false;
	end
	
	root = fGetRoot(spell.Name);
	
	if table.find({'banish','banishga','holy'},root) ~= nil then
		-- Offensive Divine
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_OffensiveDivine,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.OffensiveDivine,gProfile.Sets.CurrentGear);
		end
		
		-- Check for an elemental obi. First determine if a bonus is possible 
		-- based on day's element and/or weather
		pDay,pWeather = fCheckObiDW('light');
		if pDay + pWeather > 0 then
			sGear = gcinclude.fCheckForElementalGearByValue('obi','MEacc',root);
			if sGear ~= nil then
				gProfile.Sets.CurrentGear['Waist'] = sGear;
			end
		end	
	elseif table.find({'flash','repose'},root) ~= nil then
		-- Enfeebling Divine
		if bTank == true then
			gcinclude.MoveToCurrent(gProfile.Sets.Tank_EnfeebleDivine,gProfile.Sets.CurrentGear);
		else
			gcinclude.MoveToCurrent(gProfile.Sets.EnfeebleDivine,gProfile.Sets.CurrentGear);
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
	sGear = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
	if sGear ~= nil then
		gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
	end			
end		-- MidcastDivineMagic

--[[
	MidcastEnfeeblingMagic handles all of the equipment management when a enfeeble
	spell is cast.
--]]

function MidcastEnfeeblingMagic()
end

--[[
	fMidcastEnhancingMagic handles all of the equipment management when a enhancing
	spell is cast.
--]]

function fMidcastEnhancingMagic()
end

--[[
	fMidcastElementalMagic handles all of the equipment management when a elemental
	spell is cast.
--]]

function fMidcastElementalMagic()
end

--[[
	fMidcastSummoning handles all of the equipment management when a avatar/spirit
	summoning spell is cast.
--]]

function fMidcastSummoning()
end

--[[
	fMidcastBlueMagic handles all of the equipment management when a blue magic
	spell is cast.
	
	WIP: Until more details are released on how HorizonXI are implementing blue
	magic, this function is nothing but a stub function
--]]

function fMidcastBlueMagic()
end

--[[
	fMidcastGeomancy handles all of the equipment management when a geomancy magic
	spell is cast.
	
	WIP: Until more details are released on how HorizonXI is implementing geomancy
	magic, this function is nothing but a stub function
--]]

function fMidcastGeomancy()
end

--[[
	fMidcastNinjutsu handles all of the equipment management when a ninjutsu spell
	is cast.
--]]

function fMidcastNinjutsu()
end

--[[
	fHandleMidcast is a coordinating routine that's used to call the independent types
	of magic routines. Unlike the previous implementation, the multitude of tiers are
	collapsed to help give the player agency.
--]]

function gcinclude.fHandleMidcast()
	local spell = gData.GetAction();
	
	if spell.Skill == 'Singing' then
		fMidcastSinging();
	elseif spell.Skill == 'Healing Magic' then
		fMidcastHealingMagic();
	elseif spell.Skill == 'Dark Magic' then
		fMidcastDarkMagic();
	elseif spell.Skill == 'Divine Magic' then
		fMidcastDivineMagic();
	elseif spell.Skill == 'Enfeebling Magic' then
		fMidcastEnfeeblingMagic();
	elseif spell.Skill == 'Enhancing Magic' then
		fMidcastEnhancingMagic();
	elseif spell.Skill == 'Elemental Magic' then
		fMidcastElementalMagic();
	elseif spell.Skill == 'Summoning' then
		fMidcastSummoning();
	elseif spell.Skill == 'Blue Magic' then
		fMidcastBlueMagic();
	elseif spell.Skill == 'Geomancy' then
		fMidcastGeomancy();
	elseif spell.Skill == 'Ninjutsu' then
		fMidcastNinjutsu();
	end
end

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
	local sSet,sGear;
	local cKey;

	if bTank == nil then		-- Need to check because of transition state of change
		bTank = false;
	end
	
	gcinclude.settings.priorityMidCast = string.upper(gcinclude.settings.priorityMidCast);
	for i = 1,string.len(gcinclude.settings.priorityMidCast),1 do
		cKey = string.sub(gcinclude.settings.priorityMidCast,i,i);
		sGear = nil;
		
		if cKey == 'A' then				-- midcast gear
			gcinclude.MoveToCurrent(gProfile.Sets.Midcast,gProfile.Sets.CurrentGear);
		elseif cKey == 'B' then			-- Spell Interruption Rate gear
			if spell.Skill ~= 'Singing' then
				gcinclude.MoveToCurrent(gProfile.Sets.SIR,gProfile.Sets.CurrentGear);
			end
		elseif cKey == 'C' then			-- INT/MND gear?
			sSet = fWhichStat(spell.Name);
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
			elseif string.match(spell.Name, 'Phalanx') then
				gcinclude.MoveToCurrent(gProfile.Sets.Phalanx,gProfile.Sets.CurrentGear);
			end
		elseif cKey == 'G' then				-- Elemental Obi
			if spell.Skill ~= 'Summoning' then
				local sRoot = fGetRoot(spell.Name);
				sGear = gcinclude.fCheckForElementalGearByValue('obi','MEacc',sRoot);
				if sGear ~= nil then
					gProfile.Sets.CurrentGear['Waist'] = sGear;
				end
			end
		elseif cKey == 'H' then				-- Elemental Stave
			if spell.Skill == 'Singing' then
				sGear = gcinclude.fCheckForElementalGearByValue('staff','SongAffinity',spell.Name);
			elseif spell.Skill ~= 'Summoning' then
				sGear = gcinclude.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
			-- The elemental staff has no effect on the summons. It does have an effect if
			-- you have a pet out. Skip it here, but make sure handles in the job file.
			end
		
			if sGear ~= nil then
				gcinclude.fSwapToStave(sGear,false,gProfile.Sets.CurrentGear);
			end
		end
	end
end		-- gcinclude.HandleMidcast

--[[
	gcinclude.fHandleWeaponskill loads the appropriate gear for the weapon skill
	you're doing
--]]

function gcinclude.fHandleWeaponskill()
	local ws = gData.GetAction();
	local lName = string.lower(ws.Name);
	local sName;
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
	
			local sGorget = gcinclude.fCheckForElementalGearByValue('gorget','eleWS',ws.Name);
			if sGorget ~= nil then
				gProfile.Sets.CurrentGear['Neck'] = sGorget;
			end
	
		elseif cKey == 'D' then		-- accuracy		
			-- Next check on accuracy. Use Tank_accuracy if /tank = true
			gcinclude.FractionalAccuracy(gProfile.Sets.Accuracy,gProfile.Sets.Tank_Accuracy);
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