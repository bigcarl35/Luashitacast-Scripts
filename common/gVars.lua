local gVars = {};

--[[
    This module contains all of globally defined variables and structures as well as all the modules needed to run my
    luashitacast. The intent is to make a centrally located place for all this information. Makes it easier to access
    and easier to control.
--]]

--[[
    *****************************
    * dynamic global structures *
    *****************************
--]]

-- Lists used for alll elemental gear, broken out by type, element, and usage. Included is a reference to where the
-- item that matches that type/element can be found in the GearDetails table.
gVars.tElemental_gear = {
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
            ['Grip'] = { ['Name'] = 'Fire Grip', ['Ref'] = {} },
            ['Affinity'] = { 'blaze','burn','firaga','fire','flare','enfire','katon' },
            ['SongAffinity'] = { 'ice threnody' },
            ['Summons'] = { 'ifrit','fire spirit','firespirit','fire' }
        },
        ['ice'] = {
            ['Weak'] = 'fire',
            ['NQ'] = { ['Name'] = 'Ice staff', ['Ref'] = {} },
            ['HQ'] = {['Name'] = 'Aquilo\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Ice Grip', ['Ref'] = {} },
            ['Affinity'] = { 'blizzaga','blizzard','freeze','frost','ice','enblizzard','jubaku','hyoton','bind','distract','paralyze' },
            ['SongAffinity'] = { 'wind threnody' },
            ['Summons'] = { 'shiva','ice spirit','icespirit','ice' },
        },
        ['wind'] = {
            ['Weak'] = 'ice',
            ['NQ'] = { ['Name'] = 'Wind staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Auster\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Wind Grip', ['Ref'] = {} },
            ['Affinity'] = { 'aero','aeroga','choke','tornado','enaero','huton','gravity','silence' },
            ['SongAffinity'] = { 'earth threnody' },
            ['Summons'] = { 'garuda','air spirit','fCheckInlineWeatherairspirit','air','siren' },
        },
        ['earth'] = {
            ['Weak'] = 'wind',
            ['NQ'] = { ['Name'] = 'Earth staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Terra\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Earth Grip', ['Ref'] = {} },
            ['Affinity'] = { 'quake','rasp','stone','stonega','enstone','hojo','doton','slow' },
            ['SongAffinity'] = { 'lightning threnody', 'battlefield elegy', 'carnage elegy' },
            ['Summons'] = {'titan','earth spirit','earthspirit','earth' },
        },
        ['thunder'] = {
            ['Weak'] = 'earth',
            ['NQ'] = { ['Name'] = 'Thunder staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Jupiter\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Thunder Grip', ['Ref'] = {} },
            ['Affinity'] = { 'burst','shock','thundaga','thunder','enthunder','raiton' },
            ['SongAffinity'] = { 'water threnody' },
            ['Summons'] = { 'ramuh','thunder spirit','thunderspirit','thunder' },
        },
        ['water'] = {
            ['Weak'] = 'thunder',
            ['NQ'] = { ['Name'] = 'Water staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Neptune\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Water Grip', ['Ref'] = {} },
            ['Affinity'] = { 'drown','flood','poison','poisonga','water','waterga','enwater','dokumori','suiton' },
            ['SongAffinity'] = { 'fire threnody' },
            ['Summons'] = { 'leviathan','water spirit','waterspirit','water' },
        },
        ['light'] = {
            ['Weak'] = 'dark',
            ['NQ'] = { ['Name'] = 'Light staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Apollo\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Light Grip', ['Ref'] = {} },
            ['Affinity'] = { 'banish','banishga','curaga','cure','dia','diaga','flash','holy','enlight','repose','inundation' },
            ['SongAffinity'] = { 'dark threnody','foe requiem','foe requiem ii','foe requiem iii','foe requiem iv','foe requiem v','foe requiem vi','foe lullaby','horde lullaby',
                'magic finale','maiden\'s virelai' },
                ['Summons'] = {'carbuncle','light spirit','lightspirit','light','cait sith','caitsith','alexander'},
        },
        ['dark'] = {
            ['Weak'] = 'light',
            ['NQ'] = { ['Name'] = 'Dark staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Pluto\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Dark Grip', ['Ref'] = {} },
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
            ['eleWS'] = { 'energy steal','energy drain','sanguine blade','dark harvest','shadow death','infernal scythe','blade: ei','starburst',
                'sunburst','cataclysm','vidohunir','omniscience','leaden suite' },
        },
    },
    ['gorget'] = {
        ['level'] = 72,
        ['fire'] = {
            ['Weak'] = 'water',
            ['Name'] = 'Flame gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'liquefaction','fusion' },
            ['eleWS'] = { 'arching arrow','ascetic\'s fury','asuran fists','atonement','blade: shun','decimation','detonator','drakesbane','dulling arrow','empyreal arrow','final heaven',
                'flaming arrow','full swing','garland of bliss','heavy shot','hexa strike','hot shot','insurgency','knights of round','last stand','mandalic stab','mistral axe',
                'metatron torment','realmrazer','red lotus blade','scourge','shijin spiral','sniper shot','spinning attack','spinning axe','stringing pummel','tachi: kagero','tachi: kasha',
                'upheaval','wheeling thrust' },
        },
        ['ice'] = {
            ['Weak'] = 'fire',
            ['Name'] = 'Snow gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'induration','distortion' },
            ['eleWS'] = { 'blade: to','blast arrow','cross reaper','death blossom','expiacion','freezebite','frostbite','full break','geirskogul','ground strike','guillotine','quietus',
                'impulse drive','mordant rime','namas arrow','piercing arrow','pyrrhic kleos','rudra\'s storm','ruinator','raging rush','shadow of death','shattersoul','skullbreaker',
                'smash axe','spiral hell','steel cyclone','tachi: gekko','tachi: hobaku','tachi: rana','tachi: yukikaze','tornado kick','vidohunir' },
        },
        ['wind'] = {
            ['Weak'] = 'ice',
            ['Name'] = 'Breeze gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'detonation','fragmentation' },
            ['eleWS'] = { 'aeolian edge','backhand blow','black halo','blade: jin','blade: kamu','blade: to','camlann\'s torment','coronach','cyclone','dancing edge','death blossom',
                'dragon kick','earth crusher','exenterator','freezebite','gale axe','ground strike','gust slash','king\'s justice','mordant rime','raging axe','randgrith',
                'red lotus blade','resolution','ruinator','savage blade','shark bite','shell crusher','sidewinder','slug shot','spinning slash','steel cyclone','tachi: jinpu',
                'tachi: kaiten','taichi: shoha','taichi: yukikaze','tornado kick','trueflight','true strike','victory smite','vidohunir' },
        },
        ['earth'] = {
            ['Weak'] = 'wind',
            ['Name'] = 'Soil gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'scission','gravitation' },
            ['eleWS'] = { 'aeolian edge','asuran fists','avalanche axe','blade: ei','blade: ku','blade: ten','calamity','catastrophe','crescent moon','dancing edge','entropy','eviseration',
                'exenterator','expiacion','fast blade','hard slash','impulse drive','iron tempest','king\'s justice','leaden salute','mercy stroke','nightmare scythe','omniscience',
                'primal rend','pyrrhic kleos','rampage','requiscat','resolution','retibution','savage blade','seraph blade','shattersoul','shining blade','sickle moon','slice','spinning axe',
                'spinning scythe','spiral hell','stardiver','stringing pummel','sturmwind','swift blade','tachi: enpi','tachi: jinpu','tachi: rana','trueflight','viper bite','vorpal blade',
                'wasp sting' },
        },
        ['thunder'] = {
            ['Weak'] = 'earth',
            ['Name'] = 'Thunder gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'impaction','fragmentation' },
            ['eleWS'] = { 'aeolian edge','apex arrow','armor break','avalanche axe','black halo','blade: chi','blade: jin','blade: kamu','blade: shun','calamity','camlann\'s torment',
                'circle blade','combo','cyclone','death blossom','dragon kick','earth crusher','exenterator','flat blade','full swing','ground strike','heavy swing','howling fist',
                'judgement','king\'s justice','leg sweep','mordant rime','raging axe','raging fist','raiden thrust','realmrazer','resolution','rock crusher','savage blade','seraph strike',
                'shark bite','shield break','shining strike','shoulder tackle','sickle moon','skewer','spinning attack','spinning axe','tachi: goten','tachi: koki','tachi: shoha',
                'thunder thrust','true strike','victory smite','vidohunir','vorpal blade','weapon break' },
        },
        ['water'] = {
            ['Weak'] = 'thunder',
            ['Name'] = 'Aqua gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'reverberation','distortion' },
            ['eleWS'] = { 'atonement','blade: teki','brainshaker','circle blade','cross reaper','dark harvest','entropy','quietus','death blossom','decimation','expiacion','full break',
                'garland of bliss','gate of tartarus','geirskogul','ground strike','last stand','mordant rime','namas arrow','piercing arrow','pyrrhic kleos','rudra\'s storm','primal rend',
                'raging rush','retribution','ruinator','shadow of death','shockwave','shoulder tackle','sidewinder','skullbreaker','slug shot','smash axe','spinning scythe','spiral hell',
                'split shot','steel cyclone','sturmwind','sunburst','tachi: gekko','tachi: koki','vidohunir','vorpal thrust' },
        },
        ['light'] = {
            ['Weak'] = 'dark',
            ['Name'] = 'Light gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'transfixion','fusion','light' },
            ['eleWS'] = { 'apex arrow','arching arrow','ascetic\'s fury','atonement','blade: chi','blade: ku','blade: rin','blade: shun','blast arrow','blast shot','camlann\'s torment',
                'decimation','detonator','double thrust','drakesbane','dulling arrow','empyreal arrow','eviseration','final heaven','flaming arrow','garland of bliss','heavy shot',
                'hexa strike','hot shot','howling fist','insurgency','knight\'s of round','leaden salute','last stand','mandalic stab','metatron torment','mistral axe','omniscience',
                'piercing arrow','power slash','realmrazer','raiden thrust','scourge','shijin spiral','sidewinder','skewer','slug shot','sniper shot','split shot','stardiver','tachi: enpi',
                'tachi: goten','tachi: kasha','thunder thrust','torcleaver','victory smite','upheaval','vorpal scythe','vorpal thrust','wheeling thrust' },
        },
        ['dark'] = {
            ['Weak'] = 'light',
            ['Name'] = 'Shadow gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'compression','gravitation','darkness' },
            ['eleWS'] = { 'asuran fists','black halo','blade: ei','blade: hi','blade: kamu','blade: ku','blade: ten','catastrophe','quietus','entropy','eviseration','impulse drive',
                'insurgency','keen edge','leaden salute','maturns gear swapping on or offndalic stab','mercy stroke','requiscat','rundra\'s storm','nightmare scythe','omniscience','one inch punch','penta thrust',
                'primal rend','retribution','shattersoul','starburst','stardiver','stringing pummel','sunburst','swift blade','tachi: kasha','tachi: rana','tachi: shoha','upheaval',
                'gate of tartarus' },
        },
        ['searched'] = false,
    },
};

-- Define constants for Region so typos aren't made
gVars._REGION_SANDY = 1;
gVars._REGION_BASTOK = 2;
gVars._REGION_WINDY = 3;
gVars._REGION_NA = 0;
gVars._REGION_UNKNOWN = -1;

-- This table tracks regional control using zone id's associated with a region, and querying the server for who last gained
-- conquest of the region. This table is automatically populated by digesting the appropriate packet from the server when
-- the player zones.

gVars.RegionControl = {
    ['Argoneau'] 		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {152,7,8,151,200,119,120}},
    ['Bastok'] 			= { ['own'] = gVars._REGION_BASTOK,  ['zones'] = {234,235,236,237}},
    ['Derfland']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {147,197,109,148,110}},
    ['ElshimoLowlands']	= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {250,252,176,123}},
    ['ElshimoUplands']	= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {207,211,160,205,163,159,124}},
    ['Fauregandi']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {111,203,204,9,206,166,10}},
    ['Gustaberg']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {191,173,106,143,107,144,172}},
    ['Jeuno']			= { ['own'] = gVars._REGION_NA,      ['zones'] = {243,244,245,246}},
    ['Kolshushu']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {4,118,213,3,198,249,117}},	-- Purgonorgo Isle doesn't have a separate ID
    ['Kuzotz']			= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {209,114,168,208,247,125}},
    ['LiTelor']			= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {153,202,154,251,122,121}},
    ['Movapolos']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {13,12,11}},
    ['Norvallen']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {105,104,2,150,149,1,195}},
    ['QuifimIsland']	= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {127,184,157,126,179,158}},
    ['Ronfaure']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {167,101,141,140,139,190,100,142}},
    ['Sandoria']		= { ['own'] = gVars._REGION_SANDY,   ['zones'] = {230,231,232,233}},
    ['Sarutabaruta']	= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {146,116,170,145,192,194,169,115}},
    ['Tavnazia']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {24,25,31,27,30,29,28,32,26}},
    ['Tulia']			= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {181,180,130,178,177}},
    ['Valdeaunia']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {6,161,162,165,5,112}},
    ['Vollbow']			= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {113,201,212,174,128}},
    ['Windurst']		= { ['own'] = gVars._REGION_WINDY,   ['zones'] = {238,239,240,241,242}},
    ['Zulkheim']		= { ['own'] = gVars._REGION_UNKNOWN, ['zones'] = {196,108,102,193,248,103}},
    ['Dynamis']			= { ['own'] = gVars._REGION_NA,      ['zones'] = {39,40,41,42,134,135,185,186,187,188}},
    ['Lumoria']			= { ['own'] = gVars._REGION_NA,      ['zones'] = {33,34,35,36,37,38}},
    ['Promyvion']		= { ['own'] = gVars._REGION_NA,      ['zones'] = {16,17,18,19,20,21,22,23,39,40,41,42}}
};

-- List of all conquest region controller's designations
gVars.tRegionControllerSettings = {
    [-1] = 'Unassigned', [0]  = 'N/A', [1]  = 'San d\'Orian', [2]  = 'Bastokian', [3]  = 'Windurstian', [4]  = 'Beastmen'
};

-- Define region status
gVars._REGION_STATUS_UNKNOWN = 'Unknown';
gVars._REGION_STATUS_NA = 'N/A';
gVars._REGION_STATUS_OWNED = 'Owned';
gVars._REGION_STATUS_NOT_OWNED = 'Not Owned';
gVars._REGION_STATS_NA_NOT_OWNED = 'N/A (Not Owned)';
gVars._REGION_STATUS_MUST_ZONE = 'Must Zone';

-- This table contains a list of all of the gear found by Luashitacast in your job and crossjob file's gear sets.
-- It is dynamically populated by the fGearCheck function found in gear.lua. This table was a solution to lag that
-- was seen when querying the server too often.
gVars.tGearDetails = {
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

gVars._Progressive_ACC = 'Acc';
gVars._Progressive_TACC = 'TAcc';
gVars._Progressive_RACC = 'RAcc';
gVars._Progressive_TRACC = 'TRAcc';

-- This table holds tallied information about the different types of accuracy found in progressive structure
gVars.tProgressive =  {
    ['Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = gVars._Progressive_ACC },
    ['Tank_Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = gVars._Progressive_TACC },
    ['Ranged_Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = gVars._Progressive_RACC },
    ['Tank_Ranged_Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = gVars._Progressive_TRACC },
};

-- This structure tracks gear pieces by gear set. It is dynamically populated by the fGearCheckItem function found
-- in Gear.lua. It gets populated when /gc is run. The intention of this structure is to take advantage of the details
-- that are known during /gc so that gearsets do not have to be reparsed when doing /smg reports.
gVars.tGearsetDetails = {};

--[[
    ****************************
    * static global structures *
    ****************************
--]]

-- Version reference for Luashitacast (Boxcar)
gVars.version = {
    ['author']	= 'Paiine',
    ['name']	= 'Luashitacast (Boxcar)',
    ['version']	= '3.alpha.3',
};

-- List of all supported commands
gVars.AliasList = {
    '911','acc','ajug','cap','cc','db','dt','ei','equipit','eva','gc','gearset','gs','gswap','horn','idle',
    'kite','lock','macc','man','maxsong','maxspell','mode','petfood','ptt','pull','racc','rc','rv','sbp',
    'showit','smg','spf','ss','string','sw','tank','th','unlock','val','ver','wsdistance','wswap','t1'
};

-- Lists all player storage containers available in FFXI.
-- Quite a number of them are not valid on HorizonXI yet.
gVars.STORAGES = {
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

-- Lists spells grouped by a dependency or a type. These are "root" names
gVars.tSpellGroupings = {
    ['int']		  =  { 'gravity','blind','sleep','sleepga','poison','poisonga','bind','dispel','blaze','ice','shock' },
    ['mnd']		   = { 'paralyze','slow','slowga','frazzle','distract','silence' },
    ['eDebuff']	   = { 'drown','burn','frost','choke','rasp','shock' },
    ['barspell']   = {
        ['ele'] = { 'baraero','baraera','barblizzard','barblizzara','barfire','barfira','barstone','barstonra','barthunder','barthundra','barwater','barwatera' },
        ['status'] = { 'barsleep','barsleepra','barpoison','barpoisonra','barparalyze','barparalyzra','barblind','barblindra','barvirus','barvira','barpetrify','barpetra' },
    },
    ['enspell']    = { 'enthunder','enstone','enaero','enblizzard','enfire','enwater','enlight','endark' },
    ['spikes']	   = { 'blaze','ice','shock','dread' },
    ['avatars']    = { 'carbuncle','fenrir','ifrit','titan','leviathan','garuda','shiva','ramuh','diabolos','cait sith','siren','odin','alexander','atomos' },
    ['spirits']    = { 'fire','firespirit','fire spirit','ice','icespirit','ice spirit','air','airspirit','air spirit','earth','earthspirit','earth spirit','thunder','thunderspirit',
                       'thunder spirit','water','waterspirit','water spirit','light','lightspirit','light spirit','dark','darkspirit','dark spirit'
    },
    ['absorb']     = { 'absorb-agi','absorb-chr','absorb-dex','absorb-int','absorb-mnd','absorb-str','absorb-vit','absorb-acc','absorb-tp' },
    ['nin-buff']   = { 'tonko','utsusemi','monomi' },
    ['nin-debuff'] = { 'kurayami','hojo','dokumori','jubaku' },
    ['nin-ele']    = { 'katon','suiton','raiton','doton','huton','hyoton' },
    ['brd-enh']	   = { 'minne','minuet','paeon','pastoral','madrigal','mambo','operetta','etude','ballad','march','prelude','aubade','carol','mazurka','gavotte','capriccio',
                       'fantasia','hymnus','round'
    },
    ['brd-enf']	   = { 'requiem','threnody','lullaby','finale','elegy','virelai' }
};

-- List of all weaponskills according to desired stats
gVars.tWeaponSkills = {
    ['CHR']             = { 'shadowstitch' },
    ['DEX']             = { 'wasp sting','viper bite','blade: metsu','dancing edge' },
    ['DEXAGI']          = { 'shark bite','coronach' },
    ['DEXCHR']          = { 'eviseration' },
    ['DEXINT']          = { 'gust slash','cyclone' },
    ['INT']             = { 'gate of tartarus' },
    ['INTMND']          = { 'spirit taker' },
    ['MND']             = { 'energy steal','energy drain'},
    ['RANGED_AGI']      = { 'hot shot','split shot','sniper shot','slugshot','blast shot','heavy shot','detonator' }, -- MARKSMANSHIP
    ['RANGED_STRAGI']   = { 'flaming arrow','piercing arrow','dulling arrow','sidewinder','blast arrow','arching arrow','empyreal arrow','namas arrow' }, -- ARCHERY
    ['STR']             = { 'raging axe','smash axe','gale axe','avalanche axe','spinning axe','rampage','mistral axe','decimation','spinning attack','flat blade',
                            'circle blade','vorpal blade','hard slash','crescent moon','mercy stroke','iron tempest','sturmwind','keen edge','raging rush',
                            'metatron torment','leg sweep','skewer','wheeling thrust','impulse drive','tachi: enpi','tachi: hobaku','tachi: goten','tachi: kagero',
                            'tachi: jinpu','tachi: yukikaze','tachi: gekko','tachi: kasha','tachi: kaiten','brainshaker','skullbreaker','true strike','heavy swing',
                            'shell crusher','full swing','onslaught','double thrust','spinning scythe','Vorpal Scythe' },
    ['STRAGI']          = { 'sickle moon','vorpal thrust' },
    ['STRDEX']          = { 'combo','backhand blow','raging fists','fast blade','penta thrust','blade: rin','blade: retsu','blade: jin','blade: ten','blade: ku','Geirskogul' },
    ['STRINT']          = { 'dark harvest','shadow of death','nightmare scythe','spiral hell','burning blade','frostbite','freezebite','spinning slash','ground strike',
                            'thunder thrust','raiden thrust','blade: teki','blade: to','blade: chi','blade: ei','rock crusher','earth crusher','catastrophe' },
    ['STRINT_30_20']    = { 'red lotus blade' },
    ['STRMND']          = { 'guillotine','cross reaper','shining blade','seraph blade','swift blade','savage blade','shockwave','tachi: koki','shining strike','seraph strike',
                            'judgment','hexa strike','randgrith','retribution', 'knights of round' },
    ['STRMND_30_50']    = { 'black halo' },
    ['STRVIT']          = { 'shoulder tackle','one inch punch','final heaven' },
    ['Skill']           = { 'starlight','moonlight' },
    ['HP']              = { 'spirits within' }
};

-- Various lists of slot names. Standard is the basic 16 slots equipment grid, extended adds the metas: rings and ears, full includes subset and group, progressive is full
-- minus group and smg is the extended list minus the individual minus the individual ears and rings: ear1,ear2,ring1,ring2. (And yes, LUA lets you have a mix of implicit
-- and explicit defined arrays in the same structure.)
gVars.tSlotNames = {
    ['standard']    = { 'main','sub','range','ammo','head','neck','ear1','ear2','body','hands','ring1','rings2','back','waist','legs','feet' },
    ['extended']    = { 'main','sub','range','ammo','head','neck','ear1','ear2','ears','body','hands','ring1','rings2','rings','back','waist','legs','feet' },
    ['full']        = { 'subset','group','main','sub','range','ammo','head','neck','ear1','ear2','ears','body','hands','ring1','rings2','rings','back','waist','legs','feet' },
    ['progressive'] = { 'subset','main','sub','range','ammo','head','neck','ear1','ear2','ears','body','hands','ring1','rings2','rings','back','waist','legs','feet' },
    ['smg']         = {
                         [1] = 'main', [2] = 'sub', [3] = 'range', [4] = 'ammo', [5] = 'head', [6] = 'neck', [7] = 'ears', [8] = 'body',
                         [9] = 'hands', [10] = 'rings', [11] = 'back', [12] = 'waist', [13] = 'legs', [14] = 'feet',
                      },
};

-- Lists of valid Weapon Types. Note: while SHIELD and AMMO isn't a weapon, it conforms to the weapon type mechanism in this program
gVars.tWeaponTypes = {
    ['all']     = { 'ARCHERY','AXE','CLUB','DAGGER','GAXE','GKATANA','GSWORD','H2H','KATANA','MARKSMANSHIP','POLEARM','SCYTHE','STAVE','SWORD','THROWING','SHIELD','AMMO' },
    ['melee']   = { 'AXE','CLUB','DAGGER','GAXE','GKATANA','GSWORD','H2H','KATANA','POLEARM','SCYTHE','STAVE','SWORD','SHIELD' },
    ['range']   = { 'ARCHERY','MARKSMANSHIP','THROWING','AMMO' },
};

-- List of all towns and the areas that are specific to each nation and Jeuno
gVars.tTownAreas = {
    ['Towns'] = {   'Tavnazian Safehold','Al Zahbi','Aht Urhgan Whitegate','Nashmau','Southern San d\'Oria [S]','Bastok Markets [S]',
                    'Windurst Waters [S]','San d\'Oria-Jeuno Airship','Bastok-Jeuno Airship','Windurst-Jeuno Airship','Kazham-Jeuno Airship',
                    'Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille','Bastok Mines','Bastok Markets',
                    'Port Bastok','Metalworks','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower',
                    'Ru\'Lude Gardens','Upper Jeuno','Lower Jeuno','Port Jeuno','Rabao','Selbina','Mhaura','Kazham','Norg','Mog Garden',
                    'Celennia Memorial Library','Western Adoulin','Eastern Adoulin' },
    ['Windy']  = { 'Windurst Waters [S]','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower' },
    ['Sandy']  = { 'Southern San d\'Oria [S]','Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille' },
    ['Bastok'] = { 'Bastok Markets [S]','Bastok Mines','Bastok Markets','Port Bastok','Metalworks' },
    ['Jeuno']  = { 'Ru\'Lude Gardens','Upper Jeuno','Lower Jeuno','Port Jeuno' }
};

-- Lists of gear storages that contain gear that can be equipped from outside of a moghouse
gVars.EQUIPABLE = {
    gVars.STORAGES[1],		-- Inventory
    gVars.STORAGES[9],		-- Wardrobe
    gVars.STORAGES[11],		-- Wardrobe 2
    gVars.STORAGES[17]		-- Wardrobe 8
};

-- List of gear storages that can contain gear that can be equipped
gVars.EQUIPABLE_LIST = {
    gVars.STORAGES[1]['id'],    -- Inventory
    gVars.STORAGES[9]['id'],    -- Wardrobe
    gVars.STORAGES[11]['id'],   -- Wardrobe 2
    gVars.STORAGES[17]['id']    -- Wardrobe 8
};

-- List of gear storages that can contain gear that can be equipped, but not event gear
gVars.EQUIPABLE_NONHOLIDAY = {
    gVars.STORAGES[1],		-- Inventory
    gVars.STORAGES[9],		-- Wardrobe
    gVars.STORAGES[11]		-- Wardrobe 2
};

-- List of storages that can contain other items besides gear. Please not that gear can be
-- contained in these storages too
gVars.NON_GEAR = {
    gVars.STORAGES[1],		-- Inventory
    gVars.STORAGES[2],		-- Safe
    gVars.STORAGES[3],		-- Storage
    gVars.STORAGES[5],		-- Locker
    gVars.STORAGES[6],		-- Satchel
    gVars.STORAGES[7],		-- Sack
    gVars.STORAGES[8],		-- Case
    gVars.STORAGES[10],		-- Safe 2
};

--[[
    ********************
    * global variables *
    ********************
--]]

-- Define arrays for toggles and cycles
gVars.Toggles = {};
gVars.Cycles = {};

-- Holding variable for all of the messages that should only be displayed once
gVars.GearWarnings = nil;

-- These two variables are used to store the invoked type of craft/gather type
gVars.Craft=nil;
gVars.Gather=nil;

-- Temporary holding variables for the main hand and off hand weapons
gVars.weapon = nil;
gVars.offhand = nil;

-- Indicates if /GC has been run or not
gVars.bGC = false;

-- Current regional setting
gVars.sRegion = gVars._REGION_STATUS_MUST_ZONE;
--[[
    ********************
    * global constants *
    ********************
--]]

-- Define constants to specify style off formatted input/output slot references
gVars._SLOT_LA = 'LC';     -- lowercase slot name
gVars._SLOT_UA = 'UC';     -- uppercase slot name
gVars._SLOT_N  = 'N';      -- numeric
gVars._SLOT_FA = 'FA';     -- formatted output: first letter uppercase, rest lowercase

-- Define constants for LOCK and UNLOCK
gVars._LOCK   = 'lock';
gVars._UNLOCK = 'unlock';

-- define the code lists for the crafting and gathering types
gVars._Crafting_Types = 'ALC,BONE,CLOTH,COOK,GSM,LTH,BSM,WW';
gVars._Gathering_Types = 'HELM,DIG,CLAM,FISH';

-- List of all valid jobs
gVars._validJobs = 'BLM,BLU,BRD,BST,COR,DNC,DRG,DRK,GEO,MNK,PLD,PUP,RDM,RNG,RUN,SAM,SCH,SMN,THF,WAR,WHM';

-- List of all magic using jobs
gVars._sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,BLU,SCH,GEO,RUN,NIN,BRD';

-- List of all jobs that can use ranged weapons (either range or ammo slot)
gVars._sRangedJobs = 'NIN,PUP,SAM,DNC,COR,RNG,BLM,SCH,THF,WAR,BRD,MNK,WHM,RDM';

-- List of all elements
gVars._AllElements = 'fire,ice,wind,earth,thunder,water,light,dark';

-- List of valid bst buff settings
gVars._sDB_Debuffs = 'BPP,WSS';

gVars._sDB_NORM = 'Norm';
gVars._sDB_BPP = 'BPP';
gVars._sDB_WSS = 'WSS';

-- List of all toggles and cycles to reduce the likeliness of a typo or case mismatch.
-- Start with ones that are
gVars._GSWAP        = 'GSwap';      -- Gear Swap
gVars._KITE         = 'Kite';       -- Kiting
gVars._EVASION      = 'Eva';        -- Evasion
gVars._IDLE         = 'Idle';       -- Idle
gVars._SPF          = 'SPF';        -- Show Pull Feedback
gVars._WSWAP        = 'WSwap';      -- Weapon Swap
gVars._TANK         = 'Tank';       -- Tanking
gVars._MACC         = 'MAcc';       -- Magic Accuracy
gVars._TH           = 'TH';         -- Treasure Hunter
-- Job specific ones
gVars._AJUG         = 'AJug';       -- Automatic pet Jug equipping
gVars._DB           = 'DB';         -- pet DeBuff removal
gVars._INSTRUMENT   = 'Instrument'; -- What type of instrument is the default
gVars._SBP          = 'sBP';        -- Show Blood Pact
gVars._MODE         = 'Mode';       -- SMN preference mode: ATTK (attack), PERP (perpetuation), or ENMM (emnity minus)
gVars._SS           = 'SS';         -- Show Steals
gVars._DT           = 'DT';         -- Damage Taken
gVars._REGION       = 'Region';     -- Region ownership
-- Static display bar referernces
gVars._JOB          = 'Job';        -- Job/sj
gVars._CAP          = 'Cap';        -- Level cap
gVars._GC           = 'GC';         -- Gear Check
gVars._ACC          = 'Acc';        -- Accuracy
gVars._RACC         = 'RAcc';       -- Range Accuracy
gVars._LOCKS        = 'Locks';      -- Locked slots
gVars._DAY          = 'Day';        -- Day
gVars._TIME         = 'Time';       -- Time
gVars._MOON         = 'Moon';       -- Moon phase
gVars._WEATHER      = 'Weather';    -- Weather
gVars._ZONE         = 'Zone';       -- Zone name
gVars._CC           = 'CC';         -- Conditional Codes

-- Define constants for DT so typos aren't made
gVars._DT_OFF = 'Off';
gVars._DT_PHY = 'Physical';
gVars._DT_MAG = 'Magical';
gVars._DT_BRE = 'Breath';

gVars._DT_P = 'P';
gVars._DT_M = 'M';
gVars._DT_B = 'B';

-- Define constants for Mode so typos aren't made
gVars._MODE_ATTACK = 'ATTK';
gVars._MODE_PERPETUATION = 'PERP';
gVars._MODE_ENMITY_MINUS = 'ENMM';

gVars._MODE_A = 'A';
gVars._MODE_P = 'P';
gVars._MODE_E = 'E';

-- define constants for Instrument so typos aren't made
gVars._HORN = 'Horn';
gVars._STRING = 'String';

-- Since gVars is loaded from all job files, the individual modules will be loaded here
crossjobs   = gFunc.LoadFile('common\\crossjobs.lua');
utilities   = gFunc.LoadFile('common\\utilities.lua');
validate    = gFunc.LoadFile('common\\validate.lua');
slips       = gFunc.LoadFile('common\\slips.lua');
reporting   = gFunc.LoadFile('common\\reporting.lua');
pets        = gFunc.LoadFile('common\\pets.lua');
magic       = gFunc.LoadFile('common\\magic.lua');
locks       = gFunc.LoadFile('common\\locks.lua');
inline      = gFunc.LoadFile('common\\inline.lua');
gear        = gFunc.LoadFile('common\\gear.lua');
displaybar  = gFunc.LoadFile('common\\displaybar.lua');

return gVars;
