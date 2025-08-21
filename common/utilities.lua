local utilities = {};

version = {
    ['author']	= 'Paiine',
    ['name']	= 'Luashitacast (Karma)',
    ['version']	= '2.0.alpha'
};

-- List of all days including the strong and weak elements
utilities.tWeekDayElement = {
    ['Firesday'] =     { ['strong'] = 'fire',    ['weak'] = 'water' },
    ['Earthsday'] =    { ['strong'] = 'earth',   ['weak'] = 'wind' },
    ['Watersday'] =    { ['strong'] = 'water',   ['weak'] = 'thunder' },
    ['Windsday'] =     { ['strong'] = 'wind',    ['weak'] = 'ice' },
    ['Iceday'] =       { ['strong'] = 'ice',     ['weak'] = 'fire' },
    ['Lightningday'] = { ['strong'] = 'thunder', ['weak'] = 'earth' },
    ['Lightsday'] =    { ['strong'] = 'light',   ['weak'] = 'dark' },
    ['Darksday'] =     { ['strong'] = 'dark',    ['weak'] = 'light' }
};

-- Lists all player storage containers available in FFXI.
-- Quite a number of them are not valid on HorizonXI yet.
utilities.STORAGES = {
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


-- Listed below are spells grouped by a dependency or a type. These are
-- root names
utilities.tSpellGroupings = {
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
        ['ele'] = { 'baraero','baraera','barblizzard','barblizzara','barfire','barfira','barstone','barstonera','barthunder','barthundra','barwater','barwatera' },
        ['status'] = { 'barsleep','barsleepra','barpoison','barpoisonra','barparalyze','barparalyzra','barblind','barblindra','barvirus','barvira','barpetrify','barpetra' }
    },
    ['enspell']    = { 'enthunder','enstone','enaero','enblizzard','enfire','enwater','enlight','endark' },
    ['spikes']	   = { 'blaze','ice','shock','dread' },
    ['spirits']    = {
        'fire','firespirit','fire spirit','ice','icespirit','ice spirit','air','airspirit','air spirit','earth','earthspirit','earth spirit','thunder','thunderspirit',
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

-- Lists all of the elemental gear broken out by type, element, and usage.
-- included in a reference to where the item that matches that type/element
-- can be found in the GearDetails table.
utilities.tElemental_gear = {
    ['relic'] = {gcinclude.OwnNation = -1;
        gcinclude.basetime = os.time();
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
        },gcinclude.OwnNation = -1;
        gcinclude.basetime = os.time();
        ['wind'] = {
            ['Weak'] = 'ice',
            ['NQ'] = { ['Name'] = 'Wind staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Auster\'s staff', ['Ref'] = {} },
            ['Affinity'] = { 'aero','aeroga','choke','tornado','enaero','huton','gravity','silence' },
            ['SongAffinity'] = { 'earth threnody' },
            ['Summons'] = { 'garuda','air spirit','fCheckInlineWeatherairspirit','air','siren' },
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
            ['SongAffinity'] = { 'dark threnody','foe requiem','foe requiem ii','foe requiem iii','foe requiem iv','foe requiem v','foe requiem vi','foe lullaby','horde lullaby',
                'magic finale','maiden\'s virelai' },
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
            ['eleWS'] = { 'cloudsplitter','thunder tutilities.fSummonersPetElementhrust','raiden thrust','tachi: goten' },
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
            ['eleWS'] = { 'energy steal','energy drain','sanguine blade','dark harvest','shadow of death','infernal scythe','blade: ei','starburst','sunburst','cataclysm','vidohunir',
                'omniscience','leaden suite' },
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
                'insurgency','keen edge','leaden salute','mandalic stab','mercy stroke','requiscat','rundra\'s storm','nightmare scythe','omniscience','one inch punch','penta thrust',
                'primal rend','retribution','shattersoul','starburst','stardiver','stringing pummel','sunburst','swift blade','tachi: kasha','tachi: rana','tachi: shoha','upheaval',
                'gate of tartarus' },
        },
        ['searched'] = false,
    },
};

-- Lists storage containers that can be equipped from outside of a moghouse
utilities.EQUIPABLE = {
    utilities.STORAGES[1],		-- Inventory
    utilities.STORAGES[9],		-- Wardrobe
    utilities.STORAGES[11],		-- Wardrobe 2
    utilities.STORAGES[17]		-- Wardrobe 8
};

utilities.EQUIPABLE_NONHOLIDAY = {
    utilities.STORAGES[1],		-- Inventory
    utilities.STORAGES[9],		-- Wardrobe
    utilities.STORAGES[11]		-- Wardrobe 2
};

utilities.NON_GEAR = {
    utilities.STORAGES[1],		-- Inventory
    utilities.STORAGES[2],		-- Safe
    utilities.STORAGES[3],		-- Storage
    utilities.STORAGES[5],		-- Locker
    utilities.STORAGES[6],		-- Satchel
    utilities.STORAGES[7],		-- Sack
    utilities.STORAGES[8],		-- Case
    utilities.STORAGES[10],		-- Safe 2
};

-- Define constants for input/output types for how slot references are formatted
utilities._SLOT_LA = 'LA';     -- lowercase slot name
utilities._SLOT_UA = 'UA';     -- uppercase slot name
utilities._SLOT_N  = 'N';      -- numeric
utilities._SLOT_FA = 'FA';     -- formatted output: first letter uppercase, rest lowercase

-- Holding variable for all of the messages that should only be displayed once
utilities.GearWarnings = nil;

-- The following is used in the /GC nag reminder
utilities.basetime = os.time();

crossjobs = required('crossjobs');

--[[
    fDisplayOnce will display the passed message if it hasn't been displayed
    before. If it has been displayed, the message isn't repeated.

    Parameters:
        msg         What should be displayed
        bOverride   Should the "only display once" rule be ignored
--]]

function utilities.fDisplayOnce(msg,bOverride)
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

    if utilities.GearWarnings == nil or
        (utilities.GearWarnings ~= nil and string.find(utilities.GearWarnings,tmp) == nil) or
            bOverride == true then
        print(chat.message(msg));

        if utilities.GearWarnings == nil then
            utilities.GearWarnings = msg;
        else
            utilities.GearWarnings = utilities.GearWarnings .. ',' .. msg;
        end
    end
end     -- utilities.fDisplayOnce

--[[
    fDisplayVerion displays version details including the changelog since the last release.
--]]

    function utilities.fDisplayVersion()
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
end     -- utilities.fDisplayVersion

--[[
    fStartReminder is a simple routine used for displaying a nag message for the player
    to run /GC. Initially it displayes 15 seconds after logging in, but from then on
    it will display every 5 minutes until /GC is run. (Gear swapping does not occur
    until this command is run.)
--]]

function utilities.StartReminder()
    local iTestVal = crossjobs.settings.bMinBasetime;
    local iNow = os.time();

    if gcdisplay.GetGC() == true then
        return;
    end

    if crossjobs.settings.bGCReminder == true then
        -- Since reminder already shown once, change the wait
        -- interval from 15 seconds to 5 minutes
        iTestVal = crossjobs.settings.bMaxBasetime;
    end

    if os.difftime(iNow,utilities.basetime) >= iTestVal then
        print(chat.message('************'));
        if iTestVal == crossjobs.settings.bMinBasetime then
            print(chat.message('FYI: Remember to do a /gc once \'data download\' finishes'));
        else
            print(chat.message('FYI: Remember to do a /gc'));
        end
        print(chat.message('************'));
        crossjobs.settings.bGCReminder = true;
        -- Change the base to current so that comparison is from now forward
        utilities.basetime = iNow;
    end
end     -- utilities.StartReminder

--[[
    fFormattedWord takes the passed in word and formats it in the indicated
    style.

    Parameter:
        sWord       Word to format
        sStyle      Style of the formatting

    Returned:
        Formatted string or nil
--]]

function utilities.fFormattedWord(sWord,sStyle)
local sTmp = nil;

    if sWord ~= nil then
        if sStyle == utilities._SLOT_FA then
            sTmp = string.upper(string.sub(sWord,1,1)) .. string.lower(string.sub(sWord,2,-1));
        elseif sStyle == utilities._SLOT_LA then
            sTmp = string.lower(sWord);
        elseif sStyle == utilities._SLOT_UA = 'UA' then
            sTmp = string.upper(sWord);
        end
    end

    return sTmp;
end     -- utilities.fFormattedWord

--[[
    fCheckTime determines if the current server time is found in the passed name time range.

    Parameters:
        hr          Current hour
        sTime       Named time to check against

    Returned:
        True/False, error message
--]]

function utilities.fCheckTime(hr,sTime)

    local bGood=false;
    local smsg;

    if sTime == 'NIGHTTIME' then
        bGood = (hr >= 17 or hr <= 6);
    elseif t == 'DAYTIME' then
        bGood = (hr >= 6 and hr <= 18);
    elseif t == 'DUSK2DAWN' then
        bGood = (hr >= 17 or hr <= 7);
    elseif t == 'DAWN' then
        bGood = (hr >= 6 and hr <= 7);
    elseif t == 'DAY' then
        bGood = (hr >= 7 and hr <=17);
    elseif t == 'DUSK' then
        bGood = (hr >= 17 and hr <= 18);
    elseif t == 'EVENING' then
        bGood = (hr >= 18 and hr <= 20);
    elseif t == 'DEADOFNIGHT' then
        bGood = (hr >= 20 and hr <= 4);
    else
        smsg = 'Warning: Unknown named time: '.. sTime;
        return bGood,smg;
    end

    return bGood,nil;
end     -- utilities.fCheckTime

--[[
    fIsPetSummonersPet determines if the player has a pet out and that it's a summoner's pet

    Parameters:
        pet     pet structure

    returned:
        True/False
--]]

function utilities.fSummonersPetElement(pet)

    if pet ~= nil and pet.Name ~= nil then
        -- we know there's a pet
        for i,j in utilities.tElemental_gear['staff'] do
            -- Walk the staves, ignoring "level" and process the summons table for each element
            if i ~= 'level' then
                if table.find(j['Summons'],string.lower(pet.Name)) ~= nil then
                    -- Summoner's pet found
                    return i;
                end
            end
        end
        -- No found summoner's pet
        return nil;
    else
        -- No pet
        return nil;
    end
end     -- utilities.fSummonersPetElement

--[[
    fTranslateWhichSlot determines if the passed in value is a valid slot designation and then translates it to the
    indicated format.

    Valid values are:  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
    main,sub,range,ammo,head,neck,ear1,ear2,body,hands,ring1,ring2,back,waist,legs,feet

    Note: rings and ears are not valid since they represent two slots. The invoking routine must decode these
    before invoking this function.

    Parameters:
        sSlots      Comma delimited list of slots
        bInverse    Process sSlots as is or invert the list

    Returned:
        bGood       Was the slot list valid?
        sList       Comma delimited correctly formatted list
--]]

-- Need more work. The inverted portion will not work if the passed in numbers are not in order!!!

function utilities.fListValidSlots(sCode,bInverse)
    local bGood = true;     -- Assume valid
    local sList = ',';
    local slots = {
         [1] =  {['Name'] = 'MAIN', ['Have'] = false},   [2] = {['Name'] = 'SUB', ['Have'] = false},
         [3] =  {['Name'] = 'RANGE', ['Have'] = false},  [4] = {['Name'] = 'AMMO', ['Have'] = false},
         [5] =  {['Name'] = 'HEAD', ['Have'] = false},   [6] = {['Name'] = 'NECK', ['Have'] = false},
         [7] =  {['Name'] = 'EAR1', ['Have'] = false},   [8] = {['Name'] = 'EAR2', ['Have'] = false},
         [9] =  {['Name'] = 'BODY', ['Have'] = false},  [10] = {['Name'] = 'HANDS', ['Have'] = false},
         [11] = {['Name'] = 'RING1', ['Have'] = false}, [12] = {['Name'] = 'RING2', ['Have'] = false},
         [13] = {['Name'] = 'BACK', ['Have'] = false},  [14] = {['Name'] = 'WAIST', ['Have'] = false},
         [15] = {['Name'] = 'LEGS', ['Have'] = false},  [16] = {['Name'] = 'FEET', ['Have'] = false}
    };

    sCode = ',' .. string.upper(sCode) .. ',';

    if bInverse == true then
        -- Flip the default settings since it's an inverse
        for i,j in ipairs(slots) do
            j['Have'] = true;
        end
    end

    -- Now process the list
    for i,j in ipairs(slots) do
        if string.find(sCode,j['Name']) ~= nil or string.find(sCode,','..tostring(i)..',') ~= nil then
            slots[i]['Have'] == not slots[i]['Have'];
        end
    end

    -- And now create the returned list
    for i,j in ipairs(slots) do
        if j['Have'] == true then
            sList = sList .. utilities.fTranslateWhichSlot(i,utilities._SLOT_FA) .. ',';
        end

    -- Then format the list accordingly
    if sList ~= ',' then
        sList = string.sub(sList,2,-2);     -- Remove the extra commas
        return true,sList;
    else
        return false,nil
    end
end     -- utilities.fListValidSlots

--[[
    fTranslateWhichSlot determines if the passed in value is a valid slot designation and then translates it to the
    indicated format.  Only a single value should be passed in.

    Valid val's are:  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
                      main,sub,range,ammo,head,neck,ear1,ear2,body,hands,ring1,ring2,back,waist,legs,feet

    Note: rings and ears are not valid since they represent two slots. The invoking routine must decode these
    before invoking this function.

    Parameters:
                val         value   slot number/name
                sType       N,LA,MA  designates what format the output should be in: numeric, lowercase alpha, Mixed case alpha

    Returned:
                requested slot formatted as indicated
                or nil if passed in slot invalid or an error occurs
--]]

function utilities.fTranslateWhichSlot(val,sType)
    local sValidSlotOutputTypes = utilities._SLOT_LA .. ',' .. utilities._SLOT_UA .. ',' .. utilities._SLOT_FA .. ',' .. utilities._SLOT_N;
    local slots = {
        { ['aSlot'] = 'main',  ['nSlot'] = 1,  ['fSlot'] = 'Main' },
        { ['aSlot'] = 'sub',   ['nSlot'] = 2,  ['fSlot'] = 'Sub' },
        { ['aSlot'] = 'range', ['nSlot'] = 3,  ['fSlot'] = 'Range' },
        { ['aSlot'] = 'ammo',  ['nSlot'] = 4,  ['fSlot'] = 'Ammo' },
        { ['aSlot'] = 'head',  ['nSlot'] = 5,  ['fSlot'] = 'Head' },
        { ['aSlot'] = 'neck',  ['nSlot'] = 6,  ['fSlot'] = 'Neck' },
        { ['aSlot'] = 'ear1',  ['nSlot'] = 7,  ['fSlot'] = 'Ear1' },
        { ['aSlot'] = 'ear2',  ['nSlot'] = 8,  ['fSlot'] = 'Ear2' },
        { ['aSlot'] = 'body',  ['nSlot'] = 9,  ['fSlot'] = 'Body' },
        { ['aSlot'] = 'hands', ['nSlot'] = 10, ['fSlot'] = 'Hands' },
        { ['aSlot'] = 'ring1', ['nSlot'] = 11, ['fSlot'] = 'Ring1' },
        { ['aSlot'] = 'ring2', ['nSlot'] = 12, ['fSlot'] = 'Ring2' },
        { ['aSlot'] = 'back',  ['nSlot'] = 13, ['fSlot'] = 'Back' },
        { ['aSlot'] = 'waist', ['nSlot'] = 14, ['fSlot'] = 'Waist' },
        { ['aSlot'] = 'legs',  ['nSlot'] = 15, ['fSlot'] = 'Legs' },
        { ['aSlot'] = 'feet',  ['nSlot'] = 16, ['fSlot'] = 'Feet' }
    };

    if val == nil then
        print(chat.header('fTranslateWhichSlot'):append(chat.message('No slot specified. Skipping...')));
        return nil;
    elseif string.find(val,',') ~= nil then
        print(chat.header('fTranslateWhichSlot'):append(chat.message('Only one slot can be passed in: ' .. val .. 'Skipping...')));
        return nil;
    end

    if sType ~= nil then
        sType = string.upper(sType);    -- Make sure uppercase
    end

    if sType == nil or string.find(sValidSlotOutputTypes,sType) == nil then
        sType = utilities._SLOT_LA;       -- assume lowercase slot name
    end

    val = string.lower(val);             -- Make sure lowercase

    for i,j in pairs(slots) do
        -- Since we don't know if the passed in value is a string or a number or a number passed in as a string...
        if (type(val) == 'string' and (j['aSlot'] == val or j['nSlot'] == tonumber(val)) or (type(val) == 'number' and j['nSlot'] == val) then
            -- There was a match, format the result accordingly
            local rVal;
            if sType == utilities._SLOT_FA then      -- Mixed case: Upper first letter, lower rest
                rVal = j['fSlot'];
            elseif sType == utilities._SLOT_LA then  -- lowercase name
                rVal = j['aSlot'];
            elseif sType == utilities._SLOT_N then  -- lowercase name
                rVal = j['nSlot']
            elseif sType == utilities._SLOT_UA then  -- uppercase name
                rVal = string.upper(j['aSlot']);
            else                                    -- numeric
                rVal = j['nSlot'];
            end
            return rVal;
        end
    end

    -- If you got here it wasn't found
    print(chat.header('fTranslateWhichSlot'):append(chat.message('Unrecognized slot: ' .. val .. 'Skipping...')));
    return nil;
end     -- utilities.fTranslateWhichSlot

--[[
    fGetRoot determines the "base" of a spell/song name passed in. (The base is the first word in the spell/song name.)

    Parameters:
        sSpell      Spell or song name to take the root of
        bVersion    T/F     If true will only remove the version number of the spell (eg., I, II, III... etc)

    Returned:
        Root of spell/song name or nil if an error occurred
--]]

function utilities.fGetRoot(sSpell,bVersion)
    local i,root;

    if bVersion == nil then
        bVersion = false;
    end

    sSpell = string.lower(sSpell);
    root = sSpell;

    if bVersion == true then
        i = string.find(spell, " [^ ]*$");
        if i ~= nil and string.find('i,ii,iii,iv,v,vi',string.sub(spell,i+1,-1)) ~= nil then
            root = string.sub(spellName,1,i-1);
        end
    else
        i = string.find(sSpell,' ');
        if i ~= nil then
            root = string.sub(sSpell,1,i-1);
        end

        -- Only ninjutsu have a ":" in the name. Remove if found on the end
        if string.sub(root,-1,-1) == ':' then
            root = string.sub(root,1,-2);
        end
    end
    return root;
end     -- utilities.fGetRoot

--[[
    fGetTableByName returns the gear set that is associated with the set name passed to it.
    It does this by walking the Sets (either gProfile.Sets or gcinclude.Sets)

    Returned: gearset/nil
--]]

function utilities.fGetTableByName(sName)
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

    for k,l in pairs(crossjobs.Sets) do
        if string.lower(k) == s2 then
            return l;
        end
    end

    return nil;
end     -- utilities.fGetTableByName

--[[
    fMakeConditionalTable takes the passed, // delimited list and
    returns the individual codes in a table

    Parameters:
        sList       // delimited list of inline conditionals

    Returned:
        Table of the split apart inline conditionals, all uppercase
--]]

function utilities.fMakeConditionalTable(sList)
    local tTbl = {};
    local iPos;

    iPos = 1;		-- Assume start at first position
    while iPos ~= nil do
        -- Look for the next //
        iPos = string.find(string.sub(sList,3,-1),'//');
        if iPos ~= nil then
            -- since found, add the current entry
            table.insert(tTbl,string.sub(sList,3,iPos+2-1));    -- skip the //, include up to next //
            -- and move the list to the next inline
            sList = string.sub(sList,iPos+2,-1);                -- save portion from next // onwards
        else
            -- since no more found, just save the conditional entry
            table.insert(tTbl,string.sub(sList,3,-1));          -- skip the //
        end
    end
    return tTbl;
end     -- utilities.fMakeConditionalTable

--[[
    fBuffed determines if the player has the buff/debuff or not. The passed buff name
    can be a substring, but make sure the that's all that is needed to uniquely identify
    the buff.

    Parameters:
        sCode               Name of the buff/debuff
        bStart      T/F     sCode must match from start?

    Returned:
        True or false if buff/debuff found
--]]

function utilities.fBuffed(sCode,bStart)
    local buffs = AshitaCore:GetMemoryManager():GetPlayer():GetBuffs();
    local pos;

    if bStart == nil then
        bStart = false;
    end

    sCode = string.lower(sCode);
    for _, buff in pairs(buffs) do
        local buffString = AshitaCore:GetResourceManager():GetString("buffs.names", buff);

        if (buffString) then
            pos = string.find(string.lower(buffString),sCode);
            if pos ~= nil then
                if (bStart == true and pos == 1) or (bStart == false) then
                    return true;
                end
            end
        end
    end
    return false;
end     --  utilities.fBuffed

--[[
    fCheckPartyJob determines if the party has a member whose job is the passed job.

    Parameters
        job         The job to look for
        bNotMe      Should I be excluded from the search

    Returned
        bFound      Is there a player that matches the criteria
--]]

function utilities.fCheckPartyJob(job,bNotMe)
    local pParty = AshitaCore:GetMemoryManager():GetParty();
    local bFound = false;
    local iStart = 1;

    if bNotMe ~= nil and bNotMe == true then
        iStart = 2;
    end

    job = string.upper(job);

    for i=iStart,6,1 do
        if (pParty:GetMemberIsActive(i - 1) == 1) then
            -- Player found
            local mainJob = pParty:GetMemberMainJob(i - 1);
            local j = AshitaCore:GetResourceManager():GetString("jobs.names_abbr", mainJob);
            if string.find(job,j) ~= nil then
                bFound = true;
            end
        end
    end
    return bFound;
end     -- utilities.fCheckPartyJob
