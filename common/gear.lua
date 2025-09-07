local gear = T{};

local gcdisplay = require('gcdisplay');
local utilities = require('utilities');
local crossjobs = require('crossjobs');
local slips     = require('slips');
local locks     = require('locks');

-- List of items that inhibit more than the obvious gear slot. Add entries as you
-- need to account for the gear you use. Please note that ears and rings are
-- not supported. Instead, you have to be explicit (eg. ring1, ring2, ear1, ear2)
gear.tMultiSlot = {
    { ['item'] = 'Vermillion Cloak', ['slot'] = 'Body', ['affected'] = 'Head' },
    { ['item'] = 'Royal Cloak', 	 ['slot'] = 'Body', ['affected'] = 'Head' },
    { ['item'] = 'Mandra. Suit',	 ['slot'] = 'Body', ['affected'] = 'Hands,Legs,Feet' },
    { ['item'] = 'Taru. Shorts',	 ['slot'] = 'Legs', ['affected'] = 'Feet' },
    { ['item'] = 'Taru. Shorts +1',  ['slot'] = 'Legs', ['affected'] = 'Feet' },
    { ['item'] = 'Tarutaru Top',	 ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Tarutaru Top +1',  ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Wonder Top',		 ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Wonder Top +1',  	 ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Goblin Suit',      ['slot'] = 'Body', ['affected'] = 'Hands,Feet' },
};

-- List of shortcut items that can be equipped with the /equipit or /ei command
gear.tEquipIt = {
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
    ['tin']	   = { ['Name'] = 'Tinfoil Hat', ['Slot'] = 'Head' },
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

-- This structure will be dynamically populated by the fGearCheck function.
-- The slots will have a set structure providing details about every gear
-- piece in the job file/crossjobs so that when checking for the piece of
-- gear, the details that would require looking up item details will already
-- be known, thus avoiding excessive server requests.
gear.tGearDetails = {
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

-- Table of all pet food (while associated with BST, any job can equip)
gear.tPetFood = {
    [1] = { ['name'] = 'pet food alpha',  ['lvl'] = 12, ['have'] = false },
    [2] = { ['name'] = 'pet food beta',   ['lvl'] = 24, ['have'] = false },
    [3] = { ['name'] = 'pet fd. gamma',   ['lvl'] = 36, ['have'] = false },
    [4] = { ['name'] = 'pet food delta',  ['lvl'] = 48, ['have'] = false },
    [5] = { ['name'] = 'pet fd. epsilon', ['lvl'] = 60, ['have'] = false },
    [6] = { ['name'] = 'pet food zeta',   ['lvl'] = 72, ['have'] = false }
};
gear._PetFoodCount = #gear.tPetFood;

-- list of all jug pets available on HorizonXI.
gear.tJugPets = T {
    ['carrot broth']     = { ['name'] = 'Hare Familiar', ['min'] = 23, ['max'] = 35, ['have'] = false, ['fav'] = false },
    ['herbal broth']     = { ['name'] = 'Sheep Familiar', ['min'] = 23, ['max'] = 35, ['have'] = false, ['fav'] = false },
    ['humus']            = { ['name'] = 'Flowerpot Bill', ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['meat broth']       = { ['name'] = 'Tiger Familiar', ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['grass. broth']     = { ['name'] = 'Flytrap Familiar', ['min'] = 28, ['max'] = 40, ['have'] = false, ['fav'] = false },
    ['carrion broth']    = { ['name'] = 'Lizard Familiar', ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['bug broth']        = { ['name'] = 'Mayfly Familiar', ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['mole broth']       = { ['name'] = 'Eft Familiar', ['min'] = 33, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['tree sap']         = { ['name'] = 'Beetle Familiar', ['min'] = 38, ['max'] = 45, ['have'] = false, ['fav'] = false },
    ['antica broth']     = { ['name'] = 'Antlion Familiar', ['min'] = 38, ['max'] = 50, ['have'] = false, ['fav'] = false },
    ['fish broth']       = { ['name'] = 'Crab Familiar', ['min'] = 23, ['max'] = 55, ['have'] = false, ['fav'] = false },
    ['blood broth']      = { ['name'] = 'Mite Familiar', ['min'] = 43, ['max'] = 55, ['have'] = false, ['fav'] = false },
    ['f. carrot broth']  = { ['name'] = 'Keeneared Steffi', ['min'] = 43, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['s. herbal broth']  = { ['name'] = 'Lullaby Melodia', ['min'] = 43, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['rich humus']       = { ['name'] = 'Flowerpot Ben', ['min'] = 51, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['w. meat broth']    = { ['name'] = 'Saber Siravarde', ['min'] = 51, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['seedbed soil']     = { ['name'] = 'Funguar Familiar', ['min'] = 33, ['max'] = 65, ['have'] = false, ['fav'] = false },
    ['qdv. bug broth']   = { ['name'] = 'Shellbuster Orob', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['c. carrion broth'] = { ['name'] = 'Coldblood Como', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['fish oil broth']   = { ['name'] = 'Courier Carrie', ['min'] = 23, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['alchemist water']  = { ['name'] = 'Homunculus', ['min'] = 23, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['n. grass. broth']  = { ['name'] = 'Voracious Audrey', ['min'] = 53, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['l. mole broth']    = { ['name'] = 'Ambusher Allie', ['min'] = 58, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['scarlet sap']      = { ['name'] = 'Panzer Galahad', ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['c. blood broth']   = { ['name'] = 'Lifedrinker Lars', ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['f. antica broth']  = { ['name'] = 'Chopsuey Chucky', ['min'] = 63, ['max'] = 75, ['have'] = false, ['fav'] = false },
    ['sun water']        = { ['name'] = 'Amigo Sabotender', ['min'] = 75, ['max'] = 75, ['have'] = false, ['fav'] = false }
};

-- Temporary holding variables for the main hand and off hand weapons
gear.weapon = nil;
gear.offhand = nil;

-- TrackingTable tracks all targetted gear across all gearsets to manipulate
-- loading of said gear to avoid "flickering"
--
-- Table: ['gProfile'],['gs'],['slot'],['piece'],['duration'],['expiry']['primed']
gear.TrackingTable = {};

-- Temporary gear definition
gear.tGearLine = {};

local _sTrack = '//TRACK';

--[[
    BuildTrackingTable is used to generate all of the entries for the
    gear that the player has indicated should be tracked and initializes
    the values.

    Variation: TRACK a slot. It means that all gear defined for that slot
    will be tracked. (Only subsets cannot be tracked. It's way too difficult
    to even consider doing that.)
--]]


function gear.BuildTrackingTable()
    local tTarget = { gProfile.Sets, gcinclude.Sets };
    local tTemp = {};
    local iPos,ival;
    local iSec;

    -- We have to walk the sets
    for i,j in pairs({gProfile,gcinclude}) do
        -- Next walk the set definition
        for ii,jj in pairs(j) do
            -- Since track cannot be applied to "subset" or a slot, process the
            -- associated gear list.
            iPos = string.find(jj,_sTrack);
            while iPos ~= nil do
                -- We need to extract the gear piece's name and //TRACK definition
                if iPos+7 == string.len(jj) then    -- check for end of line
                    tTemp['duration'] = 10;         -- Missing duration setting, assume 10 seconds
                else
                    ival = string.match(string.sub(jj,iPos),'%d');
                    if ival > 0 then
                        tTemp['duration'] = ival;
                    else
                        tTemp['duration'] = 10;     -- 0 isn't valid. Use 10 seconds
                    end
                end
                tTemp['gProfile'] = true;
                tTemp['gs'] = i;
                tTemp['slot'] = ii;
                tTemp['primed'] = false;            -- will be set to true when this piece is equipped

                -- Need item name !!!!!

                -- Add entry to master list
                gear.TrackingTable.insert(tTemp);
                -- and see if more pieces should be tracked
                iPos = string.find(string.sub(jj,iPos+7,-1),_sTrack);
            end
        end
    end
end;        -- gear.BuildTrackingTable

--[[
    DeactivateSpentTrackingEntries is part of the tracking system to
    inhibit the flickering of gear that can occur when an inflection point
    for HP or MP is encountered. This routine walks the tracking table
    looking for entries who's "hold" settings have expired and deactivates
    the hold, accordingly.
--]]

function DeactivateSpentTrackingEntries()
end     -- DeactivateSpentTrackingEntries

--[[
    MoveToCurrent copies the gear defined in the passed set to current dynamic
    master set. Nothing is displayed, this is just a transfer routine. The
    passed set is processed and the appropriate gearpiece placed into the
    dynamic master set at each evaluated slot.

    Parameters:
    tSet            the set to process
    tMaster         the destination dynamic set
    sLimit          defines which slots should be moved
                    Please note that because it's possible to have included and
                    excluded slots at the same time (due to nexted subsets), what
                    is passed is an explicit list of slots to move. Default is ALL.
    bOverrideLocks  indicates if current locks should be ignored
    bIgnoreWSWAP    indicates if WSWAP setting should be ignored
    sSetname        identifies the name of tSet

    Change name to MoveToDynamicGS
--]]

function gear.MoveToCurrent(tSet,tMaster,sLimit,bOverrideLocks,bIgnoreWSWAP,sSetname)
    local player = gData.GetPlayer();
    local item = {};
    local ref = {};
    local ts = {};
    local ts1 = {};
    local ts2 = {};
    local root,sK,vRoot,stK,sRoot,tAffected;
    local bContinue,iNum,bGood,bSkip,bG;
    local sLimit;

    if tSet == nil or tMaster == nil then       -- missing sets
        return;
    end

    -- bIgnoreWSWAP let's the invoker ignore the check on weapon swapping
    if bIgnoreWSWAP == nil then
        bIgnoreWSWAP = false;
    end

    -- bOverride let's the invoker ignore any current locks enabled
    if bOverrideLocks == nil then
        bOverrideLocks = false;
    end

    -- sLimit indicates what slots should be included in the transfer
    if sLimit == nil then
        sLimit = 'ALL';
    end

    -- Make sure player's transition between zones is complete
    if player.MainJob == nil or player.MainJob == 'NON' then
        return;
    end

    -- Deactivate any dead entries from the tracking system
    DeactivateSpentTrackingEntries();

    -- Make sure that the passed in set is a table and not just a name.
    -- (This occurs when a subset is being processed.)
    if type(tSet) == 'string' then
        ts1 = utilities.fGetTableByName(tSet);
    else
        ts1 = tSet;
    end

    if ts1 == nil then
        return;
    end

    -- First walk through the gear slots looking for "subset"
    for k,v in pairs(ts1) do
        sK = string.lower(k);

            if string.find(sK,'subset') ~= nil then     -- Switched to a string.find since subsets can now have conditionals
                -- See if subset has an inline conditional on it and whether it's valid
                if string.find(sK,'//') then
                    bGood,tLimit = inline.fCheckSubsetInline(sK);

            if type(v) == 'table' then
                ts = v;
            else=
                ts[k] = v;
            end


            -- Then determine the appropriate set to load
            for kk,vv in pairs(ts) do
                -- In case it's a table of subsets...
                if type(vv) == 'table' then
                    ts2 = vv;
                else
                    ts2[k] = vv;
                end

                for kkk,vvv in pairs(ts2) do
                    bGood,vRoot = inline.fCheckInline(vvv,'subset',tMaster);
                    if bGood == true then
                        gear.MoveToCurrent(vRoot,tMaster,tLimit,bOverrideLocks,bIgnoreWSWAP,sSetname);
                    break;
                    end
                end
            end
        end
    end
            -- ...
end     -- gear.MoveToCurrent

--[[
    fValidateSpecial determines if the passed gear's special settings are true

    Parameters
        sSlot       The slot the piece of gear will be placed into
        sGear       The name of the piece of gear being tested

    Returned
        Are the special conditions met
--]]

function gear.fValidateSpecial(sSlot,sGear)

end     -- gear.fValidateSpecial

--[[
    fTrackingDefine makes sure that the passed definition exists in the tracking table

    Parameters
        bProfile    Profile or cross-job set
        sSet        Name of set the piece is from
        sGear       Name of gear piece to track
        iSec        How many seconds should the hold be for
--]]

-- Syntax is wrong here

function gear.fTrackingDefine(bProfile,sSet,sSlot,sGear,iSec)
    if bProfile == nil then
        bProfile = true;
    end
    if sSet == nil or sGear == nil then
        return;
    else
        if iSec == nil then
            iSec = 20;
        end
        if gear.TrackingTable[bProfile][sSet][sSlot][sGear]['duration'] == nil then
            gear.TrackingTable[bProfile][sSet][sSlot][sGear]['duration'] = iSec;
            gear.TrackingTable[bProfile][sSet][sSlot][sGear]['expiry'] = nil;
            gear.TrackingTable[bProfile][sSet][sSlot][sGear]['primed'] = false;
        end
    end
end     -- gear.fTrackingDefine

--[[
***
    FavoredJugPets determines if there's any favored BST jug pets and updates
    tJugPets accordingly.
--]]

function FavoredJugPets()
    local player = gData.GetPlayer();

    if player.MainJob == 'BST' then
        for i,j in pairs(gProfile.FavoredJugs) do
            gear.tJugPets[string.lower(j)] == true;
        end
    end
end     -- FavoredJugPets

--[[
***
    GearCheckList displays the results of a /GC command.
--]]

function gear.GearCheckList()

    if gcdisplay.GetGC() == true then
        for i,j in pairs(gear.tGearDetails) do
            print(chat.message('   [' .. i .. '] - ' .. tostring(j['num'])));
        end
    else
        print(chat.message('Warning: /GC must be run before you can get a gear check listing'))
    end
end     -- gear.GearCheckList


--[[
***
    fGearCheckItem processes the specific item sent to it and where appropriate, populates
    gear.GearDetails

    Parameters
        sSlot   - Name of the slot
        sName   - Name of the item to check
        bAccess - True = return accessibility, False = check job, access, and level

    Returned
        bAccessibility  T/F, is the item accessible
        ref             Reference to the item in tGearDetails
--]]

function fGearCheckItem(sSlot,sName,bAccess)
    local player = gData.GetPlayer();
    local bJob,bAccessible,bSlot;
    local iPos;
    local item = {};
    local tOwned = {};
    local sCodes = nil;

    -- Required fields
    if sSlot == nil or sName == nil then
        return false,nil;
    end

    -- Subsets, groups, and inline reference definitions are skipped
    if string.find('subset,group',string.lower(sSlot)) ~= nil or string.find(sName,'::') ~= nil then
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
        sCodes = string.sub(sName,iPos,-1);
        sName = string.sub(sName,1,iPos-1);
    end

    -- Now process the item
    item = AshitaCore:GetResourceManager():GetItemByName(sName,2);
    if item ~= nil then
        local bExist = (gear.tGearDetails[sSlot][sName] ~= nil); -- Note if existing record
        bJob = (bit.band(item.Jobs,utilities.JobMask[player.MainJob]) == utilities.JobMask[player.MainJob]) or
                (bit.band(item.Jobs,utilities.JobMask['Alljobs']) == utilities.JobMask['Alljobs']);
        tOwned = utilities.fCheckItemOwned(item);
        bSlot = utilities.fSlotMatch(sSlot,item.Slots);
        bAccessible = (tOwned['own'] == true and tOwned['accessible'] == true);

        -- Save item w/details
        gear.tGearDetails[sSlot][sName] = {
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
            gear.tGearDetails[sSlot][sName]['valid'] = false;
        end
        if not bExist then
            gear.tGearDetails[sSlot]['num'] = gear.tGearDetails[sSlot]['num'] + 1;
            if bAccessible then
                gear.tGearDetails[sSlot]['acc'] = gear.tGearDetails[sSlot]['acc'] + 1;
            end
        end
    else
        -- This is an erroneous item
        gear.tGearDetails[sSlot][sName] = { ['valid'] = false };
        return false,gear.tGearDetails[sSlot][sName];
    end

    -- If it still doesn't exist, return that state
    if gear.tGearDetails[sSlot][sName] == nil then
        return false,nil;
    else
        if bAccess == true then
            return (gear.tGearDetails[sSlot][sName]['accessible'] == true),gear.tGearDetails[sSlot][sName];
        else
            return (gear.tGearDetails[sSlot][sName]['job'] == true and
                    gear.tGearDetails[sSlot][sName]['accessible'] == true and
                    gear.tGearDetails[sSlot][sName]['level'] <= player.MainJobSync),
                   gear.tGearDetails[sSlot][sName];
        end
    end
end	-- gear.fGearCheckItem

--[[
***, but still need to implement more
    GearCheck is a coordinating routine that searches and extracts all the pieces of gear from all the
    gear sets in the appropriate job and crossjobsr luas
--]]

function gear.GearCheck()
	local player = gData.GetPlayer();
    local tTarget = { gProfile.Sets, crossjobs.Sets };
    local ts = {};
    local ref = {};
    local iCnt = 0;
    local bGood,s;


    -- Tallying counts from the Progressive structure
    local macc,mtacc,mracc,mtracc = fTallyProgressiveCaps();
    gcdisplay.SetAccMax(macc,mtacc,mracc,mtracc);

    -- Now start with storage slips
    print(chat.message('Info: Starting to scan for storage slips')));
    slips.FindSlips();
    s = slips.fDisplaySlips(false);
    if s == nil then
        s = 'None';
    end
    print(chat.message('Info: Found slips: ' .. s));

    -- then claim slips
    print(chat.message('Info: Starting to scan for claim slips')));
    s = ',';
    for i,j in pairs(slips.tClaimSlips) do
        if player:HasKeyItem(j['kid']) then
            j['own'] = true;
            s = s .. j['name'] .. ', ';
        end
    end
    if s == ',' then
        s = 'None';
    else
        s = string.sub(s,2,-3);
    end
    print(chat.message('Info: Found claim slips: ' .. s));

    -- next is EquipIt items
    print(chat.message('Info: Starting to scan EquipIt shortcut items')));
    for s,t in pairs(gear.tEquipIt) do
        local sSlot = t['Slot'];
        if string.find('Ring,Ear',sSlot) ~= nil then
            sSlot = sSlot .. 's';
        end
        bGood,ref = gear.fGearCheckItem(sSlot,t['Name'],false);
        if ref ~= nil and ref['valid'] == false then
            print(chat.message('Warning: Invalid EquipIt gear piece - ' .. t['Name'] .. ': ' .. s)));
        end
    end

    -- next is pet food since any job can equip it
    print(chat.message('Info: Starting to scan Pet Food items')));
    for s,t in pairs(gear.tPetFood) do
        bGood,ref = fGearCheckItem('ammo',t['name'],false);
        if ref ~= nil and ref['valid'] == false then
            print(chat.message('Warning: Invalid Pet Food - ' .. t['Name'] .. ': ' .. s)));
        end
    end

    -- next is jug pets, but only BST can equip them
    if player.MainJob == 'BST' then
        FavoredJugPets();   -- Make sure "favored" entries updated
        print(chat.message('Info: Starting to scan Jug Pets')));
        for s,t in pairs(gear.tJugPets) do
            bGood,ref = fGearCheckItem('ammo',s,false);
            if ref ~= nil and ref['valid'] == false then
                print(chat.message('Warning: Invalid Jug Pet - ' .. s .. ': ' .. s)));
            end
        end
    end

    -- now loop through the job file and crossjobs
    for s,t in pairs(tTarget) do
        if t == gProfile.Sets then
            print(chat.message('Info: Starting to scan the Job file')));
        else
            print(chat.message('Info: Starting to scan crossjobs')));
        end

        -- Loop the gear sets
        for j,k in pairs(t) do
            local fj = utilities.fFormattedWord(j,_utilities._SLOT_FA);    -- Make sure formatted correctly
            -- Process if not either 'CurrentGear' or 'Progressive'. CurrentGear
            -- is a composite from other gear sets and Progressive has a
            -- complelely different structure, it will be processed elsewhere
            if table.find({'CurrentGear','Progressive'},fj) == nil then
                -- Loop the gear set slots
                for jj,kk in pairs(k) do
                    ts = {};
                    -- Entries can be a table or a string. Make either case a table
                    if type(kk) == 'table' then
                        ts = kk;
                    else
                        ts[1] = kk;
                    end

                    if table.find(utilities.SlotNames,string.lower(jj)) == nil then
                        print(chat.message('Warning: Invalid slot name - ' .. jj .. ' in ' .. j)));
                    else
-- check for slot inlines here
                        -- Now walk the list of gear
                        for ss,tt in pairs(ts) do
                            bGood,ref = fGearCheckItem(jj,tt,false);
                            if ref ~= nil then
                                if ref['valid'] == false and ref['slot'] == nil then
                                    print(chat.message('Warning: Invalid piece of gear - ' .. tt .. ' in ' .. j)));
                                elseif ref['slot'] == false then
                                    print(chat.message('Warning: Invalid slot: ' .. jj .. ', gear - ' .. tt .. ' in ' .. j)));
                                end
                            end
                            iCnt = iCnt +1;
                            utilities.ProcessedTally('sets',iCnt,50);
                        end
                    end
                end
            elseif fj == 'Progressive' then
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

                            if table.find(utilities.SlotNames,string.lower(kj)) == nil then
                                print(chat.message('Warning: Invalid slot name - ' .. kj .. ' in Progressive ' .. ij)));
                            else
                                -- Process the list of gear
                                for ss,tt in pairs(ts) do
                                    bGood,ref = fGearCheckItem(kj,tt,false);
                                    if ref ~= nil then
                                        if ref['valid'] == false and ref['slot'] == nil then
                                            print(chat.message('Warning: Invalid piece of gear - ' .. tt .. ' in Progressive:' .. ij .. ', Stage: ' .. tostring(jj) .. ', Slot: ' .. ss)));
                                        elseif ref['slot'] == false then
                                            print(chat.message('Warning: Invalid slot: ' .. ss .. ', gear - ' .. tt .. ' in Progressive:' .. ij)));
                                        end
                                    end
                                    iCnt = iCnt +1;
                                    utilities.ProcessedTally('sets',iCnt,50);
                                end
                            end
                        end
                        iCnt = iCnt +1;
                        utilities.ProcessedTally('sets',iCnt,50);
                    end
                end
            end
        end
    end

    print(chat.message('Info: Starting to scan \'special\'')));
    for i,j in pairs(utilities.tElemental_gear) do
        if i == 'staff' then
            for ii,jj in pairs(j) do
                if string.find(crossjobs._AllElements,ii) ~= nil then
                    bGood,jj['NQ']['Ref'] = fGearCheckItem('main',jj['NQ']['Name'],false);
                    bGood,jj['HQ']['Ref'] = fGearCheckItem('main',jj['HQ']['Name'],false);
                    iCnt = iCnt + 2;
                end
                utilities.ProcessedTally('sets',iCnt,50);
            end
        elseif i == 'obi' or i == 'gorget' then
            for ii,jj in pairs(j) do
                if string.find(crossjobs._AllElements,ii) ~= nil then
                    if i == 'obi' then
                        bGood,jj['Ref'] = fGearCheckItem('waist',jj['Name'],false);
                    else
                        bGood,jj['Ref'] = fGearCheckItem('neck',jj['Name'],false);
                    end
                    iCnt = iCnt + 1;
                    utilities.ProcessedTally('sets',iCnt,50);
                end
            end
        end
    end

-- 1) Check codes goes here
-- 2) Check references goes here

    print(chat.message('Info: Scan completed')));
    print(chat.message(' ')));
    gear.GearCheckList();
end		-- gear.GearCheck

--[[
***
    TallyProgressiveCaps determines how many stages are defined in the
    Progressive entries structure: accuracy, tank accuracy, ranged
    accuracy, and tank ranged accuracy.
--]]

function fTallyProgressiveCaps()
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
end		-- fTallyProgressiveCaps

--[[
***
    fCheckForElementalGearByValue is a generalized routine that searches to see
    if the targetted elemental gear should be equipped (assuming you own the
    piece and it's accessible.)

    Parameters
        sWhat		type of elemental gear to check: staff,obi,gorget
        sWhich		which associated list to check: Affinity,Summons,MEacc,eleWS
        sElement	the key to match in the appropriate list

    return
        Record of the item,element
--]]

function gear.fCheckForElementalGearByValue(sWhat,sWhich,sElement)
    local player = gData.GetPlayer();
    local sRoot,bGood,sTarget;

    -- Make sure locks won't block equipping the item
    if sWhat == 'staff' and (locks.fIsSlotLocked('main') or locks.fIsSlotLocked('sub')) then -- staff
        return nil,nil;
    elseif sWhat == 'obi' and locks.fIsSlotLocked('waist') then -- obi
        return nil,nil;
    elseif locks.fIsSlotLocked('neck') then -- gorget
        return nil,nil;
    end

    -- What's searched for is sometimes a "root" and other times an "as-is"
    if string.find('Affinity,MEacc',sWhich) ~= nil then
        sRoot = utilities.fGetRoot(sElement);
    elseif string.find('Summons,eleWS,SongAffinity',sWhich) ~= nil then
        sRoot = string.lower(sElement);
    else
        print(chat.message('Warning: Unknown field to search: ' ..sWhich));
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
    for i,j in pairs(gear.tElemental_gear[sWhat]) do
        -- Looking for elemental entries. Ignore the rest
        if string.find(crossjobs._AllElements,i) ~= nil then
            -- Look for a match in the associated field
            if sWhat == 'staff' then
                if table.find(gear.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
                    -- Make sure the link to the dynamic table is in place
                    bGood,gear.tElemental_gear[sWhat][i]['HQ']['Ref'] =
                        gear.fGearCheckItem(sTarget,gear.tElemental_gear[sWhat][i]['HQ']['Name'],false);
                    bGood,gear.tElemental_gear[sWhat][i]['NQ']['Ref'] =
                        gear.fGearCheckItem(sTarget,gcinclude.tElemental_gear[sWhat][i]['NQ']['Name'],false);
                    -- Make sure ref in place before checking accessibility
                    if gear.tElemental_gear[sWhat][i]['HQ']['Ref'] ~= nil and
                            gear.tElemental_gear[sWhat][i]['HQ']['Ref']['accessible'] == true then
                        return gear.tElemental_gear[sWhat][i]['HQ']['Name'],i;
                    elseif gear.tElemental_gear[sWhat][i]['NQ']['Ref'] ~= nil and
                            gear.tElemental_gear[sWhat][i]['NQ']['Ref']['accessible'] == true then
                        return gear.tElemental_gear[sWhat][i]['NQ']['Name'],i;
                    else
                        return nil,nil;
                    end
                end
            elseif sWhat == 'obi' or sWhat == 'gorget' then
                if table.find(gear.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
                    bGood,gear.tElemental_gear[sWhat][i]['Ref'] =
                        gear.fGearCheckItem(sTarget,gcinclude.tElemental_gear[sWhat][i]['Name'],false);
                end

                -- Then determine if there's an obi or gorget that matches
                if gear.tElemental_gear[sWhat][i]['Ref'] ~= nil and
                        gear.tElemental_gear[sWhat][i]['Ref']['accessible'] == true then
                    return gear.tElemental_gear[sWhat][i]['Name'],i;
                end
            end
        end
    end

    -- Since we got here, either the search string wasn't found in the appropriate
    -- area or it was found, but the player doesn't have the item or it's inaccessible.
    return nil,nil;
end		-- gear.fCheckForElementalGearByValue
