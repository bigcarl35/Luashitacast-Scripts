local gear = {};

-- List of items that inhibit more than the obvious gear slot. Add entries as you
-- need to account for the gear you use. Please note that ears and rings are
-- not supported. Instead, you have to be explicit (eg. ring1, ring2, ear1, ear2)
gear.multiSlot = {
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

-- List of items that are used in the /equipit or /ei command. These are commonly
-- used for teleporting, exp boosts, reraise, etc
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

-- TrackingTable tracks all targetted gear across all gearsets to manipulate
-- loading of said gear to avoid "flickering"
--
-- Table: ['gProfile'],['gs'],['slot'],['piece'],['duration'],['expiry']['primed']
gear.TrackingTable = {};

--[[
    BuildTrackingTable is used to generate all of the entries for the
    gear that the player has indicated should be tracked and initializes
    the values.

    Variation: TRACK a slot. It means that all gear defined for that slot
    will be tracked. (Only subsets cannot be tracked. It's way too difficult
    to even consider doing that.)
--]]

local _sTrack = '//TRACK';

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
            else
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

