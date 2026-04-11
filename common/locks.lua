local locks = {};

-- List of all slots used for tracking locks and for the dynamic temporary locks
locks.tSlotLocks = {
    [1] =  { ['slot'] = 'main',  ['mask'] = {1,3},              ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [2] =  { ['slot'] = 'sub',   ['mask'] = {2,3},              ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [3] =  { ['slot'] = 'range', ['mask'] = {4},                ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [4] =  { ['slot'] = 'ammo',  ['mask'] = {8},                ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [5] =  { ['slot'] = 'head',  ['mask'] = {16},               ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [6] =  { ['slot'] = 'neck',  ['mask'] = {512},              ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [7] =  { ['slot'] = 'ear1',  ['mask'] = {2048,4096,6144},   ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [8] =  { ['slot'] = 'ear2',  ['mask'] = {2048,4096,6144},   ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [9] =  { ['slot'] = 'body',  ['mask'] = {32},               ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [10] = { ['slot'] = 'hands', ['mask'] = {64},               ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [11] = { ['slot'] = 'ring1', ['mask'] = {8192,16384,24576}, ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [12] = { ['slot'] = 'ring2', ['mask'] = {8192,16384,24576}, ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [13] = { ['slot'] = 'back',  ['mask'] = {32768},            ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [14] = { ['slot'] = 'waist', ['mask'] = {1024},             ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [15] = { ['slot'] = 'legs',  ['mask'] = {128},              ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil },
    [16] = { ['slot'] = 'feet',  ['mask'] = {256},              ['lock'] = false, ['bPrimed'] = false, ['expiry'] = nil }
};

--[[
    This component contains all routines that deal with locks

    List of routines-
        Subroutines:
            LockByGearSet           Locks all slots that have gear in them from passed gear set
            LockUnlock              Locks or unlocks specified slots
            ProcessLocks            Processes the invocation of the lock/unlock command

        Functions:
            fAreSlotsLocked         Determines if one or more passed slots are locked
            fCompactLocks           Returns comma delimited list of locks in a compact manner
            fGetLockedList          Returns comma delimited list of locked slots
            fIsSlotLocked           Determines if passed slot is locked
            fMultiSlotLockCheck     Determines if multislotted item blocked by locks
--]]

--[[
	fIsSlotLocked determines if the passed slot is locked. Please note that only slot
	names are supported. Slot numbers will cause an error

    Parameter
        val     slot name

	Returned: T/F
--]]

function locks.fIsSlotLocked(val)
	if val == nil then
		print(chat.message('Warning: slot undefined'));
		return true;	-- This error should never occur. Assume it's locked.
	elseif string.find(val,',') ~= nil then    -- Checking for a list of slots
        print(chat.message('Warning: only one slot can be identified. ' .. val .. ' was sent to be processed'));
        return true;    -- This error should never occur. Assume it's locked.
    else
        local bGood,slot = utilities.fValidSlots(val,gVars._SLOT_LA);
        if bGood == false then
            print(chat.message('Warning: unrecognized slot: ' .. val));
            return true;    -- This error should never occur. Assume it's locked.
        end
        for i,j in ipairs(locks.tSlotLocks) do
            if j['slot'] == slot then
                return locks.tSlotLocks[i]['lock'];
            end
        end
    end
    return true;	-- This line should never be encountered. Assume it's locked.
end		-- locks.fIsSlotLocked

--[[
    fAreSlotsLocked determines if one or more of the slots passed in are locked.

    Parameters:
        vals    nil     Are any slots locked or
                values  Are any of the specified slots locked, names or numbers valid, can be mixed and matched
        bAll    T/F     Check for all slots locked or at least one locked

    Returned:
        true            Based on bAll, locks have been found that match the passed conditions or if an error occurred (Assume locked then.)
        false           No slots are locked
--]]

function locks.fAreSlotsLocked(vals,bAll)
    local sSlot = nil;
    local sHold = vals;

    if bAll == nil then
        bAll = false;       -- Assume only one slot needs to be locked for true to be returned
    end

    if vals == nil then     -- Process all equipment
        for i,j in pairs(locks.tSlotLocks) do
            if bAll == true then
                if j['lock'] == false then  -- This is a failure since all slots need to be locked
                    return false;
                end
            else
                if j['lock'] == true then   -- This is a success since what is needed is at least one slot locked
                    return true;
                end
            end
        end

        -- Since no matches were found, each case is considered
        if bAll == true then    -- All had to be locked to get here
            return true;
        else                    -- None were locked so this is false
            return false;
        end
    else
        -- Since one or more slots were passed in, we don't need to check the slot validity, just process as if valid
        vals = string.lower(vals);
        local iPos = string.find(vals,',');
        repeat
            if iPos ~= nil then
                sSlot = string.sub(vals,1,iPos-1);
                vals = string.sub(vals,iPos+1,-1);
            else
                sSlot = vals;
                vals = nil;
            end

            if locks.tSlotLocks[sSlot] ~= nil then
                -- Getting here means the slot was valid
                if locks.tSlotLocks[sSlot]['lock'] == true then
                    if bAll == false then
                        -- We found one which is enough
                        return true;
                    end
                else
                    -- Can only get here if the lock is false
                    if bAll == true then
                        -- All have to be locked, so this is a failure
                        return false;
                    end
                end
            else
                -- Can only get here if the slot is unrecognized
                print(chat.message('Warning: Unrecognized slot in ' .. vals .. ': ' .. sSlot));
                return false;
            end
        until vals == nil;
    end

    -- Getting here means that no condition was decided yet

    -- Turns out if bAll is true, then the condition was met. And if bAll is false,
    -- then the condition was not met. Just returning the value of bAll returns the
    -- same results as explicitly checking bAll for true or false. Spelling it out
    -- might make more sense, but the behavior as-is returns the correct result.
    return bAll;
end     -- locks.fAreSlotsLocked

--[[
    LockByGearSet will lock all of the slots associated with the passed gear set. This is done by
    evaluating the identified gearset and loading it into a temporary dynamic gear set. Once done,
    then that gear set is walked and the appropriate slots are locked. This is the easiest way to
    determine which slots' conditions will be met without impacting the normal dynamic gear set.

    Parameters:
        gs              gear set reference
        sExceptions     nil or list of slots to omit
        bIgnoreLocks    lets invoke ignore current locks
        bIgnoreWSWAP    lets invoke ignore WSWAP setting

    Note: The previous implementation assumed all associated slots with the gearset would be
    occuppied. This implementation makes sure that's true and if not, will not lock the empty
    slot.

    Note 2: This version assumes that all of the slots in the passed in gearset that have
    items are what is to be locked. Multislotted items and any current locks have already
    been addressed.
--]]

function locks.LockByGearSet(gs,sExceptions,bDisplay)

    if gs == nil then
        print(chat.message('Warning: No gearset or an undefined gearset was passed in. No slots will be locked.'));
        return;
    end

    if sExceptions ~= nil then
        sExceptions = string.lower(sExceptions);
    end

    if bDisplay == nil then
        bDisplay = true;            -- Assume that the display bar will need to be refreshed
    end

    for i,j in pairs(gs) do
        if not (j == nil or j == '') then
            if sExceptions == nil or string.find(sExceptions,i) == nil then
                for ii,jj in pairs(locks.tSlotLocks) do
                    if jj['slot'] == utilities.fFormattedWord(i,gVars._SLOT_LA) then
                        locks.tSlotLocks[ii]['lock'] = true;
                    end
                end
            end
        end
    end
end     -- locks.LockByGearSet

--[[
    fMultiSlotLockCheck determines if the passed item is a multislotted item and
    whether there's a lock in place that would inhibit the equipping of the
    item

    Parameter
        sName       Name of item

    Returned
        bGood       Are there no locks for affected slots
        bMulti      Is the item a multi-slot item
        sAllSlots   List of affected slots

    Returned: good?,multi-slot?,list of slots
    note: ears and rings are valid here

    !!!
--]]

function locks.fMultiSlotLockCheck(sName)
    local sAffected,sSlot,sMain;
    local bGood = true;
    local bMulti = false;
    local bFound = false;
    local sAllSlots = nil;

    if sName == nil then	-- Nothing specified, nothing to check
        return true,false,nil;
    end

    -- Walk the list of multi-slotted items
    for j,k in pairs(gear.tMultiSlot) do
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

                if locks.fIsSlotLocked(sSlot) == true then
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
        bGood = not locks.fIsSlotLocked(sMain);
    end
    return bGood,bMulti,sAllSlots;
end		-- locks.fMultiSlotLockCheck

--[[
    LockUnlock locks or unlocks the specified slots.

    Parameters
        sType           'lock' or 'unlock'
        sWhich          Which slots are affected

    Returned
                        T/F, were the specified locks found
--]]

function locks.LockUnlock(sType,sWhich)
    local ss = gVars._LOCK .. ',' .. gVars._UNLOCK;
    local sList;
    local bGood;

    if string.lower(sWhich) ~= 'all' then
        bGood,sList = utilities.fValidSlots(sWhich,gVars._SLOT_LA);        -- fValidSlots will expand out EARS and RINGS
        if bGood == false then
            print(chat.message('Warning: invalid slot(s) specified: ' .. sWhich));
            return false;
        end
    else
        sList = 'all';
    end

    sList = ',' .. sList .. ',';
    for k,l in ipairs(locks.tSlotLocks) do
        if (sList == ',all,') or (string.find(sList,l['slot']) ~= nil) then
            locks.tSlotLocks[k]['lock'] = (string.lower(sType) == gVars._LOCK);
        end
    end
    return true;
end		-- locks.LockUnlock

--[[
    ProcessLocks processes the invocation of lock/unlock command

    Pararameter
        args		Passed argument list

    Invocation:
        /lock [slot name|slot number[,...] ] [visible|invisible]
        /unlock [all|list of slot names|list of slot numbers]
--]]

function locks.ProcessLocks(args)
    local slots = nil;

    if utilities.fCheckVisibility(gVars._LOCKS,args) == false then
        for i,j in pairs(args) do
            j = string.lower(j);
            if string.find('lock,unlock,visible,invisible',j) == nil then
                slots = j;
            end
        end
    end

    if args[1] == gVars._UNLOCK or slots == nil then
        -- Both /unlock or /lock with no slots will unlock
        if slots == nil then
            slots = 'all';
        else
            slots = string.lower(slots);
        end

        if locks.LockUnlock(gVars._UNLOCK,slots) == true then
            if slots == 'all' and gProfile.settings.bConfirmation == true then
                print(chat.message('Info: All slots are unlocked'));
            else
                print(chat.message('Info: \'' .. slots .. '\' have been unlocked'));
            end
        end
    else
        if locks.LockUnlock(gVars._LOCK,slots) == true then
            if slots ~= nil and gProfile.settings.bConfirmation == true then
                print(chat.message('Info: The following slot(s) are locked: ' .. slots));
            else
                print(chat.message('Info: All slots are unlocked'));
            end
        end
    end
end		-- locks.ProcessLocks

--[[
    fCompactLocks walks the locks list and generates a compact display of the active locks.

    Return:
        List of active locks
--]]

function locks.fCompactLocks()
    local bFound = false;
    local iStart = nil;
    local sList = nil;

    for i,j in ipairs(locks.tSlotLocks) do
        if j['lock'] == true then
            -- locked slot. Tag if no range started yet
            if iStart == nil then
                iStart = i;
            end
        elseif iStart ~= nil then
            -- no lock, but indicates range is done
            if i == iStart + 1 then
                -- No range, just a single #
                if sList == nil then
                    sList = tostring(iStart);
                else
                    sList = sList .. ',' .. tostring(iStart);
                end
            elseif i == iStart + 2 then
                -- No range, just back to back locks
                if sList == nil then
                    sList = tostring(iStart) .. ',' .. tostring(iStart+1);
                else
                    sList = sList .. ',' .. tostring(iStart) .. ',' .. tostring(iStart+1);
                end
            else
                -- it is a range
                if sList == nil then
                    sList = tostring(iStart) .. '-' .. tostring(i-1);
                else
                    sList = sList .. ',' .. tostring(iStart) .. '-' .. tostring(i-1);
                end
            end
            iStart = nil;
        end
    end

    if iStart ~= nil then
        if iStart+1 < 16 then
            if sList == nil then
                sList = tostring(iStart) .. '-16';
            else
                sList = sList .. ',' .. tostring(iStart) .. '-16';
            end
        elseif iStart+1 == 16 then
            if sList == nil then
                sList = '15,16';
            else
                sList = sList .. ',15,16';
            end
        else
            if sList == nil then
                sList = '16'
            else
                sList = sList .. ',16';
            end
        end
    end

    if sList == nil then
        sList = 'None';
    end
    return sList;
end     -- locks.fCompactLocks

return locks;
