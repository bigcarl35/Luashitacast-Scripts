local locks = T{};

local utilities = require('common.utilities');
local gear = require('common.gear');

--[[
    This component contains all routines that deal with locks

    List of routines-
        Subroutines:
            LockByGearSet           Locks all slots that have gear in them from passed gear set
            LockUnlock              Locks or unlocks specified slots
            ProcessLocks            Processes the invocation of the lock/unlock command

        Functions:
            fAreSlotsLocked         Determines if one or more passed slots are locked
            fGetLockedList          Returns comma delimited list of locked slots
            fIsSlotLocked           Determines if passed slot is locked
            fMultiSlotLockCheck     Determines if multislotted item blocked by locks
--]]

-- The following structure is used for locks
locks.tLocks = {
    [1] =  { ['slot'] = 'main',  ['mask'] = {1,3},              ['lock'] = false },
    [2] =  { ['slot'] = 'sub',   ['mask'] = {2,3},              ['lock'] = false },
    [3] =  { ['slot'] = 'range', ['mask'] = {4},                ['lock'] = false },
    [4] =  { ['slot'] = 'ammo',  ['mask'] = {8},                ['lock'] = false },
    [5] =  { ['slot'] = 'head',  ['mask'] = {16},               ['lock'] = false },
    [6] =  { ['slot'] = 'neck',  ['mask'] = {512},              ['lock'] = false },
    [7] =  { ['slot'] = 'ear1',  ['mask'] = {2048,4096,6144},   ['lock'] = false },
    [8] =  { ['slot'] = 'ear2',  ['mask'] = {2048,4096,6144},   ['lock'] = false },
    [9] =  { ['slot'] = 'body',  ['mask'] = {32},               ['lock'] = false },
    [10] = { ['slot'] = 'hands', ['mask'] = {64},               ['lock'] = false },
    [11] = { ['slot'] = 'ring1', ['mask'] = {8192,16384,24576}, ['lock'] = false },
    [12] = { ['slot'] = 'ring2', ['mask'] = {8192,16384,24576}, ['lock'] = false },
    [13] = { ['slot'] = 'back',  ['mask'] = {32768},            ['lock'] = false },
    [14] = { ['slot'] = 'waist', ['mask'] = {1024},             ['lock'] = false },
    [15] = { ['slot'] = 'legs',  ['mask'] = {128},              ['lock'] = false },
    [16] = { ['slot'] = 'feet',  ['mask'] = {256},              ['lock'] = false }
};

-- Define list of locks
locks.LocksNumeric = 'None';

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
        local slot = utilities.fValidSlots(val,utilities._SLOT_LA);
        if slot == nil then
            print(chat.message('Warning: unrecognized slot: ' .. val));
            return true;    -- This error should never occur. Assume it's locked.
        end
        return locks.tLocks[slot]['lock'];
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
        for i,j in pairs(locks.tLocks) do
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

            if locks.tLocks[sSlot] ~= nil then
                -- Getting here means the slot was valid
                if locks.tLocks[sSlot]['lock'] == true then
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
                print(chat.message('Warning: Unrecognized slot in ' .. vals .. ': ' .. sSlot)));
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
        bDisplay        should the display bar be updated

    Note: The previous implementation assumed all associated slots with the gearset would be
    occuppied. This implementation makes sure that's true and if not, will not lock the empty
    slot.

    Note 2: This routine does not get rid of any current locks, it just adds locks for the
    slots in the passed in gearset that evaluate to adding a piece of gear. Also, this
    routine assumes that currently locked positions will block that passed in gearset's
    overriding gear piece and that checks for multislot items will be handled in the
    MoveToCurrent function.
--]]

function locks.LockByGearSet(gs,sExceptions,bIgnoreLocks,bIgnoreWSWAP,bDisplay)
    local tDGS = {};    -- temporary dynamic gearset

    if gs == nil then
        print(chat.message('Warning: No gearset or an undefined gearset was passed in. No slots will be locked.'));
        return;
    end

    sExceptions = string.lower(sExceptions);

    if bIgnoreLocks == nil then
        bIgnoreLocks = false;       -- Assume current locks will block a slot from being overriden
    end
    if bIgnoreWSWAP == nil then
        bIgnoreWSWAP = false;       -- Assume ignoring weapon swapping is turned off
    end
    if bDisplay == nil then
        bDisplay = true;            -- Assume that the display bar will need to be refreshed
    end

    -- Now, load up the local dynamic gearset with the evaluated gearset
    gear.MoveToDynamicGS(gs,tDGS,false,nil);

    -- Then, walk the temporary dynamic set and lock any slot entries with a value unless
    -- the slot is in the passed in exceptions list.

    for i,j in pairs(tDGS) do
        if sExceptions == nil or string.find(sExceptions,i) == nil then
            if j ~= nil and j ~= '' then
                locks.tLocks[i]['lock'] = true;
            end
        end
    end

    if bDisplay == true then
            (true);
    end
end     -- locks.LockByGearSet

--[[
    fGetLockedList determines if any of the slots are locked and returns the appropriate list

    Parameters:
        bNumeric    Indicates if slot numbers or names should be returned, comma delimited

    Returned:
        list        List of slots locked or nil
--]]

function locks.fGetLockedList(bNumeric)
    local sList = nil;
    local snList = nil;

    if bNumeric == nil then
        bNumeric = true;
    end

    for i,j in ipairs(locks.tLocks) do
        if j[sWhich] == true then
            if sList == nil then
                sList = utilities.fFormattedWord(j['slot'],utilities._SLOT_FA);
                snList = tostring(i);
                locks.LocksNumeric = tostring(i);
            else
                sList = sList .. ', ' .. utilities.fFormattedWord(j['slot'],utilities._SLOT_FA);
                snList = snList .. ',' .. tostring(i);
                locks.LocksNumeric = locks.LocksNumeric .. ',' .. tostring(i);
            end
        end
    end

    if sList == nil then
        locks.LocksNumeric = 'None';
    end

    if bNumeric == true then
        return snList;
    else
        return sList;
    end
end     -- locks.fgetLockedList

--[[
   LockControl will either set or remove the locks on the specified slots passed in. Slots can be either the
   numbered positions (1-16) or names. Multiples must be comma delimited.
   !!!
--]]

function locks.LockControl(bSet,list)
end     -- locks.LockControl

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
--]]

function locks.LockUnlock(sType,sWhich)
    local ss = utilities._LOCK .. ',' .. utilities._UNLOCK;
    local sList;

    if sWhich == nil or (string.find(ss,string.lower(sType) == nil)) then
        return;
    elseif string.lower(sWhich) ~= 'all' then
        sList = utilities.fValidSlots(sWhich,utilities._SLOT_LA);        -- fValidSlots will expand out EARS and RINGS
    else
        sList = 'all';
    end

    sList = ',' .. sList .. ',';
    for k,l in ipairs(locks.tLocks) do
        if (sWhich == ',all,') or (string.find(sWhich,l['slot']) ~= nil) then
            locks.tLocks[k][s] = (string.lower(sType) == utilities._LOCK);
        end
    end
end		-- locks.LockUnlock

--[[
    ProcessLocks processes the invocation of lock/unlock command

    Pararameter
        args		Passed argument list
--]]

function locks.ProcessLocks(args)

    if args[1] == utilities._LOCK then
        if args[2] ~= nil then
            locks.LockUnlock(utilities._LOCK,args[2]);
            sList = locks.fGetLockedList(false);
            if sList ~= nil then
                print(chat.message('Info: The following slot(s) are locked: ' .. sList));
            else
                print(chat.message('Info: All slots are unlocked'));
            end
        end
    else		-- unlock
        if args[2] == nil then
            args[2] = 'all';
        end
        locks.LockUnlock(utilities._UNLOCK,args[2]);
        if string.lower(args[2]) == 'all' then
            print(chat.message('Info: All slots are unlocked'));
        else
            print(chat.message('Info: \'' .. args[2] .. '\' have been unlocked'));
        end
        locks.fGetLockedList(true);
    end
end		-- locks.ProcessLocks
