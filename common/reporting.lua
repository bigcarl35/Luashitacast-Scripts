local reporting = {};

--[[
    This component contains all functions associated with reporting

    List of routines-
        Subroutines:
            DB_ShowIt               Displays some internal settings
            DisplayCC               Displays a list of custom controls
            DisplayGD_AW            Displays list of all gear
            DisplayGD_Gs            Displays all gear for a gear set
            DisplayGD_S             Displays all gear for a slot
            DisplayMessage          Writes message to screen or file
            DisplayOnce             Displays a message once
            lDisplayItemStats       Displays the details for an item
            GearCheckList           Displays tallied results from /gc
            DisplayOut              Displays message to screen or to a file
            DisplayVersion          Displays the version and patch notes
            lGearSetListingReport   Display gear list in gearset format
            ProcessSMG              Processes the invocation of /smg
            RegionControlDisplay    Displays all regions and who controls them

        Functions:
            fCompactLocks           Generates a compact list of the locks
--]]

--[[
    DB_ShowIt will display debug details
--]]

function reporting.DB_ShowIt()
    local player = gData.GetPlayer();
    local sSlip = slips.fDisplaySlips(false);

    print(chat.message(' '));
    print(chat.message('Settings'));
    print(chat.message('--------'));
    print(chat.message('Job: ' .. player.MainJob .. '/' .. player.SubJob));
    print(chat.message('Level: ' .. tostring(player.MainJobSync) .. '(' .. tostring(player.MainJobLevel) .. ')'));
    print(chat.message(' '));
    print(chat.message('WScheck: ' .. tostring(crossjobs.settings.WScheck)));
    print(chat.message('WSdistance: ' .. tostring(crossjobs.settings.WSdistance)));
    print(chat.message('bWSOverride: ' .. tostring(crossjobs.settings.bWSOverride)));
    print(chat.message('GC run? ' .. tostring(gear,bGC)));
    if sSlip == nil then
        print(chat.message('Slips: None'));
    else
        print(chat.message('Slips: ' .. sSlip));
    end
end		-- reporting.DB_ShowIt

--[[
    DisplayVerion displays version details including the changelog since the last release.
--]]

function reporting.DisplayVersion()
    local bSkip = false;
    local rfn = gProfile.FilePath:reverse();

    -- remove the job file from path, add changelog
    rfn = string.sub(rfn,string.find(rfn,'\\'),-1);
    rfn = rfn:reverse() .. 'Documentation\\changelog.txt';

    print(chat.message(' '));
    print(chat.message(version.name .. ' Version: ' .. gVars.version));
    for line in io.lines (rfn) do
        if bSkip == false then
            print(chat.message(' '));
        bSkip = true;
    end
    print(chat.message(line));
    end
end     -- reporting.DisplayVersion

--[[
    RegionControlDisplay displays all the regions under conquest control
    along with who currently controls them.
--]]

function reporting.RegionControlDisplay()
-- List of numeric representations for who controls a region

    -- Make sure we know what nation we belong to
    if crossjobs.OwnNation == -1 then
        crossjobs.OwnNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation() + 1;
    end

    -- Make sure controller ID is valid
    if crossjobs.OwnNation < -1 or crossjobs.OwnNation > 4 then
        print(chat.message('Warning: Unknown player\'s nation = ' .. tostring(crossjobs.OwnNation)));
    else
        print(chat.message('Info: Player\'s nation = ' .. sAreas[crossjobs.OwnNation]));
    end

    print(' ');
    for i,j in pairs(gVars.RegionControl) do
        if j['own'] < 0 or j['own'] > 4 then
            print(chat.message('Huh? ' .. i ..' = ' .. tostring(j['own'])));
        else
            for ii,jj in pairs(gVars.tRegionControllerSettings) do
                if ii == j['own'] then
                    if j['own'] == 0 and utilities.fBuffed('Signet') == true then
                        print(chat.message(i .. ' = ' .. jj .. ', but \'not owned\' gear works'));
                    else
                        print(chat.message(i ..' = ' .. jj));
                    end
                    break;
                end
            end
        end
    end
end		-- reporting.RegionControlDisplay

--[[
    DisplayCC lists the valid custom conditions, their meaning, and current value
--]]

function reporting.DisplayCC()

    if gProfile.CustomConditionals ~= nil and #gProfile.CustomConditionals > 0 then
        print(chat.message('Info: Custom conditionals list:'));
        for _,j in ipairs(gProfile.CustomConditionals) do
            j['code'] = string.upper(j['code']);
            print(chat.message('   ' ... j['code'] .. ' - ' .. j['question'] .. ': ' .. utilities.GetToggle(j[code])));
        end
    else
       print(chat.message('Info: No custom conditionals are defined'));
    end
end     -- reporting.DisplayCC

--[[
    lFileItemStats displays the item definition for the passed piece of gear
    from the dynamic GearDetails table to an output file.

    Parameters
        sName       Gear name
        sSlot       Slot it equips in
        fptr        Output file pointer
--]]

function lFileItemStats(sName,sSlot,fptr)
    local msg;
    local tWhat;
    local tTrans = { [true] = 'Yes', [false] = 'No'};

    if sSlot == nil or sName == nil then
        return;
    end

    if fptr == nil then
        return;
    emd

    sSlot = string.lower(sSlot);
    sName = string.lower(sName);

    if gVars.tGearDetails[sSlot] == nil or gVars.tGearDetails[sSlot][sName] == nil then
        -- You get here if the item isn't a valid item
        fptr.write('   ' .. sName .. ' - Invalid item');
        return;
    end

    tWhat = gVars.tGearDetails[sSlot][sName];
    -- You get here if the item is valid or it's invalid because the slot
    -- is incorrect
    msg = '   ' .. string.upper(sName);
    msg = msg .. ', Level: ' .. tostring(tWhat['level']);
    fptr.write(msg);
    msg = '      ' .. 'Own it? ' .. tTrans[tWhat['own']];
    msg = msg .. ', Accessible? ' .. tTrans[tWhat['accessible']];
    fptr.write(msg);
    fptr.write('      Code Breakdown-');
    msg = '         Valid? ' .. tTrans[tWhat['valid']];
    msg = msg .. ' Slot? ' .. tTrans[tWhat['slot']];
    msg = msg .. ' Job? ' .. tTrans[tWhat['job']];
    msg = msg .. ' Porter? ' .. tTrans[tWhat['porter']];
    msg = msg .. ' Claim? ' .. tTrans[tWhat['claim']];
    fptr.write(msg);
    if tWhat['locations'] ~= nil then
        msg = '         Location(s): ' .. tWhat['locations'];
    else
        msg = '         Location(s): ';
    end
    fptr.write(fptr,msg);
    fptr.write(' ');
end	-- lFileItemStats

--[[
    lDisplayItemStats displays the item definition for the passed piece of gear
    from the dynamic GearDetails table.

    Parameters
        sName       Gear name
        sSlot       Slot it equips in
--]]

function lDisplayItemStats(sName,sSlot)
    local msg;
    local tWhat;
    local tTrans = { [true] = 'Yes', [false] = 'No'};

    if sSlot == nil or sName == nil then
        return;
    end

    sSlot = string.lower(sSlot);
    sName = string.lower(sName);

    if gear.GearDetails[sSlot] == nil or gear.GearDetails[sSlot][sName] == nil then
        -- You get here if the item isn't a valid item
        print('   ' .. sName .. ' - ' .. chat.color1(utilities.fSetColorText('Invalid item',false)));
        return;
    end

    tWhat = gear.GearDetails[sSlot][sName];
    -- You get here if the item is valid or it's invalid because the slot
    -- is incorrect
    msg = '   ' .. chat.color1(utilities.fSetColorText(nil), string.upper(sName));
    msg = msg .. ', Level: ' .. tostring(tWhat['level']);
    print(msg);
    msg = '      ' .. 'Own it? ' .. chat.color1(utilities.fSetColorText(tWhat['own']),tTrans[tWhat['own']]);
    msg = msg .. ', Accessible? ' .. chat.color1(utilities.fSetColorText(tWhat['accessible']),tTrans[tWhat['accessible']]);
    print(msg);
    print('      Code Breakdown-');
    msg = '         Valid? ' .. chat.color1(utilities.fSetColorText(tWhat['valid']),tTrans[tWhat['valid']]);
    msg = msg .. ' Slot? ' .. chat.color1(utilities.fSetColorText(tWhat['slot']),tTrans[tWhat['slot']]);
    msg = msg .. ' Job? ' .. chat.color1(utilities.fSetColorText(tWhat['job']),tTrans[tWhat['job']]);
    msg = msg .. ' Porter? ' .. chat.color1(utilities.fSetColorText(tWhat['porter'],true),tTrans[tWhat['porter']]);
    msg = msg .. ' Claim? ' .. chat.color1(utilities.fSetColorText(tWhat['claim'],true),tTrans[tWhat['claim']]);
    print(msg);
    if tWhat['locations'] ~= nil then
        msg = '         Location(s): ' .. tWhat['locations'];
    else
        msg = '         Location(s): ';
    end
    print(msg);
    print(' ');
end	-- lDisplayItemStats

--[[
    ProcessSMG processes the invocation of Show My Gear reporting command.

    Syntax
        /smg gs|sl [noac] [gs=set name,set name,...] [slot=slot name,slot name,...] [|]

        The invocation parameters can be applied in any order. The more you
        specify, the more restricted the report. Each invocation produces only
        one report. Please note that all parameters beyond the function call
        are optional.

        "gs|sl" is required to indicate the type of report that is wanted:
        gear set based or slot based. Lack of specifying either defaults to
        a gearset report.

        "/smg" with no parameters reports all gear associated with all
        gear sets.

        "noac" indicates that only items that are either invalid or unaccessible
        will be displayed.

        "gs=" is where you can restrict which gear sets are reported to a specific
        list of gear sets.

        "slot=" is where you can restrict which slots are reported to a specific
        list of slots.

        "|" indicates that the report's output should be redirected
        to a file. This will specifically be in the Reports directory found
        under ...HorizonXI\Game\config\addons\luashitacast. The file name
        will be formatted as follows: name_job_date.txt.

    Note: This implementation is different than what was done in version 2.0. It
    was changed because it makes more sense. If you want multiple reports,
    you need to run /smg multiple times. If the output is being redirected to a
    file, multiple reports will be appended to the same file as long as they
    are done on the same, real world, day.

    Pararameter
        args		Passed argument list
--]]

function reporting.ProcessSMG(args)
    local lv,iPos,fptr;
    local rec = {
        ['type'] = 'gs', ['bNoac'] = false, ['gs'] = nil, ['tgs'] = {},
        ['slot'] = nil, ['tslot'] = {}, ['filename'] = nil
    };

    -- Parse the arguments and record the appropriate aspects into the
    -- SMG definition record.

    if #args > 1 then
        for i = 1,#args,1 do
            lv = string.lower(args[i]);

            if lv == 'noac' then
                rec['bNoac'] = true;
            if string.find('gs,sl',lv) ~= nil then
                rec['type'] = lv;
            elseif string.find(lv,'gs=') ~= nil and string.len(lv) > 3 then
                rec['gs'] = string.sub(args[i],4,-1));      -- skip the gs=
                rec['tgs'] = utilities.fSplitStringByDelimiter(rec['gs'],',');
            elseif string.find(lv,'slot=') ~= nil and string.len(lv) > 5 then
                rec['slot'] = string.sub(args[i],6,-1));    -- skip the slot=
                rec['tslot'] = utilities.fSplitStringByDelimiter(rec['slot'],',');
            elseif string.sub(lv,1,1) == '|' then
                rec['bFile'] = true
                rec['filename'] = utilities.fNewFileName();
            end
        end

        -- Make sure that the gear sets and slots are defined
        if rec['gs'] == nil then
            rec['tgs'] = utilities.fGetAllGearSetNames();
        end

        if rec['slot'] == nil then
            rec['tslot'] = utilities.fGetAllSlotNames();
        end
    end

    if rec['filename'] ~= nil then
        -- Since going to a file, make sure it's open
        fptr = utilities.OpenByFilename(rec['filename']);
    end


    -- There are two types of reports: by gear sets and by slots
    if rec['type'] == 'gs' then
        -- Type is a gear set report
        if rec['bNoac'] == true then
            -- Report filtered to only show invalid or inaccessible definitions
            print(chat.message('Creating gearset report of any inaccessible or erroneous gear'));
        else
            -- Report on all gear sets
            print(chat.message('Creating gearset report of all gear'));
        end
        lGearSetListingReport(rec,fptr);
    else
        -- Type is a slots report
        if rec['bNoac'] == true then
            -- Report filtered to only show invalid or inaccessible definitions
            print(chat.message('Creating slot report of any inaccessible or erroneous gear'));
        else
            -- Report on all gear sets
            print(chat.message('Creating slot report of all gear'));
        end
        lSlotListingReport(rec,fptr);
    end

    if rec['bFile'] == true then
        print(chat.message('Complete. Report(s) written to ' .. rec['filename'] .. 'in the Reports directory'));
        io.close(fptr);
    else
        print(chat.message('Report(s) complete!'));
    end
end		-- reporting.ProcessSMG

--[[
    lGearSetListingReport generates a gear set report based on the the settings found in
    the driving record.

    Parameters
        rec     Record containing details on how the gearset report will be generated
        fptr    nil or file pointer. Indicates if the report should be displayed or saved
                to a file
--]]

function lGearSetListingReport(rec,fptr)
    local t = {};
    local ljj;
    local tgs,msg;
    local sOp,sOp2;

    if rec['gs'] == nil then
        sOp = '*';
    else
        sOp = rec['gs'];
    end

    if rec['slot'] == nil then
        sOp2 = '*';
    else
        sOp2 = rec['gs'];
    end

    msg ='Gearset Report, restrictions: gs= '..sOp .. ', slot= ' .. sOp2 .. ', Noac= '.. tostring(rec['bNoac']);
    if fptr == nil then
        print(msg);
        print(' ');
    else
        fptr.write(msg .. '\n');
        fptr.write('\n');
    end

    for i,j in pairs(rec.tgs) do
        -- For each gear set
        t = {};
        if fptr == nil then
            print('Gearset: ' .. utilities.fTranslateWhichSlot(j,gVars._SLOT_FA))
        else
            fptr.write('Gearset: ' .. utilities.fTranslateWhichSlot(j,gVars._SLOT_FA) .. '\n');
        end

        for k,l in ipairs(gVars.tSlotNames['smg']) do
            -- For each slot
            if sOp2 == '*' or string.find(rec['slot'],l) ~= nil then
                -- Either any slot or slot found in the specified slot list
                if fptr == nil then
                    print('Slot: ' .. utilities.fTranslateWhichSlot(l,gVars._SLOT_FA));
                else
                    fptr.write('Slot: ' .. utilities.fTranslateWhichSlot(l,gVars._SLOT_FA) .. '\n');
                end

                if gVars.tGearsetDetails[j][l] ~= nil then
                    for m = 1,#gVars.tGearsetDetails[j][l]['items'],1 do
                        local item = gVars.tGearsetDetails[j][l]['items'][m];
                        if (rec['bNoac'] == true and
                            (gVars.tGearDetails[l][item.gear]['valid'] == false or
                            gVars.tGearDetails[l][item.gear]['accessible'] == false)) or
                            rec['bNoac'] == false
                        then
                            -- Make sure the item id has not been displayed already for the gearset/slot
                            local bFound = false;
                            for aa=1,#t,1 do
                                if t[aa] == item.id then
                                    bFound = true;
                                    break;
                                end
                            end
                            if bFound == false then
                                if fptr == nil then
                                    lDisplayItemStats(item.gear,l);
                                else
                                    lFileItemStats(item.gear,l,fptr);
                                end
                                t[#t+1] = item.id;
                            end
                        end
                    end
                end
            end
        end
        if fptr == nil then
            print(' ');
        else
            fptr.write('\n');
        end
    end
end     -- lGearSetListingReport

--[[
    lSlotListingReport generates a gear set report based on the the settings found in
    the driving record, driven by the slot

    Parameters
        rec     Record containing details on how the gearset report will be generated
        fptr    nil or file pointer. Indicates if the report should be displayed or saved
                to a file
--]]

function lSlotListingReport(rec,fptr)
    local ljj;
    local tgs,msg;
    local sOp,sOp2;
    local items = {};
    local t = {};

    if rec['gs'] == nil then
        sOp = '*';
    else
        sOp = rec['gs'];
    end

    if rec['slot'] == nil then
        sOp2 = '*';
    else
        sOp2 = rec['gs'];
    end

    msg ='Slot Report, restrictions: gs= '..sOp .. ', slot= ' .. sOp2 .. ', Noac= '.. tostring(rec['bNoac']);
    if fptr == nil then
        print(msg);
        print(' ');
    else
        fptr.write(msg .. '\n');
        fptr.write('\n');
    end

     for k,l in ipairs(gVars.tSlotNames['smg']) do
        -- For each slot
        t = {};
        if sOp2 == '*' or string.find(rec['slot'],l) ~= nil then
            -- Either any slot or slot found in the specified slot list
            if fptr == nil then
                print('Slot: ' .. utilities.fTranslateWhichSlot(l,gVars._SLOT_FA));
            else
                fptr.write('Slot: ' .. utilities.fTranslateWhichSlot(l,gVars._SLOT_FA) .. '\n');
            end
            for i,j in pairs(rec.tgs) do
                -- For each gear set




                if gVars.tGearsetDetails[j][l] ~= nil then
                    for m = 1,#gVars.tGearsetDetails[j][l]['items'],1 do
                        local item = gVars.tGearsetDetails[j][l]['items'][m];
                        if (rec['bNoac'] == true and
                            (gVars.tGearDetails[l][item.gear]['valid'] == false or
                            gVars.tGearDetails[l][item.gear]['accessible'] == false)) or
                            rec['bNoac'] == false
                        then
                            if fptr == nil then
                                lDisplayItemStats(item.gear,l);
                            else
                                lFileItemStats(item.gear,l,fptr);
                            end
                        end
                    end
                end
            end
        end
        if fptr == nil then
            print(' ');
        else
            fptr.write('\n');
        end
    end
end     -- lSlotListingReport

--[[
    DisplayMessage is a print function that displays the passed in message to
    either a file or the screen

    Parameters
        smsg        Message to be displayed
        pFile       File pointer to append message to or nil
--]]

function reporting.DisplayMessage(pFile,smsg,sfmsg)

    if pfile ~= nil then
        if sfmsg == nil then
            sfmsg = smsg;
        end
        -- Write to the file
        pFile.write(sfmsg);
    else
        print(chat.message(smsg));
    end
end     -- reporting.DisplayMessage

--[[
    GearCheckList displays the results of a /GC command.
--]]

function reporting.GearCheckList()

    if gear.fHasGCBeenRun() == true then
        for i,j in pairs(gVars.tGearDetails) do
            print(chat.message('   [' .. i .. '] - ' .. tostring(j['num'])));
        end
    else
        print(chat.message('Warning: /GC must be run before you can get a gear check listing'))
    end
end     -- reporting.GearCheckList

--[[
    DisplayOnce displays the passed message on the screen unless it has already
    been displayed once berfore. In that case, it will not be displayed. The "once"
    behavior can be bOverriden.

    Parameters:
        msg         What should be displayed
        bOverride   Should the "only display once" rule be ignored
--]]

function reporting.DisplayOnce(msg,bOverride)
    local tmp;

    if msg == nil then
        return;
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

    if gVars.GearWarnings == nil or (gVars.GearWarnings ~= nil and string.find(gVars.GearWarnings,tmp) == nil) or
        bOverride == true then
        print(chat.message(msg));

        if gVars.GearWarnings == nil then
            gVars.GearWarnings = msg;
        else
            gVars.GearWarnings = gVars.GearWarnings .. ',' .. msg;
        end
    end
end     -- reporting.fDisplayOnce

--[[
    fCompactLocks walks the locks list and generates a compact display of the active locks.

    Return:
        List of active locks
--]]

function reporting.fCompactLocks()
    local bFound = false;
    local iStart = nil;
    local sList = nil;

    for i,j in ipairs(locks.tLocks) do
        if j['lock'] == true then
            -- locked slot. Tag if no range started yet
            if iStart == nil then
                iStart = i;
            end
        elseif iStart ~= nil then
            -- no lock, but indicates range is done
            if i == (iStart + 1) then
                -- No range, just back to back locks
                if sList == nil then
                    sList = tostring(iStart) ;
                else
                    sList = sList .. ',' .. tostring(iStart);
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
        if iStart < 16 then
            if sList == nil then
                sList = tostring(iStart) .. '-16';
            else
                sList = sList .. ',' .. tostring(iStart) .. '-16';
            end
        else
            if sList == nil then
                sList = '16'
            else
                sList = sList .. ',16';
            end
        end
    end
end     -- reporting.fCompactLocks

return reporting;
