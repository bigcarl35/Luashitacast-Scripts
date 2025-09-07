local reporting = T{};

local gcdisplay = require('gcdisplay');
local crossjobs = require('crossjobs');
local gear      = require('gear');

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
    print(chat.message('WScheck: ' .. tostring(gcinclude.settings.WScheck)));
    print(chat.message('WSdistance: ' .. tostring(gcinclude.settings.WSdistance)));
    print(chat.message('bWSOverride: ' .. tostring(gcinclude.settings.bWSOverride)));
    print(chat.message('GC run? ' .. tostring(gcdisplay.GetGC())));
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
    print(chat.message(version.name .. ' Version: ' .. tostring(version.version)));
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
    for i,j in pairs(crossjobs.RegionControl) do
        if j['own'] < 0 or j['own'] > 4 then
            print(chat.message('Huh? ' .. i ..' = ' .. tostring(j['own'])));
        else
            for ii,jj in pairs(utilities.RegionAreas) do
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
    DisplayItemStats displays the item definition for the passed piece of gear
    from the dynamic GearDetails table.

    Parameters
        sName       Gear name
        sSlot       Slot it equips in
--]]

function DisplayItemStats(sName,sSlot)
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
        print(sName .. ' - ' .. chat.color1(utilities.fSetColorText('Invalid item',false)));
        return;
    end

    tWhat = gear.GearDetails[sSlot][sName];
    -- You get here if the item is valid or it's invalid because the slot
    -- is incorrect
    msg = '   ' .. chat.color1(utilities.fSetColorText(nil), string.upper(sName));
    msg = msg .. ', Level: ' .. tostring(tWhat['level'],tostring(tWhat['level']));
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
end	-- DisplayItemStats

--[[
    DisplayGD_AW lists either all the dynamic gear definitions or just the
    ones that are invalid or inaccessible.

    Parameter
        p1      nil for all gear, 'noac' for invalid or inaccessible gear
--]]

function reporting.DisplayGD_AW(p1)
    local bShow;

    if p1 == nil then
        print(chat.message('Complete list of all gear'));
    elseif p1 == 'noac' then
        print(chat.message('Invalid or inaccessible gear'));
    end

    for slot,name in pairs(gear.GearDetails) do
        print(chat.message(' '));
        if p1 ~= nil and string.lower(p1) == 'noac' then
            print(chat.message('Slot: ' .. slot));
        else
            print(chat.message('Slot: ' .. slot .. '[' .. tostring(name['acc']) .. '/' .. tostring(name['num']) .. ']'));
        end

        for i,j in pairs(name) do
            if string.find('num,acc,vis',i) == nil then
                bShow = (p1 == nil or j['valid'] == false or j['accessible'] == false);
                if bShow == true and type(i) == 'string' then
                    DisplayItemStats(i,slot);
                end
            end
        end
    end
end		-- reporting.DisplayGD_AW

--[[
    DisplayGD_S lists all dynamic gear definitions associated with specific slot(s)

    Parameter
        p1      Slot name(s)
--]]

function reporting.DisplayGD_S(p1)

    if p1 == nil then
        return;
    end

    print(chat.message('Gear associated with slot(s): ' .. p1));

    for slot,name in pairs(gear.GearDetails) do
        if string.find(string.lower(p1),string.lower(slot)) ~= nil then
            print(chat.message(' '));
            print(chat.message('Slot: ' .. slot));

            for i,j in pairs(name) do
                if string.find('num,acc,vis',i) == nil then
                    if type(i) == 'string' then
                        DisplayItemStats(i,slot);
                    end
                end
            end
        end
    end
end		-- reporting.DisplayGD_S

--[[
    DisplayGD_Gs lists all dynamic gear definitions associated with a specific gear set

    Parameter
        p1      Gear set name
--]]

function reporting.DisplayGD_Gs(p1)
    local str,tmp;
    local tGs = {};
    local gg = {};
    local lPc = nil;

    if p1 == nil then
        return;
    end

    if string.lower(p1) == 'progressive' then
        print(chat.message('Warning: SMG does not support displaying the Progressive gear set'));
        return;
    end

    -- first check gProfile.Sets. If not found, look in gcinclude.Sets.
    tGs = utilities.fGetTableByName(p1);
    if tGs == nil then
        print(chat.message('Warning, ' .. p1 .. ': no such set exists!'));
        return;
    end

    print(' ');
    print(chat.message('Gear set: ' .. string.upper(p1)));

    -- Loop the entries first looking for subsets
    for slot,j in pairs(tGs) do
        if string.lower(slot) == 'subset' then
            if j ~= nil then
                -- then make sure that j is a table
                gg = {};
                if type(j) == 'string' then
                    gg[1] = j;
                else
                    gg = j
                end
                tmp = nil;

                for _,g in ipairs(gg) do
                    -- It's possible that a subset's entry will itself be a table. Problem is, this can
                    -- be an infinite "rabbit hole". Only one depth will be displayed
                    if type(g) == 'table' then
                        for _,tg in pairs(g) do
                            if tmp == nil then
                                tmp = utilities.fRemoveConditional(tg);
                            else
                                tmp = tmp .. ',' .. utilities.fRemoveConditional(tg);
                            end
                        end
                    else
                        if tmp == nil then
                            tmp = utilities.fRemoveConditional(g);
                        else
                            tmp = tmp .. ',' .. utilities.fRemoveConditional(g);
                        end
                    end
                end
            print(' ');
            print(chat.message('Subset: ' .. tmp));
            end
        end
    end

    -- loop on the entries of the gear set
    lPc = ',';
    for slot,j in pairs(tGs) do
        if string.lower(slot) ~= 'subset' then
            print(' ');
            print(chat.message('Slot: ' .. slot));
            -- make sure entry is not [slot] =
            if j ~= nil then
                gg = {};
                -- then make sure that j is a table
                if type(j) == 'string' then
                    gg[1] = j;
                else
                    gg = j
                end

                -- Now process the normal slot
                local t;
                for _,g in pairs(gg) do
                    if string.find(g,'::') ~= nil then
                        print(chat.message('   ' .. g));
                    else
                        t = string.upper(utilities.fRemoveConditional(g));
                        if string.find(lPc,t) == nil then
                            reporting.DisplayItemStats(t,slot);
                            lPc = lPc .. ',' ..t;
                        end
                    end
                end
            end
        end
    end
end		-- reporting.DisplayGD_Gs
