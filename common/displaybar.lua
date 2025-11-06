local displaybar = T{};

local crossjobs = require('common.crossjobs');
local utilities = require('common.utilities');
local gear = require('common.gear');
local fonts = require('fonts');

--[[
    This component contains all routines that deal with the display bar

    List of routines-
        Subroutines:
            RegionDisplay           Updates the display bar with whether region is controlled by nation's kingdom
            SetAccCur               Sets the appropriate tracking current stage
            Unload                  Unloads the font objects and registered events
            UpdateBarStatic         Updates the static portion of the display bar's variables

        Functions:
            local fAccuracyDisplay  Returns the colorized display of the appropriate accuracy
            fColor                  Formats passed string with passed color keyword
            fDisplayIt              Determines if pieces of JobBar should be displayed
--]]

-- List of know color codes by keyword for displaying to the screen
local tkwEle = {	-- cOORRGGBB where OO is opacity, RR red, GG green, BB blue
    { ['kw'] = 'firesday',		 ['color'] = '|cFFFF0000|' }, -- red
    { ['kw'] = 'earthsday',		 ['color'] = '|cFFC19A6B|' }, -- camel
    { ['kw'] = 'watersday',		 ['color'] = '|cFF1F51FF|' }, -- neon blue
    { ['kw'] = 'windsday',		 ['color'] = '|cFF4CBB17|' }, -- kelly green
    { ['kw'] = 'iceday',		 ['color'] = '|cFF00FFFF|' }, -- aqua
    { ['kw'] = 'lightningday',   ['color'] = '|cE1C16EFF|' }, -- "light purple"
    { ['kw'] = 'lightsday', 	 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'darksday', 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'clear', 		 ['color'] = '|cFFA7C7E7|' }, -- pastel blue
    { ['kw'] = 'sunshine', 		 ['color'] = '|cFFFFEA00|' }, -- bright yellow
    { ['kw'] = 'clouds', 		 ['color'] = '|cFFFFFDD0|' }, -- cream
    { ['kw'] = 'fog',	 		 ['color'] = '|cFFB2BEB5|' }, -- ash gray
    { ['kw'] = 'fire', 			 ['color'] = '|cFFFF0000|' }, -- red
    { ['kw'] = 'fire x2', 		 ['color'] = '|cFFFF0000|' }, -- red
    { ['kw'] = 'water', 		 ['color'] = '|cFF1F51FF|' }, -- neon blue
    { ['kw'] = 'water x2', 		 ['color'] = '|cFF1F51FF|' }, -- neon blue
    { ['kw'] = 'earth', 		 ['color'] = '|cFFC19A6B|' }, -- camel
    { ['kw'] = 'earth x2', 		 ['color'] = '|cFFC19A6B|' }, -- camel
    { ['kw'] = 'wind',	 		 ['color'] = '|cFF4CBB17|' }, -- kelly green
    { ['kw'] = 'wind x2', 		 ['color'] = '|cFF4CBB17|' }, -- kelly green
    { ['kw'] = 'ice',	 		 ['color'] = '|cFF00FFFF|' }, -- aqua
    { ['kw'] = 'ice x2', 		 ['color'] = '|cFF00FFFF|' }, -- aqua
    { ['kw'] = 'thunder', 		 ['color'] = '|cE1C16EFF|' }, -- "light purple"
    { ['kw'] = 'thunder x2',	 ['color'] = '|cE1C16EFF|' }, -- "light purple"
    { ['kw'] = 'light', 		 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'light x2', 		 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'dark',	 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'dark x2', 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'full moon',		 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'waning gibbous', ['color'] = '|cFFE5E4E2|' }, -- platinum
    { ['kw'] = 'last quarter', 	 ['color'] = '|cFFC0C0C0|' }, -- silver
    { ['kw'] = 'waning crescent',['color'] = '|cFF848884|' }, -- smoke
    { ['kw'] = 'new moon', 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'waxing crescent',['color'] = '|cFF848884|' }, -- smoke
    { ['kw'] = 'first quarter',  ['color'] = '|cFFC0C0C0|' }, -- silver
    { ['kw'] = 'waxing gibbous', ['color'] = '|cFFE5E4E2|' }, -- platinum
    { ['kw'] = 'green',			 ['color'] = '|cFF00FF00|' }, -- green
    { ['kw'] = 'red',			 ['color'] = '|cFFFF0000|' }, -- red
};

-- The following defines what should be displayed in the display bar
local JobBar = {
     [1] = { ['Code'] = 'WSWAP', ['Jobs'] = '-SMN,BLM', ['MJSJ'] = 'M' },		-- Some jobs swap weapons all the time
     [2] = { ['Code'] = 'Kite', ['Jobs'] = 'ALL', ['MJSJ'] = 'MS' },
     [3] = { ['Code'] = 'Tank', ['Jobs'] = 'PLD,NIN,RUN,WAR,DRK,THF,RDM,BLU', ['MJSJ'] = 'M' },
     [4] = { ['Code'] = 'Idle', ['Jobs'] = 'ALL', ['MJSJ'] = 'MS' },
     [5] = { ['Code'] = 'Eva', ['Jobs'] = 'ALL', ['MJSJ'] = 'MS' },
     [6] = { ['Code'] = 'Macc', ['Jobs'] = 'BLM,WHM,RDM,SMN,PLD,DRK,BLU,SCH,GEO,RUN', ['MJSJ'] = 'MS' },
     [7] = { ['Code'] = 'SPF', ['Jobs'] = 'ALL', ['MJSJ'] = 'MS' },
     -- Single job specific assignments
     [8] = { ['Code'] = 'TH', ['Jobs'] = 'THF', ['MJSJ'] = 'MS' },
     [9] = { ['Code'] = 'SS', ['Jobs'] = 'THF', ['MJSJ'] = 'MS' },
    [10] = { ['Code'] = 'AJug', ['Jobs'] = 'BST', ['MJSJ'] = 'M' },
    [11] = { ['Code'] = 'DB', ['Jobs'] = 'BST', ['MJSJ'] = 'M' },
    [12] = { ['Code'] = 'sBP', ['Jobs'] = 'SMN', ['MJSJ'] = 'MS' },
    [13] = { ['Code'] = 'Mode', ['Jobs'] = 'SMN', ['MJSJ'] = 'MS' },
    [14] = { ['Code'] = 'GSWAP', ['Jobs'] = 'ALL', ['MJSJ'] = 'MS'},
    -- Separate area displayed
    [15] = { ['Code'] = 'Instrument', ['Jobs'] = 'BRD', ['MJSJ'] = 'M' },
    [16] = { ['Code'] = 'DT', ['Jobs'] = 'ALL', ['MJSJ'] = 'MS' },
    [17] = { ['Code'] = 'Region', ['Jobs'] = 'ALL', ['MJSJ'] = 'MS' }
};

-- Local variables used for display purposes
local MainLV = 0;
local SubLV = 0;
local Zone = ' ';
local Main = 'FOO';
local Sub = 'BAR';

-- Generic font settings for display
local fontSettings = {
    visible = true,
    font_family = 'Arial',
    font_height = 14,
    color = 0xFFFFFFFF,			-- White
    position_x = 325,
    position_y = 0,
    background = T{
        visible = true,
        color = 0xFF000000,		-- Black
    }
};

--[[
    UpdateBarStatic updates the current identifying aspects of the display bar data (specifically the
    player specific information)
--]]

function displaybar.UpdateBarStatic()
    local player = AshitaCore:GetMemoryManager():GetPlayer();
    local pEntity = AshitaCore:GetMemoryManager():GetEntity();
    local myIndex = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
    local currentZoneID = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);
    local currentZoneName = AshitaCore:GetResourceManager():GetString('zones.names', currentZoneID);
    local MID = player:GetMainJob();
    local SID = player:GetSubJob();

    MainLV	= player:GetMainJobLevel();
    SubLV	= player:GetSubJobLevel();
    Main	= AshitaCore:GetResourceManager():GetString("jobs.names_abbr", MID);
    Sub		= AshitaCore:GetResourceManager():GetString("jobs.names_abbr", SID);
    Zone    = currentZoneName;
end		-- displaybar.UpdateBarStatic

--[[
    RegionDisplay determines if the player's nation owns the area the character is in
    or not and updates the display bar accordingly.
--]]

function displaybar.RegionDisplay()
    local zoneId = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);

    -- Make sure the player's nation is known
    if crossjobs.OwnNation == -1 then
        crossjobs.OwnNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation() + 1;
    end

    -- Determine if current zone in region controlled by player's nation
    for i,j in pairs(crossjobs.RegionControl) do
        if table.find(j['zones'],zoneId) ~= nil then
            if j['own'] == crossjobs.OwnNation then
                utilities.fSetCycle('Region','Owned');
            elseif j['own'] == 0 and utilities.fBuffed('Signet') == false then
                utilities.fSetCycle('Region','N/A');
            else
                utilities.fSetCycle('Region','Not Owned');
            end
            break;
        end
    end
end		-- displaybar.RegionDisplay

--[[
    Unload removes the objects and commands created by the display bar code
--]]

function displaybar.Unload()
    if (displaybar.FontObject ~= nil) then
        displaybar.FontObject:destroy();
    end

    ashita.events.unregister('d3d_present', 'displaybar_present_cb');
    ashita.events.unregister('command', 'displaybar_cb');
end		-- displaybar.Unload

--[[
    fWhichJobBar determines which record matches the passed in toggle

    Parameter
        s       Toggle name

    Returned
        The matching row pointer or nil
--]]

function fWhichJobBar(s)
    local bFound = false;

    for i,j in ipairs(JobBar) do
        if string.upper(j['Code']) == string.upper(s) then
            bFound = true;
            return j;
            break;
        end
    end

    if bFound == false then
        return nil;
    end
end     -- fWhichJobBar

--[[
    fDisplayIt is a function that determines if the passed string should be displayed in the luashita
    display bar.
--]]

function displaybar.fDisplayIt(s)
    local ptr = fWhichJobBar(s);local gcdisplay = require('common.gcdisplay');

    if s == nil or ptr == nil or ptr['Jobs'] == 'ALL' then	-- Missing from table or applies to all jobs, assume it should be displayed
        return true;
    else
        -- Something specific about the entry. Parse it out
        if string.sub(ptr['Jobs'],1,1) == '-' then			-- Indicates ALL but the jobs mentioned
            if string.find(ptr['MJSJ'],'M') ~= nil and string.find(ptr['Jobs'],Main) ~= nil then
                return false;
            end
            if string.find(ptr['MJSJ'],'S') ~= nil and string.find(ptr['Jobs']],Sub) ~= nil then
                return false;
            end
            return true;
        else	-- Only valid for the explicit jobs mentioned
            if string.find(ptr['MJSJ'],'M') ~= nil and string.find(ptr['Jobs',Main) ~= nil then
                return true;
            end
            if string.find(ptr['MJSJ'],'S') ~= nil and string.find(ptr['Jobs'],Sub) ~= nil then
                return true;
            end
        end
    end
    return false;
end		-- displaybar.fDisplayIt

--[[
    fColor will return the colorized string according to the keyword's color. All
    colors are denoted in hex

    Parameters
        skw         color keyword
        sMsg        message to be colorized

    Returned
        Formatted output
--]]

function displaybar.fColor(skw,sMsg)
    local sEnd = '|r';
    local sfColor;
    local sColor = nil;

    if skw == nil then
        return ' ';
    end

    skw = string.lower(skw);
    for i,j in pairs(tkwEle) do
        if j['kw'] == skw then
            sColor = j['color'];
        break;
        end
    end

    if sColor == nil then
        sfColor = sMsg;
    else
        sfColor = sColor .. sMsg .. sEnd;
    end
    return sfColor;
end		-- displaybar.fColor

--[[
    fAccuracyDisplay generates the accuracy listing highlighting what is currently enabled
    based on the passed sType. Returned is the generated colorized string.
--]]

function fAccuracyDisplay(sType)
    local msg;
    local which = {};

    if sType == nil then
        return "";
    end

    for i,j in pairs(gear.Progressive) do
        if string.lower(sType) == string.lower(j['Abbr']) then
            which = j;
        break;
        end
    end

    if which == nil then
        return "";
    else
        if which['MaxStage'] == 0 then
            msg = ' ';
        else
            for i=1,which['MaxStage'],1 do
                if i <= which['CurStage'] then
                    if msg == nil then
                        msg = fColor('green',tostring(i))
                    else
                        msg = msg .. fColor('green',',' .. tostring(i))
                    end
                else
                    if msg == nil then
                        msg = fColor('red',tostring(i))
                    else
                        msg = msg .. fColor('red',',' .. tostring(i))
                    end
                end
            end
        end
    end
    return msg;
end		-- fAccuracyDisplay

--[[
    InitializeDisplayBar creates the display bar and assigns it to an event. It is
    only run once.
--]]

function displaybar.InitializeDisplayBar()
    local pEntity = AshitaCore:GetMemoryManager():GetEntity();
    local myIndex = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);

    displaybar.UpdateBarStatic();
    displaybar.FontObject = fonts.new(fontSettings);

    ashita.events.register('d3d_present', 'displaybar_present_cb', function ()
        local display = MainLV;

        if gProfile.settings.PlayerCappedLevel > 0 then
            local stmp = string.format('[%d]',gProfile.settings.PlayerCappedLevel);
            display = display .. displaybar.fColor('red',stmp);
        end
        display = display .. Main .. '/' .. SubLV .. Sub .. ' |';

        if gear.bGC == true then
            display = display .. ' ' .. fColor('green','GC') .. ' ';
        else
            display = display .. ' ' .. fColor('red','GC') .. ' ';
        end
        display = display .. '|';

        for k, v in pairs(utilities.Toggles) do
            if displaybar.fDisplayIt(k) == true then
                display = display .. ' ';
                if (v == true) then
                    display = display .. fColor('green',k) .. ' ';
                else
                    display = display .. fColor('red',k) .. ' ';
                end
            end
        end
        display = display .. '|';
        for key, value in pairs(utilities.Cycles) do
            if displaybar.fDisplayIt(key) == true then
                display = display .. '  ' .. key .. ': ' .. fColor('green',value.Array[value.Index]);
            end
        end

        -- Accuracy
        if utilities.GetToggle('Tank') == true then
            display = display .. ' | Acc: ' .. fAccuracyDisplay('TAcc');
            display = display .. ' | Racc: ' .. fAccuracyDisplay('TRAcc');
        else
            display = display .. ' | Acc: ' .. fAccuracyDisplay('Acc');
            display = display .. ' | Racc: ' .. fAccuracyDisplay('RAcc');
        end

        -- Locks
        if locks.LocksNumeric ~= 'None' then
            display = display .. ' | Locks: ' .. fColor('green',locks.LocksNumerics);
        else
            display = display .. ' | Locks: ' .. fColor('red',locks.LocksNumeric);
        end

        local env = gData.GetEnvironment();
        display = display .. string.format(' | %s | %02d:%02d | %d%% %s | %s ',
            fColor(env.Day,env.Day),env.Timestamp.hour,env.Timestamp.minute,env.MoonPercent,fColor(env.MoonPhase,env.MoonPhase),fColor(env.RawWeather,env.RawWeather));
        display = display .. ' | ' .. Zone;
        displaybar.FontObject.text = display;
    end);
end		-- displaybar.Initialize

--[[
    SetAccCur sets the current stage level for the specified type of
    accuracy. The passed in value is checked versus the maximum to
    make sure the stage is valid.

    Parameters
        sType       Type of accuracy
        val         Value to set accuracy settiing to
--]]

function displaybar.SetAccCur(sType,val)
    if sType == nil then
        sType = 'Acc';
    end

    if val == nil then
        val = 0;
    elseif type(val) == 'string' then
        val = tonumber(val);
    end

    if sType == 'Acc' then
        if val < 0 or val > gear.Progressive['Accuracy']['MaxStage'] then
            val = 0;
        end
        gear.Progressive['Accuracy']['CurStage'] = val;
    elseif sType == 'TAcc' then
        if val < 0 or val > gear.Progressive['Tank_Accuracy']['MaxStage'] then
            val = 0;
        end
        gear.Progressive['Tank_Accuracy']['CurStage'] = val;
    elseif sType == 'RAcc' then
        if val < 0 or val > gear.Progressive['Ranged_Accuracy']['MaxStage'] then
            val = 0;
        end
        gear.Progressive['Ranged_Accuracy']['CurStage'] = val;
    elseif sType == 'TRAcc' then
        if val < 0 or val > gear.Progressive['Tank_Ranged_Accuracy']['MaxStage'] then
            val = 0;
        end
        gear.Progressive['Tank_Ranged_Accuracy']['CurStage'] = val;
    end
end		-- displaybar.SetAccCur

--[[
    displaybar_cb registers the command so that the display bar can be turned on or off
--]]

ashita.events.register('command', 'displaybar_cb', function (e)
    local args = e.command:args()

    if #args == 0 or args[1] ~= '/displaybar' then
        return
    end

    e.blocked = true

    if #args == 1 then
        displaybar.FontObject.visible = not displaybar.FontObject.visible;
    end
end);
