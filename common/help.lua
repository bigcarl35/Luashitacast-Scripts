local help = {};

--[[
    This component contains routines associated with the help system

    List of routines-
        Subroutines:
            ShowHelp            Displays list of all commands and what they mean
--]]

--[[
    ShowHelp Displays help for all the commands across jobs
--]]

function help.ShowHelp(sWhich)
	local player = gData.GetPlayer();

    if sWhich ~= nil then
        ShowHelpFor(sWhich);
    else

--[[
    '911','acc','ajug','cap','cc','db','dbar','dt','ei','equipit','eva','gc','gearset','gs','gswap',
    'horn','idle','kite','lock','macc','man','maxsong','maxspell','mode','petfood','pull','racc',
    'rc','rv','sbp','showit','smg','spf','ss','string','sw','tank','th','unlock','val','ver',
    'wsdistance','wswap','t1'
--]]

        print(chat.message('The following commands are available to use from within Luashitacast. These are targetting either your specific job or are available across all jobs.\n'));
        print(chat.message('Commands for all jobs: '));
        print(chat.message('/acc [?|stage] -- indicates which accuracy stage should be equipped'));
        print(chat.message('/dt -- Indicates type of damage taken set should be used: Physical, Magical, Breath. Physical is assumed'));
        print(chat.message('/equipit|ei code|name [slot] [1|2] --Equips specified item in the specified slot and locks the affected slot(s)'));
        print(chat.message('/eva -- Toggles whether evasion set should be equipped or not. Default is FALSE'));
        print(chat.message('/gc -- Builds a table of all your gear in your gearsets. Must be run for gearswapping'));
        print(chat.message('/gearset|gs name -- Will equip the named gear set and then lock the affected slots'));
        print(chat.message('          [ALC|BON|CTH|COOK|GSM|LTH|SMT|WW] -- Equips the specified crafting gear'));
        print(chat.message('          [HELM|DIG|CLAM|FISH] -- Equips the specified gathering gear'));
        print(chat.message('/gswap -- Toggles whether automatic gear swaps occur or not. Default is TRUE'));
        print(chat.message('/idle -- Toggles whether \'Travel\' gear is equipped when idle. Default is TRUE'));
        print(chat.message('/kite -- Equips defined movement set.'));
        print(chat.message('/lock [all|#\'s|names] -- Locks specified equipment slots disabling luashitacast from changing gear in those slots'));
        print(chat.message('/man [command] -- Display this listing or specific details on the specified command'));
        print(chat.message('/maxsong name [target] -- Determines the highest level song your current job can cast that contains the passed name'));
        print(chat.message('/maxspell name [target] -- Determines the highest level spell your current job can cast that contains the passed name'));
        print(chat.message('/petfood name --Equips the specified pet food'));
        print(chat.message('/racc [?|stage] -- indicates which ranged accuracy stage should be equipped'));
        print(chat.message('/rc -- Displays who controls what region'));
        print(chat.message('/rv -- Refreshes the global variables, used to fix display bar issues'));
        print(chat.message('/showit -- Displays some global settings. Used mostly for debugging'));
        print(chat.message('/smg [slot=|gs=] -- Displays details on gear matching the query'));
        print(chat.message('/tank -- Toggles whether tanking TP gear set should be equipped. Default is TRUE for PLD,NIN,RUN and FALSE for DRK,WAR,THF,RDM'));
        print(chat.message('/th -- Toggles whether treasure hunter gear should be equipped. Default is FALSE'));

        print(chat.message('/unlock [all|#\'s|names] -- Unlocks specified locked slots'));
        print(chat.message('/ver -- Displays Luashitacast\'s version and any patch notes'));
        print(chat.message('/wsdistance [#] -- Toggles whether a distance check is done for non-ranged weaponskills and how far. Default TRUE at ' .. tostring(gProfile.settings.WSdistance) .. ' yalms'));
        if string.find('SMN,BLM',player.MainJob) == nil then
            print(chat.message('/wswap -- Toggles whether weapons will be swapped as needed. Default depends on job, FALSE to preserve TP'));
        end

        if player.MainJob == 'BST' then
            print(chat.message(' '));
            print(chat.message('Command(s) specific for BST/ or /BST:'));
            print(chat.message('/ajug -- Toggles whether the automated jug pet system is enabled. Default is TRUE. (BST/* only)'));
            print(chat.message('/db [Norm|BPP|WSS] --Indicates body piece wanted for for debuffing your pet'));
        end

        if player.MainJob == 'THF' or player.SubJob == 'THF' then
            print(chat.message('/SS -- Indicates if attempting to steal should be displayed. Default is FALSE'));
        end

        if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
            print(chat.message(' '));
            print(chat.message('Command(s) specific for SMN/ or /SMN:'));
            print(chat.message('/sbp -- Toggles whether offensive blood pacts will show a message in party chat. Default is True'));
        end

        if player.MainJob == 'BRD' then
            print(chat.message(' '));
            print(chat.message('Command(s) specific for BRD/:'));
            print(chat.message('/horn -- Indicates that the instrument should be a wind instrument'));
            print(chat.message('/string -- Indicates that the instrument should be a stringed instrument'));
        end

        print(chat.message(' '));
        print(chat.message('Some /lac commands of note:'));
        print(chat.message('/lac disable -- Disables all gear slots so that no automatic gear changes can occur.'));
        print(chat.message('/lac enable -- Enables all gear slots so automatic gear changes can occur.'));
        print(chat.message('/lac load -- Loads the Luashitacast BST definitions'));
        print(chat.message('/lac unload -- Unloads the Luashitacast BST definitions'));
        print(chat.message('/lac reload -- Unloads and reloads the Luashitacast BST definition'));
        print(chat.message('/lac addset \"name\" -- Saves the current equipped gear into Luashitacast\'s DRK definition file. Don\'t include the \"\'s.'));
        print(chat.message('/lac list -- Lists all the defined gear sets from your BST definition.'));
        print(chat.message(' '));
        print(chat.message('Please note that if you use style lock, you will not see the gear changing, but it is changing'));
    end
end		-- help.ShowHelp

--[[
    IndividualHelp Displays specific help for the specified command
--]]
function IndividualHelp(id)

    if id = gVars._911 then         -- SMN only, /911
        print(chat.message('Form: /911'));
        print(' ');
        -- More goes here
    elseif id == gVars._ACC then    -- /acc
        print(chat.message('Form: /acc [#|max|all|none] [help]'));
        print(' ');
        print(chat.message('/acc is used to defined stages in the "progressive" structure found in your job file.'));
        print(chat.message('Listed in the display bar (if not invisible), each number designates a set of gear'));
        print(chat.message('effectively defining a stage. The number of stages available depends on what you defined.'));
        print(chat.message('Each successive stage contains the lesser numbers, so stage 3 represents stage 1, 2, and 3.'));
        print(chat.message('This lets you decide how much accuracy gear should be equipped. Too much? Pick a lower number.'));
        print(chat.message('Too little? Pick a higher number. Selecting "max" or "all" will enable all your accuracy'));
        print(chat.message('stage definitions. Selecting "none" or leaving out a stage will disable equipping any accuracy'));
        print(chat.message('gear. The visible/invisible option indicates if the accuracy options should be displayed in'));
        print(chat.message('display bar. (Type \'/man visible\' for more details on this option.'));
    elseif id = gVars._AJUG then    -- BST only, /ajug
        print(chat.message('Form: /ajug [help]'));
        print(' ');
        -- More goes here
    elseif id == gVars._CAP then    -- /cap
        print(chat.message('Form: /cap [#] [help]')):
        print(' ');
        print(chat.message('The /cap command is used to artificially limit what gear your character can wear to a specific');
        print(chat.message('level. This is used mostly for testing gear sets. The cap command tells Luashitacast to not');
        print(chat.message('equip gear higher than the sepcified number (between 1 and your character\'s current level.)'));
        print(chat.message('Selecting level 0 or leaving the cap option empty will turn the level cap off. The visible/'));
        print(chat.message('invisible option indicates if the level cap option should be displayed in display bar. (Type'));
        print(chat.message('\'/man visible\' for more details on this option.'));
    elseif id == gVars._CC then     -- /cc
        print(chat.message('Form: /cc[#] [file[=name]][+][help]'));
        print(' ');
        print(chat.message('Custom conditional codes are codes that a player creates in their job file that are not predefined'));
        print(chat.message('in Luashitacast. They ask a simple Yes/No question. Their meaning is important to the player, but'));
        print(chat.message('are unknown to Luashitacast. Each code included is valid, the comparison depends on if that code is'));
        print(chat.message('enabled or not. You toggle the code by typing /cc# where \'#\' is the number of the code. The state'));
        print(chat.message('of the code is displayed in the display bar by the CC: label. Typing just /cc will display all of'));
        print(chat.message('custom codes and their meanings. The \'file\' option lets you write that list to a file. (Type /man'));
        print(chat.message('file for more details on this option.) The visible/invisible option indicates if the custom conditional'));
        print(chat.message('option should be displayed in display bar. (Type \'/man visible\' for more details on this option.'));
    elseif id = gVars._DB then      -- BST only, /db
        print(chat.message('Form: /db [BPP|WSS] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._DBAR then    -- /dbar
        print(chat.message('Form: /dbar [show] [bar=v|i] [vis=[all|none|name,name,...]] [vis=[all|none|name,name,...]] [invis=[all|none|name,name,...]] [save|reset] [file[=name]][+] [help]'));
        print(' ');
        print(chat.message('Part of the display bar manipulation system, dbar gives the player information about the display bar'));
        print(chat.message('and/or lets them save the layout to a startup configuration file. "Pos" displays the XY position of'));
        print(chat.message('where the displaybar is located. "Show" displays all the configurable details of the display bar (the'));
        print(chat.message('file" option lets you save the "show" report to a file). "Save" tells Luashitacast to save the current'));
        print(chat.message('display bar configuration to a start up file so that the next time Luashitacast is loaded, those '));
        print(chat.message('display settings will be used. "Reset" tells Luashitacast to switch the display bar\'s settings back'));
        print(chat.message('to the way it was when it first loaded. The \'file\' option lets you write the "show" list to a file. '));
        print(chat.message('(Type \'/man file\' for more details on this option.) The visible/invisible option indicates if the'));
        print(chat.message('display bar should be displayed or not. (Type \'/man visible\' for more details on this option.'));
    elseif id = gVars._DT then      -- /dt
        print(chat.message('/dt [M|P|B] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._EI then      -- /ei or /equipit
        print(chat.message('Form: /ei code|"name" [help] or /equipit code|"name" [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._EVASION then -- /eva
        print(chat.message('Form: /eva [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._GC then      -- /gc
        print(chat.message('Form: /gc [list] [file[=name]][+] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._GS then      -- /gs or /gearset
        print(chat.message('Form: /gs "name" [w][l] [help] or /gearset "name" [w][l] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._GSWAP then   -- /gswap
        print(chat.message('Form: /gswap [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._HORN then    -- BRD only, /horn
        print(chat.message('Form: /horn [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._IDLE then    -- /idle
        print(chat.message('Form: /idle [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._KITE then    -- /kite
        print(chat.message('Form: /kite [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._LACHELP then -- /lachelp

    elseif id = gVars._LOCKS then   -- /locks
        print(chat.message('Form: /lock [slot name|slot number[,...]] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._MACC then    -- /macc
        print(chat.message('Form: /macc [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._MAXSONG then -- /maxsong
        print(chat.message('Form: /maxsong song [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._MAXSPELL then -- /maxspell
        print(chat.message('Form: /maxspell spell [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._MODE then    -- SMN Only, /mode
        print(chat.message('Form: /mode [PERP|ATTK|ENMM] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._PETFOOD then  -- BST only, /petfood
        print(chat.message('Form: /petfood [name] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._PULL then    -- /pull
        print(chat.message('Form: /pull [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._RACC then    -- /racc
        print(chat.message('Form: /racc [#|max|all|none] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._RC then     -- /rc
        print(chat.message('Form: /rc [file[=name]][+] [help]]'));
        print(' ');
        -- More goes here
    elseif id = gVars._RV then     -- /rv
        print(chat.message('Form: /rv [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._SBP then     -- SMN only, /sbp
        print(chat.message('Form: /sbp [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._SHOWIT then -- /showit
        print(chat.message('Form: /showit [file[=name]][+] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._SMG then     -- /smg
        print(chat.message('Form: /smg [g|s] [noac] [gs=set name,set name,...] [slot=slot name,slot name,...] [file[=name]][+] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._SPF then     -- /spf
        print(chat.message('Form: /spf [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._SS then      -- THF only, /ss
        print(chat.message('Form: /ss [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._STRING then  -- BRD only, /string
        print(chat.message('Form: /string [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._SW then      -- /sw
        print(chat.message('Form: /sw [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._TANK then    -- /tank
        print(chat.message('Form: /tank [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._TH then      -- /th
        print(chat.message('Form: /th [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._UNLOCK then  -- /unlock
        print(chat.message('Form: /unlock [all|list of slot names|list of slot numbers] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._VALIDATE then -- /val
        print(chat.message('Form: /val gs=[all|name,name,...] [file[=name]][+] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._VERSION then  -- /ver
        print(chat.message('Form: /ver [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._WSDISTANCE then -- /WSDISTANCE
        print(chat.message('Form: /wsdistance [#] [help]'));
        print(' ');
        -- More goes here
    elseif id = gVars._WSWAP then   -- /wswap
        print(chat.message('Form: /wswap [help]'));
        print(' ');
        -- More goes here
    end
end     -- IndividualHelp

return help;
