local help = T{};

local crossjobs = require('common.crossjobs');

--[[
    This component contains routines associated with the help system

    List of routines-
        Subroutines:
            ShowHelp            Displays list of all commands and what they mean
--]]

--[[
    ShowHelp Displays help for all the commands across jobs
--]]

function help.ShowHelp()
	local player = gData.GetPlayer();

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
    print(chat.message('/help [command] -- Display this listing or specific details on the specified command'));
    print(chat.message('/idle -- Toggles whether \'Travel\' gear is equipped when idle. Default is TRUE'));
    print(chat.message('/kite -- Equips defined movement set.'));
    print(chat.message('/lock [all|#\'s|names] -- Locks specified equipment slots disabling luashitacast from changing gear in those slots'));
    print(chat.message('/maxsong name [target] -- Determines the highest level song your current job can cast that contains the passed name'));
    print(chat.message('/maxspell name [target] -- Determines the highest level spell your current job can cast that contains the passed name'));
    print(chat.message('/petfood name --Equips the specified pet food'));
    print(chat.message('/racc [?|stage] -- indicates which ranged accuracy stage should be equipped'));
    print(chat.message('/rc -- Displays who controls what region'));
    print(chat.message('/rv -- Refreshes the global variables, used to fix display bar issues'));
    print(chat.message('/showit -- Displays some global settings. Used mostly for debugging'));
    print(chat.message('/smg [slot=|gs=] -- Displays details on gear matching the query'));

    if string.find('PLD,NIN,DRK,WAR,THF,RDM,RUN',player.MainJob) ~= nil then
        print(chat.message('/tank -- Toggles whether tanking TP gear set should be equipped. Default is TRUE for PLD,NIN,RUN and FALSE for DRK,WAR,THF,RDM'));
    end

    print(chat.message('/unlock [all|#\'s|names] -- Unlocks specified locked slots'));
    print(chat.message('/ver -- Displays Luashitacast\'s version and any patch notes'));
    print(chat.message('/wsdistance [#] -- Toggles whether a distance check is done for non-ranged weaponskills and how far. Default TRUE at ' .. tostring(crossjobs.settings.WSdistance) .. ' yalms'));
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
        print(chat.message('/th -- Toggles whether treasure hunter gear should be equipped. Default is FALSE'));
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
end		-- help.ShowHelp

