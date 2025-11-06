local magic = T{};

local crossjobs = require('common.crossjobs');
local utilities = require('common.utilities');
local gear = require('common.gear');

--[[
    This component contains all routines that deal with magic

    List of routines-
        Subroutines:
            HandleMidcast                   Coordinating routine that invokes appropriate midcast based on type
            HandlePrecast                   Equips the appropriate precast gear
            MaxCast                         Casts the highest version of the passed spell/song
            Local MidcastDarkMagic          Handles all gear appropriate for dark magic
            local MidcastDivineMagic        Handles all gear appropriate for divine magic
            local MidcastElementalMagic     Handles all gear appropriate for elemental magic
            local MidcastEnfeeblingMagic    Handles all gear appropriate for enfeebling magic
            local MidcastEnhancingMagic     Handles all gear appropriate for enhancing magic
            local MidcastHealingMagic       Handles all gear appropriate for healing magic
            local MidcastNinjutsu           Handles all gear appropriate for ninjutsu
            local MidcastSinging            Handles all gear appropriate for singing/instrument playing
            local MidcastSummoning          Handles all gear appropriate for summoning magic

        Functions:
            fBardSongType                   Determines if the bard song being sung is of type passed

--]]

--[[
    This table contains a list of all of the spells and songs that have multiple tiers, listed in descending order.
    For the spells, included is what job can cast the spell at what level, the MP cost, and Spell ID. For the
    songs, listed is the song id and the level it can be cast at.

    Please note that entries that will be included when Treasures of Aht Urgan is released are currently commented out.
--]]

magic.Tiered = {
    ['spells'] = {
        ['aero'] = {
            [1] = { ['Name'] = 'Aero IV', ['SID'] = 157, ['MP'] = 115, ['BLM'] = 72, ['SCH'] = 72 },
            [2] = { ['Name'] = 'Aero III', ['SID'] = 156, ['MP'] = 54, ['RDM'] = 69, ['BLM'] = 59, ['SCH'] = 60, ['GEO'] = 64 },
            [3] = { ['Name'] = 'Aero II', ['SID'] = 155, ['MP'] = 22, ['RDM'] = 45, ['DRK'] = 54, ['BLM'] = 34, ['SCH'] = 38, ['GEO'] = 42 },
            [4] = { ['Name'] = 'Aero', ['SID'] = 154, ['MP'] = 6, ['RDM'] = 14, ['DRK'] = 17, ['BLM'] = 9, ['SCH'] = 12, ['GEO'] = 14 }
        },
        ['aeroga'] = {
            [1] = { ['Name'] = 'Aeroga III', ['SID'] = 186, ['MP'] = 232, ['BLM'] = 67 },
            [2] = { ['Name'] = 'Aeroga II', ['SID'] = 185, ['MP'] = 131, ['BLM'] = 48 },
            [3] = { ['Name'] = 'Aeroga', ['SID'] = 184, ['MP'] = 45, ['BLM'] = 23 }
        },
        ['banish'] = {
            [1] = { ['Name'] = 'Banish III', ['SID'] = 30, ['MP'] = 96, ['WHM'] = 65 },
            [2] = { ['Name'] = 'Banish II', ['SID'] = 29, ['MP'] = 57, ['WHM'] = 30, ['PLD'] = 34 },
            [3] = { ['Name'] = 'Banish', ['SID'] = 28, ['MP'] = 15, ['WHM'] = 5, ['PLD'] = 7 }
        },
        ['banishga'] = {
            [1] = { ['Name'] = 'Banishga II', ['SID'] = 39, ['MP'] = 120, ['WHM'] = 40 },
            [2] = { ['Name'] = 'Banishga', ['SID'] = 38, ['MP'] = 41, ['WHM'] = 15, ['PLD'] = 30 }
        },
        ['blizzaga'] = {
            [1] = { ['Name'] = 'Blizzaga III', ['SID'] = 181, ['MP'] = 297, ['BLM'] = 71 },
            [2] = { ['Name'] = 'Blizzaga II', ['SID'] = 180, ['MP'] = 175, ['BLM'] = 57 },
            [3] = { ['Name'] = 'Blizzaga', ['SID'] = 179, ['MP'] = 80, ['BLM'] = 32 }
        },
        ['blizzard'] = {
            [1] = { ['Name'] = 'Blizzard IV', ['SID'] = 152, ['MP'] = 162, ['BLM'] = 74, ['SCH'] = 74 },
            [2] = { ['Name'] = 'Blizzard III', ['SID'] = 151, ['MP'] = 75, ['RDM'] = 73, ['BLM'] = 64, ['SCH'] = 66, ['GEO'] = 70 },
            [3] = { ['Name'] = 'Blizzard II', ['SID'] = 150, ['MP'] = 31, ['RDM'] = 55, ['DRK'] = 66, ['BLM'] = 42, ['SCH'] = 46, ['GEO'] = 50 },
            [4] = { ['Name'] = 'Blizzard', ['SID'] = 149, ['MP'] = 8, ['RDM'] = 24, ['DRK'] = 29, ['BLM'] = 17, ['SCH'] = 20, ['GEO'] = 24 }
        },
        ['cure'] = {
            [1] = { ['Name'] = 'Cure V', ['SID'] = 5, ['MP'] = 135, ['WHM'] = 61 },
            [2] = { ['Name'] = 'Cure IV', ['SID'] = 4, ['MP'] = 88, ['WHM'] = 41, ['RDM'] = 48, ['PLD'] = 55, ['SCH'] = 55 },
            [3] = { ['Name'] = 'Cure III', ['SID'] = 3, ['MP'] = 46, ['WHM'] = 21, ['RDM'] = 26, ['PLD'] = 30, ['SCH'] = 30 },
            [4] = { ['Name'] = 'Cure II', ['SID'] = 2, ['MP'] = 24, ['WHM'] = 11, ['RDM'] = 14, ['PLD'] = 17, ['SCH'] = 17 },
            [5] = { ['Name'] = 'Cure', ['SID'] = 1, ['MP'] = 8, ['WHM'] = 1, ['RDM'] = 3, ['PLD'] = 5, ['SCH'] = 5 }
        },
        ['curaga'] = {
            [1] = { ['Name'] = 'Curaga IV', ['SID'] = 10, ['MP'] = 260, ['WHM'] = 71 },
            [2] = { ['Name'] = 'Curaga III', ['SID'] = 9, ['MP'] = 180, ['WHM'] = 51 },
            [3] = { ['Name'] = 'Curaga II', ['SID'] = 8, ['MP'] = 120, ['WHM'] = 31 },
            [4] = { ['Name'] = 'Curaga', ['SID'] = 7, ['MP'] = 60, ['WHM'] = 16 }
        },
        ['dia'] = {
            [1] = { ['Name'] = 'Dia III', ['SID'] = 25, ['MP'] = 45, ['RDM'] = 75 },
            [2] = { ['Name'] = 'Dia II', ['SID'] = 24, ['MP'] = 30, ['WHM'] = 36, ['RDM'] = 31 },
            [3] = { ['Name'] = 'Dia', ['SID'] = 23, ['MP'] = 7, ['WHM'] = 3, ['RDM'] = 1 }
        },
        ['fire'] = {
            [1] = { ['Name'] = 'Fire IV', ['SID'] = 147, ['MP'] = 135, ['BLM'] = 73, ['SCH'] = 73 },
            [2] = { ['Name'] = 'Fire III', ['SID'] = 146, ['MP'] = 63, ['RDM'] = 71, ['BLM'] = 62, ['SCH'] = 63, ['GEO'] = 67 },
            [3] = { ['Name'] = 'Fire II', ['SID'] = 145, ['MP'] = 26, ['RDM'] = 50, ['DRK'] = 50, ['BLM'] = 38, ['SCH'] = 42, ['GEO'] = 46 },
            [4] = { ['Name'] = 'Fire', ['SID'] = 144, ['MP'] = 7, ['RDM'] = 19, ['DRK'] = 23, ['BLM'] = 13, ['SCH'] = 42, ['GEO'] = 19 }
        },
        ['firaga'] = {
            [1] = { ['Name'] = 'Firaga III', ['SID'] = 176, ['MP'] = 263, ['BLM'] = 69 },
            [2] = { ['Name'] = 'Firaga II', ['SID'] = 175, ['MP'] = 153, ['BLM'] = 53 },
            [3] = { ['Name'] = 'Firaga', ['SID'] = 174, ['MP'] = 57, ['BLM'] = 28 }
        },
        ['paralyze'] = {
            [1] = { ['Name'] = 'Paralyze II', ['SID'] = 80, ['MP'] = 36, ['RDM'] = 75 },
            [2] = { ['Name'] = 'Paralyze', ['SID'] = 58, ['MP'] = 6, ['WHM'] = 4, ['RDM'] = 6 }
        },
        ['phalanx'] = {
            [1] = { ['Name'] = 'Phalanx', ['SID'] = 106, ['MP'] = 21, ['RDM'] = 33, ['RUN'] = 68 }
            --[2] = { ['Name'] = 'Phalanx II', ['SID'] = 107, ['MP'] = 42, ['RDM'] = 75 },      -- Make #1 when uncommented
        },
        ['protect'] = {
            [1] = { ['Name'] = 'Protect IV', ['SID'] = 46, ['MP'] = 65, ['WHM'] = 63, ['RDM'] = 63, ['PLD'] = 70, ['SCH'] = 66 },
            [2] = { ['Name'] = 'Protect III', ['SID'] = 45, ['MP'] = 46, ['WHM'] = 47, ['RDM'] = 47, ['PLD'] = 50, ['SCH'] = 50, ['RUN'] = 60 },
            [3] = { ['Name'] = 'Protect II', ['SID'] = 44, ['MP'] = 28, ['WHM'] = 27, ['RDM'] = 27, ['PLD'] = 30, ['SCH'] = 30, ['RUN'] = 40 },
            [4] = { ['Name'] = 'Protect', ['SID'] = 43, ['MP'] = 9, ['WHM'] = 7, ['RDM'] = 7, ['PLD'] = 10, ['SCH'] = 10, ['RUN'] = 20 }
        },
        ['protectra'] = {
            [1] = { ['Name'] = 'Protectra V', ['SID'] = 129, ['MP'] = 84, ['WHM'] = 75 },
            [2] = { ['Name'] = 'Protectra IV', ['SID'] = 128, ['MP'] = 65, ['WHM'] = 63 },
            [3] = { ['Name'] = 'Protectra III', ['SID'] = 127, ['MP'] = 46, ['WHM'] = 47 },
            [4] = { ['Name'] = 'Protectra II', ['SID'] = 126, ['MP'] = 28, ['WHM'] = 27 },
            [5] = { ['Name'] = 'Protectra', ['SID'] = 125, ['MP'] = 9, ['WHM'] = 7 }
        },
        ['raise'] = {
            [1] = { ['Name'] = 'Raise III', ['SID'] = 140, ['MP'] = 150, ['WHM'] = 70 },
            [2] = { ['Name'] = 'Raise II', ['SID'] = 13, ['MP'] = 150, ['WHM'] = 56, ['SCH'] = 70 },
            [3] = { ['Name'] = 'Raise', ['SID'] = 12, ['MP'] = 150, ['WHM'] = 25, ['RDM'] = 35, ['PLD'] = 50, ['SCH'] = 35 }
        },
        ['regen'] = {
            [1] = { ['Name'] = 'Regen III', ['SID'] = 111, ['MP'] = 64, ['WHM'] = 66, ['SCH'] = 59, ['RUN'] = 70 },
            [2] = { ['Name'] = 'Regen II', ['SID'] = 110, ['MP'] = 36, ['WHM'] = 44, ['SCH'] = 37, ['RUN'] = 48 },
            [3] = { ['Name'] = 'Regen', ['SID'] = 108, ['MP'] = 15, ['WHM'] = 21, ['RDM'] = 21, ['SCH'] = 18, ['RUN'] = 23 }
        },
        ['reraise'] = {
            [1] = { ['Name'] = 'Reraise III', ['SID'] = 142, ['MP'] = 150, ['WHM'] = 70 },
            [2] = { ['Name'] = 'Reraise II', ['SID'] = 141, ['MP'] = 150, ['WHM'] = 56, ['SCH'] = 70 },
            [3] = { ['Name'] = 'Reraise', ['SID'] = 135, ['MP'] = 150, ['WHM'] = 25, ['PLD'] = 35 }
        },
        ['shell'] = {
            [1] = { ['Name'] = 'Shell IV', ['SID'] = 51, ['MP'] = 75, ['WHM'] = 68, ['RDM'] = 68, ['SCH'] = 71, ['RUN'] = 70 },
            [2] = { ['Name'] = 'Shell III', ['SID'] = 50, ['MP'] = 56, ['WHM'] = 57, ['RDM'] = 57, ['PLD'] = 60, ['SCH'] = 60, ['RUN'] = 50 },
            [3] = { ['Name'] = 'Shell II', ['SID'] = 49, ['MP'] = 37, ['WHM'] = 37, ['RDM'] = 37, ['PLD'] = 40, ['SCH'] = 40, ['RUN'] = 30 },
            [4] = { ['Name'] = 'Shell', ['SID'] = 48, ['MP'] = 18, ['WHM'] = 17, ['RDM'] = 17, ['PLD'] = 20, ['SCH'] = 20, ['RUN'] = 10 }
        },
        ['shellra'] = {
            [1] = { ['Name'] = 'Shellra V', ['SID'] = 134, ['MP'] = 93, ['WHM'] = 75 },
            [2] = { ['Name'] = 'Shellra IV', ['SID'] = 133, ['MP'] = 75, ['WHM'] = 68 },
            [3] = { ['Name'] = 'Shellra III', ['SID'] = 132, ['MP'] = 56, ['WHM'] = 57 },
            [4] = { ['Name'] = 'Shellra II', ['SID'] = 131, ['MP'] = 37, ['WHM'] = 37 },
            [5] = { ['Name'] = 'Shellra', ['SID'] = 130, ['MP'] = 18, ['WHM'] = 17 }
        },
        ['slow'] = {
            [1] = { ['Name'] = 'Slow II', ['SID'] = 79, ['MP'] = 45, ['RDM'] = 75 },
            [2] = { ['Name'] = 'Slow', ['SID'] = 59, ['MP'] = 12, ['WHM'] = 13, ['RDM'] = 13 }
        },
        ['stonega'] = {
            [1] = { ['Name'] = 'Stonega III', ['SID'] = 191, ['MP'] = 175, ['BLM'] = 63 },
            [2] = { ['Name'] = 'Stonega II', ['SID'] = 190, ['MP'] = 93, ['BLM'] = 40 },
            [3] = { ['Name'] = 'Stonega', ['SID'] = 189, ['MP'] = 24, ['BLM'] = 15 }
        },
        ['stone'] = {
            [1] = { ['Name'] = 'Stone IV', ['SID'] = 162, ['MP'] = 88, ['BLM'] = 68, ['SCH'] = 70 },
            [2] = { ['Name'] = 'Stone III', ['SID'] = 161, ['MP'] = 40, ['RDM'] = 65, ['BLM'] = 51, ['SCH'] = 54, ['GEO'] = 58 },
            [3] = { ['Name'] = 'Stone II', ['SID'] = 160, ['MP'] = 16, ['RDM'] = 35, ['DRK'] = 42, ['BLM'] = 26, ['SCH'] = 30, ['GEO'] = 34 },
            [4] = { ['Name'] = 'Stone', ['SID'] = 159, ['MP'] = 4, ['RDM'] = 4, ['DRK'] = 5, ['BLM'] = 1, ['SCH'] = 4, ['GEO'] = 4 }
        },
        ['thundaga'] = {
            [1] = { ['Name'] = 'Thundaga III', ['SID'] = 196, ['MP'] = 332, ['BLM'] = 73 },
            [2] = { ['Name'] = 'Thundaga II', ['SID'] = 195, ['MP'] = 200, ['BLM'] = 61 },
            [3] = { ['Name'] = 'Thundaga', ['SID'] = 194, ['MP'] = 105, ['BLM'] = 36 }
        },
        ['thunder'] = {
            [1] = { ['Name'] = 'Thunder IV', ['SID'] = 167, ['MP'] = 194, ['BLM'] = 75, ['SCH'] = 75 },
            [2] = { ['Name'] = 'Thunder III', ['SID'] = 166, ['MP'] = 91, ['RDM'] = 75, ['BLM'] = 66, ['SCH'] = 69, ['GEO'] = 73 },
            [3] = { ['Name'] = 'Thunder II', ['SID'] = 165, ['MP'] = 37, ['RDM'] = 60, ['DRK'] = 72, ['BLM'] = 46, ['SCH'] = 51, ['GEO'] = 54 },
            [4] = { ['Name'] = 'Thunder', ['SID'] = 164, ['MP'] = 9, ['RDM'] = 29, ['DRK'] = 35, ['BLM'] = 21, ['SCH'] = 24, ['GEO'] = 23 }
        },
        ['water'] = {
            [1] = { ['Name'] = 'Water IV', ['SID'] = 172, ['MP'] = 99, ['BLM'] = 70, ['SCH'] = 71 },
            [2] = { ['Name'] = 'Water III', ['SID'] = 171, ['MP'] = 46, ['RDM'] = 67, ['BLM'] = 55, ['SCH'] = 57, ['GEO'] = 61 },
            [3] = { ['Name'] = 'Water II', ['SID'] = 170, ['MP'] = 19, ['RDM'] = 40, ['DRK'] = 48, ['BLM'] = 30, ['SCH'] = 34, ['GEO'] = 38 },
            [4] = { ['Name'] = 'Water', ['SID'] = 169, ['MP'] = 5, ['RDM'] = 9, ['DRK'] = 11, ['BLM'] = 5, ['SCH'] = 8, ['GEO'] = 9 }
        },
        ['watera'] = {
            [1] = { ['Name'] = 'Watera III', ['SID'] = 201, ['MP'] = 202, ['BLM'] = 65 },
            [2] = { ['Name'] = 'Watera II', ['SID'] = 200, ['MP'] = 112, ['BLM'] = 44 },
            [3] = { ['Name'] = 'Watera', ['SID'] = 199, ['MP'] = 34, ['BLM'] = 19 }
        },
        ['flare'] = {
            [1] = { ['Name'] = 'Flare', ['SID'] = 204, ['MP'] = 315, ['BLM'] = 60 },
            -- [2] = { ['Name'] = 'Flare II', ['SID'] = 205, ['MP'] = 280, ['BLM'] = 75 }     -- Make #1 when uncommented
        },
        ['freeze'] = {
            [1] = { ['Name'] = 'Freeze', ['SID'] = 206, ['MP'] = 315, ['BLM'] = 50 },
            -- [2] = { ['Name'] = 'Freeze II', ['SID'] = 207, ['MP'] = 280, ['BLM'] = 75 }     -- Make #1 when uncommented
        },
        ['tornado'] = {
            [1] = { ['Name'] = 'Tornado', ['SID'] = 208, ['MP'] = 315, ['BLM'] = 52 },
            -- [2] = { ['Name'] = 'Tornado II', ['SID'] = 209, ['MP'] = 280, ['BLM'] = 75 }     -- Make #1 when uncommented
        },
        ['quake'] = {
            [1] = { ['Name'] = 'Quake', ['SID'] = 210, ['MP'] = 315, ['BLM'] = 54 },
            -- [2] = { ['Name'] = 'Quake II', ['SID'] = 211, ['MP'] = 280, ['BLM'] = 75 }     -- Make #1 when uncommented
        },
        ['burst'] = {
            [1] = { ['Name'] = 'Burst', ['SID'] = 212, ['MP'] = 315, ['BLM'] = 56 },
            -- [2] = { ['Name'] = 'Burst II', ['SID'] = 213, ['MP'] = 280, ['BLM'] = 75 }     -- Make #1 when uncommented
        },
        ['flood'] = {
            [1] = { ['Name'] = 'Flood', ['SID'] = 214, ['MP'] = 315, ['BLM'] = 58 },
            -- [2] = { ['Name'] = 'Flood II', ['SID'] = 215, ['MP'] = 280, ['BLM'] = 75 }     -- Make #1 when uncommented
        },
        ['poison'] = {
            [1] = { ['Name'] = 'Poison II', ['SID'] = 221, ['MP'] = 38, ['RDM'] = 46, ['BLM'] = 43, ['DRK'] = 46 },
            [2] = { ['Name'] = 'Poison', ['SID'] = 220, ['MP'] = 5, ['RDM'] = 5, ['BLM'] = 3, ['DRK'] = 6 }
        },
        ['poisonga'] = {
            [1] = { ['Name'] = 'Poison II', ['SID'] = 226, ['MP'] = 112, ['BLM'] = 64, ['DRK'] = 66 },
            [2] = { ['Name'] = 'Poisonga', ['SID'] = 225, ['MP'] = 5, ['BLM'] = 24, ['DRK'] = 26 }
        },
        ['bio'] = {
            [1] = { ['Name'] = 'Bio II', ['SID'] = 231, ['MP'] = 36, ['RDM'] = 36, ['BLM'] = 35, ['DRK'] = 40 },
            [2] = { ['Name'] = 'Bio', ['SID'] = 230, ['MP'] = 15, ['RDM'] = 10, ['BLM'] = 10, ['DRK'] = 15 }
            -- [3] { ['Name'] = 'Bio III', ['SID'] = 232, ['MP'] = 36, ['RDM'] songs= 75 }     -- Make #1 when uncommented
        },
        ['drain'] = {
            [1] = { ['Name'] = 'Drain', ['SID'] = 245, ['MP'] = 21, ['SCH'] = 21, ['BLM'] = 12, ['DRK'] = 10 },
            -- [2] = { ['Name'] = 'Drain II', ['SID'] = 246, ['MP'] = 37, ['DRK'] = 62 }     -- Make #1 when uncommented
        },
        ['sleep'] = {
            [1] = { ['Name'] = 'Sleep II', ['SID'] = 259, ['MP'] = 29, ['RDM'] = 46, ['SCH'] = 65, ['BLM'] = 41, ['DRK'] = 56, ['GEO'] = 70 },
            [2] = { ['Name'] = 'Sleep', ['SID'] = 253, ['MP'] = 19, ['RDM'] = 25, ['SCH'] = 30, ['BLM'] = 20, ['DRK'] = 30, ['GEO'] = 35 }
        },
        ['sleepga'] = {
            [1] = { ['Name'] = 'Sleepga II', ['SID'] = 274, ['MP'] = 58, ['BLM'] = 56 },
            [2] = { ['Name'] = 'Sleepga', ['SID'] = 273, ['MP'] = 38, ['BLM'] = 31 }
        },
        ['blind'] = {
            [1] = { ['Name'] = 'Blind', ['SID'] = 254, ['MP'] = 5, ['RDM'] = 8, ['BLM'] = 4 },
            -- [2] = { ['Name'] = 'Blind II', ['SID'] = 276, ['MP'] = 31, ['BIn a silly moodLM'] = 75 }     -- Make #1 when uncommented
        },
        ['enfire'] = {
            [1] = { ['Name'] = 'Enfire', ['SID'] = 100, ['MP'] = 12, ['RDM'] = 24 },
            -- [2] = { ['Name'] = 'Enfire II', ['SID'] = 312, ['MP'] = 24, ['RDM'] = 58 }     -- Make #1 when uncommented
        },
        ['enblizzard'] = {
            [1] = { ['Name'] = 'Enblizzard', ['SID'] = 101, ['MP'] = 12, ['RDM'] = 22 },
            -- [2] = { ['Name'] = 'Enblizzard II', ['SID'] = 313, ['MP'] = 24, ['RDM'] = 56 }     -- Make #1 when uncommented
        },
        ['enaero'] = {
            [1] = { ['Name'] = 'Enaero', ['SID'] = 102, ['MP'] = 12, ['RDM'] = 20 },
            -- [2] = { ['Name'] = 'Enaero II', ['SID'] = 314, ['MP'] = 24, ['RDM'] = 52 }     -- Make #1 when uncommented
        },
        ['enstone'] = {
            [1] = { ['Name'] = 'Enstone', ['SID'] = 103, ['MP'] = 12, ['RDM'] = 18 },
            -- [2] = { ['Name'] = 'Enstone II', ['SID'] = 315, ['MP'] = 24, ['RDM'] = 52 }     -- Make #1 when uncommented
        },
        ['enthunder'] = {
            [1] = { ['Name'] = 'Enthunder', ['SID'] = 104, ['MP'] = 12, ['RDM'] = 16 },
            -- [2] = { ['Name'] = 'Enthunder II', ['SID'] = 316, ['MP'] = 24, ['RDM'] = 50 }     -- Make #1 when uncommented
        },
        ['enwater'] = {
            [1] = { ['Name'] = 'Enwater', ['SID'] = 105, ['MP'] = 12, ['RDM'] = 27 },
            -- [2] = { ['Name'] = 'Enwater II', ['SID'] = 317, ['MP'] = 24, ['RDM'] = 60 }     -- Make #1 when uncommented
        },
        ['katon'] = {
            [1] = { ['Name'] = 'Katon: Ni', ['SID'] = 321, ['MP'] = 0, ['NIN'] = 40 },
            [2] = { ['Name'] = 'Katon: Ichi', ['SID'] = 320, ['MP'] = 0, ['NIN'] = 15 }
            -- [3] = { ['Name'] = 'Katon: San', ['SID'] = 322, ['MP'] = 0, ['NIN'] = 75 }     -- Make #1 when uncommented
        },
        ['hyoton'] = {
            [1] = { ['Name'] = 'Hyoton: Ni', ['SID'] = 324, ['MP'] = 0, ['NIN'] = 40 },
            [2] = { ['Name'] = 'Hyoton: Ichi', ['SID'] = 323, ['MP'] = 0, ['NIN'] = 15 }
            --		{ ['Name'] = 'Hyoton: San', ['SID'] = 325, ['MP'] = 0, ['NIN'] = 75 }     -- Make #1 when uncommented
        },
        ['huton'] = {
            [1] = { ['Name'] = 'Huton: Ni', ['SID'] = 327, ['MP'] = 0, ['NIN'] = 40 },
            [2] = { ['Name'] = 'Huton: Ichi', ['SID'] = 326, ['MP'] = 0, ['NIN'] = 15 }
            -- [3] = { ['Name'] = 'Huton: San', ['SID'] = 328, ['MP'] = 0, ['NIN'] = 75 }     -- Make #1 when uncommented
        },
        ['doton'] = {
            [1] = { ['Name'] = 'Doton: Ni', ['SID'] = 330, ['MP'] = 0, ['NIN'] = 40 },
            [2] = { ['Name'] = 'Doton: Ichi', ['SID'] = 329, ['MP'] = 0, ['NIN'] = 15 }
            -- [3] = { ['Name'] = 'Doton: San', ['SID'] = 331, ['MP'] = 0, ['NIN'] = 75 }     -- Make #1 when uncommented
        },
        ['raiton'] = {
            [1] = { ['Name'] = 'Raiton: Ni', ['SID'] = 333, ['MP'] = 0, ['NIN'] = 40 },
            [2] = { ['Name'] = 'Raiton: Ichi', ['SID'] = 332, ['MP'] = 0, ['NIN'] = 15 }
            -- [3] = { ['Name'] = 'Raiton: San', ['SID'] = 334, ['MP'] = 0, ['NIN'] = 75 }     -- Make #1 when uncommented
        },
        ['suiton'] = {
            [1] = { ['Name'] = 'Suiton: Ni', ['SID'] = 336, ['MP'] = 0, ['NIN'] = 40 },
            [2] = { ['Name'] = 'Suiton: Ichi', ['SID'] = 335, ['MP'] = 0, ['NIN'] = 15 }
            -- [3] = { ['Name'] = 'Suiton: San', ['SID'] = 337, ['MP'] = 0, ['NIN'] = 75 }     -- Make #1 when uncommented
        },
        ['utsusemi'] = {
            [1] = { ['Name'] = 'Utsusemi: Ni', ['SID'] = 339, ['MP'] = 0, ['NIN'] = 37 },
            [2] = { ['Name'] = 'Utsusemi: Ichi', ['SID'] = 338, ['MP'] = 0, ['NIN'] = 12 }
        },
        ['hojo'] = {
            [1] = { ['Name'] = 'Hojo: Ni', ['SID'] = 345, ['MP'] = 0, ['NIN'] = 48 },
            [2] = { ['Name'] = 'Hojo: Ichi', ['SID'] = 344, ['MP'] = 0, ['NIN'] = 23 }
        },
        ['tonko'] = {
            [1] = { ['Name'] = 'Tonko: Ni', ['SID'] = 345, ['MP'] = 0, ['NIN'] = 34 },
            [2] = { ['Name'] = 'Tonko: Ichi', ['SID'] = 344, ['MP'] = 0, ['NIN'] = 9 }
        },
    },
    ['songs'] = {
        ['requiem'] = {
            [1] = { ['Name'] = 'Foe Requiem VI', ['SID'] = 373, ['Lvl'] = 67 },
            [2] = { ['Name'] = 'Foe Requiem V', ['SID'] = 372, ['Lvl'] = 57 },
            [3] = { ['Name'] = 'Foe Requiem IV', ['SID'] = 371, ['Lvl'] = 47 },
            [4] = { ['Name'] = 'Foe Requiem III', ['SID'] = 370, ['Lvl'] = 37 },
            [5] = { ['Name'] = 'Foe Requiem II', ['SID'] = 369, ['Lvl'] = 17 },
            [6] = { ['Name'] = 'Foe Requiem', ['SID'] = 368, ['Lvl'] = 7 }
        },
        ['paeon'] = {
            [1] = { ['Name'] = 'Army\'s Paeon V', ['SID'] = 382, ['Lvl'] = 65 },
            [2] = { ['Name'] = 'Army\'s Paeon IV', ['SID'] = 381, ['Lvl'] = 45 },
            [3] = { ['Name'] = 'Army\'s Paeon III', ['SID'] = 380, ['Lvl'] = 35 },
            [4] = { ['Name'] = 'Army\'s Paeon II', ['SID'] = 379, ['Lvl'] = 15 },
            [5] = { ['Name'] = 'Army\'s Paeon', ['SID'] = 378, ['Lvl'] = 5 }
        },
        ['ballad'] = {
            [1] = { ['Name'] = 'Mage\'s Ballad II', ['SID'] = 387, ['Lvl'] = 55 },
            [2] = { ['Name'] = 'Mage\'s Ballad', ['SID'] = 386, ['Lvl'] = 25 }
        },
        ['minne'] = {
            [1] = { ['Name'] = 'Knight\'s Minne IV', ['SID'] = 392, ['Lvl'] = 61 },
            [2] = { ['Name'] = 'Knight\'s Minne III', ['SID'] = 391, ['Lvl'] = 41 },
            [3] = { ['Name'] = 'Knight\'s Minne II', ['SID'] = 390, ['Lvl'] = 21 },
            [4] = { ['Name'] = 'Knight\'s Minne', ['SID'] = 389, ['Lvl'] = 1 }
        },
        ['minuet'] = {
            [1] = { ['Name'] = 'Valor Minuet IV', ['SID'] = 397, ['Lvl'] = 63 },
            [2] = { ['Name'] = 'Valor Minuet III', ['SID'] = 396, ['Lvl'] = 43 },
            [3] = { ['Name'] = 'Valor Minuet II', ['SID'] = 395, ['Lvl'] = 23 },
            [4] = { ['Name'] = 'Valor Minuet', ['SID'] = 394, ['Lvl'] = 3 }
        },
        ['madrigal'] = {
            [1] = { ['Name'] = 'Blade Madrigal', ['SID'] = 400, ['Lvl'] = 51 },
            [2] = { ['Name'] = 'Sword Madrigal', ['SID'] = 399, ['Lvl'] = 1 }
        },
        ['mambo'] = {
            [1] = { ['Name'] = 'Dragonfoe Mambo', ['SID'] = 404, ['Lvl'] = 53 },
            [2] = { ['Name'] = 'Sheepfoe Mambo', ['SID'] = 403, ['Lvl'] = 13 }
        },
        ['elegy'] = {
            [1] = { ['Name'] = 'Carnage Elegy', ['SID'] = 422, ['Lvl'] = 59 },
            [2] = { ['Name'] = 'Battlefield Elegy', ['SID'] = 421, ['Lvl'] = 39 }
        },
        ['march'] = {
            [1] = { ['Name'] = 'Victory March', ['SID'] = 420, ['Lvl'] = 60 },
            [2] = { ['Name'] = 'Advancing March', ['SID'] = 419, ['Lvl'] = 29 }
        }
    }
};

--[[
    fBardSongType determines if bard song being cast is of the type being passed.

    Parameter
        sType   Enfeebling (Enf) or Enhancement (Enh)

    Returned
        T/F, Was the song the type inquired about
--]]

function magic.fBardSongType(sType)
    local spell = gData.GetAction();
    local bGood = false;

    if sType == nil or spell.Name == nil then
        return false;
    end

    if string.lower(sType) == 'enh' then
        for i,j in pairs(utilities.tSpellGroupings['brd-enh']) do
            if string.find(string.lower(spell.Name),j) ~= nil then
                bGood = true;
                break;
            end
        end
    else
        for i,j in pairs(utilities.tSpellGroupings['brd-enf']) do
            if string.find(string.lower(spell.Name),j) ~= nil then
                bGood = true;
                break;
            end
        end
    end
    return bGood;
end		-- magic.fBardSongType

--[[
    HandlePrecast equips the appropriate precast gear
--]]

function magic.HandlePrecast()
    local spell = gData.GetAction();

    -- Clear out the CurrentGear in case of leftovers
    crossjobs.ClearSet(crossjobs.Sets.CurrentGear);

    if spell.Skill == 'Singing' then
        gear.MoveToDynamicGS(gProfile.Sets.SingingPrecast,crossjobs.Sets.CurrentGear,false,'SingingPrecast');
    else
       gear.MoveToDynamicGS(gProfile.Sets.Precast,crossjobs.Sets.CurrentGear,false,'Precast');
    end

    gear.EquipTheGear(crossjobs.Sets.CurrentGear);
end		-- magic.HandlePrecast

--[[
    HandleMidcast is a coordinating routine that's used to call the independent types
    of magic routines.
--]]

function magic.HandleMidcast()
    local spell = gData.GetAction();

    -- Clear out the CurrentGear in case of leftovers
    crossjobs.ClearSet(crossjobs.Sets.CurrentGear);

    if spell.Skill == 'Singing' then
        MidcastSinging();
    elseif spell.Skill == 'Healing Magic' then
        MidcastHealingMagic();
    elseif spell.Skill == 'Dark Magic' then
        MidcastDarkMagic();
    elseif spell.Skill == 'Divine Magic' then
        MidcastDivineMagic();
    elseif spell.Skill == 'Enfeebling Magic' then
        MidcastEnfeeblingMagic();
    elseif spell.Skill == 'Enhancing Magic' then
        MidcastEnhancingMagic();
    elseif spell.Skill == 'Elemental Magic' then
        MidcastElementalMagic();
    elseif spell.Skill == 'Summoning' then
        MidcastSummoning();
    elseif spell.Skill == 'Blue Magic' then
    	MidcastBlueMagic();
    --elseif spell.Skill == 'Geomancy' then
    --	MidcastGeomancy();
    elseif spell.Skill == 'Ninjutsu' then
        MidcastNinjutsu();
    end

    gear.EquipTheGear(sets.CurrentGear);
end		-- magic.MidcastNinjutsu

--[[
    MidcastSinging handles all of the equipment management when a song is cast
--]]

function MidcastSinging()
    local spell = gData.GetAction();

    if magic.fBardSongType('enh') == true then
        -- Enhancement song
        gear.MoveToDynamicGS(gProfile.Sets.EnhancementSinging,crossjobs.Sets.CurrentGear,false,'EnhancementSinging');
    elseif fBardSongType('enf') == true then
        -- Enfeebling song
        gear.MoveToDynamicGS(gProfile.Sets.EnfeeblingSinging,crossjobs.Sets.CurrentGear,false,'EnfeeblingSinging');
    end
end		-- MidcastSinging

--[[
    MidcastHealingMagic handles all of the equipment management when a healing spell
    is cast. Healing magic covers: cures to heal players, debuffs to remove status effects
    on players, and offensive cures to damage undead monsters. Each type is handled here.
--]]

function MidcastHealingMagic()
    local ti = gData.GetTargetIndex();
    local target = gData.GetEntity(ti);
    local spell = gData.GetAction();
    local root,sGear,pDay,pWeather;
    local sEle;

    root = utilities.fGetRoot(spell.Name,false);

    if string.find('curaga,cure',root) == nil then
        -- Start with the non-cure based spells. Even if magic accuracy indicated, these
        -- spells always hit and thus do not need magic accuracy. Further, an elemental
        -- stave will have no effect either.
        gear.MoveToDynamicGS(gProfile.Sets.HealingMagic,crossjobs.Sets.CurrentGear,false,'HealingMagic');
    else
        if target ~= nil then
            -- Some type of cure
            if target.Type == 'Monster' then
                -- Until I figure out how to determine that a monster is undead, just assume
                -- that if targetting a monster, it is undead.
                gear.MoveToDynamicGS(gProfile.Sets.OffensiveCuring,crossjobs.Sets.CurrentGear,false,'OffensiveCuring');
                -- Check for an elemental obi since this is an offensive spell. First
                -- determine if a bonus is possible based on day's element and/or weather
                sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
                if sGear ~= nil then
                    pDay,pWeather = utilities.fCheckObiDW(sEle);
                    if pDay + pWeather > 0 then
                        crossjobs.Sets.CurrentGear['Waist'] = sGear;
                    end
                end

                -- See if Macc should be added
                if utilities.fGetToggle('Macc') then
                    gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
                end
            else
                -- This is the the type of curing magic most folks assume happens
                gear.MoveToDynamicGS(gProfile.Sets.CuringMagic,crossjobs.Sets.CurrentGear,false,'CuringMagic');
            end
        end

        -- While the reasoning is different, both types of "cures" can use an elemental
        -- stave. (Offensive cures take advantage of affinity while regular cures
        -- appreciate the cure potency on a light-based staff.)
        sGear,sEle = gear.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
        if sGear ~= nil then
            gear.fSwapToStave(sGear,false,crossjobs.Sets.CurrentGear);
        end
    end
end		-- MidcastHealingMagic

--[[
    MidcastDarkMagic handles all of the equipment management when a dark spell is cast. Dark
    spells are dependent on the level of the dark spell magic. Most dark spells extract a
    "strength" from the target and gives it to the player. The exception is bio/ii (which is
    a dot and lowers the target's attack), tractor, and stun.

    For drain/Aspir, Dark Magic Skill > all else. For absorb spells, Dark=MAcc=2xINT
--]]

function MidcastDarkMagic()
    local spell = gData.GetAction();
    local ew = gData.GetEquipment();
    local root,pDay,pWeather,sGear;
    local sEle;

    root = utilities.fGetRoot(spell.Name,false);

    if table.find(utilities.tSpellGroupings['absorb'],root) ~= nil then
        -- It's an absorb spell
        gear.MoveToDynamicGS(gProfile.Sets.Absorb,crossjobs.Sets.CurrentGear,false,'Absorb');

        -- See if Macc should be added
        if utilities.fGetToggle('Macc') then
            gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
        end
    elseif root == 'drain' then
        gear.MoveToDynamicGS(gProfile.Sets.Drain,crossjobs.Sets.CurrentGear,false,'Drain');

        -- Check for an elemental obi. First determine if a bonus is possible
        -- based on day's element and/or weather
        sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
        if sGear ~= nil then
            pDay,pWeather = utilities.fCheckObiDW(sEle);
            if pDay + pWeather > 0 then
                crossjobs.Sets.CurrentGear['Waist'] = sGear;
            end
        end

        -- See if Macc should be added
        if utilities.fGetToggle('Macc') then
            gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
        end

        -- And an elemental staff, for the affinity
        sGear,sEle = gear.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
        if sGear ~= nil then
            -- There's a wild exception that needs to be checked here. If /WSWAP is
            -- enabled and the player is wielding Y's Scythe (only equipable by DRK)
            -- and sGear = 'Dark Staff', then don't equip it. Y's scythe grants +1
            -- dark magic affinity which is what a dark staff does. There's no
            -- advantage to equipping the staff.
            if not (utilities.fGetToggle('WSwap') == true and
                    ew['Main'] == 'Y\'s Scythe' and sGear == 'Dark Staff') then
                gear.fSwapToStave(sGear,false,crossjobs.Sets.CurrentGear);
            end
        end
    elseif root == 'aspir' then
        gear.MoveToDynamicGS(gProfile.Sets.Aspir,crossjobs.Sets.CurrentGear,false,'Aspir');

        -- Check for an elemental obi. First determine if a bonus is possible
        -- based on day's element and/or weather
        sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
        if sGear ~= nil then
            pDay,pWeather = utilities.fCheckObiDW(sEle);
            if pDay + pWeather > 0 then
                crossjobs.Sets.CurrentGear['Waist'] = sGear;
            end
        end

        -- See if Macc should be added
        if utilities.fGetToggle('Macc') then
            gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
        end

        -- And an elemental staff, for the affinity
        sGear,sEle = gear.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
        if sGear ~= nil then
            -- There's a wild exception that needs to be checked here. If /WSWAP is
            -- enabled and the player is wielding Y's Scythe (only equipable by DRK)
            -- and sGear = 'Dark Staff', then don't equip it. Y's scythe grants +1
            -- dark magic affinity which is what a dark staff does. There's no
            -- advantage to equipping the staff.
            if not (utilities.fGetToggle('WSwap') == true and
                ew['Main'] == 'Y\'s Scythe' and sGear == 'Dark Staff') then
                gear.fSwapToStave(sGear,false,crossjobs.Sets.CurrentGear);
            end
        end
    elseif root == 'dread' then
        -- Dread Spikes, out of era, but coming soonish
        gear.MoveToDynamicGS(gProfile.Sets.Dreadspikes,crossjobs.Sets.CurrentGear,false,'Dreadspikes');
    else
        -- All other dark magic spells
        gear.MoveToDynamicGS(gProfile.Sets.DarkMagic,crossjobs.Sets.CurrentGear,false,'DarkMagic');

        -- See if Macc should be added
        if utilities.fGetToggle('Macc') then
            gear.MoveToCurrent(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
        end
    end
end		-- MidcastDarkMagic

--[[
    MidcastDivineMagic handles all of the equipment management when a divine spell is cast.
    Divine Magic is highly dependent on the level of the divine magic skill and MND. This
    routine handles three types of divine magic: offensive "nukes", Enfeebling, and Enhancing.
--]]

function MidcastDivineMagic()
    local spell = gData.GetAction();
    local root,pDay,pWeather,sGear;
    local sEle;

    root = utilities.fGetRoot(spell.Name,false);

    if table.find({'banish','banishga','holy','enlight'},root) ~= nil then
        -- Offensive Divine spell
        gear.MoveToDynamicGS(gProfile.Sets.OffensiveDivine,crossjobs.Sets.CurrentGear,false,'OffensiveDivine');

        -- Check for an elemental obi. First determine if a bonus is possible
        -- based on day's element and/or weather
        sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
        if sGear ~= nil then
            pDay,pWeather = utilities.fCheckObiDW(sEle);
            if pDay + pWeather > 0 then
                crossjobs.Sets.CurrentGear['Waist'] = sGear;
            end
        end

        -- See if Macc should be added
        if utilities.fGetToggle('Macc') then
            gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
        end
    elseif table.find({'flash','repose'},root) ~= nil then
        -- Enfeebling Divine spell
        gear.MoveToDynamicGS(gProfile.Sets.EnfeebleDivine,crossjobs.Sets.CurrentGear,false,'EnfeebleDivine');

        -- See if Macc should be added
        if utilities.fGetToggle('Macc') then
            gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
        end
    else
        -- Enhancing Divine spell
        gear.MoveToDynamicGS(gProfile.Sets.EnhanceDivine,crossjobs.Sets.CurrentGear,false,'EnhanceDivine');
    end

    -- And see if an elemental staff would be useful, for the affinity
    sGear,sEle = gear.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
    if sGear ~= nil then
        gear.fSwapToStave(sGear,false,crossjobs.Sets.CurrentGear);
    end
end		-- MidcastDivineMagic

--[[
    MidcastEnfeeblingMagic handles all of the equipment management when a enfeeble
    spell is cast. There are two subdivisions of enfeeble spells, those that depend
    on INT and those that depend on MND. The rest of the gear details are fairly in
    common: you want high enfeeble magic skill, magical accuracy, etc.
--]]

function MidcastEnfeeblingMagic()
    local spell = gData.GetAction();
    local root,sGear,sEle;
    local pDay,pWeather;

    root = utilities.fGetRoot(spell.Name);

    if table.find(utilities.tSpellGroupings['int'],root) ~= nil then
        -- INT: gravity,bind,blind,dispel,sleep,sleepga,poison,poisonga
        gear.MoveToDynamicGS(gProfile.Sets.EnfeeblingINT,crossjobs.Sets.CurrentGear,false,'EnfeeblingINT');
    elseif table.find(utilities.tSpellGroupings['mnd'],root) ~= nil then
        -- MND: paralyze,silence,slow,slowga,frazzle,distract
        gear.MoveToDynamicGS(gProfile.Sets.EnfeeblingMND,crossjobs.Sets.CurrentGear,false,'EnfeeblingMND');
    else
        gear.MoveToDynamicGS(gProfile.Sets.EnfeeblingMagic,crossjobs.Sets.CurrentGear,false,'EnfeeblingMagic');
    end

    -- See if an elemental obi would make sense for the Magical Elemental accuracy
    sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
    if sGear ~= nil then
        pDay,pWeather = utilities.fCheckObiDW(sEle);
        if pDay + pWeather > 0 then
            crossjobs.Sets.CurrentGear['Waist'] = sGear;
        end
    end

    -- See if Macc should be added
    if utilities.fGetToggle('Macc') then
        gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
    end

    -- And then if an elemental staff would be useful, for the affinity
    sGear,sEle = gear.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
    if sGear ~= nil then
        gear.fSwapToStave(sGear,false,crossjobs.Sets.CurrentGear);
    end
end		-- MidcastEnfeeblingMagic

--[[
    MidcastEnhancingMagic handles all of the equipment management when an enhancing
    spell is cast. Enhancing magic is sort of a catch-all category. It includes bar-
    spells, en-spells, protect/shell, regen, spikes, teleports, and warps.
--]]

function MidcastEnhancingMagic()
    local spell = gData.GetAction();
    local root,sGear,sEle;
    local pDay,pWeather;

    root = utilities.fGetRoot(spell.Name,false);

    if table.find(utilities.tSpellGroupings['barspell']['ele'],root) ~= nil or
        table.find(utilities.tSpellGroupings['barspell']['status'],root) ~= nil then
        -- A bar spell
        gear.MoveToDynamicGS(gProfile.Sets.Barspell,crossjobs.Sets.CurrentGear,false,'Barspell');
    elseif table.find(utilities.tSpellGroupings['enspell'],root) ~= nil then
        -- En-spell: en"element". Sword enhancing gear applies to all melee
        -- weapons and is applied when the spell is cast. Damage is calculated
        -- after the weapon hits.
        gear.MoveToDynamicGS(gProfile.Sets.Enspell,crossjobs.Sets.CurrentGear,false,'Enspell');

        -- See if an elemental obi would make sense for the Magical Elemental accuracy
        sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
        if sGear ~= nil then
            pDay,pWeather = utilities.fCheckObiDW(sEle);
            if pDay + pWeather > 0 then
                crossjobs.Sets.CurrentGear['Waist'] = sGear;
            end
        end
    elseif table.find(utilities.tSpellGroupings['spikes'],root) ~= nil then
        -- Spike spell: Blaze, Ice, and Shock. Damage based on INT (capped), MAB,
        -- day/weather bonuses, Magic Affinity at time of hit. Enhancing spike gear
        -- is equipped when cast
        gear.MoveToDynamicGS(gProfile.Sets.Spike,gProfile.crossjobs.CurrentGear,false,'Spike');

        -- See if an elemental obi would make sense for the Magical Elemental accuracy
        sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
        if sGear ~= nil then
            pDay,pWeather = utilities.fCheckObiDW(sEle);
            if pDay + pWeather > 0 then
                crossjobs.Sets.CurrentGear['Waist'] = sGear;
            end
        end
    elseif root == 'stoneskin' then
        gear.MoveToDynamicGS(gProfile.Sets.Stoneskin,crossjobs.Sets.CurrentGear,false,'Stoneskin');
    elseif root == 'sneak' then
        gear.MoveToDynamicGS(gProfile.Sets.Sneak,crossjobs.Sets.CurrentGear,false,'Sneak');
    elseif root == 'invisible' then
        gear.MoveToDynamicGS(gProfile.Sets.Invisible,crossjobs.Sets.CurrentGear,false,'Invisible');
    elseif root == 'phalanx' then
        gear.MoveToDynamicGS(gProfile.Sets.Phalanx,crossjobs.Sets.CurrentGear,false,'Phalanx');
    else
        -- Catch all for the rest of the enhancing spells
        gear.MoveToDynamicGS(gProfile.Sets.EnhancingMagic,crossjobs.Sets.CurrentGear,false,'EnhancingMagic');
    end
end		-- MidcastEnhancingMagic

--[[
    MidcastElementalMagic handles all of the equipment management when a elemental
    spell is cast. Elemental magic on HorizonXI falls into two categories: nukes and
    debuffs. (You could argue that ancient magic should be broken out too, but for
    now it falls under nukes.) All elemental spells are INT based. Nukes are based
    on the difference in INT between the caster and the target whereas for debuffs,
    the caster's INT is used to determine the amount of damage each tick the debuff
    does as well as how much the target stat is lowered. Elemental magic skill is
    used to determine accuracy and spell interruption rate.
--]]

function MidcastElementalMagic()
    local spell = gData.GetAction();
    local root,sGear,sEle;
    local pDay,pWeather;

    root = utilities.fGetRoot(spell.Name,false);

    if table.find(utilities.tSpellGroupings['eDebuff'],root) ~= nil then
        -- Elemental debuff spell
        gear.MoveToDynamicGS(gProfile.Sets.ElementalDebuff,crossjobs.Sets.CurrentGear,false,'ElementalDebuff');
    else
        -- Nuke spell
        gear.MoveToDynamicGS(gProfile.Sets.ElementalNuke,crossjobs.Sets.CurrentGear,false,'ElementalNuke');
    end

    -- See if an elemental obi would make sense for the Magical Elemental accuracy
    sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
    if sGear ~= nil then
        pDay,pWeather = utilities.fCheckObiDW(sEle);
        if pDay + pWeather > 0 then
            crossjobs.Sets.CurrentGear['Waist'] = sGear;
        end
    end

    -- See if Macc should be added
    if utilities.fGetToggle('Macc') then
        gear.MoveToDynamicGS(gProfile.Sets.Macc,crossjobs.Sets.CurrentGear,false,'Macc');
    end

    -- And then if an elemental staff would be useful, for the affinity
    sGear,sEle = gear.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
    if sGear ~= nil then
        gear.fSwapToStave(sGear,false,crossjobs.Sets.CurrentGear);
    end
end		-- MidcastElementalMagic

--[[
    MidcastSummoning handles all of the equipment management when a avatar/spirit
    summoning spell is cast. This routine is straightforward. While some gear is
    more advantageous than others, that can all be handled with inline conditionals.
--]]

function MidcastSummoning()
    gear.MoveToDynamicGS(gProfile.Sets.Summoning,crossjobs.Sets.CurrentGear,false,'Summoning');
end		-- MidcastSummoning

--[[
    MidcastBlueMagic handles all of the equipment management when a blue magic
    spell is cast.

    WIP: Until more details are released on how HorizonXI is implementing blue
    magic, this function is nothing but a stub function
--]]

function MidcastBlueMagic()
end		-- MidcastBlueMagic

--[[
    fMidcastGeomancy handles all of the equipment management when a geomancy magic
    spell is cast.

    WIP: Until more details are released on how HorizonXI is implementing geomancy
    magic, this function is nothing but a stub function
--]]

function MidcastGeomancy()
end		-- MidcastGeomancy

--[[
    MidcastNinjutsu handles all of the equipment management when a ninjutsu spell
    is cast.
--]]

function MidcastNinjutsu()
    local spell = gData.GetAction();
    local root,sGear,sEle;
    local pDay,pWeather;

    root = utilities.fGetRoot(spell.Name);

    -- There's three types of ninjutsu: buff, debuff and elemental. Anything
    -- else is a mystery and will be processed with the current gear.
    if table.find(utilities.tSpellGroupings['nin-buff'],root) ~= nil then
        -- Buff
        gear.MoveToDynamicGS(gProfile.Sets.NinjutsuBuff,crossjobs.Sets.CurrentGear,false,'NinjutsuBuff');
    else
        if table.find(utilities.tSpellGroupings['nin-debuff'],root) ~= nil then
            -- Debuff
            gear.MoveToDynamicGS(gProfile.Sets.NinjutsuDebuff,crossjobs.Sets.CurrentGear,false,'NinjutsuDebuff');
        elseif table.find(utilities.tSpellGroupings['nin-ele'],root) ~= nil then
            -- Elemental
            gear.MoveToDynamicGS(gProfile.Sets.NinjutsuElemental,crossjobs.Sets.CurrentGear,false,'NinjutsuElemental');

            -- See if an elemental obi would make sense for the Magical Elemental accuracy
            sGear,sEle = gear.fCheckForElementalGearByValue('obi','MEacc',root);
            if sGear ~= nil then
                pDay,pWeather = utilities.fCheckObiDW(sEle);
                if pDay + pWeather > 0 then
                    crossjobs.Sets.CurrentGear['Waist'] = sGear;
                end
            end
        else
            return;
        end

        -- And then if an elemental staff would be useful, for the affinity
        sGear,sEle = gear.fCheckForElementalGearByValue('staff','Affinity',spell.Name);
        if sGear ~= nil then
            gear.fSwapToStave(sGear,false,crossjobs.Sets.CurrentGear);
        end
    end
end		-- MidcastNinjutsu

--[[
    MaxCast sees if the passed in spell/song is a tiered spell and determines the
    highest tier of the spell the player can cast. The routine makes that decision
    based on the level of the spell/song, the player's MP, does the player know
    the spell/song, and is that spell/song off cool down. If indicated, the
    routine will invoke the found spell/song.

    Parameters:
        sName               Name of the spell's root or song's buff name
        bSpell      T/F     Is the past value the name of a spell or song
        sTarget             Who/what should the spell/song be cast on
        bCast       T/F     Should the found spell/song be cast
--]]

function magic.MaxCast(sName,bSpell,sTarget,bCast)
    local player = gData.GetPlayer();
    local sMain = player.MainJob;
    local sSub = player.SubJob;
    local MainLvl = player.MainJobSync;
    local SubLvl = player.SubJobSync;
    local root,sCmd;
    local tInd = {};

    if sSpell == nil then
        print(chat.message('Warning: No spell/song specified. Aborting...'));
        return;
    end

    if bSpell == nil then
        bSpell = true;      -- Got to assume something, assuming a spell since not specified
    end

    if sTarget == nil then
        -- Since target is missing, use the appropriate default setting
        if bSpell == true then
            sTarget = '<' .. crossjobs.settings.DefaultSpellTarget .. '>';
        else
            sTarget = '<' .. crossjobs.settings.DefaultSongTarget .. '>';
        end
    elseif string.find(sTarget,'<') == nil then
        sTarget = '<' .. sTarget .. '>';
    end

    -- Make sure all parameters passed make sense
    if bCast == nil then
        bCast = false;
    end

    -- Now, determine where in the Tiered structure to point to based on whether a spell or a song
    if bSpell == true then
        tInd = magic.Tiered.spells;
    else
        tInd = magic.Tiered.songs;
    end

    root = utilities.fGetRoot(sName);
    sCmd = nil;

    -- Now, walk the list looking for a matched entry for the "root"
    for i,j in pairs(tInd) do
        -- If the group matches the root
        if i == root then
            -- Found the spell/song. Loop the tiers to find the maximum one to cast
            for ii,jj in ipairs(j) do
                -- Here is where we care if we're dealing with a spell or song. Start with Spell
                if bSpell == true then
                    -- Make sure the player's job/subjob can cast it and meet the level requirements
                    if (jj[sMain] ~= nil and jj[sMain] <= MainLvl) or (jj[sSub] ~= nil and jj[sSub] <= SubLvl) then
                        -- Now make sure the player knows the spell
                        if AshitaCore:GetMemoryManager():GetPlayer():HasSpell(jj['SID']) then
                            -- Then make sure it's not on cool down
                            if AshitaCore:GetMemoryManager():GetRecast():GetSpellTimer(jj['SID']) == 0 then
                                -- And lastly, do they have enough MP
                                if player.MP >= jj['MP'] then
                                    if bCast == true then
                                        sCmd = '/ma "' .. jj['Name'] .. '" ' .. sTarget;
                                    else
                                        sCmd = jj['Name'];
                                    end
                                    break;
                                end
                            else
                                print(chat.message('Info: ' .. jj['Name'] .. ' is on cool down. Skipping'));
                            end
                        else
                            print(chat.message('Info: You should be able to cast '.. jj['Name'] .. ', but don\'t know it. Skipping'));
                        end
                    end
                else
                    -- You have to be a BRD or /BRD to sing a song
                    if sMain == 'BRD' or sSub == 'BRD' then
                        -- Are they high enough level to use it
                        if (sMain == 'BRD' and jj['Lvl'] <= MainLvl ) or (sSub == 'BRD' and jj['Lvl'] <= SubLvl) then
                            -- Now make sure the player knows the song
                            if AshitaCore:GetMemoryManager():GetPlayer():HasSpell(jj['SID']) then
                                -- And lastly, make sure it's not on cool down
                                if AshitaCore:GetMemoryManager():GetRecast():GetSpellTimer(jj['SID']) == 0 then
                                    if bCast == true then
                                        sCmd = '/ma "' .. jj['Name'] .. '" ' .. sTarget;
                                    else
                                        sCmd = jj['Name'];
                                    end
                                    break;
                                else
                                    print(chat.message('Info: ' .. jj['Name'] .. ' is on cool down. Skipping'));
                                end
                            else
                                print(chat.message('Info: You should be able to cast '.. jj['Name'] .. ', but don\'t know it. Skipping'));
                            end
                        end
                    else
                        print(chat.message('Warning: Only bards can sing songs. Skipping')));
                        break;
                    end
                end
            end
        end
        if sCmd ~= nil then
            break;
        end
    end

    if sCmd ~= nil then
        if bCast == true then
            AshitaCore:GetChatManager():QueueCommand(1, sCmd);
        else
            print(chat.message('Info: ' .. sCmd .. ' is the maximum version you can cast/sing now.'));
        end
    else
        print(chat.message('Info: '.. sName ..' not found, probably not a tiered spell/song.'));
        if bCast == true then
            -- Let's try to cast it even so. Assuming everything, just try
            print(chat.message('Info: Trying to cast/sing '.. sName.. ' as is'));
            sCmd = '/ma "' .. sName .. '" ' .. sTarget;
            AshitaCore:GetChatManager():QueueCommand(1, sCmd);
            end
        end
    end
end     -- magic.MaxCast
