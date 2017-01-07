------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--          Copyright 2009-2011 M. Grella, M. Nicola, D. Christen
--
--  In collaboration with Politecnico di Torino and Università di Torino.
--
--  Supported by
--   (a) Fondazione CRT under a "VivoMeglio" grant (Speak2Home)
--   (b) Region Piedmont under a “Converging Technologies” programme (ATLAS)
--        
--  The project aims at developing open-source linguistic technologies for the
--  Italian language to improve the welfare of people, especially impaired users.
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

package body NLColl.Annotators.Tokenizer is

    procedure Tokenize is
        Unicode_Str    : constant Wide_Wide_String := "ÈloEl BarçaoÈkièng";
        -- Test Unicode
        
        Unicode_Length : constant Natural := Unicode_Str'Length;
    begin

        Put_Line(Unicode_Str);
        Put_Line(Unicode_Length'Img);
        Put_Line(To_Upper(Unicode_Str));

        ----------------------------------------
        --     Features      |  Window  |  D  --
        ----------------------------------------
        --      Symbols      |    1     |  8  --
        --   Capitalization  |   +/- 1  |  4  --
        --   Pref/Suff (2,3) |   +/- 1  | 16  --
        --       Words       |   +/- 3  | 64  --
        ----------------------------------------

        declare
            First : constant Natural := Unicode_Str'First;
            Last  : constant Natural := Unicode_Str'Last;
        begin

            Put_Line(Is_Upper(Unicode_Str(First))'Img);

            for C in 2 .. 3 loop
                Put_Line(Unicode_Str(First .. First + C - 1));
                Put_Line(Unicode_Str(Last - (C - First) .. Last));
            end loop;
        end;
    end Tokenize;
    
    procedure Empty is
    begin
        null;
    end Empty;
    
end NLColl.Annotators.Tokenizer;
