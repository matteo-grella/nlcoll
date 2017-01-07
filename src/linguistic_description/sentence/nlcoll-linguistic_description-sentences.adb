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

package body NLColl.Linguistic_Description.Sentences is

    package body Tokens is

        procedure Pop
          (Token_Vector : in out Token_Index_Vectors.Vector) is
        begin
            Token_Vector.Delete_Last;
        end Pop;

        function Pop
          (Token_Vector : in out Token_Index_Vectors.Vector) return Token_Index_Type is
        begin
            return Last_Element : constant Token_Index_Type := Token_Vector.Last_Element do
                Token_Vector.Delete_Last;
            end return;
        end Pop;

        procedure Finalize
          (Token_Vector : in Token_Vectors.Vector) is
        begin

            for I in Token_Vector.First_Index .. Token_Vector.Last_Index loop
                declare
                    Token : Token_Access := Token_Vector.Element (I);
                begin
                    Free (Token);
                end;
            end loop;

        end Finalize;
    end Tokens;

end NLColl.Linguistic_Description.Sentences;
