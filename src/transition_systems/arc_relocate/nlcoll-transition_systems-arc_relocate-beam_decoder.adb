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

package body NLColl.Transition_Systems.Arc_Relocate.Beam_Decoder is

    function Insert_State
      (State_List         : in out State_Access_Lists.List;
       State              : in     State_Access;
       Max_Beam_Size      : in     Positive) return Real is

        use type State_Access_Lists.Cursor;

        State_List_Cursor  : State_Access_Lists.Cursor;

        New_State_Inserted : Boolean := False;
    begin

        ---
        -- If list is full, create space: free & delete last element
        ---

        if Natural (State_List.Length) = Max_Beam_Size then
            State_Finalize (State_List.Last_Element.all);
            Free (State_List.Reference (State_List.Last));
            State_List.Delete_Last;
        end if;


        ---
        -- Insert state in the right position
        ---

        State_List_Cursor := State_List.First;

        while State_Access_Lists.Has_Element (State_List_Cursor) loop

            if State.Log_Prob > State_Access_Lists.Element (State_List_Cursor).Log_Prob then
                State_List.Insert (State_List_Cursor, State);
                New_State_Inserted := True;
                exit;
            end if;

            State_List_Cursor := State_Access_Lists.Next (State_List_Cursor);
        end loop;

        ---
        -- Not yet inserted: the new state becomes the last element
        ---

        if not New_State_Inserted then
            State_List.Append (State);
        end if;

        ---
        -- Worst probability from last state in list
        ---

        return State_List.Last_Element.Log_Prob;

    end Insert_State;

end NLColl.Transition_Systems.Arc_Relocate.Beam_Decoder;

