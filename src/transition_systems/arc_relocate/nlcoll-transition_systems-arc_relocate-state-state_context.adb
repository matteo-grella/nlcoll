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

separate (NLColl.Transition_Systems.Arc_Relocate.State)

package body State_Context is
    
    procedure Print_Context
      (Context : in Context_Type) is
    begin
        Put("Stack(3)  => " & Get_Token_ID(Context.Stack(3))'Img); Put(" | ");
        Put("Stack(2)  => " & Get_Token_ID(Context.Stack(2))'Img); Put(" | ");
        Put("Stack(1)  => " & Get_Token_ID(Context.Stack(1))'Img); Put(" | ");
        Put("Stack(0)  => " & Get_Token_ID(Context.Stack(0))'Img); Put(" | ");
        Put("Buffer(0) => " & Get_Token_ID(Context.Buffer(0))'Img); Put(" | ");
        Put("Buffer(1) => " & Get_Token_ID(Context.Buffer(1))'Img); Put(" | ");
        Put("Buffer(2) => " & Get_Token_ID(Context.Buffer(2))'Img); Put(" | " );
        New_Line;
    end Print_Context;
    
    procedure Get_Context
      (Context  : in out Context_Type;
       State    : in     State_Type;
       Sentence : in     Sentence_Type) is

        Stack_Length  : constant Length_Type := Length_Type (State.Stack.Length);
        Buffer_Length : constant Length_Type := Length_Type (State.Buffer.Length);
    begin

        Context.Stack (3)
          := (if Stack_Length > 3
              then Sentence.Elements (Index_Type (State.Stack.Element (State.Stack.Last_Index - Index_Type (3))) - Index_Type (1))
              else null);

        Context.Stack (2)
          := (if Stack_Length > 2
              then Sentence.Elements (Index_Type (State.Stack.Element (State.Stack.Last_Index - Index_Type (2))) - Index_Type (1))
              else null);

        Context.Stack (1)
          := (if Stack_Length > 1
              then Sentence.Elements (Index_Type (State.Stack.Element (State.Stack.Last_Index - Index_Type (1))) - Index_Type (1))
              else null);

        Context.Stack (0)
          := (if Stack_Length > 0
              then Sentence.Elements (Index_Type (State.Stack.Last_Element) - Index_Type (1))
              else null);

        Context.Buffer (0)
          := (if Buffer_Length > 0
              then Sentence.Elements (Index_Type (State.Buffer.Last_Element) - Index_Type (1))
              else null);

        Context.Buffer (1)
          := (if Buffer_Length > 1
              then Sentence.Elements (Index_Type (State.Buffer.Element (State.Buffer.Last_Index - Index_Type (1))) - Index_Type (1))
              else null);

        Context.Buffer (2)
          := (if Buffer_Length > 2
              then Sentence.Elements (Index_Type (State.Buffer.Element (State.Buffer.Last_Index - Index_Type (2))) - Index_Type (1))
              else null);

        Context.Buffer (3)
          := (if Buffer_Length > 3
              then Sentence.Elements (Index_Type (State.Buffer.Element (State.Buffer.Last_Index - Index_Type (3))) - Index_Type (1))
              else null);

    end Get_Context;
    
end State_Context;
