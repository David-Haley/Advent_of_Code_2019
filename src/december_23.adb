with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Intercode_09; use Intercode_09;

procedure December_23 is
   -- N.B. Produces the correct answers but continues to run indefinitely. There
   -- is no provision for terminating the 151 tasks that are created!

   subtype Network_Addresses is Program_Store_Elements range 0 .. 49;
   NAT_Address : constant Program_Store_Elements := 255;

   type Packets is record
      Address : Network_Addresses;
      X, Y : Program_Store_Elements;
   end Record; -- Packets

   package QI is new
     Ada.Containers.Synchronized_Queue_Interfaces (Packets);

   package Rx_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (QI);

   task type NIC_Rxs is
      entry Start (Address : Network_Addresses);
   end NIC_Rxs;

   task type NIC_Txs is
      entry Start (Address : Network_Addresses);
   end NIC_Txs;

   type Network_Node is record
      CPU : Processor;
      NIC_Rx : NIC_Rxs;
      NIC_Tx : NIC_Txs;
      Rx_Queue : Rx_Queues.Queue;
   end record; -- Network_Node

   Network_Array : array (Network_Addresses) of Network_Node;

   task NAT is
   end NAT;

   NAT_Queue : Rx_Queues.Queue;

   task body NIC_Rxs is

      Finished : Boolean := False;
      This_Address : Network_Addresses;
      No_Data : constant Program_Store_Elements := -1;
      Packet : Packets;

   begin -- NIC_Rxs
      accept Start (Address : in Network_Addresses) do
         This_Address := Address;
      end Start;
      Network_Array (This_Address).CPU.Send_Input (This_Address);
      loop -- receive one packet
         if Network_Array (This_Address).Rx_Queue.Current_Use > 0 then
            Network_Array (This_Address).Rx_Queue.Dequeue (Packet);
            Assert (Packet.Address = This_Address,
                    "CPU:" & Network_Addresses'Image (This_Address) &
                      " Mismatch Rx Address:" &
                      Program_Store_Elements'Image (Packet.Address));
            begin -- Input exception
               Network_Array (This_Address).CPU.Send_Input (Packet.X);
               Network_Array (This_Address).CPU.Send_Input (Packet.Y);
            exception
               when Tasking_Error =>
                  Finished := True;
            end; -- Input exception
            exit when Finished;
         else
            begin -- Input exception
               Network_Array (This_Address).CPU.Send_Input (No_Data);
            exception
               when Tasking_Error =>
                  Finished := True;
            end; -- Input exception
            exit when Finished;
            delay 0.01;
            -- Reduces wastage of host CPU time as it suspendes the task for
            -- 10 ms when the Queue is empty. The task could wait on Send_Input;
            -- however this is contary to the problem description.
         end if; -- Network_Array (This_Address).Rx_Queue.Current_Use > 0
      end loop; -- receive one packet
   end NIC_Rxs;

   task body NIC_Txs is

      Finished : Boolean := False;
      This_Address : Network_Addresses;
      Packet : Packets;
      First_TX : Program_Store_Elements;

   begin -- NIC_Txs
      accept Start (Address : in Network_Addresses) do
         This_Address := Address;
      end Start;
      loop -- transmit one packet
         begin -- Output exception
            Network_Array (This_Address).CPU.Receive_Output (First_TX);
            -- the receive data cannot be of type Network address because the
            -- NAT uses address 255.
            Network_Array (This_Address).CPU.Receive_Output (Packet.X);
            Network_Array (This_Address).CPU.Receive_Output (Packet.Y);
         exception
            when Tasking_Error =>
               Finished := True;
         end; -- Output exception
         exit when Finished;
         if First_TX = NAT_Address then
            Packet.Address := This_Address;
            -- Address of sending computer substituted for the destination for
            -- debugging purposes for my input it was always 22. This was useful
            -- to know because it ment that that there is only one computer
            -- writing to the queue and hence the order of receive data should
            -- be deterministic.
            NAT_Queue.Enqueue (Packet);
         elsif First_TX in Network_Addresses then
            Packet.Address := First_TX;
            Network_Array (Packet.Address).Rx_Queue.Enqueue (Packet);
         else
            Put_Line ("CPU:" & Network_Addresses'Image (This_Address) &
                        "Bad Destination");
         end if; -- First_TX = 255
      end loop; -- transmit one packet
   end NIC_Txs;

   task body NAT is

      Packet, Previous : Packets;
      All_Idle, First_Match : Boolean := True;

   begin -- NAT
      Previous.Y := 0;
      NAT_Queue.Dequeue (Packet);
      Put_Line ("First NAT receive:" & " Y:" &
                  Program_Store_Elements'Image (Packet.Y));
      while First_Match loop
         while NAT_Queue.Current_Use > 0 loop
            NAT_Queue.Dequeue (Packet);
         end loop; -- NAT_Queue.Current_Use > 0
         All_Idle := True;
         for A in Network_Addresses loop
            All_Idle := All_Idle and
              Network_Array (A).Rx_Queue.Current_Use = 0;
         end loop; -- A in Network_Addresses
         if All_Idle then
            if Packet.Y = Previous.Y and First_Match then
               Put_Line ("First NAT receive match:" & " Y:" &
                           Program_Store_Elements'Image (Packet.Y));
               First_Match := False;
            end if; -- Packet.Y = Previous.Y and First_Match
            Previous := Packet;
            Packet.Address := 0;
            Network_Array (0).Rx_Queue.Enqueue (Packet);
            delay 0.1;
            -- delay desirable, posssibly essential to allow network traffic to
            -- finish before reading what is in the NAT Queue.
         end if; --  All_Idle;
      end loop; -- First_Match
      for A in Network_Addresses loop
         abort Network_Array (A).NIC_Rx, Network_Array (A).NIC_Tx,
           Network_Array (A).CPU;
      end loop; -- A in Network_Addresses
   end NAT;

begin -- December_23
   for A in Network_Addresses loop
      Network_Array (A).CPU.Load_Program ("December_23.txt");
      Network_Array (A).NIC_Rx.Start (A);
      Network_Array (A).NIC_Tx.Start (A);
   end loop;
   Put_Line ("All CPUs Initialised");
   for A in Network_Addresses loop
      Network_Array (A).CPU.Run_Program;
   end loop; -- A in Network_Addresses
   Put_Line ("All CPUs Started");
end December_23;
