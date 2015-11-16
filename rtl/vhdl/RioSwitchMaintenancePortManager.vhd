-------------------------------------------------------------------------------
-- (C) Copyright 2013-2015 Authors and the Free Software Foundation.
--
-- This file is part of OpenRIO.
--
-- OpenRIO is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- OpenRIO is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU General Lesser Public License
-- along with OpenRIO. If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Description
-- Helper module for RioSwitch.
-- 
-- Backlog:
-- 
-- 
-- Author(s): 
-- - Anders Thornemo, anders.thornemo@se.transport.bombardier.com
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- RioSwitchMaintenancePortManager
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioSwitchMaintenancePortManager
-------------------------------------------------------------------------------
entity RioSwitchMaintenancePortManager is
  generic(
    SWITCH_PORTS : natural := 4;
    PORT_WRITE_TIMEOUT_VALUE : natural);
  port(
    clk : in  std_logic;
    areset_n : in  std_logic;

    -- Signal interconnect for MaintenanceInbound    --
    readRequestReadyInbound_i : in  std_logic;
    writeRequestReadyInbound_i : in  std_logic;
    readResponseReadyInbound_i : in  std_logic;
    writeResponseReadyInbound_i : in  std_logic;
    portWriteReadyInbound_i : in  std_logic;
    vcInbound_i : in  std_logic;
    crfInbound_i : in  std_logic;
    prioInbound_i : in  std_logic_vector( 1 downto 0);
    ttInbound_i : in  std_logic_vector( 1 downto 0);
    dstIdInbound_i : in  std_logic_vector(31 downto 0);
    srcIdInbound_i : in  std_logic_vector(31 downto 0);                  
    sizeInbound_i : in  std_logic_vector( 3 downto 0);                  
    statusInbound_i : in  std_logic_vector( 3 downto 0);
    tidInbound_i : in  std_logic_vector( 7 downto 0);
    hopInbound_i : in  std_logic_vector( 7 downto 0);
    offsetInbound_i : in  std_logic_vector(20 downto 0);
    wdptrInbound_i : in  std_logic;
    payloadLengthInbound_i : in  std_logic_vector( 2 downto 0);
    payloadIndexInbound_o : out std_logic_vector( 2 downto 0);                  
    payloadInbound_i : in  std_logic_vector(63 downto 0);
    doneInbound_o : out std_logic;

    -- Signal interconnect for MaintenanceOutbound   --
    readRequestReadyOutbound_o : out std_logic;
    writeRequestReadyOutbound_o : out std_logic;
    readResponseReadyOutbound_o : out std_logic;
    writeResponseReadyOutbound_o : out std_logic;
    portWriteReadyOutbound_o : out std_logic;
    vcOutbound_o : out std_logic;
    crfOutbound_o : out std_logic;
    prioOutbound_o : out std_logic_vector( 1 downto 0);
    ttOutbound_o : out std_logic_vector( 1 downto 0);
    dstIdOutbound_o : out std_logic_vector(31 downto 0);
    srcIdOutbound_o : out std_logic_vector(31 downto 0);
    sizeOutbound_o : out std_logic_vector( 3 downto 0);
    statusOutbound_o : out std_logic_vector( 3 downto 0);
    tidOutbound_o : out std_logic_vector( 7 downto 0);
    hopOutbound_o : out std_logic_vector( 7 downto 0);
    offsetOutbound_o : out std_logic_vector(20 downto 0);
    wdptrOutbound_o : out std_logic;
    payloadLengthOutbound_o : out std_logic_vector( 2 downto 0);
    payloadIndexOutbound_i : in  std_logic_vector( 2 downto 0);
    payloadOutbound_o : out std_logic_vector(63 downto 0);
    doneOutbound_i : in  std_logic;

    -- Signal interconnect for RioLogicalMaintenance --
    readRequestReadyMnt_o : out std_logic;
    writeRequestReadyMnt_o : out std_logic;
    sizeMnt_o : out std_logic_vector( 3 downto 0);
    offsetMnt_o : out std_logic_vector(20 downto 0);
    wdptrMnt_o : out std_logic;
    payloadLengthMnt_o : out std_logic_vector( 2 downto 0);
    payloadIndexMnt_i : in  std_logic_vector( 2 downto 0);
    payloadMnt_o : out std_logic_vector(63 downto 0);
    doneMnt_i : in  std_logic;
    readResponseReadyMnt_i : in  std_logic;
    writeResponseReadyMnt_i : in  std_logic;
    statusMnt_i : in  std_logic_vector( 3 downto 0);
    payloadLengthMnt_i : in  std_logic_vector( 2 downto 0);
    payloadIndexMnt_o : out std_logic_vector( 2 downto 0);
    payloadMnt_i : in  std_logic_vector(63 downto 0);
    doneMnt_o : out std_logic;

    -- Port control / Arbiter
    inboundFramePort_i : in  std_logic_vector( 7 downto 0);
    outboundFramePort_o : out std_logic_vector( 7 downto 0);
    lookupData_i : in  std_logic_vector( 7 downto 0);
    lookupAddr_o : out std_logic_vector(15 downto 0);
    lookupStb_o : out std_logic;
    lookupAck_i : in  std_logic;

    -- Various signals for the Error Reporting Port-Write Packet Data Payload/HotSwap event --
    hotSwapEvent_i : in  std_logic_vector(SWITCH_PORTS-1 downto 0);
    dev16_deviceID_msb : in  std_logic_vector( 7 downto 0); 
    dev8_deviceID : in  std_logic_vector( 7 downto 0); 
    portWriteTransmissionDisable_i : in  std_logic;                                       -- Normal='0', if '1' no error-reporting port-write event message is sent.
    componentTag_CSR_i : in  std_logic_vector(31 downto 0);                   -- CSR is sent with Error Reporting port-write event message.
    portNwriteDisabled_i : in  std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0'); -- from lp-serial: Port N Error and Status CSR (standard:bit 26), if set -no port write event will be generated for the port.
    portNwritePending_i : in  std_logic_vector(SWITCH_PORTS-1 downto 0);       -- If set, the port-write has not been acknowledged and the packet shall restransmitt until ack.
    setPortNwritePending_o : out std_logic_vector(SWITCH_PORTS-1 downto 0);       -- Commands to set Port-Write Pending Status Bit of Port-n-Error and Status CSR
    portNindex_o : out integer range 0 to SWITCH_PORTS;                 -- Index selector for PortNerrorDetect_CSR_i
    portNerrorDetect_CSR_i : in  std_logic_vector(31 downto 0);                   -- CSR is sent with Error Reporting port-write event message.
    logicalTransportLayerErrorDetectCSR_i : in  std_logic_vector(31 downto 0) := (others=>'0')   -- CSR is sent with Error Reporting port-write event message.
    );
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioSwitchMaintenancePortManager
-------------------------------------------------------------------------------
architecture rtl of RioSwitchMaintenancePortManager is

--constant tt_Dev8               : std_logic_vector( 1 downto 0) := b"00";  --tt field definition for  8 bit address mode
  constant tt_Dev16              : std_logic_vector( 1 downto 0) := b"01";  --tt field definition for 16 bit address mode
--constant tt_Dev32              : std_logic_vector( 1 downto 0) := b"10";  --tt field definition for 32 bit address mode
  
  signal  tt_pwError            : std_logic_vector( 1 downto 0);
  constant  prio_pwError          : std_logic_vector( 1 downto 0) := b"00";
  constant  tid_pwError           : std_logic_vector( 7 downto 0) := x"00";
  constant  hop_pwError           : std_logic_vector( 7 downto 0) := x"FF";
  constant  offset_pwError        : std_logic_vector(20 downto 0) := (others=>'0');  --offset is a reserved field for maintenance port-write packets
  signal  dstId_pwError         : std_logic_vector(31 downto 0) := (others=>'0');
  signal  srcId_pwError         : std_logic_vector(31 downto 0) := (others=>'0');
  constant  vc_pwError            : std_logic := '0';
  constant  crf_pwError           : std_logic := '0';
  constant  wdPtr_pwError         : std_logic := '1';                          -- /  max 16 byte in a write transfer frame
  constant  size_pwError          : std_logic_vector( 3 downto 0) := b"1011";  -- \  max 16 byte in a write transfer frame
  constant  payloadLength_pwError : std_logic_vector( 2 downto 0) := o"2";
  signal  payload_pwError       : std_logic_vector(63 downto 0);
  constant  implSpecificBits      : std_logic_vector(23 downto 0) := x"000000";  -- Implementation Specific Bits of the Error Reporting port-write packet data payload
  
  type    hotSwapMessageSequencer_FSM_type is (test, send);
  signal  hotSwapMessageSequencer_FSM : hotSwapMessageSequencer_FSM_type := test;
  
  signal  PortNindex            : integer range 0 to SWITCH_PORTS := 0;
  signal  portNwriteTimeout     : std_logic_vector(SWITCH_PORTS-1 downto 0);
  signal  portNwriteTimeoutWD   : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
  signal  hotSwapEvent_sticky   : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
  signal  clearHotSwapEvent     : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
  signal  setPortNwritePending  : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
  signal     PortNwritePending_D: std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
  signal  hotSwapEventPending   : std_logic := '0';
  signal  lookupAddr            : std_logic_vector(lookupAddr_o'range) := (others=>'0');
  signal  HotSwap_event         : std_logic := '0';
  signal  ImpSpecificAndPortId  : std_logic_vector(31 downto 0) := (others=>'0');

  signal  sendPacket            : std_logic;
  signal  forwardPacket         : std_logic;

  signal  readRequestInbound    : std_logic;
  signal  writeRequestInbound   : std_logic;
  signal  readResponseInbound   : std_logic;
  signal  writeResponseInbound  : std_logic;
  signal  doneInbound           : std_logic;
  signal  portWriteInbound      : std_logic;
  signal  outboundFramePort     : std_logic_vector( 7 downto 0);
  
begin

  --process(Dev8_or_16) --, Dev32_PW) --select tt-bits for HotSwap-message, based on Dev8/16/32 config.bits
  --begin
  --   if Dev32_PW='1' then
  --      tt_pwError<=tt_Dev32;
  --   els
  --      if Dev8_or_16='1' then
  tt_pwError<=tt_Dev16;
  --   else
  --      tt_pwError<=tt_Dev8;
  --   end if;
  --end process;

  process(clk, areset_n)  --monitor the hotSwapEvent bits and make them "sticky"
  begin
    if areset_n='0' then
      hotSwapEvent_sticky<=(others=>'0');
      PortNwritePending_D <= (others=>'0');
    --
    elsif rising_edge(clk) then
      PortNwritePending_D <= PortNwritePending_i;
      --
      for portIndex in 0 to SWITCH_PORTS-1 loop
        if portNwriteDisabled_i(portIndex)='1' or PortWriteTransmissionDisable_i='1' then
          hotSwapEvent_sticky(portIndex)<='0';
        elsif hotSwapEvent_i(portIndex)='1' then
          hotSwapEvent_sticky(portIndex)<='1';
        elsif PortNwritePending_D(portIndex)='1' and PortNwritePending_i(portIndex)='0' then      --clear on '1' to '0' transition
          hotSwapEvent_sticky(portIndex)<='0';
        end if;
      end loop;
    --
    end if;
  end process;
  
-- 
  hotSwapMessageSequencer:process(clk, areset_n)
  -- check for pending hot swap events
  begin
    if areset_n='0' then
      hotSwapMessageSequencer_FSM <= test;
      hotSwapEventPending <= '0';
      PortNindex <= 0;
      portNwriteTimeoutWD <= (others=>'0');
    --
    elsif rising_edge(clk) then
      case hotSwapMessageSequencer_FSM is
        when test   => if PortNindex>=SWITCH_PORTS or PortWriteTransmissionDisable_i='1' then
                         --all ports have been evaluated, restart.
                         PortNindex <= 0;
                         hotSwapEventPending <= '0';
                         portNwriteTimeoutWD  <= (others=>'0');
                         hotSwapMessageSequencer_FSM <= test;
                       elsif hotSwapEvent_sticky(PortNindex)='1' and portNwriteDisabled_i(PortNindex)='0' and
                         (PortNwritePending_D(PortNindex)='0' or (PortNwritePending_D(PortNindex)='1' and portNwriteTimeout(PortNindex)='1')) then
                         -- Clearing the hotSwapEvent_sticky bit takes one clk from write to PortNwritePending, use a delayed PortNwritePending so that they match in time.
                         -- Send error event. (skip this transmission if: Pending is set and Timeout has not expired)
                         hotSwapMessageSequencer_FSM <= send;
                         portNwriteTimeoutWD(PortNindex) <= '1';
                         hotSwapEventPending <= '1';
                       else
                         PortNindex <= PortNindex+1;
                         hotSwapEventPending <= '0';
                         hotSwapMessageSequencer_FSM <= test;
                       end if;
                       
        when send   => ImpSpecificAndPortId <= implSpecificBits & std_logic_vector(to_unsigned(PortNindex,8));
                       --
                       if doneInbound='1' and HotSwap_event = '1' then
                         portNwriteTimeoutWD<=(others=>'0');
                         hotSwapMessageSequencer_FSM<=test;
                         hotSwapEventPending <= '0';
                         PortNindex<=PortNindex+1;
                       end if;
                       
        when others => hotSwapMessageSequencer_FSM<=test;
      end case;
    end if;
  end process;
  
  -- Instantiation of port-write timers. 
  -- The timer supervises when a port-write has been acknowledged and resends
  -- the port-write if the acknowledge has taken a too long time to receive.
  portWriteTimerN: for portIndex in 0 to SWITCH_PORTS-1 generate
    portWriteTimer: block
      port(
        clk : in std_logic;
        areset_n : in std_logic;
        clear_i : in std_logic;
        timeout_o : out std_logic);

      port map(clk=>clk,
               areset_n=>areset_n,
               clear_i=>portNwriteTimeoutWD(portIndex),
               timeout_o=>portNwriteTimeout(portIndex));

      signal counter : natural range 0 to PORT_WRITE_TIMEOUT_VALUE := PORT_WRITE_TIMEOUT_VALUE;
    begin

      process(clk, areset_n)
      begin
        if (areset_n = '0') then
          counter <= PORT_WRITE_TIMEOUT_VALUE;
          timeout_o <= '0';
        elsif (rising_edge(clk)) then
          if (clear_i = '1') and (PORT_WRITE_TIMEOUT_VALUE /= 0) then
            counter <= PORT_WRITE_TIMEOUT_VALUE;
            timeout_o <= '0';
          else
            if (counter /= 0) then
              counter <= counter - 1;
            else
              timeout_o <= '1';
            end if;
          end if;
        end if;
      end process;

    end block;
  end generate;
  
  -----------------------------------------------------------------------------
  -- Main switch maintenance controller.
  -- This controller decides when to forward packets and when to consume and
  -- produce responses instead.
  -- It also determines when portWrite-packets are allowed to be sent.
  -----------------------------------------------------------------------------
  RioSwitchMaintenance: process(clk, areset_n)
    type MasterStateType is (STATE_IDLE,
                             STATE_START_PORT_LOOKUP,
                             STATE_WAIT_PORT_LOOKUP,
                             STATE_WAIT_COMPLETE);
    variable masterState : MasterStateType;
  begin
    if areset_n = '0' then
      masterState := STATE_IDLE;
      
      sendPacket <= '0';
      forwardPacket <= '0';
      outboundFramePort <= (others=>'0');
      
      lookupStb_o  <= '0';
      lookupAddr   <= (others => '0');
      HotSwap_event<= '0';
      setPortNwritePending<=(others=>'0');         
      
      srcId_pwError<=(others=>'0');
      dstId_pwError<=(others=>'0');
      
    elsif rising_edge(clk) then
      case masterState is
        
        when STATE_IDLE =>
          ---------------------------------------------------------------------
          -- Wait for frame to be available.
          ---------------------------------------------------------------------
          -- REMARK: Discard erronous frames.
          sendPacket <= '0';
          setPortNwritePending<=(others=>'0');
          if (readRequestInbound = '1' or writeRequestInbound = '1') and hopInbound_i = x"00" then 
            masterState := STATE_WAIT_COMPLETE;
            forwardPacket <= '0';
            outboundFramePort <= inboundFramePort_i;
            HotSwap_event <= '0';
          elsif readResponseInbound  = '1' or (readRequestInbound  = '1' and hopInbound_i /= x"00") or
            writeResponseInbound = '1' or (writeRequestInbound = '1' and hopInbound_i /= x"00") or
            portWriteInbound     = '1' then
            masterState := STATE_START_PORT_LOOKUP;
            forwardPacket <= '1';
            HotSwap_event <= '0';
          elsif hotSwapEventPending='1' and PortWriteTransmissionDisable_i='0' then
            HotSwap_event <= '1';  -- Send the HotSwap Event as defined by hotSwapMessageSequencer, unless port-writes are explicitly disabled..!
            forwardPacket <= '0';
            masterState := STATE_START_PORT_LOOKUP;
          else
            HotSwap_event <= '0';
            forwardPacket <= '0';
          end if;
          
        when STATE_START_PORT_LOOKUP =>
          ---------------------------------------------------------------------
          -- The destination port of the packet should be read from the routing
          -- table.
          ---------------------------------------------------------------------
          
          -- Initiate a port-lookup of the destination address.
          lookupStb_o <= '1';
          
          if HotSwap_event='1' then
            --Error management message
            --  if Dev32_PW='1' then  -- and Dev32_support bit set in Processing Element Features CAR???
            --     --use 32 bit addresses
            --     lookupAddr <= Dev32_deviceID(15 downto 0); --32 bit address is NOT yet supported by other parts of the switch, but if set..use lsb bits?
            --   --lookupAddr <= Dev32_deviceID;
            --  els
            --     if Dev8_or_16='1' then
            --use 16 bit addresses
            lookupAddr <= Dev16_deviceID_msb & Dev8_deviceID;
          --lookupAddr <= x"0000" & Dev16_deviceID_msb & Dev8_deviceID;
          --  else
          --     --use 8 bit addresses
          --     lookupAddr <= x"00" & Dev8_deviceID;
          --   --lookupAddr <= x"000000" & Dev8_deviceID;
          --  end if;
          else
            --"normal" access
            lookupAddr <= dstIdInbound_i(15 downto 0);
          end if;
          
          masterState := STATE_WAIT_PORT_LOOKUP;
          
        when STATE_WAIT_PORT_LOOKUP =>
          ---------------------------------------------------------------------
          -- Wait for the destination port lookup to complete.
          ---------------------------------------------------------------------
          
          if lookupAck_i = '1' then
            -- The address lookup is complete.
            
            -- Terminate the lookup cycle.
            lookupStb_o <= '0';
            
            -- Wait for the target port to reply.
            outboundFramePort <= lookupData_i;
            masterState := STATE_WAIT_COMPLETE;
            
            dstId_pwError(lookupAddr'range) <= lookupAddr; --not defined?
            srcId_pwError <= (others=>'0'); --not defined or require reverse lookup..
            
          else
          -- Wait until the address lookup is complete.
          -- REMARK: Timeout here???
          end if;
          
        when STATE_WAIT_COMPLETE =>
          ---------------------------------------------------------------------
          -- Indicate that the packet can be sent and wait for it to be
          -- transmitted.
          ---------------------------------------------------------------------
          sendPacket <= '1';
          if doneInbound = '1' then
            masterState := STATE_IDLE;
            HotSwap_event<='0';
            if HotSwap_event='1' then
              setPortNwritePending(PortNindex)<='1';
            end if;
          end if;
          
        when others =>
      ---------------------------------------------------------------------
      -- 
      ---------------------------------------------------------------------
      end case;
    end if;
  end process;

  
  lookupAddr_o <= lookupAddr;
  
  readRequestInbound  <=  readRequestReadyInbound_i  when HotSwap_event = '0' else '0';
  writeRequestInbound  <= writeRequestReadyInbound_i  when HotSwap_event = '0' else '0';
  readResponseInbound <=  readResponseReadyInbound_i when HotSwap_event = '0' else '0';
  writeResponseInbound <= writeResponseReadyInbound_i when HotSwap_event = '0' else '0';
  portWriteInbound     <= portWriteReadyInbound_i     when HotSwap_event = '0' else '0';
  
  sizeOutbound_o        <= size_pwError   when HotSwap_event='1' else sizeInbound_i;
  offsetOutbound_o      <= offset_pwError when HotSwap_event='1' else offsetInbound_i;
  wdptrOutbound_o       <= wdPtr_pwError  when HotSwap_event='1' else wdptrInbound_i;
  outboundFramePort_o   <= outboundFramePort;
  
  doneInbound_o         <= doneInbound;
  
  sizeMnt_o             <= sizeInbound_i;
  offsetMnt_o           <= offsetInbound_i; 
  wdptrMnt_o            <= wdptrInbound_i;
  payloadLengthMnt_o    <= payloadLengthInbound_i;
  payloadMnt_o          <= payloadInbound_i;
  doneMnt_o             <= doneOutbound_i;
  payloadIndexMnt_o     <= payloadIndexOutbound_i;
  
  PortNindex_o          <= PortNindex;

  readRequestReadyMnt_o <= '0'                                      when HotSwap_event = '1' else
                           readRequestInbound and sendPacket       when forwardPacket = '0' else '0';
  
  writeRequestReadyMnt_o<= '0'                                      when HotSwap_event = '1' else
                           writeRequestInbound and sendPacket       when forwardPacket = '0' else '0';
  
  payloadIndexInbound_o <= payloadIndexOutbound_i                   when forwardPacket = '1' else payloadIndexMnt_i;
  
  srcIdOutbound_o       <= srcId_pwError                            when HotSwap_event = '1' else 
                           srcIdInbound_i                           when forwardPacket = '1' else dstIdInbound_i;
  
  dstIdOutbound_o       <= dstId_pwError                            when HotSwap_event = '1' else
                           dstIdInbound_i                           when forwardPacket = '1' else srcIdInbound_i;
  
  statusOutbound_o      <= statusInbound_i                          when forwardPacket = '1' else statusMnt_i;
  hopOutbound_o       <= std_logic_vector(unsigned(hopInbound_i)-1) when forwardPacket = '1' else x"ff";
  
  doneInbound           <= doneOutbound_i                           when HotSwap_event = '1' else
                           doneOutbound_i                           when forwardPacket = '1' else doneMnt_i;
  
  readRequestReadyOutbound_o   <= '0'                               when HotSwap_event = '1' else
                                  readRequestInbound  and sendPacket      when forwardPacket = '1' else '0';
  
  writeRequestReadyOutbound_o  <= '0'                               when HotSwap_event = '1' else
                                  writeRequestInbound  and sendPacket      when forwardPacket = '1' else '0';
  
  readResponseReadyOutbound_o  <= '0'                               when HotSwap_event = '1' else
                                  readResponseInbound and sendPacket      when forwardPacket = '1' else readResponseReadyMnt_i;
  
  writeResponseReadyOutbound_o <= '0'                               when HotSwap_event = '1' else
                                  writeResponseInbound and sendPacket      when forwardPacket = '1' else writeResponseReadyMnt_i;
  
  portWriteReadyOutbound_o <=                       sendPacket      when HotSwap_event = '1' else
                                                    portWriteInbound  and sendPacket      when forwardPacket = '1' else '0';
  
  vcOutbound_o             <=   vc_pwError                          when HotSwap_event = '1' else vcInbound_i;
  crfOutbound_o            <=  crf_pwError                          when HotSwap_event = '1' else crfInbound_i;
  prioOutbound_o           <= prio_pwError                          when HotSwap_event = '1' else prioInbound_i;
  ttOutbound_o             <=   tt_pwError                          when HotSwap_event = '1' else ttInbound_i;
  tidOutbound_o            <=  tid_pwError                          when HotSwap_event = '1' else tidInbound_i;
  
  payloadLengthOutbound_o  <= payloadLength_pwError                 when HotSwap_event = '1' else
                              payloadLengthInbound_i                when forwardPacket = '1' else payloadLengthMnt_i;
  
  payloadOutbound_o        <= payload_pwError                       when HotSwap_event = '1' else
                              payloadInbound_i                      when forwardPacket = '1' else payloadMnt_i;
  
  payload_pwError  <= ComponentTag_CSR_i   & PortNerrorDetect_CSR_i when payloadIndexOutbound_i=b"000" else
                      ImpSpecificAndPortId & LogicalTransportLayerErrorDetectCSR_i;

  setPortNwritePending_o <= setPortNwritePending;
  
end architecture;
