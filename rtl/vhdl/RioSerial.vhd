-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing the transmission channel independent parts of the LP-Serial
-- Physical Layer Specification (RapidIO 2.2, part 6).
-- 
-- To Do:
-- -
-- 
-- Author(s): 
-- - Magnus Rosenius, magro732@opencores.org 
-- 
-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2013 Authors and OPENCORES.ORG 
-- 
-- This source file may be used and distributed without 
-- restriction provided that this copyright statement is not 
-- removed from the file and that any derivative work contains 
-- the original copyright notice and the associated disclaimer. 
-- 
-- This source file is free software; you can redistribute it 
-- and/or modify it under the terms of the GNU Lesser General 
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any 
-- later version. 
-- 
-- This source is distributed in the hope that it will be 
-- useful, but WITHOUT ANY WARRANTY; without even the implied 
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
-- PURPOSE. See the GNU Lesser General Public License for more 
-- details. 
-- 
-- You should have received a copy of the GNU Lesser General 
-- Public License along with this source; if not, download it 
-- from http://www.opencores.org/lgpl.shtml 
-- 
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- RioSerial
--
-- Generics
-- --------
-- TIMEOUT_WIDTH - The number of bits to be used in the portLinkTimeout signal.
--
-- Signals
-- -------
-- System signals.
-- clk - System clock.
-- areset_n - System reset. Asynchronous, active low. 
--
-- Configuration signals. These are used to change the runtime behaviour.
-- portLinkTimeout_i - The number of ticks to wait for a packet-accepted before
--   a timeout occurrs.
-- linkInitialized_o - Indicates if a link partner is answering with valid
--   status-control-symbols. 
-- inputPortEnable_i - Activate the input port for non-maintenance packets. If
--   deasserted, only non-maintenance packets are allowed.
-- outputPortEnable_i - Activate the output port for non-maintenance packets.
--   If deasserted, only non-maintenance packets are allowed.
--
-- This interface makes it possible to read and write ackId in both outbound
-- and inbound directions. All input signals are validated by localAckIdWrite.
-- localAckIdWrite_i - Indicate if a localAckId write operation is ongoing.
--   Usually this signal is high one tick. 
-- clrOutstandingAckId_i - Clear outstanding ackId, i.e. reset the transmission
--   window. The signal is only read if localAckIdWrite_i is high.
-- inboundAckId_i - The value to set the inbound ackId (the ackId that the
--  next inbound packet should have) to. This signal is only read if localAckIdWrite
--  is high.
-- outstandingAckId_i - The value to set the outstanding ackId (the ackId
--   transmitted but not acknowledged) to. This signal is only read if localAckIdWrite
--   is high.
-- outboundAckId_i - The value to set the outbound ackId (the ackId that the
--   next outbound packet will have) to. This signal is only read if localAckIdWrite
--   is high.
-- inboundAckId_o - The current inbound ackId.
-- outstandingAckId_o - The current outstanding ackId.
-- outboundAckId_o - The current outbound ackId.
--
-- This is the interface to the packet buffering sublayer. 
-- The window signals are used to send packets without removing them from the
-- memory storage. This way, many packet can be sent without awaiting
-- packet-accepted symbols and if a packet-accepted gets lost, it is possible
-- to revert and resend a packet. This is achived by reading readWindowEmpty
-- for new packet and asserting readWindowNext when a packet has been sent.
-- When the packet-accepted is received, readFrame should be asserted to remove the
-- packet from the storage. If a packet-accepted is missing, readWindowReset is
-- asserted to set the current packet to read to the one that has not received
-- a packet-accepted.
-- readFrameEmpty_i - Indicate if a packet is ready in the outbound direction.
--   Once deasserted, it is possible to read the packet content using
--   readContent_o to update readContentData and readContentEnd.
-- readFrame_o - Assert this signal for one tick to discard the oldest packet.
--   It should be used when a packet has been fully read, a linkpartner has
--   accepted it and the resources occupied by it should be returned to be
--   used for new packets.
-- readFrameRestart_o - Assert this signal to restart the reading of the
--   current packet. readContentData and readContentEnd will be reset to the
--   first content of the packet. 
-- readFrameAborted_i - This signal is asserted if the current packet was
--   aborted while it was written. It is used when a transmitter starts to send a
--   packet before it has been fully received and it is cancelled before it is
--   completed. A one tick asserted readFrameRestart signal resets this signal.
-- readWindowEmpty_i - Indicate if there are more packets to send.
-- readWindowReset_o - Reset the current packet to the oldest stored in the memory.
-- readWindowNext_o - Indicate that a new packet should be read. Must only be
--   asserted if readWindowEmpty is deasserted. It should be high for one tick.
-- readContentEmpty_i - Indicate if there are any packet content to be read.
--   This signal is updated directly when packet content is written making it
--   possible to read packet content before the full packet has been written to
--   the memory storage.
-- readContent_o - Update readContentData and readContentEnd.
-- readContentEnd_i - Indicate if the end of the current packet has been
--   reached. When asserted, readContentData is not valid.
-- readContentData_i - The content of the current packet.
-- writeFrameFull_i - Indicate if the inbound packet storage is ready to accept
--   a new packet.
-- writeFrame_o - Indicate that a new complete inbound packet has been written.
-- writeFrameAbort_o - Indicate that the current packet is aborted and that all
--   data written for this packet should be discarded. 
-- writeContent_o - Indicate that writeContentData is valid and should be
--   written into the packet content storage. 
-- writeContentData_o - The content to write to the packet content storage.
--
-- This is the interface to the PCS (Physical Control Sublayer). Four types of
-- symbols exist, idle, control, data and error.
-- Idle symbols are transmitted when nothing else can be transmitted. They are
-- mainly intended to enforce a timing on the transmitted symbols. This is
-- needed to be able to guarantee that a status-control-symbol is transmitted
-- at least once every 256 symbol.
-- Control symbols contain control-symbols as described by the standard.
-- Data symbols contains a 32-bit fragment of a RapidIO packet.
-- Error symbols indicate that a corrupted symbol was received. This could be
-- used by a PCS layer to indicate that a transmission error was detected and
-- that the above layers should send link-requests to ensure the synchronism
-- between the link-partners.
-- The signals in this interface are:
-- portInitialized_i - An asserted signal on this pin indicates that the PCS
--   layer has established synchronization with the link and is ready to accept
--   symbols. 
-- outboundSymbolEmpty_o - An asserted signal indicates that there are no
--   outbound symbols to read. Once deasserted, outboundSymbol_o will be
--   already be valid. This signal will be updated one tick after
--   outboundSymbolRead_i has been asserted.
-- outboundSymbolRead_i - Indicate that outboundSymbol_o has been read and a
--   new value could be accepted. It should be active for one tick. 
-- REMARK: Update this comment...
-- outboundSymbol_o - The outbound symbol. It is divided into two parts,
--   symbolType and symbolContent.
--   symbolType - The two MSB bits are the type of the symbol according to
--   table below:
--     00=IDLE, the rest of the bits are not used.
--     01=CONTROL, the control symbols payload (24 bits) are placed in the MSB
--       part of the symbolContent.
--     10=ERROR, the rest of the bits are not used.
--     11=DATA, all the remaining bits contain the number of valid words and
--     the payload of the symbol.
--   symbolContent - The rest of the bits are symbol content. If there are
--     multiple words in the symbols they must be set to zero. The first
--     received word is placed in the MSB part of this field.
-- inboundSymbolFull_o - An asserted signal indicates that no more inbound
--   symbols can be accepted.
-- inboundSymbolWrite_i - Indicate that inboundSymbol_i contains valid
--   information that should be forwarded. Should be active for one tick.
-- inboundSymbol_i - The inbound symbol. See outboundSymbol_o for formating.
-------------------------------------------------------------------------------
-- REMARK: Remove multi-symbol support to make the code more readable...
-- REMARK: Make more signals synchronous...
-- REMARK: Add support for the change of ackIds...
-- REMARK: Update all comments...
-- REMARK: Make sure that RioPacketBuffer can handle simultanous accesses at
-- the window-pins and the normal-pins... Add another singlememory...
-- REMARK: Make the pipe-line stages into block-statements to make the code more readable...
-- REMARK: Be consistent, tx->outbound and rx->inbound or egress/ingress...
-- REMARK: Add enable to fifo:s...

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for RioSerial.
-------------------------------------------------------------------------------
entity RioSerial is
  generic(
    TIMEOUT_WIDTH : natural;
    SYMBOL_COUNTER_WIDTH : natural := 8;
    TICKS_SEND_STATUS_STARTUP : natural := 15;
    TICKS_SEND_STATUS_OPERATIONAL : natural := 255);
  port(
    -- System signals.
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    -- Status signals for maintenance operations.
    portLinkTimeout_i : in std_logic_vector(TIMEOUT_WIDTH-1 downto 0);
    linkInitialized_o : out std_logic;
    inputPortEnable_i : in std_logic;
    outputPortEnable_i : in std_logic;

    -- Support for portLocalAckIdCSR.
    localAckIdWrite_i : in std_logic;
    clrOutstandingAckId_i : in std_logic;
    inboundAckId_i : in std_logic_vector(4 downto 0);
    outstandingAckId_i : in std_logic_vector(4 downto 0);
    outboundAckId_i : in std_logic_vector(4 downto 0);
    inboundAckId_o : out std_logic_vector(4 downto 0);
    outstandingAckId_o : out std_logic_vector(4 downto 0);
    outboundAckId_o : out std_logic_vector(4 downto 0);
    
    -- Outbound frame interface.
    readFrameEmpty_i : in std_logic;
    readFrame_o : out std_logic;
    readFrameRestart_o : out std_logic;
    readFrameAborted_i : in std_logic;
    readWindowEmpty_i : in std_logic;
    readWindowReset_o : out std_logic;
    readWindowNext_o : out std_logic;
    readContentEmpty_i : in std_logic;
    readContent_o : out std_logic;
    readContentEnd_i : in std_logic;
    readContentData_i : in std_logic_vector(31 downto 0);

    -- Inbound frame interface.
    writeFrameFull_i : in std_logic;
    writeFrame_o : out std_logic;
    writeFrameAbort_o : out std_logic;
    writeContent_o : out std_logic;
    writeContentData_o : out std_logic_vector(31 downto 0);

    -- PCS layer signals.
    portInitialized_i : in std_logic;
    outboundControlValid_o : out std_logic;
    outboundControlSymbol_o : out std_logic_vector(23 downto 0);
    outboundDataValid_o : out std_logic;
    outboundDataSymbol_o : out std_logic_vector(31 downto 0);
    inboundControlValid_i : in std_logic;
    inboundControlSymbol_i : in std_logic_vector(23 downto 0);
    inboundDataValid_i : in std_logic;
    inboundDataSymbol_i : in std_logic_vector(31 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioSerial.
-------------------------------------------------------------------------------
architecture RioSerialImpl of RioSerial is

  component RioFifo is
    generic(
      DEPTH_WIDTH : natural;
      DATA_WIDTH : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      empty_o : out std_logic;
      read_i : in std_logic;
      data_o : out std_logic_vector(DATA_WIDTH-1 downto 0);

      write_i : in std_logic;
      data_i : in std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;
  
  component RioTransmitter is
    generic(
      TIMEOUT_WIDTH : natural;
      NUMBER_SYMBOLS : natural range 1 to 1 := 1;
      SYMBOL_COUNTER_WIDTH : natural;
      TICKS_SEND_STATUS_STARTUP : natural;
      TICKS_SEND_STATUS_OPERATIONAL : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      portLinkTimeout_i : in std_logic_vector(TIMEOUT_WIDTH-1 downto 0);
      portEnable_i : in std_logic;

      localAckIdWrite_i : in std_logic;
      clrOutstandingAckId_i : in std_logic;
      outstandingAckId_i : in std_logic_vector(4 downto 0);
      outboundAckId_i : in std_logic_vector(4 downto 0);
      outstandingAckId_o : out std_logic_vector(4 downto 0);
      outboundAckId_o : out std_logic_vector(4 downto 0);
      
      portInitialized_i : in std_logic;
      outboundControlValid_o : out std_logic;
      outboundControlSymbol_o : out std_logic_vector(23 downto 0);
      outboundDataValid_o : out std_logic;
      outboundDataSymbol_o : out std_logic_vector(31 downto 0);

      txControlEmpty_i : in std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
      txControlSymbol_i : in std_logic_vector(12*NUMBER_SYMBOLS downto 0);
      txControlUpdate_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);

      rxControlEmpty_i : in std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
      rxControlSymbol_i : in std_logic_vector(12*NUMBER_SYMBOLS downto 0);
      rxControlUpdate_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);

      linkInitialized_o : out std_logic;
      linkInitialized_i : in std_logic;
      ackIdStatus_i : in std_logic_vector(4 downto 0);

      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readFrameRestart_o : out std_logic;
      readFrameAborted_i : in std_logic;
      readWindowEmpty_i : in std_logic;
      readWindowReset_o : out std_logic;
      readWindowNext_o : out std_logic;
      readContentEmpty_i : in std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(32*NUMBER_SYMBOLS-1 downto 0));
  end component;

  component RioReceiver is
    generic(
      NUMBER_SYMBOLS : natural range 1 to 1 := 1);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      portEnable_i : in std_logic;
      
      localAckIdWrite_i : in std_logic;
      inboundAckId_i : in std_logic_vector(4 downto 0);
      inboundAckId_o : out std_logic_vector(4 downto 0);
      
      portInitialized_i : in std_logic;
      inboundControlValid_i : in std_logic;
      inboundControlSymbol_i : in std_logic_vector(23 downto 0);
      inboundDataValid_i : in std_logic;
      inboundDataSymbol_i : in std_logic_vector(31 downto 0);

      txControlWrite_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
      txControlSymbol_o : out std_logic_vector(12*NUMBER_SYMBOLS downto 0);
      rxControlWrite_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
      rxControlSymbol_o : out std_logic_vector(12*NUMBER_SYMBOLS downto 0);

      ackIdStatus_o : out std_logic_vector(4 downto 0);
      linkInitialized_o : out std_logic;
      
      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(32*NUMBER_SYMBOLS-1 downto 0));
  end component;

  constant NUMBER_SYMBOLS : natural := 1;
  
  signal linkInitializedRx : std_logic;
  signal linkInitializedTx : std_logic;
  signal ackIdStatus : std_logic_vector(4 downto 0);
  
  signal txControlWrite : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal txControlWriteSymbol : std_logic_vector(12*NUMBER_SYMBOLS downto 0);
  signal txControlReadEmpty : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal txControlRead : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal txControlReadSymbol : std_logic_vector(12*NUMBER_SYMBOLS downto 0);

  signal rxControlWrite : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal rxControlWriteSymbol : std_logic_vector(12*NUMBER_SYMBOLS downto 0);
  signal rxControlReadEmpty : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal rxControlRead : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal rxControlReadSymbol : std_logic_vector(12*NUMBER_SYMBOLS downto 0);

begin

  linkInitialized_o <=
    '1' when ((linkInitializedRx = '1') and (linkInitializedTx = '1')) else '0';
  
  -----------------------------------------------------------------------------
  -- Serial layer modules.
  -----------------------------------------------------------------------------
  
  Transmitter: RioTransmitter
    generic map(
      TIMEOUT_WIDTH=>TIMEOUT_WIDTH,
      NUMBER_SYMBOLS=>NUMBER_SYMBOLS,
      SYMBOL_COUNTER_WIDTH=>SYMBOL_COUNTER_WIDTH,
      TICKS_SEND_STATUS_STARTUP=>TICKS_SEND_STATUS_STARTUP,
      TICKS_SEND_STATUS_OPERATIONAL=>TICKS_SEND_STATUS_OPERATIONAL)
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable,
      portLinkTimeout_i=>portLinkTimeout_i,
      portEnable_i=>outputPortEnable_i,
      localAckIdWrite_i=>localAckIdWrite_i, 
      clrOutstandingAckId_i=>clrOutstandingAckId_i, 
      outstandingAckId_i=>outstandingAckId_i, 
      outboundAckId_i=>outboundAckId_i, 
      outstandingAckId_o=>outstandingAckId_o, 
      outboundAckId_o=>outboundAckId_o, 
      portInitialized_i=>portInitialized_i,
      outboundControlValid_o=>outboundControlValid_o,
      outboundControlSymbol_o=>outboundControlSymbol_o,
      outboundDataValid_o=>outboundDataValid_o,
      outboundDataSymbol_o=>outboundDataSymbol_o,
      txControlEmpty_i=>txControlReadEmpty,
      txControlSymbol_i=>txControlReadSymbol,
      txControlUpdate_o=>txControlRead, 
      rxControlEmpty_i=>rxControlReadEmpty,
      rxControlSymbol_i=>rxControlReadSymbol,
      rxControlUpdate_o=>rxControlRead,
      linkInitialized_o=>linkInitializedTx,
      linkInitialized_i=>linkInitializedRx,
      ackIdStatus_i=>ackIdStatus, 
      readFrameEmpty_i=>readFrameEmpty_i,
      readFrame_o=>readFrame_o, 
      readFrameRestart_o=>readFrameRestart_o,
      readFrameAborted_i=>readFrameAborted_i,
      readWindowEmpty_i=>readWindowEmpty_i,
      readWindowReset_o=>readWindowReset_o,
      readWindowNext_o=>readWindowNext_o,
      readContentEmpty_i=>readContentEmpty_i,
      readContent_o=>readContent_o, 
      readContentEnd_i=>readContentEnd_i,
      readContentData_i=>readContentData_i(32*NUMBER_SYMBOLS-1 downto 0));

  SymbolFifo: for i in 0 to NUMBER_SYMBOLS-1 generate
    TxSymbolFifo: RioFifo
      generic map(DEPTH_WIDTH=>5, DATA_WIDTH=>13)
      port map(
        clk=>clk, areset_n=>areset_n,
        empty_o=>txControlReadEmpty(i),
        read_i=>txControlRead(i),
        data_o=>txControlReadSymbol(12*(i+1) downto 12*i),
        write_i=>txControlWrite(i),
        data_i=>txControlWriteSymbol(12*(i+1) downto 12*i));

    RxSymbolFifo: RioFifo
      generic map(DEPTH_WIDTH=>5, DATA_WIDTH=>13)
      port map(
        clk=>clk, areset_n=>areset_n,
        empty_o=>rxControlReadEmpty(i),
        read_i=>rxControlRead(i),
        data_o=>rxControlReadSymbol(12*(i+1) downto 12*i),
        write_i=>rxControlWrite(i), 
        data_i=>rxControlWriteSymbol(12*(i+1) downto 12*i));
  end generate;
    
  Receiver: RioReceiver
    generic map(NUMBER_SYMBOLS=>NUMBER_SYMBOLS)
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable,
      portEnable_i=>inputPortEnable_i,
      localAckIdWrite_i=>localAckIdWrite_i, 
      inboundAckId_i=>inboundAckId_i, 
      inboundAckId_o=>inboundAckId_o, 
      portInitialized_i=>portInitialized_i,
      inboundControlValid_i=>inboundControlValid_i,
      inboundControlSymbol_i=>inboundControlSymbol_i,
      inboundDataValid_i=>inboundDataValid_i,
      inboundDataSymbol_i=>inboundDataSymbol_i,
      txControlWrite_o=>txControlWrite,
      txControlSymbol_o=>txControlWriteSymbol, 
      rxControlWrite_o=>rxControlWrite,
      rxControlSymbol_o=>rxControlWriteSymbol, 
      ackIdStatus_o=>ackIdStatus, 
      linkInitialized_o=>linkInitializedRx,
      writeFrameFull_i=>writeFrameFull_i,
      writeFrame_o=>writeFrame_o,
      writeFrameAbort_o=>writeFrameAbort_o, 
      writeContent_o=>writeContent_o,
      writeContentData_o=>writeContentData_o(32*NUMBER_SYMBOLS-1 downto 0));
        
end architecture;



-------------------------------------------------------------------------------
-- RioTransmitter.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for RioTransmitter.
-------------------------------------------------------------------------------
entity RioTransmitter is
  generic(
    TIMEOUT_WIDTH : natural;
    NUMBER_SYMBOLS : natural range 1 to 1 := 1;
    SYMBOL_COUNTER_WIDTH : natural;
    TICKS_SEND_STATUS_STARTUP : natural;
    TICKS_SEND_STATUS_OPERATIONAL : natural);
  port(
    -- System signals.
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    -- Status signals used for maintenance.
    portLinkTimeout_i : in std_logic_vector(TIMEOUT_WIDTH-1 downto 0);
    portEnable_i : in std_logic;

    -- Support for localAckIdCSR.
    localAckIdWrite_i : in std_logic;
    clrOutstandingAckId_i : in std_logic;
    outstandingAckId_i : in std_logic_vector(4 downto 0);
    outboundAckId_i : in std_logic_vector(4 downto 0);
    outstandingAckId_o : out std_logic_vector(4 downto 0);
    outboundAckId_o : out std_logic_vector(4 downto 0);
    
    -- Port output interface.
    portInitialized_i : in std_logic;
    outboundControlValid_o : out std_logic;
    outboundControlSymbol_o : out std_logic_vector(23 downto 0);
    outboundDataValid_o : out std_logic;
    outboundDataSymbol_o : out std_logic_vector(31 downto 0);

    -- Control symbols aimed to the transmitter.
    txControlEmpty_i : in std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
    txControlSymbol_i : in std_logic_vector(12*NUMBER_SYMBOLS downto 0);
    txControlUpdate_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);

    -- Control symbols from the receiver to send.
    rxControlEmpty_i : in std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
    rxControlSymbol_i : in std_logic_vector(12*NUMBER_SYMBOLS downto 0);
    rxControlUpdate_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);

    -- Internal signalling from the receiver part.
    linkInitialized_o : out std_logic;
    linkInitialized_i : in std_logic;
    ackIdStatus_i : in std_logic_vector(4 downto 0);

    -- Frame buffer interface.
    readFrameEmpty_i : in std_logic;
    readFrame_o : out std_logic;
    readFrameRestart_o : out std_logic;
    readFrameAborted_i : in std_logic;
    readWindowEmpty_i : in std_logic;
    readWindowReset_o : out std_logic;
    readWindowNext_o : out std_logic;
    readContentEmpty_i : in std_logic;
    readContent_o : out std_logic;
    readContentEnd_i : in std_logic;
    readContentData_i : in std_logic_vector(32*NUMBER_SYMBOLS-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioTransmitter.
-------------------------------------------------------------------------------
architecture RioTransmitterImpl of RioTransmitter is

  component RioTransmitterCore is
    generic(
      SYMBOL_COUNTER_WIDTH : natural;
      TICKS_SEND_STATUS_STARTUP : natural;
      TICKS_SEND_STATUS_OPERATIONAL : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      portEnable_i : in std_logic;

      portInitialized_i : in std_logic;
      outboundControlValid_o : out std_logic;
      outboundControlSymbol_o : out std_logic_vector(23 downto 0);
      outboundDataValid_o : out std_logic;
      outboundDataSymbol_o : out std_logic_vector(31 downto 0);

      txControlEmpty_i : in std_logic;
      txControlSymbol_i : in std_logic_vector(12 downto 0);
      txControlUpdate_o : out std_logic;

      rxControlEmpty_i : in std_logic;
      rxControlSymbol_i : in std_logic_vector(12 downto 0);
      rxControlUpdate_o : out std_logic;

      linkInitialized_o : out std_logic;
      linkInitialized_i : in std_logic;
      ackIdStatus_i : in std_logic_vector(4 downto 0);

      timeSentSet_o : out std_logic;
      timeSentReset_o : out std_logic;
      timeSentExpired_i : in std_logic;

      operational_i : in std_logic;
      operational_o : out std_logic;
      ackId_i : in std_logic_vector(4 downto 0);
      ackId_o : out std_logic_vector(4 downto 0);
      bufferStatus_i : in std_logic_vector(4 downto 0);
      bufferStatus_o : out std_logic_vector(4 downto 0);
      statusReceived_i : in std_logic;
      statusReceived_o : out std_logic;
      numberSentLinkRequests_i : in std_logic_vector(1 downto 0);
      numberSentLinkRequests_o : out std_logic_vector(1 downto 0);
      outputErrorStopped_i : in std_logic;
      outputErrorStopped_o : out std_logic;
      fatalError_i : in std_logic;
      fatalError_o : out std_logic;
      recoverActive_i : in std_logic;
      recoverActive_o : out std_logic;
      recoverCounter_i : in std_logic_vector(4 downto 0);
      recoverCounter_o : out std_logic_vector(4 downto 0);
      ackIdWindow_i : in std_logic_vector(4 downto 0);
      ackIdWindow_o : out std_logic_vector(4 downto 0);
      frameState_i : in std_logic_vector(3 downto 0);
      frameState_o : out std_logic_vector(3 downto 0);
      frameContent_i : in std_logic_vector(31 downto 0);
      frameContent_o : out std_logic_vector(31 downto 0);
      statusCounter_i : in std_logic_vector(3 downto 0);
      statusCounter_o : out std_logic_vector(3 downto 0);

      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readFrameRestart_o : out std_logic;
      readFrameAborted_i : in std_logic;
      readWindowEmpty_i : in std_logic;
      readWindowReset_o : out std_logic;
      readWindowNext_o : out std_logic;
      readContentEmpty_i : in std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0));
  end component;
  
  component MemorySimpleDualPortAsync is
    generic(
      ADDRESS_WIDTH : natural := 1;
      DATA_WIDTH : natural := 1;
      INIT_VALUE : std_logic := 'U');
    port(
      clkA_i : in std_logic;
      enableA_i : in std_logic;
      addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);

      addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;

  signal timeCurrent : std_logic_vector(TIMEOUT_WIDTH downto 0);
  signal timeSentElapsed : unsigned(TIMEOUT_WIDTH downto 0);
  signal timeSentDelta : unsigned(TIMEOUT_WIDTH downto 0);
  signal timeSentExpired : std_logic;
  signal timeSentSet : std_logic; 
  signal timeSentReset : std_logic; 

  signal timeSentEnable : std_logic;
  signal timeSentWriteAddress : std_logic_vector(4 downto 0); 
  signal timeSentReadAddress : std_logic_vector(4 downto 0);  
  signal timeSentReadData : std_logic_vector(TIMEOUT_WIDTH downto 0); 
  
  signal operationalCurrent, operationalNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal ackIdCurrent, ackIdNext : std_logic_vector(5*NUMBER_SYMBOLS-1 downto 0);
  signal bufferStatusCurrent, bufferStatusNext : std_logic_vector(5*NUMBER_SYMBOLS-1 downto 0);
  signal statusReceivedCurrent, statusReceivedNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal numberSentLinkRequestsCurrent, numberSentLinkRequestsNext : std_logic_vector(2*NUMBER_SYMBOLS-1 downto 0);
  signal outputErrorStoppedCurrent, outputErrorStoppedNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal fatalErrorCurrent, fatalErrorNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal recoverActiveCurrent, recoverActiveNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal recoverCounterCurrent, recoverCounterNext : std_logic_vector(5*NUMBER_SYMBOLS-1 downto 0);
  signal ackIdWindowCurrent, ackIdWindowNext : std_logic_vector(5*NUMBER_SYMBOLS-1 downto 0);
  signal frameStateCurrent, frameStateNext : std_logic_vector(4*NUMBER_SYMBOLS-1 downto 0);
  signal frameContentCurrent, frameContentNext : std_logic_vector(32*NUMBER_SYMBOLS-1 downto 0);
  signal statusCounterCurrent, statusCounterNext : std_logic_vector(4*NUMBER_SYMBOLS-1 downto 0);

  signal readFrame : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal readFrameRestart : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal readWindowReset : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal readWindowNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal readContent : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
begin

  -----------------------------------------------------------------------------
  -- Output generation to packet buffer.
  -----------------------------------------------------------------------------
  process(readFrame, readFrameRestart,
          readWindowReset, readWindowNext, readContent)
  begin
    readFrame_o <= '0';
    readFrameRestart_o <= '0';
    readWindowReset_o <= '0';
    readWindowNext_o <= '0';
    readContent_o <= '0';    
    for i in 0 to NUMBER_SYMBOLS-1 loop
      if (readFrame(i) = '1') then
        readFrame_o <= '1';
      end if;
        
      if (readFrameRestart(i) = '1') then
        readFrameRestart_o <= '1';
      end if;

      if (readWindowReset(i) = '1') then
        readWindowReset_o <= '1';
      end if;

      if (readWindowNext(i) = '1') then
        readWindowNext_o <= '1';
      end if;

      if (readContent(i) = '1') then
        readContent_o <= '1';
      end if;
    end loop;
  end process;

  -----------------------------------------------------------------------------
  -- Timeout logic.
  -----------------------------------------------------------------------------
  process(areset_n, clk)
  begin
    if (areset_n = '0') then
      timeSentElapsed <= (others=>'0');
      timeSentDelta <= (others=>'0');
      timeCurrent <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (timeSentEnable = '0') then
        timeSentElapsed <= unsigned(timeCurrent) - unsigned(timeSentReadData);
        timeSentDelta <= unsigned('0' & portLinkTimeout_i) - timeSentElapsed;
      else
        timeSentElapsed <= (others=>'0');
        timeSentDelta <= (others=>'0');
      end if;
      timeCurrent <= std_logic_vector(unsigned(timeCurrent) + 1);
    end if;
  end process;
  
  timeSentExpired <= timeSentDelta(TIMEOUT_WIDTH);
  
  timeSentEnable <= enable and (timeSentSet or timeSentReset);
  timeSentWriteAddress <= ackIdWindowCurrent when timeSentSet = '1' else
                          ackIdCurrent;
  timeSentReadAddress <= ackIdCurrent;
  
  TimeoutMemory: MemorySimpleDualPortAsync
    generic map(ADDRESS_WIDTH=>5, DATA_WIDTH=>TIMEOUT_WIDTH+1, INIT_VALUE=>'0')
    port map(
      clkA_i=>clk, enableA_i=>timeSentEnable,
      addressA_i=>timeSentWriteAddress, dataA_i=>timeCurrent,
      addressB_i=>timeSentReadAddress, dataB_o=>timeSentReadData);

  -----------------------------------------------------------------------------
  -- Protocol core and synchronization.
  -----------------------------------------------------------------------------
  process(areset_n, clk)
  begin
    if (areset_n = '0') then
      operationalCurrent <= (others=>'0');
      ackIdCurrent <= (others=>'0');
      bufferStatusCurrent <= (others=>'0');
      statusReceivedCurrent <= (others=>'0');
      numberSentLinkRequestsCurrent <= (others=>'0');
      outputErrorStoppedCurrent <= (others=>'0');
      fatalErrorCurrent <= (others=>'0');
      recoverActiveCurrent <= (others=>'0');
      recoverCounterCurrent <= (others=>'0');
      ackIdWindowCurrent <= (others=>'0');
      frameStateCurrent <= (others=>'0');
      frameContentCurrent <= (others=>'0');
      statusCounterCurrent <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        operationalCurrent <= operationalNext;
        ackIdCurrent <= ackIdNext;
        bufferStatusCurrent <= bufferStatusNext;
        statusReceivedCurrent <= statusReceivedNext;
        numberSentLinkRequestsCurrent <= numberSentLinkRequestsNext;
        outputErrorStoppedCurrent <= outputErrorStoppedNext;
        fatalErrorCurrent <= fatalErrorNext;
        recoverActiveCurrent <= recoverActiveNext;
        recoverCounterCurrent <= recoverCounterNext;
        ackIdWindowCurrent <= ackIdWindowNext;
        frameStateCurrent <= frameStateNext;
        frameContentCurrent <= frameContentNext;
        statusCounterCurrent <= statusCounterNext;
      end if;
    end if;
  end process;

  CoreGeneration: for i in 0 to NUMBER_SYMBOLS-1 generate
    TxCore: RioTransmitterCore 
      generic map(SYMBOL_COUNTER_WIDTH=>SYMBOL_COUNTER_WIDTH,
                  TICKS_SEND_STATUS_STARTUP=>TICKS_SEND_STATUS_STARTUP,
                  TICKS_SEND_STATUS_OPERATIONAL=>TICKS_SEND_STATUS_OPERATIONAL)
      port map(
        clk=>clk, areset_n=>areset_n, enable=>enable,
        portEnable_i=>portEnable_i, 
        portInitialized_i=>portInitialized_i, 
        outboundControlValid_o=>outboundControlValid_o,
        outboundControlSymbol_o=>outboundControlSymbol_o,
        outboundDataValid_o=>outboundDataValid_o,
        outboundDataSymbol_o=>outboundDataSymbol_o,
        txControlEmpty_i=>txControlEmpty_i(i), 
        txControlSymbol_i=>txControlSymbol_i(13*(i+1)-1 downto 13*i), 
        txControlUpdate_o=>txControlUpdate_o(i), 
        rxControlEmpty_i=>rxControlEmpty_i(i), 
        rxControlSymbol_i=>rxControlSymbol_i(13*(i+1)-1 downto 13*i), 
        rxControlUpdate_o=>rxControlUpdate_o(i), 
        linkInitialized_o=>linkInitialized_o, 
        linkInitialized_i=>linkInitialized_i, 
        ackIdStatus_i=>ackIdStatus_i, 
        timeSentSet_o=>timeSentSet, 
        timeSentReset_o=>timeSentReset,
        timeSentExpired_i=>timeSentExpired,
        operational_i=>operationalCurrent(i),
        operational_o=>operationalNext(i), 
        ackId_i=>ackIdCurrent(5*(i+1)-1 downto 5*i),
        ackId_o=>ackIdNext(5*(i+1)-1 downto 5*i), 
        bufferStatus_i=>bufferStatusCurrent(5*(i+1)-1 downto 5*i),
        bufferStatus_o=>bufferStatusNext(5*(i+1)-1 downto 5*i), 
        statusReceived_i=>statusReceivedCurrent(i),
        statusReceived_o=>statusReceivedNext(i), 
        numberSentLinkRequests_i=>numberSentLinkRequestsCurrent(2*(i+1)-1 downto 2*i),
        numberSentLinkRequests_o=>numberSentLinkRequestsNext(2*(i+1)-1 downto 2*i), 
        outputErrorStopped_i=>outputErrorStoppedCurrent(i),
        outputErrorStopped_o=>outputErrorStoppedNext(i), 
        fatalError_i=>fatalErrorCurrent(i),
        fatalError_o=>fatalErrorNext(i),
        recoverActive_i=>recoverActiveCurrent(i),
        recoverActive_o=>recoverActiveNext(i), 
        recoverCounter_i=>recoverCounterCurrent(5*(i+1)-1 downto 5*i),
        recoverCounter_o=>recoverCounterNext(5*(i+1)-1 downto 5*i), 
        ackIdWindow_i=>ackIdWindowCurrent(5*(i+1)-1 downto 5*i),
        ackIdWindow_o=>ackIdWindowNext(5*(i+1)-1 downto 5*i), 
        frameState_i=>frameStateCurrent(4*(i+1)-1 downto 4*i),
        frameState_o=>frameStateNext(4*(i+1)-1 downto 4*i), 
        frameContent_i=>frameContentCurrent(32*(i+1)-1 downto 32*i),
        frameContent_o=>frameContentNext(32*(i+1)-1 downto 32*i), 
        statusCounter_i=>statusCounterCurrent(4*(i+1)-1 downto 4*i),
        statusCounter_o=>statusCounterNext(4*(i+1)-1 downto 4*i), 
        readFrameEmpty_i=>readFrameEmpty_i, 
        readFrame_o=>readFrame(i), 
        readFrameRestart_o=>readFrameRestart(i), 
        readFrameAborted_i=>readFrameAborted_i, 
        readWindowEmpty_i=>readWindowEmpty_i, 
        readWindowReset_o=>readWindowReset(i), 
        readWindowNext_o=>readWindowNext(i), 
        readContentEmpty_i=>readContentEmpty_i, 
        readContent_o=>readContent(i), 
        readContentEnd_i=>readContentEnd_i, 
        readContentData_i=>readContentData_i);
    end generate;
    
end architecture;



-------------------------------------------------------------------------------
-- RioTransmitterCore
-- This module generate control- and data-symbols each clock tick. A
-- tick when nothing is generated it is assumed that an idle sequence is
-- generated.
--
-- TICKS_SEND_STATUS - The number of consequtive ticks sending nothing to wait
-- before a status-control-symbol is transmitted.
--
-- outboundControlValid_o='0' and outboundDataValid_o='0': nothing to send.
-- outboundControlValid_o='0' and outboundDataValid_o='1': outboundDataSymbol_o contains a data-symbol.
-- outboundControlValid_o='1' and outboundDataValid_o='0': outboundControlSymbol_o contains a control-symbol.
-- outboundControlValid_o='1' and outboundDataValid_o='1': an error has occurred.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioTransmitterCore.
-------------------------------------------------------------------------------
entity RioTransmitterCore is
  generic(
    SYMBOL_COUNTER_WIDTH : natural;
    TICKS_SEND_STATUS_STARTUP : natural;
    TICKS_SEND_STATUS_OPERATIONAL : natural);
  port(
    -- System signals.
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    -- Status signals used for maintenance.
    portEnable_i : in std_logic;

    -- Port output interface.
    portInitialized_i : in std_logic;
    outboundControlValid_o : out std_logic;
    outboundControlSymbol_o : out std_logic_vector(23 downto 0);
    outboundDataValid_o : out std_logic;
    outboundDataSymbol_o : out std_logic_vector(31 downto 0);

    -- Control symbols aimed to the transmitter.
    txControlEmpty_i : in std_logic;
    txControlSymbol_i : in std_logic_vector(12 downto 0);
    txControlUpdate_o : out std_logic;

    -- Control symbols from the receiver to send.
    rxControlEmpty_i : in std_logic;
    rxControlSymbol_i : in std_logic_vector(12 downto 0);
    rxControlUpdate_o : out std_logic;

    -- Internal signalling from the receiver part.
    linkInitialized_o : out std_logic;
    linkInitialized_i : in std_logic;
    ackIdStatus_i : in std_logic_vector(4 downto 0);

    -- Timeout signals.
    timeSentSet_o : out std_logic;
    timeSentReset_o : out std_logic;
    timeSentExpired_i : in std_logic;
    
    -- Internal core variables for cascading.
    operational_i : in std_logic;
    operational_o : out std_logic;
    ackId_i : in std_logic_vector(4 downto 0);
    ackId_o : out std_logic_vector(4 downto 0);
    bufferStatus_i : in std_logic_vector(4 downto 0);
    bufferStatus_o : out std_logic_vector(4 downto 0);
    statusReceived_i : in std_logic;
    statusReceived_o : out std_logic;
    numberSentLinkRequests_i : in std_logic_vector(1 downto 0);
    numberSentLinkRequests_o : out std_logic_vector(1 downto 0);
    outputErrorStopped_i : in std_logic;
    outputErrorStopped_o : out std_logic;
    fatalError_i : in std_logic;
    fatalError_o : out std_logic;
    recoverActive_i : in std_logic;
    recoverActive_o : out std_logic;
    recoverCounter_i : in std_logic_vector(4 downto 0);
    recoverCounter_o : out std_logic_vector(4 downto 0);
    ackIdWindow_i : in std_logic_vector(4 downto 0);
    ackIdWindow_o : out std_logic_vector(4 downto 0);
    frameState_i : in std_logic_vector(3 downto 0);
    frameState_o : out std_logic_vector(3 downto 0);
    frameContent_i : in std_logic_vector(31 downto 0);
    frameContent_o : out std_logic_vector(31 downto 0);
    statusCounter_i : in std_logic_vector(3 downto 0);
    statusCounter_o : out std_logic_vector(3 downto 0);
    
    -- Frame buffer interface.
    readFrameEmpty_i : in std_logic;
    readFrame_o : out std_logic;
    readFrameRestart_o : out std_logic;
    readFrameAborted_i : in std_logic;
    readWindowEmpty_i : in std_logic;
    readWindowReset_o : out std_logic;
    readWindowNext_o : out std_logic;
    readContentEmpty_i : in std_logic;
    readContent_o : out std_logic;
    readContentEnd_i : in std_logic;
    readContentData_i : in std_logic_vector(31 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioTransmitterCore.
-------------------------------------------------------------------------------
architecture RioTransmitterCoreImpl of RioTransmitterCore is

  -- The number of statuses to transmit at link initialization before starting
  -- to send packets.
  constant NUMBER_STATUS_TRANSMIT : std_logic_vector(3 downto 0) := "1111";

  -- The number of retries to make when a link-request has not been answered.
  constant NUMBER_LINK_RESPONSE_RETRIES : std_logic_vector(1 downto 0) := "10";

  -- States for frame transmission.
  constant FRAME_IDLE : std_logic_vector(3 downto 0) := "0000";
  constant FRAME_BUFFER : std_logic_vector(3 downto 0) := "0001";
  constant FRAME_START : std_logic_vector(3 downto 0) := "0010";
  constant FRAME_FIRST : std_logic_vector(3 downto 0) := "0011";
  constant FRAME_MIDDLE : std_logic_vector(3 downto 0) := "0100";
  constant FRAME_INSERT_IDLE : std_logic_vector(3 downto 0) := "0101";
  constant FRAME_INSERT_WAIT : std_logic_vector(3 downto 0) := "0110";
  constant FRAME_LAST : std_logic_vector(3 downto 0) := "0111";
  constant FRAME_START_CONTINUE : std_logic_vector(3 downto 0) := "1000";
  constant FRAME_END : std_logic_vector(3 downto 0) := "1001";
  constant FRAME_DISCARD : std_logic_vector(3 downto 0) := "1010";

  -- CRC-5 calculation unit for control-symbol generation.
  component Crc5ITU is
    port(
      d_i : in  std_logic_vector(18 downto 0);
      crc_o : out std_logic_vector(4 downto 0));
  end component;

  -- Stage-1 signals.
  alias txControlStype0 : std_logic_vector(2 downto 0) is txControlSymbol_i(12 downto 10);
  alias txControlParameter0 : std_logic_vector(4 downto 0) is txControlSymbol_i(9 downto 5);
  alias txControlParameter1 : std_logic_vector(4 downto 0) is txControlSymbol_i(4 downto 0);
  signal txControlUpdateOut : std_logic;
  signal sendRestartFromRetry, sendRestartFromRetryOut : std_logic;
  signal sendLinkRequest, sendLinkRequestOut : std_logic;

  -- Stage-2 signals.
  signal readFrameOut : std_logic;
  signal readFrameRestartOut : std_logic;
  signal readWindowResetOut : std_logic;
  signal readWindowNextOut : std_logic;
  signal readContentOut : std_logic;
  signal discardFrameOut : std_logic;
  signal symbolControlRestartOut, symbolControlRestart : std_logic;
  signal symbolControlLinkRequestOut, symbolControlLinkRequest : std_logic;
  signal symbolControlStartOut, symbolControlStart : std_logic;
  signal symbolControlEndOut, symbolControlEnd : std_logic;
  signal symbolDataOut, symbolData : std_logic;
  signal symbolDataContentOut, symbolDataContent : std_logic_vector(31 downto 0);

  -- Stage-3 signals.
  alias rxControlStype0 : std_logic_vector(2 downto 0) is rxControlSymbol_i(12 downto 10);
  alias rxControlParameter0 : std_logic_vector(4 downto 0) is rxControlSymbol_i(9 downto 5);
  alias rxControlParameter1 : std_logic_vector(4 downto 0) is rxControlSymbol_i(4 downto 0);
  signal rxControlUpdateOut : std_logic;
  signal symbolControlStype1 : std_logic;
  signal controlValidOut, controlValid : std_logic;
  signal stype0Out, stype0 : std_logic_vector(2 downto 0);
  signal parameter0Out, parameter0 : std_logic_vector(4 downto 0);
  signal parameter1Out, parameter1 : std_logic_vector(4 downto 0);
  signal stype1 : std_logic_vector(2 downto 0);
  signal cmd : std_logic_vector(2 downto 0);
  signal dataValid : std_logic;
  signal dataContent : std_logic_vector(31 downto 0);

  -- Stage-4 signals.
  signal symbolCounter : std_logic_vector(SYMBOL_COUNTER_WIDTH-1 downto 0);
  signal sendStatusRequired : std_logic;
  signal controlContent : std_logic_vector(23 downto 0);
  signal crc5 : std_logic_vector(4 downto 0);


begin

  linkInitialized_o <= operational_i;
                       
  -----------------------------------------------------------------------------
  -- First pipeline stage.
  -- Receive control-symbols from our link-partner and supervise timeouts.
  -- Input: ackId, ackIdWindow, timeoutExpired
  -- Output: sendLinkRequest, sendRestartFromRetry, ackId
  -----------------------------------------------------------------------------
  -- REMARK: Update the comment above...

  txControlUpdate_o <= txControlUpdateOut and enable;
  
  process(outputErrorStopped_i, recoverActive_i, recoverCounter_i,
          ackId_i, ackIdWindow_i, bufferStatus_i, statusReceived_i,
          numberSentLinkRequests_i,
          operational_i,
          txControlEmpty_i, txControlStype0,
          txControlParameter0, txControlParameter1,
          timeSentExpired_i,
          fatalError_i)
  begin
    outputErrorStopped_o <= outputErrorStopped_i;
    fatalError_o <= fatalError_i;
    recoverActive_o <= recoverActive_i;
    recoverCounter_o <= recoverCounter_i;
    ackId_o <= ackId_i;
    bufferStatus_o <= bufferStatus_i;
    statusReceived_o <= statusReceived_i;
    numberSentLinkRequests_o <= numberSentLinkRequests_i;

    timeSentReset_o <= '0';
    txControlUpdateOut <= '0';
    readFrameOut <= '0';
    
    sendRestartFromRetryOut <= '0';
    sendLinkRequestOut <= '0';

    if (fatalError_i = '1') then
      outputErrorStopped_o <= '0';
      fatalError_o <= '0';
    elsif (recoverActive_i = '1') then
      if (ackId_i /= recoverCounter_i) then
        ackId_o <= std_logic_vector(unsigned(ackId_i) + 1);
        readFrameOut <= '1';
      else
        recoverActive_o <= '0';
        outputErrorStopped_o <= '0';
      end if;
    else
      if (operational_i = '0') then
        -- Not operational mode.

        -- Check if any new symbol has been received from the link-partner.
        if (txControlEmpty_i = '0') then
          -- New symbol from link-partner.

          -- Check if the symbol is a status-control-symbol.
          if (txControlStype0 = STYPE0_STATUS) then
            -- A status-control symbol has been received.
            -- Update variables from the input status control symbol.
            ackId_o <= txControlParameter0;
            bufferStatus_o <= txControlParameter1;
            outputErrorStopped_o <= '0';
            statusReceived_o <= '1';
          else
            -- Discard all other received symbols in this state.
          end if;
          txControlUpdateOut <= '1';
        end if;
      else
        -- Operational mode.
        
        -- Make sure to reset the status received flag.
        statusReceived_o <= '0';

        -- Check if the oldest frame timeout has expired.
        if ((ackId_i /= ackIdWindow_i) and
            (timeSentExpired_i = '1')) then
          -- There has been a timeout on a transmitted frame.

          -- Reset the timeout to expire when the transmitted link-request has
          -- timed out instead.
          timeSentReset_o <= '1';

          -- Check if we are in the output-error-stopped state.
          if (outputErrorStopped_i = '1') then
            -- In the output-error-stopped state.
            
            -- Count the number of link-requests that has been sent and abort if
            -- there has been no reply for too many times.
            if (unsigned(numberSentLinkRequests_i) /= 0) then
              -- Not sent link-request too many times.
              -- Send another link-request.
              sendLinkRequestOut <= '1';
              numberSentLinkRequests_o <= std_logic_vector(unsigned(numberSentLinkRequests_i) - 1);
            else
              -- No response for too many times.
              -- Indicate that a fatal error has occurred.
              fatalError_o <= '1';
            end if;
          else
            -- Not in output-error-stopped and there is a timeout.
            -- Enter output-error-stopped state and send a link-request.
            sendLinkRequestOut <= '1';
            numberSentLinkRequests_o <= NUMBER_LINK_RESPONSE_RETRIES;
            outputErrorStopped_o <= '1';
          end if;
        else
          -- There has been no timeout.
          
          -- Check if any control symbol has been received from the link
          -- partner.
          if (txControlEmpty_i = '0') then
            -- A control symbol has been received.

            -- Check the received control symbol.
            case txControlStype0 is
              
              when STYPE0_STATUS =>
                if (outputErrorStopped_i = '0') then
                  -- Save the number of buffers in the link partner.
                  bufferStatus_o <= txControlParameter1;
                end if;
                
              when STYPE0_PACKET_ACCEPTED =>
                -- The link partner is accepting a frame.

                if (outputErrorStopped_i = '0') then
                  -- Save the number of buffers in the link partner.
                  bufferStatus_o <= txControlParameter1;
                  
                  -- Check if expecting this type of reply and that the ackId is
                  -- expected.
                  if ((ackId_i /= ackIdWindow_i) and
                      (ackId_i = txControlParameter0)) then
                    -- The packet-accepted is expected and the ackId is the expected.
                    -- The frame has been accepted by the link partner.
                    
                    -- Update to a new buffer and increment the ackId.
                    readFrameOut <= '1';
                    ackId_o <= std_logic_vector(unsigned(ackId_i) + 1);
                  else
                    -- Unexpected packet-accepted or packet-accepted for
                    -- unexpected ackId.
                    sendLinkRequestOut <= '1';
                    numberSentLinkRequests_o <= NUMBER_LINK_RESPONSE_RETRIES;
                    outputErrorStopped_o <= '1';
                  end if;
                end if;
                
              when STYPE0_PACKET_RETRY =>
                -- The link partner has asked for a frame retransmission.

                if (outputErrorStopped_i = '0') then
                  -- Save the number of buffers in the link partner.
                  bufferStatus_o <= txControlParameter1;

                  -- Check if the ackId is the one expected.
                  if (ackId_i = txControlParameter0) then
                    -- The ackId to retry is expected.
                    -- Go to the output-retry-stopped state.
                    -- Note that the output-retry-stopped state is equivalent
                    -- to sending a restart-from-retry.
                    sendRestartFromRetryOut <= '1';
                  else
                    -- Unexpected ackId to retry.
                    sendLinkRequestOut <= '1';
                    numberSentLinkRequests_o <= NUMBER_LINK_RESPONSE_RETRIES;
                    outputErrorStopped_o <= '1';
                  end if;
                end if;
                
              when STYPE0_PACKET_NOT_ACCEPTED =>
                if (outputErrorStopped_i = '0') then
                  -- Packet was rejected by the link-partner.
                  sendLinkRequestOut <= '1';
                  numberSentLinkRequests_o <= NUMBER_LINK_RESPONSE_RETRIES;
                  outputErrorStopped_o <= '1';
                end if;
                
              when STYPE0_LINK_RESPONSE =>
                if (outputErrorStopped_i = '1') then
                  -- Check if the link partner return value is acceptable.
                  if ((unsigned(txControlParameter0) - unsigned(ackId_i)) <=
                      (unsigned(ackIdWindow_i) - unsigned(ackId_i))) then
                    -- Recoverable error.
                    -- Use the received ackId and recover by removing packets
                    -- that has been received by the link-partner.
                    recoverCounter_o <= txControlParameter0;
                    recoverActive_o <= '1';
                  else
                    -- Totally out of sync.
                    -- Indicate that a fatal error has occurred.
                    fatalError_o <= '1';
                  end if;
                else
                  -- Dont expect or need a link-response in this state.
                  -- Discard it.
                end if;
                
              when STYPE0_VC_STATUS =>
                -- Not supported.
                -- Discard it.
                
              when STYPE0_RESERVED =>
                -- Not supported.
                -- Discard it.

              when STYPE0_IMPLEMENTATION_DEFINED =>
                -- Not supported.
                -- Discard it.
                
              when others =>
                null;
            end case;

            -- Indicate the control symbol has been processed.
            txControlUpdateOut <= '1';
          end if;
        end if;
      end if;
    end if;
  end process;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      readFrame_o <= '0';
      
      sendRestartFromRetry <= '0';
      sendLinkRequest <= '0';
    elsif (clk'event and clk = '1') then
      readFrame_o <= '0';
      
      if (enable = '1') then
        readFrame_o <= readFrameOut or discardFrameOut;
        
        sendRestartFromRetry <= sendRestartFromRetryOut;
        sendLinkRequest <= sendLinkRequestOut;
      end if;
    end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- Second pipeline stage.
  -- Create stype1-part of symbols and data symbols. Save the time when a
  -- packet was fully sent.
  -- Input:  sendRestartFromRetry, sendLinkRequest
  -- Output: ackIdWindow, frameState, timeout(0 to 31), 
  --         symbolControlStart, symbolControlEnd, symbolControlRestart,
  --         symbolControlLinkRequest, symbolData2, symbolData2Content.
  -----------------------------------------------------------------------------

  readFrameRestart_o <= readFrameRestartOut and enable;
  readWindowReset_o <= readWindowResetOut and enable;
  readWindowNext_o <= readWindowNextOut and enable;
  readContent_o <= readContentOut and enable;

  -- This process decide which stype1-part of a control symbols to send as well
  -- as all data symbols.
  process(readWindowEmpty_i, bufferStatus_i,
          readContentData_i, readContentEnd_i,
          recoverActive_i, ackId_i, operational_i, outputErrorStopped_i, portEnable_i,
          frameState_i, frameContent_i, 
          ackIdWindow_i,
          sendRestartFromRetry, sendLinkRequest,
          rxControlEmpty_i,          
          fatalError_i)
  begin
    readFrameRestartOut <= '0';
    readWindowResetOut <= '0';
    readWindowNextOut <= '0';
    readContentOut <= '0';

    frameState_o <= frameState_i;
    frameContent_o <= frameContent_i;
    ackIdWindow_o <= ackIdWindow_i;
    
    timeSentSet_o <= '0';

    symbolControlRestartOut <= '0';
    symbolControlLinkRequestOut <= '0';
    symbolControlStartOut <= '0';
    symbolControlEndOut <= '0';
    symbolDataOut <= '0';
    symbolDataContentOut <= (others => '0');

    discardFrameOut <= '0';

    -- Check if allowed to send anything.
    if ((fatalError_i = '1') or (recoverActive_i = '1') or
        (operational_i = '0')) then
      -----------------------------------------------------------------------
      -- This state is entered at startup or if any error has occurred.
      -- A port that is not initialized or in an error state should not
      -- transmit any packets.
      -----------------------------------------------------------------------
      
      -- Initialize framing before entering the operational state.
      readWindowResetOut <= '1';
      frameState_o <= FRAME_IDLE;
      ackIdWindow_o <= ackId_i;
    else
      -------------------------------------------------------------------
      -- This state is the operational state. It relays frames and handle
      -- flow control.
      -------------------------------------------------------------------
      
      if (sendRestartFromRetry = '1') then
        -- Required to send a restart-from-retry.
        
        -- Send a restart-from-retry control symbol to acknowledge the restart
        -- of the frame.
        symbolControlRestartOut <= '1';

        -- Make sure there wont be any timeout before the frame is
        -- starting to be retransmitted.
        timeSentSet_o <= '1';

        -- Restart the frame transmission.
        -- Since the transmission-window is reset, it is ok to go to the
        -- FRAME_IDLE-state even if the present state was FRAME_DISCARD since
        -- we will end up in the discard state anyway after the frames has
        -- been resent.
        ackIdWindow_o <= ackId_i;
        frameState_o <= FRAME_IDLE;
        readWindowResetOut <= '1';
      elsif (sendLinkRequest = '1') then
        -- Required to send a link-request.
        -- There is no need to restart the packet transmission since we do
        -- not yet know which packets that was successfully received by our
        -- link partner. Wait for the reply before anything is discarded.

        -- Send a link-request symbol.
        symbolControlLinkRequestOut <= '1';

        -- Write the current timer value.
        timeSentSet_o <= '1';
      elsif ((sendRestartFromRetry = '0') and (sendLinkRequest = '0')  and
             (outputErrorStopped_i = '0')) then
        -- No control-symbol is required to be sent to the link-partner.
        -- Proceed to send a pending packet.

        -- Check the current state of the frame transfer.
        case frameState_i is
          
          when FRAME_IDLE =>
            ---------------------------------------------------------------
            -- No frame has been started.
            ---------------------------------------------------------------

            -- Wait for a new frame to arrive from the frame buffer.
            if (readWindowEmpty_i = '0') then
              -- Update the output from the frame buffer to contain the
              -- data when it is read later.
              readContentOut <= '1';
              frameState_o <= FRAME_START;
            end if;

          when FRAME_START | FRAME_START_CONTINUE =>
            -------------------------------------------------------
            -- Check if we are allowed to transmit this packet.
            -------------------------------------------------------
            -- The packet may be not allowed, i.e. a non-maintenance
            -- sent when only maintenance is allowed. The link-partner can be
            -- busy, i.e. not having enough buffers to receive the new packet
            -- in or the number of outstanding packets may be too large.
            -- This state should result in a start-of-frame if possible or
            -- end-of-frame if a frame has not been ended properly yet.

            -- Check if the packet is allowed.
            if ((portEnable_i = '1') or
                (readContentData_i(19 downto 16) = FTYPE_MAINTENANCE_CLASS)) then
              -- Packet is allowed.

              -- Check if the link is able to accept the new frame.
              if ((bufferStatus_i /= "00000") and
                  ((unsigned(ackIdWindow_i)+1) /= unsigned(ackId_i))) then
                -- There are buffers ready to receive the new packet at the other
                -- side and there are ackIds left to tag it.
                -- The packet may be transmitted.

                -- Read the next packet content and buffer the current output.
                readContentOut <= '1';
                frameContent_o <= readContentData_i;
                
                -- Send a control symbol to start the packet and a status to
                -- complete the symbol.
                symbolControlStartOut <= '1';
                
                -- Proceed to send the first packet data symbol containing
                -- the ackId.
                frameState_o <= FRAME_FIRST;
              else
                -- The link cannot accept the packet.
                -- Wait in this state and dont do anything.
                if (frameState_i = FRAME_START_CONTINUE) then
                  symbolControlEndOut <= '1';
                end if;
                readFrameRestartOut <= '1';
                frameState_o <= FRAME_IDLE;
              end if;
            else
              -- The packet is not allowed.
              -- Discard it.
              if (frameState_i = FRAME_START_CONTINUE) then
                symbolControlEndOut <= '1';
              end if;
              frameState_o <= FRAME_DISCARD;
            end if;

          when FRAME_FIRST =>
            ---------------------------------------------------------------
            -- Send the first packet content containing our current
            -- ackId.
            ---------------------------------------------------------------

            -- Write a new data symbol and fill in our ackId on the
            -- packet.
            symbolDataOut <= '1';
            symbolDataContentOut <=
              std_logic_vector(ackIdWindow_i) & "0" & frameContent_i(25 downto 0);

            -- Read the next frame content and go to next state to send it.
            readContentOut <= '1';
            frameContent_o <= readContentData_i;
            frameState_o <= FRAME_MIDDLE;
            
          when FRAME_MIDDLE | FRAME_INSERT_WAIT =>
            ---------------------------------------------------------------
            -- The frame has not been fully sent.
            -- Send a data symbol until the last part of the packet is
            -- detected.
            ---------------------------------------------------------------
            
            -- Write a new data symbol.
            symbolDataOut <= '1';
            symbolDataContentOut <= frameContent_i;
            frameContent_o <= readContentData_i;

            -- Check if the packet is ending.
            if (readContentEnd_i = '1') then
              -- The packet is ending.
              readWindowNextOut <= '1';
              frameState_o <= FRAME_LAST;
            elsif ((frameState_i = FRAME_MIDDLE) and (rxControlEmpty_i = '0')) then
              -- There is a pending control-symbol from the receiver.
              -- This must only be entered if the previous state was a data
              -- symbol, not if an idle symbol was inserted.
              frameState_o <= FRAME_INSERT_IDLE;
            else
              -- The packet is not ending.
              readContentOut <= '1';
              frameState_o <= FRAME_MIDDLE;
            end if;

          when FRAME_INSERT_IDLE =>
            -----------------------------------------------------------------
            -- Dont send a data-symbol this tick to allow for a pending 
            -- control-symbol from the receiver to be inserted into the stream.
            -----------------------------------------------------------------

            symbolDataOut <= '0';
            readContentOut <= '1';
            frameState_o <= FRAME_INSERT_WAIT;
            
          when FRAME_LAST =>
            -----------------------------------------------------------------
            -- Sending the last data symbol of a packet.
            -- If there are pending packets, they can be started immediatly,
            -- otherwise, the current packet should be ended.
            -----------------------------------------------------------------

            -- Send the last data symbol of the packet.
            symbolDataOut <= '1';
            symbolDataContentOut <= frameContent_i;

            -- Check if there are pending packets.
            if (readWindowEmpty_i = '0') then
              -- Pending packet exist.
              readContentOut <= '1';
              frameState_o <= FRAME_START_CONTINUE;
            else
              -- No pending packets.
              frameState_o <= FRAME_END;
            end if;

            -- Update the window ackId.
            ackIdWindow_o <= std_logic_vector(unsigned(ackIdWindow_i) + 1);

            -- Start timeout supervision for the transmitted frame.
            timeSentSet_o <= '1';

          when FRAME_END =>
            -----------------------------------------------------------------
            -- There are no packets to directly follow the one that is ending.
            -- Mark the current packet as ended.
            -----------------------------------------------------------------

            -- Send a control symbol to end the frame. 
            symbolControlEndOut <= '1';
            frameState_o <= FRAME_IDLE;

          when FRAME_DISCARD =>
            ---------------------------------------------------------------
            -- The packet should be discarded.
            -- Hold any pending packets until there are no outstanding ones.
            ---------------------------------------------------------------

            -- Check that there are no outstanding packets.
            if(unsigned(ackIdWindow_i) = unsigned(ackId_i)) then
              -- No unacknowledged packets.
              -- It is now safe to remove the unallowed packet.
              discardFrameOut <= '1';

              -- Go back and send a new frame.
              frameState_o <= FRAME_IDLE;
            else
              -- Still outstanding packets.
              -- Dont do anything.
            end if;
            
          when others =>
            ---------------------------------------------------------------
            -- 
            ---------------------------------------------------------------
            null;
            
        end case;
      end if;
    end if;
  end process;  

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      symbolControlRestart <= '0';
      symbolControlLinkRequest <= '0';
      symbolControlStart <= '0';
      symbolControlEnd <= '0';
      symbolData <= '0';
      symbolDataContent <= (others => '0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        symbolControlRestart <= symbolControlRestartOut;
        symbolControlLinkRequest <= symbolControlLinkRequestOut;
        symbolControlStart <= symbolControlStartOut;
        symbolControlEnd <= symbolControlEndOut;
        symbolData <= symbolDataOut;
        symbolDataContent <= symbolDataContentOut;
      end if;
    end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- Third pipeline stage.
  -- Create the stype0 and stype1 part of a control symbol.
  -- This process makes sure that the buffer status are transmitted at least
  -- every 255 symbol.
  -- At startup it makes sure that at least 16 status symbols are transmitted
  -- before the operational-state is entered.
  -- Input:  symbolControlStart, symbolControlEnd, symbolControlRestart,
  --         symbolControlLinkRequest, symbolData, symbolDataContent
  -- Output: operational_o, 
  --         symbolControl, stype0, parameter0, parameter1, stype1, cmd
  -----------------------------------------------------------------------------

  rxControlUpdate_o <= rxControlUpdateOut and enable;
      
  symbolControlStype1 <=
    symbolControlRestart or symbolControlLinkRequest or
    symbolControlStart or symbolControlEnd;
  
  process(linkInitialized_i, ackIdStatus_i, portInitialized_i,
          operational_i, statusCounter_i, statusReceived_i,
          rxControlEmpty_i,
          sendStatusRequired,
          symbolControlStype1, symbolData,
          rxControlStype0, rxControlParameter0, rxControlParameter1,
          fatalError_i)
  begin
    operational_o <= operational_i;
    statusCounter_o <= statusCounter_i;

    rxControlUpdateOut <= '0';
    
    controlValidOut <= '0';
    stype0Out <= STYPE0_STATUS;
    parameter0Out <= ackIdStatus_i;
    parameter1Out <= "11111";
    
    if (fatalError_i = '1') then
      -- Reset when a fatal error occurrs.
      operational_o <= '0';
      statusCounter_o <= NUMBER_STATUS_TRANSMIT;
    else
      -- Check the operational state.
      if (operational_i = '0') then
        -----------------------------------------------------------------------
        -- This state is entered at startup. A port that is not initialized
        -- should only transmit idle sequences.
        -----------------------------------------------------------------------
        
        -- Check if the port is initialized.
        if (portInitialized_i = '1') then
          ---------------------------------------------------------------------
          -- The specification requires a status control symbol being sent at
          -- least every 1024 code word until an error-free status has been
          -- received. This implies that at most 256 idle sequences should be
          -- sent in between status control symbols. Once an error-free status 
          -- has been received, status symbols may be sent more rapidly. At
          -- least 15 statuses has to be transmitted once an error-free status
          -- has been received.
          ---------------------------------------------------------------------

          -- Check if we are ready to change state to operational.
          if ((linkInitialized_i = '1') and (unsigned(statusCounter_i) = 0)) then
            -- Receiver has received enough error free status symbols and we
            -- have transmitted enough.
            
            -- Considder ourselfs operational.
            operational_o <= '1';
          end if;
          
          -- Check if a status symbol should be transmitted.
          if (sendStatusRequired = '1') then
            -- A status symbol should be transmitted.
            
            -- Send a status control symbol to the link partner.
            controlValidOut <= '1';

            -- Check if the number of transmitted statuses should be updated.
            if (statusReceived_i = '1') and (unsigned(statusCounter_i) /= 0) then
              statusCounter_o <= std_logic_vector(unsigned(statusCounter_i) - 1);
            end if;
          end if;
        else
          -- The port is not initialized.
          -- Reset initialization variables.
          operational_o <= '0';
          statusCounter_o <= NUMBER_STATUS_TRANSMIT;
        end if;
      else
        ---------------------------------------------------------------------
        -- This is the operational state.
        -- It is entered once the link has been considdered up and running.
        ---------------------------------------------------------------------

        -- Check if the port is still initialized.
        if (portInitialized_i = '1') then
          -- The port is still initialized.

          -- Check if any dataSymbol is about to be sent.
          if (symbolData = '0') then
            -- No pending data symbol.

            -- Check if there is a pending control-symbol from the receiver.
            if (rxControlEmpty_i = '1') then
              -- No pending control-symbol from the receiver.

              -- Check if there is a pending control-symbol from ourselfs.
              if (symbolControlStype1 = '0') then
                -- No pending control-symbol from ourselfs.

                -- Check if we need to send a status-control-symbol.
                if (sendStatusRequired = '0') then
                  -- Not required to send a status-control-symbol.
                  -- Don't send anything.
                else
                  -- Required to send a status-control-symbol.
                  -- Indicate a status-control-symbol is ready.
                  controlValidOut <= '1';
                end if;
              else
                -- Pending control-symbol from ourselfs.
                -- Send it and reset the counter since a status-control-symbol
                -- will be combined with it.
                controlValidOut <= '1';
              end if;
            else
              -- Pending control-symbol from the receiver.
              -- If there is a pending control-symbol from ourselfs they will be
              -- combined.
              
              -- Remove the control-symbol from the fifo.
              rxControlUpdateOut <= '1';
              
              -- Send the receiver symbol.
              controlValidOut <= '1';
              stype0Out <= rxControlStype0;
              parameter0Out <= rxControlParameter0;
              parameter1Out <= rxControlParameter1;
            end if;
          else
            -- Pending data symbol.
            -- Send the data symbol.
          end if;
        else
          -- The port is not initialized anymore.
          -- Change the operational state.
          operational_o <= '0';
          statusCounter_o <= NUMBER_STATUS_TRANSMIT;
        end if;          
      end if;
    end if;
  end process;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      dataValid <= '0';
      dataContent <= (others=>'0');

      controlValid <= '0';
      stype0 <= (others=>'0');
      parameter0 <= (others=>'0');
      parameter1 <= (others=>'0');
      stype1 <= (others=>'0');
      cmd <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        dataValid <= symbolData;
        dataContent <= symbolDataContent;
        
        controlValid <= controlValidOut;
        stype0 <= stype0Out;
        parameter0 <= parameter0Out;
        parameter1 <= parameter1Out;
        if (symbolControlStart = '1') then
          stype1 <= STYPE1_START_OF_PACKET;
          cmd <= "000";
        elsif (symbolControlEnd = '1') then
          stype1 <= STYPE1_END_OF_PACKET;
          cmd <= "000";
        elsif (symbolControlRestart = '1') then
          stype1 <= STYPE1_RESTART_FROM_RETRY;
          cmd <= "000";
        elsif (symbolControlLinkRequest = '1') then
          stype1 <= STYPE1_LINK_REQUEST;
          cmd <= LINK_REQUEST_CMD_INPUT_STATUS;
        else
          stype1 <= STYPE1_NOP;
          cmd <= "000";
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Fourth pipeline stage.
  -- Make all symbols ready for transmission, i.e. calculate the CRC5 on
  -- control symbols and choose which symbol to send.
  -- Inputs: controlValid, stype0, parameter0, parameter1, stype1, cmd
  -----------------------------------------------------------------------------

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      sendStatusRequired <= '0';
      symbolCounter <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (portInitialized_i = '0') then
        sendStatusRequired <= '0';
        symbolCounter <=
          std_logic_vector(to_unsigned(TICKS_SEND_STATUS_STARTUP,
                                       SYMBOL_COUNTER_WIDTH));
      elsif (enable = '1') then
        if ((controlValid = '1') and
            ((stype0 = STYPE0_STATUS) or
             (stype0 = STYPE0_PACKET_ACCEPTED) or
             (stype0 = STYPE0_PACKET_RETRY))) then
          if (operational_i = '0') then
            symbolCounter <=
              std_logic_vector(to_unsigned(TICKS_SEND_STATUS_STARTUP,
                                           SYMBOL_COUNTER_WIDTH));
          else
            symbolCounter <= 
              std_logic_vector(to_unsigned(TICKS_SEND_STATUS_OPERATIONAL,
                                           SYMBOL_COUNTER_WIDTH));
          end if;
        else
          if (unsigned(symbolCounter) = 0) then
            sendStatusRequired <= '1';
            symbolCounter <= std_logic_vector(unsigned(symbolCounter) - 1);
          else
            sendStatusRequired <= '0';
            symbolCounter <= std_logic_vector(unsigned(symbolCounter) - 1);
          end if;
        end if;
      end if;
    end if;
  end process;
  
  controlContent(23 downto 21) <= stype0;
  controlContent(20 downto 16) <= parameter0;
  controlContent(15 downto 11) <= parameter1;
  controlContent(10 downto 8) <= stype1;
  controlContent(7 downto 5) <= cmd;
  controlContent(4 downto 0) <= crc5;

  Crc5Calculator: Crc5ITU 
    port map(
      d_i=>controlContent(23 downto 5), crc_o=>crc5);

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      outboundControlValid_o <= '0';
      outboundControlSymbol_o <= (others=>'0');
      outboundDataValid_o <= '0';
      outboundDataSymbol_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        outboundControlValid_o <= controlValid;
        outboundControlSymbol_o <= controlContent;
        outboundDataValid_o <= dataValid;
        outboundDataSymbol_o <= dataContent;
      end if;
    end if;
  end process;
  
end architecture;



-------------------------------------------------------------------------------
-- RioReciever.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for RioReceiver.
-------------------------------------------------------------------------------
entity RioReceiver is
  generic(
    NUMBER_SYMBOLS : natural range 1 to 1 := 1);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;
    
    portEnable_i : in std_logic;
    
    localAckIdWrite_i : in std_logic;
    inboundAckId_i : in std_logic_vector(4 downto 0);
    inboundAckId_o : out std_logic_vector(4 downto 0);
    
    portInitialized_i : in std_logic;
    inboundControlValid_i : in std_logic;
    inboundControlSymbol_i : in std_logic_vector(23 downto 0);
    inboundDataValid_i : in std_logic;
    inboundDataSymbol_i : in std_logic_vector(31 downto 0);
    
    txControlWrite_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
    txControlSymbol_o : out std_logic_vector(12*NUMBER_SYMBOLS downto 0);
    rxControlWrite_o : out std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
    rxControlSymbol_o : out std_logic_vector(12*NUMBER_SYMBOLS downto 0);
    
    ackIdStatus_o : out std_logic_vector(4 downto 0);
    linkInitialized_o : out std_logic;
    
    writeFrameFull_i : in std_logic;
    writeFrame_o : out std_logic;
    writeFrameAbort_o : out std_logic;
    writeContent_o : out std_logic;
    writeContentData_o : out std_logic_vector(32*NUMBER_SYMBOLS-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioReceiver.
-------------------------------------------------------------------------------
architecture RioReceiverImpl of RioReceiver is

  component RioReceiverCore is
    generic(
      NUMBER_SYMBOLS : natural range 1 to 1 := 1);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      -- Status signals used for maintenance.
      portEnable_i : in std_logic;

      -- Support for localAckIdCSR.
      -- REMARK: Add support for this???
      localAckIdWrite_i : in std_logic;
      inboundAckId_i : in std_logic_vector(4 downto 0);
      inboundAckId_o : out std_logic_vector(4 downto 0);
      
      -- Port input interface.
      portInitialized_i : in std_logic;
      inboundControlValid_i : in std_logic;
      inboundControlSymbol_i : in std_logic_vector(23 downto 0);
      inboundDataValid_i : in std_logic;
      inboundDataSymbol_i : in std_logic_vector(31 downto 0);

      -- Receiver has received a control symbol containing:
      -- packet-accepted, packet-retry, packet-not-accepted, 
      -- status, VC_status, link-response
      txControlWrite_o : out std_logic;
      txControlSymbol_o : out std_logic_vector(12 downto 0);

      -- Receiver wants to signal the link partner:
      -- a new frame has been accepted => packet-accepted(rxAckId, bufferStatus)
      -- a frame needs to be retransmitted due to buffering =>
      -- packet-retry(rxAckId, bufferStatus)
      -- a frame is rejected due to errors => packet-not-accepted
      -- a link-request should be answered => link-response
      rxControlWrite_o : out std_logic;
      rxControlSymbol_o : out std_logic_vector(12 downto 0);

      -- Status signals used internally.
      ackIdStatus_o : out std_logic_vector(4 downto 0);
      linkInitialized_o : out std_logic;

      -- Core->Core cascading signals.
      operational_i : in std_logic;
      operational_o : out std_logic;
      inputRetryStopped_i : in std_logic;
      inputRetryStopped_o : out std_logic;
      inputErrorStopped_i : in std_logic;
      inputErrorStopped_o : out std_logic;
      ackId_i : in unsigned(4 downto 0);
      ackId_o : out unsigned(4 downto 0);
      frameIndex_i : in std_logic_vector(6 downto 0);
      frameIndex_o : out std_logic_vector(6 downto 0);
      crc_i : in std_logic_vector(15 downto 0);
      crc_o : out std_logic_vector(15 downto 0);
      
      -- Frame buffering interface.
      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(32*NUMBER_SYMBOLS-1 downto 0));
  end component;

  signal operationalCurrent, operationalNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal inputRetryStoppedCurrent, inputRetryStoppedNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal inputErrorStoppedCurrent, inputErrorStoppedNext : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal ackIdCurrent, ackIdNext : unsigned(5*NUMBER_SYMBOLS-1 downto 0);
  signal frameIndexCurrent, frameIndexNext : std_logic_vector(7*NUMBER_SYMBOLS-1 downto 0);
  signal crcCurrent, crcNext : std_logic_vector(16*NUMBER_SYMBOLS-1 downto 0);

  signal txControlWrite : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  signal rxControlWrite : std_logic_vector(NUMBER_SYMBOLS-1 downto 0);
  
begin

  -----------------------------------------------------------------------------
  -- Protocol core and synchronization.
  -----------------------------------------------------------------------------
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      operationalCurrent <= (others=>'0');
      inputRetryStoppedCurrent <= (others=>'0');
      inputErrorStoppedCurrent <= (others=>'0');
      ackIdCurrent <= (others=>'0');
      frameIndexCurrent <= (others => '0');
      crcCurrent <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        operationalCurrent <= operationalNext;
        inputRetryStoppedCurrent <= inputRetryStoppedNext;
        inputErrorStoppedCurrent <= inputErrorStoppedNext;
        ackIdCurrent <= ackIdNext;
        frameIndexCurrent <= frameIndexNext;
        crcCurrent <= crcNext;
      end if;
    end if;
  end process;

  CoreGeneration: for i in 0 to NUMBER_SYMBOLS-1 generate
    txControlWrite_o(i) <= txControlWrite(i);
    rxControlWrite_o(i) <= rxControlWrite(i);
  
    ReceiverCore: RioReceiverCore
      generic map(NUMBER_SYMBOLS=>NUMBER_SYMBOLS)
      port map(
        clk=>clk,
        areset_n=>areset_n,
        enable=>enable,
        portEnable_i=>portEnable_i,
        localAckIdWrite_i=>localAckIdWrite_i,
        inboundAckId_i=>inboundAckId_i,
        inboundAckId_o=>inboundAckId_o,
        portInitialized_i=>portInitialized_i,
        inboundControlValid_i=>inboundControlValid_i,
        inboundControlSymbol_i=>inboundControlSymbol_i,
        inboundDataValid_i=>inboundDataValid_i,
        inboundDataSymbol_i=>inboundDataSymbol_i,
        txControlWrite_o=>txControlWrite(i),
        txControlSymbol_o=>txControlSymbol_o(13*(i+1)-1 downto 13*i),
        rxControlWrite_o=>rxControlWrite(i),
        rxControlSymbol_o=>rxControlSymbol_o(13*(i+1)-1 downto 13*i),
        ackIdStatus_o=>ackIdStatus_o,
        linkInitialized_o=>linkInitialized_o,
        operational_i=>operationalCurrent(i),
        operational_o=>operationalNext(i),
        inputRetryStopped_i=>inputRetryStoppedCurrent(i),
        inputRetryStopped_o=>inputRetryStoppedNext(i),
        inputErrorStopped_i=>inputErrorStoppedCurrent(i),
        inputErrorStopped_o=>inputErrorStoppedNext(i),
        ackId_i=>ackIdCurrent(5*(i+1)-1 downto 5*i),
        ackId_o=>ackIdNext(5*(i+1)-1 downto 5*i),
        frameIndex_i=>frameIndexCurrent(7*(i+1)-1 downto 7*i),
        frameIndex_o=>frameIndexNext(7*(i+1)-1 downto 7*i),
        crc_i=>crcCurrent(16*(i+1)-1 downto 16*i),
        crc_o=>crcNext(16*(i+1)-1 downto 16*i),
        writeFrameFull_i=>writeFrameFull_i,
        writeFrame_o=>writeFrame_o,
        writeFrameAbort_o=>writeFrameAbort_o,
        writeContent_o=>writeContent_o,
        writeContentData_o=>writeContentData_o(32*(i+1)-1 downto 32*i));
  end generate;
  
end architecture;
  
-------------------------------------------------------------------------------
-- RioReceiverCore
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioReceiverCore is
  generic(
    NUMBER_SYMBOLS : natural range 1 to 1 := 1);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    -- Status signals used for maintenance.
    portEnable_i : in std_logic;

    -- Support for localAckIdCSR.
    -- REMARK: Add support for this???
    localAckIdWrite_i : in std_logic;
    inboundAckId_i : in std_logic_vector(4 downto 0);
    inboundAckId_o : out std_logic_vector(4 downto 0);
    
    -- Port input interface.
    portInitialized_i : in std_logic;
    inboundControlValid_i : in std_logic;
    inboundControlSymbol_i : in std_logic_vector(23 downto 0);
    inboundDataValid_i : in std_logic;
    inboundDataSymbol_i : in std_logic_vector(31 downto 0);

    -- Receiver has received a control symbol containing:
    -- packet-accepted, packet-retry, packet-not-accepted, 
    -- status, VC_status, link-response
    txControlWrite_o : out std_logic;
    txControlSymbol_o : out std_logic_vector(12 downto 0);

    -- Reciever wants to signal the link partner:
    -- a new frame has been accepted => packet-accepted(rxAckId, bufferStatus)
    -- a frame needs to be retransmitted due to buffering =>
    -- packet-retry(rxAckId, bufferStatus)
    -- a frame is rejected due to errors => packet-not-accepted
    -- a link-request should be answered => link-response
    rxControlWrite_o : out std_logic;
    rxControlSymbol_o : out std_logic_vector(12 downto 0);

    -- Status signals used internally.
    ackIdStatus_o : out std_logic_vector(4 downto 0);
    linkInitialized_o : out std_logic;

    -- Core->Core cascading signals.
    operational_i : in std_logic;
    operational_o : out std_logic;
    inputRetryStopped_i : in std_logic;
    inputRetryStopped_o : out std_logic;
    inputErrorStopped_i : in std_logic;
    inputErrorStopped_o : out std_logic;
    ackId_i : in unsigned(4 downto 0);
    ackId_o : out unsigned(4 downto 0);
    frameIndex_i : in std_logic_vector(6 downto 0);
    frameIndex_o : out std_logic_vector(6 downto 0);
    crc_i : in std_logic_vector(15 downto 0);
    crc_o : out std_logic_vector(15 downto 0);
    
    -- Frame buffering interface.
    writeFrameFull_i : in std_logic;
    writeFrame_o : out std_logic;
    writeFrameAbort_o : out std_logic;
    writeContent_o : out std_logic;
    writeContentData_o : out std_logic_vector(32*NUMBER_SYMBOLS-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioReceiverCoreImpl of RioReceiverCore is

  component Crc5ITU is
    port(
      d_i : in  std_logic_vector(18 downto 0);
      crc_o : out std_logic_vector(4 downto 0));
  end component;

  component Crc16CITT is
    port(
      d_i : in  std_logic_vector(15 downto 0);
      crc_i : in  std_logic_vector(15 downto 0);
      crc_o : out std_logic_vector(15 downto 0));
  end component;

  signal crc5 : std_logic_vector(4 downto 0);
  signal crc5Valid : std_logic;
  signal symbolErrorValid0 : std_logic;
  signal symbolControlValid0 : std_logic;
  signal symbolControlContent0 : std_logic_vector(23 downto 0);
  signal symbolDataValid0 : std_logic;
  signal symbolDataContent0 : std_logic_vector(31 downto 0);
  
  signal symbolErrorValid1 : std_logic;
  signal symbolControlCrcValid1 : std_logic;
  signal symbolControlValid1 : std_logic;
  signal symbolControlContent1 : std_logic_vector(23 downto 0);
  signal symbolDataValid1 : std_logic;
  signal symbolDataContent1 : std_logic_vector(31 downto 0);
  signal stype0Status : std_logic;
  signal stype1Start : std_logic;
  signal stype1End : std_logic;
  signal stype1Stomp : std_logic;
  signal stype1RestartFromRetry : std_logic;
  signal stype1LinkRequest : std_logic;

  signal writeFrameOut : std_logic;
  signal writeFrameAbortOut : std_logic;
  signal writeContentOut : std_logic;
  signal crc16Data : std_logic_vector(31 downto 0);
  signal crc16Current : std_logic_vector(15 downto 0);
  signal crc16Temp : std_logic_vector(15 downto 0);
  signal crc16Next : std_logic_vector(15 downto 0);
  signal crc16Valid : std_logic;
  signal rxControlWriteOut : std_logic;
  signal rxControlSymbolOut : std_logic_vector(12 downto 0);
  
begin

  linkInitialized_o <= operational_i;
  ackIdStatus_o <= std_logic_vector(ackId_i);
  inboundAckId_o <= std_logic_vector(ackId_i);

  -----------------------------------------------------------------------------
  -- First pipeline stage.
  -- Check the validity of the symbol, CRC5 on control symbols, and save the
  -- symbol content for the next stage.
  -----------------------------------------------------------------------------

  Crc5Calculator: Crc5ITU 
    port map(d_i=>inboundControlSymbol_i(23 downto 5), crc_o=>crc5);

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      crc5Valid <= '0';
      symbolErrorValid0 <= '0';
      symbolControlValid0 <= '0';
      symbolControlContent0 <= (others=>'0');
      symbolDataValid0 <= '0';
      symbolDataContent0 <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then 
        if (crc5 = inboundControlSymbol_i(4 downto 0)) then
          crc5Valid <= '1';
        else
          crc5Valid <= '0';
        end if;
        if (inboundControlValid_i = '1') and (inboundDataValid_i = '1') then
          symbolErrorValid0 <= '1';
          symbolControlValid0 <= '0';
          symbolDataValid0 <= '0';
        else
          symbolErrorValid0 <= '0';
          symbolControlValid0 <= inboundControlValid_i;
          symbolControlContent0 <= inboundControlSymbol_i;
          symbolDataValid0 <= inboundDataValid_i;
          symbolDataContent0 <= inboundDataSymbol_i;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Second pipeline stage.
  -- Separate the part of the control symbol that are going to the transmitter
  -- side and check the type of symbol for this side. 
  -----------------------------------------------------------------------------

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      txControlWrite_o <= '0';
      txControlSymbol_o <= (others => '0');

      symbolErrorValid1 <= '0';
      
      symbolControlCrcValid1 <= '0';
      symbolControlValid1 <= '0';
      symbolControlContent1 <= (others => '0');
      symbolDataValid1 <= '0';
      symbolDataContent1 <= (others=>'0');

      stype0Status <= '0';
      stype1Start <= '0';
      stype1End <= '0';
      stype1Stomp <= '0';
      stype1RestartFromRetry <= '0';
      stype1LinkRequest <= '0';
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        symbolErrorValid1 <= symbolErrorValid0;
        
        symbolControlCrcValid1 <= crc5Valid;
        symbolControlValid1 <= symbolControlValid0;
        symbolControlContent1 <= symbolControlContent0;
        symbolDataValid1 <= symbolDataValid0;
        symbolDataContent1 <= symbolDataContent0;

        txControlWrite_o <= '0';
        txControlSymbol_o <= symbolControlContent0(23 downto 11);
        
        if (symbolControlValid0 = '1') then
          if (crc5Valid = '1') then
            -- Forward the part of the control-symbol that are targeted to the
            -- transmitter.
            txControlWrite_o <= '1';
          end if;
          
          if (symbolControlContent0(23 downto 21) = STYPE0_STATUS) then
            stype0Status <= '1';
          else
            stype0Status <= '0';
          end if;
          if (symbolControlContent0(10 downto 8) = STYPE1_START_OF_PACKET) then
            stype1Start <= '1';
          else
            stype1Start <= '0';
          end if;
          if (symbolControlContent0(10 downto 8) = STYPE1_END_OF_PACKET) then
            stype1End <= '1';
          else
            stype1End <= '0';
          end if;
          if (symbolControlContent0(10 downto 8) = STYPE1_STOMP) then
            stype1Stomp <= '1';
          else
            stype1Stomp <= '0';
          end if;
          if (symbolControlContent0(10 downto 8) = STYPE1_RESTART_FROM_RETRY) then
            stype1RestartFromRetry <= '1';
          else
            stype1RestartFromRetry <= '0';
          end if;
          if (symbolControlContent0(10 downto 8) = STYPE1_LINK_REQUEST) then
            stype1LinkRequest <= '1';
          else
            stype1LinkRequest <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Third pipeline stage.
  -- Update the CRC16 for the packet.
  -- Update the buffered data and write it to the packet buffer if needed.
  -- Update the main receiver state machine.
  -- Generate reply symbols to the link-partner.
  -----------------------------------------------------------------------------

  -- Set the output to save when writing new frame content.
  writeFrame_o <= writeFrameOut and enable;
  writeFrameAbort_o <= writeFrameAbortOut and enable;
  writeContent_o <= writeContentOut and enable;
  writeContentData_o <= symbolDataContent1;
  
  -- Create the new input depending on the current frame position.
  crc16Data(31 downto 26) <= "000000" when (unsigned(frameIndex_i) = 1) else
                             symbolDataContent1(31 downto 26);
  crc16Data(25 downto 0) <= symbolDataContent1(25 downto 0);

  -- Initialize the crc at frame start.
  crc16Current <= crc_i when (unsigned(frameIndex_i) /= 1) else (others => '1');

  -- Crc calculation units.
  Crc16Msb: Crc16CITT
    port map(
      d_i=>crc16Data(31 downto 16), crc_i=>crc16Current, crc_o=>crc16Temp);
  Crc16Lsb: Crc16CITT
    port map(
      d_i=>crc16Data(15 downto 0), crc_i=>crc16Temp, crc_o=>crc16Next);

  -- Save the new CRC value when a dataSymbol was received.
  crc_o <= crc_i when (symbolDataValid1 = '0') else crc16Next;

  -- Check if the CRC is ok.
  crc16Valid <= '1' when (crc_i = x"0000") else '0';

  -- The main protocol handling process.
  process(portInitialized_i, portEnable_i, writeFrameFull_i,
          operational_i, ackId_i, frameIndex_i, 
          inputRetryStopped_i, inputErrorStopped_i,
          symbolControlValid1, symbolControlCrcValid1, symbolControlContent1,
          stype0Status,
          stype1Start, stype1End, stype1Stomp, stype1RestartFromRetry, stype1LinkRequest, 
          symbolDataValid1,
          crc16Valid)
  begin
    operational_o <= operational_i;
    frameIndex_o <= frameIndex_i;
    inputRetryStopped_o <= inputRetryStopped_i;
    inputErrorStopped_o <= inputErrorStopped_i;
    ackId_o <= ackId_i;
    
    rxControlWriteOut <= '0';
    rxControlSymbolOut <= (others => '0');
    
    writeFrameOut <= '0';
    writeFrameAbortOut <= '0';
    writeContentOut <= '0';

    -- Act on the current state.
    if (operational_i = '0') then 
      ---------------------------------------------------------------------
      -- The port is not operational and is waiting for status control
      -- symbols to be received on the link. Count the number
      -- of error-free status symbols and considder the link operational
      -- when enough of them has been received. Frames are not allowed
      -- here.
      ---------------------------------------------------------------------
      
      -- Check if the port is initialized.
      if (portInitialized_i = '1') then
        -- Port is initialized.
        
        -- Check if the control symbol has a valid checksum.
        if (symbolControlCrcValid1 = '1') then
          -- The control symbol has a valid checksum.
          
          -- Check the stype0 part if we should count the number of
          -- error-free status symbols.
          if (stype0Status = '1') then
            -- The symbol is a status.
            
            -- Check if enough status symbols have been received.
            if (unsigned(frameIndex_i) = 7) then
              -- Enough status symbols have been received.

              -- Go to operational state and make sure all variables are
              -- reset.
              operational_o <= '1';
              frameIndex_o <= (others => '0');
              inputRetryStopped_o <= '0';
              inputErrorStopped_o <= '0';
              
              -- Reset all packets.
              writeFrameAbortOut <= '1';
            else
              -- Increase the number of error-free status symbols that
              -- has been received.
              frameIndex_o <= std_logic_vector(unsigned(frameIndex_i) + 1);
            end if;
          else
            -- The symbol is not a status.
            -- Dont do anything.
          end if;
        else
          -- A control symbol with CRC5 error was recevied.
          frameIndex_o <= (others => '0');
        end if;
      else
        -- The port has become uninitialized.
        frameIndex_o <= (others => '0');
      end if;
    else
      ---------------------------------------------------------------------
      -- The port has been initialized and enough error free status symbols
      -- have been received. Forward data frames to the frame buffer
      -- interface. This is the normal operational state.
      ---------------------------------------------------------------------
      
      -- Check that the port is initialized.
      if (portInitialized_i = '1') then
        -- The port and link is initialized.

        -- Check if an error has been detected by the PCS.
        if (symbolErrorValid1 = '1') then
          -- An error-symbol from the PCS has been received.
          -- Send a packet-not-accepted and indicate the error.
          rxControlWriteOut <= '1';
          rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                "00000" &
                                PACKET_NOT_ACCEPTED_CAUSE_INVALID_CHARACTER;
          inputErrorStopped_o <= '1';
        end if;
        
        -- Check if the control symbol has a valid CRC-5.
        if (symbolControlCrcValid1 = '1') then
          -- The symbol is correct.

          -- Check if a packet is starting or ending.
          if (((stype1Start = '1') or (stype1End = '1')) and
              (inputRetryStopped_i = '0') and (inputErrorStopped_i = '0')) then
            -------------------------------------------------------------
            -- Start the reception of a new frame or end a currently
            -- ongoing frame and start a new one.
            -------------------------------------------------------------
            
            -- Check if a frame has already been started.
            if ((unsigned(frameIndex_i) /= 0) or (stype1End = '1')) then
              -- A frame is already started or is ending.
              -- Complete the last frame and start to ackumulate a new one
              -- and update the ackId.

              -- Check if the frame is too short.
              if (unsigned(frameIndex_i) > 3) then
                -- The frame is not too short.

                -- Reset the frame index to indicate the frame is started.
                frameIndex_o <= "0000001";
                
                -- Check the CRC-16 and the length of the received frame.
                if (crc16Valid = '1') then
                  -- The CRC-16 is ok.

                  -- Update the frame buffer to indicate that the frame has
                  -- been completly received.
                  writeFrameOut <= '1';

                  -- Update ackId.
                  ackId_o <= ackId_i + 1;

                  -- Send packet-accepted.
                  -- The buffer status is appended by the transmitter
                  -- when sent to get the latest number.
                  -- REMARK: Move this out of this process and into pipeline
                  -- stage 4.
                  rxControlWriteOut <= '1';
                  rxControlSymbolOut <= STYPE0_PACKET_ACCEPTED &
                                        std_logic_vector(ackId_i) &
                                        "11111";
                else
                  -- The CRC-16 is not ok.

                  -- Make the transmitter send a packet-not-accepted to indicate
                  -- that the received packet contained a CRC error.
                  rxControlWriteOut <= '1';
                  rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                        "00000" &
                                        PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC;
                  inputErrorStopped_o <= '1';
                end if;
              else
                -- The packet is too short.
                -- Make the transmitter send a packet-not-accepted to indicated
                -- that the received packet was too small.
                rxControlWriteOut <= '1';
                rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                      "00000" &
                                      PACKET_NOT_ACCEPTED_CAUSE_GENERAL_ERROR;
                inputErrorStopped_o <= '1';
              end if;
            else
              -- No frame has been started.
              -- This is the normal case when there was no frame started
              -- before. 
              
              -- Reset the frame index to indicate the frame is started.
              frameIndex_o <= "0000001";
            end if;
          end if;
          
          if ((stype1Stomp = '1') and
              (inputRetryStopped_i = '0') and (inputErrorStopped_i = '0')) then 
            -------------------------------------------------------------
            -- Restart the reception of an old frame.
            -------------------------------------------------------------
            -- See 5.10 in the 2.2 standard.
            
            -- Make the transmitter send a packet-retry to indicate
            -- that the packet cannot be accepted.
            rxControlWriteOut <= '1';
            rxControlSymbolOut <= STYPE0_PACKET_RETRY &
                                  std_logic_vector(ackId_i) &
                                  "11111";
            
            -- Enter the input retry-stopped state.
            inputRetryStopped_o <= '1';
          end if;

          if (stype1RestartFromRetry = '1') then
            if (inputRetryStopped_i = '1') then
              -------------------------------------------------------------
              -- The receiver indicates a restart from a retry sent
              -- from us.
              -------------------------------------------------------------

              -- Abort the frame and reset frame reception.
              frameIndex_o <= (others => '0');
              writeFrameAbortOut <= '1';
              
              -- Go back to the normal operational state.
              inputRetryStopped_o <= '0';
            else
              -------------------------------------------------------------
              -- The receiver indicates a restart from a retry sent
              -- from us.
              -------------------------------------------------------------
              -- See 5.10 in the standard.
              -- Protocol error, this symbol should not be received here since
              -- we should have been in input-retry-stopped. 
              
              -- Send a packet-not-accepted to indicate that a protocol
              -- error has occurred.
              rxControlWriteOut <= '1';
              rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                    "00000" &
                                    PACKET_NOT_ACCEPTED_CAUSE_GENERAL_ERROR;
              inputErrorStopped_o <= '1';
            end if;
          end if;

          if (stype1LinkRequest = '1') then
            -------------------------------------------------------------
            -- Reply to a LINK-REQUEST.
            -------------------------------------------------------------
            
            -- Check the command part.
            if (symbolControlContent1(15 downto 13) = "100") then
              -- Return input port status command.
              -- This functions as a link-request(restart-from-error)
              -- control symbol under error situations.

              if (inputErrorStopped_i = '1') then 
                rxControlWriteOut <= '1';
                rxControlSymbolOut <= STYPE0_LINK_RESPONSE &
                                      std_logic_vector(ackId_i) &
                                      "00101";
              elsif (inputRetryStopped_i = '1') then 
                rxControlWriteOut <= '1';
                rxControlSymbolOut <= STYPE0_LINK_RESPONSE &
                                      std_logic_vector(ackId_i) &
                                      "00100";
              else
                -- Send a link response containing an ok reply.
                rxControlWriteOut <= '1';
                rxControlSymbolOut <= STYPE0_LINK_RESPONSE &
                                      std_logic_vector(ackId_i) &
                                      "10000";
              end if;
            else
              -- Reset device command or other unsupported command.
              -- Discard this.
            end if;
            
            -- Abort the frame and reset frame reception.
            inputRetryStopped_o <= '0';
            inputErrorStopped_o <= '0';
            frameIndex_o <= (others=>'0');
            writeFrameAbortOut <= '1';
          end if;
        else
          -- A control symbol contains a crc error.

          -- Send a packet-not-accepted to indicate that a corrupted
          -- control-symbol has been received and change state.
          rxControlWriteOut <= '1';
          rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                "00000" &
                                PACKET_NOT_ACCEPTED_CAUSE_CONTROL_CRC;
          inputErrorStopped_o <= '1';
        end if;
        
        if ((symbolDataValid1 = '1')  and
            (inputRetryStopped_i = '0') and (inputErrorStopped_i = '0')) then
          -------------------------------------------------------------
          -- This is a data symbol.
          -------------------------------------------------------------
          -- REMARK: Add check for in-the-middle-crc here...

          case frameIndex_i is
            when "0000000" | "1000110" =>
              -- A frame has not been started or is too long.
              -- Send packet-not-accepted.
              rxControlWriteOut <= '1';
              rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                    "00000" &
                                    PACKET_NOT_ACCEPTED_CAUSE_GENERAL_ERROR;
              inputErrorStopped_o <= '1';
            when "0000001" =>
              -- Check if the packet contains the correct ackId.
              if (unsigned(symbolDataContent1(31 downto 27)) = ackId_i) then
                -- The packet has a correct ackId.
                
                -- Check if the packet is allowed.
                if ((portEnable_i = '1') or
                    (symbolDataContent1(19 downto 16) = FTYPE_MAINTENANCE_CLASS)) then
                  -- The packet is allowed.

                  -- Check if there are buffers available to store the new
                  -- packet.
                  if (writeFrameFull_i = '0') then
                    -- There are buffering space available to store the new
                    -- data.
                    
                    -- Write the symbol content and increment the number of
                    -- received data symbols.
                    writeContentOut <= '1';
                    frameIndex_o <= std_logic_vector(unsigned(frameIndex_i) + 1);
                  else
                    -- The packet buffer is full.
                    -- Let the link-partner resend the packet.
                    rxControlWriteOut <= '1';
                    rxControlSymbolOut <= STYPE0_PACKET_RETRY &
                                          std_logic_vector(ackId_i) &
                                          "11111";
                    inputRetryStopped_o <= '1';
                  end if;
                else
                  -- A non-maintenance packet is not allowed.
                  -- Send packet-not-accepted.
                  rxControlWriteOut <= '1';
                  rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                        "00000" &
                                        PACKET_NOT_ACCEPTED_CAUSE_NON_MAINTENANCE_STOPPED;
                  inputErrorStopped_o <= '1';
                end if;
              else
                -- The ackId is unexpected.
                -- Send packet-not-accepted.
                rxControlWriteOut <= '1';
                rxControlSymbolOut <= STYPE0_PACKET_NOT_ACCEPTED &
                                      "00000" &
                                      PACKET_NOT_ACCEPTED_CAUSE_UNEXPECTED_ACKID;
                inputErrorStopped_o <= '1';
              end if;
            when others =>
              -- A frame has been started and is not too long.
              -- Check if the buffer entry is ready to be written
              -- into the packet buffer.
              writeContentOut <= '1';

              -- Increment the number of received data symbols.
              frameIndex_o <= std_logic_vector(unsigned(frameIndex_i) + 1);
          end case;
        end if;
      else
        -- The port has become uninitialized.
        -- Go back to the uninitialized state.
        operational_o <= '0';
      end if;
    end if;
  end process;

  -- REMARK: Add writeContentXXX-registers...
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      rxControlWrite_o <= '0';
      rxControlSymbol_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        rxControlWrite_o <= rxControlWriteOut and symbolControlValid1;
        rxControlSymbol_o <= rxControlSymbolOut;
      end if;
    end if;
  end process;
  
end architecture;



-------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioFifo is
  generic(
    DEPTH_WIDTH : natural;
    DATA_WIDTH : natural);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    empty_o : out std_logic;
    read_i : in std_logic;
    data_o : out std_logic_vector(DATA_WIDTH-1 downto 0);

    write_i : in std_logic;
    data_i : in std_logic_vector(DATA_WIDTH-1 downto 0));
end entity;
       

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioFifoImpl of RioFifo is

  component MemorySimpleDualPortAsync is
    generic(
      ADDRESS_WIDTH : natural := 1;
      DATA_WIDTH : natural := 1;
      INIT_VALUE : std_logic := 'U');
    port(
      clkA_i : in std_logic;
      enableA_i : in std_logic;
      addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);

      addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;

  signal empty : std_logic;
  signal full : std_logic;

  signal readAddress : std_logic_vector(DEPTH_WIDTH-1 downto 0);
  signal readAddressInc : std_logic_vector(DEPTH_WIDTH-1 downto 0);
  signal writeAddress : std_logic_vector(DEPTH_WIDTH-1 downto 0);
  signal writeAddressInc : std_logic_vector(DEPTH_WIDTH-1 downto 0);
begin

  empty_o <= empty;

  readAddressInc <= std_logic_vector(unsigned(readAddress) + 1);
  writeAddressInc <= std_logic_vector(unsigned(writeAddress) + 1);

  process(areset_n, clk)
  begin
    if (areset_n = '0') then
      empty <= '1';
      full <= '0';
      readAddress <= (others=>'0');
      writeAddress <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (empty = '1') then
        if (write_i = '1') then
          empty <= '0';
          writeAddress <= writeAddressInc;
        end if;
      end if;
      if (full = '1') then
        if (read_i = '1') then
          full <= '0';
          readAddress <= readAddressInc;
        end if;
      end if;
      if (empty = '0') and (full = '0') then
        if (write_i = '1') and (read_i = '0') then
          writeAddress <= writeAddressInc;
          if (writeAddressInc = readAddress) then
            full <= '1';
          end if;
        end if;
        if (write_i = '0') and (read_i = '1') then
          readAddress <= readAddressInc;
          if (readAddressInc = writeAddress) then
            empty <= '1';
          end if;
        end if;
        if (write_i = '1') and (read_i = '1') then
          writeAddress <= writeAddressInc;
          readAddress <= readAddressInc;
        end if;
      end if;
    end if;
  end process;
  
  Memory: MemorySimpleDualPortAsync
    generic map(ADDRESS_WIDTH=>DEPTH_WIDTH,
                DATA_WIDTH=>DATA_WIDTH,
                INIT_VALUE=>'0')
    port map(
      clkA_i=>clk, enableA_i=>write_i,
      addressA_i=>writeAddress, dataA_i=>data_i,
      addressB_i=>readAddress, dataB_o=>data_o);
end architecture;



-------------------------------------------------------------------------------
-- A CRC-5 calculator following the implementation proposed in the 2.2
-- standard.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity Crc5ITU is
  port(
    d_i : in  std_logic_vector(18 downto 0);
    crc_o : out std_logic_vector(4 downto 0));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture Crc5Impl of Crc5ITU is
  signal d : std_logic_vector(0 to 18);
  signal c : std_logic_vector(0 to 4);

begin
  -- Reverse the bit vector indexes to make them the same as in the standard.
  d(18) <= d_i(0); d(17) <= d_i(1); d(16) <= d_i(2); d(15) <= d_i(3);
  d(14) <= d_i(4); d(13) <= d_i(5); d(12) <= d_i(6); d(11) <= d_i(7);
  d(10) <= d_i(8); d(9) <= d_i(9); d(8) <= d_i(10); d(7) <= d_i(11);
  d(6) <= d_i(12); d(5) <= d_i(13); d(4) <= d_i(14); d(3) <= d_i(15);
  d(2) <= d_i(16); d(1) <= d_i(17); d(0) <= d_i(18);

  -- Calculate the resulting crc.
  c(0) <= d(18) xor d(16) xor d(15) xor d(12) xor
          d(10) xor d(5) xor d(4) xor d(3) xor
          d(1) xor d(0);
  c(1) <= (not d(18)) xor d(17) xor d(15) xor d(13) xor
          d(12) xor d(11) xor d(10) xor d(6) xor
          d(3) xor d(2) xor d(0);
  c(2) <= (not d(18)) xor d(16) xor d(14) xor d(13) xor
          d(12) xor d(11) xor d(7) xor d(4) xor
          d(3) xor d(1);
  c(3) <= (not d(18)) xor d(17) xor d(16) xor d(14) xor
          d(13) xor d(10) xor d(8) xor d(3) xor
          d(2) xor d(1);
  c(4) <= d(18) xor d(17) xor d(15) xor d(14) xor
          d(11) xor d(9) xor d(4) xor d(3) xor
          d(2) xor d(0);

  -- Reverse the bit vector indexes to make them the same as in the standard.
  crc_o(4) <= c(0); crc_o(3) <= c(1); crc_o(2) <= c(2); crc_o(1) <= c(3);
  crc_o(0) <= c(4);
end architecture;
