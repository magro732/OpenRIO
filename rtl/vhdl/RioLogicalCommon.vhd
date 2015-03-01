-------------------------------------------------------------------------------
-- RioLogicalCommon.
-------------------------------------------------------------------------------
-- Ingress:
-- * Removes in-the-middle and trailing CRC.
-- * Forwards packets to logical-layer handlers depending on ftype and
--   transaction (output as address).
-- * Outputs header and deviceIDs in seperate accesses to facilitate 8- and
--   16-bit deviceAddress support. All fields are right-justified.
-- Egress:
-- * Adds in-the-middle and trailing CRC.
-- * Receives packets from logical-layer handlers.
-- * Receives header and deviceIDs in seperate accesses to facilitate 8- and
--   16-bit deviceAddress support. All fields are right-justified.
-------------------------------------------------------------------------------
-- REMARK: Egress; Places packets in different queues depending on the packet priority?
-- REMARK: Use inputOutput/message/maintenance/gsm/...-strobes instead?
-- REMARK: If the deviceId:s are removed, it will work for both 8/16-bit deviceIds.
--          case (ftype) is
--            when x"1" =>
--              -- Intervention-request class.
--              gsmStb_o <= '1';
--            when x"2" =>
--              -- Request-class.
--              if ((transaction = "0100") or
--                  (transaction = "1100") or (transaction = "1101") or
--                  (transaction = "1110") or (transaction = "1111")) then
--                inputOutputStb_o <= '1';
--              else
--                gsmStb_o <= '1';
--              end if;
--            when x"5" =>
--              -- Write-class.
--              if ((transaction = "0100") or (transaction = "0101") or 
--                  (transaction = "1100") or (transaction = "1101") or
--                  (transaction = "1110")) then
--                inputOutputStb_o <= '1';
--              elsif ((transaction = "0000") or (transaction = "0001")) then
--                gsmStb_o <= '1';
--              end if;
--            when x"6" =>
--              -- Streaming-Write class.
--              inputOutputStb_o <= '1';
--            when x"7" =>
--              -- Flow-control class.
--              flowControlStb_o <= '1';
--            when x"8" =>
--              -- Maintenance class.
--              maintenanceStb_o <= '1';
--            when x"9" =>
--              -- Data-Streaming class.
--              dataStreamingStb_o <= '1';
--            when x"a" =>
--              -- Doorbell class.
--              -- REMARK: Make this belong to input/output since the packets
--              -- and their responses look the same?
--              messagePassingStb_o <= '1';
--            when x"b" =>
--              -- Message class.
--              messagePassingStb_o <= '1';
--            when x"d" =>
--              -- Response class.
--              -- REMARK: Seperate strobe for this???
--              if ((transaction = "0000") or (transaction = "1000")) then
--                -- REMARK: Doorbell-response going in here as well... *sigh*
--                -- REMARK: GSM-responses going in here as well...
--                responseStb_o <= '1';
--              elsif (transaction = "0001") then
--                messagePassing <= '1';
--              end if;
--            when others =>
--              -- Unsupported ftype.
--              -- REMARK: Discard this packet.
--          end case;

-- tt=00
-- 0: header(15:0);dest(7:0);src(7:0);
-- 1: transaction(3:0)
-- shifter: 32 (32 empty)
-- tt=01
-- 0: header(15:0);dest(15:0);
-- 1: src(15:0);transaction(3:0)
-- shifter: 16 (48 empty)

-------------------------------------------------------------------------------
-- RioLogicalCommonIngress.
-------------------------------------------------------------------------------
-- REMARK: Check the destination address to see if it matches the one configured???
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioLogicalCommonIngress.
-------------------------------------------------------------------------------
entity RioLogicalCommonIngress is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    
    readFrameEmpty_i : in std_logic;
    readFrame_o : out std_logic;
    readContent_o : out std_logic;
    readContentEnd_i : in std_logic;
    readContentData_i : in std_logic_vector(31 downto 0);

    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterAdr_o : out std_logic_vector(7 downto 0);
    masterSel_o : out std_logic_vector(3 downto 0);
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioLogicalCommonIngress of RioLogicalCommonIngress is

begin

  process(clk, areset_n)
  begin
    if (areset_n = '0') then

    elsif (clk'event and clk = '1') then
      readContent_o <= '0';
      
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetPosition <= 0;
          if (readFrameEmpty_i = '0') then
            readContent_o <= '1';
            state <= WAIT_HEADER_0;
          end if;
          
        when WAIT_HEADER_0 =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          readContent_o <= '1';
          state <= HEADER_0;

        when HEADER_0 =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetContent <= packetContent(31 downto 0) & readContentData_i;
          packetPosition <= packetPosition + 1;
          readContent_o <= '1';

          tt <= readContentData_i(21 downto 20);
          ftype <= readContentData_i(19 downto 16);

          state <= HEADER_1;
          
        when HEADER_1 =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetContent <= packetContent(31 downto 0) & readContentData_i;
          packetPosition <= packetPosition + 1;

          if (tt = "00") then
            transaction <= readContentData_i(31 downto 28);
          elsif (tt = "01") then
            transaction <= readContentData_i(15 downto 12);
          end if;
          
          state <= SEND_HEADER;

        when SEND_HEADER =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          masterStb_o <= '1';
          masterAdr_o <= ftype & transaction;
          masterSel_o <= x"0011";
          masterDat_o <= x"0000" & packetContent(63 downto 48);
          packetContent <= packetContent(47 downto 0) & x"0000";

          state <= SEND_DESTINATION;

        when SEND_DESTINATION =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            if (tt = "00") then
              masterSel_o <= x"0001";
              masterDat_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              masterSel_o <= x"0011";
              masterDat_o <= x"0000" & packetContent(63 downto 48);
              packetContent <= packetContent(31 downto 0) & readContentData_i;
              readContent_o <= '1';
            end if;

            state <= SEND_SOURCE;
          end if;
          
        when SEND_SOURCE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            if (tt = "00") then
              masterSel_o <= x"0001";
              masterDat_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              masterSel_o <= x"0011";
              masterDat_o <= x"0000" & packetContent(63 downto 48);
              packetContent <= packetContent(47 downto 0) & x"0000";
            end if;

            state <= FORWARD;
          end if;
          
        when FORWARD =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            masterSel_o <= x"1111";
            masterDat_o <= packetContent(63 downto 32);

            packetPosition <= packetPosition + 1;

            -- REMARK: Rewrite depending on tt-field to compensate for
            -- different number of valid bits in the shifter...
            if (packetPosition < 20) then
              packetContent <=
                packetContent(31 downto 0) & readContentData_i;
            elsif (packetPosition = 20) then
              packetContent <=
                packetContent(31 downto 0) & readContentData_i(15 downto 0) & x"0000";
            else
              packetContent <=
                packetContent(15 downto 0) & readContentData_i & x"0000";
            end if;
            
            if (readContentEnd_i = '0') then
              readContent_o <= '1';
            else
              readFrame_o <= '1';
              state <= FORWARD_LAST;
            end if;
          end if;

        when FORWARD_LAST =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          -- REMARK: The last always contain the CRC?
          if (masterAck_i = '1') then
            masterSel_o <= x"1111";
            masterDat_o <= packetContent(63 downto 32);
            state <= END_PACKET;
          end if;
          
        when END_PACKET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            state <= IDLE;
          end if;
          
        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          state <= IDLE;
      end case;
    end if;
  end process;

end architecture;


-------------------------------------------------------------------------------
-- RioLogicalCommonEgress.
-- Only 8-bit and 16-bit deviceId are supported. The first write must contain
-- the 16-bit header, the second write must contain the destination address and
-- the third must contain the source address.
-- CRC is calculated during the transfer and inserted at byte 81 and 82 and
-- appended to the packet when it ends.
-- slaveSelect_i - four bits indicating valid bytes in slaveData_i.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioLogicalCommonEgress.
-------------------------------------------------------------------------------
entity RioLogicalCommonEgress is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    writeFrameFull_i : in std_logic;
    writeFrame_o : out std_logic;
    writeFrameAbort_o : out std_logic;
    writeContent_o : out std_logic;
    writeContentData_o : out std_logic_vector(31 downto 0);

    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveSel_i : in std_logic_vector(3 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioLogicalCommonEgress.
-------------------------------------------------------------------------------
architecture RioLogicalCommonEgress of RioLogicalCommonEgress is

  component Crc16CITT is
    port(
      d_i : in  std_logic_vector(15 downto 0);
      crc_i : in  std_logic_vector(15 downto 0);
      crc_o : out std_logic_vector(15 downto 0));
  end component;

  signal crc16Current, crc16Temp, crc16Next: std_logic_vector(15 downto 0);

begin

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      crc16Current <= x"0000";
      writeContentData <= (others=>'0');
    elsif (clk'event and clk = '1') then
      writeContent_o <= '0';
      writeFrame_o <= '0';
      
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetPosition <= 0;
          if (writeFrameFull_i = '0') then
            state <= HEADER_GET;
            crc16Current <= x"ffff";
          end if;

        when GET_HEADER =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            -- REMARK: Only support the header in one position?
            if (slaveSelect_i = "1100") then
              header <= slaveData_i(31 downto 16);
              tt <= slaveData_i(21 downto 20);
            elsif (slaveSelect_i = "0110") then
              header <= slaveData_i(23 downto 8);
              tt <= slaveData_i(13 downto 12);
            elsif (slaveSelect_i = "0011") then
              header <= slaveData_i(15 downto 0);
              tt <= slaveData_i(5 downto 4);
            else
              -- REMARK: Not supported.
            end if;
            
            slaveAck_o <= '1';

            state <= HEADER_ACK;
          else
            state <= RESTART_FRAME;
          end if;

        when HEADER_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';
          state <= DESTINATION_GET;

        when DESTINATION_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            -- REMARK: Only support the destination in one position?
            if (slaveSelect_i = "1000") and (tt = "00") then
              dstAddr <= slaveData_i(31 downto 24);
            elsif (slaveSelect_i = "0100") and (tt = "00") then
              dstAddr <= slaveData_i(23 downto 16);
            elsif (slaveSelect_i = "0010") and (tt = "00") then
              dstAddr <= slaveData_i(15 downto 8);
            elsif (slaveSelect_i = "0001") and (tt = "00") then
              dstAddr <= slaveData_i(7 downto 0);
            elsif (slaveSelect_i = "1100") and (tt = "01") then
              writeContent_o <= '1';
              writeContentData <= header & slaveData_i(31 downto 16);
              packetPosition <= packetPosition + 1;
            elsif (slaveSelect_i = "0110") and (tt = "01") then
              writeContent_o <= '1';
              writeContentData <= header & slaveData_i(24 downto 8);
              packetPosition <= packetPosition + 1;
            elsif (slaveSelect_i = "0011") and (tt = "01") then
              writeContent_o <= '1';
              writeContentData <= header & slaveData_i(15 downto 0);
              packetPosition <= packetPosition + 1;
            else
              -- REMARK: Not supported.
            end if;
            
            slaveAck_o <= '1';

            state <= DESTINATION_ACK;
          else
            state <= RESTART_FRAME;
          end if;
        
        when DESTINATION_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';
          state <= SOURCE_GET;

        when SOURCE_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            -- REMARK: Only support the source in one position?
            if (slaveSelect_i = "1000") and (tt = "00") then
              halfWordPending <= '0';
              writeContent_o <= '1';
              writeContentData <= header & dstAddr & slaveData_i(31 downto 24);
              packetPosition <= packetPosition + 1;
            elsif (slaveSelect_i = "0100") and (tt = "00") then
              halfWordPending <= '0';
              writeContent_o <= '1';
              writeContentData <= header & dstAddr & slaveData_i(23 downto 16);
              packetPosition <= packetPosition + 1;
            elsif (slaveSelect_i = "0010") and (tt = "00") then
              halfWordPending <= '0';
              writeContent_o <= '1';
              writeContentData <= header & dstAddr & slaveData_i(15 downto 8);
              packetPosition <= packetPosition + 1;
            elsif (slaveSelect_i = "0001") and (tt = "00") then
              halfWordPending <= '0';
              writeContent_o <= '1';
              writeContentData <= header & dstAddr & slaveData_i(7 downto 0);
              packetPosition <= packetPosition + 1;
            elsif (slaveSelect_i = "1100") and (tt = "01") then
              halfWordPending <= '1';
              halfWord <= slaveData_i(31 downto 16);
            elsif (slaveSelect_i = "0110") and (tt = "01") then
              halfWordPending <= '1';
              halfWord <= slaveData_i(24 downto 8);
            elsif (slaveSelect_i = "0011") and (tt = "01") then
              halfWordPending <= '1';
              halfWord <= slaveData_i(15 downto 0);
            else
              -- REMARK: Not supported.
            end if;
            
            slaveAck_o <= '1';

            state <= SOURCE_ACK;
          else
            state <= RESTART_FRAME;
          end if;
        
        when SOURCE_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';

          if (tt = "00") then
            crcCurrent <= crcNext;
          end if;
          
          state <= CONTENT_GET;
          
        when CONTENT_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            -- REMARK: Only support full writes? Not possible with the last
            -- access though.
            if (slaveSelect_i = "1111") then
              if (halfWordPending = '0') then
                writeContent_o <= '1';
                writeContentData <= slaveData_i;
                packetPosition <= packetPosition + 1;
              else
                writeContent_o <= '1';
                writeContentData <= halfWord & slaveData_i(31 downto 16);
                packetPosition <= packetPosition + 1;
                halfWord <= slaveData_i(15 downto 0);
              end if;
            elsif (slaveSelect_i = "1100") then
              if (halfWordPending = '0') then
                halfWordPending <= '1';
                halfWord <= slaveData_i(31 downto 16);
              else
                writeContent_o <= '1';
                writeContentData <= halfWord & slaveData_i(31 downto 16);
                packetPosition <= packetPosition + 1;
                halfWordPending <= '0';
              end if;
            elsif (slaveSelect_i = "0011") then
              if (halfWordPending = '0') then
                halfWordPending <= '1';
                halfWord <= slaveData_i(15 downto 0);
              else
                writeContent_o <= '1';
                writeContentData <= halfWord & slaveData_i(15 downto 0);
                packetPosition <= packetPosition + 1;
                halfWordPending <= '0';
              end if;
            end if;
            
            slaveAck_o <= '1';

            state <= CONTENT_ACK;
          else
            state <= CRC_APPEND;
          end if;

        when CONTENT_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';
          
          crc16Current <= crc16Next;

          if (packetPosition = 20) then
            if (halfWordPending = '0') then
              halfWordPending <= '1';
              halfWord <= crc16Next;
            else
              -- REMARK: The current CRC has to be updated when this is written.
              writeContent_o <= '1';
              writeContentData <= halfWord & crc16Next;
              packetPosition <= packetPosition + 1;
              halfWordPending <= '0';
              halfWord <= crc16Next;
            end if;
          end if;
          
          state <= CONTENT_GET;

        when CRC_APPEND =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (halfWordPending = '0') then
            writeContent_o <= '1';
            writeContentData <= crc16Current & x"0000";
            packetPosition <= packetPosition + 1;
          else
            writeContent_o <= '1';
            writeContentData <= halfWord & crc16Current;
            packetPosition <= packetPosition + 1;
          end if;

          state <= SEND_FRAME;

        when SEND_FRAME =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          writeFrame_o <= '1';
          state <= WAIT_UPDATE;

        when RESTART_FRAME =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          writeFrameAbort_o <= '1';
          state <= WAIT_UPDATE;

        when WAIT_UPDATE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          state <= IDLE;
          
        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Packet CRC calculation.
  -----------------------------------------------------------------------------

  Crc16High: Crc16CITT
    port map(
      d_i=>writeContentData(31 downto 16), crc_i=>crc16Current, crc_o=>crc16Temp);
  Crc16Low: Crc16CITT
    port map(
      d_i=>writeContentData(15 downto 0), crc_i=>crc16Temp, crc_o=>crc16Next);

end architecture;





-------------------------------------------------------------------------------
-- RioLogicalMaintenanceRequest
-- This logical layer module handles ingress maintenance requests.
-- Addresses: 0x80 (maint read request) and 0x81 (maint write request).
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for RioLogicalMaintenanceRequest.
-------------------------------------------------------------------------------
entity RioLogicalMaintenanceRequest is
  generic(
    DEVICE_IDENTITY : std_logic_vector(15 downto 0);
    DEVICE_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
    DEVICE_REV : std_logic_vector(31 downto 0);
    ASSY_IDENTITY : std_logic_vector(15 downto 0);
    ASSY_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
    ASSY_REV : std_logic_vector(15 downto 0);
    DEFAULT_BASE_DEVICE_ID : std_logic_vector(15 downto 0) := x"ffff");
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    configStb_o : out std_logic;
    configWe_o : out std_logic;
    configAdr_o : out std_logic_vector(23 downto 0);
    configDat_o : out std_logic_vector(63 downto 0);
    configSel_o : out std_logic_vector(7 downto 0);
    configDat_i : in std_logic_vector(63 downto 0);
    configAck_i : in std_logic;
    
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveAdr_i : in std_logic_vector(7 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic;

    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioLogicalMaintenanceRequest of RioLogicalMaintenanceRequest is
  component MemorySinglePort is
    generic(
      ADDRESS_WIDTH : natural := 1;
      DATA_WIDTH : natural := 1);
    port(
      clk_i : in std_logic;
      enable_i : in std_logic;
      writeEnable_i : in std_logic;
      address_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      data_i : in std_logic_vector(DATA_WIDTH-1 downto 0);
      data_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;
  
begin

  slaveAck_o <= slaveAck;
  MaintenanceRequest: process(clk, areset_n)
  begin
    if (areset_n = '0') then

    elsif (clk'event and clk = '1') then
      case state is
        when WAIT_PACKET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (slaveCyc_i = '1') then
            if (slaveAck = '0') then
              if (slaveStb_i = '1') then
                if (slaveAddress_i = x"80") then
                  -- Maintenance read request.
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      header <= slaveDat_i(15 downto 0);
                    when 1 =>
                      -- destId
                      destId <= slaveDat_i;
                    when 2 =>
                      -- srcId
                      srcId <= slaveDat_i;
                    when 3 =>
                      -- transaction & rdsize & srcTID & hop & config_offset(20:13)
                      size <= slaveDat_i(27 downto 24);
                      srcTid <= slaveDat_i(23 downto 16);
                      configOffset(20 downto 13) <= slaveDat_i(7 downto 0);
                    when 4 =>
                      -- config_offset(12:0) & wdptr & rsrv & crc(15:0)
                      configOffset(12 downto 0) <= slaveDat_i(31 downto 16);
                      wdptr <= slaveDat_i(18);
                      maintReadComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance read request.
                      -- Discard.
                  end case;
                elsif (slaveAddress_i = x"81") then
                  -- Maintenance write request.
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      header <= slaveDat_i(15 downto 0);
                    when 1 =>
                      -- destId
                      destId <= slaveDat_i;
                    when 2 =>
                      -- srcId
                      srcId <= slaveDat_i;
                    when 3 =>
                      -- transaction & wrsize & srcTID & hop & config_offset(20:13)
                      size <= slaveDat_i(27 downto 24);
                      srcTid <= slaveDat_i(23 downto 16);
                      configOffset(20 downto 13) <= slaveDat_i(7 downto 0);
                    when 4 =>
                      -- config_offset(12:0) & wdptr & rsrv & double-word(63:48)
                      configOffset(12 downto 0) <= slaveDat_i(31 downto 16);
                      configData(63 downto 48) <= slaveData_i(15 downto 0);
                      wdptr <= slaveDat_i(18);
                      memoryEnable <= '1';
                      memoryAddress <= 0;
                    when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 =>
                      -- double-word(47:16)
                      configData(47 downto 16) <= slaveData_i;
                    when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 =>
                      -- double-word(15:0) & double-word(63:48)
                      memoryAddress <= memoryAddress + 1;
                      memoryWrite <= '1';
                      memoryDataIn <= configData(63 downto 16) & slaveData_i(31 downto 16);
                      configData(63 downto 48) <= slaveData_i(15 downto 0);
                      maintWriteComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance read request.
                      -- Discard.
                  end case;
                end if;
                slaveAck <= '1';
              end if;
            else
              packetIndex <= packetIndex + 1;
              slaveAck <= '0';
            end if;
          else
            if (maintReadComplete = '1') then
              state <= CONFIG_READ;
              configIterator <= bytes;
              memoryEnable <= '1';
              memoryAddress <= 0;
            end if;
            if (maintWriteComplete = '1') then
              state <= CONFIG_WRITE;
            end if;
            packetIndex <= 0;
            maintReadComplete <= '0';
            maintWriteComplete <= '0';
          end if;

        when CONFIG_READ =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          configStb_o <= '1';
          configWe_o <= '0';
          configAdr_o <= configOffset;
          configSel_o <= byteLanes;
          configIterator <= configIterator - 1;
          configSpaceState <= CONFIG_READ_ACK;

        when CONFIG_READ_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            memoryAddress <= memoryAddress + 1;
            memoryWrite <= '1';
            memoryDataIn <= configSpaceDat_i;
            
            if (configIterator /= 0) then
              configAdr_o <= configAdr_o + 1;
              state <= CONFIG_READ;
            else
              configStb_o <= '0';
              packetIndex <= 0;
              state <= CONFIG_READ_RESPONSE;
            end if;
          end if;

        when CONFIG_READ_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          masterCyc_o <= '1';
          masterStb_o <= '1';
          masterDat_o <= header;
          packetIndex <= packetIndex + 1;
          state <= CONFIG_READ_RESPONSE_ACK;

        when CONFIG_READ_RESPONSE_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            masterCyc_o <= '1';
            masterStb_o <= '1';
            case (packetIndex) is
              when 0 =>
                -- x"0000" & ackid & vc & crf & prio & tt & ftype
                masterDat_o <= header;
              when 1 =>
                -- destination is the source.
                masterDat_o <= srcId;
              when 2 =>
                -- source is the destination.
                masterDat_o <= destId;
              when 3 =>
                -- transaction & status & targetTID & hop & reserved(7:0)
                masterDat_o <= "0010" & "0000" & srcTid & x"ff" & x"00";
              when 4 =>
                -- reserved(15:0) & double-word0(63:32)
                masterDat_o <= x"0000" & memoryDataOut(63 downto 32);
              when 5 =>
                masterDat_o <= memoryDataOut(31 downto 0) & x"0000";
              -- REMARK: Add more here to send the full response...
              when others =>
                state <= WAIT_PACKET;
            end case;
            packetIndex <= packetIndex + 1;
          end if;

        when CONFIG_WRITE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------

        when others =>

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  --
  -----------------------------------------------------------------------------
  -- REMARK: Make this a common component?
  -- REMARK: Change bytes to double-words?
  process(wdptr, size)
  begin
    case (wdptr & size) is 
      when "00000" =>
        bytes <= 1;
        byteLanes <= "10000000";
      when "00001" =>
        bytes <= 1;
        byteLanes <= "01000000";
        
      when "00010" =>
        bytes <= 1;
        byteLanes <= "00100000";
      when "00011" =>
        bytes <= 1;
        byteLanes <= "00010000";
        
      when "10000" =>
        bytes <= 1;
        byteLanes <= "00001000";
      when "10001" =>
        bytes <= 1;
        byteLanes <= "00000100";
        
      when "10010" =>
        bytes <= 1;
        byteLanes <= "00000010";
      when "10011" =>
        bytes <= 1;
        byteLanes <= "00000001";
        
      when "00100" =>
        bytes <= 2;
        byteLanes <= "11000000";
      when "00101" =>
        bytes <= 3;
        byteLanes <= "11100000";
        
      when "00110" =>
        bytes <= 2;
        byteLanes <= "00110000";
      when "00111" =>
        bytes <= 5;
        byteLanes <= "11111000";
        
      when "10100" =>
        bytes <= 2;
        byteLanes <= "00001100";
      when "10101" =>
        bytes <= 3;
        byteLanes <= "00000111";
        
      when "10110" =>
        bytes <= 2;
        byteLanes <= "00000011";
      when "10111" =>
        bytes <= 5;
        byteLanes <= "00011111";
        
      when "01000" =>
        bytes <= 4;
        byteLanes <= "11110000";
      when "11000" =>
        bytes <= 4;
        byteLanes <= "00001111";
        
      when "01001" =>
        bytes <= 6;
        byteLanes <= "11111100";
      when "11001" =>
        bytes <= 6;
        byteLanes <= "00111111";
        
      when "01010" =>
        bytes <= 7;
        byteLanes <= "11111110";
      when "11010" =>
        bytes <= 7;
        byteLanes <= "01111111";
        
      when "01011" =>
        bytes <= 8;
        byteLanes <= "11111111";
      when "11011" =>
        bytes <= 16;
        byteLanes <= "11111111";
        
      when "01100" =>
        bytes <= 32;
        byteLanes <= "11111111";
      when "11100" =>
        bytes <= 64;
        byteLanes <= "11111111";
        
      when "01101" =>
        bytes <= 96;
        byteLanes <= "11111111";
      when "11101" =>
        bytes <= 128;
        byteLanes <= "11111111";
        
      when "01110" =>
        bytes <= 160;
        byteLanes <= "11111111";
      when "11110" =>
        bytes <= 192;
        byteLanes <= "11111111";
        
      when "01111" =>
        bytes <= 224;
        byteLanes <= "11111111";
      when "11111" =>
        bytes <= 256;
        byteLanes <= "11111111";

    end case;
  end process;
  
end architecture;


entity MaintenanceReadRequestInbound is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveAdr_i : in std_logic_vector(7 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic;

    header_o : out std_logic_vector(15 downto 0);
    dstId_o : out std_logic_vector(31 downto 0);
    srcId_o : out std_logic_vector(31 downto 0);
    tid_o : out std_logic_vector(7 downto 0);
    configOffset_o : out std_logic_vector(21 downto 0);
    configLength_o : out std_logic_vector(3 downto 0);
    configSelect_o : out std_logic_vector(7 downto 0);
    ready_o : out std_logic;
    done_i : in std_logic);
end entity;


architecture MaintenanceReadRequestInbound of MaintenanceReadRequestInbound is
  
begin

  ready_o <= maintReadComplete;
  slaveAck_o <= slaveAck;
  MaintenanceReadRequest: process(clk, areset_n)
  begin
    if (areset_n = '0') then

    elsif (clk'event and clk = '1') then
      case state is
        when WAIT_PACKET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (slaveCyc_i = '1') then
            if (slaveAck = '0') then
              if (slaveStb_i = '1') then
                if (slaveAddress_i = x"80") then
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      header <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- destid
                      destId <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcid
                      srcId <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction & rdsize & srcTID & hop & config_offset(20:13)
                      size <= slaveDat_i(27 downto 24);
                      srcTid <= slaveDat_i(23 downto 16);
                      configOffset(20 downto 13) <= slaveDat_i(7 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- config_offset(12:0) & wdptr & rsrv & crc(15:0)
                      configOffset(12 downto 0) <= slaveDat_i(31 downto 16);
                      wdptr <= slaveDat_i(18);
                      packetIndex <= packetIndex + 1;
                      maintReadComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance read request.
                      -- Discard.
                  end case;
                end if;
                slaveAck <= '1';
              end if;
            else
              slaveAck <= '0';
            end if;
          else
            if (maintReadComplete = '1') then
              state <= READY;
            end if;
            packetIndex <= 0;
          end if;

        when READY =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (done_i = '1') then
            maintReadComplete <= '0';
            state <= WAIT_PACKET;
          end if;
          
        when others =>

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  --
  -----------------------------------------------------------------------------
  -- REMARK: Make this a common component?
  -- REMARK: Change bytes to double-words?
  process(wdptr, size)
  begin
    case (wdptr & size) is 
      when "00000" =>
        bytes <= 1;
        byteLanes <= "10000000";
      when "00001" =>
        bytes <= 1;
        byteLanes <= "01000000";
        
      when "00010" =>
        bytes <= 1;
        byteLanes <= "00100000";
      when "00011" =>
        bytes <= 1;
        byteLanes <= "00010000";
        
      when "10000" =>
        bytes <= 1;
        byteLanes <= "00001000";
      when "10001" =>
        bytes <= 1;
        byteLanes <= "00000100";
        
      when "10010" =>
        bytes <= 1;
        byteLanes <= "00000010";
      when "10011" =>
        bytes <= 1;
        byteLanes <= "00000001";
        
      when "00100" =>
        bytes <= 2;
        byteLanes <= "11000000";
      when "00101" =>
        bytes <= 3;
        byteLanes <= "11100000";
        
      when "00110" =>
        bytes <= 2;
        byteLanes <= "00110000";
      when "00111" =>
        bytes <= 5;
        byteLanes <= "11111000";
        
      when "10100" =>
        bytes <= 2;
        byteLanes <= "00001100";
      when "10101" =>
        bytes <= 3;
        byteLanes <= "00000111";
        
      when "10110" =>
        bytes <= 2;
        byteLanes <= "00000011";
      when "10111" =>
        bytes <= 5;
        byteLanes <= "00011111";
        
      when "01000" =>
        bytes <= 4;
        byteLanes <= "11110000";
      when "11000" =>
        bytes <= 4;
        byteLanes <= "00001111";
        
      when "01001" =>
        bytes <= 6;
        byteLanes <= "11111100";
      when "11001" =>
        bytes <= 6;
        byteLanes <= "00111111";
        
      when "01010" =>
        bytes <= 7;
        byteLanes <= "11111110";
      when "11010" =>
        bytes <= 7;
        byteLanes <= "01111111";
        
      when "01011" =>
        bytes <= 8;
        byteLanes <= "11111111";
      when "11011" =>
        bytes <= 16;
        byteLanes <= "11111111";
        
      when "01100" =>
        bytes <= 32;
        byteLanes <= "11111111";
      when "11100" =>
        bytes <= 64;
        byteLanes <= "11111111";
        
      when "01101" =>
        bytes <= 96;
        byteLanes <= "11111111";
      when "11101" =>
        bytes <= 128;
        byteLanes <= "11111111";
        
      when "01110" =>
        bytes <= 160;
        byteLanes <= "11111111";
      when "11110" =>
        bytes <= 192;
        byteLanes <= "11111111";
        
      when "01111" =>
        bytes <= 224;
        byteLanes <= "11111111";
      when "11111" =>
        bytes <= 256;
        byteLanes <= "11111111";

    end case;
  end process;
  
end architecture;
