-------------------------------------------------------------------------------
-- RioLogicalCommon.
-------------------------------------------------------------------------------
-- Ingress:
-- * Removes in-the-middle-CRC.
-- * Forwards packets to logical-layer handlers depending on ftype and
--   transaction (output as address).
-- * Outputs header and deviceIDs in seperate accesses to facilitate 8- and
--   16-bit deviceAddress support. All fields are right-justified.
-- Egress:
-- * Adds CRC.
-- * Receives packets from logical-layer handlers.
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
    masterAddress_o : out std_logic_vector(7 downto 0);
    masterData_o : out std_logic_vector(31 downto 0);
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
          masterAddress_o <= ftype & transaction;
          masterData_o <= x"0000" & packetContent(63 downto 48);
          packetContent <= packetContent(47 downto 0) & x"0000";

          state <= SEND_DESTINATION;

        when SEND_DESTINATION =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            if (tt = "00") then
              masterData_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              masterData_o <= x"0000" & packetContent(63 downto 48);
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
              masterData_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              masterData_o <= x"0000" & packetContent(63 downto 48);
              packetContent <= packetContent(47 downto 0) & x"0000";
            end if;

            state <= FORWARD;
          end if;
          
        when FORWARD =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            masterData_o <= packetContent(63 downto 32);

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
          if (masterAck_i = '1') then
            masterData_o <= packetContent(63 downto 32);
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
          if (writeFrameFull_i = '0') then
            state <= HEADER_GET;
            packetPosition <= 0;
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
              -- Not supported.
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

    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveAdr_i : in std_logic_vector(7 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic;

    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterSel_o : out std_logic_vector(3 downto 0);
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic);
end entity;

architecture RioLogicalMaintenanceRequest of RioLogicalMaintenanceRequest is
  
begin

  slaveAck_o <= slaveAck;
  MaintenanceRequest: process(clk, areset_n)
  begin
    if (areset_n = '0') then

    elsif (clk'event and clk = '1') then
      if (slaveCyc_i = '0') then
        packetIndex <= 0;
      elsif (slaveStb_i = '1') then
        if (slaveAck = '0') then
          if (slaveAddress_i = x"80") then
            -- Maintenance read request.
            case (packetIndex) is
              when 0 =>
                -- x"0000" & ackid & vc & crf & prio & tt & ftype
              when 1 =>
                -- destId
              when 2 =>
                -- srcId
              when 3 =>
                -- transaction & rdsize & srcTID & hop & config_offset(20:13)
              when 4 =>
                -- config_offset(12:0) & wdptr & rsrv & crc(15:0)
              when others =>
                -- REMARK: Larger packets not supported.
            end case;
          elsif (slaveAddress_i = x"81") then
            -- Maintenance write request.
            case (packetIndex) is
              when 0 =>
                -- x"0000" & ackid & vc & crf & prio & tt & ftype
              when 1 =>
                -- destId
              when 2 =>
                -- srcId
              when 3 =>
                -- transaction & wrsize & srcTID & hop & config_offset(20:13)
              when 4 =>
                -- config_offset(12:0) & wdptr & rsrv & double-word(63:48)
              when 5 =>
                -- double-word(47:16)
              when 6 =>
                -- double-word(15:0) & crc(15:0)
              when others =>
                -- REMARK: Larger packets not supported.
            end case;
          end if;
          slaveAck <= '1';
        else
          packetIndex <= packetIndex + 1;
          slaveAck <= '0';
        end if;
      end if;
    end if;
  end process;


  -----------------------------------------------------------------------------
  -- findInPayload
  -- find out number of the bytes and first byte's position in the payload.
  -----------------------------------------------------------------------------
  findInPayload: process(wdptr, size)
  begin
    case size is 
      when "0000" =>
        reserved <= '0';
        numberOfByte <= 1;
        if wdptr = '1' then
          pos <= 4;
        else
          pos <= 0;
        end if;
      when "0001" =>
        reserved <= '0';
        numberOfByte <= 1;
        if wdptr = '1' then
          pos <= 5;
        else
          pos <= 1;
        end if;
      when "0010" =>
        reserved <= '0';
        numberOfByte <= 1;
        if wdptr = '1' then
          pos <= 6;
        else
          pos <= 2;
        end if;
      when "0011" =>
        reserved <= '0';
        numberOfByte <= 1;
        if wdptr = '1' then
          pos <= 7;
        else
          pos <= 3;
        end if;
      when "0100" =>
        reserved <= '0';
        numberOfByte <= 2;
        if wdptr = '1' then
          pos <= 4;
        else
          pos <= 0;
        end if;
      when "0101" =>
        reserved <= '0';
        numberOfByte <= 3;
        if wdptr = '1' then
          pos <= 5;
        else
          pos <= 0;
        end if;
      when "0110" =>
        reserved <= '0';
        numberOfByte <= 2;
        if wdptr = '1' then
          pos <= 6;
        else
          pos <= 2;
        end if;
      when "0111" =>
        reserved <= '0';
        numberOfByte <= 5;
        if wdptr = '1' then
          pos <= 3;
        else
          pos <= 0;
        end if;
      when "1000" =>
        reserved <= '0';
        numberOfByte <= 4;
        if wdptr = '1' then
          pos <= 4;
        else
          pos <= 0;
        end if;
      when "1001" =>
        reserved <= '0';
        numberOfByte <= 6;
        if wdptr = '1' then
          pos <= 2;
        else
          pos <= 0;
        end if;
      when "1010" =>
        reserved <= '0';
        numberOfByte <= 7;
        if wdptr = '1' then
          pos <= 1;
        else
          pos <= 0;
        end if;
      when "1011" =>
        reserved <= '0';
        if wdptr = '1' then
          numberOfByte <= 16;
        else
          numberOfByte <= 8;
        end if;
        pos <= 0;
      when "1100" =>
        reserved <= '0';
        if wdptr = '1' then
          numberOfByte <= 64;
        else
          numberOfByte <= 32;
        end if;
        pos <= 0;
      when "1101" =>
        if wdptr = '1' then
          reserved <= '0';
          numberOfByte <= 128;
        else
          reserved <= '1';
          numberOfByte <= 96;
        end if;
        pos <= 0;
      when "1110" =>
        if wdptr = '1' then
          numberOfByte <= 192;
        else
          numberOfByte <= 160;
        end if;
        reserved <= '1';
        pos <= 0;
      when "1111" =>
        if wdptr = '1' then
          reserved <= '0';
          numberOfByte <= 256;
        else
          reserved <= '1';
          numberOfByte <= 224;
        end if;
        pos <= 0;
      when others =>
        reserved <= '1';
        numberOfByte <= 0;
        pos <= 0;
    end case;
  end process;
  
end architecture;
