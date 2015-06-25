----------------------------------------------------------------------------------
-- __/\\\\\\\\\\\\\\\_        ____/\\\\\\\\\_____        __/\\\\\\\\\\\\\\\_        
--  _\///////\\\/////__        __/\\\///////\\\___        _\///////\\\/////__       
--   _______\/\\\_______        _\/\\\_____\/\\\___        _______\/\\\_______      
--    _______\/\\\_______        _\/\\\\\\\\\\\/____        _______\/\\\_______     
--     _______\/\\\_______        _\/\\\//////\\\____        _______\/\\\_______    
--      _______\/\\\_______        _\/\\\____\//\\\___        _______\/\\\_______   
--       _______\/\\\_______        _\/\\\_____\//\\\__        _______\/\\\_______  
--        _______\/\\\_______        _\/\\\______\//\\\_        _______\/\\\_______ 
--         _______\///________        _\///________\///__        _______\///________
-- 
-- Create Date: 
-- Author:  arnaud.samama@thalesgroup.com
-- Design Name: 
-- Module Name: top - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------
-- 
-- Copyright (C) 2014 Authors 
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
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity dual_input_fifo is
port (  
    rst_n       : in std_logic;
    clk         : in std_logic;
    halfword_1  : in std_logic_vector(15 downto 0);
    halfword_2  : in std_logic_vector(15 downto 0);

    wr_type     : in std_logic_vector(1 downto 0);
    end_frame   : in std_logic;

    fullwordout : out std_logic_vector(31 downto 0);
    valid       : out std_logic;
    last_frame  : out std_logic;
    
    readword    : in std_logic

    
    );
end dual_input_fifo;

architecture Behavioral of dual_input_fifo is

    type memory_type is array (0 to 7) of std_logic_vector(17 downto 0);
    
    signal memory : memory_type :=(others => (others => '0'));   
    signal last_frame_q0 : std_logic;
 
    signal readptr,readptr2, writeptr : unsigned(2 downto 0)  ;  --read and write pointer   
    signal readptr_next,writeptr_next : unsigned(2 downto 0);
    signal valid_o : std_logic;
    constant BIT_ENDING : integer := 16;
    constant BIT_STARTING : integer := 17;
begin

    valid <= valid_o;
    fullwordout(31 downto 16)  <= memory(conv_integer( readptr  ))(15 downto 0);
    fullwordout(15 downto 0)   <= memory(conv_integer( readptr2 ))(15 downto 0);
    -- ASA last_frame_q0                 <= memory(conv_integer( readptr  ))(BIT_ENDING) or 
    last_frame                 <= memory(conv_integer( readptr  ))(BIT_ENDING) or 
                                          memory(conv_integer( readptr2 ))(BIT_ENDING);
    process(clk)

    begin
        if rising_edge(clk) then

            -- We have to assert the last frame, just after we have written the last full word
            -- ASA last_frame <= last_frame_q0;
        end if;
    end process;

    readptr2        <= readptr  + 1;
    
    readptr_next    <= readptr  + 2;
    writeptr_next   <= writeptr + 1;

    writing : process(clk, rst_n)
        variable write_inc : unsigned(writeptr'range);
        
    begin
        if rst_n = '0' then
            writeptr <= (others => '0'); 
            valid_o <= '0';            
        elsif rising_edge(clk) then
            write_inc := "000";
            
            if wr_type = "01" then
                memory(conv_integer(writeptr))(15 downto 0) <= halfword_1;
                memory(conv_integer(writeptr))(BIT_ENDING)  <= end_frame;
                write_inc := "001";

            elsif wr_type = "11" then
                memory(conv_integer(writeptr     ))(15 downto 0)    <= halfword_1;
                memory(conv_integer(writeptr_next))(15 downto 0)    <= halfword_2;
                
                memory(conv_integer(writeptr     ))(BIT_ENDING)     <= end_frame;
                memory(conv_integer(writeptr_next))(BIT_ENDING)     <= end_frame;
                
                write_inc := "010";
            end if;  
            
            writeptr <= writeptr + write_inc;  
            
            -- We need 2 half word between the write ptr and the read ptr
            --  to have a full word valid
            
            if readword = '1' and valid_o = '1' then
                if (readptr_next  /= (writeptr + write_inc ) )  and (readptr_next   /= (writeptr + write_inc -1)) then
                    valid_o <= '1';
                else
                    valid_o <= '0';
                end if;   
            else
                -- If no one if currently reading the data, then we look, if in the next cycle
                --  the data will be valid, i.e. we have at least 2 half word between the last read
                --  data, and the next location available.
                if (readptr  /= (writeptr ) )  and (readptr + 1  /= writeptr ) then
                    valid_o <= '1';
                else
                    valid_o <= '0';
                end if;   
            end if;
            
        end if;
        
    end process;
    
    
    reading: process(clk)
    begin
        if rst_n = '0' then
            readptr <= (others => '0');    
        elsif rising_edge(clk) then
            if readword = '1' and valid_o = '1' then
                readptr <= readptr + 2;
            end if;          
        end if;       
    end process;
    
    -- Data validity is where there is a complete word written
    -- validity: process(clk)
    -- begin
        -- if rst_n = '0' then
            -- valid_o <= '0';    
        -- elsif rising_edge(clk) then
            -- -- if the data is being written to evaluate the validity on the next cycle
            -- --  we have to compare the result of the write being proceeded
            -- if wr_dual = '1' then
                -- if (writeptr + 2) /= readptr_next and (writeptr + 2) - readptr_next /= 1 then
                    -- valid_o <= '1';
                -- end if;
                
            -- elsif writeptr /= readptr and  writeptr /= readptr + 1  then
                -- valid_o <= '1';
            -- else
                -- valid_o <= '0';
            -- end if;          
        -- end if;       
    -- end process;
    
    
end Behavioral;
