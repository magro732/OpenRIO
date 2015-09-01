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

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity srio_gt_wrapper_v6_4x is
	port(
		SYSCLK_IN 					: out std_logic;
		SYSCLK_IN_P                	: in   std_logic;
		SYSCLK_IN_N                 : in   std_logic;
			
		RXUSRCLK : OUT std_logic;
		RXUSRCLK2 : OUT std_logic;
		TXUSRCLK : OUT std_logic;
		TXUSRCLK2 : OUT std_logic;
		
		-- PAD where is connected the reference clock driving the MGT
		CLK0_GTREFCLK_PAD_N: IN std_logic;
		CLK0_GTREFCLK_PAD_P: IN std_logic;

		CLK1_GTREFCLK_PAD_N: IN std_logic;
		CLK1_GTREFCLK_PAD_P: IN std_logic;
		
		GTXRESET : IN std_logic;--
		RXBUFRST : IN std_logic;--
		
		RXN0 : IN std_logic;--
		RXN1 : IN std_logic;
		RXN2 : IN std_logic;
		RXN3 : IN std_logic;
		RXP0 : IN std_logic;--
		RXP1 : IN std_logic;
		RXP2 : IN std_logic;
		RXP3 : IN std_logic;
		TXINHIBIT           : in  std_logic_vector(0 to 3);
		ENCHANSYNC : IN std_logic;
		TXDATA0 : IN std_logic_vector(15 downto 0);--
		TXDATA1 : IN std_logic_vector(15 downto 0);
		TXDATA2 : IN std_logic_vector(15 downto 0);
		TXDATA3 : IN std_logic_vector(15 downto 0);
		TXCHARISK0 : IN std_logic_vector(1 downto 0);--
		TXCHARISK1 : IN std_logic_vector(1 downto 0);
		TXCHARISK2 : IN std_logic_vector(1 downto 0);
		TXCHARISK3 : IN std_logic_vector(1 downto 0);          
		TXN0 : OUT std_logic;
		TXN1 : OUT std_logic;
		TXN2 : OUT std_logic;
		TXN3 : OUT std_logic;
		TXP0 : OUT std_logic;
		TXP1 : OUT std_logic;
		TXP2 : OUT std_logic;
		TXP3 : OUT std_logic;
		PLLLKDET : OUT std_logic;
		RXDATA0 : OUT std_logic_vector(15 downto 0);--
		RXDATA1 : OUT std_logic_vector(15 downto 0);
		RXDATA2 : OUT std_logic_vector(15 downto 0);
		RXDATA3 : OUT std_logic_vector(15 downto 0);
		
		RXDATA0VALID : IN std_logic;
		RXDATA1VALID : IN std_logic;
		RXDATA2VALID : IN std_logic;
		RXDATA3VALID : IN std_logic;
		
		RXCHARISK0 : OUT std_logic_vector(1 downto 0);--
		RXCHARISK1 : OUT std_logic_vector(1 downto 0);
		RXCHARISK2 : OUT std_logic_vector(1 downto 0);
		RXCHARISK3 : OUT std_logic_vector(1 downto 0);
		RXCHARISCOMMA0 : OUT std_logic_vector(1 downto 0);--
		RXCHARISCOMMA1 : OUT std_logic_vector(1 downto 0);
		RXCHARISCOMMA2 : OUT std_logic_vector(1 downto 0);
		RXCHARISCOMMA3 : OUT std_logic_vector(1 downto 0);
        RXBYTEISALIGNED: OUT  std_logic_vector(3 downto 0);--
        RXBYTEREALIGN  : OUT  std_logic_vector(3 downto 0);--
        RXELECIDLE     : OUT std_logic_vector(3 downto 0);
		RXDISPERR0 : OUT std_logic_vector(1 downto 0);--
		RXDISPERR1 : OUT std_logic_vector(1 downto 0);
		RXDISPERR2 : OUT std_logic_vector(1 downto 0);
		RXDISPERR3 : OUT std_logic_vector(1 downto 0);
		RXNOTINTABLE0 : OUT std_logic_vector(1 downto 0);--
		RXNOTINTABLE1 : OUT std_logic_vector(1 downto 0);
		RXNOTINTABLE2 : OUT std_logic_vector(1 downto 0);
		RXNOTINTABLE3 : OUT std_logic_vector(1 downto 0);
		RXBUFERR : OUT std_logic;
		CHBONDDONE0 : OUT std_logic;--
		CHBONDDONE1 : OUT std_logic;
		CHBONDDONE2 : OUT std_logic;
		CHBONDDONE3 : OUT std_logic;
        RXCLKSTABLE0 : OUT std_logic;--
		RXCLKSTABLE1 : OUT std_logic;
		RXCLKSTABLE2 : OUT std_logic;
		RXCLKSTABLE3 : OUT std_logic;
		LOOPBACK0 : IN std_logic_vector(2 downto 0); --
		LOOPBACK1 : IN std_logic_vector(2 downto 0);
		LOOPBACK2 : IN std_logic_vector(2 downto 0);
		LOOPBACK3 : IN std_logic_vector(2 downto 0)
		);
end srio_gt_wrapper_v6_4x;

architecture Behavioral of srio_gt_wrapper_v6_4x is

	component gtwizard_0
	port
	(
		SOFT_RESET_IN                           : in   std_logic;
		DONT_RESET_ON_DATA_ERROR_IN             : in   std_logic;
		--Q0_CLK0_GTREFCLK_PAD_N_IN               : in   std_logic;
		--Q0_CLK0_GTREFCLK_PAD_P_IN               : in   std_logic;
		--Q0_CLK1_GTREFCLK_PAD_N_IN               : in   std_logic;
		--Q0_CLK1_GTREFCLK_PAD_P_IN               : in   std_logic;

		Q0_CLK1_GTREFCLK_PAD_N_IN               : in   std_logic;
    		Q0_CLK1_GTREFCLK_PAD_P_IN               : in   std_logic;

		GT0_TX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT0_RX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT0_DATA_VALID_IN                       : in   std_logic;
		GT1_TX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT1_RX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT1_DATA_VALID_IN                       : in   std_logic;
		GT2_TX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT2_RX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT2_DATA_VALID_IN                       : in   std_logic;
		GT3_TX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT3_RX_FSM_RESET_DONE_OUT               : out  std_logic;
		GT3_DATA_VALID_IN                       : in   std_logic;
	 
		GT0_TXUSRCLK_OUT                        : out  std_logic;
		GT0_TXUSRCLK2_OUT                       : out  std_logic;
		GT0_RXUSRCLK_OUT                        : out  std_logic;
		GT0_RXUSRCLK2_OUT                       : out  std_logic;
	 
		GT1_TXUSRCLK_OUT                        : out  std_logic;
		GT1_TXUSRCLK2_OUT                       : out  std_logic;
		GT1_RXUSRCLK_OUT                        : out  std_logic;
		GT1_RXUSRCLK2_OUT                       : out  std_logic;
	 
		GT2_TXUSRCLK_OUT                        : out  std_logic;
		GT2_TXUSRCLK2_OUT                       : out  std_logic;
		GT2_RXUSRCLK_OUT                        : out  std_logic;
		GT2_RXUSRCLK2_OUT                       : out  std_logic;
	 
		GT3_TXUSRCLK_OUT                        : out  std_logic;
		GT3_TXUSRCLK2_OUT                       : out  std_logic;
		GT3_RXUSRCLK_OUT                        : out  std_logic;
		GT3_RXUSRCLK2_OUT                       : out  std_logic;

		--_________________________________________________________________________
		--GT0  (X0Y0)
		--____________________________CHANNEL PORTS________________________________
		--------------------------------- CPLL Ports -------------------------------
		gt0_cpllfbclklost_out                   : out  std_logic;
		gt0_cplllock_out                        : out  std_logic;
		gt0_cpllreset_in                        : in   std_logic;
		-------------------------- Channel - Clocking Ports ------------------------
		--gt0_gtrefclk0_in                        : in   std_logic;
		---------------------------- Channel - DRP Ports  --------------------------
		gt0_drpaddr_in                          : in   std_logic_vector(8 downto 0);
		gt0_drpdi_in                            : in   std_logic_vector(15 downto 0);
		gt0_drpdo_out                           : out  std_logic_vector(15 downto 0);
		gt0_drpen_in                            : in   std_logic;
		gt0_drprdy_out                          : out  std_logic;
		gt0_drpwe_in                            : in   std_logic;
		--------------------------- Digital Monitor Ports --------------------------
		gt0_dmonitorout_out                     : out  std_logic_vector(7 downto 0);
		------------------------------- Loopback Ports -----------------------------
		gt0_loopback_in                         : in   std_logic_vector(2 downto 0);
		--------------------- RX Initialization and Reset Ports --------------------
		gt0_eyescanreset_in                     : in   std_logic;
		gt0_rxuserrdy_in                        : in   std_logic;
		-------------------------- RX Margin Analysis Ports ------------------------
		gt0_eyescandataerror_out                : out  std_logic;
		gt0_eyescantrigger_in                   : in   std_logic;
		------------------- Receive Ports - Clock Correction Ports -----------------
		gt0_rxclkcorcnt_out                     : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - FPGA RX interface Ports -----------------
		gt0_rxdata_out                          : out  std_logic_vector(15 downto 0);
		------------------- Receive Ports - Pattern Checker Ports ------------------
		gt0_rxprbserr_out                       : out  std_logic;
		gt0_rxprbssel_in                        : in   std_logic_vector(2 downto 0);
		------------------- Receive Ports - Pattern Checker ports ------------------
		gt0_rxprbscntreset_in                   : in   std_logic;
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
		gt0_rxdisperr_out                       : out  std_logic_vector(1 downto 0);
		gt0_rxnotintable_out                    : out  std_logic_vector(1 downto 0);
		--------------------------- Receive Ports - RX AFE -------------------------
		gt0_gtxrxp_in                           : in   std_logic;
		------------------------ Receive Ports - RX AFE Ports ----------------------
		gt0_gtxrxn_in                           : in   std_logic;
        ------------------- Receive Ports - RX Buffer Bypass Ports -----------------
        ----gt0_rxstatus_out                        : out  std_logic_vector(2 downto 0);
		------------------ Receive Ports - RX Channel Bonding Ports ----------------
		gt0_rxchanbondseq_out                   : out  std_logic;
		gt0_rxchbonden_in                       : in   std_logic;
		gt0_rxchbondlevel_in                    : in   std_logic_vector(2 downto 0);
		gt0_rxchbondmaster_in                   : in   std_logic;
		gt0_rxchbondo_out                       : out  std_logic_vector(4 downto 0);
		gt0_rxchbondslave_in                    : in   std_logic;
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
		gt0_rxchanisaligned_out                 : out  std_logic;
		gt0_rxchanrealign_out                   : out  std_logic;
		--------------------- Receive Ports - RX Equalizer Ports -------------------
		gt0_rxdfelpmreset_in                    : in   std_logic;
		gt0_rxmonitorout_out                    : out  std_logic_vector(6 downto 0);
		gt0_rxmonitorsel_in                     : in   std_logic_vector(1 downto 0);
		------------- Receive Ports - RX Initialization and Reset Ports ------------
		gt0_gtrxreset_in                        : in   std_logic;
		gt0_rxpmareset_in                       : in   std_logic;
		------------------ Receive Ports - RX OOB signalling Ports -----------------
		gt0_rxelecidle_out                      : out  std_logic;
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
		gt0_rxchariscomma_out                   : out  std_logic_vector(1 downto 0);
		gt0_rxcharisk_out                       : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
		gt0_rxchbondi_in                        : in   std_logic_vector(4 downto 0);
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
		gt0_rxresetdone_out                     : out  std_logic;
		--------------------- TX Initialization and Reset Ports --------------------
		gt0_gttxreset_in                        : in   std_logic;
		gt0_txuserrdy_in                        : in   std_logic;
		------------------ Transmit Ports - Pattern Generator Ports ----------------
		--gt0_txprbsforceerr_in                   : in   std_logic;
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
		gt0_txinhibit_in                        : in   std_logic;
		------------------ Transmit Ports - TX Data Path interface -----------------
		gt0_txdata_in                           : in   std_logic_vector(15 downto 0);
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
		gt0_gtxtxn_out                          : out  std_logic;
		gt0_gtxtxp_out                          : out  std_logic;
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
		gt0_txoutclkfabric_out                  : out  std_logic;
		gt0_txoutclkpcs_out                     : out  std_logic;
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
		gt0_txcharisk_in                        : in   std_logic_vector(1 downto 0);
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
		gt0_txresetdone_out                     : out  std_logic;
		------------------ Transmit Ports - pattern Generator Ports ----------------
		--gt0_txprbssel_in                        : in   std_logic_vector(2 downto 0);

		--GT1  (X0Y1)
		--____________________________CHANNEL PORTS________________________________
		-----------------   --------------------------------- CPLL Ports -------------------------------
		gt1_cpllfbclklost_out                   : out  std_logic;
		gt1_cplllock_out                        : out  std_logic;
		gt1_cpllreset_in                        : in   std_logic;
		-------------------------- Channel - Clocking Ports ------------------------
		--gt1_gtrefclk0_in                        : in   std_logic;
		----------- Channel - DRP Ports  --------------------------
		gt1_drpaddr_in                          : in   std_logic_vector(8 downto 0);
		gt1_drpdi_in                            : in   std_logic_vector(15 downto 0);
		gt1_drpdo_out                           : out  std_logic_vector(15 downto 0);
		gt1_drpen_in                            : in   std_logic;
		gt1_drprdy_out                          : out  std_logic;
		gt1_drpwe_in                            : in   std_logic;
		--------------------------- Digital Monitor Ports --------------------------
		gt1_dmonitorout_out                     : out  std_logic_vector(7 downto 0);
		------------------------------- Loopback Ports -----------------------------
		gt1_loopback_in                         : in   std_logic_vector(2 downto 0);
		--------------------- RX Initialization and Reset Ports --------------------
		gt1_eyescanreset_in                     : in   std_logic;
		gt1_rxuserrdy_in                        : in   std_logic;
		-------------------------- RX Margin Analysis Ports ------------------------
		gt1_eyescandataerror_out                : out  std_logic;
		gt1_eyescantrigger_in                   : in   std_logic;
		------------------- Receive Ports - Clock Correction Ports -----------------
		gt1_rxclkcorcnt_out                     : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - FPGA RX interface Ports -----------------
		gt1_rxdata_out                          : out  std_logic_vector(15 downto 0);
		------------------- Receive Ports - Pattern Checker Ports ------------------
		gt1_rxprbserr_out                       : out  std_logic;
		gt1_rxprbssel_in                        : in   std_logic_vector(2 downto 0);
		------------------- Receive Ports - Pattern Checker ports ------------------
		gt1_rxprbscntreset_in                   : in   std_logic;
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
		gt1_rxdisperr_out                       : out  std_logic_vector(1 downto 0);
		gt1_rxnotintable_out                    : out  std_logic_vector(1 downto 0);
		--------------------------- Receive Ports - RX AFE -------------------------
		gt1_gtxrxp_in                           : in   std_logic;
		------------------------ Receive Ports - RX AFE Ports ----------------------
		gt1_gtxrxn_in                           : in   std_logic;
        ------------------- Receive Ports - RX Buffer Bypass Ports -----------------
        --gt1_rxstatus_out                        : out  std_logic_vector(2 downto 0);
		------------------ Receive Ports - RX Channel Bonding Ports ----------------
		gt1_rxchanbondseq_out                   : out  std_logic;
		gt1_rxchbonden_in                       : in   std_logic;
		gt1_rxchbondlevel_in                    : in   std_logic_vector(2 downto 0);
		gt1_rxchbondmaster_in                   : in   std_logic;
		gt1_rxchbondo_out                       : out  std_logic_vector(4 downto 0);
		gt1_rxchbondslave_in                    : in   std_logic;
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
		gt1_rxchanisaligned_out                 : out  std_logic;
		gt1_rxchanrealign_out                   : out  std_logic;
		--------------------- Receive Ports - RX Equalizer Ports -------------------
		gt1_rxdfelpmreset_in                    : in   std_logic;
		gt1_rxmonitorout_out                    : out  std_logic_vector(6 downto 0);
		gt1_rxmonitorsel_in                     : in   std_logic_vector(1 downto 0);
		------------- Receive Ports - RX Initialization and Reset Ports ------------
		gt1_gtrxreset_in                        : in   std_logic;
		gt1_rxpmareset_in                       : in   std_logic;
		------------------ Receive Ports - RX OOB signalling Ports -----------------
		gt1_rxelecidle_out                      : out  std_logic;
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
		gt1_rxchariscomma_out                   : out  std_logic_vector(1 downto 0);
		gt1_rxcharisk_out                       : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
		gt1_rxchbondi_in                        : in   std_logic_vector(4 downto 0);
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
		gt1_rxresetdone_out                     : out  std_logic;
		--------------------- TX Initialization and Reset Ports --------------------
		gt1_gttxreset_in                        : in   std_logic;
		gt1_txuserrdy_in                        : in   std_logic;
		------------------ Transmit Ports - Pattern Generator Ports ----------------
		--gt1_txprbsforceerr_in                   : in   std_logic;
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
		gt1_txinhibit_in                        : in   std_logic;
		------------------ Transmit Ports - TX Data Path interface -----------------
		gt1_txdata_in                           : in   std_logic_vector(15 downto 0);
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
		gt1_gtxtxn_out                          : out  std_logic;
		gt1_gtxtxp_out                          : out  std_logic;
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
		gt1_txoutclkfabric_out                  : out  std_logic;
		gt1_txoutclkpcs_out                     : out  std_logic;
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
		gt1_txcharisk_in                        : in   std_logic_vector(1 downto 0);
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
		gt1_txresetdone_out                     : out  std_logic;
		------------------ Transmit Ports - pattern Generator Ports ----------------
		--gt1_txprbssel_in                        : in   std_logic_vector(2 downto 0);

		--GT2  (X0Y2)
		--____________________________CHANNEL PORTS________________________________
		--------------------   --------------------------------- CPLL Ports -------------------------------
		gt2_cpllfbclklost_out                   : out  std_logic;
		gt2_cplllock_out                        : out  std_logic;
		gt2_cpllreset_in                        : in   std_logic;
		-------------------------- Channel - Clocking Ports ------------------------
		--gt2_gtrefclk0_in                        : in   std_logic;
		-------- Channel - DRP Ports  --------------------------
		gt2_drpaddr_in                          : in   std_logic_vector(8 downto 0);
		gt2_drpdi_in                            : in   std_logic_vector(15 downto 0);
		gt2_drpdo_out                           : out  std_logic_vector(15 downto 0);
		gt2_drpen_in                            : in   std_logic;
		gt2_drprdy_out                          : out  std_logic;
		gt2_drpwe_in                            : in   std_logic;
		--------------------------- Digital Monitor Ports --------------------------
		gt2_dmonitorout_out                     : out  std_logic_vector(7 downto 0);
		------------------------------- Loopback Ports -----------------------------
		gt2_loopback_in                         : in   std_logic_vector(2 downto 0);
		--------------------- RX Initialization and Reset Ports --------------------
		gt2_eyescanreset_in                     : in   std_logic;
		gt2_rxuserrdy_in                        : in   std_logic;
		-------------------------- RX Margin Analysis Ports ------------------------
		gt2_eyescandataerror_out                : out  std_logic;
		gt2_eyescantrigger_in                   : in   std_logic;
		------------------- Receive Ports - Clock Correction Ports -----------------
		gt2_rxclkcorcnt_out                     : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - FPGA RX interface Ports -----------------
		gt2_rxdata_out                          : out  std_logic_vector(15 downto 0);
		------------------- Receive Ports - Pattern Checker Ports ------------------
		gt2_rxprbserr_out                       : out  std_logic;
		gt2_rxprbssel_in                        : in   std_logic_vector(2 downto 0);
		------------------- Receive Ports - Pattern Checker ports ------------------
		gt2_rxprbscntreset_in                   : in   std_logic;
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
		gt2_rxdisperr_out                       : out  std_logic_vector(1 downto 0);
		gt2_rxnotintable_out                    : out  std_logic_vector(1 downto 0);
		--------------------------- Receive Ports - RX AFE -------------------------
		gt2_gtxrxp_in                           : in   std_logic;
		------------------------ Receive Ports - RX AFE Ports ----------------------
		gt2_gtxrxn_in                           : in   std_logic;
        ------------------- Receive Ports - RX Buffer Bypass Ports -----------------
        --gt2_rxstatus_out                        : out  std_logic_vector(2 downto 0);
		------------------ Receive Ports - RX Channel Bonding Ports ----------------
		gt2_rxchanbondseq_out                   : out  std_logic;
		gt2_rxchbonden_in                       : in   std_logic;
		gt2_rxchbondlevel_in                    : in   std_logic_vector(2 downto 0);
		gt2_rxchbondmaster_in                   : in   std_logic;
		gt2_rxchbondo_out                       : out  std_logic_vector(4 downto 0);
		gt2_rxchbondslave_in                    : in   std_logic;
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
		gt2_rxchanisaligned_out                 : out  std_logic;
		gt2_rxchanrealign_out                   : out  std_logic;
		--------------------- Receive Ports - RX Equalizer Ports -------------------
		gt2_rxdfelpmreset_in                    : in   std_logic;
		gt2_rxmonitorout_out                    : out  std_logic_vector(6 downto 0);
		gt2_rxmonitorsel_in                     : in   std_logic_vector(1 downto 0);
		------------- Receive Ports - RX Initialization and Reset Ports ------------
		gt2_gtrxreset_in                        : in   std_logic;
		gt2_rxpmareset_in                       : in   std_logic;
		------------------ Receive Ports - RX OOB signalling Ports -----------------
		gt2_rxelecidle_out                      : out  std_logic;
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
		gt2_rxchariscomma_out                   : out  std_logic_vector(1 downto 0);
		gt2_rxcharisk_out                       : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
		gt2_rxchbondi_in                        : in   std_logic_vector(4 downto 0);
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
		gt2_rxresetdone_out                     : out  std_logic;
		--------------------- TX Initialization and Reset Ports --------------------
		gt2_gttxreset_in                        : in   std_logic;
		gt2_txuserrdy_in                        : in   std_logic;
		------------------ Transmit Ports - Pattern Generator Ports ----------------
		--gt2_txprbsforceerr_in                   : in   std_logic;
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
		gt2_txinhibit_in                        : in   std_logic;
		------------------ Transmit Ports - TX Data Path interface -----------------
		gt2_txdata_in                           : in   std_logic_vector(15 downto 0);
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
		gt2_gtxtxn_out                          : out  std_logic;
		gt2_gtxtxp_out                          : out  std_logic;
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
		gt2_txoutclkfabric_out                  : out  std_logic;
		gt2_txoutclkpcs_out                     : out  std_logic;
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
		gt2_txcharisk_in                        : in   std_logic_vector(1 downto 0);
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
		gt2_txresetdone_out                     : out  std_logic;
		------------------ Transmit Ports - pattern Generator Ports ----------------
		--gt2_txprbssel_in                        : in   std_logic_vector(2 downto 0);

		--GT3  (X0Y3)
		--____________________________CHANNEL PORTS________________________________
		---------    --------------------------------- CPLL Ports -------------------------------
		gt3_cpllfbclklost_out                   : out  std_logic;
		gt3_cplllock_out                        : out  std_logic;
		gt3_cpllreset_in                        : in   std_logic;
		-------------------------- Channel - Clocking Ports ------------------------
		--gt3_gtrefclk0_in                        : in   std_logic;
		------------------- Channel - DRP Ports  --------------------------
		gt3_drpaddr_in                          : in   std_logic_vector(8 downto 0);
		gt3_drpdi_in                            : in   std_logic_vector(15 downto 0);
		gt3_drpdo_out                           : out  std_logic_vector(15 downto 0);
		gt3_drpen_in                            : in   std_logic;
		gt3_drprdy_out                          : out  std_logic;
		gt3_drpwe_in                            : in   std_logic;
		--------------------------- Digital Monitor Ports --------------------------
		gt3_dmonitorout_out                     : out  std_logic_vector(7 downto 0);
		------------------------------- Loopback Ports -----------------------------
		gt3_loopback_in                         : in   std_logic_vector(2 downto 0);
		--------------------- RX Initialization and Reset Ports --------------------
		gt3_eyescanreset_in                     : in   std_logic;
		gt3_rxuserrdy_in                        : in   std_logic;
		-------------------------- RX Margin Analysis Ports ------------------------
		gt3_eyescandataerror_out                : out  std_logic;
		gt3_eyescantrigger_in                   : in   std_logic;
		------------------- Receive Ports - Clock Correction Ports -----------------
		gt3_rxclkcorcnt_out                     : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - FPGA RX interface Ports -----------------
		gt3_rxdata_out                          : out  std_logic_vector(15 downto 0);
		------------------- Receive Ports - Pattern Checker Ports ------------------
		gt3_rxprbserr_out                       : out  std_logic;
		gt3_rxprbssel_in                        : in   std_logic_vector(2 downto 0);
		------------------- Receive Ports - Pattern Checker ports ------------------
		gt3_rxprbscntreset_in                   : in   std_logic;
		------------------ Receive Ports - RX OOB signalling Ports -----------------
		gt3_rxelecidle_out                      : out  std_logic;
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
		gt3_rxdisperr_out                       : out  std_logic_vector(1 downto 0);
		gt3_rxnotintable_out                    : out  std_logic_vector(1 downto 0);
		--------------------------- Receive Ports - RX AFE -------------------------
		gt3_gtxrxp_in                           : in   std_logic;
		------------------------ Receive Ports - RX AFE Ports ----------------------
		gt3_gtxrxn_in                           : in   std_logic;
        ------------------- Receive Ports - RX Buffer Bypass Ports -----------------
        --gt3_rxstatus_out                        : out  std_logic_vector(2 downto 0);
		------------------ Receive Ports - RX Channel Bonding Ports ----------------
		gt3_rxchanbondseq_out                   : out  std_logic;
		gt3_rxchbonden_in                       : in   std_logic;
		gt3_rxchbondlevel_in                    : in   std_logic_vector(2 downto 0);
		gt3_rxchbondmaster_in                   : in   std_logic;
		gt3_rxchbondo_out                       : out  std_logic_vector(4 downto 0);
		gt3_rxchbondslave_in                    : in   std_logic;
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
		gt3_rxchanisaligned_out                 : out  std_logic;
		gt3_rxchanrealign_out                   : out  std_logic;
		--------------------- Receive Ports - RX Equalizer Ports -------------------
		gt3_rxdfelpmreset_in                    : in   std_logic;
		gt3_rxmonitorout_out                    : out  std_logic_vector(6 downto 0);
		gt3_rxmonitorsel_in                     : in   std_logic_vector(1 downto 0);
		------------- Receive Ports - RX Initialization and Reset Ports ------------
		gt3_gtrxreset_in                        : in   std_logic;
		gt3_rxpmareset_in                       : in   std_logic;
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
		gt3_rxchariscomma_out                   : out  std_logic_vector(1 downto 0);
		gt3_rxcharisk_out                       : out  std_logic_vector(1 downto 0);
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
		gt3_rxchbondi_in                        : in   std_logic_vector(4 downto 0);
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
		gt3_rxresetdone_out                     : out  std_logic;
		--------------------- TX Initialization and Reset Ports --------------------
		gt3_gttxreset_in                        : in   std_logic;
		gt3_txuserrdy_in                        : in   std_logic;
		------------------ Transmit Ports - Pattern Generator Ports ----------------
		--gt3_txprbsforceerr_in                   : in   std_logic;
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
		gt3_txinhibit_in                        : in   std_logic;
		------------------ Transmit Ports - TX Data Path interface -----------------
		gt3_txdata_in                           : in   std_logic_vector(15 downto 0);
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
		gt3_gtxtxn_out                          : out  std_logic;
		gt3_gtxtxp_out                          : out  std_logic;
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
		gt3_txoutclkfabric_out                  : out  std_logic;
		gt3_txoutclkpcs_out                     : out  std_logic;
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
		gt3_txcharisk_in                        : in   std_logic_vector(1 downto 0);
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
		gt3_txresetdone_out                     : out  std_logic;
		------------------ Transmit Ports - pattern Generator Ports ----------------
		--gt3_txprbssel_in                        : in   std_logic_vector(2 downto 0);

	GT0_DRPADDR_COMMON_IN                   : in   std_logic_vector(7 downto 0);
	GT0_DRPDI_COMMON_IN                     : in   std_logic_vector(15 downto 0);
	GT0_DRPDO_COMMON_OUT                    : out  std_logic_vector(15 downto 0);
	GT0_DRPEN_COMMON_IN                     : in   std_logic;
	GT0_DRPRDY_COMMON_OUT                   : out  std_logic;
	GT0_DRPWE_COMMON_IN                     : in   std_logic;
	
    --____________________________COMMON PORTS________________________________
	SYSCLK_IN_P                	: in   std_logic;
	SYSCLK_IN_N                 : in   std_logic

	);

	end component;

	------------------------------- Global Signals -----------------------------
	signal  tied_to_ground_i                : std_logic;
	signal  tied_to_ground_vec_i            : std_logic_vector(63 downto 0);
	signal  tied_to_vcc_i                   : std_logic;
	signal  tied_to_vcc_vec_i               : std_logic_vector(7 downto 0);
	
	-------------------------- Channel Bonding Wires ---------------------------
	signal    gt0_rxchbondo_i                 : std_logic_vector(4 downto 0);
	signal    gt1_rxchbondo_i                 : std_logic_vector(4 downto 0);
	signal    gt2_rxchbondo_i                 : std_logic_vector(4 downto 0);
	signal    gt3_rxchbondo_i                 : std_logic_vector(4 downto 0);
	
	------------------ Transmit Ports - pattern Generator Ports ----------------
    signal  gt0_txprbssel_i                 : std_logic_vector(2 downto 0);
    signal  gt1_txprbssel_i                 : std_logic_vector(2 downto 0);
    signal  gt2_txprbssel_i                 : std_logic_vector(2 downto 0);
    signal  gt3_txprbssel_i                 : std_logic_vector(2 downto 0);
	
    signal  gt0_rxprbssel_i                 : std_logic_vector(2 downto 0);
    signal  gt1_rxprbssel_i                 : std_logic_vector(2 downto 0);
    signal  gt2_rxprbssel_i                 : std_logic_vector(2 downto 0);
    signal  gt3_rxprbssel_i                 : std_logic_vector(2 downto 0);
  
	---------------------------- Channel - DRP Ports  --------------------------
    signal  gt1_drpaddr_i                   : std_logic_vector(8 downto 0);
    signal  gt1_drpdi_i                     : std_logic_vector(15 downto 0);
    signal  gt1_drpdo_i                     : std_logic_vector(15 downto 0);
    signal  gt1_drpen_i                     : std_logic;
    signal  gt1_drprdy_i                    : std_logic;
    signal  gt1_drpwe_i                     : std_logic;
	
    signal  gt0_drpaddr_i                   : std_logic_vector(8 downto 0);
    signal  gt0_drpdi_i                     : std_logic_vector(15 downto 0);
    signal  gt0_drpdo_i                     : std_logic_vector(15 downto 0);
    signal  gt0_drpen_i                     : std_logic;
    signal  gt0_drprdy_i                    : std_logic;
    signal  gt0_drpwe_i                     : std_logic;
	
    signal  gt2_drpaddr_i                   : std_logic_vector(8 downto 0);
    signal  gt2_drpdi_i                     : std_logic_vector(15 downto 0);
    signal  gt2_drpdo_i                     : std_logic_vector(15 downto 0);
    signal  gt2_drpen_i                     : std_logic;
    signal  gt2_drprdy_i                    : std_logic;
    signal  gt2_drpwe_i                     : std_logic;
	
    signal  gt3_drpaddr_i                   : std_logic_vector(8 downto 0);
    signal  gt3_drpdi_i                     : std_logic_vector(15 downto 0);
    signal  gt3_drpdo_i                     : std_logic_vector(15 downto 0);
    signal  gt3_drpen_i                     : std_logic;
    signal  gt3_drprdy_i                    : std_logic;
    signal  gt3_drpwe_i                     : std_logic;
	
    ------------------- Receive Ports - Pattern Checker ports ------------------
    signal  gt0_rxprbscntreset_in            : std_logic;
    signal  gt1_rxprbscntreset_in            : std_logic;
    signal  gt2_rxprbscntreset_in            : std_logic;
    signal  gt3_rxprbscntreset_in            : std_logic;
    
    signal    txinhibit02_q : std_logic;
    signal    txinhibit13_q : std_logic;
    
    signal gt0_txusrclk     : std_logic; 
    signal gt0_txusrclk2    : std_logic; 
    signal gt0_rxusrclk     : std_logic;
    signal gt0_rxusrclk2    : std_logic;
    
    constant TCQ : time := 100 ns;
    
    signal gt0_rxprbserr_out :  std_logic;
    signal gt1_rxprbserr_out :  std_logic;
    signal gt2_rxprbserr_out :  std_logic;
    signal gt3_rxprbserr_out :  std_logic;
    
    signal gt0_rxchanisaligned                 : std_logic;
    signal gt1_rxchanisaligned                 : std_logic;
    signal gt2_rxchanisaligned                 : std_logic;
    signal gt3_rxchanisaligned                 : std_logic;

    signal gt0_GTXRESET                 : std_logic;
    signal gt1_GTXRESET                 : std_logic;
    signal gt2_GTXRESET                 : std_logic;
    signal gt3_GTXRESET                 : std_logic;

    signal gt0_cplllock                 : std_logic;
    signal gt1_cplllock                 : std_logic;
    signal gt2_cplllock                 : std_logic;
    signal gt3_cplllock                 : std_logic;

    signal gt0_rxstatus_out                        : std_logic_vector(2 downto 0);
    signal gt1_rxstatus_out                        : std_logic_vector(2 downto 0);
    signal gt2_rxstatus_out                        : std_logic_vector(2 downto 0);
    signal gt3_rxstatus_out                        : std_logic_vector(2 downto 0);
  ----------------------------- Reference Clocks ----------------------------
    signal    q0_clk0_refclk_i                : std_logic;
    signal    q0_clk1_refclk_i                : std_logic;

    signal    GT0_TX_FSM_RESET_DONE_OUT                : std_logic;
    signal    GT0_RX_FSM_RESET_DONE_OUT                : std_logic;

    signal    GT1_TX_FSM_RESET_DONE_OUT                : std_logic;
    signal    GT1_RX_FSM_RESET_DONE_OUT                : std_logic;

    signal    GT2_TX_FSM_RESET_DONE_OUT                : std_logic;
    signal    GT2_RX_FSM_RESET_DONE_OUT                : std_logic;

    signal    GT3_TX_FSM_RESET_DONE_OUT                : std_logic;
    signal    GT3_RX_FSM_RESET_DONE_OUT                : std_logic;

    signal 	buffer_reseted : std_logic;
    signal 	RXBUFRST_intern : std_logic;
    signal 	RXBUFRST_2 : std_logic;

    signal rxelecidle_phy     : std_logic_vector(3 downto 0);
begin


    -- 
    -- When the user has set the loopback mode either in Near-End PCS Loopback or  Near-End PMA Loopback
    --  we don't care about the physical connection, so we report it ok
    -- elsewhere this is the physical cable status
    --
    RXELECIDLE(0) <= '0' when LOOPBACK0="001" or LOOPBACK0="010" else rxelecidle_phy(0);
    RXELECIDLE(1) <= '0' when LOOPBACK1="001" or LOOPBACK1="010" else rxelecidle_phy(1);
    RXELECIDLE(2) <= '0' when LOOPBACK2="001" or LOOPBACK2="010" else rxelecidle_phy(2);
    RXELECIDLE(3) <= '0' when LOOPBACK3="001" or LOOPBACK3="010" else rxelecidle_phy(3);
    

	RXBUFRST_intern <= '1' when  RXBUFRST = '1' else RXBUFRST_2;
    
	q0_clk0_refclk_i                             <= '0';
	q0_clk1_refclk_i                             <= '0';

	--  Static signal Assigments
	tied_to_ground_i                             <= '0';
	tied_to_ground_vec_i                         <= x"0000000000000000";
	tied_to_vcc_i                                <= '1';
	tied_to_vcc_vec_i                            <= "11111111";

	RXCLKSTABLE0 <= GT0_RX_FSM_RESET_DONE_OUT and GT0_TX_FSM_RESET_DONE_OUT;
	RXCLKSTABLE1 <= GT1_RX_FSM_RESET_DONE_OUT and GT1_TX_FSM_RESET_DONE_OUT;
	RXCLKSTABLE2 <= GT2_RX_FSM_RESET_DONE_OUT and GT2_TX_FSM_RESET_DONE_OUT;
	RXCLKSTABLE3 <= GT3_RX_FSM_RESET_DONE_OUT and GT3_TX_FSM_RESET_DONE_OUT;

	gtwizard_0_i : gtwizard_0
	port map
	(
		SOFT_RESET_IN 					=> tied_to_ground_i,
		DONT_RESET_ON_DATA_ERROR_IN 	=> tied_to_ground_i,
		
		--Q0_CLK0_GTREFCLK_PAD_N_IN 	=> CLK0_GTREFCLK_PAD_N,
		--Q0_CLK0_GTREFCLK_PAD_P_IN 	=> CLK0_GTREFCLK_PAD_P,

		Q0_CLK1_GTREFCLK_PAD_N_IN 	=> CLK1_GTREFCLK_PAD_N,
		Q0_CLK1_GTREFCLK_PAD_P_IN 	=> CLK1_GTREFCLK_PAD_P,

		GT0_TX_FSM_RESET_DONE_OUT 	=> GT0_TX_FSM_RESET_DONE_OUT,
		GT0_RX_FSM_RESET_DONE_OUT 	=> GT0_RX_FSM_RESET_DONE_OUT,
		GT0_DATA_VALID_IN 		=> RXDATA0VALID,
		
		GT1_TX_FSM_RESET_DONE_OUT 	=> GT1_TX_FSM_RESET_DONE_OUT,
		GT1_RX_FSM_RESET_DONE_OUT 	=> GT1_RX_FSM_RESET_DONE_OUT,
		GT1_DATA_VALID_IN		=> RXDATA1VALID,
		
		GT2_TX_FSM_RESET_DONE_OUT 	=> GT2_TX_FSM_RESET_DONE_OUT,
		GT2_RX_FSM_RESET_DONE_OUT 	=> GT2_RX_FSM_RESET_DONE_OUT,
		GT2_DATA_VALID_IN 		=> RXDATA2VALID,
		
		GT3_TX_FSM_RESET_DONE_OUT 	=> GT3_TX_FSM_RESET_DONE_OUT,
		GT3_RX_FSM_RESET_DONE_OUT	=> GT3_RX_FSM_RESET_DONE_OUT,
		GT3_DATA_VALID_IN 		=> RXDATA3VALID,

		--
		-- Clock domain for capturing the data
		--
		GT0_TXUSRCLK_OUT 	=> gt0_txusrclk,
		GT0_TXUSRCLK2_OUT 	=> gt0_txusrclk2,
		GT0_RXUSRCLK_OUT 	=> gt0_rxusrclk,
		GT0_RXUSRCLK2_OUT 	=> gt0_rxusrclk2,
		
		--
		-- As we are using a single clock source for the Quad
		--  Clocks are actually all mapped on the GT0, in gtwizard_0_usrclk_source.vhd
		--
		GT1_TXUSRCLK_OUT 	=> open,
		GT1_TXUSRCLK2_OUT 	=> open,
		GT1_RXUSRCLK_OUT 	=> open,
		GT1_RXUSRCLK2_OUT 	=> open,

		GT2_TXUSRCLK_OUT 	=> open,
		GT2_TXUSRCLK2_OUT 	=> open,
		GT2_RXUSRCLK_OUT 	=> open,
		GT2_RXUSRCLK2_OUT 	=> open,

		GT3_TXUSRCLK_OUT 	=> open,
		GT3_TXUSRCLK2_OUT 	=> open,
		GT3_RXUSRCLK_OUT 	=> open,
		GT3_RXUSRCLK2_OUT 	=> open,

		--_________________________________________________________________________
		--GT0  (X0Y0)
		--____________________________CHANNEL PORTS________________________________
		--------------------------------- CPLL Ports -------------------------------
		gt0_cpllfbclklost_out           =>      open,
		gt0_cplllock_out                =>      gt0_cplllock,
		gt0_cpllreset_in                =>      tied_to_ground_i,
		-------------------------- Channel - Clocking Ports ------------------------
		--gt0_gtrefclk0_in                =>      q0_clk0_refclk_i, -- PIN reserved for test purpose UG476 p.37
		---------------------------- Channel - DRP Ports  --------------------------
		gt0_drpaddr_in                  =>      gt0_drpaddr_i,
		gt0_drpdi_in                    =>      gt0_drpdi_i,
		gt0_drpdo_out                   =>      open,
		gt0_drpen_in                    =>      gt0_drpen_i,
		gt0_drprdy_out                  =>      open,
		gt0_drpwe_in                    =>      gt0_drpwe_i,
			--------------------------- Digital Monitor Ports --------------------------
			gt0_dmonitorout_out             =>      open,
		------------------------------- Loopback Ports -----------------------------
			gt0_loopback_in                 =>      LOOPBACK0,
		--------------------- RX Initialization and Reset Ports --------------------
			gt0_eyescanreset_in             =>      tied_to_ground_i,
			gt0_rxuserrdy_in                =>      tied_to_ground_i,
		-------------------------- RX Margin Analysis Ports ------------------------
			gt0_eyescandataerror_out        =>      open,
			gt0_eyescantrigger_in           =>      tied_to_ground_i,
		------------------- Receive Ports - Clock Correction Ports -----------------
			gt0_rxclkcorcnt_out             =>      open,
		------------------ Receive Ports - FPGA RX interface Ports -----------------
			gt0_rxdata_out                  =>      RXDATA0,
		------------------- Receive Ports - Pattern Checker Ports ------------------
			gt0_rxprbserr_out               =>      gt0_rxprbserr_out,
			gt0_rxprbssel_in                =>      (others => '0'),
		------------------- Receive Ports - Pattern Checker ports ------------------
			gt0_rxprbscntreset_in           =>      gt0_rxprbscntreset_in,
		------------------ Receive Ports - RX OOB signalling Ports -----------------
			gt0_rxelecidle_out              =>      rxelecidle_phy(0),
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
			gt0_rxdisperr_out               =>      RXDISPERR0,
			gt0_rxnotintable_out            =>      RXNOTINTABLE0,
		--------------------------- Receive Ports - RX AFE -------------------------
			gt0_gtxrxp_in                   =>      RXP0,
		------------------------ Receive Ports - RX AFE Ports ----------------------
			gt0_gtxrxn_in                   =>      RXN0,
            
            --gt0_rxstatus_out                =>      gt0_rxstatus_out,
            
		------------------ Receive Ports - RX Channel Bonding Ports ----------------
			gt0_rxchanbondseq_out           =>      open,
			gt0_rxchbonden_in               =>      ENCHANSYNC,
			gt0_rxchbondlevel_in            =>      "000", -- cf ref design not sure why gt0 is not the master
			gt0_rxchbondmaster_in           =>      tied_to_ground_i,
			gt0_rxchbondo_out               =>      gt0_rxchbondo_i,
			gt0_rxchbondslave_in            =>      tied_to_vcc_i,
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
			gt0_rxchanisaligned_out         =>      gt0_rxchanisaligned,
			gt0_rxchanrealign_out           =>      RXBYTEREALIGN(0),
		--------------------- Receive Ports - RX Equalizer Ports -------------------
			gt0_rxdfelpmreset_in            =>      tied_to_ground_i,
			gt0_rxmonitorout_out            =>      open,
			gt0_rxmonitorsel_in             =>      "00",
		------------- Receive Ports - RX Initialization and Reset Ports ------------
			gt0_gtrxreset_in                =>      RXBUFRST_intern,
			gt0_rxpmareset_in               =>      tied_to_ground_i, -- FIXME Is it the correct reset ?
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
			gt0_rxchariscomma_out           =>      RXCHARISCOMMA0,
			gt0_rxcharisk_out               =>      RXCHARISK0,
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
			gt0_rxchbondi_in                =>      gt1_rxchbondo_i,
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
			gt0_rxresetdone_out             =>      open,
		--------------------- TX Initialization and Reset Ports --------------------
			gt0_gttxreset_in                =>      gt0_GTXRESET,
			gt0_txuserrdy_in                =>      tied_to_ground_i,
		------------------ Transmit Ports - Pattern Generator Ports ----------------
			--gt0_txprbsforceerr_in           =>      tied_to_ground_i,
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
			gt0_txinhibit_in                =>      TXINHIBIT(0),
		------------------ Transmit Ports - TX Data Path interface -----------------
			gt0_txdata_in                   =>      TXDATA0,
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
			gt0_gtxtxn_out                  =>      TXN0,
			gt0_gtxtxp_out                  =>      TXP0,
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
			gt0_txoutclkfabric_out          =>      open,
			gt0_txoutclkpcs_out             =>      open,
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
			gt0_txcharisk_in                =>      TXCHARISK0,
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
			gt0_txresetdone_out             =>      open,
		------------------ Transmit Ports - pattern Generator Ports ----------------
			--gt0_txprbssel_in                =>      gt0_txprbssel_i,

		--GT1  (X0Y1)
		--____________________________CHANNEL PORTS________________________________
			--------------------------------- CPLL Ports -------------------------------
			gt1_cpllfbclklost_out           =>      open,
			gt1_cplllock_out                =>      gt1_cplllock,
			gt1_cpllreset_in                =>      tied_to_ground_i,
			-------------------------- Channel - Clocking Ports ------------------------
			--gt1_gtrefclk0_in                =>      q0_clk1_refclk_i, -- PIN reserved for test purpose UG476 p.37
			---------------------------- Channel - DRP Ports  --------------------------
			gt1_drpaddr_in                  =>      gt1_drpaddr_i,
			gt1_drpdi_in                    =>      gt1_drpdi_i,
			gt1_drpdo_out                   =>      open,
			gt1_drpen_in                    =>      gt1_drpen_i,
			gt1_drprdy_out                  =>      open,
			gt1_drpwe_in                    =>      gt1_drpwe_i,
		--------------------------- Digital Monitor Ports --------------------------
			gt1_dmonitorout_out             =>      open,
		------------------------------- Loopback Ports -----------------------------
			gt1_loopback_in                 =>      LOOPBACK1,
		--------------------- RX Initialization and Reset Ports --------------------
			gt1_eyescanreset_in             =>      tied_to_ground_i,
			gt1_rxuserrdy_in                =>      tied_to_ground_i,
		-------------------------- RX Margin Analysis Ports ------------------------
			gt1_eyescandataerror_out        =>      open,
			gt1_eyescantrigger_in           =>      tied_to_ground_i,
		------------------- Receive Ports - Clock Correction Ports -----------------
			gt1_rxclkcorcnt_out             =>      open,
		------------------ Receive Ports - FPGA RX interface Ports -----------------
			gt1_rxdata_out                  =>      RXDATA1,
		------------------- Receive Ports - Pattern Checker Ports ------------------
			gt1_rxprbserr_out               =>      gt1_rxprbserr_out,
			gt1_rxprbssel_in                =>      (others => '0'),
		------------------- Receive Ports - Pattern Checker ports ------------------
			gt1_rxprbscntreset_in           =>      gt1_rxprbscntreset_in,
		------------------ Receive Ports - RX OOB signalling Ports -----------------
			gt1_rxelecidle_out              =>      rxelecidle_phy(1),
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
			gt1_rxdisperr_out               =>      RXDISPERR1,
			gt1_rxnotintable_out            =>      RXNOTINTABLE1,
		--------------------------- Receive Ports - RX AFE -------------------------
			gt1_gtxrxp_in                   =>      RXP1,
		------------------------ Receive Ports - RX AFE Ports ----------------------
			gt1_gtxrxn_in                   =>      RXN1,
            --gt1_rxstatus_out                =>      gt1_rxstatus_out,
            
		------------------ Receive Ports - RX Channel Bonding Ports ----------------
			gt1_rxchanbondseq_out           =>      open,
			gt1_rxchbonden_in               =>      ENCHANSYNC,
			gt1_rxchbondlevel_in            =>      "001",
			gt1_rxchbondmaster_in           =>      tied_to_ground_i,
			gt1_rxchbondo_out               =>      gt1_rxchbondo_i,
			gt1_rxchbondslave_in            =>      tied_to_vcc_i,
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
			gt1_rxchanisaligned_out         =>      gt1_rxchanisaligned,
			gt1_rxchanrealign_out           =>      RXBYTEREALIGN(1),
		--------------------- Receive Ports - RX Equalizer Ports -------------------
			gt1_rxdfelpmreset_in            =>      tied_to_ground_i,
			gt1_rxmonitorout_out            =>      open,
			gt1_rxmonitorsel_in             =>      "00",
		------------- Receive Ports - RX Initialization and Reset Ports ------------
			gt1_gtrxreset_in                =>      RXBUFRST_intern,
			gt1_rxpmareset_in               =>      tied_to_ground_i, -- FIXME Is it the correct reset ?
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
			gt1_rxchariscomma_out           =>      RXCHARISCOMMA1,
			gt1_rxcharisk_out               =>      RXCHARISK1,
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
			gt1_rxchbondi_in                =>      gt2_rxchbondo_i,
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
			gt1_rxresetdone_out             =>      open,
		--------------------- TX Initialization and Reset Ports --------------------
			gt1_gttxreset_in                =>      gt1_GTXRESET,
			gt1_txuserrdy_in                =>      tied_to_ground_i,
		------------------ Transmit Ports - Pattern Generator Ports ----------------
			--gt1_txprbsforceerr_in           =>      tied_to_ground_i,
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
			gt1_txinhibit_in                =>      TXINHIBIT(1),
		------------------ Transmit Ports - TX Data Path interface -----------------
			gt1_txdata_in                   =>      TXDATA1,
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
			gt1_gtxtxn_out                  =>      TXN1,
			gt1_gtxtxp_out                  =>      TXP1,
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
			gt1_txoutclkfabric_out          =>      open,
			gt1_txoutclkpcs_out             =>      open,
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
			gt1_txcharisk_in                =>      TXCHARISK1,
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
			gt1_txresetdone_out             =>      open,
		------------------ Transmit Ports - pattern Generator Ports ----------------
			--gt1_txprbssel_in                =>      gt1_txprbssel_i,

		--GT2  (X0Y2)
		--____________________________CHANNEL PORTS________________________________
			--------------------------------- CPLL Ports -------------------------------
			gt2_cpllfbclklost_out           =>      open,
			gt2_cplllock_out                =>      gt2_cplllock,
			gt2_cpllreset_in                =>      tied_to_ground_i,
			-------------------------- Channel - Clocking Ports ------------------------
			--gt2_gtrefclk0_in                =>      q0_clk1_refclk_i, -- PIN reserved for test purpose UG476 p.37
			---------------------------- Channel - DRP Ports  --------------------------
			gt2_drpaddr_in                  =>      gt2_drpaddr_i,
			gt2_drpdi_in                    =>      gt2_drpdi_i,
			gt2_drpdo_out                   =>      open,
			gt2_drpen_in                    =>      gt2_drpen_i,
			gt2_drprdy_out                  =>      open,
			gt2_drpwe_in                    =>      gt2_drpwe_i,
		--------------------------- Digital Monitor Ports --------------------------
			gt2_dmonitorout_out             =>      open,
		------------------------------- Loopback Ports -----------------------------
			gt2_loopback_in                 =>      LOOPBACK2,
		--------------------- RX Initialization and Reset Ports --------------------
			gt2_eyescanreset_in             =>      tied_to_ground_i,
			gt2_rxuserrdy_in                =>      tied_to_ground_i,
		-------------------------- RX Margin Analysis Ports ------------------------
			gt2_eyescandataerror_out        =>      open,
			gt2_eyescantrigger_in           =>      tied_to_ground_i,
		------------------- Receive Ports - Clock Correction Ports -----------------
			gt2_rxclkcorcnt_out             =>      open,
		------------------ Receive Ports - FPGA RX interface Ports -----------------
			gt2_rxdata_out                  =>      RXDATA2,
		------------------- Receive Ports - Pattern Checker Ports ------------------
			gt2_rxprbserr_out               =>      gt2_rxprbserr_out,
			gt2_rxprbssel_in                =>      (others => '0'),
		------------------- Receive Ports - Pattern Checker ports ------------------
			gt2_rxprbscntreset_in           =>      gt2_rxprbscntreset_in,
		------------------ Receive Ports - RX OOB signalling Ports -----------------
			gt2_rxelecidle_out              =>      rxelecidle_phy(2),
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
			gt2_rxdisperr_out               =>      RXDISPERR2,
			gt2_rxnotintable_out            =>      RXNOTINTABLE2,
		--------------------------- Receive Ports - RX AFE -------------------------
			gt2_gtxrxp_in                   =>      RXP2,
		------------------------ Receive Ports - RX AFE Ports ----------------------
			gt2_gtxrxn_in                   =>      RXN2,
            --gt2_rxstatus_out                =>      gt2_rxstatus_out,
            
		------------------ Receive Ports - RX Channel Bonding Ports ----------------
			gt2_rxchanbondseq_out           =>      open,
			gt2_rxchbonden_in               =>      ENCHANSYNC,
			gt2_rxchbondlevel_in            =>      "010",
			gt2_rxchbondmaster_in           =>      tied_to_vcc_i,
			gt2_rxchbondo_out               =>      gt2_rxchbondo_i,
			gt2_rxchbondslave_in            =>      tied_to_ground_i,
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
			gt2_rxchanisaligned_out         =>      gt2_rxchanisaligned,
			gt2_rxchanrealign_out           =>      RXBYTEREALIGN(2),
		--------------------- Receive Ports - RX Equalizer Ports -------------------
			gt2_rxdfelpmreset_in            =>      tied_to_ground_i,
			gt2_rxmonitorout_out            =>      open,
			gt2_rxmonitorsel_in             =>      "00",
		------------- Receive Ports - RX Initialization and Reset Ports ------------
			gt2_gtrxreset_in                =>      RXBUFRST_intern,
			gt2_rxpmareset_in               =>      tied_to_ground_i, 
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
			gt2_rxchariscomma_out           =>      RXCHARISCOMMA2,
			gt2_rxcharisk_out               =>      RXCHARISK2,
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
			gt2_rxchbondi_in                =>      tied_to_ground_vec_i(4 downto 0),
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
			gt2_rxresetdone_out             =>      open,
		--------------------- TX Initialization and Reset Ports --------------------
			gt2_gttxreset_in                =>      gt2_GTXRESET,
			gt2_txuserrdy_in                =>      tied_to_ground_i,
		------------------ Transmit Ports - Pattern Generator Ports ----------------
			--gt2_txprbsforceerr_in           =>      tied_to_ground_i,
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
			gt2_txinhibit_in                =>      TXINHIBIT(2),
		------------------ Transmit Ports - TX Data Path interface -----------------
			gt2_txdata_in                   =>      TXDATA2,
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
			gt2_gtxtxn_out                  =>      TXN2,
			gt2_gtxtxp_out                  =>      TXP2,
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
			gt2_txoutclkfabric_out          =>      open,
			gt2_txoutclkpcs_out             =>      open,
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
			gt2_txcharisk_in                =>      TXCHARISK2,
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
			gt2_txresetdone_out             =>      open,
		------------------ Transmit Ports - pattern Generator Ports ----------------
			--gt2_txprbssel_in                =>      gt2_txprbssel_i,

		--GT3  (X0Y3)
		--____________________________CHANNEL PORTS________________________________
			--------------------------------- CPLL Ports -------------------------------
			gt3_cpllfbclklost_out           =>      open,
			gt3_cplllock_out                =>      gt3_cplllock,
			gt3_cpllreset_in                =>      tied_to_ground_i,
			-------------------------- Channel - Clocking Ports ------------------------
			--gt3_gtrefclk0_in                =>      q0_clk1_refclk_i, -- PIN reserved for test purpose UG476 p.37
			---------------------------- Channel - DRP Ports  --------------------------
			gt3_drpaddr_in                  =>      gt3_drpaddr_i,
			gt3_drpdi_in                    =>      gt3_drpdi_i,
			gt3_drpdo_out                   =>      open,
			gt3_drpen_in                    =>      gt3_drpen_i,
			gt3_drprdy_out                  =>      open,
			gt3_drpwe_in                    =>      gt3_drpwe_i,
		--------------------------- Digital Monitor Ports --------------------------
			gt3_dmonitorout_out             =>      open,
		------------------------------- Loopback Ports -----------------------------
			gt3_loopback_in                 =>      LOOPBACK3,
		--------------------- RX Initialization and Reset Ports --------------------
			gt3_eyescanreset_in             =>      tied_to_ground_i,
			gt3_rxuserrdy_in                =>      tied_to_ground_i,
		-------------------------- RX Margin Analysis Ports ------------------------
			gt3_eyescandataerror_out        =>      open,
			gt3_eyescantrigger_in           =>      tied_to_ground_i,
		------------------- Receive Ports - Clock Correction Ports -----------------
			gt3_rxclkcorcnt_out             =>      open,
		------------------ Receive Ports - FPGA RX interface Ports -----------------
			gt3_rxdata_out                  =>      RXDATA3,
		------------------- Receive Ports - Pattern Checker Ports ------------------
			gt3_rxprbserr_out               =>      gt3_rxprbserr_out,
			gt3_rxprbssel_in                =>      (others => '0'),
		------------------- Receive Ports - Pattern Checker ports ------------------
			gt3_rxprbscntreset_in           =>      gt3_rxprbscntreset_in,
		------------------ Receive Ports - RX OOB signalling Ports -----------------
			gt3_rxelecidle_out              =>      rxelecidle_phy(3),
		------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
			gt3_rxdisperr_out               =>      RXDISPERR3,
			gt3_rxnotintable_out            =>      RXNOTINTABLE3,
		--------------------------- Receive Ports - RX AFE -------------------------
			gt3_gtxrxp_in                   =>      RXP3,
		------------------------ Receive Ports - RX AFE Ports ----------------------
			gt3_gtxrxn_in                   =>      RXN3,
            
            --gt3_rxstatus_out                =>      gt3_rxstatus_out,
            

            ------------------ Receive Ports - RX Channel Bonding Ports ----------------
			gt3_rxchanbondseq_out           =>      open,
			gt3_rxchbonden_in               =>      ENCHANSYNC,
			gt3_rxchbondlevel_in            =>      "001",
			gt3_rxchbondmaster_in           =>      tied_to_ground_i,
			gt3_rxchbondo_out               =>      gt3_rxchbondo_i,
			gt3_rxchbondslave_in            =>      tied_to_vcc_i,
		----------------- Receive Ports - RX Channel Bonding Ports  ----------------
			gt3_rxchanisaligned_out         =>      gt3_rxchanisaligned,
			gt3_rxchanrealign_out           =>      RXBYTEREALIGN(3),
		--------------------- Receive Ports - RX Equalizer Ports -------------------
			gt3_rxdfelpmreset_in            =>      tied_to_ground_i,
			gt3_rxmonitorout_out            =>      open,
			gt3_rxmonitorsel_in             =>      "00",
		------------- Receive Ports - RX Initialization and Reset Ports ------------
			gt3_gtrxreset_in                =>      RXBUFRST_intern,
			gt3_rxpmareset_in               =>      tied_to_ground_i, -- FIXME Is it the correct reset ?
		------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
			gt3_rxchariscomma_out           =>      RXCHARISCOMMA3,
			gt3_rxcharisk_out               =>      RXCHARISK3,
		------------------ Receive Ports - Rx Channel Bonding Ports ----------------
			gt3_rxchbondi_in                =>      gt2_rxchbondo_i,
		-------------- Receive Ports -RX Initialization and Reset Ports ------------
			gt3_rxresetdone_out             =>      open,
		--------------------- TX Initialization and Reset Ports --------------------
			gt3_gttxreset_in                =>      gt3_GTXRESET,
			gt3_txuserrdy_in                =>      tied_to_ground_i,
		------------------ Transmit Ports - Pattern Generator Ports ----------------
			--gt3_txprbsforceerr_in           =>      tied_to_ground_i,
		--------------- Transmit Ports - TX Configurable Driver Ports --------------
			gt3_txinhibit_in                =>      TXINHIBIT(3),
		------------------ Transmit Ports - TX Data Path interface -----------------
			gt3_txdata_in                   =>      TXDATA3,
		---------------- Transmit Ports - TX Driver and OOB signaling --------------
			gt3_gtxtxn_out                  =>      TXN3,
			gt3_gtxtxp_out                  =>      TXP3,
		----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
			gt3_txoutclkfabric_out          =>      open,
			gt3_txoutclkpcs_out             =>      open,
		--------------------- Transmit Ports - TX Gearbox Ports --------------------
			gt3_txcharisk_in                =>      TXCHARISK3,
		------------- Transmit Ports - TX Initialization and Reset Ports -----------
			gt3_txresetdone_out             =>      open,
		------------------ Transmit Ports - pattern Generator Ports ----------------
			--gt3_txprbssel_in                =>      gt3_txprbssel_i,

		GT0_DRPADDR_COMMON_IN 		=> ( others => '0'),
		GT0_DRPDI_COMMON_IN 		=> ( others => '0'),
		GT0_DRPDO_COMMON_OUT 		=> open,
		GT0_DRPEN_COMMON_IN 		=> '0',
		GT0_DRPRDY_COMMON_OUT 		=> open,
		GT0_DRPWE_COMMON_IN 		=> '0',

		
		SYSCLK_IN_P => SYSCLK_IN_P,
		SYSCLK_IN_N => SYSCLK_IN_N
	);
	
	------------ optional Ports assignments --------------
	gt0_rxprbssel_i                              <= (others => '0');
	gt0_txprbssel_i                              <= (others => '0');
	
	gt1_rxprbssel_i                              <= (others => '0');
	gt1_txprbssel_i                              <= (others => '0');
	
	gt2_rxprbssel_i                              <= (others => '0');
	gt2_txprbssel_i                              <= (others => '0');

	gt3_rxprbssel_i                              <= (others => '0');
	gt3_txprbssel_i                              <= (others => '0');
	
		
    TXUSRCLK      <= gt0_txusrclk;
    TXUSRCLK2     <= gt0_txusrclk2;
    RXUSRCLK      <= gt0_rxusrclk;
    RXUSRCLK2     <= gt0_rxusrclk2;
	
	------------ Unused Dynamic Reconfiguration ports -------------
	gt0_drpaddr_i <= (others => '0');
	gt0_drpdi_i <= (others => '0');
	gt0_drpen_i <= '0';
	gt0_drpwe_i <= '0';
	gt1_drpaddr_i <= (others => '0');
	gt1_drpdi_i <= (others => '0');
	gt1_drpen_i <= '0';
	gt1_drpwe_i <= '0';
	gt2_drpaddr_i <= (others => '0');
	gt2_drpdi_i <= (others => '0');
	gt2_drpen_i <= '0';
	gt2_drpwe_i <= '0';
	gt3_drpaddr_i <= (others => '0');
	gt3_drpdi_i <= (others => '0');
	gt3_drpen_i <= '0';
	gt3_drpwe_i <= '0';
	
	gt0_rxprbscntreset_in                         <= tied_to_ground_i;
	gt1_rxprbscntreset_in                         <= tied_to_ground_i;
	gt2_rxprbscntreset_in                         <= tied_to_ground_i;
	gt3_rxprbscntreset_in                         <= tied_to_ground_i;
	
	CHBONDDONE0 <= gt0_rxchanisaligned; 
	CHBONDDONE1 <= gt1_rxchanisaligned;
	CHBONDDONE2 <= gt2_rxchanisaligned;
	CHBONDDONE3 <= gt3_rxchanisaligned;
	
	RXBYTEISALIGNED(0)  <= gt0_rxchanisaligned;
	RXBYTEISALIGNED(1)  <= gt1_rxchanisaligned;
	RXBYTEISALIGNED(2)  <= gt2_rxchanisaligned;
	RXBYTEISALIGNED(3)  <= gt3_rxchanisaligned;
	
	--FIXME
	--RXBUFERR <= '0'; 
	-- Determine falling edge of TXINHIBIT and force a TXRESET
    --process (gt0_txusrclk2) begin
        --if rising_edge(gt0_txusrclk2) then
           --txinhibit02_q <= TXINHIBIT_02 after TCQ;
           --txinhibit13_q <= TXINHIBIT_13 after TCQ;
        --end if;
    --end process;
    --
     	--
    --process (gt0_rxusrclk2, GTXRESET) begin
        --if (GTXRESET = '1') then
           --RXBUFERR <= '0' after TCQ;
        --elsif rising_edge(gt0_rxusrclk2) then
           --if ((txinhibit02_q = '1') or (txinhibit13_q = '1')) then
              --RXBUFERR <= '1';
           --elsif ((gt0_rxchanisaligned and gt1_rxchanisaligned and gt2_rxchanisaligned and gt3_rxchanisaligned) = '1') then
              --RXBUFERR <= gt0_rxprbserr_out or gt1_rxprbserr_out or gt2_rxprbserr_out or gt3_rxprbserr_out after TCQ;
           --else
              --RXBUFERR <= gt0_rxprbserr_out after TCQ;
           --end if;
        --end if;
    --end process;
 
	process (GTXRESET, gt0_rxusrclk2)
	begin
		if (GTXRESET = '1') then

			buffer_reseted <= '0';
			RXBUFRST_2 <= '0';

		elsif rising_edge(gt0_rxusrclk2) then

			if GT0_RX_FSM_RESET_DONE_OUT = '0' or GT0_TX_FSM_RESET_DONE_OUT = '0' then
				buffer_reseted <= '0';
				RXBUFRST_2 <= '0';

			elsif GT0_RX_FSM_RESET_DONE_OUT = '1' and GT0_TX_FSM_RESET_DONE_OUT = '1' and buffer_reseted = '0' then
				RXBUFRST_2 <= '1';
				buffer_reseted <= '1';
			else 
				RXBUFRST_2 <= '0';

			end if;

		end if;

	end process;


    gt0_GTXRESET <= '1' when gt0_cplllock = '0' else GTXRESET;
    gt1_GTXRESET <= '1' when gt1_cplllock = '0' else GTXRESET;
    gt2_GTXRESET <= '1' when gt2_cplllock = '0' else GTXRESET;
    gt3_GTXRESET <= '1' when gt3_cplllock = '0' else GTXRESET;

end Behavioral;
