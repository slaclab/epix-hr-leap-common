-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Core Interface
-------------------------------------------------------------------------------
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'Simple-PGPv4-KCU105-Example', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.AxiLitePkg.all;
use surf.Pgp4Pkg.all;
use surf.AxiPkg.all;
use surf.I2cPkg.all;
use surf.SsiCmdMasterPkg.all;
use surf.SsiPkg.all;

library unisim;
use unisim.vcomponents.all;

library work;
use work.CorePkg.all;

entity Core is 
    generic (
      BUILD_INFO_G         : BuildInfoType;
      TPD_G                : time            := 1 ns;
      SIMULATION_G         : boolean         := false;
      NUM_OF_LANES_G       : integer         := 4;
      NUM_OF_PSCOPE_G      : integer         := 4;
      NUM_OF_SLOW_ADCS_G   : integer         := 4;
      MEMORY_INIT_FILE_G   : string          := "none";
      SLOW_ADC_AXI_CFG_G   : AxiStreamConfigType := ssiAxiStreamConfig(4);
      PGP_GT_TYPE_G        : string          := "GTH"; -- either "GTH" or "GTY"
      PGP_RATE_G           : string          := "10.3125Gbps";
      AXIL_CLK_FREQ_G      : real            := 156.25E+6 -- In units of Hz
   );
   port (
      axilClk          : out   sl;
      axilRst          : out   sl;
      pcieDaqTrigPause : out   sl;
      --------------------------------------------
      --          Top Level Ports 
      --------------------------------------------
      -- AXI-Lite Register Interface (sysClk domain)
      -- Register Address Range = [0x00000000:0x80000000]
      axilReadMaster  : out   AxiLiteReadMasterType;
      axilReadSlave   : in    AxiLiteReadSlaveType;
      axilWriteMaster : out   AxiLiteWriteMasterType;
      axilWriteSlave  : in    AxiLiteWriteSlaveType;
      -- Streaming Interfaces (axilClk domain)
      asicDataMasters : in    AxiStreamMasterArray(NUM_OF_LANES_G - 1 downto 0);
      asicDataSlaves  : out   AxiStreamSlaveArray(NUM_OF_LANES_G - 1 downto 0);
      oscopeMasters   : in    AxiStreamMasterArray( NUM_OF_PSCOPE_G - 1 downto 0);
      oscopeSlaves    : out   AxiStreamSlaveArray( NUM_OF_PSCOPE_G - 1 downto 0);
      slowAdcMasters  : in    AxiStreamMasterArray( NUM_OF_SLOW_ADCS_G - 1 downto 0);
      slowAdcSlaves   : out   AxiStreamSlaveArray( NUM_OF_SLOW_ADCS_G - 1 downto 0);

      ---------------------------------------------
      --          Core Ports
      ---------------------------------------------
      -- Transceiver high speed lanes
      fpgaOutObTransInP : out slv(7 downto 0);
      fpgaOutObTransInM : out slv(7 downto 0);
      fpgaInObTransOutP : in  slv(7 downto 0);
      fpgaInObTransOutM : in  slv(7 downto 0);

      -- Transceiver low speed control
      obTransScl        : inout sl;
      obTransSda        : inout sl;
      obTransResetL     : out sl;
      obTransIntL       : in sl;

      -- Jitter Cleaner PLL Ports
      jitclnrCsL     : out sl;
      jitclnrIntr    : in sl;
      jitclnrLolL    : in sl;
      jitclnrOeL     : out sl;
      jitclnrRstL    : out sl;
      jitclnrSclk    : out sl;
      jitclnrSdio    : out sl;
      jitclnrSdo     : in sl;
      jitclnrSel     : out slv(1 downto 0);

      -- LMK61E2
      pllClkScl       : inout sl;
      pllClkSda       : inout sl;

      -- GT Clock Ports
      gtPllClkP       : in    sl;
      gtPllClkM       : in    sl;
      gtRefClkP       : in    sl;
      gtRefClkM       : in    sl;

      -- XADC Ports
      vPIn            : in    sl;
      vNIn            : in    sl;
      
      -- ssi commands
      ssiCmd          : out    SsiCmdMasterType
    );
end entity;

architecture rtl of Core is

   constant SYSDEV_INDEX_C     : natural  := 0;
   constant PGP_INDEX_C        : natural  := 1;
   constant APP_INDEX_C        : natural  := 2;
   constant NUM_AXIL_MASTERS_C : positive := 3; 
   
   constant XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXIL_MASTERS_C-1 downto 0) := (
                           SYSDEV_INDEX_C => (baseAddr     => x"0000_0000",
                                              addrBits     => 24,
                                              connectivity => x"FFFF"),
                     
                           PGP_INDEX_C    => (baseAddr     => x"0100_0000",
                                              addrBits     => 24,
                                              connectivity => x"FFFF"),
                     
                           APP_INDEX_C    => (baseAddr     => x"8000_0000",
                                              addrBits     => 31,
                                              connectivity => x"FFFF"));

   constant AXIL_CLK_PERIOD_C : real := (1.0/AXIL_CLK_FREQ_G);  -- In units of seconds

   signal mAxilWriteMaster : AxiLiteWriteMasterType;
   signal mAxilWriteSlave  : AxiLiteWriteSlaveType;
   signal mAxilReadMaster  : AxiLiteReadMasterType;
   signal mAxilReadSlave   : AxiLiteReadSlaveType;

   signal axilWriteMasters : AxiLiteWriteMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0) := (others => AXI_LITE_WRITE_SLAVE_EMPTY_SLVERR_C);
   signal axilReadMasters  : AxiLiteReadMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0)  := (others => AXI_LITE_READ_SLAVE_EMPTY_SLVERR_C);

   signal ssiCmdMaster     : AxiStreamMasterType;
   signal ssiCmdSlave      : AxiStreamSlaveType;

   signal axilClock : sl;
   signal axilReset : sl;
   signal fabRefClk : sl;
   signal gtRefClk  : sl;
   signal fabClock  : sl;
   signal fabReset  : sl;

  begin
    
   axilClk <= axilClock;
   U_axilRst : entity surf.RstPipeline
      generic map (
         TPD_G => TPD_G)
      port map (
         clk    => axilClock,
         rstIn  => axilReset,
         rstOut => axilRst);

   GEN_PGP : if (SIMULATION_G = false) generate

      U_IBUFDS_GT : IBUFDS_GTE4
         generic map (
            REFCLK_EN_TX_PATH  => '0',
            REFCLK_HROW_CK_SEL => "00",  -- 2'b00: ODIV2 = O
            REFCLK_ICNTL_RX    => "00"
         )
         port map (
            I     => gtRefClkP,
            IB    => gtRefClkM,
            CEB   => '0',
            ODIV2 => fabRefClk,
            O     => gtRefClk
         );

      U_BUFG_GT : BUFG_GT
         port map (
            I       => fabRefClk,
            CE      => '1',
            CEMASK  => '1',
            CLR     => '0',
            CLRMASK => '1',
            DIV     => "000",           -- Divide by 1
            O       => fabClock
         );

      U_PwrUpRst : entity surf.PwrUpRst
         generic map(
            TPD_G         => TPD_G,
            SIM_SPEEDUP_G => SIMULATION_G)
         port map(
            clk    => fabClock,
            rstOut => fabReset);

      U_axilClock : entity surf.ClockManagerUltraScale
         generic map(
            TPD_G             => TPD_G,
            SIMULATION_G      => SIMULATION_G,
            TYPE_G            => "PLL",
            INPUT_BUFG_G      => false,
            FB_BUFG_G         => true,
            RST_IN_POLARITY_G => '1',
            NUM_CLOCKS_G      => 1,
            -- MMCM attributes
            CLKIN_PERIOD_G    => AXIL_CLK_PERIOD_C*1.0e+9, -- In units of ns
            CLKFBOUT_MULT_G   => 8,     -- 1.25GHz = 8 x 156.25 MHz, TODO: These values will depend on AXIL_CLK_FREQ_G
            CLKOUT0_DIVIDE_G  => 8      -- 156.25 MHz = 1.25GHz/8
         )
         port map(
            -- Clock Input
            clkIn     => fabClock,
            rstIn     => fabReset,
            -- Clock Outputs
            clkOut(0) => axilClock,
            -- Reset Outputs
            rstOut(0) => axilReset);

      
      ---------------------------------------
      --          PGP Module
      ---------------------------------------
      GEN_PGP_GTH : if (PGP_GT_TYPE_G = "GTH") generate
         U_Pgp : entity work.GthPgpWrapper
            generic map (
               TPD_G              => TPD_G,
               SIMULATION_G       => SIMULATION_G,
               AXIL_BASE_ADDR_G   => XBAR_CONFIG_C(PGP_INDEX_C).baseAddr,
               NUM_OF_SLOW_ADCS_G => NUM_OF_SLOW_ADCS_G,
               NUM_OF_PSCOPE_G    => NUM_OF_PSCOPE_G,
               NUM_OF_LANES_G     => NUM_OF_LANES_G,
               SLOW_ADC_AXI_CFG_G => SLOW_ADC_AXI_CFG_G,
               PGP_RATE_G         => PGP_RATE_G,
               AXIL_CLK_FREQ_G    => AXIL_CLK_FREQ_G
            )
            port map (
               -- Clock and Reset
               axilClk          => axilClock,
               axilRst          => axilReset,

               -- Master AXI-Lite Interface
               mAxilReadMaster  => mAxilReadMaster,
               mAxilReadSlave   => mAxilReadSlave,
               mAxilWriteMaster => mAxilWriteMaster,
               mAxilWriteSlave  => mAxilWriteSlave,

               -- Slave AXI-Lite Interfaces
               sAxilReadMaster  => axilReadMasters(PGP_INDEX_C),
               sAxilReadSlave   => axilReadSlaves(PGP_INDEX_C),
               sAxilWriteMaster => axilWriteMasters(PGP_INDEX_C),
               sAxilWriteSlave  => axilWriteSlaves(PGP_INDEX_C),

               -- Streaming Interfaces
               asicDataMasters  => asicDataMasters,
               asicDataSlaves   => asicDataSlaves,
               oscopeMasters    => oscopeMasters,
               oscopeSlaves     => oscopeSlaves,
               slowAdcMasters   => slowAdcMasters,
               slowAdcSlaves    => slowAdcSlaves,
               -- LEAP Transceiver Ports
               gtRefClk         => gtRefClk,
               leapTxP          => fpgaOutObTransInP,
               leapTxN          => fpgaOutObTransInM,
               leapRxP          => fpgaInObTransOutP,
               leapRxN          => fpgaInObTransOutM,

               -- Backend PCIe DAQ trigger pause for XPM (refer to TimingRx.vhd)
               pcieDaqTrigPause => pcieDaqTrigPause,
               -- SW trigger
               ssiCmd           => ssiCmd
            );
      end generate;

      GEN_PGP_GTY : if (PGP_GT_TYPE_G = "GTY") generate
         U_Pgp : entity work.GtyPgpWrapper
            generic map (
               TPD_G              => TPD_G,
               SIMULATION_G       => SIMULATION_G,
               AXIL_BASE_ADDR_G   => XBAR_CONFIG_C(PGP_INDEX_C).baseAddr,
               NUM_OF_SLOW_ADCS_G => NUM_OF_SLOW_ADCS_G,
               NUM_OF_PSCOPE_G    => NUM_OF_PSCOPE_G,
               NUM_OF_LANES_G     => NUM_OF_LANES_G,
               SLOW_ADC_AXI_CFG_G => SLOW_ADC_AXI_CFG_G,
               PGP_RATE_G         => PGP_RATE_G,
               AXIL_CLK_FREQ_G    => AXIL_CLK_FREQ_G
            )
            port map (
               -- Clock and Reset
               axilClk          => axilClock,
               axilRst          => axilReset,

               -- Master AXI-Lite Interface
               mAxilReadMaster  => mAxilReadMaster,
               mAxilReadSlave   => mAxilReadSlave,
               mAxilWriteMaster => mAxilWriteMaster,
               mAxilWriteSlave  => mAxilWriteSlave,

               -- Slave AXI-Lite Interfaces
               sAxilReadMaster  => axilReadMasters(PGP_INDEX_C),
               sAxilReadSlave   => axilReadSlaves(PGP_INDEX_C),
               sAxilWriteMaster => axilWriteMasters(PGP_INDEX_C),
               sAxilWriteSlave  => axilWriteSlaves(PGP_INDEX_C),

               -- Streaming Interfaces
               asicDataMasters  => asicDataMasters,
               asicDataSlaves   => asicDataSlaves,
               remoteDmaPause   => remoteDmaPause,
               oscopeMasters    => oscopeMasters,
               oscopeSlaves     => oscopeSlaves,
               slowAdcMasters   => slowAdcMasters,
               slowAdcSlaves    => slowAdcSlaves,

               -- LEAP Transceiver Ports
               gtRefClk         => gtRefClk,
               leapTxP          => fpgaOutObTransInP,
               leapTxN          => fpgaOutObTransInM,
               leapRxP          => fpgaInObTransOutP,
               leapRxN          => fpgaInObTransOutM,

               -- SW trigger
               ssiCmd           => ssiCmd
            );
      end generate;
   end generate;

   GEN_ROGUE_TCP : if (SIMULATION_G = true) generate

      U_ClkRst : entity surf.ClkRst
         generic map (
            CLK_PERIOD_G      => 6.4 ns,
            RST_START_DELAY_G => 0 ns,
            RST_HOLD_TIME_G   => 1 us
         )
         port map (
            clkP => axilClock,
            rst  => axilReset);

      U_axiLite : entity surf.RogueTcpMemoryWrap
         generic map (
            TPD_G      => TPD_G,
            PORT_NUM_G => 24000        -- TCP Ports [24000:24001]
         )
         port map (
            axilClk         => axilClock,
            axilRst         => axilReset,
            axilReadMaster  => mAxilReadMaster,
            axilReadSlave   => mAxilReadSlave,
            axilWriteMaster => mAxilWriteMaster,
            axilWriteSlave  => mAxilWriteSlave
         );

      GEN_VEC : for i in NUM_OF_LANES_G - 1 downto 0 generate
         U_asicData : entity surf.RogueTcpStreamWrap
            generic map (
               TPD_G         => TPD_G,
               PORT_NUM_G    => 24002+2*i,  -- TCP Ports [24002:24002+NUM_OF_LANES_G*2-1]
               SSI_EN_G      => true,
               AXIS_CONFIG_G => SSI_CONFIG_INIT_C)
            port map (
               axisClk     => axilClock,
               axisRst     => axilReset,
               sAxisMaster => asicDataMasters(i),
               sAxisSlave  => asicDataSlaves(i),
               mAxisMaster => open,
               mAxisSlave  => AXI_STREAM_SLAVE_FORCE_C);
      end generate GEN_VEC;

      U_ssiCmdData : entity surf.RogueTcpStreamWrap
      generic map (
         TPD_G         => TPD_G,
         PORT_NUM_G    => 24012,  -- TCP Ports [24012]
         SSI_EN_G      => true,
         AXIS_CONFIG_G => APP_AXIS_CONFIG_C
      )
      port map (
         axisClk     => axilClock,
         axisRst     => axilReset,
         sAxisMaster => AXI_STREAM_MASTER_INIT_C,
         sAxisSlave  => open,
         mAxisMaster => ssiCmdMaster,
         mAxisSlave  => ssiCmdSlave
      );

      GEN_SLOW_ADC_VEC : for i in NUM_OF_SLOW_ADCS_G - 1 downto 0 generate
         U_slowAdcData : entity surf.RogueTcpStreamWrap
            generic map (
               TPD_G         => TPD_G,
               PORT_NUM_G    => 24016+2*i,  -- TCP Ports [24016:24016+NUM_OF_SLOW_ADCS_G*2-1]
               SSI_EN_G      => true,
               AXIS_CONFIG_G => SLOW_ADC_AXI_CFG_G)
            port map (
               axisClk     => axilClock,
               axisRst     => axilReset,
               sAxisMaster => slowAdcMasters(i),
               sAxisSlave  => slowAdcSlaves(i),
               mAxisMaster => open,
               mAxisSlave  => AXI_STREAM_SLAVE_FORCE_C);
      end generate GEN_SLOW_ADC_VEC;

      U_SsiCmdMaster : entity surf.SsiCmdMaster
      generic map (
         TPD_G               => TPD_G,
         AXI_STREAM_CONFIG_G => PGP4_AXIS_CONFIG_C,
         SLAVE_READY_EN_G    => SIMULATION_G
      )
      port map (
         -- Streaming Data Interface
         axisClk     => axilClock,
         axisRst     => axilReset,
         sAxisMaster => ssiCmdMaster,
         sAxisSlave  => ssiCmdSlave,
         sAxisCtrl   => open,
         -- Command signals
         cmdClk      => axilClock,
         cmdRst      => axilReset,
         cmdMaster   => ssiCmd
      );
   end generate;

   ---------------------------
   -- AXI-Lite Crossbar Module
   ---------------------------
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         TPD_G              => TPD_G,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => NUM_AXIL_MASTERS_C,
         MASTERS_CONFIG_G   => XBAR_CONFIG_C)
      port map (
         sAxiWriteMasters(0) => mAxilWriteMaster,
         sAxiWriteSlaves(0)  => mAxilWriteSlave,
         sAxiReadMasters(0)  => mAxilReadMaster,
         sAxiReadSlaves(0)   => mAxilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves,
         axiClk              => axilClock,
         axiClkRst           => axilReset);

   ----------------------------------
   --       System Devices
   ----------------------------------
   U_SysDev : entity work.SystemDevices
      generic map (
         TPD_G              => TPD_G,
         SIMULATION_G       => SIMULATION_G,
         BUILD_INFO_G       => BUILD_INFO_G,
         AXIL_BASE_ADDR_G   => XBAR_CONFIG_C(SYSDEV_INDEX_C).baseAddr,
         MEMORY_INIT_FILE_G => MEMORY_INIT_FILE_G,
         AXIL_CLK_FREQ_G    => AXIL_CLK_FREQ_G
      )
      port map(
         -- AXI-Lite Interface
         axilClk         => axilClock,
         axilRst         => axilReset,
         axilReadMaster  => axilReadMasters(SYSDEV_INDEX_C),
         axilReadSlave   => axilReadSlaves(SYSDEV_INDEX_C),
         axilWriteMaster => axilWriteMasters(SYSDEV_INDEX_C),
         axilWriteSlave  => axilWriteSlaves(SYSDEV_INDEX_C),
         -------------------
         --  Top Level Ports
         -------------------
         -- Jitter Cleaner PLL Ports
         jitclnrCsL       => jitclnrCsL, 
         jitclnrIntr      => jitclnrIntr,
         jitclnrLolL      => jitclnrLolL,
         jitclnrOeL       => jitclnrOeL, 
         jitclnrRstL      => jitclnrRstL,
         jitclnrSclk      => jitclnrSclk,
         jitclnrSdio      => jitclnrSdio,
         jitclnrSdo       => jitclnrSdo, 
         jitclnrSel       => jitclnrSel,

         -- LEAP Transceiver Ports
         obTransScl      => obTransScl,
         obTransSda      => obTransSda,
         obTransResetL   => obTransResetL,
         obTransIntL     => obTransIntL,

         -- LMK61E2
         pllClkScl       => pllClkScl,
         pllClkSda       => pllClkSda,

         -- SYSMON Ports
         vPIn            => vPIn,
         vNIn            => vNIn
      );

   ----------------------------------------------------------------------
   --             Map the Application AXI-Lite Bus
   ----------------------------------------------------------------------
   axilReadMaster               <= axilReadMasters(APP_INDEX_C);
   axilReadSlaves(APP_INDEX_C)  <= axilReadSlave;
   axilWriteMaster              <= axilWriteMasters(APP_INDEX_C);
   axilWriteSlaves(APP_INDEX_C) <= axilWriteSlave;


end rtl ;
