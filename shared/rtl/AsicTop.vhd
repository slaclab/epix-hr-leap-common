-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Application interface
-------------------------------------------------------------------------------
-- This file is part of 'epix-hr-leap-common'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'epix-hr-leap-common', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.AxiLitePkg.all;
use surf.Pgp2bPkg.all;
use surf.SsiCmdMasterPkg.all;
use surf.SsiPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library l2si_core;
use l2si_core.L2SiPkg.all;

library unisim;
use unisim.vcomponents.all;

use work.AppPkg.all;
use work.CorePkg.all;

library epix_hr_core;

entity AsicTop is
   generic (
      TPD_G                   : time          := 1 ns;
      SIMULATION_G            : boolean       := false;
      SN_CLK_PERIOD_G         : real          := 10.0e-9;
      AXIL_BASE_ADDR_G        : slv(31 downto 0);
      NUM_OF_PSCOPE_G         : integer       := 4;
      NUM_DS2411_G            : integer       := 3;
      NUM_OF_SLOW_ADCS_G      : integer       := 2;
      NUM_LANES_G             : integer       := 5; 
      BUILD_INFO_G            : BuildInfoType
   );
   port (
      -- Clocking ports
      sysClk      : in sl;
      sysRst      : in sl;

      -- Trigger Interface (triggerClk domain)
      triggerClk           : out   sl;
      triggerRst           : out   sl;
      triggerData          : in    TriggerEventDataArray(1 downto 0);
      -- Optional: L1 trigger feedback (eventClk domain)
      l1Clk                : out   sl                    := '0';
      l1Rst                : out   sl                    := '0';
      l1Feedbacks          : out   TriggerL1FeedbackArray(1 downto 0):= (others => TRIGGER_L1_FEEDBACK_INIT_C);
      l1Acks               : in    slv(1 downto 0);
      -- Event streams (eventClk domain)
      eventClk             : out   sl;
      eventRst             : out   sl;
      eventTrigMsgMasters  : in    AxiStreamMasterArray(1 downto 0);
      eventTrigMsgSlaves   : out   AxiStreamSlaveArray(1 downto 0) := (others => AXI_STREAM_SLAVE_FORCE_C);
      eventTrigMsgCtrl     : in    AxiStreamCtrlArray(1 downto 0);
      eventTimingMsgMasters: in    AxiStreamMasterArray(1 downto 0);
      eventTimingMsgSlaves : out   AxiStreamSlaveArray(1 downto 0) := (others => AXI_STREAM_SLAVE_FORCE_C);
      clearReadout         : in    slv(1 downto 0);
      -- ADC/DAC Debug Trigger Interface (axilClk domain)
      oscopeAcqStart       : out   slv(NUM_OF_PSCOPE_G - 1 downto 0);
      oscopeTrigBus        : out   slv(11 downto 0);
      slowAdcAcqStart      : out   slv(NUM_OF_SLOW_ADCS_G - 1 downto 0);
      dacTrig              : out   sl;
      -- SSP Interfaces (sspClk domain)
      sspClk               : in sl;
      sspRst               : in sl;
      sspLinkUp            : in Slv24Array(NUM_LANES_G - 1 downto 0);
      sspValid             : in Slv24Array(NUM_LANES_G - 1 downto 0);
      sspData              : in Slv16Array((NUM_LANES_G * 24)-1 downto 0);
      sspSof               : in Slv24Array(NUM_LANES_G - 1 downto 0);
      sspEof               : in Slv24Array(NUM_LANES_G - 1 downto 0);
      sspEofe              : in Slv24Array(NUM_LANES_G - 1 downto 0);

      ----------------------------------------
      --      Interfaces to Application     --
      ----------------------------------------
      -- AXI-Lite Interface (axilClk domain): Address Range = [0x80000000:0xFFFFFFFF]
      axilClk            : in  sl;
      axilRst            : in  sl;
      axilReadMaster     : in  AxiLiteReadMasterType;
      axilReadSlave      : out AxiLiteReadSlaveType;
      axilWriteMaster    : in  AxiLiteWriteMasterType;
      axilWriteSlave     : out AxiLiteWriteSlaveType;

      -- Streaming Interfaces (axilClk domain)
      asicDataMasters    : out AxiStreamMasterArray(NUM_LANES_G - 1 downto 0);
      asicDataSlaves     : in  AxiStreamSlaveArray(NUM_LANES_G - 1 downto 0);

      ----------------------------------------
      --          Top Level Ports           --
      ----------------------------------------
      -- ASIC Waveform Ports
      asicDm               : in    slv(1 downto 0);
      asicGr               : out   sl;
      asicR0               : out   sl;
      asicAcq              : out   sl;
      asicSync             : out   sl;
      asicSro              : out   sl;
      asicDigRst           : out   sl;
      asicClkSyncEn        : out   sl;


      serialNumber         : inout slv(NUM_DS2411_G-1 downto 0);
      
      -- TTL external input triggers
      runTrigger      : in  sl;
      daqTrigger      : in  sl;

      -- SSI commands
      ssiCmd      : in SsiCmdMasterType;

      -- Timing link up
      v1LinkUp             : in    sl;
      v2LinkUp             : in    sl;

      boardConfig          : out   AppConfigType;

      digOut               : in   slv(1 downto 0);
      pwrGood              : in    sl;
      acqStart             : out   sl;

      rdClkSel             : out   sl
   );
end AsicTop;

architecture rtl of AsicTop is

   constant REGCTRL_AXI_INDEX_C               : natural := 0;
   constant TRIGCTRL_AXI_INDEX_C              : natural := 1;
   constant DIG_ASIC_BASE_STREAM_AXI_INDEX_C  : natural := 2;
   constant EVENTBUILDER_BASE_INDEX_C         : natural := DIG_ASIC_BASE_STREAM_AXI_INDEX_C + NUM_LANES_G;
   constant NUM_AXIL_MASTERS_C                : natural := EVENTBUILDER_BASE_INDEX_C    + NUM_LANES_G;

   constant AXIL_CONFIG_C              : AxiLiteCrossbarMasterConfigArray(NUM_AXIL_MASTERS_C-1 downto 0) := genAxiLiteConfig(NUM_AXIL_MASTERS_C, AXIL_BASE_ADDR_G, 24, 20);

   -- Master AXI-Lite Signals
   signal axilWriteMasters             : AxiLiteWriteMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilWriteSlaves              : AxiLiteWriteSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0)  := (others => AXI_LITE_WRITE_SLAVE_EMPTY_SLVERR_C);
   signal axilReadMasters              : AxiLiteReadMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilReadSlaves               : AxiLiteReadSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0) := (others => AXI_LITE_READ_SLAVE_EMPTY_SLVERR_C);
   


   -- AXI-Lite batcher
   signal axilBatcherReadMaster  : AxiLiteReadMasterArray(NUM_LANES_G - 1 downto 0);
   signal axilBatcherReadSlave   : AxiLiteReadSlaveArray(NUM_LANES_G - 1 downto 0);
   signal axilBatcherWriteMaster : AxiLiteWriteMasterArray(NUM_LANES_G - 1 downto 0);
   signal axilBatcherWriteSlave  : AxiLiteWriteSlaveArray(NUM_LANES_G - 1 downto 0);

   -- duplicated timing information
   signal eventTrigMsgMasterArray  : AxiStreamMasterArray(NUM_LANES_G - 1 downto 0);
   signal eventTrigMsgSlaveArray   : AxiStreamSlaveArray(NUM_LANES_G - 1 downto 0);

   -- Timing info synched to axilClk
   signal eventTimingMsgMasterAxiLSync  : AxiStreamMasterType;
   signal eventTimingMsgSlaveAxiLSync   : AxiStreamSlaveType;

   -- AXI/PRBS Streams, one per carrier(2M)/ASIC(HrM)
   signal mAxisMastersASIC             : AxiStreamMasterArray(NUM_LANES_G - 1 downto 0);
   signal mAxisSlavesASIC              : AxiStreamSlaveArray(NUM_LANES_G - 1 downto 0);
   
   

   -- ASIC signals (placeholders)
   signal iAsicR0                      : sl;
   signal iAsicAcq                     : sl;
   signal iAsicDigRst                  : sl;
   signal iAsicSRO                     : sl;
   signal iAsicClkSyncEn               : sl;
   signal iAsicGlblRst                 : sl;
   signal iAsicSync                    : sl;

   signal dataSend                     : sl;
   signal dataSendStreched             : sl;

   signal saciPrepReadoutAck           : sl;

   signal timingRunTrigger             : sl;
   signal timingDaqTrigger             : sl;

   -- External Signals 

   signal boardConfigSig               : AppConfigType;
   signal acqStartSig                  : sl;

begin

   triggerClk       <= axilClk;
   triggerRst       <= axilRst;

   eventClk         <= axilClk;
   eventRst         <= axilRst;

   dacTrig          <= acqStartSig;
   acqStart         <= acqStartSig;
   oscopeAcqStart   <= (others => acqStartSig);
   oscopeTrigBus    <= (others => acqStartSig);
   slowAdcAcqStart  <= (others => acqStartSig);
   boardConfig      <= boardConfigSig;

   -----------------
   -- [0] RunTrigger
   -----------------
   timingRunTrigger <= triggerData(0).valid and triggerData(0).l0Accept;

   --------------------------------------------------------
   -- [1] DaqTrigger: DaqTrigger only undergo back pressure
   --------------------------------------------------------
   timingDaqTrigger <= triggerData(1).valid and triggerData(1).l0Accept;

  U_ASIC_XBAR : entity surf.AxiLiteCrossbar
  generic map (
      TPD_G              => TPD_G,
      NUM_SLAVE_SLOTS_G  => 1,
      NUM_MASTER_SLOTS_G => NUM_AXIL_MASTERS_C,
      MASTERS_CONFIG_G   => AXIL_CONFIG_C)
  port map (
      axiClk              => axilClk,
      axiClkRst           => axilRst,
      sAxiWriteMasters(0) => axilWriteMaster,
      sAxiWriteSlaves(0)  => axilWriteSlave,
      sAxiReadMasters(0)  => axilReadMaster,
      sAxiReadSlaves(0)   => axilReadSlave,
      mAxiWriteMasters    => axilWriteMasters,
      mAxiWriteSlaves     => axilWriteSlaves,
      mAxiReadMasters     => axilReadMasters,
      mAxiReadSlaves      => axilReadSlaves);

-----------------------------------------------------------------------------
-- Regiester control
-----------------------------------------------------------------------------
   U_RegCtrl : entity work.RegisterControlDualClock
      generic map (
         TPD_G           => TPD_G,
         SN_CLK_PERIOD_G    => SN_CLK_PERIOD_G,
         NUM_DS2411_G    => NUM_DS2411_G,
         BUILD_INFO_G    => BUILD_INFO_G
      )
      port map (
         axilClk          => axilClk,
         axilRst          => axilRst,
         -- AXI-Lite Register Interface (axiClk domain)
         axiReadMaster   => axilReadMasters(REGCTRL_AXI_INDEX_C),
         axiReadSlave    => axilReadSlaves(REGCTRL_AXI_INDEX_C),
         axiWriteMaster  => axilWriteMasters(REGCTRL_AXI_INDEX_C),
         axiWriteSlave   => axilWriteSlaves(REGCTRL_AXI_INDEX_C),
         -- Register Inputs/Outputs (axilClk domain)
         boardConfig    => boardConfigSig,
         -- 1-wire board ID interfaces
         serialIdIo     => serialNumber,
         -- ASICs acquisition signals
         acqStart       => acqStartSig,
         asicR0         => iAsicR0,
         asicAcq        => iAsicAcq,
         asicDigRst     => iAsicDigRst,
         asicSRO        => iAsicSRO,
         asicClkSyncEn  => iAsicClkSyncEn,
         asicGlblRst    => iAsicGlblRst,
         asicSync       => iAsicSync,
         -- sys clock signals (ASIC RD clock domain)
         sysRst         => sysRst,
         sysClk         => sysClk,
         saciReadoutReq => open,
         saciReadoutAck => saciPrepReadoutAck,
         errInhibit     => open,
         rdClkSel       => rdClkSel,
         v1LinkUp       => v1LinkUp,
         v2LinkUp       => v2LinkUp,
         digOut         => digOut,
         pwrGood        => pwrGood
      );

   ---------------------
   -- Trig control    --
   --------------------- 
   U_TrigControl : entity epix_hr_core.TrigControlAxi
      generic map(
         PULSE_WIDTH_G     => 1
      )
      port map (
         -- Trigger outputs
         appClk            => axilClk,
         appRst            => axilRst,
         acqStart          => acqStartSig,
         dataSend          => dataSend,
         -- External trigger inputs
         runTrigger        => runTrigger,
         daqTrigger        => daqTrigger,
         -- PGP clocks and reset
         sysClk            => axilClk,
         sysRst            => axilRst,
         -- SW trigger in (from VC)
         ssiCmd            => ssiCmd,
         -- Fiber optic trigger (axilClk domain)
         pgpRxOut          => PGP2B_RX_OUT_INIT_C,
         -- Fiducial code output
         opCodeOut         => open,
         -- Timing Triggers
         timingRunTrigger  => timingRunTrigger,
         timingDaqTrigger  => timingDaqTrigger,
         -- AXI lite slave port for register access
         axilClk           => axilClk,
         axilRst           => axilRst,
         sAxilWriteMaster  => axilWriteMasters(TRIGCTRL_AXI_INDEX_C),
         sAxilWriteSlave   => axilWriteSlaves(TRIGCTRL_AXI_INDEX_C),
         sAxilReadMaster   => axilReadMasters(TRIGCTRL_AXI_INDEX_C),
         sAxilReadSlave    => axilReadSlaves(TRIGCTRL_AXI_INDEX_C),

         runTrigPause      => eventTrigMsgCtrl(0).pause,
         daqTrigPause      => eventTrigMsgCtrl(1).pause
      );
   
   -------------------------------------------------
   -- AxiStream repeater
   -------------------------------------------------
   U_AxiStreamRepeater_timing : entity surf.AxiStreamRepeater
      generic map(
         TPD_G                => TPD_G,
         NUM_MASTERS_G        => NUM_LANES_G,
         INCR_AXIS_ID_G       => false,
         INPUT_PIPE_STAGES_G  => 0,
         OUTPUT_PIPE_STAGES_G => 0)
      port map(
         -- Clock and reset
         axisClk      => axilClk,
         axisRst      => axilRst,
         -- Slave
         sAxisMaster  => eventTrigMsgMasters(1),
         sAxisSlave   => eventTrigMsgSlaves(1),
         -- Masters
         mAxisMasters => eventTrigMsgMasterArray,
         mAxisSlaves  => eventTrigMsgSlaveArray
      );
   
      -----------------------------------------------------------------------------
      -- generate stream frames
      -----------------------------------------------------------------------------
      G_ASICS : for i in NUM_LANES_G - 1 downto 0 generate
         U_DigitalAsicStreamAxiV2  : entity work.DigitalAsicStreamAxiV2
            generic map(
               TPD_G               => TPD_G,
               VC_NO_G             => "0000",
               LANE_NO_G           => toSlv(i, 4),
               ASIC_NO_G           => toSlv(i, 3),
               LANES_NO_G          => 24,
               AXIL_ERR_RESP_G     => AXI_RESP_DECERR_C
               )
            port map(
               -- Deserialized data port
               deserClk          => sspClk,
               deserRst          => sspRst,
               rxValid           => sspValid(i),
               rxData            => sspData(24*i+23 downto 24*i),
               rxSof             => sspSof(i),
               rxEof             => sspEof(i),
               rxEofe            => sspEofe(i),
            
               -- AXI lite slave port for register access
               axilClk           => axilClk,
               axilRst           => axilRst,
               sAxilWriteMaster  => axilWriteMasters(DIG_ASIC_BASE_STREAM_AXI_INDEX_C + i),
               sAxilWriteSlave   => axilWriteSlaves(DIG_ASIC_BASE_STREAM_AXI_INDEX_C + i),
               sAxilReadMaster   => axilReadMasters(DIG_ASIC_BASE_STREAM_AXI_INDEX_C + i),
               sAxilReadSlave    => axilReadSlaves(DIG_ASIC_BASE_STREAM_AXI_INDEX_C + i),
            
               -- AXI data stream output
               axisClk           => axilClk,
               axisRst           => axilRst,
               mAxisMaster       => mAxisMastersASIC(i),
               mAxisSlave        => mAxisSlavesASIC(i),
            
               -- acquisition number input to the header
               acqNo             => boardConfigSig.acqCnt,
               startRdout        => dataSendStreched
            );
      end generate;
      
      G_EventBuilders : for i in 0 to NUM_LANES_G-1 generate
         U_EventBuilder : entity surf.AxiStreamBatcherEventBuilder
            generic map (
               TPD_G          => TPD_G,
               NUM_SLAVES_G   => 2,
               MODE_G         => "ROUTED",
               TDEST_ROUTES_G => (
                  0           => "0000000-",
                  1           => "00000010"),
               TRANS_TDEST_G  => X"01",
               AXIS_CONFIG_G  => SSI_CONFIG_INIT_C
               )
               port map (
               -- Clock and Reset
               axisClk                    => axilClk,
               axisRst                    => axilRst,
               -- AXI-Lite Interface (axisClk domain)
               axilReadMaster             => axilReadMasters(EVENTBUILDER_BASE_INDEX_C + i),
               axilReadSlave              => axilReadSlaves(EVENTBUILDER_BASE_INDEX_C + i),
               axilWriteMaster            => axilWriteMasters(EVENTBUILDER_BASE_INDEX_C + i),
               axilWriteSlave             => axilWriteSlaves(EVENTBUILDER_BASE_INDEX_C + i),
               -- Inbound Master AXIS Interfaces
               sAxisMasters(0)            => eventTrigMsgMasterArray(i),
               sAxisMasters(1)            => mAxisMastersASIC(i),
               -- Inbound Slave AXIS Interfaces
               sAxisSlaves(0)             => eventTrigMsgSlaveArray(i),
               sAxisSlaves(1)             => mAxisSlavesASIC(i),
               -- Outbound AXIS
               mAxisMaster                => asicDataMasters(i), --to core
               mAxisSlave                 => asicDataSlaves(i)   --to core
               );
   end generate;


   
      U_DataSendStretcher : entity surf.SynchronizerOneShot 
         generic map(
            TPD_G          => TPD_G,
            RST_ASYNC_G    => false,
            RST_POLARITY_G => '1',    -- '1' for active HIGH reset, '0' for active LOW reset
            BYPASS_SYNC_G  => false,  -- Bypass RstSync module for synchronous data configuration
            IN_POLARITY_G  => '1',    -- 0 for active LOW, 1 for active HIGH
            OUT_POLARITY_G => '1',    -- 0 for active LOW, 1 for active HIGH
            OUT_DELAY_G    => 3,      -- Stages in output sync chain
            PULSE_WIDTH_G  => 8       -- one-shot pulse width duration (units of clk cycles)
         )
         port map(
            clk     => axilClk,
            rst     => axilRst,
            dataIn  => dataSend,
            dataOut => dataSendStreched
         );

   
   asicDigRst <= iAsicDigRst;
   asicAcq <= iAsicAcq;
   asicSro <= iAsicSRO;
   asicGr <= iAsicGlblRst;
   asicSync <= iAsicSync;
   asicR0   <= iAsicR0;
   asicClkSyncEn <= iAsicClkSyncEn;

end architecture;
