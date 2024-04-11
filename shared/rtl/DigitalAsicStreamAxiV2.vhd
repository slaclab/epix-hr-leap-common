-------------------------------------------------------------------------------
-- File       : DigitalAsicStreamAxi.vhd
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- This file is part of 'EPIX HR Firmware'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'EPIX HR Firmware', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

LIBRARY ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_misc.all;
use IEEE.numeric_std.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;

entity DigitalAsicStreamAxiV2 is 
   generic (
      TPD_G           	   : time := 1 ns;
      VC_NO_G              : slv(3 downto 0)  := "0000";
      LANE_NO_G            : slv(3 downto 0)  := "0000";
      ASIC_NO_G            : slv(2 downto 0)  := "000";
      LANES_NO_G           : natural := 6;
      GAIN_BIT_REMAP_G     : boolean := true; --true moves LSB to MSB
      AXIL_ERR_RESP_G      : slv(1 downto 0)  := AXI_RESP_DECERR_C
   );
   port ( 
      -- Deserialized data port
      deserClk          : in  sl;
      deserRst          : in  sl;
      rxValid           : in  slv(LANES_NO_G-1 downto 0);
      rxData            : in  Slv16Array(LANES_NO_G-1 downto 0);
      rxSof             : in  slv(LANES_NO_G-1 downto 0);
      rxEof             : in  slv(LANES_NO_G-1 downto 0);
      rxEofe            : in  slv(LANES_NO_G-1 downto 0);
      
      
      -- AXI lite slave port for register access
      axilClk           : in  sl;
      axilRst           : in  sl;
      sAxilWriteMaster  : in  AxiLiteWriteMasterType;
      sAxilWriteSlave   : out AxiLiteWriteSlaveType;
      sAxilReadMaster   : in  AxiLiteReadMasterType;
      sAxilReadSlave    : out AxiLiteReadSlaveType;
      
      -- AXI data stream output
      axisClk           : in  sl;
      axisRst           : in  sl;
      mAxisMaster       : out AxiStreamMasterType;
      mAxisSlave        : in  AxiStreamSlaveType;
      
      -- acquisition number input to the header
      acqNo             : in  slv(31 downto 0);
      
      -- readout request input
      startRdout        : in  sl
      
   );
end DigitalAsicStreamAxiV2;


-- Define architecture
architecture RTL of DigitalAsicStreamAxiV2 is

   -- makes the fifo input with 2B per stream
   constant AXI_STREAM_CONFIG_I_C : AxiStreamConfigType   := ssiAxiStreamConfig(2*LANES_NO_G, TKEEP_COMP_C);
   constant AXI_STREAM_CONFIG_W_C : AxiStreamConfigType   := ssiAxiStreamConfig(48, TKEEP_COMP_C);
   constant AXI_STREAM_CONFIG_O_C : AxiStreamConfigType   := ssiAxiStreamConfig(16, TKEEP_COMP_C);
   constant VECTOR_OF_ONES_C  : slv(LANES_NO_G-1 downto 0) := (others => '1');
   constant VECTOR_OF_ZEROS_C : slv(LANES_NO_G-1 downto 0) := (others => '0');
   -- PGP3 protocol is using 128bit (check for global constant for this configuration)
   
   type StateType is (IDLE_S, WAIT_SOF_S, HDR_S, DATA_S, TIMEOUT_S, TAIL_S);
   
   type RegType is record
      state                   : StateType;
      stateD1                 : StateType;
      disableLane             : slv(LANES_NO_G-1 downto 0);
      enumDisLane             : slv(LANES_NO_G-1 downto 0);
      gainBitRemap            : slv(LANES_NO_G-1 downto 0);
      dataReqLane             : slv(15 downto 0);
      dataCntLane             : Slv16Array(LANES_NO_G-1 downto 0);
      dataCntLaneReg          : Slv16Array(LANES_NO_G-1 downto 0);
      dataCntLaneMin          : Slv16Array(LANES_NO_G-1 downto 0);
      dataCntLaneMax          : Slv16Array(LANES_NO_G-1 downto 0);
      dataDlyLane             : Slv16Array(LANES_NO_G-1 downto 0);
      dataDlyLaneReg          : Slv16Array(LANES_NO_G-1 downto 0);
      dataOvfLane             : Slv16Array(LANES_NO_G-1 downto 0);
      stCnt                   : slv(15 downto 0);
      frmSize                 : slv(15 downto 0);
      frmMax                  : slv(15 downto 0);
      frmMin                  : slv(15 downto 0);
      timeoutCntLane          : Slv16Array(LANES_NO_G-1 downto 0);
      acqNo                   : Slv32Array(1 downto 0);
      frmCnt                  : slv(31 downto 0); 
      rstCnt                  : sl;
      startRdSync             : slv(3 downto 0);
      dFifoRd                 : slv(LANES_NO_G-1 downto 0);
      fillOnFailEn            : sl;
      tempDisableLane         : slv(LANES_NO_G-1 downto 0);
      fillOnFailLastMask      : slv(LANES_NO_G-1 downto 0);
      fillOnFailCnt           : slv(31 downto 0); 
      fillOnFailCntLane       : Slv32Array(LANES_NO_G-1 downto 0);      
      fillOnFailTimeout       : slv(31 downto 0); 
      fillOnFailTimeoutOffset : slv(31 downto 0); 
      fillOnFailTimeoutTotal  : slv(31 downto 0); 
      fillOnFailTimeoutCntr   : slv(31 downto 0); 
      wsofStateCntrMin        : slv(15 downto 0);
      wsofStateCntrMax        : slv(15 downto 0);
      wsofStateCntr           : slv(15 downto 0);
      dataStateCntrMin        : slv(15 downto 0);
      dataStateCntrMax        : slv(15 downto 0);
      dataStateCntr           : slv(15 downto 0);
      hdrStateCntrMin         : slv(15 downto 0);
      hdrStateCntrMax         : slv(15 downto 0); 
      hdrStateCntr            : slv(15 downto 0); 
      frameCyclesCtrMin       : slv(15 downto 0);
      frameCyclesCntrMax      : slv(15 downto 0); 
      frameCyclesCntr         : slv(15 downto 0);    
      readyLowCyclesCtrMin    : slv(15 downto 0);    
      readyLowCyclesCtrMax    : slv(15 downto 0);    
      readyLowCyclesCtr       : slv(15 downto 0);
      txMaster                : AxiStreamMasterType;
      axilWriteSlave          : AxiLiteWriteSlaveType;
      axilReadSlave           : AxiLiteReadSlaveType;
   end record;

   constant REG_INIT_C : RegType := (
      state                       => IDLE_S,
      stateD1                     => IDLE_S,
      disableLane                 => (others=>'0'),
      enumDisLane                 => (others=>'0'),
      gainBitRemap                => (others=>'1'),
      dataReqLane                 => (others=>'0'),
      dataCntLane                 => (others=>(others=>'0')),
      dataCntLaneReg              => (others=>(others=>'0')),
      dataCntLaneMin              => (others=>(others=>'1')),
      dataCntLaneMax              => (others=>(others=>'0')),
      dataDlyLane                 => (others=>(others=>'0')),
      dataDlyLaneReg              => (others=>(others=>'0')),
      dataOvfLane                 => (others=>(others=>'0')),
      fillOnFailCntLane           => (others=>(others=>'0')),
      stCnt                       => (others=>'0'),
      frmSize                     => (others=>'0'),
      frmMax                      => (others=>'0'),
      frmMin                      => (others=>'1'),
      timeoutCntLane              => (others=>(others=>'0')),
      acqNo                       => (others=>(others=>'0')),
      frmCnt                      => (others=>'0'),
      rstCnt                      => '0',
      fillOnFailEn                => '0',
      fillOnFailCnt               => (others=>'0'),
      fillOnFailTimeout           => (others=>'0'),
      fillOnFailTimeoutOffset     => (others=>'0'),
      fillOnFailTimeoutTotal      => (others=>'0'),
      tempDisableLane             => (others=>'0'),
      fillOnFailLastMask          => (others=>'0'),
      fillOnFailTimeoutCntr       => (others=>'0'),
      startRdSync                 => (others=>'0'),
      dFifoRd                     => (others=>'0'),
      wsofStateCntrMin            => (others=>'0'),
      wsofStateCntrMax            => (others=>'0'),
      wsofStateCntr               => (others=>'0'),
      dataStateCntrMin            => (others=>'0'),
      dataStateCntrMax            => (others=>'0'),
      dataStateCntr               => (others=>'0'),
      hdrStateCntrMin             => (others=>'0'),
      hdrStateCntrMax             => (others=>'0'), 
      hdrStateCntr                => (others=>'0'),  
      frameCyclesCtrMin           => (others=>'0'),  
      frameCyclesCntrMax          => (others=>'0'),   
      frameCyclesCntr             => (others=>'0'),    
      readyLowCyclesCtrMin        => (others=>'0'),        
      readyLowCyclesCtrMax        => (others=>'0'),        
      readyLowCyclesCtr           => (others=>'0'),                     
      txMaster                    => AXI_STREAM_MASTER_INIT_C,
      axilWriteSlave              => AXI_LITE_WRITE_SLAVE_INIT_C,
      axilReadSlave               => AXI_LITE_READ_SLAVE_INIT_C
   );
   
   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;
   
   signal dFifoRd       : slv(LANES_NO_G-1 downto 0);
   signal dFifoEofe     : slv(LANES_NO_G-1 downto 0);
   signal dFifoEof      : slv(LANES_NO_G-1 downto 0);
   signal dFifoSof      : slv(LANES_NO_G-1 downto 0);
   signal dFifoValid    : slv(LANES_NO_G-1 downto 0);
   signal dFifoOut      : slv16Array(LANES_NO_G-1 downto 0);
   signal dFifoExtData  : slv(16*LANES_NO_G-1 downto 0) := (others => '0');
   signal dFifoRst      : sl;

   signal rxDataReMap   : Slv16Array(LANES_NO_G-1 downto 0);
   signal rxFull        : slv(LANES_NO_G-1 downto 0);
   signal notFull       : slv(LANES_NO_G-1 downto 0);
   signal wrAck         : slv(LANES_NO_G-1 downto 0);
   signal overflow      : slv(LANES_NO_G-1 downto 0);
   signal wrDataCount   : slv9Array(LANES_NO_G-1 downto 0);
   signal rdDataCount   : slv9Array(LANES_NO_G-1 downto 0);
   signal underflow     : slv(LANES_NO_G-1 downto 0);
   signal empty         : slv(LANES_NO_G-1 downto 0);
   signal DeserAxisDualClockFifoFull : sl;
   signal DeserAxisDualClockFifoWrCnt : slv(12 downto 0);
   
   signal startRdSync   : sl;
   
   signal txSlave          : AxiStreamSlaveType;
   signal sAxisMasterWide  : AxiStreamMasterType;
   signal sAxisSlaveWide   : AxiStreamSlaveType;
   
   signal acqNoSync     : slv(31 downto 0);
   
   signal axilWriteMaster  : AxiLiteWriteMasterType;
   signal axilWriteSlave   : AxiLiteWriteSlaveType;
   signal axilReadMaster   : AxiLiteReadMasterType;
   signal axilReadSlave    : AxiLiteReadSlaveType;

   signal cycleCounter     : slv(15 downto 0);
   
   attribute keep : string;
   attribute keep of r           : signal is "true";
   attribute keep of startRdSync : signal is "true";
   attribute keep of dFifoEofe   : signal is "true";
   attribute keep of dFifoEof    : signal is "true";
   attribute keep of dFifoSof    : signal is "true";
   attribute keep of dFifoValid  : signal is "true";

   attribute keep of empty       : signal is "true";
   attribute keep of underflow   : signal is "true";
   attribute keep of rdDataCount : signal is "true";
   attribute keep of wrDataCount : signal is "true";
   attribute keep of overflow    : signal is "true";
   attribute keep of wrAck       : signal is "true";
   attribute keep of notFull     : signal is "true";
   attribute keep of DeserAxisDualClockFifoFull : signal is "true";
   attribute keep of DeserAxisDualClockFifoWrCnt : signal is "true";
   attribute keep of cycleCounter : signal is "true";

begin
   
   ----------------------------------------------------------------------------
   -- Cross clocking synchronizers
   ----------------------------------------------------------------------------
   cycleCounter <= r.stCnt(15 downto 0);

   AcqNoSync_U : entity surf.SynchronizerVector
   generic map (
      WIDTH_G => 32)
   port map (
      clk     => deserClk,
      rst     => deserRst,
      dataIn  => acqNo,
      dataOut => acqNoSync
   );
   
   SroSync_U : entity surf.SynchronizerEdge
   port map (
      clk         => deserClk,
      rst         => deserRst,
      dataIn      => startRdout,
      risingEdge  => startRdSync
   );
   
   AxilSync_U : entity surf.AxiLiteAsync
   generic map(
      PIPE_STAGES_G => 1
   )
   port map (
      -- Slave Port
      sAxiClk         => axilClk,
      sAxiClkRst      => axilRst,
      sAxiWriteMaster => sAxilWriteMaster,
      sAxiWriteSlave  => sAxilWriteSlave,
      sAxiReadMaster  => sAxilReadMaster,
      sAxiReadSlave   => sAxilReadSlave,
      -- Master Port
      mAxiClk         => deserClk,
      mAxiClkRst      => deserRst,
      mAxiWriteMaster => axilWriteMaster,
      mAxiWriteSlave  => axilWriteSlave,
      mAxiReadMaster  => axilReadMaster,
      mAxiReadSlave   => axilReadSlave
   );
   
   ----------------------------------------------------------------------------
   -- Instatiate one FIFO per data stream.
   ----------------------------------------------------------------------------
   G_FIFO : for i in 0 to LANES_NO_G-1 generate
     
      -- ePixHR10k has the gian bit defined as LSB and it is remapped as MSB.
      U_GainBitReMap : process (rxData, r)
      begin
        if (GAIN_BIT_REMAP_G = true) then
          rxDataReMap(i)(14 downto 0)   <= rxData(i)(15 downto 1);
          rxDataReMap(i)(15)            <= rxData(i)(0);
        else
          rxDataReMap(i) <= rxData(i);
        end if;
      end process;       
   
      -- async fifo for data
      DataFifo_U : entity surf.FifoCascade
      generic map (
         GEN_SYNC_FIFO_G   => true,
         FWFT_EN_G         => true,
         ADDR_WIDTH_G      => 9,
         DATA_WIDTH_G      => 19
         )
      port map (
         rst               => dFifoRst,
         wr_clk            => deserClk,
         wr_en             => rxValid(i),
         full              => rxFull(i),
         wr_data_count     => wrDataCount(i),
         not_full          => notFull(i),
         overflow          => overflow(i),
         wr_ack            => wrAck(i),
         din(15 downto 0)  => rxDataReMap(i),
         din(16)           => rxEofe(i),
         din(17)           => rxEof(i),
         din(18)           => rxSof(i),
         rd_clk            => deserClk,
         rd_en             => dFifoRd(i),
         dout(15 downto 0) => dFifoOut(i),
         dout(16)          => dFifoEofe(i),
         dout(17)          => dFifoEof(i),
         dout(18)          => dFifoSof(i),
         valid             => dFifoValid(i),
         empty             => empty(i),
         rd_data_count     => rdDataCount(i),
         underflow         => underflow(i)
      );
      
      -- in cPix seeing corrupted junk being stuck in one of the lanes 
      -- this can only be cleared by the FSM stuck waiting for data
      dFifoRst <= deserRst or startRdSync;
      
      
      dataExt : process(dFifoOut, r.disableLane, r.enumDisLane, r.tempDisableLane, r.fillOnFailEn)
      begin
         if r.disableLane(i) = '1' or (r.fillOnFailEn = '1' and r.tempDisableLane(i) = '1') then
            if r.enumDisLane(i) = '0' then
               dFifoExtData(16*i+15 downto 16*i) <= (others => '0');
            else
               dFifoExtData(16*i+15 downto 16*i) <= toSlv(i,16);
            end if;
         else
            dFifoExtData(16*i+15 downto 16*i) <= dFifoOut(i);
         end if;
      end process;
         
   end generate;


   comb : process (deserRst, axilReadMaster, axilWriteMaster, txSlave, r, 
      acqNoSync, dFifoExtData, dFifoValid, dFifoSof, dFifoEof, dFifoEofe, startRdSync, rxValid, rxSof, rxEof, rxEofe, rxFull) is
      variable v             : RegType;
      variable regCon        : AxiLiteEndPointType;
      variable fillOnFailEnV : slv(LANES_NO_G-1 downto 0);
   begin
      v := r;
      
      v.rstCnt := '0';
      v.dFifoRd := (others=>'0');
      v.stateD1 := r.state;
      v.startRdSync(3) := startRdSync;
      v.startRdSync(2) := r.startRdSync(3);
      v.startRdSync(1) := r.startRdSync(2);
      v.startRdSync(0) := r.startRdSync(1);
      fillOnFailEnV := (others => r.fillOnFailEn);

      axiSlaveWaitTxn(regCon, axilWriteMaster, axilReadMaster, v.axilWriteSlave, v.axilReadSlave);
      
      axiSlaveRegisterR(regCon, x"000",  0, r.frmCnt);
      axiSlaveRegisterR(regCon, x"004",  0, r.frmSize);
      axiSlaveRegisterR(regCon, x"008",  0, r.frmMax);
      axiSlaveRegisterR(regCon, x"00C",  0, r.frmMin);
      axiSlaveRegister (regCon, x"024",  0, v.rstCnt);
      axiSlaveRegister (regCon, x"028",  0, v.dataReqLane);
      axiSlaveRegister (regCon, x"02C",  0, v.disableLane);
      axiSlaveRegister (regCon, x"030",  0, v.enumDisLane);
      axiSlaveRegister (regCon, x"034",  0, v.gainBitRemap);
      axiSlaveRegister (regCon, x"038",  0, v.fillOnFailEn);
      axiSlaveRegister (regCon, x"03C",  0, v.fillOnFailTimeoutOffset);
      axiSlaveRegister (regCon, x"040",  0, v.fillOnFailTimeout);
      axiSlaveRegisterR(regCon, x"044",  0, r.fillOnFailCnt);
      axiSlaveRegisterR(regCon, x"048",  0, r.fillOnFailLastMask);
      axiSlaveRegisterR(regCon, x"04C",  0, std_logic_vector(to_unsigned(StateType'pos(r.state), 8))); 
      axiSlaveRegisterR(regCon, x"05C",  0, r.wsofStateCntrMin);
      axiSlaveRegisterR(regCon, x"060",  0, r.wsofStateCntrMax);
      axiSlaveRegisterR(regCon, x"064",  0, r.wsofStateCntr);
      axiSlaveRegisterR(regCon, x"068",  0, r.dataStateCntrMin);
      axiSlaveRegisterR(regCon, x"06C",  0, r.dataStateCntrMax);
      axiSlaveRegisterR(regCon, x"070",  0, r.dataStateCntr);
      axiSlaveRegisterR(regCon, x"074",  0, r.hdrStateCntrMin);
      axiSlaveRegisterR(regCon, x"078",  0, r.hdrStateCntrMax);
      axiSlaveRegisterR(regCon, x"07C",  0, r.hdrStateCntr);
      axiSlaveRegisterR(regCon, x"080",  0, r.frameCyclesCtrMin);
      axiSlaveRegisterR(regCon, x"084",  0, r.frameCyclesCntrMax);
      axiSlaveRegisterR(regCon, x"088",  0, r.frameCyclesCntr);
      axiSlaveRegisterR(regCon, x"08C",  0, r.readyLowCyclesCtrMin);
      axiSlaveRegisterR(regCon, x"090",  0, r.readyLowCyclesCtrMax);
      axiSlaveRegisterR(regCon, x"094",  0, r.readyLowCyclesCtr);

      for i in 0 to (LANES_NO_G-1) loop
         axiSlaveRegisterR(regCon, x"100"+toSlv(i*4,12),  0, r.timeoutCntLane(i));
         axiSlaveRegisterR(regCon, x"200"+toSlv(i*4,12),  0, r.dataCntLane(i));
         axiSlaveRegisterR(regCon, x"300"+toSlv(i*4,12),  0, r.dataCntLaneReg(i));
         axiSlaveRegisterR(regCon, x"400"+toSlv(i*4,12),  0, r.dataCntLaneMin(i));
         axiSlaveRegisterR(regCon, x"500"+toSlv(i*4,12),  0, r.dataCntLaneMax(i));
         axiSlaveRegisterR(regCon, x"600"+toSlv(i*4,12),  0, r.dataDlyLaneReg(i));
         axiSlaveRegisterR(regCon, x"700"+toSlv(i*4,12),  0, r.dataOvfLane(i));
         axiSlaveRegisterR(regCon, x"800"+toSlv(i*4,12),  0, r.fillOnFailCntLane(i));
      end loop;
      
      axiSlaveDefault(regCon, v.axilWriteSlave, v.axilReadSlave, AXIL_ERR_RESP_G);
      
      -- axi stream logic

      -- sync acquisition number
      v.acqNo(0) := acqNoSync;
      
      v.fillOnFailTimeoutTotal := r.fillOnFailTimeout + r.fillOnFailTimeoutOffset;
      
      -- Reset strobing Signals
      if (txSlave.tReady = '1') then
         v.txMaster.tValid := '0';
         v.txMaster.tLast  := '0';
         v.txMaster.tUser  := (others => '0');
         v.txMaster.tKeep  := (others => '1');
         v.txMaster.tStrb  := (others => '1');
         v.txMaster.tData  := (others => '0');
      end if;
      
      case r.state is
         when IDLE_S =>

            -- reset temporary disable for autofill on failure
            v.tempDisableLane := (others => '0');
            v.fillOnFailTimeoutCntr := (others => '0');
            if startRdSync = '1' then
               v.state := WAIT_SOF_S;
            end if;
            
         -- condition to enter here is that startRdSync already arrived
         when WAIT_SOF_S =>
         
            --keeps flushing data until all SOF show up
            for i in 0 to (LANES_NO_G-1) loop
               if dFifoSof(i) = '0' and dFifoValid(i) = '1' then
                  v.dFifoRd(i) := '1';
               end if;             
            end loop;
            
            -- next SRO while waiting for previous SOF. Too late to recover using autoFillOnFailure
            for i in 0 to (LANES_NO_G-1) loop
               if startRdSync = '1' and dFifoSof(i) = '0' then
                  v.timeoutCntLane(i) := r.timeoutCntLane(i) + 1;
                end if;
            end loop;
            
            if ((dFifoSof(LANES_NO_G-1 downto 0) or r.disableLane or (fillOnFailEnV and r.tempDisableLane)) = VECTOR_OF_ONES_C(LANES_NO_G-1 downto 0)) then
               v.acqNo(1) := r.acqNo(0);
               v.state := HDR_S;
               v.fillOnFailTimeoutCntr := (others => '0');
               -- 
            else -- If not transitioning to next state, count one to fillOnFail counter
               if (r.fillOnFailTimeoutCntr < r.fillOnFailTimeoutTotal) then
                  v.fillOnFailTimeoutCntr := r.fillOnFailTimeoutCntr + 1;
               end if;
            end if;

            -- reach limit to fill on fail counter, disable lane temporarily for this image
            if (r.fillOnFailTimeoutCntr >= r.fillOnFailTimeoutTotal) then
               for i in 0 to (LANES_NO_G-1) loop
                  if dFifoSof(i) = '0' and r.disableLane(i) = '0' and r.fillOnFailEn = '1' then
                     v.tempDisableLane(i) := '1';
                  end if;
               end loop;
            end if;

            v.stCnt := (others=>'0');
            
         when HDR_S =>
            ------------------------------------------------------------------
            -- HEADER
            ------------------------------------------------------------------    
            if v.txMaster.tValid = '0' then
               v.txMaster.tValid := '1';
               v.state := DATA_S;
               v.txMaster.tData(31 downto  0) := x"000000" & LANE_NO_G & VC_NO_G;
               v.txMaster.tData(63 downto 32) := r.acqNo(1)(31 downto 0);
               v.txMaster.tData(79 downto 64) := (15 downto 3 => '0') & ASIC_NO_G;
               v.txMaster.tData(95 downto 80) := x"0000";
               ssiSetUserSof(AXI_STREAM_CONFIG_I_C, v.txMaster, '1');
            end if;
            -- No fillOnFail handling here

         when DATA_S =>
            -- if comb valid is set to 0, means that ready is 1
            if ((dFifoValid or r.disableLane or (r.tempDisableLane and fillOnFailEnV)) = VECTOR_OF_ONES_C(LANES_NO_G-1 downto 0)) and v.txMaster.tValid = '0' then
               
               v.txMaster.tValid := '1';
               v.txMaster.tData(16*LANES_NO_G-1 downto 0) := dFifoExtData;
               --v.txMaster.tData(16*LANES_NO_G-1 downto 0) := x"0000_0001_0002_0003_0004_0005_0006_0007_0008_0009_000A"  & r.stCnt;
               
               v.fillOnFailTimeoutCntr := (others => '0');
               
               v.dFifoRd := (others=>'1');
                           
               v.stCnt := r.stCnt + 1;
               if r.stCnt = r.dataReqLane then 
                  v.frmSize := r.stCnt;
                  v.stCnt := (others=>'0');
                  
                  if r.frmMax <= v.frmSize then
                     v.frmMax := v.frmSize;
                  end if;
                  
                  if r.frmMin >= v.frmSize then
                     v.frmMin := v.frmSize;
                  end if;
                  
                  v.frmCnt := r.frmCnt + 1;
                  
                  v.state := TAIL_S;

                  -- Increment monitor registers before exit to IDLE state
                  for i in 0 to (LANES_NO_G-1) loop
                     if r.tempDisableLane(i) = '1' and r.fillOnFailEn = '1' then
                        v.fillOnFailCntLane(i) := r.fillOnFailCntLane(i) + 1;
                     end if;
                  end loop;                  
                  if (r.fillOnFailEn = '1' and or_reduce(r.tempDisableLane) = '1') then
                     v.fillOnFailCnt := r.fillOnFailCnt + 1;
                  end if;
               end if;  
            
            elsif startRdSync = '1' then
               v.state := TIMEOUT_S;
               -- Increment monitor registers before exit to TIMEOUT_S state
               for i in 0 to (LANES_NO_G-1) loop
                  if (dFifoValid(i) or r.disableLane(i)) = '0' then
                     v.timeoutCntLane(i) := r.timeoutCntLane(i) + 1;
                  end if;
                  if r.tempDisableLane(i) = '1' and r.fillOnFailEn = '1' then
                     v.fillOnFailCntLane(i) := r.fillOnFailCntLane(i) + 1;
                  end if;
               end loop;
               if (r.fillOnFailEn = '1' and or_reduce(r.tempDisableLane) = '1') then
                  v.fillOnFailCnt := r.fillOnFailCnt + 1;
               end if;
            else -- if non of the above, increment fill-on-fail timeout counter
               if (r.fillOnFailTimeoutCntr >= r.fillOnFailTimeoutOffset) then
                  for i in 0 to (LANES_NO_G-1) loop
                     if dFifoValid(i) = '0' and r.disableLane(i) = '0' and r.fillOnFailEn = '1' then
                        v.tempDisableLane(i) := '1';
                     end if;
                  end loop;
               else
                  v.fillOnFailTimeoutCntr := r.fillOnFailTimeoutCntr + 1;
               end if;               
            end if;
            v.fillOnFailLastMask := v.tempDisableLane;

         when TIMEOUT_S =>
            if v.txMaster.tValid = '0' then
               v.txMaster.tLast := '1';
               v.txMaster.tValid := '1';
               v.tempDisableLane := (others => '0');
               v.fillOnFailTimeoutCntr := (others => '0');
               ssiSetUserEofe(AXI_STREAM_CONFIG_I_C, v.txMaster, '1');
               v.state := WAIT_SOF_S;
            end if;

         when TAIL_S =>
            ------------------------------------------------------------------
            -- TAIL_S
            ------------------------------------------------------------------    
            if v.txMaster.tValid = '0' then
               v.txMaster.tValid := '1';
               v.txMaster.tLast := '1';
               v.state := IDLE_S;
               v.txMaster.tData(63 downto 0) := x"00" & r.disableLane & x"00" & (fillOnFailEnV and r.tempDisableLane);
               v.txMaster.tKeep  :=  (v.txMaster.tKeep'left downto 64 => '0') & ( 63 downto 0 => '1');
               ssiSetUserEofe(AXI_STREAM_CONFIG_I_C, v.txMaster, '0');
            end if;

         when others =>
      end case;
      
      -- reset counters
      if r.rstCnt = '1' then
         v.frmCnt            := (others=>'0');
         v.frmSize           := (others=>'0');
         v.frmMax            := (others=>'0');
         v.frmMin            := (others=>'1');
         v.timeoutCntLane    := (others=>(others=>'0'));
         v.fillOnFailCntLane := (others=>(others=>'0'));
         v.fillOnFailCnt     := (others=>'0');
      end if;
      
      -- counters on the write side of the lane's buffer FIFO
      for i in 0 to (LANES_NO_G-1) loop
         
         -- count incoming data per lane
         -- store min and max
         if r.rstCnt = '1' then
            v.dataCntLaneReg(i)  := (others=>'0');
            v.dataCntLaneMin(i)  := (others=>'1');
            v.dataCntLaneMax(i)  := (others=>'0');
            v.dataCntLane(i)     := (others=>'0');
         elsif (r.stateD1 = DATA_S and r.state /= DATA_S) then -- update actual, min, max register when leaving DATA_S (on timeout or normally)
            v.dataCntLaneReg(i) := r.dataCntLane(i);
            if r.dataCntLaneMax(i) <= r.dataCntLane(i) then
               v.dataCntLaneMax(i) := r.dataCntLane(i);
            end if;
            if r.dataCntLaneMin(i) >= r.dataCntLane(i) then
               v.dataCntLaneMin(i) := r.dataCntLane(i);
            end if;
         elsif r.startRdSync(0) = '1' then                     -- startRdSync must be delayed few cycles as the same signal is taking the FSM out from DATA_S (condition above)
            v.dataCntLane(i) := (others=>'0');                 -- reset counter before next data cycle
         elsif rxValid(i) = '1' and rxSof(i) = '0' then
            v.dataCntLane(i) := r.dataCntLane(i) + 1;
         end if;
         
         -- count delay from SRO to SOF
         if r.rstCnt = '1' then
            v.dataDlyLaneReg(i)  := (others=>'0');
            v.dataDlyLane(i)     := (others=>'0');
         elsif startRdSync = '1' then
            v.dataDlyLane(i) := (others=>'0');
         -- Register data only on time of transition out of WAIT_SOF_S state
         elsif (r.stateD1 = WAIT_SOF_S and r.state /= WAIT_SOF_S) then
            v.dataDlyLaneReg(i) := r.dataDlyLane(i);
         -- Check if there is not data on that lane in this cycle (only significant in WAIT_SOF_S state)
         elsif dFifoSof(i) = '0' and r.dataDlyLane(i) /= x"ffff" then
            v.dataDlyLane(i) := r.dataDlyLane(i) + 1;
         end if;
         
         -- count writes to full FIFO (overflow)
         if r.rstCnt = '1' then
            v.dataOvfLane(i) := (others=>'0');
         elsif rxFull(i) = '1' and rxValid(i) = '1' and r.dataOvfLane(i) /= x"ffff" then
            v.dataOvfLane(i) := r.dataOvfLane(i) + 1;
         end if;
         
      end loop;

      if r.rstCnt = '1' then
         v.wsofStateCntrMin  := (others=>'1');
         v.wsofStateCntrMax  := (others=>'0');
         v.wsofStateCntr     := (others=>'0');
      else
         if (r.stateD1 = WAIT_SOF_S and r.state /= WAIT_SOF_S) then -- update actual, min, max register when leaving DATA_S (on timeout or normally)
            if r.wsofStateCntrMax <= r.wsofStateCntr then
               v.wsofStateCntrMax := r.wsofStateCntr;
            end if;
            if r.wsofStateCntrMin >= r.wsofStateCntr then
               v.wsofStateCntrMin := r.wsofStateCntr;
            end if;
            v.wsofStateCntr := (others=>'0');
         elsif (r.state = WAIT_SOF_S) then
            v.wsofStateCntr := r.wsofStateCntr + 1;
         end if;
      end if;

      if r.rstCnt = '1' then
         v.dataStateCntrMin  := (others=>'1');
         v.dataStateCntrMax  := (others=>'0');
         v.dataStateCntr     := (others=>'0');
      else
         if (r.stateD1 = DATA_S and r.state /= DATA_S) then -- update actual, min, max register when leaving DATA_S (on timeout or normally)
            if r.dataStateCntrMax <= r.dataStateCntr then
               v.dataStateCntrMax := r.dataStateCntr;
            end if;
            if r.dataStateCntrMin >= r.dataStateCntr then
               v.dataStateCntrMin := r.dataStateCntr;
            end if;
            v.dataStateCntr := (others=>'0');
         elsif (r.state = DATA_S) then
            v.dataStateCntr := r.dataStateCntr + 1;
         end if;
      end if;

      if r.rstCnt = '1' then
         v.hdrStateCntrMin  := (others=>'1');
         v.hdrStateCntrMax  := (others=>'0');
         v.hdrStateCntr     := (others=>'0');
      -- when previous state is HDR_S and current state is not. Meaning leaving HDR_S
      else
         if (r.stateD1 = HDR_S and r.state /= HDR_S) then
            if r.hdrStateCntrMax <= r.hdrStateCntr then
               v.hdrStateCntrMax := r.hdrStateCntr;
            end if;
            if r.hdrStateCntrMin >= r.hdrStateCntr then
               v.hdrStateCntrMin := r.hdrStateCntr;
            end if;
            v.hdrStateCntr := (others=>'0');
         -- When current state is not HDR_S
         elsif (r.state = HDR_S) then
            v.hdrStateCntr := r.hdrStateCntr + 1;
         end if;
      end if;

      if r.rstCnt = '1' then
         v.frameCyclesCtrMin  := (others=>'1');
         v.frameCyclesCntrMax  := (others=>'0');
         v.frameCyclesCntr     := (others=>'0');
      -- When previous state was not IDLE_S and current state is IDLE_S. Meaning entering IDLE_S state.
      else
         if (r.stateD1 /= IDLE_S and r.state = IDLE_S) or (r.state = TIMEOUT_S) then 
            if r.frameCyclesCntrMax <= r.frameCyclesCntr then
               v.frameCyclesCntrMax := r.frameCyclesCntr;
            end if;
            if r.frameCyclesCtrMin >= r.frameCyclesCntr then
               v.frameCyclesCtrMin := r.frameCyclesCntr;
            end if;
         end if;
      -- when previous state is IDLE_S and current state is not. Meaning leaving IDLE_S
         if (r.stateD1 = IDLE_S and r.state /= IDLE_S) or (r.state = TIMEOUT_S) then
            v.frameCyclesCntr := (others=>'0');
         elsif (r.state /= IDLE_S) then
            v.frameCyclesCntr := r.frameCyclesCntr + 1;
         end if;
      end if;

      if r.rstCnt = '1' then
         v.readyLowCyclesCtrMin  := (others=>'1');
         v.readyLowCyclesCtrMax  := (others=>'0');
         v.readyLowCyclesCtr     := (others=>'0');
      else
         if (r.stateD1 = DATA_S and r.state /= DATA_S) or (r.state = TIMEOUT_S) then 
            if r.readyLowCyclesCtrMax <= r.readyLowCyclesCtr then
               v.readyLowCyclesCtrMax := r.readyLowCyclesCtr;
            end if;
            if r.readyLowCyclesCtrMin >= r.readyLowCyclesCtr then
               v.readyLowCyclesCtrMin := r.readyLowCyclesCtr;
            end if;
            v.readyLowCyclesCtr := (others=>'0');
         end if;
      -- when previous state is DATA_S and current state is not. Meaning leaving DATA_S
         if (r.state = DATA_S and txSlave.tReady = '0') then
            v.readyLowCyclesCtr := r.readyLowCyclesCtr + 1;
         end if;
      end if;

      -- reset logic      
      if (deserRst = '1') then
         v := REG_INIT_C;
      end if;

      -- outputs
      
      rin <= v;

      axilWriteSlave <= r.axilWriteSlave;
      axilReadSlave  <= r.axilReadSlave;
      dFifoRd        <= v.dFifoRd;

   end process comb;

   seq : process (deserClk) is
   begin
      if (rising_edge(deserClk)) then
         r <= rin after TPD_G;
      end if;
   end process seq;
   
   
   ----------------------------------------------------------------------------
   -- axi stream fifo
   -- deserializer clock to axis clock crossing
   -- gearbox 4/3 by double stream resizing 
   -- must be able to store whole frame if AXIS is muxed
   ----------------------------------------------------------------------------
   DeserAxisDualClockFifo_U: entity surf.AxiStreamFifoV2
   generic map(
      GEN_SYNC_FIFO_G      => false,
      FIFO_ADDR_WIDTH_G    => 13,
      CASCADE_SIZE_G       => 1,
      INT_WIDTH_SELECT_G   => "WIDE",
      SLAVE_AXI_CONFIG_G   => AXI_STREAM_CONFIG_I_C,
      MASTER_AXI_CONFIG_G  => AXI_STREAM_CONFIG_W_C
   )
   port map(
      sAxisClk    => deserClk,
      sAxisRst    => deserRst,
      sAxisMaster => r.txMaster,
      sAxisSlave  => txSlave,
      mAxisClk    => axisClk,
      mAxisRst    => axisRst,
      mAxisMaster => sAxisMasterWide,
      mAxisSlave  => sAxisSlaveWide,
      fifoFull    => DeserAxisDualClockFifoFull,
      fifoWrCnt   => DeserAxisDualClockFifoWrCnt
   );
   
   
   AxisResize48to16_U: entity surf.AxiStreamResize
   generic map(
      -- General Configurations
      TPD_G      => TPD_G,
      READY_EN_G => true,
      -- AXI Stream Port Configurations
      SLAVE_AXI_CONFIG_G  => AXI_STREAM_CONFIG_W_C,
      MASTER_AXI_CONFIG_G => AXI_STREAM_CONFIG_O_C
      )
   port map(
      -- Clock and reset
      axisClk     => axisClk,
      axisRst     => axisRst,
      -- Slave Port
      sAxisMaster => sAxisMasterWide,
      sAxisSlave  => sAxisSlaveWide,
      -- Master Port
      mAxisMaster => mAxisMaster,
      mAxisSlave  => mAxisSlave
   );

end RTL;
