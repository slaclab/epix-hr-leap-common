-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: ASIC Deserializer Top-Level
-------------------------------------------------------------------------------
-- This file is part of 'epix-hr-leap-common' submodule.
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

library work;
use work.AppPkg.all;

library unisim;
use unisim.vcomponents.all;

entity AppDeser is
   generic (
      TPD_G            : time    := 1 ns;
      SIMULATION_G     : boolean := false;
      AXIL_BASE_ADDR_G : slv(31 downto 0);
      NUM_OF_LANES_G  : integer := 5);
   port (
      -- Clocks and Resets
      sspClk4x        : in  sl;
      -- ASIC Ports
      asicDataP       : in    Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      asicDataM       : in    Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      -- AXI-Lite Interface (axilClk domain)
      axilClk         : in  sl;
      axilRst         : in  sl;

      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;

      mAxilWriteMasters : in  AxiLiteWriteMasterArray(NUM_OF_LANES_G-1 downto 0) := (others => AXI_LITE_WRITE_MASTER_INIT_C);
      mAxilWriteSlaves  : out AxiLiteWriteSlaveArray(NUM_OF_LANES_G-1 downto 0);
      mAxilReadMasters  : in  AxiLiteReadMasterArray(NUM_OF_LANES_G-1 downto 0) := (others => AXI_LITE_READ_MASTER_INIT_C);
      mAxilReadSlaves   : out AxiLiteReadSlaveArray(NUM_OF_LANES_G-1 downto 0);

      -- SSP Interfaces (sspClk domain)
      sspClk          : in  sl;
      sspRst          : in  sl;
      -- Ssp data outputs
      sspLinkUp       : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspValid        : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspData         : out Slv16Array((NUM_OF_LANES_G * 24)-1 downto 0);
      sspSof          : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspEof          : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspEofe         : out Slv24Array(NUM_OF_LANES_G - 1 downto 0));
end AppDeser;

architecture mapping of AppDeser is
  
   constant NUM_AXIL_MASTERS_C : positive := NUM_OF_LANES_G;

   constant XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXIL_MASTERS_C-1 downto 0) := genAxiLiteConfig(NUM_AXIL_MASTERS_C, AXIL_BASE_ADDR_G, 16, 12);

   constant U_2S1MXBARDESER_CONFIG_C  : AxiLiteCrossbarMasterConfigArray(0 downto 0) := (
      0                => (   baseAddr     => AXIL_BASE_ADDR_G,
                              addrBits     => 24,
                              connectivity => x"FFFF")
                              );

   signal axilWriteMasters : AxiLiteWriteMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0) := (others => AXI_LITE_WRITE_SLAVE_EMPTY_SLVERR_C);
   signal axilReadMasters  : AxiLiteReadMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0)  := (others => AXI_LITE_READ_SLAVE_EMPTY_SLVERR_C);

   signal axilWriteMastersMerged : AxiLiteWriteMasterArray(NUM_AXIL_MASTERS_C-1 downto 0); 
   signal axilWriteSlavesMerged  : AxiLiteWriteSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0) := (others => AXI_LITE_WRITE_SLAVE_EMPTY_SLVERR_C); 
   signal axilReadMastersMerged  : AxiLiteReadMasterArray(NUM_AXIL_MASTERS_C-1 downto 0); 
   signal axilReadSlavesMerged   : AxiLiteReadSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0) := (others => AXI_LITE_READ_SLAVE_EMPTY_SLVERR_C);

   signal sspReset         : slv(NUM_OF_LANES_G-1 downto 0);
   
begin

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
         sAxiWriteMasters(0) => axilWriteMaster,
         sAxiWriteSlaves(0)  => axilWriteSlave,
         sAxiReadMasters(0)  => axilReadMaster,
         sAxiReadSlaves(0)   => axilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves,
         axiClk              => axilClk,
         axiClkRst           => axilRst);



   GEN_VEC : for i in NUM_OF_LANES_G - 1 downto 0 generate

      U_2S1MXBARAPPDESER : entity surf.AxiLiteCrossbar
         generic map (
            TPD_G              => TPD_G,
            NUM_SLAVE_SLOTS_G  => 2,
            NUM_MASTER_SLOTS_G => 1,
            MASTERS_CONFIG_G   => U_2S1MXBARDESER_CONFIG_C)
         port map (
            axiClk               => axilClk,
            axiClkRst            => axilRst,

            sAxiWriteMasters(0)  => mAxilWriteMasters(i),
            sAxiWriteMasters(1)  => axilWriteMasters(i),
            sAxiWriteSlaves(0)   => mAxilWriteSlaves(i),
            sAxiWriteSlaves(1)   => axilWriteSlaves(i),
            sAxiReadMasters(0)   => mAxilReadMasters(i),
            sAxiReadMasters(1)   => axilReadMasters(i),
            sAxiReadSlaves(0)    => mAxilReadSlaves(i),
            sAxiReadSlaves(1)    => axilReadSlaves(i),
         
            mAxiWriteMasters(0) => axilWriteMastersMerged(i),
            mAxiWriteSlaves(0)  => axilWriteSlavesMerged(i),
            mAxiReadMasters(0)  => axilReadMastersMerged(i),
            mAxiReadSlaves(0)   => axilReadSlavesMerged(i)  
         );


      U_Deser_Group : entity work.AppDeserGroup
         generic map (
            TPD_G          => TPD_G,
            SIMULATION_G   => SIMULATION_G)
         port map (

            -- Asic Ports
            asicDataP        => asicDataP(i),
            asicDataM        => asicDataM(i),

            -- AXI-Lite Interface (axilClk domain)
            axilClk          => axilClk,
            axilRst          => axilRst,
            axilReadMaster   => axilReadMastersMerged(i),
            axilReadSlave    => axilReadSlavesMerged(i),
            axilWriteMaster  => axilWriteMastersMerged(i),
            axilWriteSlave   => axilWriteSlavesMerged(i),


            sspClk4x         => sspClk4x,
            sspClk           => sspClk,
            sspRst           => sspReset(i),

            sspLinkUp         => sspLinkUp(i),
            sspValid          => sspValid(i),
            sspData           => sspData(24*i+23 downto 24*i),
            sspSof            => sspSof(i),
            sspEof            => sspEof(i),
            sspEofe           => sspEofe(i)
         );

      U_reset : entity surf.RstPipeline
         generic map (
            TPD_G => TPD_G)
         port map (
            clk    => sspClk,
            rstIn  => sspRst,
            rstOut => sspReset(i));

   end generate GEN_VEC;

end mapping;
