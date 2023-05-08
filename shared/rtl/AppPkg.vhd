-------------------------------------------------------------------------------
-- File       : AppPkg.vhd
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Application's Package File
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
use surf.AxiLitePkg.all;

package AppPkg is

   constant XIL_DEVICE_C               : string  := "ULTRASCALE_PLUS";

   type AppConfigType is record
      AppVersion           : slv(31 downto 0);
      powerEnable          : slv(3 downto 0);
      asicMask             : slv(31 downto 0);
      acqCnt               : slv(31 downto 0);
      requestStartupCal    : sl;
      startupAck           : sl;
      startupFail          : sl;
      epixhrDbgSel1        : slv(5 downto 0);
      epixhrDbgSel2        : slv(5 downto 0);
      epixhrDbgSel3        : slv(3 downto 0);
   end record;


   constant APP_CONFIG_INIT_C : AppConfigType := (
      AppVersion           => (others => '0'),
      powerEnable          => (others => '0'),
      asicMask             => (others => '0'),
      acqCnt               => (others => '0'),
      requestStartupCal    => '1',
      startupAck           => '0',
      startupFail          => '0',
      epixhrDbgSel1        => (others => '0'),
      epixhrDbgSel2        => (others => '0'),
      epixhrDbgSel3        => (others => '0')
   );
   
  

end package AppPkg;

package body AppPkg is

end package body AppPkg;
