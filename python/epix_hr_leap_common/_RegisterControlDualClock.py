#-----------------------------------------------------------------------------
# This file is part of the 'epix-hr-leap-common'. It is subject to
# the license terms in the LICENSE.txt file found in the top-level directory
# of this distribution and at:
#    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
# No part of the 'epix-hr-leap-common', including this file, may be
# copied, modified, propagated, or distributed except according to the terms
# contained in the LICENSE.txt file.
#-----------------------------------------------------------------------------

import pyrogue as pr

class RegisterControlDualClock(pr.Device):
   def __init__(self, debugChEnum=[], snEnum={}, **kwargs):
      """Create the configuration device for HR Gen1 core FPGA registers"""
      """Version 1 is for ASIC 0.1 and test ADC ASIC"""
      """Version 2 is for ASIC 0.2 whee ClkSyncEn has been added"""

      version = 2
      
      super().__init__(description='HR Gen 1 core FPGA configuration registers', **kwargs)
      
      # Creation. memBase is either the register bus server (srp, rce mapped memory, etc) or the device which
      # contains this object. In most cases the parent and memBase are the same but they can be 
      # different in more complex bus structures. They will also be different for the top most node.
      # The setMemBase call can be used to update the memBase for this Device. All sub-devices and local
      # blocks will be updated.
      
      if snEnum == {} :
         snEnum = {0: 'DigIDLow', 1: 'DigIDHigh', 2: 'AnalogIDLow', 3: 'AnalogIDHigh',
                   4: 'CarrierIDLow', 5: 'CarrierIDHigh'}
         
      #Setup registers & variables
      
      self.add(pr.RemoteVariable(name='Version',         description='Version',           offset=0x00000000, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{:#x}',  verify = False, mode='RO'))
      
      for snId in snEnum.keys():
        self.add(pr.RemoteVariable(name=snEnum[snId],         description=snEnum[snId],           offset=((snId*4)+4), bitSize=32, bitOffset=0, base=pr.UInt, disp = '{:#x}', mode='RO'))
        
      self.add(pr.RemoteVariable(name='IDreset',         description='Reset DS2411Core module',         offset=0x000000FC, bitOffset=0, base=pr.UInt, mode='RW'))
      
      self.add(pr.RemoteVariable(name='GlblRstPolarityN',description='GlblRstPolarityN (active low)',   offset=0x00000100, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='ClkSyncEn',       description='Enables clock to be available inside ASIC.',   offset=0x00000100, bitSize=1,  bitOffset=1, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='RoLogicRstN',     description='Enables digital rodout clock. (Active low)',   offset=0x00000100, bitSize=1,  bitOffset=2, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='SyncPolarity',    description='SyncPolarity',      offset=0x00000104, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='asicRefClockFreq',description='reference clock requency to the ASIC',     offset=0x00000268, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RO'))            
      self.add(pr.RemoteVariable(name='SyncDelay',       description='SyncDelay (relative - after Acq goes inactive)',         offset=0x00000108, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='SyncDelay_us',    description='SyncDelay in us (asicRefClockFreq domain)',   mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverter, linkedSet=self.reverseTimeConverter, dependencies = [self.SyncDelay, self.asicRefClockFreq]))
      self.add(pr.RemoteVariable(name='SyncWidth',       description='SyncWidth',         offset=0x0000010C, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='SR0Polarity',     description='SR0Polarity',       offset=0x00000110, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='SR0Delay1',       description='SR0Delay1 (relative - after Acq goes inactive)',         offset=0x00000114, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='SR0Delay_us',     description='SR0Delay in us (asicRefClockFreq domain)',    mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverter, linkedSet=self.reverseTimeConverter, dependencies = [self.SR0Delay1, self.asicRefClockFreq]))
      self.add(pr.RemoteVariable(name='SR0Width1',       description='SR0Width1',         offset=0x00000118, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='SR0Width1_us',    description='SR0 width in us (asicRefClockFreq domain)',   mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverter, linkedSet=self.reverseTimeConverter, dependencies = [self.SR0Width1, self.asicRefClockFreq]))
      self.add(pr.RemoteVariable(name='ePixAdcSHPeriod', description='Period',            offset=0x0000011C, bitSize=16, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='ePixAdcSHOffset', description='Offset',            offset=0x00000120, bitSize=16, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))

      
      self.add(pr.RemoteVariable(name='AcqPolarity',     description='AcqPolarity',       offset=0x00000200, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='AcqDelay1',       description='AcqDelay1 (Absolute - from time 0)',          offset=0x00000204, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='AcqDelay1_us',    description='AcqDelay1 in us (AXI clk domain)',    mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverterAppClock, linkedSet=self.reverseTimeConverterAppClock, dependencies = [self.AcqDelay1]))
      self.add(pr.RemoteVariable(name='AcqWidth1',       description='AcqWidth',          offset=0x00000208, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='AcqWidth1_us',    description='AcqWidth1 in us (AXI clk domain)',    mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverterAppClock, linkedSet=self.reverseTimeConverterAppClock, dependencies = [self.AcqWidth1]))
      self.add(pr.RemoteVariable(name='AcqDelay2',       description='AcqDelay2 (Absolute - from time 0)',          offset=0x0000020C, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='AcqDelay2_us',    description='AcqDelay2 in us (AXI clk domain)',    mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverterAppClock, linkedSet=self.reverseTimeConverterAppClock, dependencies = [self.AcqDelay2]))
      self.add(pr.RemoteVariable(name='AcqWidth2',       description='AcqWidth2',          offset=0x00000210, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='AcqWidth2_us',    description='AcqWidth2 in us (AXI clk domain)',    mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverterAppClock, linkedSet=self.reverseTimeConverterAppClock, dependencies = [self.AcqWidth2]))
      self.add(pr.RemoteVariable(name='R0Polarity',      description='Polarity',          offset=0x00000214, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='R0Delay',         description='R0Delay (Absolute - from time 0)',             offset=0x00000218, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='R0Delay_us',      description='R0Delay in us (AXI clk domain)',    mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverterAppClock, linkedSet=self.reverseTimeConverterAppClock, dependencies = [self.R0Delay]))
      self.add(pr.RemoteVariable(name='R0Width',         description='R0Width',             offset=0x0000021C, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.LinkVariable(  name='R0Width_us',      description='R0Width in us (AXI clk domain)',    mode='RW', units='uS', disp='{:1.3f}', linkedGet=self.timeConverterAppClock, linkedSet=self.reverseTimeConverterAppClock, dependencies = [self.R0Width]))
      self.add(pr.RemoteVariable(name='PPbePolarity',    description='PPbePolarity',      offset=0x00000220, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='PPbeDelay',       description='PPbeDelay',         offset=0x00000224, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='PPbeWidth',       description='PPbeWidth',         offset=0x00000228, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='PpmatPolarity',   description='PpmatPolarity',     offset=0x0000022C, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='PpmatDelay',      description='PpmatDelay',        offset=0x00000230, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='PpmatWidth',      description='PpmatWidth',        offset=0x00000234, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='SaciSyncPolarity',description='SaciSyncPolarity',  offset=0x00000238, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add(pr.RemoteVariable(name='SaciSyncDelay',   description='SaciSyncDelay',     offset=0x0000023C, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='SaciSyncWidth',   description='SaciSyncWidth',     offset=0x00000240, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))

      self.add(pr.RemoteVariable(name='AcqCnt',          description='AcqCnt',            offset=0x00000244, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RO', pollInterval = 1))
      self.add(pr.RemoteVariable(name='SaciPrepRdoutCnt',description='SaciPrepRdoutCnt',  offset=0x00000248, bitSize=32, bitOffset=0, base=pr.UInt, disp = '{}', mode='RO'))
      self.add(pr.RemoteVariable(name='ResetCounters',   description='ResetCounters',     offset=0x0000024C, bitSize=1,  bitOffset=0, base=pr.Bool, mode='RW'))
      self.add((
         pr.RemoteVariable(name='AsicPwrEnable',         description='AsicPower',         offset=0x00000250, bitSize=1, bitOffset=0,  base=pr.Bool, mode='RW'),
         pr.RemoteVariable(name='AsicPwrManual',         description='AsicPower',         offset=0x00000250, bitSize=1, bitOffset=16, base=pr.Bool, mode='RW'),
         pr.RemoteVariable(name='AsicPwrManualDig',      description='AsicPower',         offset=0x00000250, bitSize=1, bitOffset=20, base=pr.Bool, mode='RW'),
         pr.RemoteVariable(name='AsicPwrManualAna',      description='AsicPower',         offset=0x00000250, bitSize=1, bitOffset=21, base=pr.Bool, mode='RW'),
         pr.RemoteVariable(name='AsicPwrManualIo',       description='AsicPower',         offset=0x00000250, bitSize=1, bitOffset=22, base=pr.Bool, mode='RW'),
         pr.RemoteVariable(name='AsicPwrManualFpga',     description='AsicPower',         offset=0x00000250, bitSize=1, bitOffset=23, base=pr.Bool, mode='RW')))
      self.add(pr.RemoteVariable(name='AsicMask',        description='AsicMask',          offset=0x00000254, bitSize=32,bitOffset=0,  base=pr.UInt, disp = '{:#x}',  mode='RO'))
      self.add(pr.RemoteVariable(name='DebugSel0',       description='Debug Sel 0',       offset=0x00000258, bitSize=6, bitOffset=0,  mode='RW', enum=debugChEnum[0]))
      self.add(pr.RemoteVariable(name='DebugSel1',       description='Debug Sel 1',       offset=0x0000025C, bitSize=6, bitOffset=0,  mode='RW', enum=debugChEnum[1]))
      self.add((
         pr.RemoteVariable(name='StartupReq',            description='AdcStartup',        offset=0x00000264, bitSize=1, bitOffset=0, base=pr.Bool, mode='RW'),
         pr.RemoteVariable(name='getSerialNumbers',      description='Get serial number', offset=0x00000264, bitSize=1, bitOffset=1, base=pr.Bool, mode='RW')))
      self.add((
         pr.RemoteVariable(name='timingV1LinkUp',              description='Timing Status',     offset=0x0000026C, bitSize=1, bitOffset=3, base=pr.Bool, mode='RO'),
         pr.RemoteVariable(name='timingV2LinkUp',              description='Timing Status',     offset=0x0000026C, bitSize=1, bitOffset=4, base=pr.Bool, mode='RO'),
         pr.RemoteVariable(name='digOutSync[0]',              description='Timing Status',     offset=0x0000026C, bitSize=1, bitOffset=5, base=pr.Bool, mode='RO'),
         pr.RemoteVariable(name='digOutSync[1]',              description='Timing Status',     offset=0x0000026C, bitSize=1, bitOffset=6, base=pr.Bool, mode='RO'))
      )
      self.add(pr.RemoteVariable(name='AsicRdClk', description='Asic ReadOut clock source', offset=0x00000270, bitSize=1, bitOffset=0, base=pr.Bool, mode='RW'))

     
     
      #####################################
      # Create commands
      #####################################
      
      # A command has an associated function. The function can be a series of
      # python commands in a string. Function calls are executed in the command scope
      # the passed arg is available as 'arg'. Use 'dev' to get to device scope.
      # A command can also be a call to a local function with local scope.
      # The command object and the arg are passed

   @staticmethod   
   def timeConverter(var, read):
      raw = var.dependencies[0].get(read=read)
      freq = var.dependencies[1].get(read=read)
      if freq == 0:
          freq = 1
      return ((1/freq) * raw * 1e+6)

   @staticmethod   
   def reverseTimeConverter(var, value, write):
      freq = var.dependencies[1].value()
      var.dependencies[0].set(value=int(value/(1e+6/freq)), write=write)
      return value
   

   @staticmethod   
   def timeConverterAppClock(var, read):
      """Converts a number of cycles in micro seconds."""
      raw = var.dependencies[0].get(read=read)
      #freq 156.25MHz
      return (raw / 156.25)

   @staticmethod   
   def reverseTimeConverterAppClock(var, value, write):
      """Converts micro seconds to cycles."""
      uS = value

      #freq 156.25MHz
      var.dependencies[0].set(value = int(uS * 156.25), write=write)
      return value

   @staticmethod   
   def frequencyConverter(self):
      def func(dev, var):         
         return '{:.3f} kHz'.format(1/(self.clkPeriod * self._count(var.dependencies)) * 1e-3)
      return func

