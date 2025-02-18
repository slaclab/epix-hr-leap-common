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

class DigitalAsicStreamAxi(pr.Device):
   def __init__(self, numberLanes=1, **kwargs):
      super().__init__(description='Asic data packet registers', **kwargs)
      
      #Setup registers & variables
      states = {0:"IDLE_S",
                1:"WAIT_SOF_S",
                2:"HDR_S",
                3:"DATA_S",
                4:"TIMEOUT_S",
                5:"TAIL_S" }

      yesNo = { 0 : "Disabled",
                1 : "Enabled" }

      self.add(pr.RemoteVariable(name='FrameCount',        description='Complete frame count',                                  offset=0x00000000, bitSize=32,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RO', pollInterval = 1))
      self.add(pr.RemoteVariable(name='FrameSize',         description='Complete frame size',                                   offset=0x00000004, bitSize=16,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RO', pollInterval = 1))
      self.add(pr.RemoteVariable(name='FrameMaxSize',      description='Max frame size',                                offset=0x00000008, bitSize=16,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RO', pollInterval = 1))
      self.add(pr.RemoteVariable(name='FrameMinSize',      description='Min frame size',                                offset=0x0000000C, bitSize=16,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RO', pollInterval = 1))
      #self.add(pr.RemoteVariable(name='ResetCounters',     description='ResetCounters',                               offset=0x00000024, bitSize=1,   bitOffset=0, base=pr.Bool, mode='WO'))
      self.add(pr.RemoteVariable(name='asicDataReq',       description='Number of samples requested per ADC stream.', offset=0x00000028, bitSize=16,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='DisableLane',       description='Disable selected lanes.',                     offset=0x0000002C, bitSize=numberLanes,  bitOffset=0, base=pr.UInt, mode='RW'))
      self.add(pr.RemoteVariable(name='EnumerateDisLane',  description='Insert lane number into disabled lane.',      offset=0x00000030, bitSize=numberLanes,  bitOffset=0, base=pr.UInt, mode='RW'))

      self.add(pr.RemoteVariable(name='FillOnFailEn',               description='Dynamically handles failing lanes, inserts 0s',            offset=0x00000038, bitSize=1,   bitOffset=0, base=pr.UInt, disp = '{}', mode='RW', enum=yesNo))
      self.add(pr.RemoteVariable(name='FillOnFailPersistantDisable',description='Failing lanes are perminantly disabled',                   offset=0x00000014, bitSize=1,   bitOffset=0, base=pr.UInt, disp = '{}', mode='RW', enum=yesNo))
      self.add(pr.RemoteVariable(name='SroToSofTimeout',            description='Timeout waiting for Sof After Sro',                        offset=0x0000003C, bitSize=32,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='DataTimeout',                description='Timeout waiting for all lanes to send a single pixel',     offset=0x00000040, bitSize=32,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='FillOnFailCnt',              description='No. of images where fill-on-fail was activated',           offset=0x00000044, bitSize=32,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RO', pollInterval = 1))

      self.add(pr.RemoteVariable(name='FillOnFailLastMask',     description='Last temporary mask used to disable lanes', offset=0x00000048, bitSize=24,  bitOffset=0, base=pr.UInt, mode='RO', pollInterval = 1))
      self.add(pr.RemoteVariable(name='State',                  description='IDLE_S, WAIT_SOF_S, HDR_S, DATA_S, TIMEOUT_S', offset=0x0000004C, bitSize=8,  bitOffset=0, base=pr.UInt, mode='RO', enum=states, pollInterval = 1))

      self.add(pr.RemoteVariable(name='WsofStateCntrMin', description='', offset=0x0000005C, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='WsofStateCntrMax', description='', offset=0x00000060, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='WsofStateCntr', description='', offset=0x00000064, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='DataStateCntrMin', description='', offset=0x00000068, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='DataStateCntrMax', description='', offset=0x0000006C, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='DataStateCntr', description='', offset=0x00000070, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='HdrStateCntrMin', description='', offset=0x00000074, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='HdrStateCntrMax', description='', offset=0x00000078, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='HdrStateCntr', description='', offset=0x0000007C, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='FrameCyclesCtrMin', description='', offset=0x00000080, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='FrameCyclesCntrMax', description='', offset=0x00000084, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='FrameCyclesCntr', description='', offset=0x00000088, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='ReadyLowCyclesCtrMin', description='', offset=0x0000008C, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='ReadyLowCyclesCtrMax', description='', offset=0x00000090, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='ReadyLowCyclesCtr', description='', offset=0x00000094, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))

      self.add(pr.RemoteVariable(name='TrigToSroCntrMin', description='', offset=0x00000098, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='TrigToSroCntrMax', description='', offset=0x0000009C, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='TrigToSroCntr', description='', offset=0x00000010, bitSize=16,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))

      self.add(pr.RemoteVariable(name='fillOnFailDataMax', description='', offset=0x00000018, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))

      self.add(pr.RemoteVariable(name='dFifoValid', description='', offset=0x0000012C, bitSize=24,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='dFifoSof', description='', offset=0x00000130, bitSize=24,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))

      for i in range(0, numberLanes):
         self.add(pr.RemoteVariable(
            name         = f'rdDataCount[{i}]',
            description  = '',
            offset       = 0x134 + i*4,
            bitSize      = 9,
            mode         = 'RO',
            pollInterval = 1,
            disp         = '{}', 
         ))


      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
            name         = f'SroToSofCntrMax[{i-1}]',
            description  = 'counts the number of cycles between the arrival of SRO and the SOF',
            offset       = 0x020 + i*0x100,
            bitSize      = 16,
            mode         = 'RO',
            pollInterval = 1,
            disp         = '{}', 
         ))

      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'TimeoutCntLane[{i-1}]',
         description  = 'Count of times SM waiting for data till next trigger',
         offset       = 0x000 + i*0x100,
         bitSize      = 16,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}', 
      ))
      
      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'DataCntLaneAct[{i-1}]',
         description  = 'Last data cycle count. Should be the same as asicDataReq',
         offset       = 0x004 + i*0x100,
         bitSize      = 16,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}', 
      ))
      
      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'DataCntLaneReg[{i-1}]',
         description  = 'Last data cycle count when leaving DATA_S state. Should be the same as asicDataReq',
         offset       = 0x008 + i*0x100,
         bitSize      = 16,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}', 
      ))
      
      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'DataCntLaneMin[{i-1}]',
         description  = 'Minimum data cycles counted when leaving DATA_S state. Should be the same as asicDataReq',
         offset       = 0x00C + i*0x100,
         bitSize      = 16,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}', 
      ))
      
      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'DataCntLaneMax[{i-1}]',
         description  = 'MAx. data cycles counted when leaving DATA_S state. Should be the same as asicDataReq',
         offset       = 0x010 + i*0x100,
         bitSize      = 16,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}', 
      ))
      
      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'DataDlyLaneReg[{i-1}]',
         description  = 'Number of cycles until SM transitions out of WAIT_SOF_S state (delay)',
         offset       = 0x0014 + i*0x100,
         bitSize      = 16,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}',  
      ))
      
      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'DataOvfLane[{i-1}]',
         description  = 'counts the times overflow happens (Fifo Full + new data available)',
         offset       = 0x0018 + i*0x100,
         bitSize      = 16,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}', 
      ))

      for i in range(1, numberLanes+1):
         self.add(pr.RemoteVariable(
         name         = f'FillOnFailCntLane[{i-1}]',
         description  = 'counts the times a lane is temporarily disabled after a failure was detected',
         offset       = 0x001C + i*0x100,
         bitSize      = 32,
         mode         = 'RO',
         pollInterval = 1,
         disp         = '{}', 
      ))

      self.add(pr.RemoteCommand(name='CountReset', description='Resets counters', 
                             offset=0x00000024, bitSize=1, bitOffset=0, function=pr.Command.touchOne))
      
      @self.command()
      def FixTimeouts():
         maxSroToSofDelay = 0
         for i in range(numberLanes) :
            current = self.SroToSofCntrMax[i].get()
            if (maxSroToSofDelay < current) :
               maxSroToSofDelay = current
         self.SroToSofTimeout.set(maxSroToSofDelay + 10)


         self.DataTimeout.set(self.fillOnFailDataMax.get() + 10)
