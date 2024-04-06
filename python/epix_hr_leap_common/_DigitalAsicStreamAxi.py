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
                4:"TIMEOUT_S"}

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

      self.add(pr.RemoteVariable(name='fillOnFailEn',            description='Dynamically handles failing lanes, inserts 0s',  offset=0x00000038, bitSize=1,   bitOffset=0, base=pr.UInt, disp = '{}', mode='RW', enum=yesNo))
      self.add(pr.RemoteVariable(name='fillOnFailTimeoutOffset', description='Timeout offset value for fill-on-fail without data arrival delay.',                 offset=0x0000003C, bitSize=32,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='fillOnFailTimeout',       description='Timeout value for fill-on-fail',                 offset=0x00000040, bitSize=32,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RW'))
      self.add(pr.RemoteVariable(name='fillOnFailCnt',           description='No. of images where fill-on-fail was activated', offset=0x00000044, bitSize=32,  bitOffset=0, base=pr.UInt, disp = '{}', mode='RO'))

      self.add(pr.RemoteVariable(name='fillOnFailLastMask',     description='Last temporary mask used to disable lanes', offset=0x00000048, bitSize=24,  bitOffset=0, base=pr.UInt, mode='RO', pollInterval = 1))
      self.add(pr.RemoteVariable(name='State',                  description='IDLE_S, WAIT_SOF_S, HDR_S, DATA_S, TIMEOUT_S', offset=0x0000004C, bitSize=8,  bitOffset=0, base=pr.UInt, mode='RO', enum=states, pollInterval = 1))

      self.add(pr.RemoteVariable(name='wsofStateCntrMin', description='', offset=0x0000005C, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='wsofStateCntrMax', description='', offset=0x00000060, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='wsofStateCntr', description='', offset=0x00000064, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='dataStateCntrMin', description='', offset=0x00000068, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='dataStateCntrMax', description='', offset=0x0000006C, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='dataStateCntr', description='', offset=0x00000070, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='hdrStateCntrMin', description='', offset=0x00000074, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='hdrStateCntrMax', description='', offset=0x00000078, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='hdrStateCntr', description='', offset=0x0000007C, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='frameCyclesCtrMin', description='', offset=0x00000080, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='frameCyclesCntrMax', description='', offset=0x00000084, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))
      self.add(pr.RemoteVariable(name='frameCyclesCntr', description='', offset=0x00000088, bitSize=32,  bitOffset=0, base=pr.UInt, mode='RO', disp = '{}', pollInterval = 1))


      self.addRemoteVariables(
         name         = 'TimeoutCntLane',
         description  = 'Count of times SM waiting for data till next trigger',
         offset       = 0x100,
         bitSize      = 16,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}', 
      )
      
      self.addRemoteVariables(
         name         = 'DataCntLaneAct',
         description  = 'Last data cycle count. Should be the same as asicDataReq',
         offset       = 0x200,
         bitSize      = 16,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}', 
      )
      
      self.addRemoteVariables(
         name         = 'DataCntLaneReg',
         description  = 'Last data cycle count when leaving DATA_S state. Should be the same as asicDataReq',
         offset       = 0x300,
         bitSize      = 16,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}', 
      )
      
      self.addRemoteVariables(
         name         = 'DataCntLaneMin',
         description  = 'Minimum data cycles counted when leaving DATA_S state. Should be the same as asicDataReq',
         offset       = 0x400,
         bitSize      = 16,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}', 
      )
      
      self.addRemoteVariables(
         name         = 'DataCntLaneMax',
         description  = 'MAx. data cycles counted when leaving DATA_S state. Should be the same as asicDataReq',
         offset       = 0x500,
         bitSize      = 16,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}', 
      )
      
      self.addRemoteVariables(
         name         = 'DataDlyLaneReg',
         description  = 'Number of cycles until SM transitions out of WAIT_SOF_S state (delay)',
         offset       = 0x600,
         bitSize      = 16,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}',  
      )
      
      self.addRemoteVariables(
         name         = 'DataOvfLane',
         description  = 'counts the times overflow happens (Fifo Full + new data available)',
         offset       = 0x700,
         bitSize      = 16,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}', 
      )

      self.addRemoteVariables(
         name         = 'fillOnFailCntLane',
         description  = 'counts the times a lane is temporarily disabled after a failure was detected',
         offset       = 0x800,
         bitSize      = 32,
         mode         = 'RO',
         number       = numberLanes,
         stride       = 4,
         pollInterval = 1,
         disp         = '{}', 
      )

      self.add(pr.RemoteCommand(name='CountReset', description='Resets counters', 
                             offset=0x00000024, bitSize=1, bitOffset=0, function=pr.Command.touchOne))
      
