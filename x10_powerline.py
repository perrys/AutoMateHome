#!/usr/bin/python

# This file is part of AutoMateHome.
#
# AutoMateHome is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# AutoMateHome is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with AutoMateHome.  If not, see <http://www.gnu.org/licenses/>.

import utils

import datetime
import unittest

"""X10 powerline endode/decode functions. The data used to write this
   module was gathered mainly from the protocol.txt file distributed
   with the heyu package, which itself was derived from an X10 webpage
   published in 1996, with several later corrections and
   contributions.

   The heyu package was written for the CM11/CM12 (serial port)
   controller units, but much of the same protocol is still used by
   the CM15"""


# binary values for house (A-P) and unit (1-16) codes in order:
X10_CODES_ZERO_OFFSET = (6,14,2,10,1,9,5,13,7,15,3,11,0,8,4,12)

# translator for unit codes to values:
UNITCODE_TO_VALUE_MAP = dict([(i+1, c) for (i,c) in enumerate(X10_CODES_ZERO_OFFSET)])

# translator for house codes to values:
HOUSECODE_TO_VALUE_MAP = dict([(chr(ord('A') + i), c) for (i,c) in enumerate(X10_CODES_ZERO_OFFSET)])

# translator for values to unit codes:
VALUE_TO_UNITCODE_MAP = dict([(v,k) for (k,v) in UNITCODE_TO_VALUE_MAP.iteritems()])

# translator for values to unit codes:
VALUE_TO_HOUSECODE_MAP = dict([(v,k) for (k,v) in HOUSECODE_TO_VALUE_MAP.iteritems()])

# Human-readable names of functions 0-15
FUNCTIONS_ZERO_OFFSET = Functions = ("All Units Off",
                                    "All Lights On",
                                     "On",
                                     "Off",
                                     "Dim",
                                     "Bright",
                                     "All Lights Off",
                                     "Extended Code",
                                     "Hail Request",
                                     "Hail Acknowledge",
                                     "Pre-set Dim (1)",
                                     "Pre-set Dim (2)",
                                     "Extended Data Transfer",
                                     "Status On",
                                     "Status Off",
                                     "Status Request")

# translator for gapless, lowercase function names to codes:
UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP = dict([(k.replace(" ","_").lower(), i) for (i,k) in enumerate(FUNCTIONS_ZERO_OFFSET)])

# translator for function codes to human-readable names:
FUNCTIONCODE_TO_STRING_MAP = dict([(i, k) for (i,k) in enumerate(FUNCTIONS_ZERO_OFFSET)])

def header(dims, isFunction, isExtended):
  """Return an X10-encoded header byte given the number of dims and the kind of function"""

# From the protocol.txt file:
# 
# The Header:Code combination is configured thus:
# 
# 	Bit:	7   6   5   4   3   2   1   0
# 	Header:	< Dim amount    >   1  F/A E/S
# 
# Where:	
# 
# Dim amount (dims) is a value between 0 and 22 identifying the number of dims to 
# be transmitted (22 is equivalent to 100%)
# 
# Bit 2 is always set to '1' to ensure that the interface is able to
# maintain synchronization.
# 
# F/A defines whether the following byte is a function (1) or address (0).
# 
# E/S defines whether the following byte is an extended transmission (1)
# or a standard transmission (0).

  code = 0;
  dims = max(0, min(22,dims))
  code = dims << 1
  code |= 1
  code = code << 1
  if isFunction: code += 1
  code = code << 1
  if isExtended: code += 1
  return code

def address_or_function_code(housecode, addressOrFunctionCode):
  """Return an X10-encoded byte for the given address or function"""

# From the protocol.txt file:
# 
# 	Bit: 	 7   6   5   4   3   2   1   0
# 	Address: < Housecode >   <Device Code>
# 	Function:< Housecode >   < Function  >
# 
# Note the function only operates for devices addressed with the same Housecode.

  housecode = HOUSECODE_TO_VALUE_MAP[housecode.upper()]
  code = housecode << 4
  code |= (addressOrFunctionCode & 0xF)
  return code

def header_and_address(housecode, address):
  """Return 2 bytes encoding the given housecode and address"""
  hi = header(0, False, False)
  lo = address_or_function_code(housecode, UNITCODE_TO_VALUE_MAP[address])
  return utils.bytes_2_str((hi, lo))

def header_and_standard_function(housecode, functionCode, dims=0):
  """Return 2 bytes encoding the given housecode, function and number of dims"""
  if not isinstance(functionCode, int):
    functionCode = UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP[functionCode]
  hi = header(dims, True, False)
  lo = address_or_function_code(housecode, functionCode)
  return utils.bytes_2_str((hi, lo))


def encodeClockBytes(housecode, theTime = datetime.datetime.now(), timerPurge=False, clearBatteryTimer=False, clearMonitoredStatus=False):

# From protocol.txt:
#
# For a CM11, the time request from the interface is:	0xa5.
# 
# The PC must then respond with the following transmission
# 
#     Note:  The bit range is backwards from what you'd expect in serial 
#     communications.  Bit 55-48 is actually the first byte transmitted,
#     etc.  To make matters worse, the bit orientation is correct within
#     the bit range, IE bits 4-7 of byte 6 _IS_ the monitored house code.
#     Further, bits 0 and 1 of byte 6 appear to be flipped.  I get a 
#     "monitor status clear" if bit 0 is set.
#     The original docs had bit 23 as part of current hours AND day.
#     DBS Jan 1, 1997
# 
#     Descriptions of bits 0-3 are now correct as shown below.
#     CWS Sep 1, 2002
# 
# Bit range	Description
# 55 to 48	timer download header (0x9b)			(byte 0)
# 47 to 40	Current time (seconds)				(byte 1)
# 39 to 32	Current time (minutes ranging from 0 to 119)    (byte 2)
# 31 to 24	Current time (hours/2, ranging from 0 to 11)	(byte 3)
# 23 to 15	Current year day (MSB is bit 15)		(byte 4+.1)
# 14 to 8		Day mask (SMTWTFS)				(byte 5-.1)
# 7 to 4		Monitored house code				(byte 6...)
# 3		Reserved
# 2		Timer purge flag 		
# 1		Battery timer clear flag
# 0		Monitored status clear flag	
# 
# The CM11a will not respond to any other transmission until its time
# request is satisfied.  Per Buzz Burrowes, sending just the header (0x9b)
# followed by some indeterminate delay of the order of 10 milliseconds
# is sufficient to satisfy the time request without having to modify the
# clock setting. (CWS May 19, 2003)
  
  x10_housecode = HOUSECODE_TO_VALUE_MAP[housecode]
  yearday = int(theTime.strftime("%j"))
  weekday = int(theTime.strftime("%w"))
  daymask = 1 << weekday
  clearFlags = 0
  if timerPurge: clearFlags |= (1 << 2)
  if clearBatteryTimer: clearFlags |= (1 << 1)
  if clearMonitoredStatus: clearFlags |= 1
  return utils.bytes_2_str([theTime.second,
                            theTime.minute + 60 * (theTime.hour & 1),
                            theTime.hour / 2,
                            yearday % 256,
                            ((yearday / 256) << 7) | daymask,
                            (x10_housecode << 4) | clearFlags])

def decodeClockBytes(data, isStatusResponse=False):
  if isinstance(data, str):
    data = utils.str_2_bytes(data)
  if len(data) != 6:
    raise Exception("clock data should be 6 bytes long")
  secs = data[0]
  mins = data[1]
  hours = data[2] * 2
  hours += mins / 60
  mins = mins % 60
  yearday = data[3]
  yearday += (data[4] & 0x80) << 1
  daymask = data[4] & 0x7F
  timeStr = "%(yearday)d %(hours)d %(mins)d %(secs)d" % locals()
  theTime = datetime.datetime.strptime(timeStr, "%j %H %M %S")
  housecode = VALUE_TO_HOUSECODE_MAP[data[5] >> 4]

  timerPurge = None
  clearBatteryTimer = None
  clearMonitoredStatus = None
  if not isStatusResponse:
    timerPurge = (data[5] & 4) != 0
    clearBatteryTimer = (data[5] & 2) != 0
    clearMonitoredStatus = (data[5] & 1) != 0
  return (housecode, theTime, daymask, timerPurge, clearBatteryTimer, clearMonitoredStatus)

def decodeDawnDusk(data):
  if isinstance(data, str):
    data = utils.str_2_bytes(data)
  expectedLength = 3
  if len(data) != expectedLength:
    raise Exception("dawn/dusk data should be %(expectedLength)d bytes long" % locals())
  stopHour = 2 * (data[0] >> 4)
  startHour = 2 * (data[0] & 0xF)
  stopMins = data[1] & 0x7F
  startMins = data[2] & 0x7F
  return (datetime.time(startHour + (startMins/60), startMins % 60),
          datetime.time(stopHour  + (stopMins/60),  stopMins % 60))
  

def decodeStatusResponse(data):
  if isinstance(data, str):
    data = utils.str_2_bytes(data)
  expectedLength = 14
  if len(data) != expectedLength:
    raise Exception("status data should be %(expectedLength)d bytes long" % locals())
  batteryTimer = data[0] * 0x100 + data[1]
  clock = decodeClockBytes(data[2:8], True)
  houseCode = clock[0]
  deviceTime = clock[1]
  daymask = clock[2]
  firmwareRevision = data[7] & 0xFF
  monitoredDevices = data[8] * 0x100 + data[9]
  monitoredDevicesOnMask = data[10] * 0x100 + data[11]
  monitoredDevicesDims = data[12] * 0x100 + data[13]
  return (houseCode, deviceTime, daymask, firmwareRevision, monitoredDevices, monitoredDevicesOnMask, monitoredDevicesDims)

def decodeTimerInitiator(data):
  if len(data) != 9:
    raise Exception("timer initiator should be 9 bytes")
  dayOfWeekMask = data[0] & 0x7F
  startDay = data[1]
  stopDay = data[2]
  startHour = 2 * (data[3] >> 4)
  stopHour = 2 * (data[3] & 0xF)
  startDay += 0x100 * (data[4] >> 7)
  startMins = data[4] & 0x7F
  stopDay += 0x100 * (data[5] >> 7)
  stopMins = data[5] & 0x7F
  startSecurityBit = data[6] >> 7
  startMacroPtr = 0x100 * ((data[6] & 0x30) >> 4)
  stopSecurityBit = (data[6] & 0x4) >> 2
  stopMacroPtr = 0x100 * (data[6] & 0x3)
  startMacroPtr += data[7]
  stopMacroPtr += data[8]
  return (dayOfWeekMask, startDay, stopDay, 
          datetime.time(startHour + (startMins/60), startMins % 60), 
          datetime.time(stopHour + (stopMins/60), stopMins % 60),
          startSecurityBit, stopSecurityBit,
          startMacroPtr, stopMacroPtr)

if __name__ == "__main__":

  from utils import *

  class Tester(unittest.TestCase):
    def testHeader(self):
      functionHeaderZeroDim = header(0, True, False)
      self.assertEqual(0x06, functionHeaderZeroDim)
      functionHeader16Dim = header(16, True, False)
      self.assertEqual(0x86, functionHeader16Dim)
      addressHeader = header(0, False, False)
      self.assertEqual(0x04, addressHeader)

    def testAddressOrFunction(self):
      addressA1 = address_or_function_code("A", UNITCODE_TO_VALUE_MAP[1])
      self.assertEqual(0x66, addressA1)
      functionADim = address_or_function_code("A", UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["dim"])
      self.assertEqual(0x64, functionADim)

    def testMaps(self):
      self.assertEqual(6, UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["all_lights_off"])
      self.assertEqual("Extended Code", FUNCTIONCODE_TO_STRING_MAP[7])

      self.assertEqual("H", VALUE_TO_HOUSECODE_MAP[0xd])
      self.assertEqual(7, HOUSECODE_TO_VALUE_MAP["I"])

      self.assertEqual(10, VALUE_TO_UNITCODE_MAP[0xf])
      self.assertEqual(0xd, UNITCODE_TO_VALUE_MAP[8])

    def testHeaderAndAddress(self):
      self.assertEqual(utils.bytes_2_str((0x4, 0x6E)), header_and_address("A", 2))

    def testHeaderAndFunction(self):
      self.assertEqual(utils.bytes_2_str((0x86, 0x64)), header_and_standard_function("A", "dim", 16))

    def testClockBytes(self):
      sampledBytes = printed_bytes_to_bytes("002300790862") # set at 2013-05-01 00:35
      theTime = datetime.datetime(2013, 5, 1, 0, 35, 0)
      houseCode = "A"
      data = encodeClockBytes(houseCode, theTime, False, True, False)
      self.assertEqual(sampledBytes, str_2_bytes(data))

      theTime = datetime.datetime(1900, 11, 12, 13, 14, 15)
      daymask = 1 << int(theTime.strftime("%w"))
      data = encodeClockBytes(houseCode, theTime, True, False, True)
      (newHouseCode, newTime, newDaymask, timerPurge, clearBatteryTimer, clearMonitoredStatus) = decodeClockBytes(data)
      self.assertEqual(houseCode, newHouseCode)
      self.assertEqual(theTime, newTime)
      self.assertEqual(daymask, newDaymask)
      self.assertTrue(timerPurge)
      self.assertFalse(clearBatteryTimer)
      self.assertTrue(clearMonitoredStatus)

    def testStatusDecode(self):
      sampledBytes = printed_bytes_to_bytes("0000002300790862004000000000")
      (houseCode, deviceTime, daymask, firmware, monitoredDevices, monitoredDevicesOnMask, monitoredDevicesDims) = \
          decodeStatusResponse(bytes_2_str(sampledBytes))
      theTime = datetime.datetime(1900, 5, 1, 0, 35, 0) # year will be 1900 as not set
      self.assertEqual("A", houseCode)
      self.assertEqual(theTime, deviceTime)
      theTime = datetime.datetime(2013, 5, 1, 0, 35, 0) # however we need to extract weekday from date 
                                                        #with correct year (as the CM15 doesn't store the year)
      theDaymask = 1 << int(theTime.strftime("%w"))
      self.assertEqual(theDaymask, daymask)
      
    def testTimerDecode(self):
#      sampledBytes = printed_bytes_to_bytes("002e004b31003b3400")
#      sampledBytes = printed_bytes_to_bytes("000154812a40000500")
      sampledBytes = printed_bytes_to_bytes("be 01 6d 4b 17 c6 00 27 2a")
      print decodeTimerInitiator(sampledBytes)

  unittest.main()
