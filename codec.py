#!/usr/bin/python

# This file is part of AutoMateHome.

# AutoMateHome is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# AutoMateHome is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied waranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with AutoMateHome.  If not, see <http://www.gnu.org/licenses/>.

"""Assorted X-10 and CM15 coding/decoding utilities"""

import utils

BITLENGTH_MASKS = (None, 0x1, 0x3, 0x7, 0xf, 0x1f, 0x3f, 0x7f, 0xff)

class codec:
  """Generic encoder/decoder class. A mapping is specified between a
  named set of integers and the bits in a list of bytes, and this
  class provides conversion methods between the two
  representations. There is also a self test facility to ensure that
  all source and destination bits are covered by the mapping"""

  def __init__(self, mapping):
    """Constructor for the codec. The values in MAPPING should have the following format:
    list( (source_byte, source_bit, field_length, destination_bit) )
    Byte and bit indices are zero-offset, the most-significant bit is bit 7."""

    self.mapping = mapping
    codelength = 0
    for l in mapping.itervalues():
      for l1 in l:
        codelength = max(l1[0]+1, codelength)
    self.codelength = codelength

  def decode(self, data, offset=0):

    if offset > 0:
      data = data[offset:offset+self.codelength]
      
    if len(data) < self.codelength:
      raise Exception("expected %d bytes to decode, got %d" % (self.codelength, len(data)))

    result = dict()
    for (key,bit_spec) in self.mapping.iteritems():
      byte = 0;
#      print key, bit_spec
      for (srcbyte, srcbit, bitlen, destbit) in bit_spec:
        assert(bitlen > 0 and bitlen <= 8)
        lshift = srcbit - bitlen + 1
        assert(lshift >= 0)
        bits = data[srcbyte]
        bits = bits >> lshift
        bits &= BITLENGTH_MASKS[bitlen]
        rshift = destbit - bitlen + 1
        assert(rshift >= 0)
        assert(0 == (BITLENGTH_MASKS[bitlen] & (byte >> rshift)))
        bits = bits << rshift
        byte |= bits
      result[key] = byte
        
    return result

  def encode(self, data):
    result = [0 for i in range(0,self.codelength)]
    for (key,bit_spec) in self.mapping.iteritems():
      byte = data[key]
      for (srcbyte, srcbit, bitlen, destbit) in bit_spec:
        assert(bitlen > 0 and bitlen <= 8)
        mask = BITLENGTH_MASKS[bitlen]
        lshift = destbit - bitlen + 1
        assert(lshift >= 0)
        bits = byte >> lshift
        bits &= mask
        rshift = srcbit - bitlen + 1
        assert(0 == (mask & (result[srcbyte] >> rshift)))
        bits = bits << rshift
        assert(rshift >= 0)
        result[srcbyte] |= bits
    return result

  def self_test(self):
    dest_dat = [0 for i in range(0,self.codelength)]

    # fill the input data with set bits up to the maximum bit length
    # in each field:
    bit_lengths = dict([(k, 0) for k in self.mapping.keys()])
    for (key,bit_spec) in self.mapping.iteritems():
      for (srcbyte, srcbit, bitlen, destbit) in bit_spec:
        bit_lengths[key] = max(bit_lengths[key], destbit)
    src_data = dict()
    for k,v in bit_lengths.iteritems():
      mask = 0
      for i in range(0,v+1):
        mask = mask << 1
        mask |= 1
      src_data[k] = mask

    for (key,bit_spec) in self.mapping.iteritems():
      for (srcbyte, srcbit, bitlen, destbit) in bit_spec:
        if not (bitlen > 0 and bitlen <= 8):
          raise Exception("invalid bitlen in %(key)s field: %(bit_spec)s" % locals())
        mask = BITLENGTH_MASKS[bitlen]
        lshift = destbit - bitlen + 1
        if not (lshift >= 0):
          raise Exception("invalid bit spec in %(key)s field: %(bit_spec)s" % locals())
        bits = src_data[key] >> lshift
        bits &= mask
        if bits != mask:
          print src_data[key], bits, mask
          raise Exception("double read from source byte %(srcbyte)d, bit %(srcbit)d by field: %(key)s " % locals())
        src_data[key] &= ~(mask << lshift)
        rshift = srcbit - bitlen + 1
        if not (rshift >= 0):
          raise Exception("invalid bit spec in %(key)s field: %(bit_spec)s" % locals())
        if not (0 == (mask & (dest_dat[srcbyte] >> rshift))):
          raise Exception("double write to source byte %(srcbyte)d by field: %(key)s " % locals())
        bits = bits << rshift
        assert(rshift >= 0)
        dest_dat[srcbyte] |= bits

    for i,b in enumerate(dest_dat):
      for j in range (7,-1,-1):
        if (b >> j) & 1 != 1:
          raise Exception("byte %(i)d bit %(j)d was not covered by this codec" % locals())
      if (b >> 8) != 0:
          raise Exception("byte %(i)d overflowed" % locals())
    for key,val in src_data.iteritems():
      if val != 0:
        raise Exception("key %(key)s was not fully covered by this codec" % locals())

    return True

# Encoding of clock bytes as per section 8 of protocol.txt. This is
# used to reset the device after a power fail.
DEVICE_CLOCK = codec({
    "second"             : [(0, 7, 8, 7)],
    "minute"             : [(1, 7, 8, 7)],
    "double_hour"        : [(2, 7, 8, 7)],
    "year_day"           : [(3, 7, 8, 7), (4, 7, 1, 8)],
    "day_mask"           : [(4, 6, 7, 6)],
    "house_code"         : [(5, 7, 4, 3)],
    "reserved_1"         : [(5, 3, 1, 0)],
    "timer_purge"        : [(5, 2, 1, 0)],
    "battery_clear"      : [(5, 1, 1, 0)],
    "monitor_status_clr" : [(5, 0, 1, 0)],
    })

# Apparent encoding of DST events in the CM15 EEPROM. Guessed from
# observations of EEPROM downloads from ActiveHome Pro.  Seems
# approximately correct although it was up to 5 days out for some
# timezones in 2013
DST_DAYS = codec({
    "begin_year_day"  : [(1, 7, 8, 7), (0, 7, 1, 8)],
    "end_year_day"   : [(3, 7, 8, 7), (2, 7, 1, 8)],
    "reserved_1"      : [(0, 6, 7, 6)],
    "reserved_2"      : [(2, 6, 7, 6)]
    })

# Monitored RF Housecodes encoded into 2 bytes in the EEPROM. Note
# that the lower codes are encoded into the first byte, opposite to
# what you would expect from bit shifting.
HOUSECODE_MASK = codec({
    'A':  [(0, 0x6-0, 1, 0)],
    'B':  [(1, 0xE-8, 1, 0)],
    'C':  [(0, 0x2-0, 1, 0)],
    'D':  [(1, 0xA-8, 1, 0)],
    'E':  [(0, 0x1-0, 1, 0)],
    'F':  [(1, 0x9-8, 1, 0)],
    'G':  [(0, 0x5-0, 1, 0)],
    'H':  [(1, 0xD-8, 1, 0)],
    'I':  [(0, 0x7-0, 1, 0)],
    'J':  [(1, 0xF-8, 1, 0)],
    'K':  [(0, 0x3-0, 1, 0)],
    'L':  [(1, 0xB-8, 1, 0)],
    'M':  [(0, 0x0-0, 1, 0)],
    'N':  [(1, 0x8-8, 1, 0)],
    'O':  [(0, 0x4-0, 1, 0)],
    'P':  [(1, 0xC-8, 1, 0)],
    })

# device codes encoded into 2 bytes in a macro. 
DEVICECODE_MASK = codec({
    1:   [(1, 0x6-0, 1, 0)],
    2:   [(0, 0xE-8, 1, 0)],
    3:   [(1, 0x2-0, 1, 0)],
    4:   [(0, 0xA-8, 1, 0)],
    5:   [(1, 0x1-0, 1, 0)],
    6:   [(0, 0x9-8, 1, 0)],
    7:   [(1, 0x5-0, 1, 0)],
    8:   [(0, 0xD-8, 1, 0)],
    9:   [(1, 0x7-0, 1, 0)],
    10:  [(0, 0xF-8, 1, 0)],
    11:  [(1, 0x3-0, 1, 0)],
    12:  [(0, 0xB-8, 1, 0)],
    13:  [(1, 0x0-0, 1, 0)],
    14:  [(0, 0x8-8, 1, 0)],
    15:  [(1, 0x4-0, 1, 0)],
    16:  [(0, 0xC-8, 1, 0)],
    })

# 3 bytes which encode a dawn/dusk pair in the CM15's EEPROM 
DAWN_DUSK_ENTRY = codec({
    "stop_double_hour"  : [(0, 7, 4, 3)],
    "start_double_hour" : [(0, 3, 4, 3)],
    "stop_min"          : [(1, 6, 7, 6)],
    "start_min"         : [(2, 6, 7, 6)],
    "reserved_1"        : [(1, 7, 1, 0)],
    "reserved_2"        : [(2, 7, 1, 0)]
    })

TIMER_INITIATOR = codec({
    "reserved_1":          [(0, 7, 1, 0)],
    "week_day_mask":       [(0, 6, 7, 6)],
    "begin_year_day":      [(1, 7, 8, 7), (4, 7, 1, 8)],
    "end_year_day":       [(2, 7, 8, 7), (5, 7, 1, 8)],
    "start_double_hour":   [(3, 7, 4, 3)],
    "stop_double_hour":    [(3, 3, 4, 3)],
    "start_min":           [(4, 6, 7, 6)],
    "stop_min":            [(5, 6, 7, 6)],
    "start_security":      [(6, 7, 1, 0)],
    "reserved_2":          [(6, 6, 1, 0)],
    "start_macro_ptr":     [(6, 5, 2, 9), (7, 7, 8, 7)],
    "stop_security":       [(6, 3, 1, 0)],
    "reserved_3":          [(6, 2, 1, 0)],
    "stop_macro_ptr":      [(6, 1, 2, 9), (8, 7, 8, 7)]
    })

MACRO_INITIATOR = codec({
    "house_code":      [(0, 7, 4, 3)],
    "unit_code":       [(0, 3, 4, 3)],
    "trigger_on_off":  [(1, 7, 1, 0)],
    "n_clauses":       [(1, 6, 2, 1)],
    "reserved":        [(1, 4, 1, 0)],
    "macro_ptr":       [(1, 3, 4, 11), (2, 7, 8, 7)]
    })

MACRO_INITIATOR_CLAUSE_HEADER = codec({
    "type":           [(0, 7, 3, 2)],
    "hdr_reserved_1": [(0, 4, 1, 0)],
    "compare_op":     [(0, 3, 2, 1)],
    "hdr_reserved_2": [(0, 1, 1, 0)],
    "logic_op":       [(0, 0, 1, 0)],
    })

MACRO_INITIATOR_CLAUSE_BODY_TIME_TYPE = codec({
    "reserved":      [(0, 5, 2, 1)],
    "sunrise":       [(0, 6, 1, 0)],
    "sunset":        [(0, 7, 1, 0)],
    "double_hour":   [(0, 3, 4, 3)],
    "is_variable":   [(1, 7, 1, 0)],
    "min":           [(1, 6, 7, 6)],
    })

MACRO_INITIATOR_CLAUSE_BODY_DATE_TYPE = codec({
    "year_day":  [(0, 7, 8, 7), (1, 7, 1, 8)],
    "reserved":  [(1, 6, 7, 6)],
    })

MACRO_INITIATOR_CLAUSE_BODY_DAY_TYPE = codec({
    "reserved_1":     [(0, 7, 1, 0)],
    "week_day_mask":  [(0, 6, 7, 6)],
    "reserved_2":     [(1, 7, 8, 7)],
    })

MACRO_HEADER = codec({
    "reserved_1": [(0, 7, 8, 8), (1, 7, 1, 0)],
    "delay_secs": [(1, 6, 7, 14), (2, 7, 8, 7)],
    "n_elements": [(3, 7, 8, 7)],
    })

MACRO_COMMON = codec({
    "house_code":       [(0, 7, 4, 3)],
    "function_code":    [(0, 3, 4, 3)],
    "unit_bitmap_hi":   [(1, 7, 8, 7)], 
    "unit_bitmap_lo":   [(2, 7, 8, 7)], 
    })

MACRO_BRIGHT_DIM_SUFFIX = codec({
    "bright_or_dim":    [(0, 7, 1, 0)],
    "reserved":         [(0, 6, 2, 1)],
    "dim_value":        [(0, 4, 5, 4)], 
    })

MACRO_EXTENDED_CMD_SUFFIX = codec({
    "byte_hi":        [(0, 7, 8, 7)],
    "byte_lo":        [(1, 7, 8, 7)],
    })

# comparison operators used in macro initiatior clauses
MACRO_INITIATOR_HEADER_COMPARE_OPS = ("==", "!=", ">", "<")

# logical operators used between macro initiatior clauses
MACRO_INITIATOR_HEADER_LOGIC_OPS = ("and", "or")

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
                                     "Extended Command",
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


if __name__ == "__main__":
        
  import unittest
  from utils import *

  class Tester(unittest.TestCase):

    def testTimerInitiator(self):
      sampledBytes = printed_bytes_to_bytes("be016d4b17c600272a")    
      self.assertTrue(TIMER_INITIATOR.self_test())
      result = TIMER_INITIATOR.decode(sampledBytes)
      self.assertEqual(1, result['begin_year_day'])
      self.assertEqual(11, result['stop_double_hour'])
      self.assertEqual(70, result['stop_min'])
      self.assertEqual(365, result['end_year_day'])
      self.assertEqual(0, result['reserved_3'])
      self.assertEqual(0, result['reserved_2'])
      self.assertEqual(1, result['reserved_1'])
      self.assertEqual(62, result['week_day_mask'])
      self.assertEqual(4, result['start_double_hour'])
      self.assertEqual(23, result['start_min'])
      self.assertEqual(0, result['start_security'])
      self.assertEqual(42, result['stop_macro_ptr'])
      self.assertEqual(39, result['start_macro_ptr'])
      self.assertEqual(sampledBytes, TIMER_INITIATOR.encode(result))

    def testMacroInitiator(self):
      sampledBytes = printed_bytes_to_bytes("e28025") # b3 on -> 0x0025 ??   
      sampledBytes = printed_bytes_to_bytes("678035") # a9 on -> 0x0035
      self.assertTrue(MACRO_INITIATOR.self_test())
      result = MACRO_INITIATOR.decode(sampledBytes)
      self.assertEqual(1, result['trigger_on_off'])
      self.assertEqual(0x7, result['unit_code'])
      self.assertEqual(0x6, result['house_code'])
      self.assertEqual(0x0, result['reserved'])
      self.assertEqual(sampledBytes, MACRO_INITIATOR.encode(result))

    def testDawnDusk(self):
      sampledBytes = printed_bytes_to_bytes("833760")    

      self.assertTrue(DAWN_DUSK_ENTRY.self_test())
      result = DAWN_DUSK_ENTRY.decode(sampledBytes)
      self.assertEqual(55, result['stop_min'])
      self.assertEqual(8, result['stop_double_hour'])
      self.assertEqual(96, result['start_min'])
      self.assertEqual(3, result['start_double_hour'])
      self.assertEqual(0, result['reserved_1'])
      self.assertEqual(0, result['reserved_2'])
      self.assertEqual(sampledBytes, DAWN_DUSK_ENTRY.encode(result))
      
    def testDeviceClock(self):
      sampledBytes = printed_bytes_to_bytes("002300790862") # set at 2013-05-01 00:35
      self.assertTrue(DEVICE_CLOCK.self_test())
      result = DEVICE_CLOCK.decode(sampledBytes)
      self.assertEqual(1, result['battery_clear'])
      self.assertEqual(0, result['monitor_status_clr'])
      self.assertEqual(0, result['timer_purge'])
      self.assertEqual(0, result['second'])
      self.assertEqual(35, result['minute'])
      self.assertEqual(121, result['year_day'])
      self.assertEqual(0, result['double_hour'])
      self.assertEqual(6, result['house_code'])
      self.assertEqual(0, result['reserved_1'])
      self.assertEqual(sampledBytes, DEVICE_CLOCK.encode(result))

    def testHousecodeMask(self):
      sampledBytes = printed_bytes_to_bytes("4a80") # A, E, J and K 
      self.assertTrue(HOUSECODE_MASK.self_test())
      result = HOUSECODE_MASK.decode(sampledBytes)
      self.assertEqual(1, result['A'])
      self.assertEqual(0, result['B'])
      self.assertEqual(0, result['C'])
      self.assertEqual(0, result['D'])
      self.assertEqual(1, result['E'])
      self.assertEqual(0, result['F'])
      self.assertEqual(0, result['G'])
      self.assertEqual(0, result['H'])
      self.assertEqual(0, result['I'])
      self.assertEqual(1, result['J'])
      self.assertEqual(1, result['K'])
      self.assertEqual(0, result['L'])
      self.assertEqual(0, result['M'])
      self.assertEqual(0, result['N'])
      self.assertEqual(0, result['O'])
      self.assertEqual(0, result['P'])
      self.assertEqual(sampledBytes, HOUSECODE_MASK.encode(result))

    def testTimerInitiator(self):
      # TODO - finish this
      self.assertTrue(TIMER_INITIATOR.self_test())

    def testMacroInitiator(self):
      # TODO - finish this
      self.assertTrue(MACRO_INITIATOR.self_test())
      self.assertTrue(MACRO_INITIATOR_CLAUSE_HEADER.self_test())
      self.assertTrue(MACRO_INITIATOR_CLAUSE_BODY_DAY_TYPE.self_test())
      self.assertTrue(MACRO_INITIATOR_CLAUSE_BODY_DATE_TYPE.self_test())
      self.assertTrue(MACRO_INITIATOR_CLAUSE_BODY_TIME_TYPE.self_test())

    def testMacroChain(self):
      # TODO - finish this
      self.assertTrue(MACRO_HEADER.self_test())
      self.assertTrue(MACRO_COMMON.self_test())
      self.assertTrue(MACRO_BRIGHT_DIM_SUFFIX.self_test())
      self.assertTrue(MACRO_EXTENDED_CMD_SUFFIX.self_test())

    def testUnitCodeMask(self):
      self.assertTrue(DEVICECODE_MASK.self_test())
      sampledBytes = printed_bytes_to_bytes("4240") # units 1, 2, 6
      result = DEVICECODE_MASK.decode(sampledBytes)
      for k,v in result.iteritems():
        if k in (1, 2, 6):
          self.assertEqual(1, v)
        else:
          self.assertEqual(0, v)

    def testDST(self):
      # TODO - finish this
      sampledBytes = printed_bytes_to_bytes("8112015b") # Australia
      sampledBytes = printed_bytes_to_bytes("0154812a") # UK
      self.assertTrue(DST_DAYS.self_test())
      result = DST_DAYS.decode(sampledBytes)


  unittest.main()

  
