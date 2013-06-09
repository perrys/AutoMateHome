#!/usr/bin/python

# This file is part of AutoMateHome.

# AutoMateHome is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# AutoMateHome is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with AutoMateHome.  If not, see <http://www.gnu.org/licenses/>.

import datetime
import xml.etree.ElementTree
import json
import sys

import codec
import utils

CLAUSE_TYPE_TIME        = 0
CLAUSE_TYPE_DATE        = 1
CLAUSE_TYPE_DAY         = 2
CLAUSE_TYPE_MODULES_ON  = 3
CLAUSE_TYPE_MODULES_OFF = 4
CLAUSE_TYPE_FLAGS_ON    = 5
CLAUSE_TYPE_FLAGS_OFF   = 6

class MacroInitiatorClause(object):

  @staticmethod
  def create(bytes, offset=0):
    # hack to avoid double read in debug mode:
    if isinstance(bytes, MemoryBuffer):
      header_byte = [bytes.peek(offset)]
    else:
      header_byte = bytes[offset:offset+1]
    attribs = codec.MACRO_INITIATOR_CLAUSE_HEADER.decode(header_byte)
    clauseType = attribs["type"]
    if clauseType == CLAUSE_TYPE_TIME:
      return MacroInitiatorClauseTimeType(bytes, offset)
    elif clauseType == CLAUSE_TYPE_DATE:
      return MacroInitiatorClauseDateType(bytes, offset)
    elif clauseType == CLAUSE_TYPE_DAY:
      return MacroInitiatorClauseDayType(bytes, offset)
    elif clauseType == CLAUSE_TYPE_MODULES_OFF or clauseType == CLAUSE_TYPE_MODULES_ON:
      return MacroInitiatorClauseModulesType(bytes, offset)
    elif clauseType == CLAUSE_TYPE_FLAGS_OFF or clauseType == CLAUSE_TYPE_FLAGS_ON:
      return MacroInitiatorClauseFlagsType(bytes, offset)
    else:
      raise Exception("unknown macro initiator clause type: 0x%0x" % clauseType)

  def __init__(self, bytes, offset=0):
    self.__dict__.update(codec.MACRO_INITIATOR_CLAUSE_HEADER.decode(bytes, offset))
    self.logic_op = codec.MACRO_INITIATOR_HEADER_LOGIC_OPS[self.logic_op]
    self.compare_op = codec.MACRO_INITIATOR_HEADER_COMPARE_OPS[self.compare_op]

  def toJSON(self):
    l = utils.mergeDict(vars(self), None, "compare_op", "logic_op")
    utils.mergeDictIfNotDefault(vars(self), l, "hdr_reserved_1", 0)
    utils.mergeDictIfNotDefault(vars(self), l, "hdr_reserved_2", 0)
    return l

class MacroInitiatorClauseTimeType(MacroInitiatorClause):
  def __init__(self, bytes, offset=0):
    super(self.__class__, self).__init__(bytes, offset)
    vars(self).update(codec.MACRO_INITIATOR_CLAUSE_BODY_TIME_TYPE.decode(bytes, 1+offset))
    self.time = utils.x10TimeToTime(self.double_hour, self.min)

  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    time = None
    if self.is_variable:
      if self.sunrise:
        time = "Sunrise"
      else:
        time = "Sunset"
    else:
      time = self.time.strftime("%H:%M")
    l["time"] = time
    utils.mergeDictIfNotDefault(vars(), l, "time", None)
    utils.mergeDictIfNotDefault(vars(self), l, "reserved", 0)
    return l

class MacroInitiatorClauseDateType(MacroInitiatorClause):
  def __init__(self, bytes, offset=0):
    super(self.__class__, self).__init__(bytes, offset)
    self.__dict__.update(codec.MACRO_INITIATOR_CLAUSE_BODY_DATE_TYPE.decode(bytes, 1+offset))
    timeStr = "%(year_day)d" % self.__dict__
    self.date = datetime.datetime.strptime(timeStr, "%j").date()

  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    utils.mergeDictIfNotDefault(vars(self), l, "year_day", None)
    l["date_display"] = utils.x10YearDayToString(self.year_day)
    utils.mergeDictIfNotDefault(vars(self), l, "reserved", 0)
    return l

class MacroInitiatorClauseDayType(MacroInitiatorClause):
  def __init__(self, bytes, offset=0):
    super(self.__class__, self).__init__(bytes, offset)
    self.__dict__.update(codec.MACRO_INITIATOR_CLAUSE_BODY_DAY_TYPE.decode(bytes, 1+offset))
    self.week_day_mask_display = utils.formatWeekMask(self.week_day_mask)

  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    utils.mergeDictIfNotDefault(vars(self), l, "week_day_mask_display", None)
    utils.mergeDictIfNotDefault(vars(self), l, "reserved_1", 0)
    utils.mergeDictIfNotDefault(vars(self), l, "reserved_2", 0)
    return l

class MacroInitiatorClauseFlagsType(MacroInitiatorClause):
  def __init__(self, bytes, offset=0):
    super(self.__class__, self).__init__(bytes, offset)
    mask = 0x100 * bytes[1+offset] + bytes[2+offset]
    self.flags = [i for i in range(15, -1, -1) if (1 & mask >> i)]

  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    l["criteria"] = self.type == CLAUSE_TYPE_FLAGS_ON and "on" or "off"
    utils.mergeDictIfNotDefault(vars(self), l, "flags", None)
    return l

class MacroInitiatorClauseModulesType(MacroInitiatorClause):
  def __init__(self, bytes, ptr=None):
    super(self.__class__, self).__init__(bytes, offset)
    mask = 0x100 * bytes[1] + bytes[2]
    self.modules = [codec.UNITCODE_TO_VALUE_MAP[i] for i in range(15, -1, -1) if (1 & (mask >> i))]

  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    l["Criteria"] = self.type == CLAUSE_TYPE_MODULES_ON and "on" or "off"
    utils.mergeDictIfNotDefault(vars(self), l, "modules", None)

class MacroInitiator:
  def __init__(self, bytes, offset=0):
    self.__dict__.update(codec.MACRO_INITIATOR.decode(bytes, offset))
    self.clauses = []
    for i in range(0, self.n_clauses):
      offset += 3
      self.clauses.append(MacroInitiatorClause.create(bytes, offset))
    self.codelength = 3 * self.n_clauses + 3
    self.house_code  = codec.VALUE_TO_HOUSECODE_MAP[self.house_code]
    self.unit_code = codec.VALUE_TO_UNITCODE_MAP[self.unit_code]

  def __str__(self):
    clauses = [("   ", str(c.__dict__)) for c in self.clauses]
    data = [
      [" house_code:", self.house_code],
      [" unit code:",  self.unit_code],
      [" trigger:",    self.trigger_on_off and "on" or "off"],
      [" macro:",      "0x%04x" % self.macro_ptr],
      [" clauses:",    "\n" + utils.formatTable(clauses)],
      ]
    return utils.formatTable(data)

  def toJSON(self):
    d = dict(self.__dict__)
    del d["n_clauses"]
    del d["codelength"]
    if self.reserved == 0:
      del d["reserved"]
    d["trigger_on_off"] = self.trigger_on_off and "on" or "off"
    return d

    # l = [self.house_code, self.unit_code, self.trigger_on_off and "on" or "off", "0x%04x" % self.macro_ptr]
    # if self.reserved != 0:
    #   l.append(self.reserved)
    # l.append(self.clauses)
    # return l

  def toXML(self, builder):
    builder.start("MacroInitiator")
    builder.start("HouseCode").data(self.house_code).end()
    builder.start("UnitCode").data(self.unit_code).end()
    builder.start("Trigger").data(self.trigger_on_off and "on" or "off").end()
    builder.start("Macro").data("0x%04x" % self.macro_ptr).end()
    if self.reserved != 0:
      builder.start("Reserved").data(self.reserved).end()
    if self.n_clauses > 0:
      builder.start("Clauses")
      for c in self.clauses:
        c.toXML(builder)
      builder.end()
    builder.end()

class MacroChain:

  def __init__(self, bytes, offset=0):
    start = offset
    self.id = offset
    self.__dict__.update(codec.MACRO_HEADER.decode(bytes, offset))
    offset += codec.MACRO_HEADER.codelength
    self.elements = []
    for i in range(0, self.n_elements):
      d = codec.MACRO_COMMON.decode(bytes, offset)
      units = d["unit_bitmap_hi"], d["unit_bitmap_lo"]
      d["units"] = [k for k,v in codec.DEVICECODE_MASK.decode(units).iteritems() if v == 1]
      del d["unit_bitmap_lo"]
      del d["unit_bitmap_hi"]
      d["function_code"] = codec.FUNCTIONS_ZERO_OFFSET[d["function_code"]]
      d["house_code"] = codec.VALUE_TO_HOUSECODE_MAP[d["house_code"]]
      self.elements.append(d)
      offset += codec.MACRO_COMMON.codelength
      func = d["function_code"]
      if func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["dim"] or \
            func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["bright"]:
        d.update(codec.MACRO_BRIGHT_DIM_SUFFIX.decode(bytes, offset))
        offset += MACRO_BRIGHT_DIM_SUFFIX.codelength
      elif func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["extended_command"]:
        d.update(codec.MACRO_EXTENDED_CMD_SUFFIX.decode(bytes, offset))
        offset += MACRO_EXTENDED_CMD_SUFFIX.codelength

  def toJSON(self):
    l = utils.mergeDict(vars(self), None, "delay_secs", "elements", "id")
    l["reserved"] = utils.to_binary_string(self.reserved_1)
    return l

class MemoryBuffer:

  def __init__(self, copy=None, debug=False, capacity=0x2000):
    self.__debug = debug
    if copy is not None:
      self.__rom = list(copy.__rom)
    else:
      self.__rom = [None for i in range(0, capacity)]

  def __len__(self):
    return len(self.__rom)

  def __getitem__(self, idx):
    l = self.__rom[idx]
    if self.__debug:
      if isinstance(idx, slice):
        for b in l:
          if b is None:
            raise Exception("attempt to read null byte in %s" %idx)
        for i in range(idx.start, idx.stop, idx.step or 1):
          self.__rom[i] = None
      else:
        if self.__rom[idx] is None:
          raise Exception("attempt to read null byte at 0x%04x" % idx)
        self.__rom[idx] = None
    return l

  def __setitem__(self, idx, val):
    self.__rom[idx] = val

  def peek(self, idx):
    return self.__rom[idx]

  def dump(self, fh):
    for i in range(0, len(self.__rom), 16):
      fh.write("0x%04x " % i)
      for j in range(0, 4):
        for k in range(0,4):
          b = self[i + j*4 + k]
          s = ".."
          if b is not None:
            s = "%02x" % b
          fh.write(s)
        fh.write(" ")
      fh.write("\n")

  def describeUnread(self):
    l = []
    for i in range(0, len(self)):
      if self.__rom[i] is None:
        if len(l) > 0 and l[-1][-1] is None:
          l[-1][-1] = i-1
      else:
        if len(l) == 0 or l[-1][-1] is not None:
          l.append([i, None])
    if len(l) > 0 and l[-1][-1] is None:
      l[-1][-1] = i
    return l

  def used(self):
    emptyCount = 0
    totalLength = len(self.__rom)
    for b in self.__rom:
      if b is None:
        emptyCount += 1
    return totalLength-emptyCount


class EEPROM:

  def __init__(self, memoryBuffer):
    self.total_length = len(memoryBuffer)
    self.used = memoryBuffer.used()
    self.macro_initiator_table_offset = utils.merge_bytes(memoryBuffer[0:2])
    self.sunrise_sunset_table_offset = utils.merge_bytes(memoryBuffer[2:4])
    self.sunrise_sunset_resolution = memoryBuffer[4]
    self.dst_data = codec.DST_DAYS.decode(memoryBuffer, 5)
    tranceivedHousecodes = codec.HOUSECODE_MASK.decode(memoryBuffer, 9)
    self.tranceivedHousecodes = [k for k,v in tranceivedHousecodes.iteritems() if v != 0]
    self.timer_initiators = []

    ptr = 0x19
    while (self.macro_initiator_table_offset - ptr) >= 10:
      if (memoryBuffer.peek(ptr) & 0x80) == 0:
        break
      t = codec.TIMER_INITIATOR.decode(memoryBuffer, ptr)
      self.timer_initiators.append(t)
      ptr += 10

    ptr = self.macro_initiator_table_offset+1
    self.macro_initiators = []
    while memoryBuffer.peek(ptr+2) != 0xff:
      mi = MacroInitiator(memoryBuffer, ptr)
      self.macro_initiators.append(mi)
      ptr += mi.codelength
      ptr += 1
      
    self.macro_chains = []
    for m in self.macro_initiators:
      self.macro_chains.append(MacroChain(memoryBuffer, m.macro_ptr + 1))

  def toJSON(self):
    d = utils.mergeDict(vars(self), None)
    tis = d["timer_initiators"] = []
    for t in self.timer_initiators:
      tijson = utils.mergeDict(t, None, "start_macro_ptr", "stop_macro_ptr")
      if utils.mergeDictIfNotDefault(t, tijson, "begin_year_day", 0):
        tijson["start_date_display"] = utils.x10YearDayToString(t["begin_year_day"])
      if utils.mergeDictIfNotDefault(t, tijson, "end_year_day", 367):
        tijson["stop_date_display"] = utils.x10YearDayToString(t["end_year_day"])
      utils.mergeDictIfNotDefault(t, tijson, "start_security", 0)
      utils.mergeDictIfNotDefault(t, tijson, "stop_security", 0)
      tijson["start_time"] = utils.x10TimeToString(t["start_double_hour"], t["start_min"])
      tijson["stop_time"] = utils.x10TimeToString(t["stop_double_hour"], t["stop_min"])
      tijson["week_day_mask"] = utils.formatWeekMask(t["week_day_mask"])
      tis.append(tijson)
    d["dst_data"]["start_date_display"] = utils.x10YearDayToString(self.dst_data["begin_year_day"])
    d["dst_data"]["stop_date_display"] = utils.x10YearDayToString(self.dst_data["end_year_day"])
    
    del d["sunrise_sunset_table_offset"]
    del d["macro_initiator_table_offset"]
    del d["used"]
    del d["total_length"]
    return d

  def describe(self):

    data = [["Memory Used:", "%d%% (%d bytes)" % (100*self.used/self.total_length, self.used) ]]

    data.append(["Macro Initiator Offset:", "0x%(macro_initiator_table_offset)04x" % self.__dict__])
    data.append(["Dawn/Dusk Table Offset:", "0x%(sunrise_sunset_table_offset)04x" % self.__dict__])
    data.append(["Dawn/Dusk Resolution:", "%(sunrise_sunset_resolution)d day(s)" % self.__dict__])

    data.append(["DST start day:", "%(begin_year_day)d" % self.dst_data])
    data.append(["DST end day:", "%(end_year_day)d" % self.dst_data])

    data.append(["Trancieved Housecode(s):", ", ".join(self.tranceivedHousecodes)])

    for j,t in enumerate(self.timer_initiators):
      r = [[" ", "\n"],
           [" days of week:", utils.formatWeekMask(t["week_day_mask"])],
           [" start DOY:", t["begin_year_day"]],
           [" stop  DOY:", t["end_year_day"]],
           [" start time:", utils.x10TimeToTime(t["start_double_hour"], t["start_min"])],
           [" stop  time:", utils.x10TimeToTime(t["stop_double_hour"], t["stop_min"])],
           [" randomize start:", t["start_security"] and "Y" or "N"],
           [" randomize stop:",  t["stop_security"]  and "Y" or "N"],
           [" start macro:", "0x%04x" % t["start_macro_ptr"]],
           [" stop macro:", "0x%04x" % t["stop_macro_ptr"]],
           ]
      data.append(["Timer %d:" % (j+1), utils.formatTable(r)])
      if utils.x10TimeToTime(t["start_double_hour"], t["start_min"]) is not None:
        mptr = t["start_macro_ptr"]
      if utils.x10TimeToTime(t["stop_double_hour"], t["stop_min"]) is not None:
        mptr = t["stop_macro_ptr"]

    for j,m in enumerate(self.macroInitiators):
      r = [[" ", "\n"],
           [" house_code:", codec.VALUE_TO_HOUSECODE_MAP[m["house_code"]]],
           [" unit code:",  codec.VALUE_TO_UNITCODE_MAP[m["unit_code"]]],
           [" trigger:",    m["trigger_on_off"] and "on" or "off"],
           [" macro:", "0x%04x" % m["macro_ptr"]],
           [" n_clauses:", m["n_clauses"]],
           [" reserved:", utils.to_binary_string(m["reserved"])],
           [" extrabytes:", " ".join([utils.bytes_to_printed_bytes(x) for x in m["extrabytes"]])],
           ]
      data.append(["Trigger %d:" % (j+1), utils.formatTable(r)])
      data.append(["Trigger %d:" % (j+1), "\n" + json.dumps(m, cls=utils.JSONEncoder, sort_keys=True,
                  indent=4, separators=(',', ': '))])
      
    return utils.formatTable(data)

def createFromUSBMONDump(fh):
  rom = MemoryBuffer()
  tok = " = fb"
  toklen = len(tok)
  for line in fh:
    i = line.find(tok)
    if i >= 0:
      line = line[i+toklen:].replace(" ", "")
      ptr = int(line[:4], 16)
      idx = 4
      while idx < len(line)-1:
        byte = int(line[idx:idx+2], 16)
        rom[ptr] = byte
        ptr += 1
        idx += 2
  return rom
    
if __name__ == "__main__":
  fh = sys.stdin
  if len(sys.argv) > 1:
    fh = open(sys.argv[1])

  origrom = createFromUSBMONDump(fh)
  origrom.dump(sys.stdout)

  mem = MemoryBuffer(origrom, True)
  rom = EEPROM(mem)
#  print rom.describe()
  print json.dumps(rom, cls=utils.JSONEncoder, sort_keys=True,
                   indent=2, separators=(',', ': '))

  unreadRanges = mem.describeUnread()
#  print unreadRanges
#  for (low, high) in unreadRanges:
#    print "0x%04x - 0x%04x" % (low, high)
