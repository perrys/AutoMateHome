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

class MacroInitiator:
  """Represents an event-based trigger for a macro chain. The trigger
  consists of a house and unit code, and zero or more clauses. When a
  matching powerline event is received and the clauses are satisfied,
  the specified macro chain will be executed."""

  def __init__(self, data, clauses):
    vars(self).update(data)
    self.clauses = clauses
    self.codelength = codec.MACRO_INITIATOR.codelength
    for c in self.clauses:
      self.codelength += c.codelength

  @classmethod
  def fromByteCode(cls, bytes, offset=0):
    """Create a macro initiator from bytecode. The code specifies the
    house/unit codes and zero or more clauses.

    The format of the bytecode is described by the MACRO_INITIATOR
    instance in the codec module."""
    
    data = codec.MACRO_INITIATOR.decode(bytes, offset)

    clauses = []
    for i in range(0, data["n_clauses"]):
      offset += 3
      clauses.append(MacroInitiatorClause.fromByteCode(bytes, offset))

    return cls(data, clauses)

  @classmethod
  def fromJSON(cls, data):
    clauses = [MacroInitiatorClause.fromJSON(c) for c in data["clauses"]]
    data = codec.MACRO_INITIATOR.createDataMap(data)
    return cls(data, clauses)
    
  def toByteCode(self):
    bytes = codec.MACRO_INITIATOR.encode(vars(self))
    for c in self.clauses:
      bytes.extend(c.toByteCode())
    return bytes

  def toJSON(self):
    d = dict()
    utils.merge_dict_if_not_default(vars(self), d, "reserved", 0)
    utils.merge_dict_if_not_default(vars(self), d, "clauses", None)
    d["house_code"]  = codec.VALUE_TO_HOUSECODE_MAP[self.house_code]
    d["unit_code"] = codec.VALUE_TO_UNITCODE_MAP[self.unit_code]
    d["trigger_on_off"] = self.trigger_on_off and "on" or "off"
    d["macro_id"] = self.macro_ptr
    utils.convert_key_to_hex_string(d, "macro_id")
    return d

class UnsuitableDataException(Exception):
  pass

class MacroInitiatorClause(object):
  """Base class for a clause in a macro initiator."""

  @staticmethod
  def fromJSON(data):
    """Create a macro clause of the correct subtype"""

    for cls in [MacroInitiatorClauseTimeType, 
                MacroInitiatorClauseDateType, 
                MacroInitiatorClauseDayType, 
                MacroInitiatorClauseModulesType, 
                MacroInitiatorClauseFlagsType]:
      try:
        return cls(data=data)
      except UnsuitableDataException:
        pass    
    raise Exception("unable to create a macro clause from: %s" % data)

  @staticmethod
  def fromByteCode(bytes, offset=0):
    """Create a macro clause of the correct subtype"""

    # hack to avoid double read in debug mode:
    if isinstance(bytes, utils.MemoryBuffer):
      header_byte = [bytes.peek(offset)]
    else:
      header_byte = bytes[offset:offset+1]

    # we need to peek at the type byte to figure out what kind of
    # clause to create:
    attribs = codec.MACRO_INITIATOR_CLAUSE_HEADER.decode(header_byte)
    clauseType = attribs["type"]
    
    ctr = CLAUSE_TYPE_DISPLATCH_TABLE.get(clauseType)
    if ctr is None:
      raise Exception("unknown macro initiator clause type: 0x%0x" % clauseType)

    return ctr(bytes, offset)

  def __init__(self, bytes=None, offset=0, data=None):
    """Base class constructor for the macro initiator clauses, which
    have a common header, described by the
    MACRO_INITIATOR_CLAUSE_HEADER instance in the codec
    module. Relevant data in the header are the logic op (i.e. "and"
    or "or") and the comparison op (less, greater, equal, not)."""

    if bytes is not None:
      data = codec.MACRO_INITIATOR_CLAUSE_HEADER.decode(bytes, offset)
    else:
      assert(data is not None)
      def substitueIndexOf(key, l):
        data[key] = l.index(data[key])
      substitueIndexOf("logic_op", codec.MACRO_INITIATOR_HEADER_LOGIC_OPS)
      substitueIndexOf("compare_op", codec.MACRO_INITIATOR_HEADER_COMPARE_OPS)
      data = codec.MACRO_INITIATOR_CLAUSE_HEADER.createDataMap(data)
    vars(self).update(data)

  def toByteCode(self):
    return codec.MACRO_INITIATOR_CLAUSE_HEADER.encode(vars(self))

  def toJSON(self):
    l = {"logic_op": codec.MACRO_INITIATOR_HEADER_LOGIC_OPS[self.logic_op],
         "compare_op": codec.MACRO_INITIATOR_HEADER_COMPARE_OPS[self.compare_op]}
    utils.merge_dict_if_not_default(vars(self), l, "hdr_reserved_1", 0)
    utils.merge_dict_if_not_default(vars(self), l, "hdr_reserved_2", 0)
    return l

class MacroInitiatorClauseTimeType(MacroInitiatorClause):
  """Clause specifying a time, which is compared with the current time
  using the compare operator. Note that the code supports special time
  codes for sunrise and sunset, which are obviously variable from day
  to day and depend on geographical location.""" 

  def __init__(self, bytes=None, offset=0, data=None):
    if bytes is not None:
      super(self.__class__, self).__init__(bytes, offset)
      data = codec.MACRO_INITIATOR_CLAUSE_BODY_TIME_TYPE.decode(bytes, 1+offset)
    else:
      data = dict(data)
      super(self.__class__, self).__init__(data=data)
      if "time" not in data:
        raise UnsuitableDataException()
      time = data.get("time")
      if time.lower() == "sunrise":
        data["is_variable"] = 1
        data["sunrise"] = 1
      elif time.lower() == "sunset":
        data["is_variable"] = 1
        data["sunset"] = 1
      else:
        (data["double_hour"], data["min"]) = utils.timespec_to_x10_time(time)
      data = codec.MACRO_INITIATOR_CLAUSE_BODY_TIME_TYPE.createDataMap(data)

    vars(self).update(data)
    self.codelength = codec.MACRO_INITIATOR_CLAUSE_HEADER.codelength + codec.MACRO_INITIATOR_CLAUSE_BODY_TIME_TYPE.codelength

  def toByteCode(self):
    bytes = super(self.__class__, self).toByteCode()
    bytes.extend(codec.MACRO_INITIATOR_CLAUSE_BODY_TIME_TYPE.encode(vars(self)))
    return bytes
                 
  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    time = None
    if self.is_variable:
      if self.sunrise:
        time = "Sunrise"
      else:
        time = "Sunset"
    else:
      time = utils.x10_time_to_string(self.double_hour, self.min)
    l["time"] = time
    utils.merge_dict_if_not_default(vars(self), l, "reserved", 0)
    return l

class MacroInitiatorClauseDateType(MacroInitiatorClause):
  """Clause specifying a day of the year, which is compared with the
  current day of the year using the compare operator. Note that the
  year day is converted to a date for display/readability but this
  will be different for dates after 28th February on yeap years."""

  def __init__(self, bytes=None, offset=0, data=None):
    if bytes is not None:
      super(self.__class__, self).__init__(bytes, offset)
      data = codec.MACRO_INITIATOR_CLAUSE_BODY_DATE_TYPE.decode(bytes, 1+offset)
    else:
      data = dict(data)
      super(self.__class__, self).__init__(data=data)
      if "year_day" not in data:
        if "date" in data:
          data["year_day"] = utils.datestring_to_x10_year_day(data["date"])
        else:
          raise UnsuitableDataException()
      data = codec.MACRO_INITIATOR_CLAUSE_BODY_DATE_TYPE.createDataMap(data)
    vars(self).update(data)
    self.codelength = codec.MACRO_INITIATOR_CLAUSE_HEADER.codelength + codec.MACRO_INITIATOR_CLAUSE_BODY_DATE_TYPE.codelength
      
  def toByteCode(self):
    bytes = super(self.__class__, self).toByteCode()
    bytes.extend(codec.MACRO_INITIATOR_CLAUSE_BODY_DATE_TYPE.encode(vars(self)))
    return bytes

  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    utils.merge_dict_if_not_default(vars(self), l, "year_day", None)
    l["date"] = utils.x10_year_day_to_string(self.year_day)
    utils.merge_dict_if_not_default(vars(self), l, "reserved", 0)
    return l

class MacroInitiatorClauseDayType(MacroInitiatorClause):
  """Clause specifying one or more days of the week. Only the "equals"
  and "not" comparison operators are valid for this type of clause."""

  def __init__(self, bytes=None, offset=0, data=None):
    if bytes is not None:
      super(self.__class__, self).__init__(bytes, offset)
      data = codec.MACRO_INITIATOR_CLAUSE_BODY_DAY_TYPE.decode(bytes, 1+offset)
    else:
      data = dict(data)
      if "week_day_mask" not in data:
        raise UnsuitableDataException()
      data["week_day_mask"] = utils.string_to_week_mask(data["week_day_mask"])
      data = codec.MACRO_INITIATOR_CLAUSE_BODY_DAY_TYPE.createDataMap(data)
    vars(self).update(data)
    self.codelength = codec.MACRO_INITIATOR_CLAUSE_HEADER.codelength + codec.MACRO_INITIATOR_CLAUSE_BODY_DAY_TYPE.codelength

  def toByteCode(self):
    bytes = super(self.__class__, self).toByteCode()
    bytes.extend(codec.MACRO_INITIATOR_CLAUSE_BODY_DAY_TYPE.encode(vars(self)))
    return bytes

  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    l["week_day_mask"] = utils.week_mask_to_string(self.week_day_mask)
    utils.merge_dict_if_not_default(vars(self), l, "reserved_1", 0)
    utils.merge_dict_if_not_default(vars(self), l, "reserved_2", 0)
    return l

class MacroInitiatorClauseFlagsType(MacroInitiatorClause):
  def __init__(self, bytes=None, offset=0, data=None):
    if bytes is not None:
      super(self.__class__, self).__init__(bytes, offset)
      mask = 0x100 * bytes[1+offset] + bytes[2+offset]
      self.flags = [i for i in range(15, -1, -1) if (1 & mask >> i)]
    else:
      if "flags" not in data or "criteria" not in data:
        raise UnsuitableDataException()
      if data["criteria"].lower() == "on":
        data["type"] = CLAUSE_TYPE_FLAGS_ON
      else:
        data["type"] = CLAUSE_TYPE_FLAGS_OFF
      vars(self).update(data)
    self.codelength = codec.MACRO_INITIATOR_CLAUSE_HEADER.codelength + 2

  def toByteCode(self):
    bytes = super(self.__class__, self).toByteCode()
    flagmask = utils.unmerge_bytes(utils.numbers_to_mask(self.flags), 2)
    bytes.extend(flagmask)
    return bytes
                  
  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    l["criteria"] = self.type == CLAUSE_TYPE_FLAGS_ON and "on" or "off"
    utils.merge_dict_if_not_default(vars(self), l, "flags", None)
    return l

class MacroInitiatorClauseModulesType(MacroInitiatorClause):
  def __init__(self, bytes=None, offset=0, data=None):
    if bytes is not None:
      super(self.__class__, self).__init__(bytes, offset)
      mask = 0x100 * bytes[1] + bytes[2]
      self.modules = [codec.UNITCODE_TO_VALUE_MAP[i] for i in range(15, -1, -1) if (1 & (mask >> i))]
    else:
      if "modules" not in data or "criteria" not in data:
        raise UnsuitableDataException()
      if data["criteria"].lower() == "on":
        data["type"] = CLAUSE_TYPE_MODULES_ON
      else:
        data["type"] = CLAUSE_TYPE_MODULES_OFF
      vars(self).update(data)
    self.codelength = codec.MACRO_INITIATOR_CLAUSE_HEADER.codelength + 2

  def toByteCode(self):
    bytes = super(self.__class__, self).toByteCode()
    coded_modules = [codec.VALUE_TO_UNITCODE_MAP[i] for i in self.modules]
    modmask = utils.unmerge_bytes(utils.numbers_to_mask(coded_modules), 2)
    bytes.extend(modmask)
    return bytes
                  
  def toJSON(self):
    l = super(self.__class__, self).toJSON()
    l["Criteria"] = self.type == CLAUSE_TYPE_MODULES_ON and "on" or "off"
    utils.merge_dict_if_not_default(vars(self), l, "modules", None)

CLAUSE_TYPE_DISPLATCH_TABLE = {
  CLAUSE_TYPE_TIME        : MacroInitiatorClauseTimeType,
  CLAUSE_TYPE_DATE        : MacroInitiatorClauseDateType,
  CLAUSE_TYPE_DAY         : MacroInitiatorClauseDayType,
  CLAUSE_TYPE_MODULES_ON  : MacroInitiatorClauseModulesType,
  CLAUSE_TYPE_MODULES_OFF : MacroInitiatorClauseModulesType,
  CLAUSE_TYPE_FLAGS_ON    : MacroInitiatorClauseFlagsType,
  CLAUSE_TYPE_FLAGS_OFF   : MacroInitiatorClauseFlagsType,
  }


class TimerInitiator:
  """Represents a timer initiator in the EEPROM - i.e, a point-in-time
  trigger for a macro chain. This is simpler to describe than a macro
  initiator as it is a time-based spec without other conditional
  parts.

  Note that timer initiators have two events and macro chains
  associated with them (start and stop).
  """

  def __init__(self, data):
    vars(self).update(data)
    self.codelength = codec.TIMER_INITIATOR.codelength

  @classmethod
  def fromByteCode(cls, bytes, offset=0):
    """Create a timer initiator from bytecode."""
    return cls(codec.TIMER_INITIATOR.decode(bytes, offset))

  def toByteCode(self):
    return codec.TIMER_INITIATOR.encode(vars(self))

  @classmethod
  def fromJSON(cls, data):
    """Create a timer initiator from a JSON-decoded struture."""
    if "begin_year_day" not in data and "begin_date" in data:
      data["begin_year_day"] = utils.datestring_to_x10_year_day(data["begin_date"])
    if "end_year_day" not in data and "end_date" in data:
      data["end_year_day"] = utils.datestring_to_x10_year_day(data["end_date"])
    def setTimes(key):
      timeSpec = data.get("%(key)s_time" % locals())
      # TODO: test null start/stop times
      if timeSpec is not None:
        data["%(key)s_double_hour" % locals()], data["%(key)s_min" % locals()] = utils.timespec_to_x10_time(timeSpec, True)
    setTimes("start")
    setTimes("stop")
    data["week_day_mask"] = utils.string_to_week_mask(data["week_day_mask"])
          
    return cls(codec.TIMER_INITIATOR.createDataMap(data))

  def toJSON(self):
    t = vars(self)
    tijson = utils.merge_dict(t, None, "start_macro_ptr", "stop_macro_ptr", "start_macro_id", "stop_macro_id")
    utils.convert_key_to_hex_string(tijson, "start_macro_ptr", "stop_macro_ptr", "start_macro_id", "stop_macro_id")
    if utils.merge_dict_if_not_default(t, tijson, "begin_year_day", 0):
      tijson["begin_date"] = utils.x10_year_day_to_string(t["begin_year_day"])
    if utils.merge_dict_if_not_default(t, tijson, "end_year_day", 367):
      tijson["end_date"] = utils.x10_year_day_to_string(t["end_year_day"])
    utils.merge_dict_if_not_default(t, tijson, "start_security", 0)
    utils.merge_dict_if_not_default(t, tijson, "stop_security", 0)
    tijson["start_time"] = utils.x10_time_to_string(t["start_double_hour"], t["start_min"])
    tijson["stop_time"] = utils.x10_time_to_string(t["stop_double_hour"], t["stop_min"])
    tijson["week_day_mask"] = utils.week_mask_to_string(t["week_day_mask"])
    return tijson

                      

class MacroChain:
  """A set of macros to be executed in response to either a macro initiator or a timer initiator."""

  def __init__(self, data):
    vars(self).update(data)

  @classmethod
  def fromByteCode(cls, bytes, offset=0):
    data = dict()
    data["id"] = offset
    offset += 1
    data.update(codec.MACRO_CHAIN_HEADER.decode(bytes, offset))
    offset += codec.MACRO_CHAIN_HEADER.codelength
    data["elements"] = []
    for i in range(0, data["n_elements"]):
      d = codec.MACRO_COMMON.decode(bytes, offset)
      data["elements"].append(d)
      offset += codec.MACRO_COMMON.codelength
      func = d["function_code"]
      if func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["dim"] or \
            func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["bright"]:
        d.update(codec.MACRO_BRIGHT_DIM_SUFFIX.decode(bytes, offset))
        offset += codec.MACRO_BRIGHT_DIM_SUFFIX.codelength
      elif func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["extended_command"]:
        d.update(codec.MACRO_EXTENDED_CMD_SUFFIX.decode(bytes, offset))
        offset += codec.MACRO_EXTENDED_CMD_SUFFIX.codelength
    return cls(data)

  def toByteCode(self):
    bytes = codec.MACRO_CHAIN_HEADER.encode(vars(self))
    for d in self.elements:
      bytes.extend(codec.MACRO_COMMON.encode(d))
      func = d["function_code"]
      if func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["dim"] or \
            func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["bright"]:
        bytes.extend(codec.MACRO_BRIGHT_DIM_SUFFIX.encode(d))
      elif func == codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP["extended_command"]:
        bytes.extend(codec.MACRO_EXTENDED_CMD_SUFFIX.encode(d))
    return bytes

  @classmethod
  def fromJSON(cls, jsondata):
    data = codec.MACRO_CHAIN_HEADER.createDataMap(jsondata)
    data["id"] = int(jsondata["id"], 16)
    elements = data["elements"] = []
    for e in jsondata["elements"]:
      elements.append(e)
      maskMap = codec.DEVICECODE_MASK.createDataMap()
      for u in e["units"]:
        maskMap[u] = 1
      e["unit_bitmap_hi"], e["unit_bitmap_lo"] = codec.DEVICECODE_MASK.encode(maskMap)
      e["function_code"] = codec.UNDERSCORED_LOWERCASE_FUNCTION_TO_CODE_MAP[e["function_code"].replace(" ","_").lower()]
      e["house_code"] = codec.HOUSECODE_TO_VALUE_MAP[e["house_code"]]
    return cls(data)

  def toJSON(self):
    l = utils.merge_dict(vars(self), None, "delay_secs", "id")
    utils.convert_key_to_hex_string(l, "id")
#    l["reserved"] = utils.to_binary_string(self.reserved_1)
    elements = l["elements"] = []
    for d in self.elements:
      units = d["unit_bitmap_hi"], d["unit_bitmap_lo"]
      d["units"] = [k for k,v in codec.DEVICECODE_MASK.decode(units).iteritems() if v == 1]
      del d["unit_bitmap_lo"]
      del d["unit_bitmap_hi"]
      d["function_code"] = codec.FUNCTIONS_ZERO_OFFSET[d["function_code"]]
      d["house_code"] = codec.VALUE_TO_HOUSECODE_MAP[d["house_code"]]
      elements.append(d)
    return l


class EEPROM:

  def __init__(self, data):
    vars(self).update(data)

  def toJSON(self):
    d = utils.merge_dict(vars(self), None)
    d["dst_data"]["begin_date"] = utils.x10_year_day_to_string(self.dst_data["begin_year_day"])
    d["dst_data"]["end_date"] = utils.x10_year_day_to_string(self.dst_data["end_year_day"])
    def processDawnDusk(l):
      da = {"rise": utils.x10_time_to_string(l["start_double_hour"], l["start_min"]), \
                              "set": utils.x10_time_to_string(l["stop_double_hour"], l["stop_min"])}
      utils.merge_dict_if_not_default(l, da, "reserved_1", 0)
      utils.merge_dict_if_not_default(l, da, "reserved_2", 0)
      return da
    sunrise_sunset_list = [processDawnDusk(l) for l in d["sunrise_sunset_times"]]
    d["sunrise_sunset_times"] = sunrise_sunset_list
    return d

  @classmethod
  def fromJSON(cls, jsonData):
    data = dict()
    utils.merge_dict_if_not_default(jsonData, data, "dst_data", None)
    def processArray(key, clz):
      data[key] = [clz.fromJSON(m) for m in jsonData[key]]
    processArray("macro_chains", MacroChain)
    processArray("macro_initiators", MacroInitiator)
    processArray("timer_initiators", TimerInitiator)
    def processDawnDusk(l):
      d = codec.DAWN_DUSK_ENTRY.createDataMap()
      utils.merge_dict_if_not_default(l, d, "reserved_1", 0)
      utils.merge_dict_if_not_default(l, d, "reserved_2", 0)
      d["start_double_hour"], d["start_min"] = utils.timespec_to_x10_time(l["rise"])
      d["stop_double_hour"],  d["stop_min"]  = utils.timespec_to_x10_time(l["set"])
      return d
    data["sunrise_sunset_times"] = [processDawnDusk(l) for l in jsonData["sunrise_sunset_times"]]
    return cls(data)

  @classmethod
  def fromByteCode(cls, memoryBuffer):
    macro_initiator_table_offset = utils.merge_bytes(memoryBuffer[0:2])
    sunrise_sunset_table_offset = utils.merge_bytes(memoryBuffer[2:4])

    self = cls(dict())
    self.sunrise_sunset_resolution = memoryBuffer[4]
    self.dst_data = codec.DST_DAYS.decode(memoryBuffer, 5)
    tranceivedHousecodes = codec.HOUSECODE_MASK.decode(memoryBuffer, 9)
    self.tranceivedHousecodes = [k for k,v in tranceivedHousecodes.iteritems() if v != 0]
    self.timer_initiators = []

    ptr = 0x19
    first_indirect_ptr = 0xffff
    while (macro_initiator_table_offset - ptr) > codec.TIMER_INITIATOR.codelength:
      t = TimerInitiator.fromByteCode(memoryBuffer, ptr)
      self.timer_initiators.append(t)
      for p in t.start_macro_ptr, t.stop_macro_ptr:
        if p > 0:
          first_indirect_ptr = min(first_indirect_ptr, p)
      ptr += t.codelength
      ptr += 1
      if (first_indirect_ptr - ptr) < codec.TIMER_INITIATOR.codelength: 
        break

    ptr = macro_initiator_table_offset+1
    self.macro_initiators = []
    while memoryBuffer.peek(ptr+2) != 0xff:
      mi = MacroInitiator.fromByteCode(memoryBuffer, ptr)
      self.macro_initiators.append(mi)
      ptr += mi.codelength
      ptr += 1
      
    self.macro_chains = []
    for m in self.macro_initiators:
      self.macro_chains.append(MacroChain.fromByteCode(memoryBuffer, m.macro_ptr))

    recorded_indirects = {}
    recorded_macros = {}

    def getMacroFromIndirect(offset):
      if offset > 0:
        indirect_ptr = recorded_indirects.get(offset)
        if indirect_ptr is None:
          indirect_ptr = utils.merge_bytes(memoryBuffer[offset+1:offset+3])
          recorded_indirects[offset] = indirect_ptr
        recorded_indirects[offset] = indirect_ptr
        return indirect_ptr
      return None

    for i,t in enumerate(self.timer_initiators):
      for k in ["start_macro_ptr", "stop_macro_ptr"]:
        m_ptr = getMacroFromIndirect(vars(t)[k])
        if m_ptr is not None:
          vars(t)[k.replace("_ptr", "_id")] = m_ptr
          m = recorded_macros.get(m_ptr)
          if m is None:
            m = MacroChain.fromByteCode(memoryBuffer, m_ptr)
            recorded_macros[m_ptr] = m
            self.macro_chains.append(m)

    end_day = 0
    ptr = sunrise_sunset_table_offset
    self.sunrise_sunset_times = []
    while end_day < 366 :
      sunrise_sunset_time = codec.DAWN_DUSK_ENTRY.decode(memoryBuffer, ptr)
      self.sunrise_sunset_times.append(sunrise_sunset_time)
      ptr += codec.DAWN_DUSK_ENTRY.codelength
      end_day += self.sunrise_sunset_resolution

    return self

  def toByteCode(self):
    """Convert the timers, macro initiators, macros, DST data,
    sunrise/sunset data and everything else described by this class
    into bytecode suitable for uploading to the CM15."""
    mem = utils.MemoryBuffer(capacity=0x2000)

    assert(0 == (self.sunrise_sunset_resolution >> 8))
    mem[4] = self.sunrise_sunset_resolution & 0xFF
    
    bytes = codec.DST_DAYS.encode(self.dst_data)
    mem.setFromByteArray(5, bytes)

    tranceivedHousecodeMap = dict([(k, 1) for k in self.tranceivedHousecodes])
    for i in range(0,16):
      c = chr(ord('A')+i)
      if c not in tranceivedHousecodeMap:
        tranceivedHousecodeMap[c] = 0
    mem.setFromByteArray(9, codec.HOUSECODE_MASK.encode(tranceivedHousecodeMap))

    # TODO: data between 0x0b and 0x18 not currently understood

    ptr = 0x18

    # the eeprom download seems to contain a lot of single bytes holding
    # the least-significant 8 bits of their address. Presumably this
    # was just a default value fill of the buffer, although these
    # markers do make the rom code a bit easier for a human to read 
    def setLowOrderAddressByte():
      mem[ptr] = ptr & 0xFF
      return ptr + 1
    
    ptr = setLowOrderAddressByte()

    # write the timer initiators. Timers refer to start and stop macro
    # chains, but the addresses of the macros are indirected through a
    # set of 2-byte pointers stored in memory after the timer
    # initiators. My guess is that this is to get around the 10-bit
    # address limitation of the codec, as this scheme allows macros to
    # be placed anywhere in the 16-bit address space while also
    # allowing much of the original eeprom hardware from the CM12 to
    # be reused.
    indirect_table = dict()
    indirect_table_offset = ptr + (len(self.timer_initiators) * codec.TIMER_INITIATOR.codelength)
    indirect_table_offset = utils.alignToBoundary(indirect_table_offset, 4)
    for t in self.timer_initiators:
      hasMacroId = False
      for k in ("start_macro_id", "stop_macro_id"):
        if k in vars(t):
          hasMacroId = True
          id = vars(t)[k]
          indirectPtr = indirect_table.get(id)
          if indirectPtr is None:
            offset = indirect_table_offset + (2 * len(indirect_table))
            indirectPtr = indirect_table[id] = offset
          t.start_macro_ptr = indirectPtr
      assert(hasMacroId)
      bytes = t.toByteCode()
      ptr = mem.setFromByteArray(ptr, bytes)
      ptr = setLowOrderAddressByte()

    # A few bytes here which are not well understood - seems to
    # contain address boundary of the macros:
    ptr = mem.setFromByteArray(ptr, [0x00])
    ptr = setLowOrderAddressByte()
    macro_limits_offset = ptr # need to  write the macro end address here 
    ptr += 2 

    # now set the offset at the start of the eeprom
    macro_initiator_table_offset = ptr + (3 * len(indirect_table))
    mem.setFromByteArray(0, utils.unmerge_bytes(macro_initiator_table_offset, 2))
   
    # write the macro initiators:
    ptr = macro_initiator_table_offset
    for m in self.macro_initiators:
      ptr = setLowOrderAddressByte()
      ptr = mem.setFromByteArray(ptr, m.toByteCode())

    ptr = setLowOrderAddressByte()
    ptr = mem.setFromByteArray(ptr, [0xff, 0xff, 0xff])

    # now write the actual macro chains:
    for m in self.macro_chains:
      ptr = setLowOrderAddressByte()
      ptr = mem.setFromByteArray(ptr, m.toByteCode())

    # go back and write the end address:
    end_of_macros_address = ptr    
    mem.setFromByteArray(macro_limits_offset, utils.unmerge_bytes(end_of_macros_address, 2))

    ptr = setLowOrderAddressByte()
    ptr = mem.setFromByteArray(ptr, [0x00])

    # fill the rest of this row and the next with 0xffs:
    fill_address = utils.alignToBoundary(ptr, 16)
    fill_address += 16
    ptr = mem.setFromByteArray(ptr, [0xff for i in range(ptr, fill_address)])

    # now write the sunrise/sunset times:
    ptr = len(mem) - 12
    for sunrise_sunset_time in reversed(self.sunrise_sunset_times):
      ptr -= codec.DAWN_DUSK_ENTRY.codelength
      mem.setFromByteArray(ptr, codec.DAWN_DUSK_ENTRY.encode(sunrise_sunset_time))

    # write this offset into byte 2:
    start_of_sunrise_sunset_table = ptr
    mem.setFromByteArray(2, utils.unmerge_bytes(start_of_sunrise_sunset_table, 2))

    # TODO: 12 bytes at the end of the eeprom not currently understood

    return mem


def createFromUSBMONDump(fh):
  rom = utils.MemoryBuffer()
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
#  origrom.dump(sys.stdout)

  mem = utils.MemoryBuffer(origrom, True)
  rom = EEPROM.fromByteCode(mem)
#  print rom.describe()
  s = json.dumps(rom, cls=utils.JSONEncoder, sort_keys=True,
                   indent=2, separators=(',', ': '))
  print s
  jdata = json.loads(s)
  rom2 = EEPROM.fromJSON(jdata)
  s2 = json.dumps(rom2, cls=utils.JSONEncoder, sort_keys=True,
                   indent=2, separators=(',', ': '))
  print s2

#  rom.toByteCode().dump(sys.stdout)

  # unreadRanges = mem.describeUnread()
  # print unreadRanges
  # for (low, high) in unreadRanges:
  #   print "0x%04x - 0x%04x" % (low, high)
#  mem.dump(sys.stdout)
