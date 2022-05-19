#include "Instructions.hpp"

std::string to_string(Instr &instr) {
  return std::visit(
      overloaded{
          [&](Push<int64_t> &val) { return fmt::format("PushI {}", val.val); },
          [&](Push<double> &val) { return fmt::format("PushD {}", val.val); },
          [&](Push<bool> &val) { return fmt::format("PushB {}", val.val); },
          [&](Push<char> &val) { return fmt::format("PushC {}", val.val); },
          [&](Push<std::string> &val) {
            return fmt::format("PushS {}", val.val);
          },
          [&](Load &val) { return fmt::format("Load {}", val.offset); },
          [&](Ref &val) { return fmt::format("Ref {}", val.offset); },
          [&](BinOp &val) { return fmt::format("BinOp \"{}\"", val.op); },
          [&](PrefixOp &val) { return fmt::format("PrefixOp \"{}\"", val.op); },
          [&](Print &) { return fmt::format("Print"); },
          [&](Ret &) { return fmt::format("Ret"); },
          [&](Call &val) { return fmt::format("Call {}", val.label); },
          [&](Jmp &val) { return fmt::format("Jmp {}", val.offset); },
          [&](Jnz &val) { return fmt::format("Jnz {}", val.offset); },
          [&](const Lbl &val) {
            return fmt::format("Label {}", val.label_name);
          },
          [&](Break &) { return fmt::format("Break"); },
          [&](Continue &) { return fmt::format("Continue"); }},
      instr);
}
