#pragma once

#include "Pass/Dump.hpp"
namespace SyOC {
class Tree2SSA;
class IRDump;
class CFGDump;
class IDominatorDump;
class IDFDump;
class SimplifyCFG;
class CFGAnalysis;
class SimpleAllocationElimination;
class TypeCheck;
class ConstantInitializerFold;
}

#include "ConstantInitializerFold.hpp"
#include "TypeCheck.hpp"
#include "Tree2SSA.hpp"
#include "Dump.hpp"
#include "SimplifyCFG.hpp"
#include "CFGAnalysis.hpp"
#include "SimpleAllocationElimination.hpp"