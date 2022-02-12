#pragma once

class BBPredSuccAnalysis;
class BasicBlockTraversalAnalysis;
class ConstantInitializerFold;
class IDominatorAnalysis;
class IRDump;
class CFGDump;
class IDominatorDump;
class SimplifyCFG;
class Tree2SSA;
class TypeCheck;
class UseAnalysis;

#include "BBPredSuccAnalysis.hpp"
#include "BasicBlockTraversalAnalysis.hpp"
#include "ConstantInitializerFold.hpp"
#include "IDominatorAnalysis.hpp"
#include "Dump.hpp"
#include "SimplifyCFG.hpp"
#include "Tree2SSA.hpp"
#include "TypeCheck.hpp"
#include "UseAnalysis.hpp"
#include "ValuePoolCompact.hpp"
#include "SimpleAllocationElimination.hpp"
#include "PromoteMemToReg.hpp"
