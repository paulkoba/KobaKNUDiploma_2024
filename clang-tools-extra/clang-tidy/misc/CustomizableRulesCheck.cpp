//===--- CustomizableRulesCheck.cpp - clang-tidy --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "CustomizableRulesCheck.h"
#include "yaml-cpp/yaml.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include <filesystem>
#include <iostream>
#include "llvm/Support/Casting.h"

constexpr std::size_t hashCString(const char* Str, std::size_t H = 14695981039346656037ull) {
  return (Str[0] == '\0') ? H : hashCString(&Str[1], (H ^ std::size_t(Str[0])) * 1099511628211ull);
}

#define LOG_DEBUG(msg) \
    do {               \
    } while(0);

#define LOG_DEBUG_2(msg) \
    do { \
    } while(0);

using namespace clang::ast_matchers;

namespace clang::tidy::misc {

class BaseCRNode {
public:
  enum CRNodeKind {
    CRN_Invalid,
    CRN_CRNode,
    CRN_SequenceCRNode,
    CRN_MapCRNNode,
  };

  BaseCRNode(CRNodeKind Kind, BaseCRNode* Parent) : Parent(Parent), Kind(Kind) {}
  BaseCRNode* Parent = nullptr;

  virtual void prettyPrint(int Depth = 0) {};

  virtual ~BaseCRNode() = default;

  CRNodeKind getKind() const { return Kind; }
private:
  CRNodeKind Kind = CRN_Invalid;
};

class CRNode : public BaseCRNode {
public:
  CRNode(const std::string& Value, BaseCRNode* Parent = nullptr) : BaseCRNode(CRN_CRNode, Parent), Value(Value) {};

  void prettyPrint(int Depth = 0) override {
    LOG_DEBUG(std::string(Depth * 4, ' ') << "CRNode: " << Value);
  };

  static bool classof(const BaseCRNode *S) {
    return S->getKind() == CRN_CRNode;
  }

  std::string Value;
};

class SequenceCRNode : public BaseCRNode {
public:
  SequenceCRNode(std::vector<std::unique_ptr<BaseCRNode>> Children = {}, BaseCRNode* Parent = nullptr) : BaseCRNode(CRN_SequenceCRNode, Parent), Children(std::move(Children)) {};
  void append(std::unique_ptr<BaseCRNode> Child) {
    if (!Child) {
      LOG_DEBUG_2(__FUNCTION__ << " Nullptr");
    } else {
      Children.push_back(std::move(Child));
    }
  }

  void prettyPrint(int Depth = 0) override {
    LOG_DEBUG(std::string(Depth * 4, ' ') << "SequenceCRNode:");
    for(const auto& El : Children) {
      El->prettyPrint(Depth + 1);
    }
  };

  static bool classof(const BaseCRNode *S) {
    return S->getKind() == CRN_SequenceCRNode;
  }

  std::vector<std::unique_ptr<BaseCRNode>> Children;
};

class MapCRNode : public BaseCRNode {
public:
  MapCRNode(BaseCRNode* Parent = nullptr) : BaseCRNode(CRN_MapCRNNode, Parent) {};
  void append(std::string Text, std::unique_ptr<BaseCRNode> Child) {
    if (!Child) {
      LOG_DEBUG("Nullptr");
    } else {
      Children.push_back(std::make_pair<>(Text, std::move(Child)));
    }
  }

  void prettyPrint(int Depth = 0) override {
    LOG_DEBUG(std::string(Depth * 4, ' ') << "MapCRNode:");
    for (const auto& El : Children) {
      LOG_DEBUG(std::string((Depth + 1) * 4, ' ') << El.first << ":");
      El.second->prettyPrint(Depth + 2);
    }
  };

  static bool classof(const BaseCRNode *S) {
    return S->getKind() == CRN_MapCRNNode;
  }

  std::vector<std::pair<std::string, std::unique_ptr<BaseCRNode>>> Children;
};

#include "CRAutogen.h"

std::unique_ptr<BaseCRNode> generateInternalTree(const YAML::Node &Node, BaseCRNode *Parent = nullptr) {
  if (Node.IsScalar()) {
    return std::make_unique<CRNode>(Node.as<std::string>(), Parent);
  }

  if (Node.IsSequence()) {
    SequenceCRNode RNode({}, Parent);
    for (const auto &Child : Node) {
      RNode.append(generateInternalTree(Child, &RNode));
    }
    return std::unique_ptr<BaseCRNode>(new SequenceCRNode(std::move(RNode)));
  }

  if (Node.IsMap()) {
    MapCRNode RNode(Parent);
    for (const auto &Child : Node) {
      RNode.append(Child.first.as<std::string>(), generateInternalTree(Child.second, &RNode));
    }

    return std::unique_ptr<BaseCRNode>(new MapCRNode(std::move(RNode)));
  }

  return nullptr;
}

void printNode(const YAML::Node &Node, int Indent = 0) {
  if (Node.IsScalar()) {
    LOG_DEBUG(std::string(Indent, ' ') << Node.as<std::string>());
  } else if (Node.IsSequence()) {
    LOG_DEBUG(std::string(Indent, ' ') << "-");
    for (const auto &Child : Node) {
      printNode(Child, Indent + 2);
    }
  } else if (Node.IsMap()) {
    for (const auto &Child : Node) {
      LOG_DEBUG(std::string(Indent, ' ') << Child.first.as<std::string>() << ": ");
      printNode(Child.second, Indent + 2);
    }
  }
}

template <typename T>
using CRPtrType = typename std::remove_reference<T>::type *;

#define CR_MATCHER_ARGS_0(functionName)                                  \
auto CRMake##functionName() { \
    using result_type = std::invoke_result_t<std::remove_pointer_t<decltype(&functionName)>, [](){}>; \
    return std::unique_ptr<result_type>(new result_type(std::invoke(functionName))); \
}

class CustomizableRuleCallback
    : public ast_matchers::MatchFinder::MatchCallback {
public:
  explicit CustomizableRuleCallback(CustomizableRulesCheck *RulesChecker)
      : CustomizableRulesChecker(RulesChecker) {}

  void run(const MatchFinder::MatchResult &Result) override {
    CustomizableRulesChecker->handleCustomRuleMatch(Result, this);
  }

public:
  CustomizableRulesCheck *CustomizableRulesChecker;
};

#define GENERATE_VARIABLE_ARG_LEN_MATCHER_0(name, argsType, returnType) \
returnType crMatcher

#define GENERATE_VARIABLE_ARG_LEN_MATCHER(name, argsType, returnType) \
GENERATE_VARIABLE_ARG_LEN_MATCHER_0(name, argsType, returnType)

ast_matchers::internal::Matcher<Decl> crDeclMatcher()
{

}
/*
namespace cr_autogen {

std::optional<clang::ast_matchers::internal::Matcher<Decl>> generate_functionDecl(BaseCRNode* Root);

std::optional<clang::ast_matchers::internal::Matcher<Decl>> parse_Decl(BaseCRNode* Root) {
  MapCRNode* RootAsMap = dyn_cast<MapCRNode>(Root);
  if (!RootAsMap) {
    LOG_DEBUG("Invalid configuration, expected map");
    return {};
  }

  if (RootAsMap->Children.size() != 1) {
    LOG_DEBUG("Invalid configuration, expected 1 element");
    return {};
  }

  auto& MatcherName = RootAsMap->Children[0].first;
  LOG_DEBUG("Handling matcher with name: " << MatcherName);

  switch(hashCString(MatcherName.c_str())) {
  case hashCString("functionDecl"):
    return generate_functionDecl(RootAsMap->Children[0].second.get());
  default:
    LOG_DEBUG("Unknown matcher: " << MatcherName);
    return {};
  }
}

std::optional<clang::ast_matchers::internal::Matcher<Decl>> generate_functionDecl(BaseCRNode* Root) {
  SequenceCRNode* Arguments = dyn_cast<SequenceCRNode>(Root);
  if (!Arguments) {
    LOG_DEBUG("Invalid configuration, expected sequence");
    return {};
  }
  switch (Arguments->Children.size()) {
  case 0:
    return std::optional<clang::ast_matchers::internal::Matcher<Decl>>(functionDecl());
  case 1: {
    auto Arg1 = parse_Decl(Arguments->Children[0].get());
    if (!Arg1) return {};
    return std::optional<clang::ast_matchers::internal::Matcher<Decl>>(functionDecl(Arg1.value()));
  }
  default:
    LOG_DEBUG("Too many arguments: " << Arguments->Children.size())
    return {};
  }

  binaryOperation();
}

}
*/
void CustomizableRulesCheck::generateRootMatcher(MatchFinder* Finder, BaseCRNode* Root) {
  auto RootDecl = cr_autogen::parse_Decl(Root);
  if (!RootDecl) {
    LOG_DEBUG("Was unable to parse as Decl");
    auto RootStmt = cr_autogen::parse_Stmt(Root);
    if (!RootStmt) {
      LOG_DEBUG("Was unable to parse as Stmt");
      return;
    }
    Finder->addMatcher(RootStmt.value(), MatchCallbacks.back().get());
  } else {
    Finder->addMatcher(RootDecl.value(), MatchCallbacks.back().get());
  }
}

void CustomizableRulesCheck::registerMatchers(MatchFinder *Finder) {
  registerRules(Finder);
  // callExpr(callee(functionDecl(hasName("vfork"), parameterCountIs(0))));
  /*ast_matchers::internal::Matcher<FunctionDecl> n1 = hasName("vfork");
  ast_matchers::internal::Matcher<FunctionDecl> n2 = parameterCountIs(0);
  ast_matchers::internal::Matcher<CallExpr> h = callExpr();
  auto n3 = functionDecl(n1, n2);

  // FIXME: Add matchers.
  MatchCallbacks.emplace_back(std::make_unique<CustomizableRuleCallback>(this));
  Finder->addMatcher(functionDecl().bind("x"), MatchCallbacks.back().get());

  auto n = has(n1); */


}

void CustomizableRulesCheck::check(const MatchFinder::MatchResult &Result) {
  diag("Test", DiagnosticIDs::Note);
}

void CustomizableRulesCheck::handleCustomRuleMatch(
    const ast_matchers::MatchFinder::MatchResult &Result,
    ast_matchers::MatchFinder::MatchCallback *Callback) {
  auto& Rule = CallbackToRule[Callback];
  const auto* MatchedDecl = Result.Nodes.getNodeAs<Decl>("x");
  if (MatchedDecl) {

    diag(MatchedDecl->getLocation(), Rule->Name + ": " + Rule->Description);
  } else {
    const auto* MatchedStmt = Result.Nodes.getNodeAs<Stmt>("x");
    if (MatchedStmt) {
      diag(MatchedStmt->getBeginLoc(), Rule->Name + ": " + Rule->Description);
    } else {
      diag(Rule->Name + ": " + Rule->Description);
      LOG_DEBUG("Failed to convert node both to Stmt or Decl");
    }
  }

/*  const auto *MatchedDecl = Result.Nodes.getNodeAs<FunctionDecl>("x");
  if (!MatchedDecl->getIdentifier() ||
      MatchedDecl->getName().startswith("sad_"))
    return;
  diag(MatchedDecl->getLocation(), "function %0 is insufficiently sad")
      << MatchedDecl
      << FixItHint::CreateInsertion(MatchedDecl->getLocation(), "sad_");
  diag(MatchedDecl->getLocation(), "insert 'sad'", DiagnosticIDs::Note); */
}

std::string
CustomizableRulesCheck::findRuleFileFilepath(const std::string &From) {
  std::filesystem::path CurrentPath = From;
  while (!CurrentPath.empty()) {
    std::filesystem::path TestFilePath =
        CurrentPath / ".custom-clang-tidy-rules.yaml";
    if (std::filesystem::exists(TestFilePath)) {
      return TestFilePath.string();
    }

    auto NewPath = CurrentPath.parent_path();
    if (NewPath == CurrentPath) {
      break;
    }
    CurrentPath = NewPath;
  }

  return "";
}

CustomizableRulesCheck::CustomizableRulesCheck(StringRef Name,
                                               ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context), RulesFilepath() {

  std::string T = Options.get("CustomRulesFilepath", "").str();
  RulesFilepath =
      findRuleFileFilepath(T.empty() ? Context->getCurrentBuildDirectory() : T);
  std::string RulesText = readFileToString(RulesFilepath);
  LOG_DEBUG("Current build directory: " + Context->getCurrentBuildDirectory());
  LOG_DEBUG("Current path: " + RulesFilepath);
  LOG_DEBUG("Contents: " + RulesText);

  try {
    auto Yaml = YAML::LoadFile(RulesFilepath);

    printNode(Yaml);
    auto Tree = generateInternalTree(Yaml);
    Tree->prettyPrint();
    populateRules(Tree.get());
    printRules();
  } catch (const std::exception &Ex) {
    diag("Caught exception: " + std::string(Ex.what()));
  }
}

bool CustomizableRulesCheck::populateRules(BaseCRNode* Node)
{
  MapCRNode* Root = dyn_cast<MapCRNode>(Node);
  if (!Root)
  {
    LOG_DEBUG("Expected map as a root element.");
    return false;
  }

  auto& RCh = Root->Children;
  if (RCh.size() != 1)
  {
    LOG_DEBUG("Expected root element to have 1 child.");
    return false;
  }

  auto& RRS = RCh[0];
  if (RRS.first != "rules")
  {
    LOG_DEBUG("Expected to find element with text: \"rules\"");
    return false;
  }

  SequenceCRNode* TSS = dyn_cast<SequenceCRNode>(RRS.second.get());
  if (!TSS)
  {
    LOG_DEBUG("Expected a sequence.");
    return false;
  }

  auto& TSSCh = TSS->Children;
  if (TSSCh.empty())
  {
    LOG_DEBUG("Expected to find rules, got nothing.")
  }

  for (const auto& El : TSSCh)
  {
    MapCRNode* RuleMap = dyn_cast<MapCRNode>(El.get());
    if (!RuleMap)
    {
      LOG_DEBUG("Expected map.")
      return false;
    }

    Rules.push_back(std::forward<std::unique_ptr<CRRule>>(generateRule(RuleMap)));
  }

  return true;
}

std::unique_ptr<CRRule> CustomizableRulesCheck::generateRule(MapCRNode* RuleNode)
{
  auto& Subtrees = RuleNode->Children;
  CRRule Rule{};
  for(const auto& El : Subtrees)
  {
    auto& Name = El.first;
    auto NameHash = hashCString(Name.c_str());

    switch(NameHash) {
    case hashCString("name"):
      Rule.Name = parseName(El.second.get());
      break;
    case hashCString("description"):
      Rule.Description = parseDescription(El.second.get());
      break;
    case hashCString("matcher"): {
      auto v1 = cr_autogen::parse_Stmt((El.second.get()));
      if (v1) {
        Rule.MatcherStmt.reset(
            new clang::ast_matchers::internal::Matcher<clang::Stmt>(
                v1.value()));
      } else {
        auto v2 = cr_autogen::parse_Decl((El.second.get()));
        LOG_DEBUG("Couldn't parse as Stmt, parsing as Decl...");
        if (v2) {
          Rule.MatcherDecl.reset(
              new clang::ast_matchers::internal::Matcher<clang::Decl>(
                  v2.value()));
        } else {
          LOG_DEBUG("Couldn't parse as Decl");
        }
      }
      break;
    }
    default:
      LOG_DEBUG("Unexpected node: " << NameHash);
    }
  }

  return std::make_unique<CRRule>(std::move(Rule));
}

std::string CustomizableRulesCheck::parseName(BaseCRNode* Node)
{
  CRNode* N = dyn_cast<CRNode>(Node);
  if(!N)
  {
    LOG_DEBUG("Invalid name.")
    return "";
  }

  return N->Value;
}

std::string CustomizableRulesCheck::parseDescription(BaseCRNode* Node)
{
  CRNode* N = dyn_cast<CRNode>(Node);
  if(!N)
  {
    LOG_DEBUG("Invalid description.")
    return "";
  }

  return N->Value;
}

std::unique_ptr<clang::ast_matchers::internal::Matcher<clang::Decl>> CustomizableRulesCheck::parseMatcher(BaseCRNode* Node)
{
  LOG_DEBUG("Not implemented");
  return nullptr;

//  return std::make_unique<clang::ast_matchers::internal::Matcher<clang::Decl>>(D.value());
}

void CustomizableRulesCheck::printRules()
{
  for (std::size_t I = 0; I < Rules.size(); ++I)
  {
    LOG_DEBUG("Rule #" << I + 1);
    LOG_DEBUG("Name: " << Rules[I]->Name);
    LOG_DEBUG("Description: " << Rules[I]->Description);
  }
}

template <typename T>
clang::ast_matchers::internal::Matcher<T> tryBindNormalMatcher(clang::ast_matchers::internal::Matcher<T>* orig) {
    auto bindableMatcher = llvm::dyn_cast<internal::BindableMatcher<T>>(orig);
    if (bindableMatcher) {
        bindableMatcher->bind("x");
        return *bindableMatcher;
    } else {
        return *orig;
    }
}

void CustomizableRulesCheck::registerRules(ast_matchers::MatchFinder* Finder)
{
  LOG_DEBUG("Registering " << Rules.size() << " rules.");
  for (std::size_t I = 0; I < Rules.size(); ++I) {
    MatchCallbacks.emplace_back(new CustomizableRuleCallback(this));
    CallbackToRule[MatchCallbacks.back().get()] = Rules[I].get();
    if (Rules[I].get()->MatcherStmt.get()) {
        auto d = clang::ast_matchers::internal::DynTypedMatcher(*Rules[I].get()->MatcherStmt).tryBind("x"); // ???
        //Rules[I].get()->MatcherStmt->bind("x");
        /*
         * auto matcher = Rules[I].get()->MatcherStmt.get();
         * matcher = tryBindNormalMatcher(matcher); */
      Finder->addMatcher(d ? clang::ast_matchers::internal::Matcher<Stmt>(d.value()) : *Rules[I].get()->MatcherStmt.get(), MatchCallbacks.back().get());
      LOG_DEBUG("Registered Stmt matcher...");
      continue;
    }

    if (Rules[I].get()->MatcherDecl.get()) {
        auto d = clang::ast_matchers::internal::DynTypedMatcher(*Rules[I].get()->MatcherDecl).tryBind("x"); // ???
        /*auto matcher = Rules[I].get()->MatcherDecl.get();
        *matcher = tryBindNormalMatcher(matcher);*/
      Finder->addMatcher(d ? clang::ast_matchers::internal::Matcher<Decl>(d.value()) : *Rules[I].get()->MatcherDecl.get(), MatchCallbacks.back().get());
      LOG_DEBUG("Registered Decl matcher...");
      continue;
    }
  }
}

} // namespace clang::tidy::misc