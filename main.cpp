#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ffi.h>
#include <map>
#include <set>
#include <string>

struct AssertRaiser {
  AssertRaiser(bool condition, char const* file, int line, char const* msg) {
    if (!condition) {
      fprintf(stderr, "%s(%d): Assertion failed: %s\n", file, line, msg);
      exit(1);
    }
  }
};

#define STRINGIZE_DETAIL(x) #x
#define STRINGIZE(x) STRINGIZE_DETAIL(x)
#define ASSERT_OBJ_NAME_DETAIL(a, b) a##b
#define ASSERT_OBJ_NAME(a, b) ASSERT_OBJ_NAME_DETAIL(a, b)
#define ASSERT(x) AssertRaiser ASSERT_OBJ_NAME(assertObj, __LINE__)((x), __FILE__, __LINE__, #x);

struct Cell {
  Cell(void* head, void* tail): head(head), tail(tail) {}
  void* head;
  void* tail;
};

Cell* make_cell(void* head, void* tail) {
  using namespace std;

  typedef std::map<std::pair<void*, void*>, Cell*> CellMap;
  static CellMap cell_map;
  CellMap::iterator pos = cell_map.find(make_pair(head, tail));
  if (pos == cell_map.end()) {
    Cell* cell = new Cell(head, tail);
    pos = cell_map.insert(make_pair(make_pair(head, tail), cell)).first;
  }

  return (*pos).second;
}

ASSERT(make_cell(0, make_cell(0, 0)) == make_cell(0, make_cell(0, 0)));

char* intern(char const* s) {
  using namespace std;

  typedef std::set<std::string> SymMap;
  static SymMap symbol_map;
  string str(s);
  SymMap::iterator pos = symbol_map.find(str);
  if (pos == symbol_map.end()) {
    pos = symbol_map.insert(str).first;
  }

  return (char*)(*pos).c_str();
}

char tag_cons[10] = "cons";

void* cons(void* head, void* tail) {
  return make_cell(
      tag_cons,
      make_cell(head, tail));
}

bool consp(void* x) {return ((Cell*)x)->head == tag_cons;}

void* car(void* p) {
  ASSERT(consp(p)); 
  void* data = ((Cell*)p)->head;
  return ((Cell*)data)->head;
}

void* cdr(void* p) {
  ASSERT(consp(p)); 
  void* data = ((Cell*)p)->head;
  return ((Cell*)data)->tail;
}

void* pop_arg(void*& ls) {
  ASSERT(consp(ls));
  void* x = car(ls);
  ls = cdr(ls);
  return x;
}

void* pop_cons(void*& ls) {
  void* x = pop_arg(ls);
  ASSERT(consp(x));
  return x;
}

char tag_symbol[10] = "symbol";

void* symbol(char const* text) {
  return make_cell(tag_symbol, intern(text));
}

bool symbolp(void* x) {return ((Cell*)x)->head == tag_symbol;}

void* pop_symbol(void*& ls) {
  void* x = pop_arg(ls);
  ASSERT(symbolp(x));
  return x;
}

void load_functions(std::map<void*, void*>& functions, void* code) {
  void* module = code;
  while (module != 0) {
    void* fundef = pop_cons(module);
    void* name = pop_symbol(fundef);
    void* parms = pop_cons(fundef);
    void* code = pop_cons(fundef);
    ASSERT(fundef == 0);

    functions.insert(std::make_pair(name, cons(parms, code)));
  }
}

struct ExecutionContext {
  ExecutionContext(std::map<void*, void*>& functions):
    functions(functions), nextTmpIdx(1) {}

  std::map<void*, void*>& functions;
  std::map<void*, void*> bindings;
  int nextTmpIdx;
};

void execute_expression(ExecutionContext& ctx, void* statement) {
  ASSERT(consp(statement));
}

void execute_statements(ExecutionContext& ctx, void* statements) {
  void* remaining_statements = statements;
  while (remaining_statements != 0) {
    void* statement = pop_arg(remaining_statements);

    // TODO: check special statements.
    execute_expression(ctx, statement);
  }
}

char exe_type_int[10] = "type_int";
char exe_type_ptr[10] = "type_ptr";

void* copy_value(void* parm_type, void* value) {
  size_t size;
  if (parm_type == exe_type_int) {
    size = sizeof(int);
  } else if (parm_type == exe_type_ptr) {
    size = sizeof(void*);
  } else {
    ASSERT(0);
    size = 0;
  }

  void* new_value = new char[size];
  memcpy(new_value, value, size);
  return new_value;
}

void destroy_value(void* /*parm_type*/, void* value) {
  delete [] (char*) value;
}

void add_binding(ExecutionContext& ctx, void* parm_name, void* value) {
  ctx.bindings.insert(std::make_pair(parm_name, value));
}

void* pop_binding(ExecutionContext& ctx, void* parm_name) {
  std::map<void*, void*>::iterator value_pos = ctx.bindings.find(parm_name);
  ASSERT(value_pos != ctx.bindings.end());
  void* value = (*value_pos).second;
  ctx.bindings.erase(value_pos);
  return value;
}

void execute_function(std::map<void*, void*>& functions, void* fn_name, void* args) {
  std::map<void*, void*>::iterator function_pos = functions.find(fn_name);
  ASSERT(function_pos != functions.end());
  void* fn = (*function_pos).second;
  void* parms = pop_cons(fn);
  void* code = pop_cons(fn);

  ExecutionContext ctx(functions);
  void* remaining_parms = parms;
  while (remaining_parms != 0) {
    void* parm = pop_symbol(remaining_parms);
    void* parm_name = pop_symbol(parm);
    void* parm_type = pop_symbol(parm);
    ASSERT(parm == 0);

    void* arg = pop_arg(args);

    add_binding(ctx, parm_name, copy_value(parm_type, arg));
  }
  ASSERT(args == 0);

  execute_statements(ctx, code);

  // Clean up args.
  // TODO: Remove in reverse order.
  remaining_parms = parms;
  while (remaining_parms != 0) {
    void* parm = pop_symbol(remaining_parms);
    void* parm_name = pop_symbol(parm);
    void* parm_type = pop_symbol(parm);
    ASSERT(parm == 0);

    void* arg = pop_binding(ctx, parm_name);
    destroy_value(parm_type, arg);
  }

  ASSERT(ctx.bindings.empty());
}

void execute(void* code) {
  std::map<void*, void*> functions;
  load_functions(functions, code);

  execute_function(functions, symbol("main"), 0);
}

int main() {
  ffi_cif cif;
  ffi_type* args[1];
  void* values[1];
  char const* s;
  int rc;

  /* Initialize the argument info vectors */
  args[0] = &ffi_type_pointer;
  values[0] = &s;

  /* Initialize the cif */
  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, &ffi_type_uint, args) == FFI_OK)
  {
    s = "Hello World!";
    ffi_call(&cif, (void (*)()) puts, &rc, values);
    s = "This is cool!";
    ffi_call(&cif, (void (*)()) puts, &rc, values);
  }

  return 0;
}
