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

void* list() {
  return 0;
}

void* list(void* x0) {
  return cons(x0, list());
}

void* list(void* x0, void* x1) {
  return cons(x0, list(x1));
}

void* list(void* x0, void* x1, void* x2) {
  return cons(x0, list(x1, x2));
}

void* reverse_helper(void* prev, void* tail) {
  if (tail == 0) {
    return prev;
  } else {
    return reverse_helper(cons(car(tail), prev), cdr(tail));
  }
}
void* reverse(void* list) {
  return reverse_helper(0, list);
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

struct ExecutionContext {
  ExecutionContext(std::map<void*, void*>& functions, void* dest):
    functions(functions), dest(dest), nextTmpIdx(1) {}

  std::map<void*, void*>& functions;
  void* dest;
  std::map<void*, std::pair<void*, void*> > bindings;
  int nextTmpIdx;
};

char exe_type_int[10] = "type_int";
char exe_type_ptr[10] = "type_ptr";

size_t value_size(void* type) {
  size_t size;
  if (type == exe_type_int) {
    size = sizeof(int);
  } else if (type == exe_type_ptr) {
    size = sizeof(void*);
  } else {
    ASSERT(0);
    size = 0;
  }

  return size;
}

void* alloc_value(void* type) {
  return new char[value_size(type)];
}

void destroy_value(void* /*type*/, void* value) {
  delete [] (char*) value;
}

void* copy_value(void* type, void* value) {
  void* new_value = alloc_value(type);
  memcpy(new_value, value, value_size(type));
  return new_value;
}

struct ValueStackEntry {
  ValueStackEntry(): buffer(0), type(0), value(0) {}
  ~ValueStackEntry() {
    if (buffer != 0) {
      ASSERT(this->type != 0);
      destroy_value(this->type, this->buffer);
    }
  }

  void* buffer;
  void* type;
  void* value;
};

void set_value_stack_entry_reference(ValueStackEntry& dest, void* type, void* value) {
  ASSERT(dest.buffer == 0);
  ASSERT(dest.type == 0);
  ASSERT(dest.value == 0);
  dest.type = type;
  dest.value = value;
  ASSERT(dest.type != 0);
  ASSERT(dest.value != 0);
}

void allocate_value_stack_entry(ValueStackEntry& dest, void* type) {
  ASSERT(dest.buffer == 0);
  ASSERT(dest.type == 0);
  ASSERT(dest.value == 0);
  dest.value = dest.buffer = alloc_value(type);
  dest.type = type;
  ASSERT(dest.type != 0);
  ASSERT(dest.value != 0);
}

void add_binding(ExecutionContext& ctx, void* type, void* name, void* value) {
  ctx.bindings.insert(
      std::make_pair(name, std::make_pair(type, value)));
}

void* get_binding_type(ExecutionContext& ctx, void* name) {
  std::map<void*, std::pair<void*, void*> >::iterator value_pos =
    ctx.bindings.find(name);
  ASSERT(value_pos != ctx.bindings.end());
  return (*value_pos).second.first;
}

std::pair<void*, void*> get_binding(ExecutionContext& ctx, void* name) {
  std::map<void*, std::pair<void*, void*> >::iterator value_pos =
    ctx.bindings.find(name);
  ASSERT(value_pos != ctx.bindings.end());
  return (*value_pos).second;
}

void* pop_binding(ExecutionContext& ctx, void* type, void* name) {
  std::map<void*, std::pair<void*, void*> >::iterator value_pos =
    ctx.bindings.find(name);
  ASSERT(value_pos != ctx.bindings.end());
  void* actual_type = (*value_pos).second.first;
  ASSERT(actual_type == type); 
  void* value = (*value_pos).second.second;
  ctx.bindings.erase(value_pos);
  return value;
}

void load_functions(std::map<void*, void*>& functions, void* code) {
  void* module = code;
  while (module != 0) {
    void* fundef = pop_cons(module);
    void* name = pop_symbol(fundef);
    void* type = pop_symbol(fundef);
    void* parms = pop_cons(fundef);
    void* code = pop_cons(fundef);
    ASSERT(fundef == 0);

    functions.insert(std::make_pair(name, list(type, parms, code)));
  }
}

void* get_function(ExecutionContext& ctx, void* fn_name) {
  std::map<void*, void*>::iterator function_pos = ctx.functions.find(fn_name);
  ASSERT(function_pos != ctx.functions.end());
  return (*function_pos).second;
}

void execute_expression(ExecutionContext& ctx, ValueStackEntry& dest, void* expression);

void execute_statements(ExecutionContext& ctx, void* statements) {
  void* remaining_statements = statements;
  while (remaining_statements != 0) {
    void* statement = pop_arg(remaining_statements);

    // TODO: check special statements.
    ValueStackEntry value_entry;
    execute_expression(ctx, value_entry, statement);
  }
}

void execute_function(std::map<void*, void*>& functions, void* fn_name, void* dest, void* args) {
  std::map<void*, void*>::iterator function_pos = functions.find(fn_name);
  ASSERT(function_pos != functions.end());
  void* fn = (*function_pos).second;
  /*void* type =*/ pop_cons(fn);
  void* parms = pop_cons(fn);
  void* code = pop_cons(fn);
  ASSERT(fn == 0);

  ExecutionContext ctx(functions, dest);
  void* remaining_parms = parms;
  while (remaining_parms != 0) {
    void* parm = pop_symbol(remaining_parms);
    void* parm_name = pop_symbol(parm);
    void* parm_type = pop_symbol(parm);
    ASSERT(parm == 0);

    void* arg = pop_arg(args);

    add_binding(ctx, parm_type, parm_name, copy_value(parm_type, arg));
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

    void* arg = pop_binding(ctx, parm_type, parm_name);
    destroy_value(parm_type, arg);
  }

  ASSERT(ctx.bindings.empty());
}

void execute_function_args(ExecutionContext& ctx, void* fn_name, void* dest,
    void* prev_args, void* arg_expressions) {
  if (arg_expressions == 0) {
    execute_function(ctx.functions, fn_name, dest, reverse(prev_args));
  } else {
    void* arg_expression = car(arg_expressions);
    ValueStackEntry value_entry;
    execute_expression(ctx, value_entry, arg_expression);
    ASSERT(value_entry.value);
    execute_function_args(ctx, fn_name, dest, cons(value_entry.value, prev_args),
        cdr(arg_expressions));
  }
}

void execute_expression(ExecutionContext& ctx, ValueStackEntry& dest, void* expression) {
  if (symbolp(expression)) {
    std::pair<void*, void*> binding = get_binding(ctx, expression);
    set_value_stack_entry_reference(dest, binding.first, binding.second);
  } else if (consp(expression)) {
    void* fn_name = car(expression);
    void* fn = get_function(ctx, fn_name);
    void* type = pop_cons(fn);
    /*void* parms =*/ pop_cons(fn);
    /*void* code =*/ pop_cons(fn);
    ASSERT(fn == 0);

    allocate_value_stack_entry(dest, type);
    execute_function_args(ctx, fn_name, dest.value, 0, cdr(expression));
  }
}

void execute(void* code) {
  std::map<void*, void*> functions;
  load_functions(functions, code);

  int result;
  execute_function(functions, symbol("main"), &result, 0);
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
