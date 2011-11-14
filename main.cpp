#include <GL/glfw.h>
#include <stdlib.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <sys/time.h>

using boost::function;
using boost::shared_ptr;
using boost::lambda::constant;
using boost::lambda::var;
using boost::lambda::bind;

//------------------------------------------------------------------------------
// Object
//------------------------------------------------------------------------------
struct ReferenceVisitor {
  virtual void visitReference(void* object) const = 0;
};

template <typename X, typename T>
struct FunctionReferenceVisitor : public ReferenceVisitor {
  function<X (T)> const & fn;
  FunctionReferenceVisitor(function<X (T)> const& fn): fn(fn) {}

  virtual void visitReference(void* object) const {
    T* typedObject = static_cast<T*>(object);
    this->fn(*typedObject);
  }
};

struct Environment {
  virtual void applyReferenceVisitor(int targetName, ReferenceVisitor const& visitor) = 0;
};

template <typename T>
struct Reference {
  Environment* environment;
  int name;
  Reference(Environment* environment, int name): environment(environment), name(name) {}
};

template <typename T>
struct Object {
  virtual int getName() const = 0;
  virtual void applyReferenceVisitor(Environment* environment, int targetName, ReferenceVisitor const& visitor) = 0;
};

//------------------------------------------------------------------------------
// Primitive
//------------------------------------------------------------------------------
template <typename T>
struct Primitive : public Object<T> {
  int name;
  function<T (Environment*)> constructor;

  Primitive(int name, function<T (Environment*)> const& constructor): name(name), constructor(constructor) {}

  virtual int getName() const {
    return this->name;
  }

  virtual void applyReferenceVisitor(Environment* environment, int targetName, ReferenceVisitor const& visitor) {
    if (this->name == targetName) {
      T object = this->constructor(environment);
      visitor.visitReference(&object);
    }
  }
};

template <typename F> struct PrimitiveConstructor {};

template <typename T>
struct PrimitiveConstructor<T ()> {
  typedef typename T::Data Data;
  Data& data;
  int name0;
  PrimitiveConstructor(Data& data): data(data) {}
  T operator()(Environment* /*environment*/) {return T(data);}
};

template <typename T, typename X0>
struct PrimitiveConstructor<T (X0)> {
  typedef typename T::Data Data;
  Data& data;
  int name0;
  PrimitiveConstructor(Data& data, int name0): data(data), name0(name0) {}
  T operator()(Environment* environment) {return T(data, Reference<X0>(environment, name0));}
};

int newName() {
  static int nextName = 100;
  return nextName++;
}

//------------------------------------------------------------------------------
// Composite
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Value
//------------------------------------------------------------------------------
template <typename T>
struct Value {
  struct Data {
    function<void (T)> set;
    Data(function<void (T)> const& set): set(set) {}
  };

  Data& data;

  Value(Data& data): data(data) {}
};

template <typename T>
shared_ptr<Object<Value<T>>> value(function<void (T)> const& set) {
  typename Value<T>::Data data(set);
  shared_ptr<Primitive<Value<T>>> object(new Primitive<Value<T>>(newName(),
        function<Value<T> (Environment*)>(PrimitiveConstructor<Value<T> ()>(data))));
  return object;
}

//------------------------------------------------------------------------------
// Sprite
//------------------------------------------------------------------------------
struct SpriteData {
  SpriteData(): position(0.0f) {}
  float position;
};
struct Sprite {
  typedef SpriteData* Data;

  Data data;
  Reference<float> position;
  Sprite(Data data, Reference<float> position): data(data), position(position) {}
};

shared_ptr<Object<Sprite>> sprite(SpriteData* data, int positionName) {
  shared_ptr<Primitive<Sprite>> object(new Primitive<Sprite>(newName(),
        function<Sprite (Environment*)>(PrimitiveConstructor<Sprite (float)>(data, positionName))));
  return object;
}

shared_ptr<Object<Value<float>>> spritePosition(SpriteData* data) {
  function<void (float)> set();
  return value(set);
}

void renderSprite(Sprite& sprite)
{
  SpriteData* spriteData = sprite.data;

  glLoadIdentity();

  glTranslatef(spriteData->position, 0.0f, -6.0f);
	
  glBegin(GL_POLYGON);
  glColor3f(1.0f, 0.0f, 0.0f);
  glVertex3f(0.0f, 1.0f, 0.0f);
  glColor3f(0.0f, 1.0f, 0.0f);
  glVertex3f(1.0f, -1.0f,  0.0f);
  glColor3f(0.0f, 0.0f, 1.0f);
  glVertex3f(-1.0f, -1.0f, 0.0f);
  glEnd();
}

//------------------------------------------------------------------------------
// Time
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// GL
//------------------------------------------------------------------------------
void InitGL(int Width, int Height)
{
  glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
  glClearDepth(1.0);
  glDepthFunc(GL_LESS);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective(45.0f,(GLfloat)Width/(GLfloat)Height,0.1f,100.0f);

  glMatrixMode(GL_MODELVIEW);
}

void GLFWCALL ReSizeGLScene(int Width, int Height)
{
  if (Height==0)
    Height=1;

  glViewport(0, 0, Width, Height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective(45.0f,(GLfloat)Width/(GLfloat)Height,0.1f,100.0f);
  glMatrixMode(GL_MODELVIEW);
}

//------------------------------------------------------------------------------
// Timing
//------------------------------------------------------------------------------
struct Clock {
  unsigned time;
  Clock(): time(retrieve_time()) {}
  unsigned get() {
    return retrieve_time() - time;
  }

private:
  unsigned retrieve_time() {
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return tv.tv_sec * 1000 + tv.tv_usec / 1000;
  }
};

//------------------------------------------------------------------------------
// App
//------------------------------------------------------------------------------
int main() {
  using namespace boost::lambda;

  bool running = true;
  if (!glfwInit())
    exit( EXIT_FAILURE );

  glfwSetWindowSizeCallback( ReSizeGLScene );

  if (!glfwOpenWindow(300, 300, 0, 0, 0, 0, 32, 0, GLFW_WINDOW)) {
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  InitGL(640, 480);

  SpriteData spriteData;
  shared_ptr<Object<Sprite>> spriteObject = sprite(&spriteData, 0);
  //SpriteData sprite_;
  //Relation<Float, Sprite> sprite_rel = sprite(&sprite_);
  //Float pos = sprite_rel.input;
  //Sprite sprite = apply(sprite_rel, Float());

  Clock clock;
  while (running) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    spriteObject->applyReferenceVisitor(0, spriteObject->getName(), FunctionReferenceVisitor<void, Sprite>(bind(&renderSprite, _1)));

    //set(pos, clock.get() * 0.001f);
    //renderSprite(sprite);

    glfwSwapBuffers();
    running =
      !glfwGetKey(GLFW_KEY_ESC) &&
      glfwGetWindowParam(GLFW_OPENED);
  }
  glfwTerminate();

  return 0;
}
