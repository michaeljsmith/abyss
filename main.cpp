#include <GL/glfw.h>
#include <stdlib.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <sys/time.h>
#include <map>
#include <cmath>

using boost::function;
using boost::shared_ptr;
using boost::lambda::constant;
using boost::lambda::var;
using boost::lambda::bind;
using std::map;
using std::make_pair;

//------------------------------------------------------------------------------
// Value
//------------------------------------------------------------------------------
template <typename T>
struct Listener {
  virtual void onApply(function<void (T&)> const& fn) = 0;
};

template <typename T>
struct Value {
  map<void const*, shared_ptr<Listener<T>>> listeners;
};

template <typename T>
struct FunctionListener : public Listener<T> {
  function<void (function<void (T&)> const&)> onApplyHandler;

  FunctionListener(function<void (function<void (T&)> const&)> const& onApplyHandler):
    onApplyHandler(onApplyHandler) {}

  virtual void onApply(function<void (T&)> const& fn) {
    this->onApplyHandler(fn);
  }
};

template <typename T>
shared_ptr<Value<T>> value() {
  shared_ptr<Value<T>> val(new Value<T>());
  return val;
}

template <typename T>
void addListener(Value<T>& value, void const* object, shared_ptr<Listener<T>> listener) {
  value.listeners.insert(make_pair(object, listener));
}

template <typename T>
void setValue(Value<T>& valueObj, T value, void* objectToSkip) {
  using namespace boost::lambda;

  function<void (T&)> setter(_1 = value);
  for (typename map<void const*, shared_ptr<Listener<T>>>::iterator pos = valueObj.listeners.begin(), end = valueObj.listeners.end(); pos != end; ++pos) {
    void const* object = (*pos).first;
    shared_ptr<Listener<T>> listener = (*pos).second;
    if (object != objectToSkip) {
      listener->onApply(setter);
    }
  }
}

//------------------------------------------------------------------------------
// Pair
//------------------------------------------------------------------------------
template <typename T0, typename T1>
struct Pair {
  struct Listener {
    virtual void onChanged0(function<void (T0)> const& fn) = 0;
    virtual void onChanged1(function<void (T1)> const& fn) = 0;
  };

  Pair(shared_ptr<T0> const& x0, shared_ptr<T1> const& x1): x0(x0), x1(x1) {
  }

  map<void const*, shared_ptr<Listener> > listeners;
  shared_ptr<T0> x0;
  shared_ptr<T1> x1;
};

template <typename T0, typename T1>
struct PairFirst : public Value<float> {
  shared_ptr<Pair<T0, T1>> pair;
};

//------------------------------------------------------------------------------
// Sin
//------------------------------------------------------------------------------
struct SineValue;
void applyToSine(SineValue* v, function<void (float&)> fn);
struct SineValue : public Value<float> {
  SineValue(shared_ptr<Value<float>> argument):
    argument(argument)
  {
    using namespace boost::lambda;

    shared_ptr<Listener<float>> listener(
        new FunctionListener<float>(
          function<void (function<void (float&)> const&)>(bind(&applyToSine, this, _1))));
    addListener(*argument, this, listener);
  }

  shared_ptr<Value<float>> argument;
};
void applyToSine(SineValue* v, function<void (float&)> fn) {
  float x = 0.0f;
  fn(x);
  setValue(*v, std::sin(x), v);
}

shared_ptr<Value<float>> sin(shared_ptr<Value<float>> const& argument) {
  shared_ptr<Value<float>> result(new SineValue(argument));
  return result;
}

//------------------------------------------------------------------------------
// Sprite
//------------------------------------------------------------------------------
struct SpriteData {
  SpriteData(): position(0.0f) {}
  float position;
};
struct Sprite {
  Sprite(SpriteData* data, shared_ptr<Value<float>> position)
    : data(data), position(position) {}
  SpriteData* data;
  shared_ptr<Value<float>> position;
};

void applyToSpritePosition(SpriteData* data, function<void (float&)> fn) {
  float x(0.0f);
  fn(x);
  data->position = x;
}

using namespace boost::lambda;
shared_ptr<Sprite> sprite(SpriteData* data, shared_ptr<Value<float>> position) {
  shared_ptr<Sprite> spr(new Sprite(data, position));
  shared_ptr<Listener<float>> listener(
      new FunctionListener<float>(
        function<void (function<void (float&)> const&)>(bind(&applyToSpritePosition, data, _1))));
  addListener(*position, spr.get(), listener);
  return spr;
}

void render(shared_ptr<Sprite> sprite)
{
  SpriteData* spriteData = sprite->data;

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

  glfwSetWindowSizeCallback(ReSizeGLScene);

  if (!glfwOpenWindow(300, 300, 0, 0, 0, 0, 32, 0, GLFW_WINDOW)) {
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  InitGL(640, 480);

  shared_ptr<Value<float>> position = value<float>();
  SpriteData spriteData1;
  shared_ptr<Sprite> spr1 = sprite(&spriteData1, position);
  SpriteData spriteData2;
  shared_ptr<Sprite> spr2 = sprite(&spriteData2, sin(position));

  Clock clock;
  while (running) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    setValue(*position, 0.001f * float(clock.get()), 0);

    render(spr1);
    render(spr2);

    glfwSwapBuffers();
    running =
      !glfwGetKey(GLFW_KEY_ESC) &&
      glfwGetWindowParam(GLFW_OPENED);
  }
  glfwTerminate();

  return 0;
}
