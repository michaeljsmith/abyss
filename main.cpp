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
struct Value {
  struct Listener {
    virtual void onChange(T) = 0;
  };

  map<void*, shared_ptr<Listener> > listeners;
};

template <typename T>
struct FunctionValueListener : public Value<T>::Listener {
  function<void (T)> onChangeHandler;
  FunctionValueListener(function<void (T)> const& onChangeHandler):
    onChangeHandler(onChangeHandler) {}
  virtual void onChange(T value) {
    this->onChangeHandler(value);
  }
};

template <typename T>
shared_ptr<Value<T>> value() {
  shared_ptr<Value<T>> val(new Value<T>());
  return val;
}

template <typename T>
void addListener(shared_ptr<Value<T>> value,
    void* object, shared_ptr<typename Value<T>::Listener> const& listener) {
  value->listeners.insert(make_pair(object, listener));
}

template <typename T>
void setValue(Value<T>& valueObj, T value, void* objectToSkip) {
  for (typename map<void*, shared_ptr<typename Value<T>::Listener>>::iterator pos = valueObj.listeners.begin(), end = valueObj.listeners.end(); pos != end; ++pos) {
    void* object = (*pos).first;
    shared_ptr<typename Value<T>::Listener> listener = (*pos).second;
    if (object != objectToSkip) {
      listener->onChange(value);
    }
  }
}

//------------------------------------------------------------------------------
// Sin
//------------------------------------------------------------------------------
struct SineValue;
void setSineValue(SineValue* v, float x);
struct SineValue : public Value<float> {
  SineValue(shared_ptr<Value<float>> const& argument):
    argument(argument)
  {
    using namespace boost::lambda;

    // TODO: Check whether bind() creates a shared_ptr to the argument - if so, this could cause a
    // circular dependency.
    shared_ptr<FunctionValueListener<float>> listener(
        new FunctionValueListener<float>(
          function<void (float)>(bind(&setSineValue, this, _1))));
    addListener(argument, this, listener);
  }

  shared_ptr<Value<float>> argument;
};
void setSineValue(SineValue* v, float x) {setValue(*v, std::sin(x), v);}

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

void setSpritePosition(SpriteData* data, float position) {
  data->position = position;
}

using namespace boost::lambda;
shared_ptr<Sprite> sprite(SpriteData* data, shared_ptr<Value<float>> position) {
  shared_ptr<Sprite> spr(new Sprite(data, position));
  shared_ptr<FunctionValueListener<float>> listener(
      new FunctionValueListener<float>(
        function<void (float)>(bind(&setSpritePosition, data, _1))));
  addListener(position, spr.get(), listener);
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

  glfwSetWindowSizeCallback( ReSizeGLScene );

  if (!glfwOpenWindow(300, 300, 0, 0, 0, 0, 32, 0, GLFW_WINDOW)) {
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  InitGL(640, 480);

  shared_ptr<Value<float>> position = value<float>();
  SpriteData spriteData;
  shared_ptr<Sprite> spr = sprite(&spriteData, sin(position));

  Clock clock;
  while (running) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    setValue(*position, 0.001f * float(clock.get()), 0);

    render(spr);

    glfwSwapBuffers();
    running =
      !glfwGetKey(GLFW_KEY_ESC) &&
      glfwGetWindowParam(GLFW_OPENED);
  }
  glfwTerminate();

  return 0;
}
