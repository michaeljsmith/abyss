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
  virtual void visitReference(void* data) const = 0;
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

struct Object {
  virtual void applyReferenceVisitor(Environment* environment, int targetName, ReferenceVisitor const& visitor) = 0;
};

//------------------------------------------------------------------------------
// Primitive
//------------------------------------------------------------------------------
template <typename T>
struct Primitive : public Object {
  virtual void applyReferenceVisitor(Environment* environment, int targetName, ReferenceVisitor const& visitor) {
  }
};

//------------------------------------------------------------------------------
// Composite
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Sprite
//------------------------------------------------------------------------------
struct Sprite {
  struct Data {
    Data(): position(0.0f) {}
    float position;
  };

  Data& data;
  Reference<float> position;

  Sprite(Data& data, Reference<float> position): data(data), position(position) {}
};

//void renderSprite(Sprite sprite)
//{
//  assert(!nullp(sprite));
//
//  SpriteData* sprite_ = sprite->head;
//  glLoadIdentity();
//
//  glTranslatef(sprite_->pos, 0.0f, -6.0f);
//	
//  glBegin(GL_POLYGON);
//  glColor3f(1.0f, 0.0f, 0.0f);
//  glVertex3f(0.0f, 1.0f, 0.0f);
//  glColor3f(0.0f, 1.0f, 0.0f);
//  glVertex3f(1.0f, -1.0f,  0.0f);
//  glColor3f(0.0f, 0.0f, 1.0f);
//  glVertex3f(-1.0f, -1.0f, 0.0f);
//  glEnd();
//}

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
  bool running = true;
  if (!glfwInit())
    exit( EXIT_FAILURE );

  glfwSetWindowSizeCallback( ReSizeGLScene );

  if (!glfwOpenWindow(300, 300, 0, 0, 0, 0, 32, 0, GLFW_WINDOW)) {
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  InitGL(640, 480);

  //SpriteData sprite_;
  //Relation<Float, Sprite> sprite_rel = sprite(&sprite_);
  //Float pos = sprite_rel.input;
  //Sprite sprite = apply(sprite_rel, Float());

  Clock clock;
  while (running) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

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
