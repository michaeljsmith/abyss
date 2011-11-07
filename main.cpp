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
// List
//------------------------------------------------------------------------------
template <typename T>
struct ListNode;

template <typename T>
struct List : public shared_ptr<ListNode<T> const> {
  List() {}
  List(ListNode<T> const* ptr): shared_ptr<ListNode<T> const>(ptr) {}
};

template <typename T>
struct ListNode {
  ListNode(T const& head, List<T> const& tail): head(head), tail(tail) {}
  T head;
  List<T> tail;
};

template <typename T>
T head(List<T> const& l) {
  return l->head;
}

template <typename T>
List<T> tail(List<T> const& l) {
  return l->tail;
}

template <typename T>
bool nullp(List<T> const& list) {
  return list == 0;
}

template <typename T>
List<T> cons(T const& x0, List<T> const& tail) {
  List<T> l(new ListNode<T>(x0, tail));
  return l;
}

template <typename T>
List<T> list() {
  return List<T>(0);
}

template <typename T>
List<T> list(T const& x0) {
  return cons(x0, list<T>());
}

template <typename T, typename F>
void for_each(F const& f, List<T> l) {
  for (List<T> rest = l; !nullp(rest); rest = tail(rest)) {
    f(head(rest));
  }
}

//------------------------------------------------------------------------------
// Object
//------------------------------------------------------------------------------
template <typename I, typename O>
struct Relation
{
  Relation(I const& input, function<O (I)> const& generate_output):
    input(input), generate_output(generate_output) {}
  I input;
  function<O (I)> generate_output;
};

template <typename I, typename O>
O apply(Relation<I, O> const& f, I const& x) {
  return f.generate_output(x);
}

//------------------------------------------------------------------------------
// Float
//------------------------------------------------------------------------------
struct Float_
{
  Float_(function<void (float)> const& set): set(set) {}
  function<void (float)> set;
};
typedef List<Float_> Float;

void set(Float float_, float x) {
  for_each(
      bind(bind(&Float_::set, boost::lambda::_1), x),
      float_);
}

//------------------------------------------------------------------------------
// Sprite
//------------------------------------------------------------------------------
struct SpriteData
{
  SpriteData(): pos(0.0f) {}
  float pos;
};
typedef List<SpriteData*> Sprite;

Relation<Float, Sprite> sprite(SpriteData* sprite) {
  return Relation<Float, Sprite>(
      Float(list(Float_(var(sprite->pos) = boost::lambda::_1))),
      function<Sprite (Float)>(constant(list(sprite))));
}

void renderSprite(Sprite sprite)
{
  assert(!nullp(sprite));

  SpriteData* sprite_ = sprite->head;
  glLoadIdentity();

  glTranslatef(sprite_->pos, 0.0f, -6.0f);
	
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
  bool running = true;
  if (!glfwInit())
    exit( EXIT_FAILURE );

  glfwSetWindowSizeCallback( ReSizeGLScene );

  if (!glfwOpenWindow(300, 300, 0, 0, 0, 0, 32, 0, GLFW_WINDOW)) {
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  InitGL(640, 480);

  SpriteData sprite_;
  Relation<Float, Sprite> sprite_rel = sprite(&sprite_);
  Float pos = sprite_rel.input;
  Sprite sprite = apply(sprite_rel, Float());

  Clock clock;
  while (running) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    set(pos, clock.get() * 0.001f);
    renderSprite(sprite);

    glfwSwapBuffers();
    running =
      !glfwGetKey(GLFW_KEY_ESC) &&
      glfwGetWindowParam(GLFW_OPENED);
  }
  glfwTerminate();

  return 0;
}
