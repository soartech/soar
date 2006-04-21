#ifndef GroupManager_h
#define GroupManager_h

#include "SoarGameObject.h"
#include "SoarGameGroup.h"

class Sorts;
/*
bool compareGroups(SoarGameGroup* a, SoarGameGroup* b) {
  return a->getDistToFocus() < b->getDistToFocus();
}*/

class GroupManager {
  public:
    GroupManager();
    ~GroupManager();

    void updateVision();
    bool assignActions();
    void processVisionCommands();

    void addGroup(SoarGameObject* object);
    // used by ORTSInterface when it sees a new object- create a group for it
    
    SoarGameGroup* getGroupNear(string type, int owner, int x, int y);
    
    void setSorts(const Sorts* s) {sorts = s;}
 
    
  private:
    int groupingRadiusSquared;
    int focusX, focusY;
    void prepareForReGroup();
    void reGroup();
    void generateGroupData();
    void adjustAttention();
    void updateFeatureMaps(bool refreshAll);

    void removeGroup(SoarGameGroup*);

    set <pair<string, int> > staleGroupCategories;
    
//    list <SoarGameGroup*> groupsInFocus;
//    list <SoarGameGroup*> groupsNotInFocus;
    
    // this set is maintained in sorted order, items toward the front
    // are closer to the center of focus, and have priority to go on the
    // input link.
    set <SoarGameGroup*> groups;

    void setAllCategoriesStale();
    
    const Sorts* sorts;
};

struct objectGroupingStruct {
  SoarGameObject* object;
  SoarGameGroup* group;
  bool assigned;
  bool oldGroup;
  int x,y;
};

#endif // GroupManager_h
