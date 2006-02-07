#include <assert.h>

#include "OrtsInterface.h"

#include "GameObj.H"
#include "GameStateModule.H"

OrtsInterface::OrtsInterface(GameStateModule* _gsm,
                             SoarInterface*   _soarInterface,
                             GroupManager*    _groupManager)
: gsm(_gsm), soarInterface(_soarInterface), groupManager(_groupManager)
{
}

void OrtsInterface::addAppearedObject(const GameObj* gameObj) {
  assert(false);
}

void OrtsInterface::addCreatedObject(GameObj* gameObj) {
  // make sure the game object does not exist in the middleware
  assert(objectMap.find(gameObj) == objectMap.end());

  SoarGameObject* newObj = new SoarGameObject(gameObj);
  
  // GroupManager takes care of setting the object->group pointers
  groupManager->addGroup(newObj);
  
  // more initializations of the object?

  objectMap[gameObj] = newObj;
}

void OrtsInterface::removeDeadObject(const GameObj* gameObj) {
  // make sure the game object exists
  assert(objectMap.find(gameObj) != objectMap.end());

  SoarGameObject* sObject = objectMap[gameObj];
  sObject->getGroup()->removeUnit(sObject);
  
  delete objectMap[gameObj];
  objectMap.erase(gameObj);
}
  
void OrtsInterface::removeVanishedObject(const GameObj* gameObj) {
  // just remove like a dead object for now, but change it later
  removeDeadObject(gameObj);
}

bool OrtsInterface::handle_event(const Event& e) {
  if (e.get_who() == GameStateModule::FROM) {
    if (e.get_what() == GameStateModule::VIEW_MSG) {
      cout << "INTERRUPT!" << endl;
      const GameChanges& changes = gsm->get_changes();

      updateSoarGameObjects(changes);
      groupManager->updateWorld();

      /* I'm assuming here that those update calls from above have already
       * updated the soar input link correctly, so commit everything
       */
      soarInterface->commitInputLinkChanges();
    }
    return true;
  }
  else {
    return false;
  }
}

void OrtsInterface::updateSoarGameObjects(const GameChanges& changed) {
  // add new objects
  FORALL(changed.new_objs, obj) {
    GameObj* gob = (*obj)->get_GameObj();
    if (gob == 0) continue;
    if (gob->sod.in_game) {
      /* It's not clear whether this object was created or appeared, maybe we
       * should drop the distinction altogether, or do some extra bookkeeping
       * here
       */
      //addAppearedObject(gob);
      addCreatedObject(gob);
    }
  }

  FORALL(changed.changed_objs, obj) {
    GameObj* gob = (*obj)->get_GameObj();
    if (gob == 0) {
      continue;
    }
    else if (gob->sod.in_game) {
      /* we should have a SoarGameObject for this GameObj, if not, we're in
       * trouble
       */
      assert(objectMap.find(gob) != objectMap.end());

      // just call update to let it know there were changes
      objectMap[gob]->update();
    }
    else if (gob == playerGameObj) {
      updateSoarPlayerInfo();
    }
  }

  FORALL(changed.vanished_objs, obj) {
    GameObj* gob = (*obj)->get_GameObj();
    if (gob == 0) continue;
    if (gob->sod.in_game) {
      /* we should have a SoarGameObject for this GameObj, if not, we're in
       * trouble
       */
      assert(objectMap.find(gob) != objectMap.end());

      removeVanishedObject(gob);
    }
  }
  
  FORALL(changed.dead_objs, obj) {
    GameObj* gob = (*obj)->get_GameObj();
    if (gob == 0) continue;
    if (gob->sod.in_game) {
      /* we should have a SoarGameObject for this GameObj, if not, we're in
       * trouble
       */
      assert(objectMap.find(gob) != objectMap.end());

      removeDeadObject(gob);
    }
  }
}

void OrtsInterface::updateSoarPlayerInfo() {
  // only know get gold for now
  soarInterface->updatePlayerGold(playerGameObj->get_int("gold"));
}

OrtsInterface::~OrtsInterface() {
  objectMapIter it = objectMap.begin();
  while (it != objectMap.end()) {
    delete (*it).second;
    it++;
  }
}
