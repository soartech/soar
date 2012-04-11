/*
 *  varray.h
 *  lcmgl
 *
 *  Created by Edwin Olson on 4/20/10.
 *  Copyright 2010 ebolson. All rights reserved.
 *
 */
#ifndef _VARRAY_H
#define _VARRAY_H

typedef struct varray varray_t;

varray_t *varray_create();
void varray_destroy(varray_t *va);
int varray_size(varray_t *va);
void varray_add(varray_t *va, void *p);
void *varray_get(varray_t *va, int idx);
void varray_remove(varray_t *va, void *d);
#endif
