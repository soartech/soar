/*
 *  varray.c
 *  lcmgl
 *
 *  Created by Edwin Olson on 4/20/10.
 *  Copyright 2010 ebolson. All rights reserved.
 *
 */

#include "varray.h"
#include <math.h>
#include <stdlib.h>
#include <assert.h>

#define MIN_ALLOC 16

struct varray
{
	int size;
	int alloc;
	void **data;
};

varray_t *varray_create()
{
	varray_t *va = calloc(1, sizeof(varray_t));
	return va;
}

void varray_destroy(varray_t *va)
{
	if (va->data != NULL)
		free(va->data);
	free(va);
}

int varray_size(varray_t *va)
{
	return va->size;
}

void varray_add(varray_t *va, void *p)
{
	if (va->size == va->alloc) {
		int newalloc = va->alloc*2;
		if (newalloc < MIN_ALLOC)
			newalloc = MIN_ALLOC;
		va->data = realloc(va->data, sizeof(void*)*newalloc);
		va->alloc = newalloc;
	}

	va->data[va->size] = p;
	va->size++;
}

void *varray_get(varray_t *va, int idx)
{
	assert(idx < va->size);
	return va->data[idx];
}

void varray_remove(varray_t *va, void *d)
{
    int outpos = 0;
    for (int inpos = 0; inpos < va->size; inpos++) {
        if (va->data[inpos] == d)
            continue;
        va->data[outpos] = va->data[inpos];
        outpos++;
    }
    va->size = outpos;
}
