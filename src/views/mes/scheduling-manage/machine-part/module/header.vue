<template>
  <div v-show="crud.searchToggle">
    <div style="display: flex">
      <common-radio-button
        ref="materialRef"
        v-model="query.material"
        :options="materialList"
        :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
        class="filter-item"
        @change="fetchTick"
      />
      <tag-tabs
        v-if="thickList?.length"
        v-model="query.thick"
        class="filter-item"
        :style="`width:calc(100% - ${materialRefWidth}px)`"
        itemKey="name"
        :data="thickList"
        @change="crud.toQuery"
      >
        <template #default="{ item }">
          <span>T = {{ item.name }}</span>
        </template>
      </tag-tabs>
    </div>
    <div>
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.length"
        size="small"
        placeholder="输入长度搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.width"
        size="small"
        placeholder="输入宽度搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
      <slot name="searchRight"></slot>
    </div>
  </div>
  <crudOperation :show-grid="false" :show-refresh="false">
    <template #optLeft>
      <slot name="optLeft" />
    </template>
    <template #viewLeft>
      <slot name="viewLeft" />
      <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
    </template>
  </crudOperation>
</template>

<script setup>
import { getMaterial, getThick } from '@/api/mes/scheduling-manage/machine-part'
import { defineEmits, ref, defineExpose, nextTick } from 'vue'
import { regHeader } from '@compos/use-crud'
import tagTabs from '@comp-common/tag-tabs'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import Scale from '@comp/Scale'
import { isBlank } from '@/utils/data-type'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)

const emit = defineEmits(['load'])

const materialRef = ref()
const materialRefWidth = ref()
const boxScale = ref(1)
const materialList = ref([])
const thickList = ref([])

function boxZoomOut() {
  if (crud.page.hasNextPage) {
    emit('load')
  }
}

async function fetchMaterial(lastQuery) {
  if (isBlank(query.projectIds)) return
  try {
    materialList.value = []
    thickList.value = []
    query.material = undefined
    query.thick = undefined
    const { content } = await getMaterial({
      dateTime: query.dateTime,
      projectIds: query.projectIds
    })
    materialList.value =
      content?.map((v, i) => {
        return {
          name: v
        }
      }) || []
    if (lastQuery && lastQuery?.material && content?.length && content.indexOf(lastQuery.material) !== -1) {
      query.material = lastQuery.material
      nextTick(() => {
        materialRefWidth.value = materialRef.value.$el.clientWidth + 15
        fetchTick(lastQuery)
      })
    } else if (materialList.value?.length) {
      query.material = materialList.value[0].name
      nextTick(() => {
        materialRefWidth.value = materialRef.value.$el.clientWidth + 15
        fetchTick()
      })
    }
  } catch (error) {
    console.log('获取材质', error)
  }
}

async function fetchTick(lastQuery) {
  if (isBlank(query.projectIds)) return
  try {
    thickList.value = []
    const { content } = await getThick({
      dateTime: query.dateTime,
      projectIds: query.projectIds,
      material: query.material
    })
    thickList.value =
      content?.map((v) => {
        return {
          name: v
        }
      }) || []
    if (lastQuery && lastQuery?.thick && content?.length && content.indexOf(lastQuery.thick) !== -1) {
      query.thick = lastQuery.thick
      crud.toQuery()
    } else if (thickList.value?.length) {
      query.thick = thickList.value[0].name
      crud.toQuery()
    }
  } catch (error) {
    console.log('获取厚度', error)
  }
}

defineExpose({
  boxScale,
  refreshConditions: fetchMaterial
})
</script>
