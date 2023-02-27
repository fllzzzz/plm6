<template>
  <div v-show="crud.searchToggle">
    <div style="display: flex">
      <common-radio-button
        ref="typeListRef"
        v-model="query.taskTypeEnum"
        :options="typeList"
        :dataStructure="{ key: 'value', label: 'name', value: 'value' }"
        :showOptionAll="typeList?.length > 1"
        class="filter-item"
        @change="fetchMaterial"
      />
      <common-radio-button
        ref="materialRef"
        v-model="query.material"
        :options="materialList"
        :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
        class="filter-item"
        @change="fetchTick"
      />
      <tag-tabs
        v-if="thickData?.length"
        v-model="query.thick"
        class="filter-item"
        :style="`width:calc(100% - ${materialRefWidth + typeListRefWidth}px)`"
        itemKey="name"
        :data="thickData"
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
      <el-input-number
        v-model.number="query.minLength"
        :min="0"
        :max="query.maxLength || 999999999"
        :precision="0"
        :controls="false"
        size="small"
        style="width: 130px"
        class="filter-item"
        clearable
        placeholder="最小长度(mm)"
        @keyup.enter="crud.toQuery"
      />
      <span class="filter-item">~</span>
      <el-input-number
        v-model.number="query.maxLength"
        class="filter-item"
        :min="query.minLength || 0"
        :precision="0"
        :max="999999999"
        :controls="false"
        size="small"
        style="width: 130px"
        clearable
        placeholder="最大长度(mm)"
        @keyup.enter="crud.toQuery"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="crud.toQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
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
import { getTypeList, getMaterial, getThick } from '@/api/mes/scheduling-manage/machine-part'
import { defineEmits, ref, defineExpose, nextTick } from 'vue'
import { regHeader } from '@compos/use-crud'
import tagTabs from '@comp-common/tag-tabs'
import crudOperation from '@crud/CRUD.operation'
import Scale from '@comp/Scale'
import { isBlank } from '@/utils/data-type'
import { componentTypeEnum } from '@enum-ms/mes'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)

const emit = defineEmits(['load', 'change'])

const materialRef = ref()
const materialRefWidth = ref()
const typeListRef = ref()
const typeListRefWidth = ref()
const boxScale = ref(1)
const typeList = ref([])
const materialList = ref([])
const thickData = ref([])

function resetQuery() {
  query.serialNumber = undefined
  query.maxLength = undefined
  query.minLength = undefined
  crud.toQuery()
}

function boxZoomOut() {
  if (crud.page.hasNextPage) {
    emit('load')
  }
}

async function fetchType(lastQuery) {
  materialList.value = []
  thickData.value = []
  typeList.value = []
  query.taskTypeEnum = undefined
  query.material = undefined
  query.thick = []
  if (isBlank(query.areaIds)) return
  try {
    const { content } = await getTypeList({
      monthList: query.monthList,
      areaIds: query.areaIds
    })
    typeList.value =
      content?.map((v) => {
        const _obj = {}
        if (v & componentTypeEnum.ARTIFACT.V) {
          _obj.name = '零件板'
        }
        if (v & componentTypeEnum.ASSEMBLE.V) {
          _obj.name = '翼腹板'
        }
        _obj.value = v
        return _obj
      }) || []
    if (lastQuery && lastQuery?.taskTypeEnum && content?.length && content.indexOf(lastQuery.taskTypeEnum) !== -1) {
      query.taskTypeEnum = lastQuery.taskTypeEnum
    } else if (typeList.value.length === 1) {
      query.taskTypeEnum = typeList.value[0].value
    }
    nextTick(() => {
      typeListRefWidth.value = typeListRef.value.$el.clientWidth + 15
      fetchMaterial(lastQuery)
    })
  } catch (error) {
    console.log('获取厚度', error)
  }
}

async function fetchMaterial(lastQuery) {
  if (isBlank(query.areaIds)) return
  try {
    const { content } = await getMaterial({
      monthList: query.monthList,
      areaIds: query.areaIds,
      taskTypeEnum: query.taskTypeEnum
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
  if (isBlank(query.areaIds)) return
  try {
    thickData.value = []
    const { content } = await getThick({
      monthList: query.monthList,
      areaIds: query.areaIds,
      material: query.material,
      taskTypeEnum: query.taskTypeEnum
    })
    thickData.value =
      content?.map((v) => {
        return {
          name: v
        }
      }) || []
    if (lastQuery && lastQuery?.thick && content?.length && content.indexOf(lastQuery.thick) !== -1) {
      query.thick = lastQuery.thick
      crud.toQuery()
    } else if (thickData.value?.length) {
      query.thick = thickData.value[0].name
      crud.toQuery()
    }
    emit('change', materialList.value, thickData.value)
  } catch (error) {
    console.log('获取厚度', error)
  }
}

defineExpose({
  boxScale,
  refreshConditions: fetchType
})
</script>
