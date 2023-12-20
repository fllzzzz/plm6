<template>
  <div class="head-container">
    <el-cascader
      v-model="query.schedulingGroupId"
      :options="schedulingGroups.list"
      :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: true, checkStrictly: true }"
      :show-all-levels="true"
      style="width: 270px"
      filterable
      clearable
      placeholder="请选择生产组"
      class="filter-item"
      @change="handleGroupChange"
    />
    <project-cascader v-model="query.projectId" clearable class="filter-item" style="width: 270px" @change="toQuery" />
    <monomer-select-area-select
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      needConvert
      clearable
      :project-id="query.projectId"
      @change="toQuery"
    />
    <!-- <el-input
      v-model="query.material"
      placeholder="按材质搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="toQuery"
    />
    <el-input
      v-model="query.thickness"
      placeholder="按板厚搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="toQuery"
    />
    <br /> -->
    <el-input
      v-model="query.artifactSerialNumber"
      placeholder="按构件编号搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="toQuery"
    />
    <el-input
      v-model="query.serialNumber"
      placeholder="按零件编号搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="toQuery"
    />
    <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="toQuery">搜索</common-button>
    <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
      重置
    </common-button>
  </div>
</template>

<script setup>
import { defineEmits, ref } from 'vue'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import { componentTypeEnum } from '@enum-ms/mes'

const emit = defineEmits(['toQuery'])
const query = ref({})

const groupLoad = ref(false)
const schedulingGroups = ref({ list: [], obj: {}})

fetchGroups()

async function fetchGroups() {
  if (groupLoad.value) return
  try {
    schedulingGroups.value = await manualFetchGroupsTree({ productType: componentTypeEnum.ARTIFACT.V }, true)
    groupLoad.value = true
  } catch (e) {
    console.log('获取生产组的信息失败', e)
  }
}

function toQuery() {
  emit('toQuery', query.value)
}

function resetQuery() {
  query.value = {}
  emit('toQuery', query.value)
}

function handleGroupChange(val) {
  query.value.workshopId = val?.[0] || undefined
  query.value.productionLineId = val?.[1] || undefined
  query.value.groupsId = val?.[2] || undefined
  toQuery()
}
</script>

<style lang="scss" scoped></style>
