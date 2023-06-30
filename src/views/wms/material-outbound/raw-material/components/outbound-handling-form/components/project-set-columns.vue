<template>
  <el-table-column prop="projectId" align="center" min-width="170px" label="所属项目">
    <template #default="{ row, $index }">
      <common-select
        v-if="row"
        :key="Math.random()"
        v-model="row.projectId"
        :options="projectOptions"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        :show-extra="$index !== 0"
        :clearable="props.projectClearable"
        :disabled-val="row.disabledProjectId"
        type="other"
        placeholder="所属项目"
        @change="handleProjectChange($event, $index, row)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="monomerId" align="center" min-width="170px" label="单体">
    <template #default="{ row, $index }">
      <common-select
        v-if="row"
        :key="Math.random()"
        v-model="row.monomerId"
        :options="projectMap?.[getProjectVal($index)]?.children || []"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        :show-extra="!projectDittoableIndex.includes($index)"
        clearable
        type="other"
        placeholder="单体"
        @change="handleMonomerChange($event, $index, row)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="areaId" align="center" min-width="170px" label="区域">
    <template #default="{ row, $index }">
      <common-select
        v-if="row"
        :key="Math.random()"
        v-model="row.areaId"
        :options="monomerMap?.[getMonomerVal($index)]?.children || []"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        :show-extra="!monomerDittoableIndex.includes($index)"
        clearable
        type="other"
        placeholder="区域"
      />
    </template>
  </el-table-column>
  <el-table-column prop="workshopId" label="车间" width="170px" align="center">
    <template #default="{ row }">
      <workshop-select v-model="row.workshopId" :type="warehouseTypeEnum.WORKSHOP.V" placeholder="可选择车间" style="width: 100%" clearable />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, watch, watchEffect, ref, nextTick } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { isBlank, isNotBlank } from '@/utils/data-type'
import useDittoRealVal from '@/composables/form/use-ditto-real-val'

import useProjectTree from '@compos/store/use-project-tree'
import workshopSelect from '@/components-system/wms/workshop-select.vue'

const props = defineProps({
  form: {
    type: Object,
    default: () => {
      return {}
    }
  },
  projectClearable: {
    type: Boolean,
    default: false
  }
})

// TODO:优化由于“同上导致的需要计算代码”
const currentForm = ref({ list: [] })

const { projectTree, projectMap, monomerMap } = useProjectTree()

watchEffect(() => {
  currentForm.value = props.form
})

const {
  initScopeList: initProjectScopeList,
  handleValueChange: handleProjectChangeForValue,
  getNotDittoArr: getProjectNotDittoArr,
  getRealVal: getProjectVal
} = useDittoRealVal('projectId')

const {
  initScopeList: initMonomerScopeList,
  handleValueChange: handleMonomerChangeForValue,
  getNotDittoArr: getMonomerNotDittoArr,
  getRealVal: getMonomerVal
} = useDittoRealVal('monomerId')

const projectDittoableIndex = computed(() => {
  return getProjectNotDittoArr()
})

const monomerDittoableIndex = computed(() => {
  return getMonomerNotDittoArr()
})

// 项目选择
const projectOptions = computed(() => {
  if (isNotBlank(projectTree.value)) {
    return projectTree.value.map((p) => {
      return { id: p.id, name: projectNameFormatter(p, { showSerialNumber: false }) }
    })
  } else {
    return null
  }
})

watchEffect(() => {
  initProjectScopeList(currentForm.value.list || [])
  initMonomerScopeList(currentForm.value.list || [])
})

// 如果只有一个项目自动赋值
watch(
  projectOptions,
  (opts) => {
    if (opts && opts.length === 1 && currentForm.value.list[0]) currentForm.value.list[0].projectId = opts[0].id
  },
  { immediate: true }
)

// 处理项目变化
function handleProjectChange(val, index, row) {
  handleProjectChangeForValue(val, index)
  if (val !== -1) {
    row.monomerId = undefined
  } else {
    nextTick(() => {
      if (isBlank(row.monomerId)) {
        row.monomerId = -1
        row.areaId = -1
      }
    })
  }
}

// 处理单体变化
function handleMonomerChange(val, index, row) {
  handleMonomerChangeForValue(val, index)
  if (val !== -1) {
    row.areaId = undefined
  } else {
    nextTick(() => {
      if (isBlank(row.areaId)) {
        row.areaId = -1
      }
    })
  }
}
</script>
