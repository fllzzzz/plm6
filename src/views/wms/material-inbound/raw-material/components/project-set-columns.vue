<template>
  <el-table-column v-if="requisitionsSNOptions" prop="requisitionsSN" label="申购单" min-width="130" align="center">
    <template #default="{ row: { sourceRow: row }, $index }">
      <common-select
        v-if="row"
        v-model="row.requisitionsSN"
        :options="getRequisitionsSNOptions($index, row)"
        :dataStructure="{ key: 'serialNumber', label: 'serialNumber', value: 'serialNumber' }"
        :show-extra="$index !== 0 && row.requisitionsDittoable"
        type="other"
        placeholder="申购单"
        clearable
        @change="handleRequisitionsSNChange($event, row, $index)"
      />
    </template>
  </el-table-column>
  <template v-if="projectOptions">
    <el-table-column prop="projectId" align="center" min-width="170px" label="所属项目">
      <template #default="{ row: { sourceRow: row }, $index }">
        <common-select
          v-if="row"
          :key="Math.random()"
          v-model="row.projectId"
          :options="projectOptions"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          :show-extra="$index !== 0"
          :disabled-val="row.disabledProjectId"
          type="other"
          placeholder="所属项目"
          @change="handleProjectChange($event, $index, row)"
        />
      </template>
    </el-table-column>
    <el-table-column prop="monomerId" align="center" min-width="170px" label="单体">
      <template #default="{ row: { sourceRow: row }, $index }">
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
      <template #default="{ row: { sourceRow: row }, $index }">
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
  </template>
</template>

<script setup>
import { defineProps, computed, watch, watchEffect, ref, nextTick } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { isBlank, isNotBlank } from '@/utils/data-type'
import useDittoRealVal from '@/composables/form/use-ditto-real-val'

import useProjectTree from '@compos/store/use-project-tree'

const props = defineProps({
  order: {
    type: Object,
    default: () => {
      return {}
    }
  },
  requisitions: {
    type: Object,
    default: () => {
      return {}
    }
  },
  form: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

// TODO:优化由于“同上导致的需要计算代码”
const currentForm = ref({ list: [] })

const { projectMap, monomerMap } = useProjectTree()

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

const { initScopeList: initReqScopeList, handleValueChange: handleReqChange, getRealVal: getReqVal } = useDittoRealVal('requisitionsSN')

const projectDittoableIndex = computed(() => {
  return getProjectNotDittoArr()
})

const monomerDittoableIndex = computed(() => {
  return getMonomerNotDittoArr()
})

// 申购单选择
const requisitionsSNOptions = computed(() => {
  if (isNotBlank(props.order) && isNotBlank(props.order.requisitionsSN)) {
    return props.order.requisitionsSN.map((v) => {
      return { serialNumber: v }
    })
  } else {
    return null
  }
})

// 项目选择
const projectOptions = computed(() => {
  const order = props.order
  if (isNotBlank(order) && isNotBlank(order.projects)) {
    return order.projects.map((p) => {
      return { id: p.id, name: projectNameFormatter(p, { showSerialNumber: false }) }
    })
  } else {
    return null
  }
})

watchEffect(() => {
  initReqScopeList(currentForm.value.list || [])
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

// 如果申购单变化，则当前选中的项目切换为申购单的项目
function handleRequisitionsSNChange(sn, row, index) {
  handleReqChange(sn, index)
  const realSN = getReqVal(index)
  if (realSN) {
    const req = props.requisitions
    row.projectId = req ? req[realSN].projectId : undefined
    console.log('row.projectId: ', row.projectId)
    row.disabledProjectId = props.order.projectIds.filter((v) => v !== row.projectId)
  } else {
    row.disabledProjectId = []
  }
}

// 获取requisitionsSNOptions
function getRequisitionsSNOptions(index, row) {
  let _SN = []
  const projectId = getProjectVal(index)
  if (isBlank(projectId)) {
    _SN = props.order.requisitionsSN.map((v) => {
      return { serialNumber: v }
    })
  } else {
    Object.keys(props.requisitions).forEach((sn) => {
      if (props.requisitions[sn].projectId === projectId) {
        _SN.push({ serialNumber: sn })
      }
    })
  }
  if (isBlank(_SN)) {
    row.requisitionsDittoable = false
    row.requisitionsSN = undefined
  } else {
    row.requisitionsDittoable = true
  }

  return _SN
}

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
