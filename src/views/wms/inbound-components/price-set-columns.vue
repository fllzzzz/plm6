<template>
  <el-table-column prop="unitPrice" align="center" width="115px" label="含税单价">
    <template #default="{ row }">
      <el-input-number
        v-model="row.unitPrice"
        :min="0"
        :max="9999999999"
        :controls="false"
        :step="5"
        :precision="2"
        size="mini"
        placeholder="含税单价"
        @change="handleUnitPriceChange($event, row)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="amount" align="center" width="135px" label="金额">
    <template #default="{ row }">
      <el-input-number
        v-model="row.amount"
        :min="0"
        :max="9999999999"
        :controls="false"
        :step="5"
        :precision="2"
        size="mini"
        placeholder="金额"
        @change="handleAmountChange($event, row)"
      />
    </template>
  </el-table-column>
  <el-table-column v-if="requisitionsSNOptions" prop="requisitionsSN" label="申购单" min-width="130" align="center">
    <template #default="{ row, $index }">
      <common-select
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
  <el-table-column v-if="projectOptions" prop="projectId" align="center" min-width="170px" label="所属项目">
    <template #default="{ row, $index }">
      <common-select
        v-model="row.projectId"
        :options="projectOptions"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        :show-extra="$index !== 0"
        :disabled-val="row.disabledProjectId"
        type="other"
        placeholder="所属项目"
        @change="handleProjectChange($event, $index)"
      />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineEmits, defineProps, computed, watch, onMounted, watchEffect, ref } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { isBlank, isNotBlank, toFixed } from '@/utils/data-type'
import useDittoRealVal from '@/composables/form/use-ditto-real-val'

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
const emit = defineEmits(['amount-change'])
const currentForm = ref({ list: [] })

watchEffect(() => { currentForm.value = props.form })

const {
  initScopeList: initProjectScopeList,
  handleValueChange: handleProjectChange,
  getRealVal: getProjectVal
} = useDittoRealVal('projectId')

const {
  initScopeList: initReqScopeList,
  handleValueChange: handleReqChange,
  getRealVal: getReqVal
} = useDittoRealVal('requisitionsSN')

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

watchEffect(
  () => {
    initReqScopeList(currentForm.value.list || [])
    initProjectScopeList(currentForm.value.list || [])
  }
)

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

onMounted(() => {
  // 首次触发，处理草稿及修改时的首次渲染
  emit('amount-change')
})

// 处理含税单价变化
function handleUnitPriceChange(val, row) {
  row.amount = toFixed(val * row.mete, 2, { toNum: true })
  emit('amount-change')
}

// 处理金额变化
function handleAmountChange(val, row) {
  row.unitPrice = toFixed(val / row.mete, 2, { toNum: true })
  emit('amount-change')
}
</script>
