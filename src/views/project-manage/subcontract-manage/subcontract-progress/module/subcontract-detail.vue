<template>
  <common-drawer
    ref="drawerRef"
    title="详情"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #titleAfter>
      <el-tag effect="plain" size="medium" v-if="detailInfo.supplierName">分包单位：{{detailInfo.supplierName}}</el-tag>
    </template>
    <template #content>
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip />
        <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip />
        <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip />
        <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip />
        <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip />
        <el-table-column prop="quantity" label="任务数" align="center" show-overflow-tooltip />
        <el-table-column prop="actualQuantity" label="完成数" align="center" show-overflow-tooltip />
        <el-table-column prop="actualMete" label="完成量" align="center" show-overflow-tooltip />
        <el-table-column prop="accountUnit" label="单位" align="center" width="70" show-overflow-tooltip />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { get as getSum } from '@/api/project-manage/subcontract-progress'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import { installProjectTypeEnum } from '@enum-ms/project'
import { projectTypeEnum } from '@enum-ms/contract'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })

// 请求参数
const params = computed(() => {
  return {
    projectId: props.detailInfo.projectId,
    supplierId: props.detailInfo.supplierId,
    monomerId: props.detailInfo.monomerId,
    areaId: props.detailInfo.areaId,
    monomerDetailType: props.detailInfo.type
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const dataFormat = ref([
  ['actualMete', ['to-fixed', 2]]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.subcontract-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['actualMete']
  })
  return summary
}

// 获取分包商任务
async function fetchList() {
  let _list = []
  if (!params.value.supplierId) {
    return
  }
  tableLoading.value = true
  try {
    const { content = [] } = await getSum({ ...params.value })
    content.map(v => {
      v.accountUnit = props.globalProject.projectType === projectTypeEnum.BRIDGE.V ? 't' : installProjectTypeEnum.V[v.productType].accountUnit
    })
    _list = content
  } catch (error) {
    console.log('获取分包商任务', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
