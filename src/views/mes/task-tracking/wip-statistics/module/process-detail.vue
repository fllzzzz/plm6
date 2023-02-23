<template>
  <common-drawer
    ref="drawerRef"
    :title="`${processInfo.name}工序在制品统计`"
    v-model="processDrawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleAfter>
      <el-tag size="small" effect="plain">
        项目：<span>{{ processData.project?.serialNumber }}-{{ processData.project?.name }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <print-table :api-key="apiKey" :params="{ ...queryParams }" size="mini" type="warning" class="filter-item" />
    </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" row-key="rowId" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" />
        <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" />
        <el-table-column prop="name" :show-overflow-tooltip="true" label="名称" />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" />
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="quantity" label="数量" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="netWeight" label="单重（kg）" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="totalNetWeight" label="总重（kg）" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getProcess } from '@/api/mes/task-tracking/wip-statistics.js'
import { defineProps, defineEmits, ref, computed } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  processInfo: {
    type: Object
  },
  processData: {
    type: Object
  }
})

const { visible: processDrawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableLoading = ref(false)
const list = ref([])
const queryParams = computed(() => {
  return {
    projectId: props.processData.project?.id,
    processId: props.processInfo?.id,
    taskTypeEnum: props.processInfo?.productType
  }
})

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const { content } = await getProcess({
      ...queryParams.value
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
  } catch (error) {
    console.log('获取工序在制品统计详情失败', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
