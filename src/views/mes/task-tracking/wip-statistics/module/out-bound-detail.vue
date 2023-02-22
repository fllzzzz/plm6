<template>
  <common-drawer ref="drawerRef" title="钢材出库记录" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <el-tag size="mini" effect="plain">
        项目：<span>{{ info.project?.serialNumber }}-{{ info.project?.name }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <print-table :api-key="apiKey" :params="{ ...queryParams }" size="mini" type="warning" class="filter-item" />
    </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" row-key="rowId" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="type" :show-overflow-tooltip="true" label="物料种类" min-width="150">
          <template #default="{ row }">
            <span>{{ row.project }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center">
          <template #default="{ row }">
            <span>{{ row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center">
          <template #default="{ row }">
            <span>{{ row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="accountingUnit" label="计量单位">
          <template #default="{ row }">
            <span>{{ row.accountingUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="quantity" label="数量">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="accountingUnit" label="核算单位" width="120">
          <template #default="{ row }">
            <span>{{ row.accountingUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="mete" label="核算量" width="100">
          <template #default="{ row }">
            <span>{{ row.mete }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="createTime" label="出库日期" width="120">
          <template #default="{ row }">
            <span>{{ row.createTime }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getOutbound } from '@/api/mes/task-tracking/wip-statistics.js'
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
  info: {
    type: Object
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

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
    projectId: props.info.project?.id,
    type: props.info.project?.type
  }
})

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const content = await getOutbound({
      ...queryParams.value
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
  } catch (error) {
    console.log('获取出库记录详情失败', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
