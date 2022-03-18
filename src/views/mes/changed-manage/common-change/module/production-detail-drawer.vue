<template>
  <common-drawer
    ref="drawerRef"
    :title="`${componentTypeEnum.VL[info?.productType]}处理详情`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="80%"
  >
    <template #content>
      <item-detail-info :productType="info?.productType" :data="headerData" showInProductionQ></item-detail-info>
      <el-divider>
        <span class="title">生产变更</span>
      </el-divider>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :summary-method="getSummaries"
        show-summary
        :data="list"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProductionLine showFactory showProcess />
        <el-table-column prop="reportTypeText" :show-overflow-tooltip="true" label="上报类型" align="center">
          <template #default="{ row }">
            <span>{{ row.reportTypeText }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="completeTime" :show-overflow-tooltip="true" label="上报时间" align="center" width="160">
          <template #default="{ row }">
            <span>{{ row.completeTime }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="quantity" :show-overflow-tooltip="true" label="处理数量" align="center">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/changed-manage/common'
import { defineProps, defineEmits, ref, computed } from 'vue'

import { abnormalReportTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

import itemDetailInfo from '@/views/mes/changed-manage/common-change/components/item-detail-info'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'

const tableRef = ref()
const drawerRef = ref()
const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList, closeHook: init })

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

const headerData = computed(() => {
  return Object.assign(props.info)
})

function init() {
  list.value = []
}

async function fetchList() {
  try {
    tableLoading.value = true
    const { content } = await detail({
      id: props.info?.id
    })
    list.value = content.map((v) => {
      v.reportTypeText = abnormalReportTypeEnum.VL[v.reportType]
      return v
    })
  } catch (error) {
    console.log('获取处理列表失败')
  } finally {
    tableLoading.value = false
  }
}

function getSummaries(param) {
  return tableSummary(param, { props: ['quantity'] })
}
</script>

<style lang="scss" scoped>
.tip {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
  color: red;
  font-size: 13px;
  margin-bottom: 15px;
  line-height: 20px;
  > span {
    display: inline-block;
  }
  > span:nth-child(1) {
    width: 50px;
    flex-shrink: 0;
  }
}
</style>
