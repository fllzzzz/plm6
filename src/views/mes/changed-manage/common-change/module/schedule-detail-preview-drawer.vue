<template>
  <common-drawer
    ref="drawerRef"
    :title="`${componentTypeEnum.VL[info?.productType]}处理预览详情`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="80%"
  >
    <template #content>
      <item-detail-info :productType="info?.productType" :data="headerData" showTaskQ></item-detail-info>
      <el-divider>
        <span class="title">排产变更</span>
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
        <el-table-column prop="askCompleteTime" :show-overflow-tooltip="true" label="要求完成日期" align="center">
          <template #default="{ row }">
            <span>{{ row.askCompleteTime }}</span>
          </template>
        </el-table-column>
        <belonging-info-columns showProductionLine showFactory />
        <el-table-column prop="taskQuantity" :show-overflow-tooltip="true" label="任务数量" align="center">
          <template #default="{ row }">
            <span>{{ row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="inProductionQuantity" :show-overflow-tooltip="true" label="已生产数量" align="center">
          <template #default="{ row }">
            <span>{{ row.inProductionQuantity }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detailPreview } from '@/api/mes/changed-manage/common'
import { defineProps, defineEmits, ref, computed } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
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
const totalHandleQuantity = ref()

const headerData = computed(() => {
  return Object.assign(props.info, { handleQuantity: totalHandleQuantity })
})

function init() {
  list.value = []
}

async function fetchList() {
  try {
    tableLoading.value = true
    const { preview, handleQuantity } = await detailPreview({
      id: props.info?.id
    })
    list.value = preview.map((v) => {
      v.canHandleQuantity = v.taskQuantity - v.inProductionQuantity
      return v
    })
    totalHandleQuantity.value = handleQuantity
  } catch (error) {
    console.log('获取处理列表失败')
  } finally {
    tableLoading.value = false
  }
}

function getSummaries(param) {
  return tableSummary(param, { props: ['taskQuantity', 'inProductionQuantity', 'canHandleQuantity', 'dealQuantity'] })
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
