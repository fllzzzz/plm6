<template>
  <common-drawer
    ref="drawerRef"
    :title="`${props.detailData?.workshop?.name}>${props.detailData?.productionLine?.name}钻孔详情`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="60%"
  >
    <template #titleAfter>
      <common-radio-button
        v-model="orderType"
        :options="typeEnum.ENUM"
        type="enum"
        size="mini"
        class="filter-item"
      />
    </template>
    <template #titleRight>
      <!-- <print-table
        api-key="mesDrillProductionTaskOrder"
        :params="{ ...commonParams }"
        size="mini"
        type="warning"
        class="filter-item"
      /> -->
      <common-button size="mini" icon="el-icon-printer" type="success" @click="printIt">打印【任务单、分拣单】</common-button>
    </template>
    <template #content>
      <div v-if="orderType === typeEnum.PRODUCTION_TASK_ORDER.V">
        <common-table
          v-loading="taskLoading"
          ref="table"
          :data="drillData"
          empty-text="暂无数据"
          :max-height="maxHeight"
          style="width: 100%"
          show-summary
          :summary-method="getSummaries"
        >
          <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="120">
            <template v-slot="scope">
              <span>{{ projectNameFormatter(scope.row.project) }}</span>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="specification" key="specification" label="规格" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="material" key="material" label="材质" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="数量" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="weight" key="weight" label="重量（kg）" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="picturePath" key="picturePath" label="图形" align="center">
            <template v-slot="scope">
              <el-image style="width: 100%; height: 100%" :src="scope.row.picturePath" fit="scale-down" />
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div v-loading="separateLoading" v-if="orderType === typeEnum.SORTING_ORDER.V">
        <separate-order-table :separateOrderInfo="separateOrderInfo" />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import fetchFn from '@/utils/print/api'
import { showDrillDetail, printSign } from '@/api/mes/work-order-manage/machine-part.js'
import { defineProps, defineEmits, ref, computed } from 'vue'
import { ElNotification, ElLoading } from 'element-plus'

import { drillListEnum as typeEnum } from '@enum-ms/mes'
import { printModeEnum } from '@/utils/print/enum'
import { tableSummary } from '@/utils/el-extra'
import { projectNameFormatter } from '@/utils/project'
import { printSeparateOrderLabel } from '@/utils/print/index'
import { codeWait } from '@/utils'
import printTemplate from '@/utils/print/default-template'
import { printTable } from '@/utils/print/table'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useGetSeparateOrder from '@compos/mes/work-order-manage/use-get-separate-order'
import separateOrderTable from './separate-order-table'

const emit = defineEmits(['update:visible'])
const drawerRef = ref()
const drillData = ref([]) // 钻孔工单详情数据
const taskLoading = ref(false)
const orderType = ref(typeEnum.PRODUCTION_TASK_ORDER.V)

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  processType: {
    type: Number
  }
})

const { maxHeight } = useMaxHeight(
  {
    wrapperBox: ['.el-drawer__body'],
    navbar: false
  },
  drawerRef
)

const taskOrderPrintKey = 'mesDrillProductionTaskOrder'
const commonParams = computed(() => {
  return { cutId: props.detailData.id, processType: props.processType }
})
const { separateLoading, separateOrderInfo, fetchSeparateOrder } = useGetSeparateOrder(commonParams)
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

async function showHook() {
  await drillDetailGet()
  await fetchSeparateOrder()
}

async function drillDetailGet() {
  try {
    taskLoading.value = true
    const data = await showDrillDetail({ ...commonParams.value })
    drillData.value = data
  } catch (error) {
    console.log('获取钻孔工单详情失败', error)
  } finally {
    taskLoading.value = false
  }
}

// --------------------------- 打印 start ------------------------------
const printLoading = ref()

async function printIt() {
  printLoading.value = ElLoading.service({
    lock: true,
    text: '正在准备加入打印队列',
    spinner: 'el-icon-loading',
    fullscreen: true
  })
  try {
    // ---------------------------生产任务单 打印 start ------------------------------
    printLoading.value.text = `正在加载数据：生产任务单`
    const config = printTemplate[taskOrderPrintKey]
    const { header, footer, table, qrCode } = (await fetchFn[taskOrderPrintKey]({ ...commonParams.value })) || {}
    printLoading.value.text = `正在加入打印队列：生产任务单`
    await codeWait(500)
    const result = await printTable({
      printMode: printModeEnum.QUEUE.V,
      header,
      footer,
      table,
      qrCode,
      config
    })
    if (!result) {
      throw new Error('导出失败')
    }
    // ---------------------------生产任务单 打印 end --------------------------------
    // --------------------------- 分拣单 打印 start ------------------------------
    printLoading.value.text = `正在加入打印队列：分拣单`
    await codeWait(500)
    await printSeparateOrderLabel({ taskNumberOrder: props.detailData.orderNumber, separateOrderInfo: separateOrderInfo.value })
    // --------------------------- 分拣单 打印 end --------------------------------
    printLoading.value.text = `已全部加入打印队列`
    await codeWait(500)
  } catch (error) {
    ElNotification({ title: '加入打印队列失败，请重试', type: 'error', duration: 2500 })
    throw new Error(error)
  } finally {
    printLoading.value.close()
    await printSign({ ...commonParams.value })
    emit('refresh')
  }
}

// --------------------------- 打印 end --------------------------------

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'weight']
  })
}
</script>

<style lang="scss" scoped></style>
