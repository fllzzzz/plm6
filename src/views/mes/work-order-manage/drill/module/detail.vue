<template>
  <common-drawer
    ref="drawerRef"
    :title="`${props.detailData?.workshop?.name}>${props.detailData?.productionLine?.name}钻孔详情`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="63%"
  >
    <template #titleAfter>
      <common-radio-button
        v-model="orderType"
        :options="typeEnum.ENUM"
        type="enum"
        :disabledVal="!separateOrderInfo.length ? [typeEnum.SORTING_ORDER.V] : []"
        size="mini"
        class="filter-item"
      />
    </template>
    <template #titleRight>
      <common-button
        v-show="orderType === typeEnum.PRODUCTION_TASK_ORDER.V"
        v-permission="permission.print"
        size="mini"
        icon="el-icon-printer"
        type="success"
        @click="printIt"
        >打印【钻孔任务单】</common-button
      >
      <common-button
        v-show="orderType === typeEnum.SORTING_ORDER.V"
        v-permission="permission.print"
        size="mini"
        icon="el-icon-printer"
        type="success"
        @click="printIt"
        >打印【分拣单】</common-button
      >
    </template>
    <template #content>
      <div v-if="orderType === typeEnum.PRODUCTION_TASK_ORDER.V">
        <production-task-order :tableData="drillData" :maxHeight="maxHeight" :tableLoading="taskLoading" />
      </div>
      <div v-loading="separateLoading" v-if="orderType === typeEnum.SORTING_ORDER.V">
        <separate-order-table :separateOrderInfo="separateOrderInfo" />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import fetchFn from '@/utils/print/api'
import { productionTaskDetail, printSign } from '@/api/mes/work-order-manage/machine-part.js'
import { defineProps, defineEmits, ref, computed, inject } from 'vue'
import { ElNotification, ElLoading } from 'element-plus'

import { drillListEnum as typeEnum } from '@enum-ms/mes'
import { printModeEnum } from '@/utils/print/enum'

import { printSeparateOrderLabel } from '@/utils/print/index'
import { codeWait } from '@/utils'
import { printTable } from '@/utils/print/table'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useDefaultTableTemplate from '@compos/use-default-table-template'
import useGetSeparateOrder from '@compos/mes/work-order-manage/use-get-separate-order'
import separateOrderTable from '../../components/separate-order-table'
import productionTaskOrder from '../../components/production-task-order.vue'

const permission = inject('permission')
const emit = defineEmits(['update:visible', 'refresh'])
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
  },
  serialNumber: {
    type: String
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
  return { cutId: props.detailData.id, processType: props.processType, serialNumber: props.serialNumber }
})
const { separateLoading, separateOrderInfo, fetchSeparateOrder } = useGetSeparateOrder(commonParams)
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

async function showHook() {
  orderType.value = typeEnum.PRODUCTION_TASK_ORDER.V
  await drillDetailGet()
  await fetchSeparateOrder()
}

async function drillDetailGet() {
  try {
    taskLoading.value = true
    const data = await productionTaskDetail({ ...commonParams.value })
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
    // ---------------------------钻孔任务单 打印 start ------------------------------
    if (orderType.value === typeEnum.PRODUCTION_TASK_ORDER.V) {
      printLoading.value.text = `正在加入打印队列：钻孔生产任务单`
      const config = await useDefaultTableTemplate(taskOrderPrintKey)
      const { header, footer, table, qrCode } = (await fetchFn[taskOrderPrintKey]({ ...commonParams.value })) || {}
      await codeWait(500)
      printLoading.value.text = `已全部加入打印队列`
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
      ElNotification({ title: '打印钻孔生产任务单成功', type: 'success', duration: 2500 })
    } else {
      // ---------------------------生产任务单 打印 end --------------------------------
      // --------------------------- 分拣单 打印 start ------------------------------
      printLoading.value.text = `正在加入打印队列：分拣单`
      await codeWait(500)
      await printSeparateOrderLabel({ taskNumberOrder: props.detailData.orderNumber, separateOrderInfo: separateOrderInfo.value })
      // --------------------------- 分拣单 打印 end --------------------------------
      printLoading.value.text = `已全部加入打印队列`
      await codeWait(500)
      ElNotification({ title: '打印分拣单成功', type: 'success', duration: 2500 })
    }
    printLoading.value.close()
    await printSign({ ...commonParams.value })
    emit('refresh')
    return
  } catch (error) {
    ElNotification({ title: '加入打印队列失败，请重试', type: 'error', duration: 2500 })
    throw new Error(error)
  }
}

// --------------------------- 打印 end --------------------------------
</script>

<style lang="scss" scoped></style>
