<template>
  <common-drawer
    ref="drawerRef"
    customClass="cutting-detail"
    :title="`${props.cuttingDetailData?.workshop?.name}>${props.cuttingDetailData?.productionLine?.name}切割详情`"
    v-model="cuttingDrawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="63%"
  >
    <template #titleAfter>
      <common-radio-button
        v-model="orderType"
        :options="typeEnum.ENUM"
        :unshowVal="cuttingDetailData.boolNestCut ? [typeEnum.PRODUCTION_TASK_ORDER.V] : [typeEnum.NESTING_TASK_ORDER.V]"
        type="enum"
        size="mini"
        class="filter-item"
      />
    </template>
    <template #titleRight>
      <common-button size="mini" icon="el-icon-printer" type="success" @click="printIt">打印【任务单、分拣单】</common-button>
    </template>
    <template #content>
      <!--任务单-->
      <div v-loading="taskLoading" :style="`height:${maxHeight}px`" v-show="orderType === typeEnum.NESTING_TASK_ORDER.V">
        <pdf :url="taskOrderPDF" :type="'canvas'" :pdfjsDistPath="pdfjsDistPath" />
      </div>
      <!-- 生产任务单 -->
      <div v-show="orderType === typeEnum.PRODUCTION_TASK_ORDER.V">
        <production-task-order :tableData="productionData" :maxHeight="maxHeight" :tableLoading="taskLoading"/>
      </div>
      <!--分拣单-->
      <div v-loading="separateLoading" v-show="orderType === typeEnum.SORTING_ORDER.V">
        <separate-order-table :separateOrderInfo="separateOrderInfo" />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import fetchFn from '@/utils/print/api'
import { showCuttingPdf, productionTaskDetail, printSign } from '@/api/mes/work-order-manage/machine-part.js'
import { defineProps, defineEmits, ref, computed } from 'vue'
import { ElNotification, ElLoading } from 'element-plus'

import { sortingListEnum as typeEnum } from '@enum-ms/mes'

import { printPDFJSCanvas } from '@/utils/print/pdf-print'
import { printSeparateOrderLabel } from '@/utils/print/index'
import printTemplate from '@/utils/print/default-template'
import { printTable } from '@/utils/print/table'
import { printModeEnum } from '@/utils/print/enum'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import useGetSeparateOrder from '@compos/mes/work-order-manage/use-get-separate-order'
import separateOrderTable from './separate-order-table'
import productionTaskOrder from './production-task-order'
import pdf from '@/components/PDF/pdf'

import { codeWait } from '@/utils'

const pdfjsDistPath = import.meta.env.BASE_URL + 'assets'
const emit = defineEmits(['update:visible', 'refresh'])
const drawerRef = ref()

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  cuttingDetailData: {
    type: Object,
    default: () => {}
  },
  processType: {
    type: Number
  }
})

const taskOrderPrintKey = 'mesNestingProductionTaskOrder'
const taskOrderPDF = ref('')
const taskLoading = ref(false)
const productionData = ref([])
const orderType = ref(typeEnum.NESTING_TASK_ORDER.V)

const { maxHeight } = useMaxHeight({
  mainBox: '.cutting-detail',
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body'],
  navbar: false,
  extraHeight: 100,
  clientHRepMainH: true
})

const commonParams = computed(() => {
  return { cutId: props.cuttingDetailData.id, processType: props.processType }
})
const { separateLoading, separateOrderInfo, fetchSeparateOrder } = useGetSeparateOrder(commonParams)
const { visible: cuttingDrawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

async function showHook() {
  if (props.cuttingDetailData.boolNestCut) {
    orderType.value = typeEnum.NESTING_TASK_ORDER.V
    await nestingDetailGet()
  } else {
    orderType.value = typeEnum.PRODUCTION_TASK_ORDER.V
    await productionDetailGet()
  }

  await fetchSeparateOrder()
}

// --------------------------- 获取生产任务单 start ------------------------------

async function productionDetailGet() {
  try {
    taskLoading.value = true
    const data = await productionTaskDetail({ ...commonParams.value })
    productionData.value = data
  } catch (error) {
    console.log('获取生产任务单详情失败', error)
  } finally {
    taskLoading.value = false
  }
}
// --------------------------- 获取生产任务单 end --------------------------------

// --------------------------- 获取任务单 start ------------------------------

async function nestingDetailGet() {
  try {
    taskLoading.value = true
    taskOrderPDF.value = ''
    const data = await showCuttingPdf({ ...commonParams.value })
    taskOrderPDF.value = await getUrlByFileReader(data)
  } catch (error) {
    console.log('获取套料任务单失败', error)
  } finally {
    taskLoading.value = false
  }
}

// 转化为文件流
function getUrlByFileReader(res) {
  return new Promise((resolve, reject) => {
    if (res && res.data && res.data.size) {
      const dataInfo = res.data
      const reader = new window.FileReader()
      // 使用readAsArrayBuffer读取文件, result属性中将包含一个 ArrayBuffer 对象以表示所读取文件的数据
      reader.readAsArrayBuffer(dataInfo)
      reader.onload = function (e) {
        console.log(e, dataInfo, 'rrrr')
        const result = e.target.result
        const contentType = dataInfo.type
        // 生成blob图片,需要参数(字节数组, 文件类型)
        const blob = new Blob([result], { type: contentType })
        // 使用 Blob 创建一个指向类型化数组的URL, URL.createObjectURL是new Blob文件的方法,可以生成一个普通的url,可以直接使用,比如用在img.src上
        const url = window.URL.createObjectURL(blob)
        console.log(url) // 产生一个类似 blob:d3958f5c-0777-0845-9dcf-2cb28783acaf 这样的URL字符串
        resolve(url)
      }
    } else {
      reject()
    }
  })
}
// --------------------------- 获取任务单 end --------------------------------

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
    if (props.cuttingDetailData.boolNestCut) {
      // --------------------------- PDF 打印 start ------------------------------
      const canvasELs = document.querySelectorAll('#viewerContainer .canvasWrapper canvas')
      for (let i = 0; i < canvasELs.length; i++) {
        const canvasBase64 = canvasELs[i].toDataURL()
        printLoading.value.text = `正在加入打印队列：套料任务单 第${i + 1}页`
        await codeWait(500)
        await printPDFJSCanvas({ canvasBase64 })
      }
    // --------------------------- PDF 打印 end --------------------------------
    } else {
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
    }

    // --------------------------- 分拣单 打印 start ------------------------------
    printLoading.value.text = `正在加入打印队列：分拣单`
    await codeWait(500)
    await printSeparateOrderLabel({ taskNumberOrder: props.cuttingDetailData.orderNumber, separateOrderInfo: separateOrderInfo.value })
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
</script>

<style lang="scss" scoped>
::v-deep(.pdfViewer .page) {
  margin: 1px;
}
</style>
