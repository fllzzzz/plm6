<template>
  <common-drawer
    ref="drawerRef"
    :title="`${componentTypeEnum.VL[info?.productType]}异常处理`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="80%"
  >
    <template #titleRight>
      <common-button size="mini" type="primary" @click="previewIt">提交预览</common-button>
    </template>
    <template #content>
      <item-detail-info :productType="info?.productType" :data="headerData" showInProductionQ showHandleQ></item-detail-info>
      <el-divider>
        <!-- <common-radio-button v-model="handleType" :options="abnormalHandleTypeEnum.ENUM" type="enum" size="small" class="filter-item" /> -->
        <span class="title">生产变更</span>
      </el-divider>
      <handle-operation
        :productType="info?.productType"
        @showEditList="showEditList"
        @showAllList="showAllList"
        @fill="fill"
        @reset="reset"
      />
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :summary-method="getSummaries"
        show-summary
        :data="showList"
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
        <el-table-column prop="quantity" :show-overflow-tooltip="true" label="生产数量" align="center">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="canHandleQuantity" :show-overflow-tooltip="true" label="可处理数量" align="center">
          <template #default="{ row }">
            <span>{{ row.canHandleQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="dealQuantity" :show-overflow-tooltip="true" label="处理数量" align="center">
          <template #default="{ row }">
            <el-input-number
              v-model="row.dealQuantity"
              :min="0"
              :max="getMax(row)"
              :disabled="!getMax(row)"
              style="width: 100%"
              size="mini"
              controls-position="right"
            ></el-input-number>
          </template>
        </el-table-column>
      </common-table>
      <div style="font-size: 9pt; color: red; margin-top: 10px">
        <span>*</span>
        <span>被处理的{{ componentTypeEnum.VL[info?.productType] }}会加入到多余产品列表，可在该列表中进行报废或二次利用 </span>
      </div>
    </template>
  </common-drawer>
  <m-preview v-model:visible="previewVisible" :info="info" :list="list" @success="handleSuccess" />
</template>

<script setup>
import { exceptionList } from '@/api/mes/changed-manage/common'
import { defineProps, defineEmits, ref, computed } from 'vue'
import { ElMessage } from 'element-plus'

import { abnormalReportTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

import handleOperation from '../components/handle-operation.vue'
import mPreview from './production-preview.vue'
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
const previewVisible = ref(false)
const list = ref([])
const showList = ref([])
// const handleType = ref(abnormalHandleTypeEnum.PRODUCTION_CHANGE.V)
const totalHandleQuantity = ref()
const processLimitObj = ref({}) // 异常处理限制

const headerData = computed(() => {
  return Object.assign(props.info, { handleQuantity: totalHandleQuantity.value })
})

function init() {
  list.value = []
  showList.value = []
}

async function fetchList() {
  processLimitObj.value = {}
  try {
    tableLoading.value = true
    const { reportList, limitList, quantity } = await exceptionList({
      id: props.info?.id
    })
    for (let i = 0; i < limitList.length; i++) {
      const _i = limitList[i]
      processLimitObj.value[_i.taskId] = {}
      for (let x = 0; x < _i.processAndQuantitys.length; x++) {
        const { processId, quantity } = _i.processAndQuantitys[x]
        processLimitObj.value[_i.taskId][parseInt(processId)] = quantity || 0
      }
    }
    list.value = reportList.map((v) => {
      v.reportTypeText = abnormalReportTypeEnum.VL[v.reportType]
      const limitQuantity = (processLimitObj.value[v.taskId] && processLimitObj.value[v.taskId][parseInt(v.processId)]) || 0
      v.canHandleQuantity = Math.min(limitQuantity, v.quantity)
      return v
    })
    showList.value = list.value
    totalHandleQuantity.value = quantity
  } catch (error) {
    console.log('获取处理列表失败')
  } finally {
    tableLoading.value = false
  }
}

function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'canHandleQuantity', 'dealQuantity'] })
}

const alreadyHandleQuantity = computed(() => {
  return list.value.reduce((pre, curr) => {
    if (curr.dealQuantity) {
      return pre + curr.dealQuantity
    } else {
      return pre
    }
  }, 0)
})

const remainHandleQuantity = computed(() => {
  return totalHandleQuantity.value - alreadyHandleQuantity.value
})

function getMax(row) {
  return Math.min(row.canHandleQuantity, remainHandleQuantity.value + (row.dealQuantity || 0))
}

function previewIt() {
  if (remainHandleQuantity.value > 0) {
    ElMessage.warning('存在未处理数量！')
    return
  }
  previewVisible.value = true
}

function handleSuccess() {
  handleClose()
  emit('success')
}

function showEditList(val) {
  showList.value = showList.value.filter((v) => v.dealQuantity)
}
function showAllList(val) {
  showList.value = list.value
}

function fill() {
  showList.value = showList.value.map((v) => {
    v.dealQuantity = getMax(v) || undefined
    return v
  })
}

function reset() {
  showList.value = showList.value.map((v) => {
    v.dealQuantity = undefined
    return v
  })
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
