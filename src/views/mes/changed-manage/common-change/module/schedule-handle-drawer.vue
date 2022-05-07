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
      <item-detail-info :productType="info?.productType" :data="headerData" showTaskQ showHandleQ></item-detail-info>
      <el-divider>
        <!-- <common-radio-button
          v-model="handleType"
          :options="abnormalHandleTypeEnum.ENUM"
          :disabledVal="[abnormalHandleTypeEnum.PRODUCTION_CHANGE.V]"
          type="enum"
          size="small"
          class="filter-item"
        /> -->
        <span class="title">排产变更</span>
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
        return-source-data
        :show-empty-symbol="false"
        :summary-method="getSummaries"
        show-summary
        :data="showList"
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
    </template>
  </common-drawer>
  <m-preview v-model:visible="previewVisible" :info="info" :list="list" @success="handleSuccess" />
</template>

<script setup>
import { taskList } from '@/api/mes/changed-manage/common'
import { defineProps, defineEmits, ref, computed } from 'vue'
import { ElMessage } from 'element-plus'

import { componentTypeEnum } from '@enum-ms/mes'
import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

import handleOperation from '../components/handle-operation.vue'
import mPreview from './schedule-preview.vue'
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
// const handleType = ref(abnormalHandleTypeEnum.SCHEDULE_CHANGE.V)
const totalHandleQuantity = ref()

const headerData = computed(() => {
  return Object.assign(props.info, { handleQuantity: totalHandleQuantity })
})

function init() {
  list.value = []
  showList.value = []
}

async function fetchList() {
  try {
    tableLoading.value = true
    const { abnormalTaskList, handleQuantity } = await taskList({
      productType: props.info?.productType,
      productId: props.info?.productId
    })
    list.value = abnormalTaskList.map((v) => {
      v.canHandleQuantity = v.taskQuantity - v.inProductionQuantity
      return v
    })
    showList.value = list.value
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
