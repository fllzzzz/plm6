<template>
  <common-drawer
    ref="drawerRef"
    :title="`${isNotBlank(info.handleType) && handleMethodEnumV[info.handleType].L}详情：${info.serialNumber}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleRight>
      <common-button size="mini" type="primary" :loading="submitLoading" @click="submit">提交</common-button>
      <common-button size="mini" type="warning" @click="reset">重置</common-button>
    </template>
    <template #content>
      <div class="tip">
        <span>* 注意：</span>
        <span>
          可操作{{ isNotBlank(info.handleType) && handleMethodEnumV[info.handleType].L }}的数量总和为{{
            info.canHandleTotalMete
          }}，请谨慎操作！</span
        >
      </div>
      <div class="list-content">
        <div class="handle-content">
          <div class="handle-title">
            <el-tag size="medium" effect="plain">可处理列表</el-tag>
            <span>
              <!-- <common-button size="mini" :disabled="remainHandleMete <= 0 || !canHandleList.length" type="success">一键处理</common-button> -->
            </span>
          </div>
          <common-table ref="tableRef" v-loading="tableLoading" :data="canHandleList" :max-height="maxHeight" style="width: 100%">
            <el-table-column label="序号" type="index" align="center" width="60" />
            <!-- <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号">
              <template v-slot="scope">
                <span>{{ scope.row.serialNumber }}</span>
              </template>
            </el-table-column> -->
            <belonging-info-columns showProductionLine showFactory />
            <!-- <el-table-column prop="teamName" :show-overflow-tooltip="true" label="班组">
              <template v-slot="scope">
                <span>{{ scope.row.teamName }}</span>
              </template>
            </el-table-column> -->
            <template v-for="item in handleMethodEnumV[info.handleType].COLUMNS || []" :key="item.field">
              <el-table-column :prop="item.field" :show-overflow-tooltip="true" :label="item.label" :width="item.width" :align="item.align">
                <template v-slot="scope">
                  <span>{{ scope.row[item.field] }}</span>
                </template>
              </el-table-column>
            </template>
            <el-table-column label="操作" width="80" align="center">
              <template v-slot="scope">
                <common-button
                  type="primary"
                  size="mini"
                  icon="el-icon-arrow-right"
                  :disabled="remainHandleMete <= 0"
                  @click="addRow(scope.$index, scope.row)"
                />
              </template>
            </el-table-column>
          </common-table>
        </div>
        <div class="preview-content">
          <div class="handle-title">
            <span>
              <el-tag size="medium" effect="plain" type="warning">预览列表</el-tag>
              <el-tag size="medium" effect="plain" type="danger" style="margin-left: 10px">剩余可操作数量：{{ remainHandleMete }}</el-tag>
            </span>
          </div>
          <common-table ref="tableRef" v-loading="tableLoading" :data="previewList" :max-height="maxHeight" style="width: 100%">
            <el-table-column label="序号" type="index" align="center" width="60" />
            <!-- <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号">
              <template v-slot="scope">
                <span>{{ scope.row.serialNumber }}</span>
              </template>
            </el-table-column> -->
            <belonging-info-columns showProductionLine showFactory />
            <!-- <el-table-column prop="teamName" :show-overflow-tooltip="true" label="班组">
              <template v-slot="scope">
                <span>{{ scope.row.teamName }}</span>
              </template>
            </el-table-column> -->
            <template v-for="item in handleMethodEnumV[info.handleType].COLUMNS || []" :key="item.field">
              <el-table-column
                v-if="item.preview"
                :prop="item.field"
                :show-overflow-tooltip="true"
                :label="item.label"
                :width="item.width"
                :align="item.align"
              >
                <template v-slot="scope">
                  <span>{{ scope.row[item.field] }}</span>
                </template>
              </el-table-column>
            </template>
            <el-table-column prop="dealMete" :show-overflow-tooltip="true" label="处理量" width="150px" align="center">
              <template v-slot="scope">
                <el-input-number
                  v-model="scope.row.dealMete"
                  :min="1"
                  :max="getMax(scope.row)"
                  style="width: 100%"
                  size="mini"
                  controls-position="right"
                ></el-input-number>
              </template>
            </el-table-column>
            <el-table-column label="操作" width="80" align="center">
              <template v-slot="scope">
                <common-button type="danger" size="mini" icon="el-icon-delete" @click="delRow(scope.$index, scope.row)" />
              </template>
            </el-table-column>
          </common-table>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { exceptionList, exceptionChange, taskChange } from '@/api/mes/changed-manage/artifact'
import { taskList } from '@/api/mes/changed-manage/common'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'
import { ElNotification, ElMessage } from 'element-plus'

import { abnormalReportTypeEnum } from '@enum-ms/mes'
import { deepClone, isNotBlank } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
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

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

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
const handleMethodEnumV = inject('handleMethodEnumV')
const handleMethodEnum = inject('handleMethodEnum')
const tableLoading = ref(false)
const submitLoading = ref(false)
const originList = ref([])
const processLimitObj = ref({}) // 异常处理限制
const canHandleList = ref([])
const previewList = ref([])

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    } else {
      init()
    }
  },
  { immediate: true }
)

const alreadyHandleMete = computed(() => {
  return previewList.value.reduce((pre, curr) => {
    if (curr.dealMete) {
      return pre + curr.dealMete
    } else {
      return pre
    }
  }, 0)
})

const remainHandleMete = computed(() => {
  return props.info.canHandleTotalMete - alreadyHandleMete.value
})

function init() {
  canHandleList.value = []
  previewList.value = []
}

function reset() {
  canHandleList.value = deepClone(originList.value)
  previewList.value = []
}

async function fetchList() {
  let _list = []
  try {
    processLimitObj.value = {}
    tableLoading.value = true
    if (props.info.handleType === handleMethodEnum.DECREASE_TASK.V) {
      const { content } = await taskList({
        productType: props.info.productType,
        productId: props.info.productId
      })
      _list = content.map((v) => {
        v.canHandleMete = v.taskQuantity - v.completeQuantity
        return v
      })
    } else {
      const { reportList, limitList } = await exceptionList({
        productType: props.info.productType,
        productId: props.info.productId
      })
      _list = reportList.map((v) => {
        v.reportTypeText = abnormalReportTypeEnum.VL[v.reportType]
        v.canHandleMete = v.quantity
        return v
      })
      limitList.forEach((o) => {
        processLimitObj.value[o.processId] = o.quantity
      })
    }
  } catch (error) {
    console.log('获取处理列表失败', error)
  } finally {
    console.log(processLimitObj.value)
    originList.value = deepClone(_list)
    canHandleList.value = deepClone(_list)
    tableLoading.value = false
  }
}

function getCanHandleMete(row) {
  if (props.info.handleType === handleMethodEnum.EXCEPTION_HANDLE.V) {
    return row.canHandleMete > processLimitObj.value[row.processId] ? processLimitObj.value[row.processId] : row.canHandleMete
  } else {
    return row.canHandleMete
  }
}

function getMax(row) {
  const _quantity = getCanHandleMete(row)
  if (_quantity < row.dealMete + remainHandleMete.value) {
    return _quantity
  } else {
    return row.dealMete + remainHandleMete.value
  }
}

function addRow(index, row) {
  const _quantity = getCanHandleMete(row)
  if (_quantity > remainHandleMete.value) {
    row.dealMete = remainHandleMete.value
  } else {
    row.dealMete = _quantity
  }
  previewList.value.push({ ...row })
  canHandleList.value.splice(index, 1)
}

function delRow(index, row) {
  delete row.dealMete
  canHandleList.value.push({ ...row })
  previewList.value.splice(index, 1)
}

async function submit() {
  if (remainHandleMete.value > 0) {
    ElMessage.warning('存在未处理数量！')
    return
  }
  if (!previewList.value || !previewList.value.length) {
    return
  }
  try {
    submitLoading.value = true
    // 处理异常处理的提交
    if (props.info.handleType === handleMethodEnum.EXCEPTION_HANDLE.V) {
      const submitList = previewList.value.map((v) => {
        const o = {}
        o.id = v.id
        o.quantity = v.dealMete
        return o
      })
      await exceptionChange({
        productType: props.info.productType,
        abnormalId: props.info.id,
        voList: submitList
      })
    }
    // 处理多余任务的提交
    if (props.info.handleType === handleMethodEnum.DECREASE_TASK.V) {
      const submitList = previewList.value.map((v) => {
        const o = {}
        o.taskId = v.taskId
        o.quantity = v.dealMete
        return o
      })
      await taskChange({
        productType: props.info.productType,
        abnormalId: props.info.id,
        taskChange: submitList
      })
    }
    ElNotification({
      title: '变更处理成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('变更处理失败', error)
  } finally {
    submitLoading.value = false
  }
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

.list-content {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: flex-start;

  .handle-content {
    box-sizing: border-box;
    padding-right: 20px;
    flex: auto;
    min-width: 260px;
    width: calc(100% - 980px);
  }
  .preview-content {
    flex: none;
    width: 980px;
  }
}

.handle-title {
  padding: 5px 0px;
  box-sizing: border-box;
  font-weight: bold;
  color: #303133;
  display: flex;
  justify-content: space-between;
  align-items: center;
}
</style>
