<template>
  <common-drawer ref="drawerRef" title="生产任务单" v-model:visible="drawerVisible" direction="rtl" :before-close="handleClose" size="87%">
    <template #titleAfter>
      <el-tag>项目：{{ props.detailData.project?.name }}</el-tag>
      <el-tag>产线：{{ props.detailData.workshop?.name }}>{{ props.detailData.productionLine?.name }}</el-tag>
      <el-tag>总量：{{ totalQuantity }}/{{ totalWeight?.toFixed(2) }}</el-tag>
      <el-tag>任务单号：{{ props.detailData.scheduleOrder }}</el-tag>
    </template>
    <template #content>
      <div class="head-container">
        <common-radio-button
          v-model="processId"
          :options="processList"
          type="other"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          size="small"
          showOptionAll
          class="filter-item"
          @change="handleProcessChange"
        />
        <common-radio-button
          v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V"
          v-model="listType"
          :options="typeEnum.ENUM"
          type="enum"
          size="small"
          class="filter-item"
          @change="handleTypeChange"
        />
        <common-button
          class="filter-item"
          v-permission="permission.allRevoke"
          v-show="isEdit === true"
          :disabled="isAllOrder === false"
          size="mini"
          type="danger"
          @click="taskOrderBack"
        >
          全部工单撤回
        </common-button>
        <div style="float: right">
          <!-- <print-table
            v-permission="permission.print"
            v-if="props.detailData.productType === componentTypeEnum.ARTIFACT.V"
            api-key="mesProductionTaskOrder"
            :params="{ ...params }"
            @print-success="addPrintRecord"
            size="mini"
            type="warning"
            class="filter-item"
          /> -->
          <!-- <print-table
            v-permission="permission.print"
            v-show="props.detailData.productType === componentTypeEnum.ASSEMBLE.V"
            :api-key="listType === typeEnum.TASK_LIST.V ? 'mesAssembleProductionTaskOrder' : 'mesAssembleNestingOrder'"
            :params="{ ...params, type: typeEnum.TASK_LIST.V }"
            @print-success="addPrintRecord"
            size="mini"
            type="warning"
            class="filter-item"
          /> -->
          <common-button v-permission="permission.cancelEdit" v-show="isEdit" size="mini" type="warning" @click="cancelEdit">取消编辑</common-button>
          <common-button v-permission="permission.edit" v-show="!processId && isEdit === false && props.detailData.productType === componentTypeEnum.ARTIFACT.V" size="mini" type="primary" @click="editMode">编辑</common-button>
          <el-popover
            v-model:visible="delBtn"
            placement="bottom"
            width="180"
            trigger="click"
            @show="onPopoverBatchClickShow"
            @hide="onPopoverBatchClickHide"
          >
            <p>是否确认批量撤回操作？</p>
            <div style="text-align: right; margin: 0">
              <common-button size="mini" type="text" @click.stop="cancelBatch">取消</common-button>
              <common-button size="mini" type="danger" @click="batchBack">确定</common-button>
            </div>
            <template #reference>
              <common-button v-permission="permission.batchRevoke" v-show="isEdit === true" size="mini" type="danger" :disabled="!selectionList.length" @click.stop="handleBatch">
                批量撤回
              </common-button>
            </template>
          </el-popover>
          <common-button v-permission="permission.print" icon="el-icon-printer" size="mini" type="success" @click="printIt">
            打印{{
              props.detailData.productType === componentTypeEnum.ARTIFACT.V
                ? '【任务清单】'
                : listType === typeEnum.TASK_LIST.V
                ? '【任务清单】'
                : '【套料清单】'
            }}
          </common-button>
        </div>
      </div>
      <div>
        <common-table
          ref="table"
          :data="tableData"
          empty-text="暂无数据"
          :max-height="isEdit === true ? maxHeight - 50 : maxHeight"
          style="width: 100%"
          @selection-change="handleSelectChange"
          v-if="props.detailData.productType === componentTypeEnum.ARTIFACT.V"
        >
          <el-table-column v-if="isEdit === true" :selectable="selectable" type="selection" align="center" width="60" />
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column :show-overflow-tooltip="true" label="单体" key="monomer.name" prop="monomer.name" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="区域" key="area.name" prop="area.name" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="编号" key="serialNumber" prop="serialNumber" align="center" />
          <el-table-column
            :show-overflow-tooltip="true"
            label="规格"
            key="specification"
            prop="specification"
            align="center"
            min-width="110px"
          />
          <el-table-column :show-overflow-tooltip="true" label="长度（mm）" key="length" prop="length" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="单净重（kg）" key="netWeight" prop="netWeight" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="单毛重（kg）" key="grossWeight" prop="grossWeight" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="任务数" key="quantity" prop="quantity" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="完成日期" key="complete" prop="completeTime" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}-{m}-{d}') : '-' }}</span>
            </template>
          </el-table-column>
          <el-table-column v-if="isEdit === true" :show-overflow-tooltip="true" label="操作" align="center" width="90px">
            <template v-slot="scope">
              <el-popover
                v-model:visible="scope.row.pop"
                placement="top"
                width="180"
                trigger="click"
                @show="onPopoverDelClickShow"
                @hide="onPopoverDelClickHide"
              >
                <p>是否确认撤回操作？</p>
                <div style="text-align: right; margin: 0">
                  <common-button size="mini" type="text" @click.stop="cancelDelete(scope.row)">取消</common-button>
                  <common-button type="primary" size="mini" @click.stop="handleDelete(scope.row)">确定</common-button>
                </div>
                <template #reference>
                  <common-button v-permission="permission.revoke" size="mini" type="danger" :disabled="scope.row.boolCanRevoke === false" @click.stop="back(scope.row)">
                    撤回
                  </common-button>
                </template>
              </el-popover>
            </template>
          </el-table-column>
        </common-table>
        <common-table
          v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V && listType === typeEnum.TASK_LIST.V"
          ref="table"
          :data="tableData"
          empty-text="暂无数据"
          :max-height="maxHeight"
          style="width: 100%"
        >
          <el-table-column :show-overflow-tooltip="true" label="属性" key="taskType" prop="taskType" align="center">
            <template v-slot="scope">
              <el-tag :type="scope.row.taskType === structureOrderTypeEnum.NESTING.V ? 'success' : 'warning'">{{
                structureOrderTypeEnum.VL[scope.row.taskType]
              }}</el-tag>
            </template>
          </el-table-column>
          <el-table-column
            :show-overflow-tooltip="true"
            label="编号"
            key="serialNumber"
            prop="serialNumber"
            align="center"
            min-width="130px"
          />
          <el-table-column :show-overflow-tooltip="true" label="规格" key="specification" prop="specification" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="长度（mm）" key="length" prop="length" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="数量" key="quantity" prop="quantity" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="单净重" key="netWeight" prop="netWeight" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="完成日期" key="complete" prop="completeTime" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}-{m}-{d}') : '-' }}</span>
            </template>
          </el-table-column>
        </common-table>
        <common-table
          v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V && listType === typeEnum.NESTING_LIST.V"
          ref="tableRef"
          :data="tableData"
          empty-text="暂无数据"
          :max-height="maxHeight"
          style="width: 100%"
          class="upload-table"
        >
          <el-table-column
            :show-overflow-tooltip="true"
            label="套料编号"
            key="serialNumber"
            prop="serialNumber"
            align="center"
            min-width="130px"
          />
          <el-table-column
            :show-overflow-tooltip="true"
            label="材料属性"
            key="typesettingAssembleName"
            prop="typesettingAssembleName"
            align="center"
          >
            <template v-slot="scope">
              <span>{{ scope.row.typesettingAssembleName }}</span>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" label="规格" key="specification" prop="specification" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="材质" key="material" prop="material" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="母材长度" key="length" prop="length" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="部件" key="serialNumber" prop="serialNumber" align="center">
            <template v-slot="scope">
              <div
                v-for="(item, index) in scope.row.assembleList"
                :key="item"
                :class="index === scope.row.assembleList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'"
              >
                <span>{{ item.serialNumber }}</span>
              </div>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" label="重量" key="weight" prop="weight" align="center">
            <template v-slot="scope">
              <div
                v-for="(item, index) in scope.row.assembleList"
                :key="item"
                :class="index === scope.row.assembleList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'"
              >
                <span>{{ item.weight }}</span>
              </div>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" label="数量" key="quantity" prop="quantity" align="center">
            <template v-slot="scope">
              <div
                v-for="(item, index) in scope.row.assembleList"
                :key="item"
                :class="index === scope.row.assembleList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'"
              >
                <span>{{ item.quantity }}</span>
              </div>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" label="利用长度" key="aLength" prop="aLength" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="损耗率" key="lossRate" prop="lossRate" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.lossRate }}%</span>
            </template>
          </el-table-column>
        </common-table>
        <!--分页组件-->
        <el-pagination
          :total="total"
          :current-page="queryPage.pageNumber"
          :page-size="queryPage.pageSize"
          style="margin-top: 8px"
          layout="total, prev, pager, next, sizes"
          @size-change="handleSizeChange"
          @current-change="handleCurrentChange"
        />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import fetchFn from '@/utils/print/api'
import {
  processInfo,
  getTaskList,
  getNestingList,
  printSign,
  backWorkOrder,
  getInitBack,
  allOrderBatch
} from '@/api/mes/work-order-manage/artifact.js'
import { defineProps, defineEmits, ref, computed, watch, inject } from 'vue'
import { ElNotification, ElLoading, ElMessageBox, ElMessage } from 'element-plus'

import { componentTypeEnum, structureOrderTypeEnum } from '@enum-ms/mes'
import { printModeEnum } from '@/utils/print/enum'

import { constantize } from '@/utils/enum/base'
import { parseTime } from '@/utils/date'
import { codeWait } from '@/utils'
// import formatFn from '@/utils/print/format/index'
import { printTable } from '@/utils/print/table'

import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import useVisible from '@compos/use-visible'
import useDefaultTableTemplate from '@compos/use-default-table-template'

const permission = inject('permission')
const drawerRef = ref()
const isEdit = ref(false)
const delBtn = ref(false)
const isAllOrder = ref()
const selectionList = ref([])
const totalQuantity = ref()
const totalWeight = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  serialNumber: {
    type: String
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchHook })

const typeEnum = {
  TASK_LIST: {
    L: '任务清单',
    K: 'TASK_LIST',
    V: 1,
    [componentTypeEnum.ARTIFACT.K]: 'mesProductionTaskOrder',
    [componentTypeEnum.ASSEMBLE.K]: 'mesAssembleProductionTaskOrder'
  },
  NESTING_LIST: { L: '套料清单', K: 'NESTING_LIST', V: 2, [componentTypeEnum.ASSEMBLE.K]: 'mesAssembleNestingOrder' }
}
constantize(typeEnum)

function editMode() {
  isEdit.value = true
  initBack()
}

function cancelEdit() {
  isEdit.value = false
}

function handleSelectChange(row) {
  selectionList.value = row
}
// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true
  },
  drawerRef
)

const tableData = ref([])
const processList = ref([])
const processId = ref()
const listType = ref(typeEnum.TASK_LIST.V)
const params = computed(() => {
  return {
    orderId: props.detailData.orderId,
    processId: processId.value,
    productType: props.detailData.productType,
    projectId: props.detailData.projectId,
    serialNumber: props.serialNumber
  }
})
// const apiKey =
//   props.detailData.productType === componentTypeEnum.ARTIFACT.V
//     ? 'mesProductionTaskOrder'
//     : listType.value === typeEnum.TASK_LIST.V
//       ? 'mesAssembleProductionTaskOrder'
//       : 'mesAssembleNestingOrder'

watch(
  () => drawerVisible.value,
  (val) => {
    if (val) {
      isEdit.value = false
      listType.value = typeEnum.TASK_LIST.V
      processId.value = undefined
      processGet()
    }
    emit('refresh')
  },
  { deep: true, immediate: true }
)

watch(
  () => processId.value,
  (val) => {
    if (val) {
      isEdit.value = false
    }
  }
)

async function processGet() {
  try {
    const data = await processInfo({
      orderId: props.detailData.orderId,
      productionLineId: props.detailData.productionLine?.id
    })
    processList.value = data
    handleProcessChange()
  } catch (error) {
    console.log('获取工序', error)
  }
}

// 构件查看、部件任务清单接口
async function fetch() {
  totalQuantity.value = 0
  totalWeight.value = 0
  let _list = []
  try {
    const query =
      props.detailData.productType === componentTypeEnum.ARTIFACT.V
        ? { ...params.value, productionLineTypeEnum: props.detailData.productionLine?.productionLineTypeEnum }
        : { ...params.value, type: listType.value }
    const { content = [], totalElements } = await getTaskList({
      ...query,
      ...queryPage
    })
    content.forEach((v) => {
      v.pop = false
      totalQuantity.value += v.quantity
      totalWeight.value += v.totalNetWeight
    })
    setTotalPage(totalElements)
    _list = content
  } catch (err) {
    console.log('获取生产任务单', err)
  } finally {
    tableData.value = _list
  }
}

// 工单撤回
function back(row) {
  row.pop = true
}

function cancelDelete(row) {
  row.pop = false
}
async function handleDelete(row) {
  const ids = []
  ids.push(row.taskId)
  try {
    await backWorkOrder(ids)
    emit('refresh')
    fetch()
    ElMessage.success(`当前工单撤回成功`)
  } catch (e) {
    console.log('撤回工单操作失败', e)
  }
}

function handleDocumentDelClick(row) {
  row.pop = false
}

function onPopoverDelClickShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentDelClick, { passive: false })
  }, 0)
}

function onPopoverDelClickHide() {
  document.removeEventListener('click', handleDocumentDelClick)
}

// 批量撤回
async function batchBack() {
  const ids = []
  selectionList.value?.forEach((v) => {
    ids.push(v.taskId)
  })
  try {
    await backWorkOrder(ids)
    emit('refresh')
    fetch()
    ElMessage.success(`工单批量撤回成功`)
  } catch (e) {
    console.log('撤回操作失败', e)
  } finally {
    delBtn.value = false
  }
}

function handleBatch() {
  delBtn.value = true
}

function cancelBatch() {
  delBtn.value = false
}

function handleDocumentBatchClick() {
  delBtn.value = false
}

function onPopoverBatchClickShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentBatchClick, { passive: false })
  }, 0)
}

function onPopoverBatchClickHide() {
  document.removeEventListener('click', handleDocumentBatchClick)
}

// 批量撤回是否禁用
function selectable(row) {
  if (row.boolCanRevoke === false) {
    return false
  } else {
    return true
  }
}

// 查询整个详情工单是否可删除
async function initBack() {
  try {
    const data = await getInitBack({
      ...params.value,
      productionLineTypeEnum: props.detailData.productionLine?.productionLineTypeEnum
    })
    isAllOrder.value = data
  } catch (e) {
    console.log('获取整个工单是否可撤回失败', e)
  }
}

// 整个工单撤回
async function taskOrderBack() {
  if (!isAllOrder.value) return
  ElMessageBox.confirm(`是否确认撤回整个工单？`, '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      await allOrderBatch(props.detailData.orderId)
      ElNotification({ title: '删除整个工单成功', type: 'success' })
      emit('refresh')
      fetch()
    } catch (error) {
      console.log('删除模型失败', error)
    }
  })
}

// 部件套料清单
async function assembleListGet() {
  let _content = []
  try {
    const { content = [], totalElements } = await getNestingList({
      ...params.value,
      type: listType.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    _content = content
  } catch (err) {
    console.log('获取部件套料清单', err)
  } finally {
    tableData.value = _content
  }
}
function handleTypeChange(val) {
  listType.value = val
  queryPage.pageNumber = 1
  if (listType.value === typeEnum.TASK_LIST.V) {
    fetch()
  } else {
    assembleListGet()
  }
}

function handleProcessChange(val) {
  if (listType.value === typeEnum.TASK_LIST.V) {
    fetch()
  } else {
    assembleListGet()
  }
}

function fetchHook() {
  isEdit.value = false
  if (listType.value === typeEnum.TASK_LIST.V) {
    fetch()
  } else {
    assembleListGet()
  }
}

// async function addPrintRecord() {
//   if (props.detailData.productType !== componentTypeEnum.ARTIFACT.V) return
//   try {
//     await printSign({ ...params.value })
//     emit('refresh')
//   } catch (error) {
//     console.log('添加打印记录失败', error)
//   }
// }

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
    const apiKey =
      props.detailData.productType === componentTypeEnum.ARTIFACT.V
        ? 'mesProductionTaskOrder'
        : listType.value === typeEnum.TASK_LIST.V
          ? 'mesAssembleProductionTaskOrder'
          : 'mesAssembleNestingOrder'
    // for (const item in typeEnum.ENUM) {
    //   console.log(typeEnum[item].V, 'item')
    //   const apiKey = typeEnum[item][componentTypeEnum.VK[props.detailData.productType]]
    //   console.log(apiKey, 'apiKey')
    //   if (!apiKey) continue
    //   printLoading.value.text = `正在加载数据：${typeEnum[item].L}`
    //   const config = await useDefaultTableTemplate(apiKey)
    //   const _params =
    //     props.detailData.productType === componentTypeEnum.ARTIFACT.V ? { ...params.value, productionLineTypeEnum: props.detailData.productionLine?.productionLineTypeEnum } : { ...params.value, type: listType.value }
    //   let _resData = (await fetchFn[apiKey](_params)) || {}
    //   if (formatFn[apiKey]) {
    //     // 数据装换
    //     _resData = await formatFn[apiKey](_resData)
    //   }
    //   const { header, footer, table, qrCode } = _resData
    //   printLoading.value.text = `正在加入打印队列：${typeEnum[item].L}`
    //   await codeWait(500)
    //   const result = await printTable({
    //     printMode: printModeEnum.QUEUE.V,
    //     header,
    //     footer,
    //     table,
    //     qrCode,
    //     config
    //   })
    //   if (!result) {
    //     throw new Error('导出失败')
    //   }
    // }
    printLoading.value.text = `正在加入打印队列：任务单`
    const config = await useDefaultTableTemplate(apiKey)
    const _params =
      props.detailData.productType === componentTypeEnum.ARTIFACT.V
        ? { ...params.value, productionLineTypeEnum: props.detailData.productionLine?.productionLineTypeEnum }
        : { ...params.value, type: listType.value }
    const { header, footer, table, qrCode } = (await fetchFn[apiKey](_params)) || {}
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
    ElNotification({ title: `打印成功`, type: 'success', duration: 2500 })

    printLoading.value.text = `已全部加入打印队列`
    await codeWait(500)
  } catch (error) {
    ElNotification({ title: '加入打印队列失败，请重试', type: 'error', duration: 2500 })
    throw new Error(error)
  } finally {
    printLoading.value.close()
    await printSign({ ...params.value })
    emit('refresh')
  }
}

// --------------------------- 打印 end --------------------------------
</script>
<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #eef7ea;
}
::v-deep(.blue-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  min-height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 2px;
  }
}
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
::v-deep(.el-table--small .el-table__cell) {
  padding: 4px 0;
}
.float-ele {
  float: left;
}
.div-ellipsis {
  width: 100%;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
</style>
