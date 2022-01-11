<template>
  <div class="head-container">
    <mHeader ref="headerRef" v-model:modifying="modifying" @preview="previewIt" />
  </div>
  <!--表格渲染-->
  <common-table
    ref="tableRef"
    v-loading="crud.loading"
    :data="crud.data"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    :cell-class-name="wrongCellMask"
    style="width: 100%"
    @selection-change="handleSelectChange"
  >
    <el-table-column v-if="modifying" type="selection" :selectable="selectableFunc" width="55" align="center" />
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column
      v-if="columns.visible('productionLine.name')"
      prop="productionLine.name"
      :show-overflow-tooltip="true"
      label="生产线"
      min-width="140px"
    >
      <template v-slot="scope">
        <span>{{ emptyTextFormatter(scope.row.workshop?.name) }}>{{ emptyTextFormatter(scope.row.productionLine?.name) }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="columns.visible('project.shortName')"
      prop="project.shortName"
      :show-overflow-tooltip="true"
      label="所属项目"
      min-width="180px"
    >
      <template #default="{ row }">
        <span v-parse-project="{ project: row.project }" v-empty-text />
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('area.name')" prop="area.name" :show-overflow-tooltip="true" label="单体区域" min-width="140px">
      <template v-slot="scope">
        <span>{{ emptyTextFormatter(scope.row.monomer?.name) }}>{{ emptyTextFormatter(scope.row.area?.name) }}</span>
      </template>
    </el-table-column>
    <productType-base-info-columns
      :productType="productType"
      :category="category"
      :columns="columns"
      :unShowField="['specification', 'material']"
    />
    <el-table-column v-if="columns.visible('schedulingQuantity')" prop="schedulingQuantity" label="任务数" min-width="100px">
      <template v-slot:header>
        <el-tooltip class="item" effect="light" :content="`‘未下发’状态下，可修改排产数`" placement="top">
          <div style="display: inline-block">
            <span>任务数</span>
            <i class="el-icon-edit" />
          </div>
        </el-tooltip>
      </template>
      <template v-slot="scope">
        <el-input-number
          v-if="modifying && scope.row.operable"
          v-model="scope.row.schedulingQuantity"
          :step="1"
          :min="0"
          :max="scope.row.sourceSchedulingQuantity"
          size="mini"
          controls-position="right"
          style="width: 100%"
        />
        <el-tag
          v-else
          :disable-transitions="true"
          effect="plain"
          style="width: 100%"
          :style="scope.row.operable ? 'cursor:pointer;' : ''"
          @click.stop="editSchedulingQuantity(scope.row)"
        >
          <span>{{ emptyTextFormatter(scope.row.sourceSchedulingQuantity) }}</span>
          <span v-if="scope.row.operable" style="float: right"><i class="el-icon-edit" /></span>
        </el-tag>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('issueStatus')" prop="issueStatus" label="状态" align="center" width="80px">
      <template v-slot="scope">
        <el-tag effect="plain" :disable-transitions="true" :type="taskIssueTypeEnum.V[scope.row.issueStatus].T">{{
          taskIssueTypeEnum.VL[scope.row.issueStatus]
        }}</el-tag>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('askCompleteTime')" prop="askCompleteTime" label="要求完成日期" align="center" min-width="110px">
      <template v-slot="scope">
        <el-date-picker
          v-if="modifying && scope.row.operable && buttonValue === operateButtonEnum.TASK_ISSUE.V"
          v-model="scope.row.askCompleteTime"
          type="date"
          size="mini"
          style="width: 100%"
          :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
          placeholder="需求完成日期"
        />
        <span v-else>{{ emptyTextFormatter(parseTime(scope.row.askCompleteTime, '{y}-{m}-{d}')) }}</span>
      </template>
    </el-table-column>
    <template v-if="crud.query.issueStatus === taskIssueTypeEnum.HAS_ISSUED.V">
      <el-table-column v-if="columns.visible('completeQuantity')" prop="completeQuantity" label="完成数量" align="center" width="80px">
        <template #default="{ row }">
          <span :class="row.completeQuantity === row.schedulingQuantity ? 'tc-success' : 'tc-danger'">{{ row.completeQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission([...taskPermission.add, ...assistPermission.get])" label="操作" align="center" width="210px">
        <template #default="{ row }">
          <common-button
            v-permission="taskPermission.add"
            :disabled="row.completeQuantity === row.schedulingQuantity"
            size="mini"
            type="danger"
            @click="toDelTask(row)"
          >
            删除任务
          </common-button>
          <common-button v-permission="assistPermission.get" size="mini" type="warning" @click="toAssistanceTask(row)">
            协同任务
          </common-button>
        </template>
      </el-table-column>
    </template>
  </common-table>
  <issue-preview v-model:visible="previewVisible" :modified-data="crud.selections" @refresh="refresh" />
  <modifyQuantityDialog v-model:visible="modifyQuantityVisible" :details="detailRow" @modifySuccess="refresh" />
  <delTask v-model:visible="delTaskVisible" :details="detailRow" @delSuccess="refresh" />
  <assistance-drawer v-model:visible="assistanceVisible" :details="detailRow" />
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/task/common'
import { computed, ref, defineProps, defineEmits, watch, provide, inject } from 'vue'
import { ElMessage } from 'element-plus'
import moment from 'moment'

import { taskIssueTypeEnum } from '@enum-ms/mes'
import { parseTime } from '@/utils/date'
import { emptyTextFormatter } from '@data-type'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useTableValidate from '@compos/form/use-table-validate'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import mHeader from './module/header'
import modifyQuantityDialog from './module/modify-quantity-dialog'
import delTask from './module/del-task'
import issuePreview from './module/issue-preview'
import assistanceDrawer from './module/assistance-drawer'

const { task, assistance } = inject('permission')
const taskPermission = task
const assistPermission = assistance
provide('assistPermission', assistPermission)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRules = {
  schedulingQuantity: [{ required: true, message: '请填写任务数', trigger: 'blur' }]
  // askCompleteTime: [{ required: true, message: '请选择需求完成日期', trigger: 'change' }]
}

const headerRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '任务详情',
    sort: [],
    permission: { ...taskPermission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const emit = defineEmits(['refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  details: {
    type: Object,
    default: () => {}
  },
  query: {
    type: Object,
    default: () => {}
  }
})
watch(
  () => props.visible,
  (visible) => {
    if (!visible) {
      crud.resetQuery()
    }
  },
  { immediate: true }
)

const { maxHeight } = useMaxHeight({
  navbar: false,
  extraBox: ['.el-drawer__header', '.head-container'],
  wrapperBox: ['.el-drawer__body'],
  clientHRepMainH: true,
  paginate: false,
  minHeight: 300,
  extraHeight: 40
})
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

const buttonValue = computed(() => {
  return headerRef.value.buttonValue
})
const operateButtonEnum = computed(() => {
  return headerRef.value.operateButtonEnum
})

const processType = computed(() => {
  return props.details?.processType
})
const productType = computed(() => {
  return props.details?.productType
})
const category = computed(() => {
  return props.details?.category || 0
})

provide('productType', productType)
provide('category', category)

watch(
  () => [processType.value, productType.value],
  () => {
    crud.refresh()
  },
  { immediate: true, deep: true }
)

watch(
  () => props.query,
  () => {
    crud.query = Object.assign(crud.query, props.query)
  },
  { immediate: true, deep: true }
)

const modifying = ref(false)
const modifyQuantityVisible = ref(false)
const delTaskVisible = ref(false)
const previewVisible = ref(false)
const assistanceVisible = ref(false)
const detailRow = ref({})

async function previewIt() {
  const { validResult } = tableValidate(crud.selections)
  if (!validResult) {
    return
  }
  if (!crud.selections || !crud.selections.length) {
    ElMessage({ message: '请选择一条数据进行操作', type: 'error' })
    return
  }
  previewVisible.value = true
}

function handleSelectChange(val) {
  val.forEach(v => {
    v.askCompleteTime = v.askCompleteTime ? v.askCompleteTime : new Date()
  })
  crud.selectionChangeHandler(val)
}

function selectableFunc(row) {
  return row.operable
}

function editSchedulingQuantity(row) {
  if (!row.operable) return
  detailRow.value = Object.assign({}, row)
  modifyQuantityVisible.value = true
}

function toDelTask(row) {
  detailRow.value = Object.assign({}, row)
  delTaskVisible.value = true
}

function toAssistanceTask(row) {
  detailRow.value = Object.assign({}, row)
  assistanceVisible.value = true
}

function refresh() {
  crud.refresh()
  emit('refresh')
}

CRUD.HOOK.beforeToQuery = () => {
  modifying.value = false
  crud.query.date = moment(props.details.date).valueOf()
  crud.query.productType = productType.value
}

CRUD.HOOK.beforeRefresh = () => {
  modifying.value = false
  crud.query.date = moment(props.details.date).valueOf()
  crud.query.productType = productType.value
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.operable = !v.issueStatus
    v.sourceSchedulingQuantity = v.schedulingQuantity
    v.modifySchedulingQuantity = v.schedulingQuantity
    // return v
  })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
