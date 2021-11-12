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
    @selection-change="crud.selectionChangeHandler"
  >
    <el-table-column v-if="modifying" type="selection" :selectable="selectableFunc" width="55" align="center" />
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column
      v-if="columns.visible('productionLineName')"
      prop="productionLineName"
      :show-overflow-tooltip="true"
      label="生产线"
      min-width="140px"
    >
      <template v-slot="scope">
        <span>{{ emptyTextFormatter(scope.row.workshopName) }}>{{ emptyTextFormatter(scope.row.productionLineName) }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectName')" prop="project" label="所属项目" min-width="250px">
      <template v-slot="scope">
        <span>{{ emptyTextFormatter(scope.row.projectName) }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="columns.visible('districtName')"
      prop="districtName"
      :show-overflow-tooltip="true"
      label="单体区域"
      min-width="140px"
    >
      <template v-slot="scope">
        <span>{{ emptyTextFormatter(scope.row.monomerName) }}>{{ emptyTextFormatter(scope.row.districtName) }}</span>
      </template>
    </el-table-column>
    <template v-for="item in needTableColumns" :key="item.field">
      <el-table-column
        v-if="item.toFixed"
        :show-overflow-tooltip="true"
        :prop="item.field"
        :label="item.label"
        align="center"
        :width="item.width"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row[item.field], item.DP) }}
        </template>
      </el-table-column>
      <el-table-column v-else :show-overflow-tooltip="true" :prop="item.field" :label="item.label" :width="item.width">
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row[item.field]) }}</span>
        </template>
      </el-table-column>
    </template>
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
        <el-tag effect="plain" :disable-transitions="true" :type="taskIssueTypeEnum[taskIssueTypeEnum.VK[scope.row.issueStatus]].T">{{
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
          placeholder="需求完成日期"
        />
        <span v-else>{{ emptyTextFormatter(parseTime(scope.row.askCompleteTime, '{y}-{m}-{d}')) }}</span>
      </template>
    </el-table-column>
  </common-table>
  <issue-preview v-model:visible="previewVisible" :modified-data="crud.selections" @refresh="refresh" />
  <modifyQuantityDialog v-model:visible="modifyQuantityVisible" :details="detailRow" @modifySuccess="refresh" />
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/task/common'
import { computed, ref, defineProps, defineEmits, watch, inject } from 'vue'
import { ElMessage } from 'element-plus'
import moment from 'moment'

import { taskIssueTypeEnum } from '@enum-ms/mes'
import { parseTime } from '@/utils/date'
import { emptyTextFormatter, toFixed } from '@data-type'

// import useCheckPermission from '@compos/use-check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useTableValidate from '@compos/form/use-table-validate'
import mHeader from './module/header'
import modifyQuantityDialog from './module/modify-quantity-dialog'
import issuePreview from './module/issue-preview'

// crud交由presenter持有
const permission = {
  get: ['taskAssignDetail:get'],
  print: ['taskAssignDetail:print'],
  detail: ['taskAssignDetail:detail'],
  download: ['taskAssignDetail:download']
}

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

const needTableColumns = inject('needTableColumns')
const headerRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '任务详情',
    sort: [],
    permission: { ...permission },
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
  }
})

const { maxHeight } = useMaxHeight({ paginate: false })
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

watch(
  () => [processType.value, productType.value],
  () => {
    crud.refresh()
  },
  { immediate: true, deep: true }
)

const modifying = ref(false)
const modifyQuantityVisible = ref(false)
const previewVisible = ref(false)
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

function selectableFunc(row) {
  return row.operable
}

function editSchedulingQuantity(row) {
  if (!row.operable) return
  detailRow.value = Object.assign({}, row)
  modifyQuantityVisible.value = true
}

function refresh() {
  crud.refresh()
  emit('refresh')
}

CRUD.HOOK.beforeToQuery = () => {
  modifying.value = false
  crud.query.date = moment(props.details.date).valueOf()
  crud.query.processType = processType.value
  crud.query.productType = productType.value
  console.log(crud.query)
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.operable = !v.issueStatus
    v.sourceSchedulingQuantity = v.schedulingQuantity
    v.modifySchedulingQuantity = v.schedulingQuantity
    v.project = {
      id: v.projectId,
      name: v.projectName,
      shortName: v.projectShortName
    }
    return v
  })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
