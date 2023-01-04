<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader>
        <template #optRight>
          <common-button
            v-permission="permission.save"
            class="filter-item"
            :disabled="!crud.selections?.length"
            size="mini"
            :loading="issueLoading"
            icon="el-icon-edit"
            type="primary"
            @click="toBatchIssue"
            >套料下发</common-button
          >
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="dataFormat"
      :cell-class-name="wrongCellMask"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" class="selection" :selectable="selectable" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('date')"
        prop="date"
        :show-overflow-tooltip="true"
        label="排产日期"
        width="110"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('userName')"
        prop="userName"
        :show-overflow-tooltip="true"
        label="操作人"
        min-width="100"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('orderNumber')"
        prop="orderNumber"
        :show-overflow-tooltip="true"
        label="套料单号"
        min-width="120"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('thick')"
        prop="thick"
        :show-overflow-tooltip="true"
        label="板厚（mm）"
        min-width="100"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('material')"
        prop="material"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="100"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('quantity')"
        prop="quantity"
        :show-overflow-tooltip="true"
        label="数量（件）"
        min-width="90"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        prop="totalNetWeight"
        :show-overflow-tooltip="true"
        label="重量（kg）"
        min-width="90"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('cutConfigId')"
        prop="cutConfigId"
        :show-overflow-tooltip="true"
        label="切割方式"
        min-width="90"
        align="center"
      >
        <template #default="{ row: { sourceRow: row } }">
          <cut-config-select
            v-if="row.issueStatusEnum === issueStatusEnum.NOT_NESTING.V"
            v-model="row.cutConfigId"
            :layOffWayType="row.boolDrawing ? undefined : false"
            clearable
          />
          <span v-else>{{ row.cutConfigName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('issueStatusEnum')"
        prop="issueStatusEnum"
        :show-overflow-tooltip="true"
        label="下发状态"
        width="90"
        align="center"
      >
        <template #default="{ row: { sourceRow: row } }">
          <el-tag v-if="row.issueStatusEnum !== issueStatusEnum.NOT_NESTING.V && !row.boolNestCutEnum" type="danger">
            {{ layOffWayTypeEnum.VL[row.boolNestCutEnum] }}
          </el-tag>
          <template v-else>
            <el-tag v-if="row.issueStatusEnum && issueStatusEnum.V[row.issueStatusEnum]" :type="issueStatusEnum.V[row.issueStatusEnum].T">
              {{ issueStatusEnum.VL[row.issueStatusEnum] }}
            </el-tag>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-permission="[...permission.del, ...permission.detail]" label="操作" width="140px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button v-permission="permission.detail" type="info" icon="el-icon-view" size="mini" @click="toDetail(row)" />
          <udOperation :showEdit="false" :showDel="row.issueStatusEnum === issueStatusEnum.NOT_NESTING.V" :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <record-detail v-model:visible="detailVisible" :recordId="currentRow?.id" @del-success="crud.toQuery" />
  </div>
</template>

<script setup>
import { record, del, saveNesting } from '@/api/mes/scheduling-manage/machine-part'
import { ref } from 'vue'
import { ElNotification, ElMessage } from 'element-plus'

import { machinePartSchedulingIssueStatusEnum as issueStatusEnum } from '@enum-ms/mes'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import { machinePartSchedulingRecordPM as permission } from '@/page-permission/mes'

import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import recordDetail from './module/record-detail.vue'
import cutConfigSelect from '@/components-system/base/cut-config-select.vue'

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '预览记录',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: record, del }
  },
  tableRef
)

const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])
const { maxHeight } = useMaxHeight({ paginate: true })

const detailVisible = ref(false)
const currentRow = ref()
const issueLoading = ref(false)

const tableRules = {
  cutConfigId: [{ required: true, message: '请选择切割方式', trigger: 'change' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}

function selectable(row, rowIndex) {
  return row.issueStatusEnum === issueStatusEnum.NOT_NESTING.V
}

function toDetail(row) {
  currentRow.value = row
  detailVisible.value = true
}

async function toBatchIssue() {
  if (!crud.selections) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  try {
    issueLoading.value = true
    const { validResult, dealList } = tableValidate(crud.selections)
    if (validResult) {
      const _resList = dealList.map((v) => {
        return {
          cutConfigId: v.cutConfigId,
          schedulingId: v.id
        }
      })
      await saveNesting(_resList)
      ElNotification({ title: '套料下发成功', type: 'success', duration: 3000 })
      crud.toQuery()
    }
  } catch (e) {
    console.log(`套料下发失败`, e)
  } finally {
    issueLoading.value = false
  }
}
</script>
