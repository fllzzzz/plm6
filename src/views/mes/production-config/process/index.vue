<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="工序名称" width="140px" />
      <el-table-column
        v-if="columns.visible('reportType')"
        key="reportType"
        prop="reportType"
        label="上报方式"
        width="170px"
        align="center"
      >
        <template v-slot:header>
          <el-tooltip
            class="item"
            effect="light"
            popper-class="test"
            placement="top"
            :content="`检验方式设四种方式：\n
          单件（不扫码）：每次只能上报一个，不需要扫码；\n
          单件（需扫码）：每次只能上报一个，需要扫码；\n
          批量（不扫码）：每次可以上报多个，不需要扫码;\n
          批量（需扫码）：每次可以上报多个，需要扫码。`"
          >
            <div style="display: inline-block">
              <span>上报方式</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-select
            v-if="checkPermission(permission.edit)"
            v-model="scope.row.reportType"
            size="small"
            placeholder="请选择"
            :disabled-val="scope.row.inspectDisabled"
            style="width: 140px"
            @change="changeReportType(scope.row, scope.row.reportType)"
          >
            <el-option
              v-for="reportType in reportTypeEnum.ENUM"
              :key="reportType.V"
              :label="reportType.L"
              :value="reportType.V"
              :disabled="scope.row.reportDisabled.includes(reportType.V)"
            />
          </el-select>
          <el-tag v-else :type="reportTypeEnum[reportTypeEnum.VK[scope.row.reportType]].T">{{
            reportTypeEnum.VL[scope.row.reportType]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inspectType')"
        key="inspectType"
        prop="inspectType"
        label="检验方式"
        width="170px"
        align="center"
      >
        <template v-slot:header>
          <el-tooltip
            class="item"
            effect="light"
            placement="top"
            :content="`检验方式设四种方式：\n
          单件（不扫码）：每次只能检验一个，不需要扫码；\n
          单件（需扫码）：每次只能检验一个，需要扫码；\n
          批量（不扫码）：每次可以检验多个，不需要扫码;\n
          批量（需扫码）：每次可以检验多个，需要扫码。`"
          >
            <div style="display: inline-block">
              <span>检验方式</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-select
            v-if="checkPermission(permission.edit)"
            v-model="scope.row.inspectType"
            size="small"
            placeholder="请选择"
            style="width: 140px"
            @change="changeInspectType(scope.row, scope.row.inspectType)"
          >
            <el-option
              v-for="inspectType in inspectTypeEnum.ENUM"
              :key="inspectType.V"
              :label="inspectType.L"
              :value="inspectType.V"
              :disabled="scope.row.inspectDisabled.includes(inspectType.V)"
            />
          </el-select>
          <el-tag v-else :type="inspectTypeEnum[inspectTypeEnum.VK[scope.row.inspectType]].T">{{
            inspectTypeEnum.VL[scope.row.inspectType]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('wageQuotaType')"
        key="wageQuotaType"
        prop="wageQuotaType"
        label="工价计价方式"
        width="170px"
        align="center"
      >
        <template v-slot="scope">
          <el-select
            v-if="checkPermission(permission.edit)"
            v-model="scope.row.wageQuotaType"
            size="small"
            placeholder="请选择"
            style="width: 100%"
            @change="changeWageQuotaType(scope.row, scope.row.wageQuotaType)"
          >
            <el-option
              v-for="item in wageQuotaTypeEnum.ENUM"
              :key="item.V"
              :label="item.L"
              :value="item.V"
              :disabled="scope.row.wageQuotaTypeDisabled.includes(item.V)"
            >
              <span style="float: left">{{ item.L }}</span>
              <span style="float: right; color: var(--el-text-color-secondary); font-size: 13px; margin-left: 15px">{{ item.unit }}</span>
            </el-option>
          </el-select>
          <span v-else>
            {{ wageQuotaTypeEnum.VL[scope.row.wageQuotaType] }}
          </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" width="100px" />
      <el-table-column
        v-if="columns.visible('type') && crud.query.sequenceType === typeEnum.ARTIFACT.V"
        key="type"
        prop="type"
        label="工序次序"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ processTypeEnum.VL[scope.row.type] }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sequenceType')" key="sequenceType" prop="sequenceType" label="类型" min-width="110px">
        <template v-slot="scope">
          <el-tag :type="typeEnum[typeEnum.VK[scope.row.sequenceType]].T">{{ typeEnum.VL[scope.row.sequenceType] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" min-width="110px" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/process'
import { ref } from 'vue'
import { ElMessageBox } from 'element-plus'

import {
  processTypeEnum,
  processMaterialListTypeEnum as typeEnum,
  processInspectTypeEnum as inspectTypeEnum,
  processReportTypeEnum as reportTypeEnum,
  wageQuotaTypeEnum
} from '@enum-ms/mes'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['process:get'],
  add: ['process:add'],
  edit: ['process:edit'],
  del: ['process:del']
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工序',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

async function changeInspectType(data, val) {
  try {
    await ElMessageBox.confirm(
      `此操作将把 “${data.name}” 工序的检验方式：\n由“${inspectTypeEnum.VL[data.orginInspectType]}”变更为 “${
        inspectTypeEnum.VL[val]
      }”, 是否继续？`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }
    )
    await crudApi.edit({ id: data.id, inspectType: val })
    crud.notify(`“${data.name}” 工序,检验方式变更为 “${inspectTypeEnum.VL[val]}” 成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.refresh()
  } catch (error) {
    console.log(error)
    data.inspectType = data.orginInspectType
  }
}

async function changeReportType(data, val) {
  try {
    await ElMessageBox.confirm(
      `此操作将把 “${data.name}” 工序的上报方式：\n由“${reportTypeEnum.VL[data.orginReportType]}”变更为 “${
        reportTypeEnum.VL[val]
      }”, 是否继续？`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }
    )
    await crudApi.edit({ id: data.id, reportType: val })
    crud.notify(`“${data.name}” 工序,上报方式变更为 “${reportTypeEnum.VL[val]}” 成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.refresh()
  } catch (error) {
    console.log(error)
    data.reportType = data.orginReportType
  }
}

async function changeWageQuotaType(data, val) {
  try {
    await ElMessageBox.confirm(
      `此操作将把 “${data.name}” 工序的工价单位：\n由“${wageQuotaTypeEnum.VL[data.orginReportType]}”变更为 “${
        wageQuotaTypeEnum.VL[val]
      }”, 是否继续？`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }
    )
    await crudApi.edit({ id: data.id, wageQuotaType: val })
    crud.notify(`“${data.name}” 工序,工价单位变更为 “${wageQuotaTypeEnum.VL[val]}” 成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.refresh()
  } catch (error) {
    console.log(error)
    data.wageQuotaType = data.orginWageQuotaType
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.createTime = parseTime(v.createTime)
    v.orginInspectType = v.inspectType
    v.orginReportType = v.reportType
    v.orginWageQuotaType = v.wageQuotaType
    v.wageQuotaTypeDisabled = getWageQuotaTypeDisabled(v)
    v.reportDisabled = getReportDisabled(v)
    v.inspectDisabled = getInspectDisabled(v)
    return v
  })
}

function getWageQuotaTypeDisabled(v) {
  if (v.sequenceType === typeEnum.MACHINE_PART.V) {
    return [wageQuotaTypeEnum.AREA.V]
  } else if (v.sequenceType === typeEnum.ARTIFACT.V && v.type === processTypeEnum.ONCE.V) {
    return [wageQuotaTypeEnum.WEIGHT.V, wageQuotaTypeEnum.AREA.V]
  }
  return []
}

function getReportDisabled(v) {
  if (v.sequenceType === typeEnum.MACHINE_PART.V || (v.sequenceType === typeEnum.ARTIFACT.V && v.type === processTypeEnum.ONCE.V)) {
    return [reportTypeEnum.BATCH_SCAN.V, reportTypeEnum.SINGLE_SCAN.V]
  }
  return []
}

function getInspectDisabled(v) {
  if (v.sequenceType === typeEnum.MACHINE_PART.V || (v.sequenceType === typeEnum.ARTIFACT.V && v.type === processTypeEnum.ONCE.V)) {
    return [inspectTypeEnum.BATCH_SCAN.V, inspectTypeEnum.SINGLE_SCAN.V]
  }
  return []
}

CRUD.HOOK.beforeToAdd = (crud, data) => {
  crud.form.type = crud.query.type
  if (crud.query.sequenceType !== typeEnum.ENCLOSURE.V) {
    crud.form.sequenceType = crud.query.sequenceType
  }
}

CRUD.HOOK.beforeSubmit = () => {
  if (crud.form.sequenceType === typeEnum.MACHINE_PART.V) {
    crud.form.type = processTypeEnum.ONCE.V
  }
  if (crud.form.sequenceType === typeEnum.ENCLOSURE.V) {
    crud.form.type = processTypeEnum.TWICE.V
  }
}
</script>
