<template>
  <div class="app-container">
    <!--工具栏-->
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="工序名称"
        align="center"
        width="140px"
      />
      <el-table-column
        v-if="columns.visible('reportType')"
        key="reportType"
        prop="reportType"
        label="上报方式"
        min-width="170px"
        align="center"
      >
        <template v-slot:header>
          <el-tooltip
            class="item"
            effect="light"
            popper-class="test"
            placement="top"
            :content="`上报方式设五种方式：\n
          机联：需与设备进行对对接，无需进行人工上报；\n
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
            style="width: 100%"
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
          <el-tag v-else :type="reportTypeEnum.V[scope.row.reportType].T">{{ reportTypeEnum.VL[scope.row.reportType] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inspectType')"
        key="inspectType"
        prop="inspectType"
        label="检验方式"
        min-width="170px"
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
            style="width: 100%"
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
          <el-tag v-else :type="inspectTypeEnum.V[scope.row.inspectType].T">{{ inspectTypeEnum.VL[scope.row.inspectType] }}</el-tag>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/enclosure/production-config/inspection-mode'
import { ref } from 'vue'
import { ElMessageBox } from 'element-plus'

import {
  processMaterialListTypeEnum as typeEnum,
  processInspectTypeEnum as inspectTypeEnum,
  processReportTypeEnum as reportTypeEnum
} from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { enclosureConfigInspectionModePM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '报检方式',
    sort: [],
    hasPagination: false,
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

async function changeInspectType(data, val) {
  try {
    await ElMessageBox.confirm(
      `此操作将把 “${data.name}” 工序的检验方式：\n由“${inspectTypeEnum.VL[data.originInspectType]}”变更为 “${
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
    data.inspectType = data.originInspectType
  }
}

async function changeReportType(data, val) {
  try {
    await ElMessageBox.confirm(
      `此操作将把 “${data.name}” 工序的上报方式：\n由“${reportTypeEnum.VL[data.originReportType]}”变更为 “${
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
    data.reportType = data.originReportType
  }
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.map((v) => {
    v.originInspectType = v.inspectType
    v.originReportType = v.reportType
    v.reportDisabled = getReportDisabled(v)
    v.inspectDisabled = getInspectDisabled(v)
    return v
  })
}

function getReportDisabled(v) {
  if (v.sequenceType === typeEnum.MACHINE_PART.V) {
    return [reportTypeEnum.BATCH_SCAN.V, reportTypeEnum.SINGLE_SCAN.V]
  }
  return []
}

function getInspectDisabled(v) {
  if (v.sequenceType === typeEnum.MACHINE_PART.V) {
    return [inspectTypeEnum.BATCH_SCAN.V, inspectTypeEnum.SINGLE_SCAN.V]
  }
  return []
}
</script>
