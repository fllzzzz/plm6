<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('project.shortName') && !crud.query.projectId"
        key="project.shortName"
        prop="project.shortName"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="250"
      >
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('licensePlate')"
        key="licensePlate"
        prop="licensePlate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="车牌号"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        label="车次"
        align="center"
        min-width="140px"
      >
        <template v-slot="scope">
          <el-tag effect="light" disable-transitions style="width: 100%; max-width: 130px">{{ scope.row.serialNumber }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('checkStatus')"
        key="checkStatus"
        prop="checkStatus"
        label="审核状态"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <el-tag v-if="scope.row.checkStatus" :type="shipAuditStatusEnum.V[scope.row.checkStatus].T" effect="plain">{{
            shipAuditStatusEnum.VL[scope.row.checkStatus]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userName')"
        key="userName"
        prop="userName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="装车人"
        align="center"
        width="140"
      >
        <template v-slot="scope">
          <span style="white-space: pre-line">
            <span>{{ scope.row.userName + '\n' }}</span>
            <span v-parse-time="'{y}-{m}-{d} {h}:{i}'">{{ scope.row.loadingTime }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('actualUserName')"
        key="actualUserName"
        prop="actualUserName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="过磅人"
        align="center"
        width="140"
      >
        <template v-slot="scope">
          <span style="white-space: pre-line">
            <span>{{ scope.row.actualUserName + '\n' }}</span>
            <span v-parse-time="'{y}-{m}-{d} {h}:{i}'">{{ scope.row.actualTime }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditUserName')"
        key="auditUserName"
        prop="auditUserName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="审核人"
        align="center"
        width="140"
      />
      <el-table-column
        v-if="columns.visible('structureMeasureMode')"
        :show-overflow-tooltip="true"
        prop="structureMeasureMode"
        label="计量方式"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ measureModeEnum.VL[scope.row.structureMeasureMode] }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('productType')" key="productType" prop="productType" label="装载类型" width="165">
        <template v-slot="scope">
          <el-tag
            v-for="item in cleanArray(EO.getBits(packTypeEnum, scope.row.productType, 'V'))"
            style="margin-right: 5px"
            :key="item"
            :type="packTypeEnum.V[item].T"
            effect="light"
            disable-transitions
            >{{ packTypeEnum.VL[item] }}</el-tag
          >
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('actualWeight')"
        :show-overflow-tooltip="true"
        prop="actualWeight"
        label="装载重量(t)"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.actualWeight, 'kg', 't', DP.COM_WT__T) }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('shipAmount')"
        :show-overflow-tooltip="true"
        prop="shipAmount"
        label="货物价值(元)"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ scope.row.shipAmount }}</span>
        </template>
      </el-table-column>
      <!--详情与下载-->
      <el-table-column v-if="checkPermission([...permission.detail])" label="操作" width="90px" align="center">
        <template v-slot="scope">
          <common-button
            v-if="scope.row.checkStatus === shipAuditStatusEnum.CHECKED.V"
            type="info"
            size="mini"
            @click.stop="showDetail(scope.row)"
          >
            查看
          </common-button>
          <common-button v-else type="primary" size="mini" @click.stop="showDetail(scope.row)"> 审核 </common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <m-detail v-model:visible="detailVisible" :detail-info="shipInfo" title="发运审核" :detailFunc="detail">
      <template #titleRight v-if="shipInfo.checkStatus === shipAuditStatusEnum.UNCHECKED.V && checkPermission([...permission.audit])">
        <common-button type="primary" :loading="loading.passLoading" size="mini" @click="auditIt(shipAuditEnum.PASS, 'passLoading')">
          同意发运
        </common-button>
        <common-button type="danger" :loading="loading.noPassLoading" size="mini" @click="auditIt(shipAuditEnum.NO_PASS, 'noPassLoading')">
          不同意发运
        </common-button>
      </template>
    </m-detail>
  </div>
</template>

<script setup>
import crudApi, { detail, audit } from '@/api/mes/pack-and-ship/ship-audit'
import { ref, reactive } from 'vue'
import { ElNotification } from 'element-plus'

import { packTypeEnum, shipAuditStatusEnum } from '@enum-ms/mes'
import { weightMeasurementModeEnum as measureModeEnum } from '@enum-ms/finance'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
// import { convertUnits } from '@/utils/convert/unit'
import { projectNameFormatter } from '@/utils/project'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail/index.vue'

// 发运审核
const shipAuditEnum = {
  PASS: { L: '同意', K: 'PASS', V: 1 },
  NO_PASS: { L: '不同意', K: 'NO_PASS', V: 2 }
}

const permission = {
  get: ['shipAudit:get'],
  detail: ['shipAudit:detail'],
  print: ['shipAudit:print'],
  audit: ['shipAudit:audit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '发运审核',
    sort: ['auditTime.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailVisible = ref(false)
const shipInfo = ref({})
const loading = reactive({
  passLoading: false,
  noPassLoading: false
})

function showDetail(row) {
  shipInfo.value = row
  detailVisible.value = true
}

async function auditIt(status, loadingLabel) {
  try {
    loading[loadingLabel] = true
    await audit({
      id: shipInfo.value.id,
      status: status.V
    })
    ElNotification({ title: '审核成功', message: `${status.L}发运`, type: 'success', duration: 2500 })
    detailVisible.value = false
    crud.toQuery()
  } catch (error) {
    console.log('发运审核失败', error)
  } finally {
    loading[loadingLabel] = false
  }
}
</script>

<style>
.contractLabel {
  width: 100px;
}
</style>
