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
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        label="车次"
        align="center"
        min-width="140px"
      >
        <template v-slot="scope">
          <table-cell-tag :show="scope.row.deliveryStatus === deliveryStatusEnum.RETURN.V" name="已取消" color="#f56c6c" />
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" sortable="custom" label="发运日期" width="120">
        <template v-slot="scope">
          <span v-parse-time="{ val: scope.row.auditTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
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
        v-if="columns.visible('manufactureType')"
        key="manufactureType"
        prop="manufactureType"
        :show-overflow-tooltip="true"
        label="制造类型"
        align="center"
        min-width="80"
      >
        <template v-slot="scope">
          <el-tag :type="manufactureTypeEnum.V[scope.row.manufactureType].T" effect="plain" disable-transitions>{{
            manufactureTypeEnum.VL[scope.row.manufactureType]
          }}</el-tag>
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
        v-if="columns.visible('driverName')"
        key="driverName"
        prop="driverName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="司机姓名"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('driverPhone')"
        key="driverPhone"
        prop="driverPhone"
        :show-overflow-tooltip="true"
        label="司机电话"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('auditUserName')"
        key="auditUserName"
        prop="auditUserName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="办理人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('actualGrossWeight')"
        key="actualGrossWeight"
        prop="actualGrossWeight"
        :show-overflow-tooltip="true"
        label="毛重（t）"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.actualGrossWeight, 'kg', 't', 3) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('emptyCars')"
        key="emptyCars"
        prop="emptyCars"
        :show-overflow-tooltip="true"
        label="皮重（t）"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.emptyCars, 'kg', 't', 3) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('actualWeight')"
        key="actualWeight"
        prop="actualWeight"
        :show-overflow-tooltip="true"
        label="净重（t）"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.actualWeight, 'kg', 't', 3) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        key="totalNetWeight"
        prop="totalNetWeight"
        :show-overflow-tooltip="true"
        label="理论重量（t）"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.totalNetWeight, 'kg', 't', 3) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('difference')"
        key="difference"
        prop="difference"
        :show-overflow-tooltip="true"
        label="差值（t）"
        align="center"
        min-width="100"
      >
        <template v-slot="scope">
          <span :style="{ color: scope.row.acceptDifference ? '#13ce66' : '#ff4949' }">{{
            convertUnits(scope.row.difference, 'kg', 't', 3)
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('differenceRate')"
        key="differenceRate"
        prop="differenceRate"
        :show-overflow-tooltip="true"
        label="差值率"
        align="center"
        min-width="100"
      >
        <template v-slot="scope">
          <span :style="{ color: scope.row.acceptDifference ? '#13ce66' : '#ff4949' }">{{ scope.row.differenceRate }}</span>
        </template>
      </el-table-column>
      <el-table-column label="构件毛重（t）" align="center" v-if="columns.visible('totalGrossWeight')" prop="totalGrossWeight" key="totalGrossWeight" :show-overflow-tooltip="true"></el-table-column>
      <!--详情与下载-->
      <el-table-column v-if="checkPermission([...permission.detail])" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <!-- 详情 -->
          <common-button type="primary" icon="el-icon-view" size="mini" @click.stop="showDetail(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <m-detail
      v-model:visible="detailVisible"
      :detail-info="shipInfo"
      title="装车详情"
      :detailFunc="crud.query.projectType === projectTypeEnum.BRIDGE.V ? detailBridge : detail"
    >
      <template #tip>
        <div style="width: 150px; height: 53px; overflow: hidden; position: absolute; top: -18px; left: -20px">
          <table-cell-tag :show="shipInfo.deliveryStatus === deliveryStatusEnum.RETURN.V" name="已取消" color="#f56c6c" />
        </div>
        <el-tag effect="plain" size="medium" type="danger">车次：{{ shipInfo.serialNumber }}</el-tag>
        <el-tag effect="plain" size="medium">项目：{{ shipInfo.project && shipInfo.project.shortName }}</el-tag>
        <el-tag effect="plain" size="medium" type="success">办理人：{{ shipInfo.auditUserName }}</el-tag>
      </template>
    </m-detail>
  </div>
</template>

<script setup>
import { get, getBridge, detail, detailBridge } from '@/api/mes/pack-and-ship/ship-list'
import { ref } from 'vue'

import { mesShipPM as permission } from '@/page-permission/ship-manage'
import { manufactureTypeEnum } from '@enum-ms/production'
import { packTypeEnum, deliveryStatusEnum } from '@enum-ms/mes'
// import { DP } from '@/settings/config'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
import { convertUnits } from '@/utils/convert/unit'
import { projectNameFormatter } from '@/utils/project'
import checkPermission from '@/utils/system/check-permission'
import { mapGetters } from '@/store/lib'
import { projectTypeEnum } from '@enum-ms/contract'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from '../components/common-detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { currentProjectType } = mapGetters(['currentProjectType'])
const tableRef = ref()
const { crud, CRUD, columns } = useCRUD(
  {
    title: '发运记录',
    sort: ['auditTime.desc'],
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow },
    invisibleColumns: ['totalGrossWeight']
  },
  tableRef
)

CRUD.HOOK.beforeToQuery = () => {
  crud.query.projectType = currentProjectType.value
  crud.crudApi.get = crud.query.projectType === projectTypeEnum.BRIDGE.V ? getBridge : get
}

const { maxHeight } = useMaxHeight({ paginate: true })

const detailVisible = ref(false)
const shipInfo = ref({})

function showDetail(row) {
  shipInfo.value = row
  detailVisible.value = true
}
</script>
