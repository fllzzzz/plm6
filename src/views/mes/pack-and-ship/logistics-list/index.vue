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
      <el-table-column type="selection" width="50" align="center" />
      <el-table-column label="序号" type="index" align="center" width="50" />
      <el-table-column
        v-if="columns.visible('supplier.name')"
        key="supplier.name"
        prop="supplier.name"
        :show-overflow-tooltip="true"
        label="物流公司"
        min-width="140"
      >
        <template v-slot="scope">
          <span>{{ scope.row.supplier && scope.row.supplier.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project.shortName') && !crud.query.projectId"
        key="project.shortName"
        prop="project.shortName"
        :show-overflow-tooltip="true"
        label="承运项目"
        min-width="250"
      >
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('carModel')"
        :show-overflow-tooltip="true"
        prop="carModel"
        label="车型"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ scope.row.carModel }}</span>
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
      />
      <el-table-column
        v-if="columns.visible('driverName')"
        key="driverName"
        prop="driverName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="司机姓名"
        align="center"
        width="100"
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
        v-if="columns.visible('actualWeight')"
        :show-overflow-tooltip="true"
        prop="actualWeight"
        label="装载重量(t)"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.actualWeight, 'kg', 't', DP.COM_WT__T) }}</span>
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
      <el-table-column v-if="columns.visible('productType')" key="productType" prop="productType" label="装载类型" width="165" >
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
      <el-table-column prop="priceType" label="计价方式" align="center">
        <template v-slot="scope">
          <span>{{ logisticsPriceTypeEnum.VL[scope.row.priceType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('supplier.price')"
        :show-overflow-tooltip="true"
        prop="supplier.price"
        label="运输单价"
        align="right"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ scope.row.supplier && toFixed(scope.row.supplier.price, DP.YUAN) }}</span>
          <span :class="scope.row.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" style="margin-left:3px;">{{ logisticsPriceTypeEnum.V[scope.row.priceType].unit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalPrice')"
        :show-overflow-tooltip="true"
        prop="totalPrice"
        label="运输费(元)"
        align="right"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalPrice, DP.YUAN) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" sortable="custom" label="承运日期" width="120">
        <template v-slot="scope">
          <span v-parse-time="{ val: scope.row.auditTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
      <el-table-column label="操作" width="140px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            size="mini"
            icon="el-icon-edit"
            type="primary"
            v-permission="permission.edit"
            @click="openForm(scope.row,'edit')"
          />
          <common-button
            size="mini"
            icon="el-icon-view"
            type="primary"
            v-permission="permission.get"
            @click="openForm(scope.row,'detail')"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <priceForm v-model="formVisible" :detailInfo="detailInfo" :showType="showType" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/pack-and-ship/logistics-list'
import { ref } from 'vue'

import { logisticsPM as permission } from '@/page-permission/mes'
import { manufactureTypeEnum } from '@enum-ms/production'
import { packTypeEnum, logisticsPriceTypeEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { DP } from '@/settings/config'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
import { toFixed } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import priceForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const formVisible = ref(false)
const detailInfo = ref({})
const showType = ref()
const { crud, columns } = useCRUD(
  {
    title: '物流记录',
    sort: ['auditTime.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    invisibleColumns: ['manufactureType', 'productType', 'serialNumber']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

function openForm(row, type) {
  showType.value = type
  detailInfo.value = row?.sourceRow
  formVisible.value = true
}
</script>
<style lang="scss" scoped>
  .blue{
    color:#409eff;
  }
  .orange{
    color:#e6a23c;
  }
</style>
