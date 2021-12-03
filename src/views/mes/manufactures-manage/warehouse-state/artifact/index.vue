<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader/>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" fixed type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        sortable="custom"
        fixed
        :show-overflow-tooltip="true"
        label="名称"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        fixed
        :show-overflow-tooltip="true"
        label="编号"
        min-width="110px"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="规格"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`长度\n(mm)`"
        align="left"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.length, DP.MES_ARTIFACT_L__MM) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('weight')"
        key="weight"
        prop="weight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单重\n(kg)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.weight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalWeight')"
        key="totalWeight"
        prop="totalWeight"
        :show-overflow-tooltip="true"
        :label="`总重\n(kg)`"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.totalWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('area')"
        key="area"
        prop="area"
        sortable="custom"
        :label="`面积\n(㎡)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.area, DP.COM_AREA__M2) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('drawingNumber')"
        key="drawingNumber"
        prop="drawingNumber"
        :show-overflow-tooltip="true"
        label="图号"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        label="清单数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="crud.query.factoryId && columns.visible('taskQuantity')"
        key="taskQuantity"
        prop="taskQuantity"
        label="任务数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('intWarehouseQuantity')"
        key="intWarehouseQuantity"
        prop="intWarehouseQuantity"
        :show-overflow-tooltip="true"
        label="入库数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('outWarehouseQuantity')"
        key="outWarehouseQuantity"
        prop="outWarehouseQuantity"
        :show-overflow-tooltip="true"
        label="出库数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('stockQuantity')"
        key="stockQuantity"
        prop="stockQuantity"
        :show-overflow-tooltip="true"
        label="库存数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('inboundWeight')"
        key="inboundWeight"
        prop="inboundWeight"
        :show-overflow-tooltip="true"
        :label="`入库重量\n(kg)`"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.inboundWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('outboundWeight')"
        key="outboundWeight"
        prop="outboundWeight"
        :show-overflow-tooltip="true"
        :label="`出库重量\n(kg)`"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          {{toFixed( scope.row.outboundWeight,DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('stockWeight')"
        key="stockWeight"
        prop="stockWeight"
        :show-overflow-tooltip="true"
        :label="`库存重量\n(kg)`"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.stockWeight,DP.COM_WT__KG) }}
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { getBoardForArtifact as get } from '@/api/mes/manufactures-manage/common'
import { ref } from 'vue'
// import { mapGetters } from '@/store/lib'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const permission = {
  get: ['artifactWarehouseState:get'],
  print: ['artifactWarehouseState:print'],
  download: ['artifactWarehouseState:download']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
// const { globalProjectId } = mapGetters(['globalProjectId'])

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '构件出入库状态',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    // requiredQuery: ['districtId'],
    invisibleColumns: ['area', 'drawingNumber']
    // queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
</script>
