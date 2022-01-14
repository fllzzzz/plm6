<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
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
        fixed
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        fixed
        sortable="custom"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('plate')"
        key="plate"
        prop="plate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="板型"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('color')"
        key="color"
        prop="color"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="颜色"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('thickness')"
        key="thickness"
        prop="thickness"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`厚度\n(mm)`"
        align="left"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('width')"
        key="width"
        prop="width"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`有效宽度\n(mm)`"
        align="left"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.width, DP.MES_ENCLOSURE_W__MM) }}
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
          {{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        :show-overflow-tooltip="true"
        :label="`总长度\n(m)`"
        align="left"
        min-width="100px"
      >
        <template v-slot="scope">
          {{ convertUnits(scope.row.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        label="清单数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        key="taskQuantity"
        prop="taskQuantity"
        label="任务数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('inboundQuantity')"
        key="inboundQuantity"
        prop="inboundQuantity"
        :show-overflow-tooltip="true"
        label="入库数量"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('outboundQuantity')"
        key="outboundQuantity"
        prop="outboundQuantity"
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
        v-if="columns.visible('inboundLength')"
        key="inboundLength"
        prop="inboundLength"
        :show-overflow-tooltip="true"
        :label="`入库长度\n(m)`"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          {{ convertUnits(scope.row.inboundLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('outboundLength')"
        key="outboundLength"
        prop="outboundLength"
        :show-overflow-tooltip="true"
        :label="`出库长度\n(m)`"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          {{ convertUnits(scope.row.outboundLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('stockLength')"
        key="stockLength"
        prop="stockLength"
        :show-overflow-tooltip="true"
        :label="`库存长度\n(m)`"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          {{ convertUnits(scope.row.stockLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { getBoardForEnclosure as get } from '@/api/mes/manufactures-manage/common'
import { ref } from 'vue'
// import { mapGetters } from '@/store/lib'

import { enclosureWarehousePM as permission } from '@/page-permission/mes'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { toFixed } from '@data-type'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

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
    title: '围护出入库状态',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    requiredQuery: ['areaId'],
    invisibleColumns: ['surfaceArea', 'inboundLength', 'outboundLength', 'stockLength'],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
</script>
