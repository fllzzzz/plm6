<template>
  <div class="app-container">
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
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="合同编号"
        min-width="120"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('shortName')"
        key="shortName"
        prop="shortName"
        :show-overflow-tooltip="true"
        label="项目名称"
        min-width="120"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.shortName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productType')"
        key="productType"
        prop="productType"
        :show-overflow-tooltip="true"
        align="center"
        label="类型"
        width="100"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ componentTypeEnum.VL[scope.row.productType] }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unit')" key="unit" prop="unit" :show-overflow-tooltip="true" label="单位" align="center" width="100">
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.unit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('beginMete')"
        key="beginMete"
        prop="beginMete"
        :show-overflow-tooltip="true"
        label="期初库存"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.BEGIN.V)">
            <span v-empty-text>{{ scope.row.beginMete }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundMete')"
        key="inboundMete"
        prop="inboundMete"
        :show-overflow-tooltip="true"
        label="入库量"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.INBOUND.V)">
            <span v-empty-text>{{ scope.row.inboundMete }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('outboundMete')"
        key="outboundMete"
        prop="outboundMete"
        :show-overflow-tooltip="true"
        label="出库量"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.OUTBOUND.V)">
            <span v-empty-text>{{ scope.row.outboundMete }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('endMete')"
        key="endMete"
        prop="endMete"
        :show-overflow-tooltip="true"
        label="期末库存"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.END.V)">
            <span v-empty-text> {{ scope.row.endMete }}</span>
          </div>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/manufactures-manage/report'
import { ref, provide } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const reportTypeEnum = {
  INBOUND: { L: '入库', K: 'INBOUND', V: 0 },
  OUTBOUND: { L: '出库', K: 'OUTBOUND', V: 1 },
  BEGIN: { L: '期初库存', K: 'BEGIN', V: 2 },
  END: { L: '期末库存', K: 'END', V: 4 }
}

provide('reportTypeEnum', reportTypeEnum)

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '入发存报表',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
</script>
