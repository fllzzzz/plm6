<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader>
        <template v-slot:customSearch>
          <el-input
            v-model="crud.query.name"
            size="small"
            placeholder="输入名称搜索"
            style="width: 170px"
            class="filter-item"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="crud.query.serialNumber"
            size="small"
            placeholder="输入编号搜索"
            style="width: 170px"
            class="filter-item"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="crud.query.specification"
            size="small"
            placeholder="输入规格搜索"
            style="width: 170px"
            class="filter-item"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="crud.query.material"
            size="small"
            placeholder="输入材质搜索"
            style="width: 170px"
            class="filter-item"
            clearable
            @keyup.enter="crud.toQuery"
          />
        </template>
        <template v-slot:summaryText="{ summary }">
          <span>{{ summary?.quantity }}件</span> /
          <span>{{ toFixed(summary?.totalNetWeight, DP.COM_WT__KG) }}kg</span>
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
      style="width: 100%"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="project.shortName" label="所属项目" min-width="200px">
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="120px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.monomer.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="120px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
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
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.length, DP.MES_ARTIFACT_L__MM) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        label="数量"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('netWeight')"
        key="netWeight"
        prop="netWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单净重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.netWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('grossWeight')"
        key="grossWeight"
        prop="grossWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单毛重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.grossWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        key="totalNetWeight"
        prop="totalNetWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总净重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.totalNetWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalGrossWeight')"
        key="totalGrossWeight"
        prop="totalGrossWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总毛重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.totalGrossWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('drawingNumber')"
        key="drawingNumber"
        prop="drawingNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="图号"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.drawingNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('surfaceArea')"
        key="surfaceArea"
        prop="surfaceArea"
        sortable="custom"
        :label="`面积\n(㎡)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.surfaceArea, DP.COM_AREA__M2) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="120px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.remark }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('date')"
        key="date"
        prop="date"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="生产日期"
        align="center"
        width="160px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.date }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi, { getSummary } from '@/api/mes/production-manage/report/artifact'
import { ref, provide } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { projectNameFormatter } from '@/utils/project'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from '../components/report-header.vue'

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

provide('getSummaryApi', getSummary)
provide('defaultQuery', {})

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产报表-结构报表',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['totalNetWeight', 'totalGrossWeight', 'drawingNumber', 'surfaceArea', 'remark']
  },
  tableRef
)

console.log('report artifact init')

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
</script>
