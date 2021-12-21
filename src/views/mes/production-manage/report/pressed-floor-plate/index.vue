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
        </template>
        <template v-slot:summaryText="{ summary }">
          <span>{{ summary?.quantity }}张</span> /
          <span>{{ toFixed(summary?.totalLength, DP.MES_ENCLOSURE_L__M) }}m</span>
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
      />
      <el-table-column
        v-if="columns.visible('plate')"
        key="plate"
        prop="plate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="板型"
        min-width="120px"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="120px"
      />
      <el-table-column
        v-if="columns.visible('thickness')"
        key="thickness"
        prop="thickness"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`板厚\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单长\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('width')"
        key="width"
        prop="width"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`有效宽度\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.width, DP.MES_ENCLOSURE_W__MM) }}</span>
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
        v-if="columns.visible('totalArea')"
        key="totalArea"
        prop="totalArea"
        sortable="custom"
        :label="`总面积\n(㎡)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalArea, DP.COM_AREA__M2) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总长度\n(m)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalLength, DP.MES_ENCLOSURE_L__M) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('weight')"
        key="weight"
        prop="weight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`重量\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.weight, DP.COM_WT__KG) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="120px"
      />
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
import crudApi, { getSummary } from '@/api/mes/production-manage/report/enclosure'
import { ref, provide } from 'vue'

import { mesEnclosureTypeEnum } from '@enum-ms/mes'
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
provide('defaultQuery', {
  category: mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V
})

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产报表-压型楼承板',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['remark', 'material', 'width', 'weight']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
</script>
