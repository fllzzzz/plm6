<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="name" label="油漆类别" align="center" />
      <el-table-column prop="measureStr" label="计量方式" align="center" />
      <el-table-column prop="thickness" label="标准涂装厚度（um）" align="center" />
      <el-table-column prop="unitPrice" label="标准单价" align="center">
        <template #default="{ row }">
          <span>{{ row.unitPrice }} {{ row.measureUnit ? `元/${wageQuotaTypeEnum.V[row.measureUnit].C_UNIT}` : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="outThickness" label="超标准设定（um）" align="center" />
      <el-table-column prop="outUnitPrice" label="超标准单价" align="center">
        <template #default="{ row }">
          <span>{{ row.outUnitPrice }} {{ row.measureUnit ? `元/${wageQuotaTypeEnum.V[row.measureUnit].C_UNIT}` : '' }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :showDel="false" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/coating-config'
import { ref } from 'vue'

import { configStatisticalCoatingPM as permission } from '@/page-permission/config'
import { wageQuotaTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'

const columnsDataFormat = [['measureStr', ['parse-enum', wageQuotaTypeEnum], { source: 'measureUnit' }]]

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud } = useCRUD(
  {
    title: '涂装配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight()
</script>
