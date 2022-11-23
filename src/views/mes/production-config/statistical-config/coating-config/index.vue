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
      <el-table-column prop="measureUnit" label="计量方式" align="center" />
      <el-table-column prop="unitPrice" label="单价（元）" align="center" />
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :showDel="false" />
        </template>
      </el-table-column>
    </common-table>
    <mForm/>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/coating-config'
import { ref } from 'vue'

import { configStatisticalCoatingPM as permission } from '@/page-permission/config'
import { paintingMeasureUnitEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'

const columnsDataFormat = [['measureUnit', ['parse-enum', paintingMeasureUnitEnum]]]

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
