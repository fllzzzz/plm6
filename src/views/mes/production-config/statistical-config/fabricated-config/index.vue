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
      <el-table-column prop="auxiliaryName" label="类别" align="center" />
      <el-table-column prop="unitPrice" label="单价（元）" align="center" />
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="130px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <udOperation :data="row" :disabledDel="isBlank(row.unitPrice)" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/fabricated-config'
import { ref } from 'vue'

import { isBlank } from '@data-type/index'
import { configStatisticalFabricatedPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '零件-栓钉套筒配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.id = v.auxiliaryId
    return v
  })
}
</script>
