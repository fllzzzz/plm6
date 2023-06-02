<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader class="head-container" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="费用科目名称"
      />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <ud-operation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/expense-subject'
import { ref } from 'vue'

import { expenseSubjectPM as permission } from '@/page-permission/config'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '费用科目',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true
})
</script>
