<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader ref="header" :permission="permission" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column :selectable="checkboxT" type="selection" width="55" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" label="名称" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission"  del-prompt="确定删除吗,如果存在下级节点则一并删除，此操作不能撤销！" :disabled-del="scope.row.id === 1"/>
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/user-manage/dept'
import { deptConfigPM as permission } from '@/page-permission/user'

import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '部门',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

function checkboxT(row, rowIndex) {
  return row.id !== 1
}
</script>
