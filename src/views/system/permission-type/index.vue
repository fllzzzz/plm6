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
      @selection-change="crud.selectionChangeHandler"
      :showEmptySymbol="false"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="类型名称" min-width="125px" />
      <el-table-column v-if="columns.visible('icon')" key="icon" prop="icon" label="图标" align="center" min-width="150px">
        <template v-slot="scope">
          <svg-icon :icon-class="scope.row.icon" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" min-width="125px">
        <template v-slot="scope">
          {{ scope.row.sort }}
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/system/permission-type'
import { ref } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['permissionType:get'],
  add: ['permissionType:add'],
  edit: ['permissionType:edit'],
  del: ['permissionType:del']
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '权限类型',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.permission-type',
  paginate: true,
  extraHeight: 157
})
</script>
