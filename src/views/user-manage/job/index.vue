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
      <el-table-column type="selection" width="55" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" label="名称" />
      <el-table-column v-if="columns.visible('dept')" key="dept" prop="dept" label="所属部门">
        <template v-slot="scope">
          <div>{{ scope.row.deptName }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center">
        <template v-slot="scope">
          {{ scope.row.sort }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('enabled')" key="enabled" prop="enabled" label="状态" align="center">
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.enabled"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="systemEnabledEnum.TRUE.V"
            :inactive-value="systemEnabledEnum.FALSE.V"
            :disabled="!checkPermission(permission.edit)"
            @change="changeStatus(scope.row, scope.row.enabled)"
          />
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
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/user-manage/job'
import { jobConfigPM as permission } from '@/page-permission/user'

import { ref } from 'vue'
import { systemEnabledEnum } from '@enum-ms/system'
import checkPermission from '@/utils/system/check-permission'

import { ElMessageBox } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import pagination from '@crud/Pagination'

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '岗位',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true
})

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + systemEnabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    const submitData = {
      id: data.id,
      enabled: data.enabled
    }
    await editStatus(submitData)
    crud.refresh()
    crud.notify(systemEnabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更岗位状态', error)
    data.enabled = data.enabled === systemEnabledEnum.TRUE.V ? systemEnabledEnum.FALSE.V : systemEnabledEnum.TRUE.V
  }
}
</script>
