<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader>
        <template #teamType>
          <slot name="teamType"></slot>
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight + 42"
      style="width: 100%"
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('processName')"
        key="processName"
        prop="processName"
        :show-overflow-tooltip="true"
        label="工序名称"
        width="120px"
      />
      <el-table-column
        v-if="columns.visible('productType')"
        key="productType"
        prop="productType"
        label="类型"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ componentTypeEnum.VL[scope.row.productType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inspectorNames')"
        key="inspectorNames"
        prop="inspectorNames"
        :show-overflow-tooltip="true"
        label="质检"
        min-width="160px"
      />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/bridge/production-config/production-line-inspection'
import { defineExpose, ref } from 'vue'
// import { useStore } from 'vuex'

import checkPermission from '@/utils/system/check-permission'
import { configProductionLineInspectPM as permission } from '@/page-permission/config'
import { bridgeProcessTypeEnum as componentTypeEnum } from '@enum-ms/bridge'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

// const store = useStore()

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '质检',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.inspectors = v.userLinkList.map((v) => {
      const user = {
        inspectionTeamId: v.inspectionTeamId,
        id: v.userId,
        name: v.userName
      }
      return user
    })
    if (v.inspectors && v.inspectors.length > 0) {
      v.inspectorNames = v.inspectors.map((v) => v.name).join(', ')
      v.inspectorIds = v.inspectors.map((v) => v.id)
    } else {
      v.inspectorNames = ''
      v.inspectorIds = []
    }
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  const members = crud.form.inspectors
  const userList = []
  for (let i = 0; i < members.length; i++) {
    if (members[i]) {
      userList.push({
        userId: members[i].id,
        userName: members[i].name,
        inspectionTeamId: crud.form.id
      })
    }
  }
  crud.form.userLinks = userList
}

// 编辑之后 取消缓存的已加载设置
// CRUD.HOOK.afterSubmit = () => {
//   store.commit('config/SET_LOADED', { key: 'inspectionTeam', loaded: false })
// }
// CRUD.HOOK.afterDelete = () => {
//   store.commit('config/SET_LOADED', { key: 'inspectionTeam', loaded: false })
// }

defineExpose({
  permission,
  toAdd: crud.toAdd
})
</script>
