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
      :max-height="maxHeight"
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
        min-width="100px"
      >
        <template #default="{ row }">
          <span>{{ row.processName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('leaderName')" key="leaderName" prop="leaderName" label="组长" min-width="100px" />
      <el-table-column
        v-if="columns.visible('memberNames')"
        key="memberNames"
        prop="memberNames"
        :show-overflow-tooltip="true"
        label="组员"
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
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/enclosure/production-config/team'
import { defineExpose, ref } from 'vue'
import { useStore } from 'vuex'
import checkPermission from '@/utils/system/check-permission'
import { enclosureConfigProductionLineTeamPM as permission } from '@/page-permission/config'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const store = useStore()

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '班组',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    const members = []
    v.userLinkList.forEach((m) => {
      if (m.boolLeaderEnum) {
        v.leaderName = m.userName
        v.leaderId = m.userId
        v.leader = {
          id: m.userId,
          name: m.userName
        }
      } else {
        members.push({
          teamId: m.teamId,
          id: m.userId,
          name: m.userName
        })
      }
    })
    v.members = members
    if (v.members.length > 0) {
      v.memberNames = v.members.map((v) => v.name).join(', ')
      v.memberIds = v.members.map((v) => v.id)
    } else {
      v.memberNames = ''
      v.memberIds = []
    }
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  const members = crud.form.members
  const userList = []
  userList.push({
    boolLeaderEnum: true,
    userId: crud.form.leader.id,
    userName: crud.form.leader.name,
    teamId: crud.form.id
  })
  for (let i = 0; i < members.length; i++) {
    if (members[i]) {
      userList.push({
        boolLeaderEnum: false,
        userId: members[i].id,
        userName: members[i].name,
        teamId: crud.form.id
      })
    }
  }
  crud.form.userLinks = userList
}

// TODO:分多模块后未处理
// 编辑之后 取消缓存的已加载设置
CRUD.HOOK.afterSubmit = () => {
  store.commit('config/SET_LOADED', { key: 'productionTeam', loaded: false })
}
CRUD.HOOK.afterDelete = () => {
  store.commit('config/SET_LOADED', { key: 'productionTeam', loaded: false })
}

defineExpose({
  permission,
  toAdd: crud.toAdd
})
</script>
